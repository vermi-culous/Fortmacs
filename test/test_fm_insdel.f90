! test_fm_insdel.f90 -- tests for fm_insdel and fm_marker position conversion.
!
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Fortmacs is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

!> Upstream has no insdel-tests.el; these check the gap buffer directly.
!> Each test works in a fresh buffer and restores the previous one.
module test_fm_insdel
   use, intrinsic :: iso_fortran_env, only: int32, int64
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use fm_error, only: status_type, FM_BUFFER_READ_ONLY
   use fm_handle, only: handle_type
   use fm_buffer, only: buffers, cur, get_buffer_create, kill_buffer, &
                        set_buffer, current_buffer, buffer_live_p, buf_bytes, &
                        temp_set_point_both, set_buffer_read_only, buf_fetch_char
   use fm_marker, only: char_to_byte, byte_to_char, buf_charpos_to_bytepos
   use fm_insdel
   implicit none(type, external)
   private

   public :: collect_fm_insdel

   !> "aé€b" etc. as bytes: é = C3 A9, € = E2 82 AC.
   character(len=*), parameter :: e_acute = char(195)//char(169)
   character(len=*), parameter :: euro = char(226)//char(130)//char(172)

contains

   subroutine collect_fm_insdel(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("insert-ascii", test_insert_ascii), &
                  new_unittest("insert-multibyte", test_insert_multibyte), &
                  new_unittest("insert-in-middle", test_insert_middle), &
                  new_unittest("insert-char", test_insert_char), &
                  new_unittest("gap-growth", test_gap_growth), &
                  new_unittest("delete-range", test_delete), &
                  new_unittest("delete-adjusts-point", test_delete_point), &
                  new_unittest("delete-clamps-and-ignores-empty", test_delete_clamp), &
                  new_unittest("delete-by-bytes", test_delete_bytes), &
                  new_unittest("char-byte-conversion", test_conversion), &
                  new_unittest("read-only", test_read_only), &
                  new_unittest("gap-shrink", test_gap_shrink), &
                  new_unittest("position-cache-slot-reuse", test_cache_reuse) &
                  ]
   end subroutine collect_fm_insdel

   !> Make and select a fresh buffer; remember the old current buffer.
   subroutine enter(name, h, old)
      character(len=*), intent(in) :: name
      type(handle_type), intent(out) :: h, old
      type(status_type) :: st

      old = current_buffer()
      call get_buffer_create(name, h, st)
      call set_buffer(h, st)
   end subroutine enter

   subroutine leave(h, old)
      type(handle_type), intent(in) :: h, old
      type(status_type) :: st

      if (buffer_live_p(old)) call set_buffer(old, st)
      call kill_buffer(h, st)
   end subroutine leave

   !> Whole buffer text.
   function contents() result(s)
      character(len=:), allocatable :: s

      s = buf_bytes(buffers(cur), 1_int64, buffers(cur)%text%z_byte)
   end function contents

   !> Move point to char position `pos`.
   subroutine goto(pos)
      integer(int64), intent(in) :: pos

      call temp_set_point_both(buffers(cur), pos, char_to_byte(pos))
   end subroutine goto

   subroutine test_insert_ascii(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("ti-ascii", h, old)
      call insert("hello", st)
      call check(error, st%ok(), "insert failed")
      if (allocated(error)) return
      call check(error, contents(), "hello")
      if (allocated(error)) return
      call check(error, buffers(cur)%pt == 6 .and. buffers(cur)%pt_byte == 6, "point")
      if (allocated(error)) return
      call check(error, buffers(cur)%text%z == 6 .and. buffers(cur)%zv == 6, "z/zv")
      if (allocated(error)) return
      call insert("", st)
      call check(error, contents(), "hello")
      call leave(h, old)
   end subroutine test_insert_ascii

   subroutine test_insert_multibyte(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("ti-mb", h, old)
      call insert("a"//e_acute//euro//"b", st)
      call check(error, contents(), "a"//e_acute//euro//"b")
      if (allocated(error)) return
      ! 4 chars, 7 bytes: Z = 5, Z_BYTE = 8.
      call check(error, buffers(cur)%text%z, 5_int64)
      if (allocated(error)) return
      call check(error, buffers(cur)%text%z_byte, 8_int64)
      if (allocated(error)) return
      call check(error, buffers(cur)%pt_byte, 8_int64)
      if (allocated(error)) return
      ! Invalid multibyte text is rejected.
      call insert(char(128), st)
      call check(error,.not. st%ok(), "invalid text accepted")
      call leave(h, old)
   end subroutine test_insert_multibyte

   subroutine test_insert_middle(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("ti-mid", h, old)
      call insert("ad", st)
      call goto(2_int64)
      call insert("bc", st)
      call check(error, contents(), "abcd")
      if (allocated(error)) return
      call check(error, buffers(cur)%pt, 4_int64)
      if (allocated(error)) return
      ! Gap left (to 1), then right (to the end).
      call goto(1_int64)
      call insert(euro, st)
      call goto(6_int64)
      call insert(e_acute, st)
      call check(error, contents(), euro//"abcd"//e_acute)
      if (allocated(error)) return
      call goto(3_int64)
      call insert("-", st)
      call check(error, contents(), euro//"a-bcd"//e_acute)
      call leave(h, old)
   end subroutine test_insert_middle

   subroutine test_insert_char(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("ti-char", h, old)
      call insert_char(int(z'20AC', int32), st)
      call insert_char(int(iachar('x'), int32), st)
      call check(error, contents(), euro//"x")
      if (allocated(error)) return
      call check(error, buf_fetch_char(buffers(cur), 1_int64), int(z'20AC', int32))
      if (allocated(error)) return
      call check(error, buf_fetch_char(buffers(cur), 4_int64), int(iachar('x'), int32))
      call leave(h, old)
   end subroutine test_insert_char

   subroutine test_gap_growth(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st
      integer :: k
      character(len=:), allocatable :: s

      call enter("ti-grow", h, old)
      do k = 1, 3000
         call insert("xy", st)
         if (.not. st%ok()) exit
         ! Alternate insertion points to keep moving the gap.
         if (mod(k, 7) == 0) call goto(int(k, int64))
      end do
      call check(error, st%ok(), "insert failed while growing")
      if (allocated(error)) return
      call check(error, buffers(cur)%text%z, 6001_int64)
      if (allocated(error)) return
      s = contents()
      call check(error, len(s), 6000)
      if (allocated(error)) return
      call check(error, count_char(s, "x"), 3000)
      if (allocated(error)) return
      call check(error, len(buffers(cur)%text%beg, kind=int64), &
                 buffers(cur)%text%z_byte - 1 + buffers(cur)%text%gap_size)
      call leave(h, old)

   contains

      pure integer function count_char(str, c)
         character(len=*), intent(in) :: str
         character, intent(in) :: c
         integer :: i

         count_char = 0
         do i = 1, len(str)
            if (str(i:i) == c) count_char = count_char + 1
         end do
      end function count_char

   end subroutine test_gap_growth

   subroutine test_delete(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st
      character(len=:), allocatable :: gone

      call enter("ti-del", h, old)
      call insert("a"//e_acute//"bcd"//euro//"e", st)
      call del_range_1(2_int64, 4_int64, .true., st, gone)
      call check(error, gone, e_acute//"b")
      if (allocated(error)) return
      call check(error, contents(), "acd"//euro//"e")
      if (allocated(error)) return
      call check(error, buffers(cur)%text%z, 6_int64)
      if (allocated(error)) return
      call del_range(4_int64, 5_int64, st)
      call check(error, contents(), "acde")
      if (allocated(error)) return
      call check(error, buffers(cur)%text%z_byte, 5_int64)
      call leave(h, old)
   end subroutine test_delete

   subroutine test_delete_point(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("ti-delpt", h, old)
      call insert("abcdefgh", st)
      ! Point after the range: shifts down.
      call goto(8_int64)
      call del_range(2_int64, 4_int64, st)
      call check(error, buffers(cur)%pt, 6_int64)
      if (allocated(error)) return
      ! Point inside the range: moves to its start.
      call goto(4_int64)
      call del_range(3_int64, 5_int64, st)
      call check(error, buffers(cur)%pt, 3_int64)
      if (allocated(error)) return
      ! Point before the range: unchanged.
      call goto(1_int64)
      call del_range(2_int64, 3_int64, st)
      call check(error, buffers(cur)%pt, 1_int64)
      if (allocated(error)) return
      call check(error, contents(), "agh")
      call leave(h, old)
   end subroutine test_delete_point

   subroutine test_delete_clamp(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st
      integer(int64) :: tick

      call enter("ti-clamp", h, old)
      call insert("abc", st)
      tick = buffers(cur)%text%modiff
      call del_range(3_int64, 2_int64, st)
      call check(error, contents() == "abc" .and. buffers(cur)%text%modiff == tick, &
                 "reversed range changed the buffer")
      if (allocated(error)) return
      call del_range(-5_int64, 100_int64, st)
      call check(error, contents(), "")
      if (allocated(error)) return
      call check(error, buffers(cur)%pt, 1_int64)
      call leave(h, old)
   end subroutine test_delete_clamp

   subroutine test_delete_bytes(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("ti-delb", h, old)
      call insert("x"//euro//"y", st)
      ! Bytes 2..4 are the euro sign.
      call del_range_byte(2_int64, 5_int64, st)
      call check(error, contents(), "xy")
      if (allocated(error)) return
      call check(error, buffers(cur)%text%z, 3_int64)
      call leave(h, old)
   end subroutine test_delete_bytes

   subroutine test_conversion(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st
      integer(int64), parameter :: bytepos(*) = [1, 2, 4, 7, 8, 9]
      integer(int64) :: c

      call enter("ti-conv", h, old)
      ! chars: a é € b c  -> bytes 1 2 4 7 8 (and Z_BYTE 9)
      call insert("a"//e_acute//euro//"bc", st)
      ! Put the gap in the middle so conversions straddle it.
      call goto(3_int64)
      call insert("", st)
      call move_gap_both(3_int64, 4_int64)
      do c = 1, 6
         call check(error, char_to_byte(c), bytepos(c))
         if (allocated(error)) return
         call check(error, byte_to_char(bytepos(c)), c)
         if (allocated(error)) return
      end do
      ! The far end of the buffer, scanning backwards.
      call goto(1_int64)
      call check(error, buf_charpos_to_bytepos(cur, 5_int64), 8_int64)
      call leave(h, old)
   end subroutine test_conversion

   subroutine test_read_only(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("ti-ro", h, old)
      call insert("abc", st)
      call set_buffer_read_only(h, .true., st)
      call insert("x", st)
      call check(error, st%code, FM_BUFFER_READ_ONLY)
      if (allocated(error)) return
      call del_range(1_int64, 2_int64, st)
      call check(error, st%code, FM_BUFFER_READ_ONLY)
      if (allocated(error)) return
      call check(error, contents(), "abc")
      if (allocated(error)) return
      ! Unprepared deletion skips the check, as upstream.
      call del_range_1(1_int64, 2_int64, .false., st)
      call check(error, contents(), "bc")
      call leave(h, old)
   end subroutine test_read_only

   subroutine test_gap_shrink(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("ti-shrink", h, old)
      call insert("abcdef", st)
      call goto(4_int64)
      call make_gap(5000_int64, st)
      call check(error, buffers(cur)%text%gap_size >= 5000, "gap did not grow")
      if (allocated(error)) return
      call make_gap(-100000_int64, st)
      call check(error, buffers(cur)%text%gap_size, 20_int64)
      if (allocated(error)) return
      call check(error, contents(), "abcdef")
      call leave(h, old)
   end subroutine test_gap_shrink

   !> The char/byte cache must not leak from a killed buffer to a new one
   !> that reuses its registry slot with the same modiff.
   subroutine test_cache_reuse(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: a, b, keep, old, prev
      type(status_type) :: st
      integer(int64) :: slot, tick

      ! A live, visible buffer to fall back to, so killing A does not
      ! create *scratch* in A's slot.
      call enter("ti-cache-keep", keep, prev)

      call enter("ti-cache-a", a, old)
      call insert(euro//euro//euro//euro//euro//euro, st)
      call check(error, char_to_byte(4_int64), 10_int64)   ! now cached
      if (allocated(error)) return
      slot = a%index
      tick = buffers(cur)%text%modiff
      call leave(a, old)

      call enter("ti-cache-b", b, old)
      call insert("aaaaa"//euro, st)
      call check(error, b%index == slot .and. buffers(cur)%text%modiff == tick, &
                 "test setup: slot or modiff differs")
      if (allocated(error)) return
      call check(error, char_to_byte(4_int64), 4_int64)
      call leave(b, old)
      call leave(keep, prev)
   end subroutine test_cache_reuse

end module test_fm_insdel
