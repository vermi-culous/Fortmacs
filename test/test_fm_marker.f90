! test_fm_marker.f90 -- tests for marker objects (fm_marker, fm_insdel).
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

!> Upstream test/src/marker-tests.el needs windows and *Help*; its point
!> (Bug#24368: a marker copied from a buffer whose char and byte
!> positions differ) is covered by `set-marker-from-other-buffer`.
module test_fm_marker
   use, intrinsic :: iso_fortran_env, only: int64
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use fm_error, only: status_type, FM_GENERIC_ERROR, FM_INVALID_HANDLE
   use fm_handle, only: handle_type
   use fm_buffer, only: buffers, cur, get_buffer_create, kill_buffer, &
                        set_buffer, current_buffer, buffer_live_p, buf_bytes, &
                        temp_set_point_both, erase_buffer
   use fm_marker
   use fm_insdel, only: insert, insert_before_markers, del_range
   implicit none(type, external)
   private

   public :: collect_fm_marker

   character(len=*), parameter :: euro = char(226)//char(130)//char(172)

contains

   subroutine collect_fm_marker(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("make-marker-points-nowhere", test_nowhere), &
                  new_unittest("set-marker-clips", test_set_clips), &
                  new_unittest("insertion-before-and-at", test_insert), &
                  new_unittest("insert-before-markers", test_insert_before_markers), &
                  new_unittest("deletion", test_delete), &
                  new_unittest("copy-marker", test_copy), &
                  new_unittest("set-marker-from-other-buffer", test_other_buffer), &
                  new_unittest("kill-buffer-detaches", test_kill), &
                  new_unittest("free-marker", test_free), &
                  new_unittest("markers-as-conversion-hints", test_hints), &
                  new_unittest("erase-buffer", test_erase) &
                  ]
   end subroutine collect_fm_marker

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

   subroutine goto(pos)
      integer(int64), intent(in) :: pos

      call temp_set_point_both(buffers(cur), pos, char_to_byte(pos))
   end subroutine goto

   integer(int64) function pos_of(m)
      type(handle_type), intent(in) :: m
      type(status_type) :: st

      call marker_position(m, pos_of, st)
      if (.not. st%ok()) pos_of = -1
   end function pos_of

   integer(int64) function byte_of(m)
      type(handle_type), intent(in) :: m
      type(status_type) :: st

      call marker_byte_position(m, byte_of, st)
      if (.not. st%ok()) byte_of = -1
   end function byte_of

   subroutine test_nowhere(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: m, b
      type(status_type) :: st
      integer(int64) :: p

      call make_marker(m)
      call marker_buffer(m, b, st)
      call check(error, st%ok() .and. b%index == 0, "fresh marker has a buffer")
      if (allocated(error)) return
      call marker_position(m, p, st)
      call check(error, st%code, FM_GENERIC_ERROR)
      if (allocated(error)) return
      call check(error, st%detail, "Marker does not point anywhere")
      call free_marker(m, st)
   end subroutine test_nowhere

   subroutine test_set_clips(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old, m, b
      type(status_type) :: st

      call enter("tm-clip", h, old)
      call insert("a"//euro//"b", st)
      call make_marker(m)
      call set_marker(m, 3_int64, st)
      call check(error, pos_of(m) == 3 .and. byte_of(m) == 5, "position 3")
      if (allocated(error)) return
      call marker_buffer(m, b, st)
      call check(error, b%index == h%index .and. b%generation == h%generation, "buffer")
      if (allocated(error)) return
      call set_marker(m, 100_int64, st)
      call check(error, pos_of(m) == 4 .and. byte_of(m) == 6, "clipped to Z")
      if (allocated(error)) return
      call set_marker(m, -3_int64, st)
      call check(error, pos_of(m) == 1 .and. byte_of(m) == 1, "clipped to BEG")
      if (allocated(error)) return
      call check(error, count_markers(cur), 1_int64)
      call free_marker(m, st)
      call leave(h, old)
   end subroutine test_set_clips

   subroutine test_insert(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old, before, at_nil, at_t, after
      type(status_type) :: st

      call enter("tm-ins", h, old)
      call insert("abcd", st)
      call copy_marker_at(2_int64, before, .false., st)
      call copy_marker_at(3_int64, at_nil, .false., st)
      call copy_marker_at(3_int64, at_t, .true., st)
      call copy_marker_at(4_int64, after, .false., st)
      call goto(3_int64)
      call insert(euro//"x", st)        ! 2 chars, 4 bytes at 3
      call check(error, pos_of(before), 2_int64)
      if (allocated(error)) return
      call check(error, pos_of(at_nil), 3_int64)
      if (allocated(error)) return
      call check(error, pos_of(at_t) == 5 .and. byte_of(at_t) == 7, "type-t marker")
      if (allocated(error)) return
      call check(error, pos_of(after) == 6 .and. byte_of(after) == 8, "later marker")
      call free_marker(before, st)
      call free_marker(at_nil, st)
      call free_marker(at_t, st)
      call free_marker(after, st)
      call leave(h, old)
   end subroutine test_insert

   subroutine test_insert_before_markers(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old, m
      type(status_type) :: st

      call enter("tm-ibm", h, old)
      call insert("ab", st)
      call copy_marker_at(2_int64, m, .false., st)
      call goto(2_int64)
      call insert_before_markers("XY", st)
      call check(error, pos_of(m), 4_int64)
      call free_marker(m, st)
      call leave(h, old)
   end subroutine test_insert_before_markers

   subroutine test_delete(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old, inside, after, at_from
      type(status_type) :: st

      call enter("tm-del", h, old)
      call insert("ab"//euro//"cdef", st)   ! a b € c d e f
      call copy_marker_at(2_int64, at_from, .false., st)
      call copy_marker_at(4_int64, inside, .false., st)
      call copy_marker_at(6_int64, after, .false., st)
      call del_range(2_int64, 5_int64, st)  ! delete b € c
      call check(error, pos_of(at_from) == 2 .and. byte_of(at_from) == 2, "at from")
      if (allocated(error)) return
      call check(error, pos_of(inside) == 2 .and. byte_of(inside) == 2, "inside")
      if (allocated(error)) return
      call check(error, pos_of(after) == 3 .and. byte_of(after) == 3, "after")
      call free_marker(at_from, st)
      call free_marker(inside, st)
      call free_marker(after, st)
      call leave(h, old)
   end subroutine test_delete

   subroutine test_copy(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old, m, c, nowhere, cn
      type(status_type) :: st
      logical :: itype

      call enter("tm-copy", h, old)
      call insert("hello", st)
      call copy_marker_at(4_int64, m, .false., st)
      call copy_marker(m, c, .true., st)
      call check(error, pos_of(c), 4_int64)
      if (allocated(error)) return
      call marker_insertion_type(c, itype, st)
      call check(error, itype, "copy lost insertion type")
      if (allocated(error)) return
      call set_marker_insertion_type(c, .false., st)
      call marker_insertion_type(c, itype, st)
      call check(error,.not. itype, "insertion type not cleared")
      if (allocated(error)) return
      call make_marker(nowhere)
      call copy_marker(nowhere, cn, .false., st)
      call check(error, pos_of(cn), -1_int64)
      call free_marker(m, st)
      call free_marker(c, st)
      call free_marker(nowhere, st)
      call free_marker(cn, st)
      call leave(h, old)
   end subroutine test_copy

   !> The Bug#24368 scenario of marker-tests.el: a marker taken from a
   !> buffer whose char and byte positions differ, set into an ASCII
   !> buffer, must get a byte position computed in the target buffer.
   subroutine test_other_buffer(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: mb, ascii, old, old2, m1, m2
      type(status_type) :: st

      call enter("tm-mb", mb, old)
      call insert(euro//euro//euro, st)
      call copy_marker_at(4_int64, m1, .false., st)    ! point-max: char 4, byte 10
      call check(error, byte_of(m1), 10_int64)
      if (allocated(error)) return

      call enter("tm-ascii", ascii, old2)
      call insert("abcdefgh", st)
      call make_marker(m2)
      call set_marker_to(m2, m1, st)                    ! into the current (ASCII) buffer
      call check(error, pos_of(m2) == 4 .and. byte_of(m2) == 4, "byte position copied across buffers")
      if (allocated(error)) return
      ! Same buffer: positions are copied directly.
      call set_marker_to(m2, m1, st, mb)
      call check(error, pos_of(m2) == 4 .and. byte_of(m2) == 10, "same-buffer copy")
      call free_marker(m1, st)
      call free_marker(m2, st)
      call leave(ascii, old2)
      call leave(mb, old)
   end subroutine test_other_buffer

   subroutine test_kill(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old, m, b
      type(status_type) :: st
      integer(int64) :: last

      call enter("tm-kill", h, old)
      call insert("abc", st)
      call copy_marker_at(3_int64, m, .false., st)
      call leave(h, old)
      call marker_buffer(m, b, st)
      call check(error, st%ok() .and. b%index == 0, "marker still in killed buffer")
      if (allocated(error)) return
      call check(error, pos_of(m), -1_int64)
      if (allocated(error)) return
      call marker_last_position(m, last, st)
      call check(error, last, 3_int64)
      call free_marker(m, st)
   end subroutine test_kill

   subroutine test_free(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old, a, b
      type(status_type) :: st
      integer(int64) :: p

      call enter("tm-free", h, old)
      call insert("abc", st)
      call copy_marker_at(1_int64, a, .false., st)
      call copy_marker_at(2_int64, b, .false., st)
      call check(error, count_markers(cur), 2_int64)
      if (allocated(error)) return
      call free_marker(a, st)
      call check(error, count_markers(cur), 1_int64)
      if (allocated(error)) return
      call marker_position(a, p, st)
      call check(error, st%code, FM_INVALID_HANDLE)
      if (allocated(error)) return
      call check(error, pos_of(b), 2_int64)
      call free_marker(b, st)
      call check(error, count_markers(cur), 0_int64)
      call leave(h, old)
   end subroutine test_free

   subroutine test_hints(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old, m
      type(status_type) :: st
      integer :: k
      integer(int64) :: c

      call enter("tm-hint", h, old)
      do k = 1, 400
         call insert(euro//"a", st)
      end do
      ! A marker in the middle; conversions near it must stay correct.
      call copy_marker_at(401_int64, m, .false., st)
      call check(error, byte_of(m), 801_int64)
      if (allocated(error)) return
      call goto(1_int64)
      do c = 395, 405
         ! odd chars are euros (3 bytes), even chars are 'a'
         call check(error, char_to_byte(c), 1 + 4*((c - 1)/2) + 3*mod(c - 1, 2_int64))
         if (allocated(error)) return
         call check(error, byte_to_char(char_to_byte(c)), c)
         if (allocated(error)) return
      end do
      call free_marker(m, st)
      call leave(h, old)
   end subroutine test_hints

   subroutine test_erase(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old, m
      type(status_type) :: st

      call enter("tm-erase", h, old)
      call insert("hello"//euro, st)
      call copy_marker_at(4_int64, m, .false., st)
      call erase_buffer(st)
      call check(error, st%ok(), "erase failed")
      if (allocated(error)) return
      call check(error, buf_bytes(buffers(cur), 1_int64, buffers(cur)%text%z_byte), "")
      if (allocated(error)) return
      call check(error, pos_of(m) == 1 .and. buffers(cur)%pt == 1, "positions after erase")
      call free_marker(m, st)
      call leave(h, old)
   end subroutine test_erase

end module test_fm_marker
