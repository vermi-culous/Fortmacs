! test_fm_editfns.f90 -- tests for fm_editfns.
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

!> The editfns-tests.el cases need `format`, text properties, undo or
!> `transpose-regions`, none of which exist yet; these are our own.
module test_fm_editfns
   use, intrinsic :: iso_fortran_env, only: int32, int64
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use fm_error, only: status_type, FM_ARGS_OUT_OF_RANGE
   use fm_handle, only: handle_type
   use fm_buffer, only: get_buffer_create, kill_buffer, set_buffer, &
                        current_buffer, buffer_live_p, erase_buffer
   use fm_marker, only: make_marker, set_marker, free_marker, marker_position
   use fm_insdel, only: insert
   use fm_editfns
   implicit none(type, external)
   private

   public :: collect_fm_editfns

   character(len=*), parameter :: euro = char(226)//char(130)//char(172)

contains

   subroutine collect_fm_editfns(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("point-and-bounds", test_bounds), &
                  new_unittest("goto-char-clips", test_goto), &
                  new_unittest("goto-char-marker", test_goto_marker), &
                  new_unittest("position-bytes", test_position_bytes), &
                  new_unittest("chars-around-point", test_chars_around), &
                  new_unittest("line-predicates", test_lines), &
                  new_unittest("narrow-and-widen", test_narrow), &
                  new_unittest("narrowing-limits-editing", test_narrow_edit), &
                  new_unittest("buffer-substring", test_substring), &
                  new_unittest("delete-region", test_delete), &
                  new_unittest("insert-char-count", test_insert_char), &
                  new_unittest("point-markers", test_point_markers), &
                  new_unittest("erase-buffer-widens", test_erase) &
                  ]
   end subroutine collect_fm_editfns

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

   subroutine test_bounds(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st
      integer(int64) :: size

      call enter("te-bounds", h, old)
      call check(error, point() == 1 .and. point_min() == 1 .and. point_max() == 1, "empty")
      if (allocated(error)) return
      call insert("ab"//euro, st)
      call check(error, point() == 4 .and. point_max() == 4 .and. buffer_size() == 3, "after insert")
      if (allocated(error)) return
      call buffer_size_of(h, size, st)
      call check(error, size, 3_int64)
      if (allocated(error)) return
      call check(error, gap_position(), 4_int64)
      call leave(h, old)
   end subroutine test_bounds

   subroutine test_goto(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("te-goto", h, old)
      call insert("hello", st)
      call goto_char(3_int64)
      call check(error, point(), 3_int64)
      if (allocated(error)) return
      call goto_char(-10_int64)
      call check(error, point(), 1_int64)
      if (allocated(error)) return
      call goto_char(99_int64)
      call check(error, point(), 6_int64)
      call leave(h, old)
   end subroutine test_goto

   !> As marker-goto-char-from-other-buffer: the byte position must be
   !> recomputed when the marker belongs to another buffer.
   subroutine test_goto_marker(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: mb, ascii, old, old2, m
      type(status_type) :: st
      integer(int64) :: bytepos
      logical :: found

      call enter("te-gm-mb", mb, old)
      call insert(euro//euro//euro, st)
      call make_marker(m)
      call set_marker(m, 3_int64, st)              ! char 3 = byte 7 here
      call enter("te-gm-ascii", ascii, old2)
      call insert("abcdef", st)
      call goto_char_marker(m, st)
      call check(error, point(), 3_int64)
      if (allocated(error)) return
      call position_bytes(point(), bytepos, found)
      call check(error, found .and. bytepos == 3, "point byte position")
      call free_marker(m, st)
      call leave(ascii, old2)
      call leave(mb, old)
   end subroutine test_goto_marker

   subroutine test_position_bytes(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st
      integer(int64) :: p
      logical :: found

      call enter("te-pb", h, old)
      call insert("a"//euro//"b", st)            ! bytes: a=1, euro=2..4, b=5
      call position_bytes(3_int64, p, found)
      call check(error, found .and. p == 5, "position-bytes 3")
      if (allocated(error)) return
      call position_bytes(10_int64, p, found)
      call check(error,.not. found, "out of range accepted")
      if (allocated(error)) return
      call byte_to_position(3_int64, p, found)    ! inside the euro sign
      call check(error, found .and. p == 2, "byte-to-position mid-char")
      if (allocated(error)) return
      call byte_to_position(6_int64, p, found)    ! Z_BYTE
      call check(error, found .and. p == 4, "byte-to-position at Z")
      call leave(h, old)
   end subroutine test_position_bytes

   subroutine test_chars_around(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st
      integer(int32) :: c
      logical :: found

      call enter("te-chars", h, old)
      call insert("a"//euro, st)
      call check(error, following_char(), 0_int32)
      if (allocated(error)) return
      call check(error, preceding_char(), int(z'20AC', int32))
      if (allocated(error)) return
      call goto_char(1_int64)
      call check(error, following_char(), int(iachar('a'), int32))
      if (allocated(error)) return
      call check(error, preceding_char(), 0_int32)
      if (allocated(error)) return
      call char_after(c, found, 2_int64)
      call check(error, found .and. c == int(z'20AC', int32), "char-after 2")
      if (allocated(error)) return
      call char_after(c, found, 3_int64)
      call check(error,.not. found, "char-after at end")
      if (allocated(error)) return
      call char_before(c, found, 3_int64)
      call check(error, found .and. c == int(z'20AC', int32), "char-before 3")
      if (allocated(error)) return
      call char_before(c, found)
      call check(error,.not. found, "char-before at bob")
      call leave(h, old)
   end subroutine test_chars_around

   subroutine test_lines(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("te-lines", h, old)
      call insert("ab"//new_line("a")//"cd", st)
      call check(error, eolp() .and. eobp() .and. .not. bolp(), "at end")
      if (allocated(error)) return
      call goto_char(4_int64)
      call check(error, bolp() .and. .not. eolp(), "start of line 2")
      if (allocated(error)) return
      call goto_char(3_int64)
      call check(error, eolp() .and. .not. bolp(), "end of line 1")
      if (allocated(error)) return
      call goto_char(1_int64)
      call check(error, bobp() .and. bolp(), "beginning")
      call leave(h, old)
   end subroutine test_lines

   subroutine test_narrow(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("te-narrow", h, old)
      call insert("0123456789", st)                 ! point at 11
      call narrow_to_region(7_int64, 3_int64, st)   ! reversed is fine
      call check(error, point_min() == 3 .and. point_max() == 7, "bounds")
      if (allocated(error)) return
      call check(error, point(), 7_int64)           ! clamped into the region
      if (allocated(error)) return
      call check(error, buffer_string(), "2345")
      if (allocated(error)) return
      call check(error, bobp() .eqv. .false., "bobp")
      if (allocated(error)) return
      call narrow_to_region(0_int64, 5_int64, st)
      call check(error, st%code, FM_ARGS_OUT_OF_RANGE)
      if (allocated(error)) return
      call widen()
      call check(error, point_min() == 1 .and. point_max() == 11, "widened")
      if (allocated(error)) return
      call check(error, buffer_string(), "0123456789")
      call leave(h, old)
   end subroutine test_narrow

   subroutine test_narrow_edit(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("te-narrow-edit", h, old)
      call insert("abcdef", st)
      call narrow_to_region(2_int64, 5_int64, st)   ! "bcd"
      call goto_char(5_int64)
      call insert("X", st)                          ! extends the region
      call check(error, point_max(), 6_int64)
      if (allocated(error)) return
      call check(error, buffer_string(), "bcdX")
      if (allocated(error)) return
      call delete_region(1_int64, 3_int64, st)       ! starts before point-min
      call check(error, st%code, FM_ARGS_OUT_OF_RANGE)
      if (allocated(error)) return
      call delete_region(2_int64, 4_int64, st)
      call check(error, buffer_string(), "dX")
      if (allocated(error)) return
      call widen()
      call check(error, buffer_string(), "adXef")
      call leave(h, old)
   end subroutine test_narrow_edit

   subroutine test_substring(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st
      character(len=:), allocatable :: s

      call enter("te-substr", h, old)
      call insert("x"//euro//"yz", st)
      call buffer_substring(4_int64, 2_int64, s, st)
      call check(error, s, euro//"y")
      if (allocated(error)) return
      call buffer_substring(1_int64, 9_int64, s, st)
      call check(error, st%code, FM_ARGS_OUT_OF_RANGE)
      if (allocated(error)) return
      call check(error, st%detail, "1, 9")
      call leave(h, old)
   end subroutine test_substring

   subroutine test_delete(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st
      character(len=:), allocatable :: s

      call enter("te-delete", h, old)
      call insert("hello world", st)
      call delete_and_extract_region(6_int64, 12_int64, s, st)
      call check(error, s, " world")
      if (allocated(error)) return
      call delete_and_extract_region(3_int64, 3_int64, s, st)
      call check(error, s == "" .and. st%ok(), "empty extraction")
      if (allocated(error)) return
      call delete_region(1_int64, 2_int64, st)
      call check(error, buffer_string(), "ello")
      call leave(h, old)
   end subroutine test_delete

   subroutine test_insert_char(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("te-insert-char", h, old)
      call insert_char_n(int(z'20AC', int32), 2000_int64, st)   ! 6000 bytes: two chunks
      call check(error, st%ok(), "insert-char failed")
      if (allocated(error)) return
      call check(error, point_max(), 2001_int64)
      if (allocated(error)) return
      call check(error, len(buffer_string()), 6000)
      if (allocated(error)) return
      call insert_char_n(int(iachar('a'), int32), 0_int64, st)
      call check(error, point_max(), 2001_int64)
      if (allocated(error)) return
      call insert_char_n(-1_int32, 1_int64, st)
      call check(error,.not. st%ok(), "invalid character accepted")
      call leave(h, old)
   end subroutine test_insert_char

   subroutine test_point_markers(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old, pm, lo, hi
      type(status_type) :: st
      integer(int64) :: p

      call enter("te-pm", h, old)
      call insert("abcdef", st)
      call narrow_to_region(2_int64, 5_int64, st)
      call goto_char(3_int64)
      call point_marker(pm)
      call point_min_marker(lo)
      call point_max_marker(hi)
      call marker_position(pm, p, st)
      call check(error, p, 3_int64)
      if (allocated(error)) return
      call marker_position(lo, p, st)
      call check(error, p, 2_int64)
      if (allocated(error)) return
      call marker_position(hi, p, st)
      call check(error, p, 5_int64)
      call free_marker(pm, st)
      call free_marker(lo, st)
      call free_marker(hi, st)
      call leave(h, old)
   end subroutine test_point_markers

   subroutine test_erase(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("te-erase", h, old)
      call insert("abcdef", st)
      call narrow_to_region(2_int64, 4_int64, st)
      call erase_buffer(st)
      call check(error, buffer_size() == 0 .and. point_min() == 1 .and. point_max() == 1, &
                 "erase-buffer must widen and delete everything")
      call leave(h, old)
   end subroutine test_erase

end module test_fm_editfns
