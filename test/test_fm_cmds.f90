! test_fm_cmds.f90 -- tests for fm_search (newlines), line positions in
! fm_editfns, and fm_cmds.  Includes a case ported from GNU Emacs 31.1
! test/src/cmds-tests.el.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

module test_fm_cmds
   use, intrinsic :: iso_fortran_env, only: int64
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use fm_error, only: status_type, FM_BEGINNING_OF_BUFFER, FM_END_OF_BUFFER
   use fm_handle, only: handle_type
   use fm_buffer, only: get_buffer_create, kill_buffer, set_buffer, &
                        current_buffer, buffer_live_p
   use fm_insdel, only: insert, move_gap_both
   use fm_search, only: find_newline, find_before_next_newline
   use fm_editfns, only: point, goto_char, buffer_string, pos_bol, pos_eol, &
                         narrow_to_region
   use fm_cmds
   implicit none(type, external)
   private

   public :: collect_fm_cmds

   character(len=*), parameter :: nl = achar(10)
   character(len=*), parameter :: euro = char(226)//char(130)//char(172)
   !> most-positive-fixnum on a 64-bit host.
   integer(int64), parameter :: most_positive_fixnum = 2_int64**61 - 1
   integer(int64), parameter :: most_negative_fixnum = -2_int64**61

contains

   subroutine collect_fm_cmds(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("find-newline-forward", test_find_forward), &
                  new_unittest("find-newline-backward", test_find_backward), &
                  new_unittest("find-before-next-newline", test_before_next), &
                  new_unittest("pos-bol-eol", test_bol_eol), &
                  new_unittest("forward-backward-char", test_chars), &
                  new_unittest("forward-line", test_forward_line), &
                  new_unittest("forward-line-with-bignum", test_bignum), &
                  new_unittest("beginning-end-of-line", test_bol_cmds), &
                  new_unittest("delete-char", test_delete_char) &
                  ]
   end subroutine collect_fm_cmds

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

   !> Three lines with a multibyte char, gap placed mid-text.
   subroutine fill_lines()
      type(status_type) :: st

      ! chars:  a  €  \n  b  b  \n  c
      ! pos:    1  2  3   4  5  6   7   (Z = 8)
      call insert("a"//euro//nl//"bb"//nl//"c", st)
      call move_gap_both(5_int64, 7_int64)
   end subroutine fill_lines

   subroutine test_find_forward(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      integer(int64) :: pos, counted, bytepos

      call enter("tc-ff", h, old)
      call fill_lines()
      pos = find_newline(1_int64, count=1_int64, counted=counted, bytepos=bytepos)
      call check(error, pos == 4 .and. counted == 1 .and. bytepos == 6, "first newline")
      if (allocated(error)) return
      pos = find_newline(1_int64, count=2_int64, counted=counted)
      call check(error, pos == 7 .and. counted == 2, "second newline (across the gap)")
      if (allocated(error)) return
      pos = find_newline(1_int64, count=5_int64, counted=counted)
      call check(error, pos == 8 .and. counted == 2, "too few: stop at ZV")
      call leave(h, old)
   end subroutine test_find_forward

   subroutine test_find_backward(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      integer(int64) :: pos, counted

      call enter("tc-fb", h, old)
      call fill_lines()
      ! Backward results are positions *after* the newline found.
      pos = find_newline(8_int64, count=-1_int64, counted=counted)
      call check(error, pos == 7 .and. counted == -1, "last newline")
      if (allocated(error)) return
      pos = find_newline(8_int64, count=-2_int64, counted=counted)
      call check(error, pos == 4 .and. counted == -2, "first newline")
      if (allocated(error)) return
      pos = find_newline(8_int64, count=-3_int64, counted=counted)
      call check(error, pos == 1 .and. counted == -2, "too few: stop at BEGV")
      call leave(h, old)
   end subroutine test_find_backward

   subroutine test_before_next(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old

      call enter("tc-bnn", h, old)
      call fill_lines()
      call check(error, find_before_next_newline(1_int64, 1_int64), 3_int64)
      if (allocated(error)) return
      call check(error, find_before_next_newline(7_int64, 1_int64), 8_int64)
      call leave(h, old)
   end subroutine test_before_next

   subroutine test_bol_eol(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("tc-bol", h, old)
      call fill_lines()
      call goto_char(5_int64)
      call check(error, pos_bol() == 4 .and. pos_eol() == 6, "line 2")
      if (allocated(error)) return
      call check(error, pos_bol(2_int64) == 7 .and. pos_eol(2_int64) == 8, "next line")
      if (allocated(error)) return
      call check(error, pos_bol(0_int64) == 1 .and. pos_eol(0_int64) == 3, "previous line")
      if (allocated(error)) return
      call check(error, point(), 5_int64)   ! point untouched
      if (allocated(error)) return
      ! Narrowing bounds the scan.
      call narrow_to_region(5_int64, 8_int64, st)
      call goto_char(5_int64)
      call check(error, pos_bol(), 5_int64)
      call leave(h, old)
   end subroutine test_bol_eol

   subroutine test_chars(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("tc-chars", h, old)
      call insert("abc", st)
      call backward_char(st, 2_int64)
      call check(error, st%ok() .and. point() == 2, "backward 2")
      if (allocated(error)) return
      call forward_char(st)
      call check(error, point(), 3_int64)
      if (allocated(error)) return
      call forward_char(st, 5_int64)
      call check(error, st%code == FM_END_OF_BUFFER .and. point() == 4, "end-of-buffer")
      if (allocated(error)) return
      call backward_char(st, 10_int64)
      call check(error, st%code == FM_BEGINNING_OF_BUFFER .and. point() == 1, "beginning-of-buffer")
      call leave(h, old)
   end subroutine test_chars

   subroutine test_forward_line(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old

      call enter("tc-fl", h, old)
      call fill_lines()
      call goto_char(2_int64)
      call check(error, forward_line(), 0_int64)
      if (allocated(error)) return
      call check(error, point(), 4_int64)
      if (allocated(error)) return
      ! Last line is non-empty: moving to its end counts as a line.
      call check(error, forward_line(5_int64), 3_int64)
      if (allocated(error)) return
      call check(error, point(), 8_int64)
      if (allocated(error)) return
      call check(error, forward_line(0_int64), 0_int64)   ! to bol
      if (allocated(error)) return
      call check(error, point(), 7_int64)
      if (allocated(error)) return
      call check(error, forward_line(-5_int64), -3_int64)
      if (allocated(error)) return
      call check(error, point(), 1_int64)
      call leave(h, old)
   end subroutine test_forward_line

   !> cmds-tests.el: forward-line-with-bignum.
   subroutine test_bignum(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("tc-bignum", h, old)
      call insert("x"//nl, st)
      call check(error, forward_line(most_negative_fixnum - 1), most_negative_fixnum)
      if (allocated(error)) return
      call check(error, forward_line(most_positive_fixnum + 2), most_positive_fixnum + 1)
      call leave(h, old)
   end subroutine test_bignum

   subroutine test_bol_cmds(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old

      call enter("tc-bolcmd", h, old)
      call fill_lines()
      call goto_char(5_int64)
      call end_of_line()
      call check(error, point(), 6_int64)
      if (allocated(error)) return
      call beginning_of_line()
      call check(error, point(), 4_int64)
      if (allocated(error)) return
      call end_of_line(2_int64)
      call check(error, point(), 8_int64)
      call leave(h, old)
   end subroutine test_bol_cmds

   subroutine test_delete_char(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      call enter("tc-del", h, old)
      call insert("abcdef", st)
      call goto_char(3_int64)
      call delete_char(2_int64, st)
      call check(error, buffer_string(), "abef")
      if (allocated(error)) return
      call delete_char(-1_int64, st)
      call check(error, buffer_string(), "aef")
      if (allocated(error)) return
      call delete_char(10_int64, st)
      call check(error, st%code == FM_END_OF_BUFFER .and. buffer_string() == "aef", "end-of-buffer")
      if (allocated(error)) return
      call delete_char(-10_int64, st)
      call check(error, st%code, FM_BEGINNING_OF_BUFFER)
      call leave(h, old)
   end subroutine test_delete_char

end module test_fm_cmds
