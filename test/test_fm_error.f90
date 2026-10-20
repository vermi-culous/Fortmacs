! test_fm_error.f90 -- tests for fm_error.
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

module test_fm_error
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use fm_error
   implicit none(type, external)
   private

   public :: collect_fm_error

contains

   subroutine collect_fm_error(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("default-is-ok", test_default_ok), &
                  new_unittest("intent-out-resets", test_intent_out_resets), &
                  new_unittest("names-and-messages", test_names), &
                  new_unittest("condition-hierarchy", test_hierarchy), &
                  new_unittest("format-error", test_format) &
                  ]
   end subroutine collect_fm_error

   subroutine test_default_ok(error)
      type(error_type), allocatable, intent(out) :: error
      type(status_type) :: st

      call check(error, st%ok(), "fresh status not ok")
      if (allocated(error)) return
      call set_error(st, FM_ARGS_OUT_OF_RANGE, "5, 10")
      call check(error,.not. st%ok(), "error status reported ok")
      if (allocated(error)) return
      call check(error, st%detail, "5, 10")
   end subroutine test_default_ok

   subroutine fail_then_return(status)
      type(status_type), intent(out) :: status
   end subroutine fail_then_return

   subroutine test_intent_out_resets(error)
      type(error_type), allocatable, intent(out) :: error
      type(status_type) :: st

      call set_error(st, FM_GENERIC_ERROR, "old")
      call fail_then_return(st)
      call check(error, st%ok(), "intent(out) did not reset status")
      if (allocated(error)) return
      call check(error,.not. allocated(st%detail), "detail survived reset")
   end subroutine test_intent_out_resets

   subroutine test_names(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, error_name(FM_ARGS_OUT_OF_RANGE), "args-out-of-range")
      if (allocated(error)) return
      call check(error, error_message(FM_BUFFER_READ_ONLY), "Buffer is read-only")
      if (allocated(error)) return
      call check(error, error_name(FM_TEXT_READ_ONLY), "text-read-only")
      if (allocated(error)) return
      call check(error, error_message(FM_USER_ERROR), "")
      if (allocated(error)) return
      call check(error, error_name(999), "error")
   end subroutine test_names

   ! Mirrors the error-conditions lists built in data.c:syms_of_data.
   subroutine test_hierarchy(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, condition_matches(FM_TEXT_READ_ONLY, FM_BUFFER_READ_ONLY), &
                 "text-read-only is not a buffer-read-only")
      if (allocated(error)) return
      call check(error, condition_matches(FM_TEXT_READ_ONLY, FM_GENERIC_ERROR), &
                 "text-read-only is not an error")
      if (allocated(error)) return
      call check(error, condition_matches(FM_OVERFLOW_ERROR, FM_ARITH_ERROR), &
                 "overflow-error is not an arith-error")
      if (allocated(error)) return
      call check(error,.not. condition_matches(FM_QUIT, FM_GENERIC_ERROR), &
                 "quit must not be an error")
      if (allocated(error)) return
      call check(error,.not. condition_matches(FM_BUFFER_READ_ONLY, FM_TEXT_READ_ONLY), &
                 "parent matched child condition")
      if (allocated(error)) return
      call check(error,.not. condition_matches(FM_OK, FM_GENERIC_ERROR), &
                 "success matched a condition")
   end subroutine test_hierarchy

   subroutine test_format(error)
      type(error_type), allocatable, intent(out) :: error
      type(status_type) :: st

      call check(error, format_error(st), "")
      if (allocated(error)) return
      call set_error(st, FM_END_OF_BUFFER)
      call check(error, format_error(st), "End of buffer")
      if (allocated(error)) return
      call set_error(st, FM_ARGS_OUT_OF_RANGE, "5, 10")
      call check(error, format_error(st), "Args out of range: 5, 10")
      if (allocated(error)) return
      call set_error(st, FM_USER_ERROR, "No more undo")
      call check(error, format_error(st), "No more undo")
   end subroutine test_format

end module test_fm_error
