! main.f90 -- test-drive runner for all Fortmacs test suites.
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

!> Runs every suite, or one suite (and optionally one test) by name:
!>   fpm test -- [suite [test]]
!> New suites are registered in the `testsuites` array below.
program tester
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, run_selected, new_testsuite, &
                        testsuite_type, select_suite
   use test_fm_version, only: collect_fm_version
   use test_fm_error, only: collect_fm_error
   use test_fm_handle, only: collect_fm_handle
   use test_fm_character, only: collect_fm_character
   implicit none(type, external)

   ! test-drive's interfaces take default integers.
   integer :: stat, is
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0
   testsuites = [ &
                new_testsuite("fm_version", collect_fm_version), &
                new_testsuite("fm_error", collect_fm_error), &
                new_testsuite("fm_handle", collect_fm_handle), &
                new_testsuite("fm_character", collect_fm_character) &
                ]

   call get_argument(1, suite_name)
   call get_argument(2, test_name)

   if (allocated(suite_name)) then
      is = select_suite(testsuites, suite_name)
      if (is > 0 .and. is <= size(testsuites)) then
         if (allocated(test_name)) then
            write (error_unit, fmt) "Suite:", testsuites(is)%name
            call run_selected(testsuites(is)%collect, test_name, &
                              error_unit, stat)
            if (stat < 0) error stop 1
         else
            write (error_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
         end if
      else
         write (error_unit, fmt) "Available testsuites"
         do is = 1, size(testsuites)
            write (error_unit, fmt) "-", testsuites(is)%name
         end do
         error stop 1
      end if
   else
      do is = 1, size(testsuites)
         write (error_unit, fmt) "Testing:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, error_unit, stat)
      end do
   end if

   if (stat > 0) then
      write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop 1
   end if

contains

   !> Fetch command-line argument `idx`, leaving `arg` unallocated if absent.
   subroutine get_argument(idx, arg)
      integer, intent(in) :: idx
      character(len=:), allocatable, intent(out) :: arg
      integer :: length, astat

      call get_command_argument(idx, length=length, status=astat)
      if (astat /= 0 .or. length == 0) return
      allocate (character(len=length) :: arg)
      call get_command_argument(idx, arg, status=astat)
      if (astat /= 0) deallocate (arg)
   end subroutine get_argument

end program tester
