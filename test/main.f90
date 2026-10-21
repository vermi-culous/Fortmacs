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
    use test_fm_buffer, only: collect_fm_buffer
    use test_fm_insdel, only: collect_fm_insdel
    use test_fm_marker, only: collect_fm_marker
    use test_fm_sysdep, only: collect_fm_sysdep
    use test_fm_editfns, only: collect_fm_editfns
    use test_fm_cmds, only: collect_fm_cmds
    use test_fm_values, only: collect_fm_values
    use test_fm_textprop, only: collect_fm_textprop
    use test_fm_keymap, only: collect_fm_keymap
    use test_fm_keyboard, only: collect_fm_keyboard
    use test_fm_bindings, only: collect_fm_bindings
    use test_fm_term_xterm, only: collect_fm_term_xterm
    use test_fm_terminfo, only: collect_fm_terminfo
    use test_fm_term, only: collect_fm_term
    use test_fm_dispnew, only: collect_fm_dispnew
    use test_fm_xdisp, only: collect_fm_xdisp
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
                 new_testsuite("fm_character", collect_fm_character), &
                 new_testsuite("fm_buffer", collect_fm_buffer), &
                 new_testsuite("fm_insdel", collect_fm_insdel), &
                 new_testsuite("fm_marker", collect_fm_marker), &
                 new_testsuite("fm_sysdep", collect_fm_sysdep), &
                 new_testsuite("fm_editfns", collect_fm_editfns), &
                 new_testsuite("fm_cmds", collect_fm_cmds), &
                 new_testsuite("fm_values", collect_fm_values), &
                 new_testsuite("fm_textprop", collect_fm_textprop), &
                 new_testsuite("fm_keymap", collect_fm_keymap), &
                 new_testsuite("fm_keyboard", collect_fm_keyboard), &
                 new_testsuite("fm_bindings", collect_fm_bindings), &
                 new_testsuite("fm_term_xterm", collect_fm_term_xterm), &
                 new_testsuite("fm_terminfo", collect_fm_terminfo), &
                 new_testsuite("fm_term", collect_fm_term), &
                 new_testsuite("fm_dispnew", collect_fm_dispnew), &
                 new_testsuite("fm_xdisp", collect_fm_xdisp) &
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
        write (error_unit, "(i0, 1x, a)") stat, "test(s) failed!"
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
