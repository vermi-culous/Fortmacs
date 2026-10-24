! test_shell_fm.f90 -- tests for shell_fm.
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

!> Ports of shell-tests.el (completion is not ported, so neither are the
!> completion tests; shell-tests-unquote-1 tests the unquoting through
!> `shell_unquote_argument').
module test_shell_fm
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, generate_new_buffer_name
    use bindings_fm, only: init_bindings
    use modes_fm, only: minor_modes
    use process_fm, only: process_arg_type
    use shell_fm, only: shell_mode, split_string_shell_command, shell_unquote_argument, shell_cd, &
                        shell_directory_tracker, list_buffers_directory
    implicit none(type, external)
    private

    public :: collect_shell_fm

contains

    subroutine collect_shell_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("shell-tests-unquote-1", test_unquote_1), &
                    new_unittest("shell-tests-split-string", test_split_string), &
                    new_unittest("shell-dirtrack-on-by-default", test_dirtrack_on), &
                    new_unittest("shell-dirtrack-should-not-be-on-in-unrelated-modes", test_dirtrack_unrelated), &
                    new_unittest("shell-dirtrack-sets-list-buffers-directory", test_sets_list_buffers_directory), &
                    new_unittest("shell-directory-tracker-cd", test_tracker_cd) &
                    ]
    end subroutine collect_shell_fm

    !> A temporary buffer in DIR (with-temp-buffer).
    subroutine temp_buffer(h, dir)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: dir
        type(status_type) :: st
        call get_buffer_create(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        buffers(cur)%directory = dir
    end subroutine temp_buffer

    subroutine test_unquote_1(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call check(error, shell_unquote_argument("te'st"), "test")
    end subroutine test_unquote_1

    !> SPLIT gives the arguments EXPECTED (separated by `|').
    subroutine split(error, string, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: string, expected
        type(process_arg_type), allocatable :: args(:)
        character(len=:), allocatable :: got
        integer :: k
        call split_string_shell_command(string, args)
        got = ""
        do k = 1, size(args)
            if (k > 1) got = got//"|"
            got = got//args(k)%s
        end do
        call check(error, got, expected, string)
    end subroutine split

    subroutine test_split_string(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call split(error, "ls /tmp", "ls|/tmp")
        if (.not. allocated(error)) call split(error, "ls '/tmp/foo bar'", "ls|/tmp/foo bar")
        if (.not. allocated(error)) call split(error, "ls ""/tmp/foo bar""", "ls|/tmp/foo bar")
        if (.not. allocated(error)) call split(error, "ls /tmp/'foo bar'", "ls|/tmp/foo bar")
        if (.not. allocated(error)) call split(error, "ls /tmp/'foo""bar'", "ls|/tmp/foo""bar")
        if (.not. allocated(error)) call split(error, "ls /tmp/""foo''bar""", "ls|/tmp/foo''bar")
        if (.not. allocated(error)) call split(error, "ls /tmp/'foo\ bar'", "ls|/tmp/foo\ bar")
        if (.not. allocated(error)) call split(error, "ls /tmp/foo\ bar", "ls|/tmp/foo bar")
    end subroutine test_split_string

    subroutine test_dirtrack_on(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call temp_buffer(h, "/tmp/")
        call shell_mode(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, minor_modes%shell_dirtrack_mode%enabled(), "dirtrack")
        call kill_buffer(h, st)
    end subroutine test_dirtrack_on

    subroutine test_dirtrack_unrelated(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call temp_buffer(h, "/tmp/")
        call check(error,.not. minor_modes%shell_dirtrack_mode%enabled(), "dirtrack")
        call kill_buffer(h, st)
    end subroutine test_dirtrack_unrelated

    subroutine test_sets_list_buffers_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: START_DIR = "/usr/bin/"
        call init_bindings()
        call temp_buffer(h, START_DIR)
        call check(error,.not. allocated(list_buffers_directory%value), "should-not")
        if (allocated(error)) return
        call shell_mode(st)
        call shell_cd("..", st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, allocated(list_buffers_directory%value), "set")
        if (.not. allocated(error)) call check(error, list_buffers_directory%value /= START_DIR, "changed")
        if (.not. allocated(error)) then
            call check(error, index(START_DIR, list_buffers_directory%value) == 1, list_buffers_directory%value)
        end if
        call kill_buffer(h, st)
    end subroutine test_sets_list_buffers_directory

    subroutine test_tracker_cd(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: START_DIR = "/usr/bin/"
        call init_bindings()
        call temp_buffer(h, START_DIR)
        call check(error,.not. allocated(list_buffers_directory%value), "should-not")
        if (allocated(error)) return
        call shell_mode(st)
        call shell_directory_tracker("cd ..", st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, allocated(list_buffers_directory%value), "set")
        if (.not. allocated(error)) call check(error, list_buffers_directory%value /= START_DIR, "changed")
        if (.not. allocated(error)) then
            call check(error, index(START_DIR, list_buffers_directory%value) == 1, list_buffers_directory%value)
        end if
        call kill_buffer(h, st)
    end subroutine test_tracker_cd
end module test_shell_fm
