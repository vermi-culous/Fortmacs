! test_simple_shell_fm.f90 -- tests for the shell commands of simple_fm.
! Ported from GNU Emacs 31.1 test/lisp/simple-tests.el (the shell command
! tests that apply), plus our own.
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

module test_simple_shell_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: nil
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer, get_buffer, &
                         current_buffer, buffer_live_p, set_buffer_read_only
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_max, goto_char, buffer_string, mark_position
    use keyboard_fm, only: kbd_store_bytes, command_loop, discard_input, last_message
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    use simple_fm, only: shell_command, shell_command_on_region, shell_command_dont_erase_buffer, push_mark, &
                         deactivate_mark, shell_command_buffer_name, DONT_ERASE_NIL, DONT_ERASE_ERASE, &
                         DONT_ERASE_BEG_LAST_OUT, DONT_ERASE_END_LAST_OUT, DONT_ERASE_SAVE_POINT
    use window_fm, only: get_buffer_window
    implicit none(type, external)
    private

    public :: collect_simple_shell_fm

    integer, parameter :: SAVE_POS(5) = [DONT_ERASE_ERASE, DONT_ERASE_NIL, DONT_ERASE_BEG_LAST_OUT, &
                                         DONT_ERASE_END_LAST_OUT, DONT_ERASE_SAVE_POINT]

contains

    subroutine collect_simple_shell_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("simple-tests-shell-command-39067", test_39067), &
                    new_unittest("simple-tests-shell-command-dont-erase-buffer", test_dont_erase_buffer), &
                    new_unittest("shell-command-messages", test_messages), &
                    new_unittest("shell-command-echo-area-or-buffer", test_echo_or_buffer), &
                    new_unittest("shell-command-insert-here", test_insert_here), &
                    new_unittest("shell-command-on-region-replace", test_replace), &
                    new_unittest("shell-command-error-buffer", test_error_buffer), &
                    new_unittest("simple-tests-async-shell-command-30280", test_async_30280), &
                    new_unittest("shell-command-async", test_async), &
                    new_unittest("shell-command-keys", test_keys) &
                    ]
    end subroutine collect_simple_shell_fm

    !> A fresh, current buffer `name', shown alone.
    subroutine fresh(name, h)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        call get_buffer_create(name, h, st)
        call set_buffer(h, st)
        call set_buffer_read_only(h, .false., st)
        call erase_buffer(st)
        call deactivate_mark(.true.)
        call show_alone()
    end subroutine fresh

    !> The commands of with-shell-command-dont-erase-buffer: STR is printed
    !> by the shell instead of `emacs --batch'.  Runs the command twice for
    !> each value of `shell_command_dont_erase_buffer' and each way of
    !> naming the output buffer (a buffer, its name, and "here" when it is
    !> the current buffer); `check_one' checks the output buffer.
    subroutine with_dont_erase_buffer(error, str, output_is_current, test_point)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: str
        logical, intent(in) :: output_is_current, test_point
        type(handle_type) :: caller, output
        type(status_type) :: st
        character(len=:), allocatable :: command, expected
        integer :: way, k, n, run
        integer(int64) :: expected_point
        command = "printf '%s' '"//str//"'"
        call fresh("caller-buf", caller)
        if (output_is_current) then
            output = caller
        else
            call get_buffer_create("output-buf", output, st)
        end if
        n = 2
        if (output_is_current) n = 3
        do way = 1, n
            do k = 1, size(SAVE_POS)
                shell_command_dont_erase_buffer = SAVE_POS(k)
                call set_buffer(output, st)
                call erase_buffer(st)
                call set_buffer(caller, st)
                do run = 1, 2
                    select case (way)
                    case (1)
                        call shell_command(command, st, output_buffer=output)
                    case (2)
                        call shell_command(command, st, output_buffer_name=buffers(output%index)%name)
                    case default
                        call shell_command(command, st, insert_here=.true.)
                    end select
                    call check(error, st%ok(), format_error(st))
                    if (allocated(error)) exit
                end do
                if (allocated(error)) exit
                call set_buffer(output, st)
                if (test_point) then
                    select case (SAVE_POS(k))
                    case (DONT_ERASE_BEG_LAST_OUT)
                        expected_point = 1 + len(str)
                    case (DONT_ERASE_END_LAST_OUT)
                        expected_point = 1 + 2*len(str)
                    case (DONT_ERASE_SAVE_POINT)
                        expected_point = 1
                    case default
                        expected_point = 1 + len(str)
                    end select
                    call check(error, point(), expected_point)
                else
                    if (SAVE_POS(k) == DONT_ERASE_ERASE) then
                        expected = str
                    else if (SAVE_POS(k) == DONT_ERASE_NIL) then
                        if (output_is_current) then
                            expected = str//str
                        else
                            expected = str
                        end if
                    else
                        expected = str//str
                    end if
                    call check(error, buffer_string(), expected)
                end if
                call set_buffer(caller, st)
                if (allocated(error)) exit
            end do
            if (allocated(error)) exit
        end do
        shell_command_dont_erase_buffer = DONT_ERASE_NIL
        call kill_buffer(caller, st)
        if (buffer_live_p(output)) call kill_buffer(output, st)
    end subroutine with_dont_erase_buffer

    !> The output buffer is erased or not according to
    !> `shell_command_dont_erase_buffer'.
    subroutine test_39067(error)
        type(error_type), allocatable, intent(out) :: error
        call with_dont_erase_buffer(error, "foo\n", .true., .false.)
        if (.not. allocated(error)) call with_dont_erase_buffer(error, "foo\n", .false., .false.)
    end subroutine test_39067

    !> The point is set at the expected position after execution of the
    !> command.
    subroutine test_dont_erase_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        call with_dont_erase_buffer(error, "foo\n", .false., .true.)
    end subroutine test_dont_erase_buffer

    !> The messages about commands with no output.
    subroutine test_messages(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh("*shell-test*", h)
        last_message = ""
        call shell_command("true", st)
        call check(error, last_message, "(Shell command succeeded with no output)")
        if (allocated(error)) return
        call shell_command("exit 3", st)
        call check(error, last_message, "(Shell command failed with code 3 and no output)")
        if (allocated(error)) return
        call shell_command("kill -TERM $$", st)
        call check(error, last_message, "(Shell command killed by signal Terminated)")
        call kill_buffer(h, st)
    end subroutine test_messages

    !> One line of output goes to the echo area (and stays in *Shell
    !> Command Output*); more is shown in that buffer's window.
    subroutine test_echo_or_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, out, win
        type(status_type) :: st
        call fresh("*shell-test*", h)
        last_message = ""
        call shell_command("echo hello", st)
        call check(error, last_message, "hello")
        if (allocated(error)) return
        out = get_buffer(shell_command_buffer_name)
        win = get_buffer_window(out)
        call check(error, win%index == 0, "not displayed")
        if (allocated(error)) return
        call set_buffer(out, st)
        call check(error, buffer_string(), "hello"//new_line("a"))
        if (allocated(error)) return
        call set_buffer(h, st)
        last_message = ""
        call shell_command("echo one; echo two", st)
        call check(error, last_message, "")
        if (allocated(error)) return
        win = get_buffer_window(out)
        call check(error, win%index /= 0, "displayed")
        if (allocated(error)) return
        call set_buffer(out, st)
        call check(error, buffer_string(), "one"//new_line("a")//"two"//new_line("a"))
        call kill_buffer(out, st)
        call kill_buffer(h, st)
    end subroutine test_echo_or_buffer

    !> With a prefix argument the output goes at point, mark after it,
    !> point before it.
    subroutine test_insert_here(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh("*shell-test*", h)
        call insert("ab", st)
        call goto_char(2_int64)
        call shell_command("printf XY", st, insert_here=.true.)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "aXYb")
        if (.not. allocated(error)) call check(error, point(), 2_int64)
        if (.not. allocated(error)) call check(error, mark_position(), 4_int64)
        call kill_buffer(h, st)
    end subroutine test_insert_here

    !> C-u M-|: the output replaces the region, point and mark around it.
    subroutine test_replace(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer :: code
        call fresh("*shell-test*", h)
        call insert("c"//new_line("a")//"a"//new_line("a")//"b"//new_line("a")//"tail", st)
        call shell_command_on_region(1_int64, 7_int64, "sort", st, replace=.true., insert_here=.true., &
                                     exit_status=code)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, code, 0)
        if (.not. allocated(error)) then
            call check(error, buffer_string(), "a"//new_line("a")//"b"//new_line("a")//"c"//new_line("a")//"tail")
        end if
        if (.not. allocated(error)) call check(error, point(), 1_int64)
        if (.not. allocated(error)) call check(error, mark_position(), 7_int64)
        call kill_buffer(h, st)
    end subroutine test_replace

    !> Standard error goes to the error buffer; the message says so.
    subroutine test_error_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, eb
        type(status_type) :: st
        call fresh("*shell-test*", h)
        last_message = ""
        call shell_command_on_region(1_int64, 1_int64, "echo oops >&2", st, error_buffer="*Shell Errors*")
        call check(error, last_message, "(Shell command succeeded with some error output)")
        if (allocated(error)) return
        eb = get_buffer("*Shell Errors*")
        call check(error, buffer_live_p(eb), "error buffer")
        if (allocated(error)) return
        call set_buffer(eb, st)
        call check(error, buffer_string(), "oops"//new_line("a"))
        if (allocated(error)) return
        ! Again: a page separator.
        call set_buffer(h, st)
        call shell_command_on_region(1_int64, 1_int64, "echo again >&2", st, error_buffer="*Shell Errors*")
        call set_buffer(eb, st)
        call check(error, buffer_string(), "oops"//new_line("a")//achar(12)//new_line("a")//"again"//new_line("a"))
        call kill_buffer(eb, st)
        call kill_buffer(h, st)
    end subroutine test_error_buffer

    !> The size of BUFFER.
    integer(int64) function size_of(buffer)
        use editfns_fm, only: buffer_size_of
        type(handle_type), intent(in) :: buffer
        type(status_type) :: st
        call buffer_size_of(buffer, size_of, st)
    end function size_of

    !> Wait (at most a minute) until the process of BUFFER has exited.
    subroutine wait_process(buffer)
        use process_fm, only: accept_process_output, get_buffer_process
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: proc
        type(status_type) :: st
        integer(int64) :: start, now, rate
        logical :: got
        call system_clock(start, rate)
        do
            if (.not. buffer_live_p(buffer)) exit
            proc = get_buffer_process(buffer)
            if (proc%index == 0) exit
            call system_clock(now)
            if ((now - start)/max(rate, 1_int64) > 60) exit
            call accept_process_output(got, st, proc, 1.0)
        end do
    end subroutine wait_process

    !> Port of simple-tests.el simple-tests-async-shell-command-30280 (sh
    !> commands instead of an Emacs in batch mode): with ASYNC_NEW_BUFFER,
    !> a second command gets a new buffer.
    subroutine test_async_30280(error)
        use simple_fm, only: async_shell_command, async_shell_command_buffer, async_shell_command_display_buffer, &
                             ASYNC_NEW_BUFFER, ASYNC_CONFIRM_NEW_BUFFER
        use buffer_fm, only: generate_new_buffer_name
        use process_fm, only: get_buffer_process, delete_process, accept_process_output
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buffer, proc, h, win
        type(status_type) :: st
        character(len=:), allocatable :: first, second
        integer(int64) :: start, now, rate
        logical :: got
        integer :: k
        call init_bindings()
        call fresh("*shell-test*", h)
        async_shell_command_buffer = ASYNC_NEW_BUFFER
        async_shell_command_display_buffer = .false.
        first = generate_new_buffer_name("name")
        ! One command has no output, the other does.
        call async_shell_command("sleep 8", st, output_buffer_name=first)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        second = generate_new_buffer_name("name")
        call async_shell_command("echo hello", st, output_buffer_name=first)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! First command should neither have nor display output.
        buffer = get_buffer(first)
        call check(error, buffer_live_p(buffer), "first buffer")
        if (allocated(error)) return
        proc = get_buffer_process(buffer)
        call check(error, proc%index /= 0, "first process")
        if (.not. allocated(error)) call check(error, size_of(buffer) == 0, "first output")
        win = get_buffer_window(buffer)
        if (.not. allocated(error)) call check(error, win%index == 0, "first window")
        if (allocated(error)) return
        ! Second command should both have and display output.
        buffer = get_buffer(second)
        call check(error, buffer_live_p(buffer), "second buffer "//second)
        if (allocated(error)) return
        call system_clock(start, rate)
        do
            if (size_of(buffer) > 0) exit
            proc = get_buffer_process(buffer)
            if (proc%index == 0) exit
            call system_clock(now)
            if ((now - start)/max(rate, 1_int64) > 60) exit
            call accept_process_output(got, st, proc, 1.0)
        end do
        call check(error, size_of(buffer) > 0, "second output")
        win = get_buffer_window(buffer)
        if (.not. allocated(error)) call check(error, win%index /= 0, "second window")
        do k = 1, 2
            if (k == 1) buffer = get_buffer(first)
            if (k == 2) buffer = get_buffer(second)
            if (.not. buffer_live_p(buffer)) cycle
            proc = get_buffer_process(buffer)
            if (proc%index /= 0) call delete_process(proc, st)
            call kill_buffer(buffer, st)
        end do
        async_shell_command_buffer = ASYNC_CONFIRM_NEW_BUFFER
        async_shell_command_display_buffer = .true.
        call kill_buffer(h, st)
    end subroutine test_async_30280

    !> A trailing & runs the command asynchronously in *Async Shell
    !> Command*, in Shell Command mode, with "Shell" as its process.
    subroutine test_async(error)
        use process_fm, only: get_buffer_process
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer
        type(status_type) :: st
        character(len=:), allocatable :: text
        call init_bindings()
        call fresh("*shell-test*", h)
        last_message = ""
        call shell_command("echo async &", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        buffer = get_buffer("*Async Shell Command*")
        call check(error, buffer_live_p(buffer), "output buffer")
        if (allocated(error)) return
        call wait_process(buffer)
        call set_buffer(buffer, st)
        call check(error, buffers(cur)%major_mode, "shell_command_mode")
        text = buffer_string()
        if (.not. allocated(error)) call check(error, index(text, "async") == 1, text)
        if (.not. allocated(error)) call check(error, last_message, "echo async: finished.")
        call kill_buffer(buffer, st)
        call kill_buffer(h, st)
    end subroutine test_async

    !> M-! and C-u M-| through the command loop.
    subroutine test_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call fresh("*shell-test*", h)
        call discard_input()
        prefix_arg = nil()
        last_message = ""
        call kbd_store_bytes(achar(27)//"!echo hi"//achar(13))
        call command_loop(st)
        call check(error, last_message, "hi")
        if (allocated(error)) return
        call insert("b"//new_line("a")//"a"//new_line("a"), st)
        call push_mark(1_int64, .true., .true.)
        call kbd_store_bytes(achar(21)//achar(27)//"|sort"//achar(13))
        call command_loop(st)
        call check(error, buffer_string(), "a"//new_line("a")//"b"//new_line("a"))
        call kill_buffer(h, st)
    end subroutine test_keys
end module test_simple_shell_fm
