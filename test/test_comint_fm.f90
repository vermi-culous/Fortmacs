! test_comint_fm.f90 -- tests for comint_fm.
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

!> Own tests: upstream's comint-tests.el tests password prompts, which
!> are not ported.  The processes are `cat' and `sleep' over pipes; the
!> waits are bounded by a wall-clock limit, not fixed delays.
module test_comint_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: nil
    use buffer_fm, only: buffers, cur, set_buffer, kill_buffer, buffer_live_p
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_max, goto_char, buffer_string
    use keyboard_fm, only: kbd_store_bytes, command_loop, discard_input
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    use process_fm, only: accept_process_output, get_buffer_process, process_status, delete_process, &
                          process_connection_type, process_shutdown, process_arg_type
    use comint_fm, only: make_comint, comint_send_input, comint_arguments, comint_input_ring_item, &
                         comint_input_ring_length, comint_previous_input, comint_interrupt_subjob, comint_bol
    implicit none(type, external)
    private

    public :: collect_comint_fm

    integer, parameter :: TIMEOUT = 60

contains

    subroutine collect_comint_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("comint-arguments", test_arguments), &
                    new_unittest("comint-send-input", test_send_input), &
                    new_unittest("comint-command-loop", test_command_loop), &
                    new_unittest("comint-interrupt-subjob", test_interrupt) &
                    ]
    end subroutine collect_comint_fm

    !> Wait until the current buffer contains TEXT, or its process has
    !> exited if TEXT is empty.
    subroutine wait_for(text)
        character(len=*), intent(in) :: text
        type(handle_type) :: proc
        type(status_type) :: st
        integer(int64) :: start, now, rate
        logical :: got
        call system_clock(start, rate)
        do
            if (len(text) > 0) then
                if (index(buffer_string(), text) > 0) exit
            end if
            proc = get_buffer_process(current())
            if (proc%index == 0) exit
            if (len(text) == 0) then
                if (process_status(proc) /= "run") exit
            end if
            call system_clock(now)
            if ((now - start)/max(rate, 1_int64) > TIMEOUT) exit
            call accept_process_output(got, st, proc, 1.0)
        end do
    end subroutine wait_for

    function current() result(h)
        use buffer_fm, only: current_buffer
        type(handle_type) :: h
        h = current_buffer()
    end function current

    !> End the process of buffer H and kill it.
    subroutine finish(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: proc
        type(status_type) :: st
        if (.not. buffer_live_p(h)) return
        proc = get_buffer_process(h)
        if (proc%index /= 0) call delete_process(proc, st)
        call kill_buffer(h, st)
        call process_shutdown()
    end subroutine finish

    subroutine test_arguments(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call check(error, comint_arguments("ls -l /tmp", 0, 0), "ls")
        if (.not. allocated(error)) call check(error, comint_arguments("ls -l /tmp", 1, 2), "-l /tmp")
        if (.not. allocated(error)) call check(error, comint_arguments("ls -l /tmp", -1, -1), "/tmp")
        if (.not. allocated(error)) call check(error, comint_arguments("echo 'a b' c", 1, 1), "'a b'")
        if (.not. allocated(error)) call check(error, comint_arguments("cat a\ b", 1, 1), "a\ b")
    end subroutine test_arguments

    !> RET sends the input after the process mark, which goes into the
    !> input ring; M-p brings it back.
    subroutine test_send_input(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        logical :: saved
        call init_bindings()
        saved = process_connection_type%value
        process_connection_type = .false.
        call make_comint("fm-cat", "cat", h, st)
        process_connection_type = saved
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_buffer(h, st)
        call show_alone()
        call check(error, buffers(cur)%name, "*fm-cat*")
        if (.not. allocated(error)) call check(error, buffers(cur)%major_mode, "comint_mode")
        if (allocated(error)) return
        call goto_char(point_max())
        call insert("hello", st)
        call comint_send_input(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_for("hello"//new_line("a")//"hello"//new_line("a"))
        call check(error, buffer_string(), "hello"//new_line("a")//"hello"//new_line("a"))
        if (.not. allocated(error)) call check(error, comint_input_ring_length(), 1)
        if (.not. allocated(error)) call check(error, comint_input_ring_item(0), "hello")
        if (allocated(error)) return
        call comint_previous_input(1, st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "hello"//new_line("a")//"hello"//new_line("a")// &
                                               "hello")
        call finish(h)
    end subroutine test_send_input

    !> Typing input and RET through the command loop; C-a goes to the
    !> start of the input, after the output.
    subroutine test_command_loop(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        logical :: saved
        integer(int64) :: input_start
        call init_bindings()
        saved = process_connection_type%value
        process_connection_type = .false.
        call make_comint("fm-cat-2", "cat", h, st)
        process_connection_type = saved
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_buffer(h, st)
        call show_alone()
        call discard_input()
        prefix_arg = nil()
        call kbd_store_bytes("one"//achar(13))
        call command_loop(st)
        call wait_for("one"//new_line("a")//"one"//new_line("a"))
        call check(error, buffer_string(), "one"//new_line("a")//"one"//new_line("a"))
        if (allocated(error)) return
        input_start = point_max()
        call goto_char(point_max())
        call insert("two", st)
        call comint_bol(.false.)
        call check(error, point(), input_start)
        call finish(h)
    end subroutine test_command_loop

    !> C-c C-c interrupts the process.
    subroutine test_interrupt(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, proc
        type(status_type) :: st
        type(process_arg_type) :: switches(1)
        logical :: saved
        call init_bindings()
        saved = process_connection_type%value
        process_connection_type = .false.
        switches(1)%s = "100"
        call make_comint("fm-sleep", "sleep", h, st, switches=switches)
        process_connection_type = saved
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_buffer(h, st)
        call comint_interrupt_subjob(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_for("Process fm-sleep interrupt")
        proc = get_buffer_process(h)
        if (proc%index /= 0) call check(error, process_status(proc) /= "run", "still running")
        if (.not. allocated(error)) call check(error, index(buffer_string(), "Process fm-sleep interrupt") > 0, &
                                               buffer_string())
        call finish(h)
    end subroutine test_interrupt
end module test_comint_fm
