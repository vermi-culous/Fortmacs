! test_timer_fm.f90 -- tests for timers (timer_fm) and their use by the
! command loop (keyboard_fm).
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

!> Own tests (upstream has no timer tests that need no Lisp).  Timers
!> fire while the command loop waits for terminal input, read from a
!> pty: a timer function writes a byte to the pty, which ends the wait.
!> Every wait is bounded: a fallback timer a few seconds later writes
!> too, and the tests check that the wait ended for the right reason.
module test_timer_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, fixnump, xfixnum
    use timer_fm, only: run_with_idle_timer, run_with_timer, run_at_time, cancel_timer, free_timer, timerp, &
                        timer_check, timer_start_idle, timer_stop_idle, timer_active_p, timer_triggered_p, &
                        cancel_function_timers, current_idle_time
    use keyboard_fm, only: read_event, set_input_fd, sit_for, discard_input, last_message
    use sysdep_fm, only: emacs_tty_type, open_pty, close_fd, init_sys_modes, reset_sys_modes, emacs_write
    implicit none(type, external)
    private

    public :: collect_timer_fm

    !> The pty's master side, which timer functions write to.
    integer(int32) :: master = -1, slave_fd = -1
    !> Calls of the counting timer function.
    integer :: calls = 0

contains

    subroutine collect_timer_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("timer-idle-fires-while-waiting", test_idle_fires), &
                    new_unittest("timer-idle-repeat-once-per-idle-period", test_idle_repeat), &
                    new_unittest("timer-ordinary-during-sit-for", test_sit_for), &
                    new_unittest("timer-cancel-and-free", test_cancel), &
                    new_unittest("timer-error-message", test_error), &
                    new_unittest("timer-invalid-repeat", test_invalid_repeat) &
                    ]
    end subroutine collect_timer_fm

    subroutine write_x(status)
        type(status_type), intent(out) :: status
        call emacs_write(master, "x", status)
    end subroutine write_x

    subroutine write_y(status)
        type(status_type), intent(out) :: status
        call emacs_write(master, "y", status)
    end subroutine write_y

    subroutine write_z(status)
        type(status_type), intent(out) :: status
        call emacs_write(master, "z", status)
    end subroutine write_z

    subroutine count_call(status)
        type(status_type), intent(out) :: status
        status = status_type()
        calls = calls + 1
    end subroutine count_call

    subroutine fail_call(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        call emacs_write(master, "x", st)
        call set_error(status, FM_GENERIC_ERROR, "boom")
    end subroutine fail_call

    !> Set up a raw pty as the terminal.
    subroutine pty_open(old, ok)
        type(emacs_tty_type), intent(out) :: old
        logical, intent(out) :: ok
        integer(int32) :: slave
        type(status_type) :: st
        ok = .false.
        call discard_input()
        call open_pty(master, slave, st)
        if (.not. st%ok()) return
        call init_sys_modes(slave, old, st)
        if (.not. st%ok()) return
        call set_input_fd(slave)
        slave_fd = slave
        ok = .true.
    end subroutine pty_open

    subroutine pty_close(old)
        type(emacs_tty_type), intent(in) :: old
        type(status_type) :: st
        call reset_sys_modes(slave_fd, old, st)
        call set_input_fd(-1_int32)
        call close_fd(slave_fd)
        call close_fd(master)
        master = -1
        call discard_input()
    end subroutine pty_close

    !> The next character read from the terminal, or -1.
    integer function next_char()
        type(value_type) :: ev
        type(status_type) :: st
        logical :: have
        next_char = -1
        call read_event(ev, have, st)
        if (have .and. fixnump(ev)) next_char = int(xfixnum(ev))
    end function next_char

    !> An idle timer runs once Emacs has been idle long enough while
    !> waiting for input, and the wait ends when it is due (no busy
    !> waiting: the wait's timeout is the time to the timer).
    subroutine test_idle_fires(error)
        type(error_type), allocatable, intent(out) :: error
        type(emacs_tty_type) :: old
        type(handle_type) :: idle, fallback
        type(status_type) :: st
        integer(int32) :: ms
        logical :: ok, idle_p
        real(real64) :: secs
        call pty_open(old, ok)
        call check(error, ok, "pty")
        if (allocated(error)) return
        call run_with_idle_timer(idle, 0.05_real64, .false., write_x, st)
        call check(error, st%ok() .and. timer_active_p(idle), "run_with_idle_timer")
        if (allocated(error)) return
        ! Not idle: idle timers are not considered.
        call check(error, timer_check() == -1, "no timer before idle")
        if (allocated(error)) return
        call timer_start_idle()
        ms = timer_check()
        call current_idle_time(idle_p, secs)
        call check(error, ms >= 1 .and. ms <= 50 .and. idle_p, "wait for the idle timer")
        call timer_stop_idle()
        if (allocated(error)) return
        call run_with_timer(fallback, 5.0_real64, write_z, st)
        call check(error, next_char() == iachar("x"), "the idle timer ended the wait")
        if (allocated(error)) return
        call check(error, timer_triggered_p(idle) .and. .not. timer_active_p(idle), "a one-shot timer is done")
        if (allocated(error)) return
        call current_idle_time(idle_p, secs)
        call check(error,.not. idle_p, "not idle after input")
        call free_timer(fallback)
        call free_timer(idle)
        call pty_close(old)
    end subroutine test_idle_fires

    !> A repeated idle timer runs once in each idle period.
    subroutine test_idle_repeat(error)
        type(error_type), allocatable, intent(out) :: error
        type(emacs_tty_type) :: old
        type(handle_type) :: idle, ordinary, fallback
        type(status_type) :: st
        logical :: ok
        call pty_open(old, ok)
        call check(error, ok, "pty")
        if (allocated(error)) return
        calls = 0
        call run_with_idle_timer(idle, 0.01_real64, .true., count_call, st)
        call run_with_timer(ordinary, 0.15_real64, write_y, st)
        call run_with_timer(fallback, 5.0_real64, write_z, st)
        call check(error, next_char() == iachar("y"), "first wait")
        if (allocated(error)) return
        call check(error, calls == 1 .and. timer_active_p(idle), "once in the first idle period")
        if (allocated(error)) return
        call free_timer(ordinary)
        call run_with_timer(ordinary, 0.1_real64, write_y, st)
        call check(error, next_char() == iachar("y"), "second wait")
        if (allocated(error)) return
        call check(error, calls == 2, "once more in the second idle period")
        call free_timer(idle)
        call free_timer(ordinary)
        call free_timer(fallback)
        call pty_close(old)
    end subroutine test_idle_repeat

    !> Ordinary timers run while `sit_for' waits; idle timers don't
    !> (Emacs is not idle during a command).
    subroutine test_sit_for(error)
        type(error_type), allocatable, intent(out) :: error
        type(emacs_tty_type) :: old
        type(handle_type) :: ordinary, idle
        type(status_type) :: st
        logical :: ok
        call pty_open(old, ok)
        call check(error, ok, "pty")
        if (allocated(error)) return
        calls = 0
        call run_with_timer(ordinary, 0.02_real64, count_call, st)
        call run_with_idle_timer(idle, 0.01_real64, .false., count_call, st)
        call sit_for(150_int32)
        call check(error, calls == 1 .and. timer_triggered_p(ordinary) .and. timer_active_p(idle), &
                   "the ordinary timer ran, the idle one waits")
        call free_timer(ordinary)
        call free_timer(idle)
        call pty_close(old)
    end subroutine test_sit_for

    !> cancel_timer takes a timer off its list; free_timer releases it;
    !> cancel_function_timers cancels by function.
    subroutine test_cancel(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        call run_with_timer(a, 100.0_real64, count_call, st)
        call check(error, timer_active_p(a) .and. timer_check() > 0, "active")
        if (allocated(error)) return
        call cancel_timer(a)
        call check(error,.not. timer_active_p(a) .and. timerp(a), "cancelled")
        if (allocated(error)) return
        call free_timer(a)
        call check(error,.not. timerp(a), "freed")
        if (allocated(error)) return
        call run_with_timer(a, 100.0_real64, count_call, st)
        call run_with_idle_timer(b, 100.0_real64, .true., count_call, st)
        call cancel_function_timers(count_call)
        call check(error,.not. timer_active_p(a) .and. .not. timer_active_p(b), "cancel_function_timers")
        if (allocated(error)) return
        call check(error, timer_check() == -1, "no timer left")
        call free_timer(a)
        call free_timer(b)
    end subroutine test_cancel

    !> An error in a timer function is reported in the echo area
    !> (timer-event-handler's message) and the command loop goes on.
    subroutine test_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(emacs_tty_type) :: old
        type(handle_type) :: a, fallback
        type(status_type) :: st
        logical :: ok
        call pty_open(old, ok)
        call check(error, ok, "pty")
        if (allocated(error)) return
        call run_with_timer(a, 0.01_real64, fail_call, st)
        call run_with_timer(fallback, 5.0_real64, write_z, st)
        call check(error, next_char() == iachar("x"), "the failing timer ran")
        if (allocated(error)) return
        call check(error, last_message, "Error running timer: boom")
        call free_timer(a)
        call free_timer(fallback)
        call pty_close(old)
    end subroutine test_error

    !> A negative REPEAT is an error.
    subroutine test_invalid_repeat(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a
        type(status_type) :: st
        call run_at_time(a, 1.0_real64, count_call, st, repeat=-1.0_real64)
        call check(error,.not. st%ok() .and. .not. timerp(a), "Invalid repetition interval")
    end subroutine test_invalid_repeat
end module test_timer_fm
