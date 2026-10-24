! timer_fm.f90 -- run a function at some time in the future.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/timer.el.
!
! Copyright (C) 1996, 2001-2026 Free Software Foundation, Inc.
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

!> Timers.  A timer is a registry object referenced by a handle (the
!> vector timer.el's cl-defstruct makes upstream): whether it is
!> triggered (inactive), when it fires, how it repeats and the procedure
!> it calls.  Ordinary timers are on `timer_list', idle timers on
!> `timer_idle_list', each sorted by time.  Times are counts of the
!> processor clock (`system_clock'): absolute for ordinary timers,
!> relative to the start of the idle period for idle timers.
!>
!> keyboard.c runs them: it records when Emacs becomes idle
!> (`timer_start_idle', `timer_stop_idle', `timer_resume_idle') and,
!> while waiting for input, `timer_check' runs every ripe timer and says
!> how long to wait for the next one.  Those are here too, so that the
!> command loop (keyboard_fm) and `sit_for' only call them.
!>
!> No garbage collector frees timers: `free_timer' cancels a timer and
!> releases its handle.
!>
!> Deferred: timer functions with arguments (a timer calls a procedure
!> of no arguments), run_at_time's string times ("11:23pm", "2 hours
!> 35 minutes"), encoded times and t (integral multiples of REPEAT,
!> timer-next-integral-multiple-of-time), `with_timeout',
!> `y_or_n_p_with_timeout', `timer_duration', `add_timeout' (obsolete),
!> keyboard.c's pending_funcalls, the timer-event-last debugging
!> variables, and timers' interactive commands (`cancel_function_timers'
!> is a function only).  The clock is monotonic, where upstream's
!> current-time is the wall clock.
module timer_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type, slot_table_type, null_handle
    use lisp_fm, only: command_proc
    use buffer_fm, only: cur, set_buffer_internal, buffer_live_p, buffer_handle, vdeactivate_mark
    implicit none(type, external)
    private

    public :: timer_create, free_timer, timerp, timer_set_time, timer_set_idle_time, timer_inc_time, &
              timer_set_function, timer_activate, timer_activate_when_idle, cancel_timer, cancel_function_timers, &
              timer_event_handler, run_at_time, run_with_timer, run_with_idle_timer, internal_timer_start_idle, &
              timer_start_idle, timer_stop_idle, timer_resume_idle, timer_check, current_idle_time, &
              timer_triggered_p, timer_active_p, timer_idle_p

    !> Maximum number of times to repeat a timer, if many repeats are delayed.
    !> Timer invocations can be delayed because Emacs is suspended or busy,
    !> or because the system's time changes.  If such an occurrence makes it
    !> appear that many invocations are overdue, this variable controls
    !> how many will really happen.  Port of timer.el timer-max-repeats.
    integer, public :: timer_max_repeats = 10

    !> keyboard.c:timers_run, the number of timers run so far (the command
    !> loop redisplays when it changes while waiting).
    integer(int64), public, protected :: timers_run = 0

    !> The message of the last error a timer function signaled ("Error
    !> running timer: ..."), for the caller of `timer_check' to show;
    !> unallocated for none.  Port of the message of
    !> timer.el:timer-event-handler (timer_fm sits below the echo area).
    character(len=:), allocatable, public :: timer_error_message

    !> One timer (timer.el's cl-defstruct timer).
    type :: timer_type
        !> False if the timer is active (waiting to be triggered), true
        !> if it is inactive ("already triggered", in theory).
        logical :: triggered = .true.
        !> Time of next trigger: for normal timers, absolute time, for
        !> idle timers, time relative to idle start; in clock counts.
        !> `time_set' is false until a time is given (timer--activate's
        !> integerp checks).
        integer(int64) :: time = 0
        logical :: time_set = .false.
        !> For normal timers, time between repetitions in seconds, or
        !> none (`repeat' false).  For idle timers, `repeat' says whether
        !> it is repeated.
        logical :: repeat = .false.
        real(real64) :: repeat_delay = 0
        !> What to do when triggered.
        procedure(command_proc), pointer, nopass :: fn => null()
        !> This is an idle timer.
        logical :: idle = .false.
    end type timer_type

    type(slot_table_type) :: table
    type(timer_type), allocatable :: timers(:)

    !> keyboard.c Vtimer_list and Vtimer_idle_list: the active timers,
    !> each list sorted by time.
    type(handle_type), allocatable :: timer_list(:), timer_idle_list(:)

    !> keyboard.c timer_idleness_start_time (valid while `idle') and
    !> timer_last_idleness_start_time, in clock counts.
    logical :: idle = .false.
    integer(int64) :: idleness_start = 0, last_idleness_start = 0

contains

    ! ------------------------------------------------------------------
    ! The clock
    ! ------------------------------------------------------------------

    !> The current time in clock counts (upstream's current-time).
    integer(int64) function now()
        call system_clock(now)
    end function now

    !> Clock counts per second.
    integer(int64) function clock_rate()
        integer(int64) :: count
        call system_clock(count, clock_rate)
        clock_rate = max(clock_rate, 1_int64)
    end function clock_rate

    !> SECS seconds in clock counts.
    integer(int64) function counts(secs)
        real(real64), intent(in) :: secs
        counts = nint(secs*real(clock_rate(), real64), int64)
    end function counts

    ! ------------------------------------------------------------------
    ! Timer objects
    ! ------------------------------------------------------------------

    !> Make a new timer, inactive and without a time or function.  Port
    !> of timer.el:timer-create.
    subroutine timer_create(timer)
        type(handle_type), intent(out) :: timer
        type(timer_type), allocatable :: grown(:)
        call table%acquire(timer)
        if (.not. allocated(timers)) then
            allocate (timers(table%capacity()))
        else if (size(timers, kind=int64) < table%capacity()) then
            allocate (grown(table%capacity()))
            grown(1:size(timers)) = timers
            call move_alloc(grown, timers)
        end if
        timers(timer%index) = timer_type()
    end subroutine timer_create

    !> Cancel TIMER and release its handle, which becomes stale.  Not a
    !> port: upstream relies on GC.
    subroutine free_timer(timer)
        type(handle_type), intent(in) :: timer
        type(status_type) :: st
        if (.not. timerp(timer)) return
        call cancel_timer(timer)
        timers(timer%index)%fn => null()
        call table%release(timer, st)
    end subroutine free_timer

    !> Return true if TIMER is a timer.  Port of timer.el:timerp (a live
    !> timer handle).
    logical function timerp(timer)
        type(handle_type), intent(in) :: timer
        timerp = table%is_live(timer)
    end function timerp

    !> Signal wrong-type-argument unless TIMER is a timer.  Port of
    !> timer.el:timer--check.
    subroutine timer_check_arg(timer, status)
        type(handle_type), intent(in) :: timer
        type(status_type), intent(out) :: status
        if (.not. timerp(timer)) call set_error(status, FM_WRONG_TYPE_ARGUMENT, "timerp")
    end subroutine timer_check_arg

    !> Whether TIMER is inactive (triggered): not waiting to be run.
    logical function timer_triggered_p(timer) result(p)
        type(handle_type), intent(in) :: timer
        p = .true.
        if (timerp(timer)) p = timers(timer%index)%triggered
    end function timer_triggered_p

    !> Whether TIMER is on `timer_list' or `timer_idle_list'.
    logical function timer_active_p(timer) result(p)
        type(handle_type), intent(in) :: timer
        p = position_in(timer_list, timer) > 0 .or. position_in(timer_idle_list, timer) > 0
    end function timer_active_p

    !> Whether TIMER is an idle timer (its idle-delay field).
    logical function timer_idle_p(timer) result(p)
        type(handle_type), intent(in) :: timer
        p = .false.
        if (timerp(timer)) p = timers(timer%index)%idle
    end function timer_idle_p

    !> Set the trigger time of TIMER to TIME seconds from now.
    !> If optional third argument DELTA is a positive number, make the timer
    !> fire repeatedly that many seconds apart.  Port of
    !> timer.el:timer-set-time (TIME is relative, in seconds; upstream it
    !> is a Lisp time value).
    subroutine timer_set_time(timer, time, status, delta)
        type(handle_type), intent(in) :: timer
        real(real64), intent(in) :: time
        type(status_type), intent(out) :: status
        real(real64), intent(in), optional :: delta
        call timer_check_arg(timer, status)
        if (.not. status%ok()) return
        associate (t => timers(timer%index))
            t%time = now() + counts(time)
            t%time_set = .true.
            t%repeat = .false.
            t%repeat_delay = 0
            if (present(delta)) then
                if (delta > 0) then
                    t%repeat = .true.
                    t%repeat_delay = delta
                end if
            end if
        end associate
    end subroutine timer_set_time

    !> Set the trigger idle time of TIMER to SECS.
    !> SECS may be an integer, floating point number, or a value in
    !> the internal time format returned by, e.g., `current_idle_time'.
    !> If optional third argument REPEAT is true, make the timer
    !> fire each time Emacs is idle for that many seconds.  Port of
    !> timer.el:timer-set-idle-time (SECS is a number of seconds).
    subroutine timer_set_idle_time(timer, secs, status, repeat)
        type(handle_type), intent(in) :: timer
        real(real64), intent(in) :: secs
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: repeat
        call timer_check_arg(timer, status)
        if (.not. status%ok()) return
        associate (t => timers(timer%index))
            t%time = counts(secs)
            t%time_set = .true.
            t%repeat = .false.
            if (present(repeat)) t%repeat = repeat
        end associate
    end subroutine timer_set_idle_time

    !> Increment the time set in TIMER by SECS seconds.
    !>
    !> Optionally also increment it by USECS microseconds, and PSECS
    !> picoseconds.  If USECS or PSECS are omitted, they are treated as
    !> zero.
    !>
    !> SECS may be a fraction.  Port of timer.el:timer-inc-time (no USECS
    !> or PSECS: SECS is a real number).
    subroutine timer_inc_time(timer, secs)
        type(handle_type), intent(in) :: timer
        real(real64), intent(in) :: secs
        if (.not. timerp(timer)) return
        timers(timer%index)%time = timers(timer%index)%time + counts(secs)
    end subroutine timer_inc_time

    !> Make TIMER call FUNCTION with optional ARGS when triggering.  Port
    !> of timer.el:timer-set-function (no ARGS).
    subroutine timer_set_function(timer, function, status)
        type(handle_type), intent(in) :: timer
        procedure(command_proc) :: function
        type(status_type), intent(out) :: status
        call timer_check_arg(timer, status)
        if (.not. status%ok()) return
        timers(timer%index)%fn => function
    end subroutine timer_set_function

    !> The position of TIMER in LIST, or 0.
    integer function position_in(list, timer) result(k)
        type(handle_type), allocatable, intent(in) :: list(:)
        type(handle_type), intent(in) :: timer
        integer :: i
        k = 0
        if (.not. allocated(list)) return
        do i = 1, size(list)
            if (list(i)%index == timer%index .and. list(i)%generation == timer%generation) then
                k = i
                return
            end if
        end do
    end function position_in

    !> Insert TIMER into the timer list (the idle list if IDLE), after
    !> the timers that trigger before it.  Port of timer.el:timer--activate.
    subroutine timer_activate_1(timer, triggered_p, idle_timer, status)
        type(handle_type), intent(in) :: timer
        logical, intent(in) :: triggered_p, idle_timer
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: list(:)
        integer :: k, n
        status = status_type()
        if (.not. timerp(timer)) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid or uninitialized timer")
            return
        end if
        if (.not. timers(timer%index)%time_set .or. .not. associated(timers(timer%index)%fn)) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid or uninitialized timer")
            return
        end if
        if (idle_timer) then
            if (allocated(timer_idle_list)) call move_alloc(timer_idle_list, list)
        else
            if (allocated(timer_list)) call move_alloc(timer_list, list)
        end if
        if (.not. allocated(list)) allocate (list(0))
        if (position_in(list, timer) > 0) then
            call set_error(status, FM_GENERIC_ERROR, "Timer already activated")
        else
            ! Skip all timers to trigger before the new one.
            n = size(list)
            k = 1
            do while (k <= n)
                if (.not. timer_time_less_p(list(k), timer)) exit
                k = k + 1
            end do
            list = [list(1:k - 1), timer, list(k:n)]
            timers(timer%index)%triggered = triggered_p
            timers(timer%index)%idle = idle_timer
        end if
        if (idle_timer) then
            call move_alloc(list, timer_idle_list)
        else
            call move_alloc(list, timer_list)
        end if
    end subroutine timer_activate_1

    !> Say whether time value T1 is less than time value T2.  Port of
    !> timer.el:timer--time-less-p.
    logical function timer_time_less_p(t1, t2)
        type(handle_type), intent(in) :: t1, t2
        timer_time_less_p = .false.
        if (.not. timerp(t1) .or. .not. timerp(t2)) return
        timer_time_less_p = timers(t1%index)%time < timers(t2%index)%time
    end function timer_time_less_p

    !> Insert TIMER into `timer_list'.
    !> If TRIGGERED_P is true, make TIMER inactive (put it on the list, but
    !> mark it as already triggered).  To remove it, use `cancel_timer'.
    !> Port of timer.el:timer-activate (no REUSE-CELL).
    subroutine timer_activate(timer, status, triggered_p)
        type(handle_type), intent(in) :: timer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: triggered_p
        logical :: triggered
        triggered = .false.
        if (present(triggered_p)) triggered = triggered_p
        call timer_activate_1(timer, triggered, .false., status)
    end subroutine timer_activate

    !> Insert TIMER into `timer_idle_list'.
    !> This arranges to activate TIMER whenever Emacs is next idle.
    !> If optional argument DONT_WAIT is true, set TIMER to activate
    !> immediately (see below), or at the right time, if Emacs is
    !> already idle.
    !>
    !> Using true DONT_WAIT is not recommended when activating an
    !> idle timer from an idle timer handler, if the timer being
    !> activated has an idleness time that is smaller or equal to
    !> the time of the current timer.  That's because the activated
    !> timer will fire right away.  Port of
    !> timer.el:timer-activate-when-idle (no REUSE-CELL).
    subroutine timer_activate_when_idle(timer, status, dont_wait)
        type(handle_type), intent(in) :: timer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: dont_wait
        logical :: triggered
        triggered = .true.
        if (present(dont_wait)) triggered = .not. dont_wait
        call timer_activate_1(timer, triggered, .true., status)
    end subroutine timer_activate_when_idle

    !> Remove TIMER from the list of active timers.  Port of
    !> timer.el:cancel-timer.
    subroutine cancel_timer(timer)
        type(handle_type), intent(in) :: timer
        logical :: was
        call cancel_timer_internal(timer, was)
    end subroutine cancel_timer

    !> Remove TIMER from the list of active timers or idle timers; WAS
    !> says whether it was on one (upstream returns the cons cell that was
    !> removed).  Port of timer.el:cancel-timer-internal.
    subroutine cancel_timer_internal(timer, was)
        type(handle_type), intent(in) :: timer
        logical, intent(out) :: was
        integer :: k
        was = .false.
        k = position_in(timer_list, timer)
        if (k > 0) then
            timer_list = [timer_list(1:k - 1), timer_list(k + 1:)]
            was = .true.
        end if
        k = position_in(timer_idle_list, timer)
        if (k > 0) then
            timer_idle_list = [timer_idle_list(1:k - 1), timer_idle_list(k + 1:)]
            was = .true.
        end if
    end subroutine cancel_timer_internal

    !> Cancel all timers which would run FUNCTION.
    !> This affects ordinary timers such as are scheduled by `run_at_time',
    !> and idle timers such as are scheduled by `run_with_idle_timer'.
    !> Port of timer.el:cancel-function-timers (a function, not a
    !> command).
    subroutine cancel_function_timers(function)
        procedure(command_proc) :: function
        call drop(timer_list)
        call drop(timer_idle_list)
    contains
        subroutine drop(list)
            type(handle_type), allocatable, intent(inout) :: list(:)
            procedure(command_proc), pointer :: p
            integer :: k
            if (.not. allocated(list)) return
            k = 1
            do while (k <= size(list))
                p => timers(list(k)%index)%fn
                if (associated(p, function)) then
                    list = [list(1:k - 1), list(k + 1:)]
                else
                    k = k + 1
                end if
            end do
        end subroutine drop
    end subroutine cancel_function_timers

    !> Call the handler for the timer TIMER.
    !> This function is called, by name, directly by the C code.  Port of
    !> timer.el:timer-event-handler (an error of the function is put in
    !> `timer_error_message').
    recursive subroutine timer_event_handler(timer)
        type(handle_type), intent(in) :: timer
        type(status_type) :: st
        type(handle_type) :: saved
        procedure(command_proc), pointer :: fn
        logical :: retrigger, cell
        real(real64) :: repeats
        if (.not. timerp(timer)) return
        retrigger = .false.
        ! Delete from queue.  Record the cons cell that was used.
        call cancel_timer_internal(timer, cell)
        ! If `cell' is nil, it means the timer was already canceled, so we
        ! shouldn't be running it at all.
        if (.not. cell) return
        ! Re-schedule if requested.
        if (timers(timer%index)%repeat) then
            if (timers(timer%index)%idle) then
                call timer_activate_when_idle(timer, st, .false.)
            else
                call timer_inc_time(timer, timers(timer%index)%repeat_delay)
                ! If real time has jumped forward, perhaps because Emacs
                ! was suspended for a long time, limit how many times
                ! things get repeated.
                if (timers(timer%index)%time < now()) then
                    repeats = real(now() - timers(timer%index)%time, real64)/real(clock_rate(), real64) &
                              /timers(timer%index)%repeat_delay
                    if (repeats > timer_max_repeats) then
                        call timer_inc_time(timer, timers(timer%index)%repeat_delay*repeats)
                    end if
                end if
                ! Place it back on the timer-list before running
                ! timer--function, so it can cancel-timer itself.
                call timer_activate(timer, st, .true.)
                retrigger = .true.
            end if
        end if
        ! Run handler.  Timer functions should not change the current
        ! buffer.
        fn => timers(timer%index)%fn
        saved = null_handle()
        if (cur /= 0) saved = buffer_handle(cur)
        if (associated(fn)) call fn(st)
        if (buffer_live_p(saved)) call set_buffer_internal(saved%index)
        if (.not. st%ok()) timer_error_message = "Error running timer: "//format_error(st)
        ! If the timer's been canceled, don't "retrigger" it since it
        ! might still be in the copy of timer-list kept by
        ! keyboard.c:timer_check (bug#14156).
        if (retrigger .and. timerp(timer)) then
            if (position_in(timer_list, timer) > 0) timers(timer%index)%triggered = .false.
        end if
    end subroutine timer_event_handler

    !> Perform an action at time TIME.
    !> Repeat the action every REPEAT seconds, if REPEAT is present.
    !> REPEAT may be a non-negative integer or floating point number.
    !> TIME is a number of seconds from now (0 meaning now).
    !>
    !> The action is to call FUNCTION.
    !>
    !> This function returns a timer object which you can use in
    !> `cancel_timer'.  Port of timer.el:run-at-time (TIME is a number;
    !> strings, encoded times and t are not ported; no ARGS).
    subroutine run_at_time(timer, time, function, status, repeat)
        type(handle_type), intent(out) :: timer
        real(real64), intent(in) :: time
        procedure(command_proc) :: function
        type(status_type), intent(out) :: status
        real(real64), intent(in), optional :: repeat
        character(len=32) :: buf
        timer = null_handle()
        if (present(repeat)) then
            if (repeat < 0) then
                write (buf, "(g0)") repeat
                call set_error(status, FM_GENERIC_ERROR, "Invalid repetition interval: "//trim(adjustl(buf)))
                return
            end if
        end if
        call timer_create(timer)
        call timer_set_time(timer, time, status, repeat)
        if (status%ok()) call timer_set_function(timer, function, status)
        if (status%ok()) call timer_activate(timer, status)
    end subroutine run_at_time

    !> Perform an action after a delay of SECS seconds.
    !> Repeat the action every REPEAT seconds, if REPEAT is present.
    !> SECS and REPEAT may be integers or floating point numbers.
    !> REPEAT, if present, must be a non-negative number.
    !> The action is to call FUNCTION.
    !>
    !> This function returns a timer object which you can use in
    !> `cancel_timer'.  Port of timer.el:run-with-timer (no ARGS).
    subroutine run_with_timer(timer, secs, function, status, repeat)
        type(handle_type), intent(out) :: timer
        real(real64), intent(in) :: secs
        procedure(command_proc) :: function
        type(status_type), intent(out) :: status
        real(real64), intent(in), optional :: repeat
        call run_at_time(timer, secs, function, status, repeat)
    end subroutine run_with_timer

    !> Perform an action the next time Emacs is idle for SECS seconds.
    !> The action is to call FUNCTION.
    !> SECS may be an integer, a floating point number, or the internal
    !> time format returned by, e.g., `current_idle_time'.
    !> If Emacs is currently idle, and has been idle for N seconds (N < SECS),
    !> then it will call FUNCTION in SECS - N seconds from now.  Using
    !> SECS <= N is not recommended if this function is invoked from an idle
    !> timer, because FUNCTION will then be called immediately.
    !>
    !> If REPEAT is true, do the action each time Emacs has been idle for
    !> exactly SECS seconds (that is, only once for each time Emacs becomes idle).
    !>
    !> This function returns a timer object which you can use in
    !> `cancel_timer'.  Port of timer.el:run-with-idle-timer (no ARGS;
    !> not a command).
    subroutine run_with_idle_timer(timer, secs, repeat, function, status)
        type(handle_type), intent(out) :: timer
        real(real64), intent(in) :: secs
        logical, intent(in) :: repeat
        procedure(command_proc) :: function
        type(status_type), intent(out) :: status
        call timer_create(timer)
        call timer_set_function(timer, function, status)
        if (status%ok()) call timer_set_idle_time(timer, secs, status, repeat)
        if (status%ok()) call timer_activate_when_idle(timer, status, .true.)
    end subroutine run_with_idle_timer

    !> Mark all idle-time timers as once again candidates for running.
    !> Port of timer.el:internal-timer-start-idle.
    subroutine internal_timer_start_idle()
        integer :: k
        if (.not. allocated(timer_idle_list)) return
        do k = 1, size(timer_idle_list)
            if (timerp(timer_idle_list(k))) timers(timer_idle_list(k)%index)%triggered = .false.
        end do
    end subroutine internal_timer_start_idle

    ! ------------------------------------------------------------------
    ! keyboard.c
    ! ------------------------------------------------------------------

    !> Record the start of when Emacs is idle, for the sake of running
    !> idle-time timers.  Port of keyboard.c:timer_start_idle.
    subroutine timer_start_idle()
        ! If we are already in the idle state, do nothing.
        if (idle) return
        idle = .true.
        idleness_start = now()
        last_idleness_start = idleness_start
        ! Mark all idle-time timers as once again candidates for running.
        call internal_timer_start_idle()
    end subroutine timer_start_idle

    !> Record that Emacs is no longer idle, so stop running idle-time
    !> timers.  Port of keyboard.c:timer_stop_idle.
    subroutine timer_stop_idle()
        idle = .false.
    end subroutine timer_stop_idle

    !> Resume idle timer from last idle start time.  Port of
    !> keyboard.c:timer_resume_idle.
    subroutine timer_resume_idle()
        if (idle) return
        idle = .true.
        idleness_start = last_idleness_start
    end subroutine timer_resume_idle

    !> Return the current length of Emacs idleness in seconds in SECS,
    !> with IDLE_P false (upstream's nil) when Emacs is not idle.  Port
    !> of keyboard.c:Fcurrent_idle_time.
    subroutine current_idle_time(idle_p, secs)
        logical, intent(out) :: idle_p
        real(real64), intent(out) :: secs
        idle_p = idle
        secs = 0
        if (idle) secs = real(now() - idleness_start, real64)/real(clock_rate(), real64)
    end subroutine current_idle_time

    !> Whether TIMER is a valid, untriggered timer (keyboard.c:decode_timer
    !> giving a valid time).
    logical function decodable(timer)
        type(handle_type), intent(in) :: timer
        decodable = .false.
        if (.not. timerp(timer)) return
        decodable = .not. timers(timer%index)%triggered
    end function decodable

    !> Check whether a timer has fired.  To prevent larger problems we
    !> simply disregard elements that are not proper timers.
    !>
    !> Returns the time to wait until the next timer fires, in clock
    !> counts.  If a timer is triggering now, return zero.  If no timer
    !> is active, return -1.
    !>
    !> If a timer is ripe, we run it.  In that case we return 0 to
    !> indicate that a new timer_check_2 call should be done.  Port of
    !> keyboard.c:timer_check_2.
    recursive integer(int64) function timer_check_2(list, idle_list) result(wait)
        type(handle_type), intent(in) :: list(:), idle_list(:)
        type(handle_type) :: timer, idle_timer, chosen
        integer(int64) :: t_now, idleness_now, difference, timer_difference, idle_timer_difference
        logical :: ripe, timer_ripe, idle_timer_ripe, have_timer, have_idle, old_deactivate_mark
        integer :: i, j
        wait = -1
        if (size(list) == 0 .and. size(idle_list) == 0) return
        t_now = now()
        idleness_now = 0
        if (idle) idleness_now = t_now - idleness_start
        i = 1
        j = 1
        do while (i <= size(list) .or. j <= size(idle_list))
            have_timer = .false.
            have_idle = .false.
            timer_ripe = .false.
            idle_timer_ripe = .false.
            timer_difference = 0
            idle_timer_difference = 0
            ! Set TIMER and TIMER_DIFFERENCE based on the next ordinary
            ! timer.  Skip past invalid timers and timers already handled.
            if (i <= size(list)) then
                timer = list(i)
                if (.not. decodable(timer)) then
                    i = i + 1
                    cycle
                end if
                have_timer = .true.
                timer_ripe = timers(timer%index)%time <= t_now
                timer_difference = abs(t_now - timers(timer%index)%time)
            end if
            ! Likewise for IDLE_TIMER and IDLE_TIMER_DIFFERENCE based on
            ! the next idle timer.
            if (j <= size(idle_list)) then
                idle_timer = idle_list(j)
                if (.not. decodable(idle_timer)) then
                    j = j + 1
                    cycle
                end if
                have_idle = .true.
                idle_timer_ripe = timers(idle_timer%index)%time <= idleness_now
                idle_timer_difference = abs(idleness_now - timers(idle_timer%index)%time)
            end if
            ! Decide which timer is the next timer, and set CHOSEN,
            ! DIFFERENCE, and RIPE accordingly.
            if (have_timer .and. choose_ordinary()) then
                chosen = timer
                i = i + 1
                difference = timer_difference
                ripe = timer_ripe
            else
                chosen = idle_timer
                j = j + 1
                difference = idle_timer_difference
                ripe = idle_timer_ripe
            end if
            ! If timer is ripe, run it if it hasn't been run.
            if (ripe) then
                if (.not. timers(chosen%index)%triggered) then
                    old_deactivate_mark = vdeactivate_mark
                    ! Mark the timer as triggered to prevent problems if
                    ! the code fails to reschedule it right.
                    timers(chosen%index)%triggered = .true.
                    call timer_event_handler(chosen)
                    vdeactivate_mark = old_deactivate_mark
                    timers_run = timers_run + 1
                end if
                wait = 0
                return
            else
                ! When we encounter a timer that is still waiting, return
                ! the amount of time to wait before it is ripe.
                wait = difference
                return
            end if
        end do
        ! No timers are pending in the future.
        wait = -1
    contains
        !> The ordinary timer comes first: the idle one is invalid, or
        !> riper, or as ripe and nearer.
        logical function choose_ordinary()
            if (.not. have_idle) then
                choose_ordinary = .true.
            else if (idle_timer_ripe .neqv. timer_ripe) then
                choose_ordinary = timer_ripe
            else if (timer_ripe) then
                choose_ordinary = idle_timer_difference < timer_difference
            else
                choose_ordinary = timer_difference < idle_timer_difference
            end if
        end function choose_ordinary
    end function timer_check_2

    !> Check whether a timer has fired.  To prevent larger problems we
    !> simply disregard elements that are not proper timers.
    !>
    !> Returns the time to wait until the next timer fires, in
    !> milliseconds rounded up (so that waiting that long finds it ripe).
    !> If no timer is active, return -1.
    !>
    !> As long as any timer is ripe, we run it.  Port of
    !> keyboard.c:timer_check.
    recursive integer(int32) function timer_check() result(ms)
        type(handle_type), allocatable :: list(:), idle_list(:)
        integer(int64) :: nexttime, rate
        ! We use copies of the timers' lists to allow a timer to add itself
        ! again, without locking up Emacs if the newly added timer is
        ! already ripe when added.
        ! Always consider the ordinary timers.
        if (allocated(timer_list)) then
            list = timer_list
        else
            allocate (list(0))
        end if
        ! Consider the idle timers only if Emacs is idle.
        if (idle .and. allocated(timer_idle_list)) then
            idle_list = timer_idle_list
        else
            allocate (idle_list(0))
        end if
        do
            nexttime = timer_check_2(list, idle_list)
            if (nexttime /= 0) exit
        end do
        if (nexttime < 0) then
            ms = -1
        else
            rate = clock_rate()
            ms = int(min(ceiling(real(nexttime, real64)*1000/real(rate, real64), int64), &
                         int(huge(0_int32), int64)), int32)
        end if
    end function timer_check
end module timer_fm
