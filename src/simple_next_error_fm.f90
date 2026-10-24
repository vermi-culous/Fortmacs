! simple_next_error_fm.f90 -- the next-error framework, a submodule of
! simple_fm.
! Ported from GNU Emacs 31.1 lisp/simple.el (the next-error support
! framework).
!
! Copyright (C) 1985-1987, 1993-2026 Free Software Foundation, Inc.
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

!> `next_error' (M-g n, M-g M-n, C-x `), `previous_error' (M-g p,
!> M-g M-p), `first_error' and `next_error_no_select': moving through the
!> messages of a buffer that has a `next_error_function', such as a
!> compilation buffer.
!>
!> Not ported yet (Deferred): highlighting the locus
!> (`next_error_highlight', `next_error_highlight_no_select': needs
!> overlays and timers) and the message (`next_error_message_highlight'),
!> the fringe arrow (`next_error_overlay_arrow_position'),
!> `next_error_recenter', `next_error_find_buffer_function' and
!> `next_error_found_function' other than their defaults (ignore),
!> `next_error_move_function', `next_error_select_buffer',
!> `next_error_follow_minor_mode', `recenter_current_error', the
!> `display_buffer_overriding_action' of `next_error_no_select' (the
!> selected window is kept), `next_error_this_buffer_no_select' and
!> `previous_error_this_buffer_no_select', the repeat map.
submodule(simple_fm) simple_next_error_fm
    use buffer_fm, only: buffer_live_p, current_buffer, set_buffer, buffer_list
    use handle_fm, only: null_handle
    use data_fm, only: add_local_variables_killer
    use window_fm, only: select_window
    use frame_fm, only: selected_frame
    use error_fm, only: FM_VOID_FUNCTION
    implicit none(type, external)

    !> A buffer's own next-error-function and next-error-buffer.
    type :: next_error_entry_type
        integer(int64) :: serial = 0
        procedure(next_error_function_proc), pointer, nopass :: fn => null()
        type(handle_type) :: next_error_buffer
    end type next_error_entry_type

    type(next_error_entry_type), allocatable :: entries(:)

contains

    integer function entry_of(bi) result(k)
        integer(int64), intent(in) :: bi
        if (allocated(entries) .and. bi > 0) then
            do k = 1, size(entries)
                if (entries(k)%serial == buffers(bi)%serial) return
            end do
        end if
        k = 0
    end function entry_of

    integer function current_entry() result(k)
        type(next_error_entry_type), allocatable :: grown(:)
        integer :: n
        k = entry_of(cur)
        if (k /= 0) return
        if (.not. allocated(entries)) allocate (entries(0))
        n = size(entries)
        allocate (grown(n + 1))
        grown(1:n) = entries
        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, entries)
        k = n + 1
    end function current_entry

    !> Forget the current buffer's next-error-function and
    !> next-error-buffer (kill-all-local-variables).
    subroutine kill_next_error_locals()
        integer :: k
        k = entry_of(cur)
        if (k == 0) return
        entries(k)%fn => null()
        entries(k)%next_error_buffer = null_handle()
    end subroutine kill_next_error_locals

    module procedure set_next_error_function
    integer :: k
    if (cur == 0) return
    k = current_entry()
    entries(k)%fn => fn
    end procedure set_next_error_function

    module procedure has_next_error_function
    integer :: k
    has_next_error_function = .false.
    k = entry_of(bi)
    if (k /= 0) has_next_error_function = associated(entries(k)%fn)
    end procedure has_next_error_function

    recursive module logical function next_error_buffer_p(buffer, avoid_current, extra_test_inclusive, &
                                                          extra_test_exclusive)
        type(handle_type), intent(in) :: buffer
        logical, intent(in), optional :: avoid_current
        procedure(predicate_proc), optional :: extra_test_inclusive, extra_test_exclusive
        type(handle_type) :: old
        type(status_type) :: st
        logical :: avoid
        avoid = .false.
        if (present(avoid_current)) avoid = avoid_current
        next_error_buffer_p = .false.
        ! First make sure it's live.
        if (.not. buffer_live_p(buffer)) return
        if (avoid .and. same_buffer(buffer, current_buffer())) return
        old = current_buffer()
        call set_buffer(buffer, st)
        if (has_next_error_function(cur)) then
            ! This is the normal test.  Optionally reject some buffers.
            next_error_buffer_p = .true.
            if (present(extra_test_exclusive)) next_error_buffer_p = extra_test_exclusive()
        else
            ! Optionally accept some other buffers.
            if (present(extra_test_inclusive)) next_error_buffer_p = extra_test_inclusive()
        end if
        if (buffer_live_p(old)) call set_buffer(old, st)
    end function next_error_buffer_p

    logical function same_buffer(a, b)
        type(handle_type), intent(in) :: a, b
        same_buffer = a%index == b%index .and. a%generation == b%generation
    end function same_buffer

    recursive module subroutine next_error_find_buffer(buffer, status, avoid_current, extra_test_inclusive, &
                                                       extra_test_exclusive)
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: avoid_current
        procedure(predicate_proc), optional :: extra_test_inclusive, extra_test_exclusive
        type(handle_type), allocatable :: all(:)
        logical :: avoid
        integer :: k
        status = status_type()
        avoid = .false.
        if (present(avoid_current)) avoid = avoid_current
        ! 1. If a customizable function returns a buffer, use it (its
        ! default, ignore, returns none).
        ! 2. If next-error-last-buffer is an acceptable buffer, use that.
        if (next_error_buffer_p(next_error_last_buffer, avoid, extra_test_inclusive, extra_test_exclusive)) then
            buffer = next_error_last_buffer
            return
        end if
        ! 3. If the current buffer is acceptable, choose it.
        if (next_error_buffer_p(current_buffer(), avoid, extra_test_inclusive, extra_test_exclusive)) then
            buffer = current_buffer()
            return
        end if
        ! 4. Look for any acceptable buffer.
        all = buffer_list()
        do k = 1, size(all)
            if (next_error_buffer_p(all(k), avoid, extra_test_inclusive, extra_test_exclusive)) then
                buffer = all(k)
                return
            end if
        end do
        ! 5. Use the current buffer as a last resort if it qualifies, even
        ! despite AVOID-CURRENT.
        if (avoid) then
            if (next_error_buffer_p(current_buffer(), .false., extra_test_inclusive, extra_test_exclusive)) then
                call message("This is the only buffer with error message locations")
                buffer = current_buffer()
                return
            end if
        end if
        ! 6. Give up.
        buffer = null_handle()
        call set_error(status, FM_GENERIC_ERROR, "No buffers contain error message locations")
    end subroutine next_error_find_buffer

    module procedure next_error
    type(handle_type) :: buffer, prev
    type(status_type) :: st
    character(len=:), allocatable :: what
    integer(int64) :: arg
    logical :: rst
    integer :: k
    arg = 1
    if (present(n)) arg = n
    rst = .false.
    if (present(reset)) rst = reset
    call next_error_find_buffer(buffer, status)
    if (.not. status%ok()) return
    ! We know here that next-error-function is a valid function we can
    ! call.
    call set_buffer(buffer, status)
    if (.not. status%ok()) return
    k = entry_of(cur)
    call entries(k)%fn(arg, rst, status)
    if (.not. status%ok()) return
    prev = next_error_last_buffer
    call next_error_found(st, buffer, current_buffer())
    if (next_error_verbose%value .or. .not. same_buffer(prev, next_error_last_buffer)) then
        if (rst) then
            what = "First"
        else if (arg == 0) then
            what = "Current"
        else if (arg < 0) then
            what = "Previous"
        else
            what = "Next"
        end if
        call message(what//" locus from "//buffers(next_error_last_buffer%index)%name)
    end if
    end procedure next_error

    module procedure next_error_internal
    type(handle_type) :: buffer, prev
    type(status_type) :: st
    integer :: k
    buffer = current_buffer()
    ! We know here that next-error-function is a valid function we can
    ! call.
    k = entry_of(cur)
    if (k == 0) then
        call set_error(status, FM_VOID_FUNCTION, "null()")
        return
    end if
    call entries(k)%fn(0_int64, .false., status)
    if (.not. status%ok()) return
    prev = next_error_last_buffer
    call next_error_found(st, buffer, current_buffer())
    if (next_error_verbose%value .or. .not. same_buffer(prev, next_error_last_buffer)) then
        call message("Current locus from "//buffers(next_error_last_buffer%index)%name)
    end if
    end procedure next_error_internal

    module procedure next_error_found
    type(handle_type) :: old
    type(status_type) :: st
    integer :: k
    if (present(from_buffer)) then
        next_error_last_buffer = from_buffer
    else
        next_error_last_buffer = current_buffer()
    end if
    if (present(to_buffer)) then
        old = current_buffer()
        call set_buffer(to_buffer, st)
        k = current_entry()
        if (present(from_buffer)) then
            entries(k)%next_error_buffer = from_buffer
        else
            entries(k)%next_error_buffer = null_handle()
        end if
        call set_buffer(old, st)
    end if
    ! (next-error-recenter, next-error-found-function and the message
    ! highlight are Deferred.)
    call next_error_hook%run(status)
    end procedure next_error_found

    !> The command: with C-u, start at the first error.  The interactive
    !> form of simple.el:next-error.
    !>
    !> Visit next `next_error' message and corresponding source code.
    recursive subroutine cmd_next_error(status)
        type(status_type), intent(out) :: status
        if (consp(current_prefix_arg)) then
            call next_error(status, 1_int64, .true.)
        else
            call next_error(status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_next_error

    !> Visit previous `next_error' message and corresponding source code.
    !>
    !> Prefix arg N says how many error messages to move backwards (or
    !> forwards, if negative).
    !>
    !> This operates on the output from the \[compile] and \[grep]
    !> commands.
    !>
    !> See `next_error' for the details.  Port of simple.el:previous-error.
    recursive subroutine previous_error(status)
        type(status_type), intent(out) :: status
        call next_error(status, -prefix_numeric_value(current_prefix_arg))
    end subroutine previous_error

    !> Restart at the first error.
    !> Visit corresponding source code.
    !> With prefix arg N, visit the source code of the Nth error.
    !> This operates on the output from the \[compile] command, for
    !> instance.  Port of simple.el:first-error.
    recursive subroutine first_error(status)
        type(status_type), intent(out) :: status
        call next_error(status, prefix_numeric_value(current_prefix_arg), .true.)
    end subroutine first_error

    !> Move point to the next error in the `next_error' buffer and
    !> highlight match.  Prefix arg N says how many error messages to move
    !> forwards (or backwards, if negative).  Finds and highlights the
    !> source line like \[next_error], but does not select the source
    !> buffer.  Port of simple.el:next-error-no-select.
    recursive subroutine next_error_no_select(status)
        type(status_type), intent(out) :: status
        call no_select(prefix_numeric_value(current_prefix_arg), status)
    end subroutine next_error_no_select

    !> Move point to the previous error in the `next_error' buffer and
    !> highlight match.  Prefix arg N says how many error messages to move
    !> backwards (or forwards, if negative).  Finds and highlights the
    !> source line like \[previous_error], but does not select the source
    !> buffer.  Port of simple.el:previous-error-no-select.
    recursive subroutine previous_error_no_select(status)
        type(status_type), intent(out) :: status
        call no_select(-prefix_numeric_value(current_prefix_arg), status)
    end subroutine previous_error_no_select

    !> next-error-no-select with N: next-error in save-selected-window.
    recursive subroutine no_select(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        type(handle_type) :: selected
        type(status_type) :: st
        selected = selected_frame%selected_window
        call next_error(status, n)
        call select_window(selected, st, norecord=.true.)
    end subroutine no_select

    module procedure syms_of_simple_next_error
    call add_local_variables_killer(kill_next_error_locals)
    commands%next_error%run => cmd_next_error
    commands%previous_error%run => previous_error
    commands%first_error%run => first_error
    commands%next_error_no_select%run => next_error_no_select
    commands%previous_error_no_select%run => previous_error_no_select
    end procedure syms_of_simple_next_error
end submodule simple_next_error_fm
