! window_scroll_fm.f90 -- scrolling and recentering the selected window.
! Ported from GNU Emacs 31.1 src/window.c (window_scroll_line_based,
! scroll_command, Fscroll_up, Fscroll_down, Frecenter,
! Fmove_to_window_line) and lisp/window.el (scroll-up-command,
! scroll-down-command, recenter-top-bottom,
! move-to-window-line-top-bottom, quit-window).
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> Part of window_fm.  Scrolling moves by screen lines (indent_fm), and
!> the screen-line code depends on window_fm, so these live in a
!> submodule.  Scrolling is line based, as upstream on text terminals,
!> with `scroll-margin' 0 and `scroll-preserve-screen-position' nil.
submodule(window_fm) window_scroll_fm
    use commands_fm, only: commands
    use error_fm, only: format_message, set_error, FM_GENERIC_ERROR, FM_BEGINNING_OF_BUFFER, FM_END_OF_BUFFER
    use lisp_fm, only: nil, nilp, consp, make_fixnum
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use buffer_fm, only: cur, current_buffer, set_buffer, buffer_live_p, buffer_list
    use editfns_fm, only: point, goto_char, eobp, bobp, point_min, point_max
    use cmds_fm, only: forward_line
    use indent_fm, only: vertical_motion, move_to_column
    use frame_fm, only: selected_frame, selected_window_ref
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: this_command, last_command
    use simple_fm, only: goal_column, line_move_visual
    implicit none(type, external)

    integer, parameter :: RECENTER_MIDDLE = 1, RECENTER_TOP = 2, RECENTER_BOTTOM = 3

    !> Cycling order for `recenter-top-bottom' (window.el
    !> recenter-positions, not yet customizable): middle, top, bottom.
    integer, parameter :: recenter_positions(3) = [RECENTER_MIDDLE, RECENTER_TOP, RECENTER_BOTTOM]

    !> Index in recenter_positions of the last op of `recenter-top-bottom'
    !> or `move-to-window-line-top-bottom'; 0 for none (window.el
    !> recenter-last-op).
    integer :: recenter_last_op = 0

contains

    !> The selected window: the minibuffer window while it is active,
    !> else the frame's selected window.
    function selected_window() result(w)
        type(window_type), pointer :: w
        w => selected_window_ref()
    end function selected_window

    !> Whether `w' shows the current buffer.
    logical function shows_current_buffer(w)
        type(window_type), intent(in) :: w
        type(status_type) :: status
        integer(int64) :: bi
        call resolve_buffer(w%buffer, bi, status)
        shows_current_buffer = status%ok() .and. bi == cur
    end function shows_current_buffer

    !> Whether point is in the `ht' screen lines from the start of `w'.
    !> Port of the use of window.c:Fpos_visible_in_window_p in
    !> window_scroll_line_based.
    logical function point_visible_p(w, ht) result(visible)
        type(window_type), intent(in) :: w
        integer, intent(in) :: ht
        integer(int64) :: opoint, start, moved
        opoint = point()
        start = window_start(w)
        visible = .false.
        if (ht <= 0 .or. opoint < start .or. start < buffers(cur)%begv .or. start > buffers(cur)%zv) return
        call goto_char(start)
        call vertical_motion(int(ht, int64), moved)
        ! Short of the end: point is on the last line shown.
        visible = moved < ht .or. opoint < point()
        call goto_char(opoint)
    end function point_visible_p

    !> Scroll `w' (showing the current buffer) up `n' screen lines, down
    !> if negative; with `whole', `n' screenfuls less
    !> next-screen-context-lines.  Port of window.c:window_scroll_line_based
    !> (scroll-margin 0, scroll-preserve-screen-position nil).
    subroutine window_scroll(w, n, whole, status)
        type(window_type), pointer, intent(in) :: w
        integer(int64), intent(in) :: n
        logical, intent(in) :: whole
        type(status_type), intent(out) :: status
        integer(int64) :: nlines, opoint, startpos, pos, moved, bottom
        integer :: ht
        logical :: lose
        status = status_type()
        ht = window_body_lines(w)
        nlines = n
        ! If scrolling screen-fulls, compute the number of lines to
        ! scroll from the window's height.
        if (whole) nlines = n*max(1, ht - next_screen_context_lines)
        opoint = point()
        startpos = window_start(w)
        if (.not. point_visible_p(w, ht)) then
            call vertical_motion(-int(ht/2, int64), moved)
            startpos = point()
        end if
        call goto_char(startpos)
        lose = nlines < 0 .and. point() == buffers(cur)%begv
        call vertical_motion(nlines, moved)
        pos = point()
        call goto_char(opoint)
        if (lose) then
            call set_error(status, FM_BEGINNING_OF_BUFFER)
            return
        end if
        if (pos >= buffers(cur)%zv) then
            call set_error(status, FM_END_OF_BUFFER)
            return
        end if
        ! Set force_start so that redisplay keeps the new start.
        call set_window_start(w, pos, status)
        if (.not. status%ok()) return
        if (nlines > 0) then
            ! If we scrolled forward, put point enough lines down that it
            ! is outside the scroll margin.
            if (pos > opoint) call goto_char(pos)
        else if (nlines < 0) then
            ! If we scrolled backward, put point near the end of the window
            ! but not within the scroll margin.
            call goto_char(pos)
            call vertical_motion(int(ht, int64), moved)
            bottom = point()
            if (moved /= ht) bottom = bottom + 1
            if (bottom > opoint) then
                call goto_char(opoint)
            else
                call vertical_motion(-1_int64, moved)
            end if
        end if
    end subroutine window_scroll

    !> Scroll the selected window by ARG in `direction' (1 up, -1 down).
    !> Port of window.c:scroll_command.
    subroutine scroll_command(arg, direction, status)
        type(value_type), intent(in) :: arg
        integer(int64), intent(in) :: direction
        type(status_type), intent(out) :: status
        type(window_type), pointer :: w
        type(handle_type) :: old
        type(status_type) :: st
        logical :: other
        w => selected_window()
        ! If selected window's buffer isn't current, make it current for
        ! the moment.
        other = .not. shows_current_buffer(w)
        if (other) then
            old = current_buffer()
            call set_buffer(w%buffer, status)
            if (.not. status%ok()) return
        end if
        if (nilp(arg)) then
            call window_scroll(w, direction, .true., status)
        else if (value_eq(arg, sym("-"))) then
            call window_scroll(w, -direction, .true., status)
        else
            call window_scroll(w, prefix_numeric_value(arg)*direction, .false., status)
        end if
        if (other) call set_buffer(old, st)
    end subroutine scroll_command

    module subroutine scroll_up(arg, status)
        type(value_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        call scroll_command(arg, 1_int64, status)
    end subroutine scroll_up

    module subroutine scroll_down(arg, status)
        type(value_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        call scroll_command(arg, -1_int64, status)
    end subroutine scroll_down

    module subroutine recenter(arg, redisplay, status)
        type(value_type), intent(in) :: arg
        logical, intent(in) :: redisplay
        type(status_type), intent(out) :: status
        type(window_type), pointer :: w
        integer(int64) :: iarg, opoint, charpos, moved
        integer :: ht
        logical :: center_p
        status = status_type()
        w => selected_window()
        if (.not. shows_current_buffer(w)) then
            call set_error(status, FM_GENERIC_ERROR, format_message("`recenter'ing a window that does not display current-buffer"))
            return
        end if
        center_p = .false.
        iarg = 0
        if (nilp(arg)) then
            ! recenter-redisplay is `tty': erase and redraw the frame.
            if (redisplay) selected_frame%garbaged = .true.
            center_p = .true.
        else if (consp(arg)) then
            ! Just C-u.
            center_p = .true.
        else
            iarg = prefix_numeric_value(arg)
        end if
        ht = window_body_lines(w)
        if (center_p) then
            iarg = ht/2
        else if (iarg < 0) then
            iarg = iarg + ht
        end if
        ! Don't let it get into the margin at either top or bottom.
        iarg = max(0_int64, min(iarg, int(ht - 1, int64)))
        opoint = point()
        call vertical_motion(-iarg, moved)
        charpos = point()
        call goto_char(opoint)
        ! Set the new window start.
        call set_window_start(w, charpos, status, noforce=.true.)
    end subroutine recenter

    module subroutine move_to_window_line(arg, line, status)
        type(value_type), intent(in) :: arg
        integer(int64), intent(out) :: line
        type(status_type), intent(out) :: status
        type(window_type), pointer :: w
        integer(int64) :: start, iarg, moved
        integer :: lines
        status = status_type()
        line = 0
        w => selected_window()
        if (.not. shows_current_buffer(w)) then
            call set_error(status, FM_GENERIC_ERROR, "move-to-window-line called from unrelated buffer")
            return
        end if
        lines = window_body_lines(w)
        start = window_start(w)
        if (start < buffers(cur)%begv .or. start > buffers(cur)%zv) then
            call vertical_motion(-int(lines/2, int64), moved)
            call set_window_start(w, point(), status)
            if (.not. status%ok()) return
        else
            call goto_char(start)
        end if
        if (nilp(arg)) then
            iarg = lines/2
        else
            iarg = prefix_numeric_value(arg)
            if (iarg < 0) iarg = iarg + lines
        end if
        call vertical_motion(iarg, line)
    end subroutine move_to_window_line

    !> Move to the goal column after a scroll command.  Port of
    !> window.el:scroll-command--goto-goal-column (truncate-lines nil).
    subroutine goto_goal_column()
        integer(int64) :: moved, reached
        if (goal_column < 0) return
        if (line_move_visual) then
            call vertical_motion(0_int64, moved, int(goal_column))
        else
            call move_to_column(goal_column, reached)
        end if
    end subroutine goto_goal_column

    recursive module subroutine scroll_up_command(arg, status)
        type(value_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: shortage
        if (.not. scroll_error_top_bottom) then
            call scroll_up(arg, status)
        else if (value_eq(arg, sym("-"))) then
            call scroll_down_command(nil(), status)
        else if (prefix_numeric_value(arg) < 0) then
            call scroll_down_command(make_fixnum(-prefix_numeric_value(arg)), status)
        else if (eobp()) then
            ! Signal error.
            call scroll_up(arg, status)
        else
            call scroll_up(arg, status)
            if (status%code == FM_END_OF_BUFFER) then
                status = status_type()
                if (nilp(arg)) then
                    ! When ARG is nil for full-screen scrolling, move to the
                    ! bottom of the buffer.
                    call goto_char(point_max())
                else
                    ! When scrolling by ARG lines can't be done, move by ARG
                    ! lines instead.
                    shortage = forward_line(prefix_numeric_value(arg))
                end if
            end if
        end if
        if (status%ok()) call goto_goal_column()
    end subroutine scroll_up_command

    recursive module subroutine scroll_down_command(arg, status)
        type(value_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: shortage
        if (.not. scroll_error_top_bottom) then
            call scroll_down(arg, status)
        else if (value_eq(arg, sym("-"))) then
            call scroll_up_command(nil(), status)
        else if (prefix_numeric_value(arg) < 0) then
            call scroll_up_command(make_fixnum(-prefix_numeric_value(arg)), status)
        else if (bobp()) then
            ! Signal error.
            call scroll_down(arg, status)
        else
            call scroll_down(arg, status)
            if (status%code == FM_BEGINNING_OF_BUFFER) then
                status = status_type()
                if (nilp(arg)) then
                    ! When ARG is nil for full-screen scrolling, move to the
                    ! top of the buffer.
                    call goto_char(point_min())
                else
                    ! When scrolling by ARG lines can't be done, move by ARG
                    ! lines instead.
                    shortage = forward_line(-prefix_numeric_value(arg))
                end if
            end if
        end if
        if (status%ok()) call goto_goal_column()
    end subroutine scroll_down_command

    !> Advance recenter_last_op: the next position after the last one if
    !> this command repeats, else the first.
    subroutine next_recenter_op()
        if (value_eq(this_command, last_command)) then
            recenter_last_op = mod(recenter_last_op, size(recenter_positions)) + 1
        else
            recenter_last_op = 1
        end if
    end subroutine next_recenter_op

    module subroutine recenter_top_bottom(arg, status)
        type(value_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        ! Always respect ARG.
        if (.not. nilp(arg)) then
            call recenter(arg, .true., status)
            return
        end if
        ! (this-scroll-margin is 0.)
        call next_recenter_op()
        select case (recenter_positions(recenter_last_op))
        case (RECENTER_MIDDLE)
            call recenter(nil(), .true., status)
        case (RECENTER_TOP)
            call recenter(make_fixnum(0_int64), .true., status)
        case default
            call recenter(make_fixnum(-1_int64), .true., status)
        end select
    end subroutine recenter_top_bottom

    module subroutine move_to_window_line_top_bottom(arg, status)
        type(value_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: line
        ! Always respect ARG.
        if (.not. nilp(arg)) then
            call move_to_window_line(arg, line, status)
            return
        end if
        call next_recenter_op()
        select case (recenter_positions(recenter_last_op))
        case (RECENTER_MIDDLE)
            call move_to_window_line(nil(), line, status)
        case (RECENTER_TOP)
            call move_to_window_line(make_fixnum(0_int64), line, status)
        case default
            call move_to_window_line(make_fixnum(-1_int64), line, status)
        end select
    end subroutine move_to_window_line_top_bottom

    subroutine cmd_scroll_up(status)
        type(status_type), intent(out) :: status
        call scroll_up(current_prefix_arg, status)
    end subroutine cmd_scroll_up

    subroutine cmd_scroll_down(status)
        type(status_type), intent(out) :: status
        call scroll_down(current_prefix_arg, status)
    end subroutine cmd_scroll_down

    subroutine cmd_scroll_up_command(status)
        type(status_type), intent(out) :: status
        call scroll_up_command(current_prefix_arg, status)
    end subroutine cmd_scroll_up_command

    subroutine cmd_scroll_down_command(status)
        type(status_type), intent(out) :: status
        call scroll_down_command(current_prefix_arg, status)
    end subroutine cmd_scroll_down_command

    subroutine cmd_recenter(status)
        type(status_type), intent(out) :: status
        call recenter(current_prefix_arg, .true., status)
    end subroutine cmd_recenter

    subroutine cmd_recenter_top_bottom(status)
        type(status_type), intent(out) :: status
        call recenter_top_bottom(current_prefix_arg, status)
    end subroutine cmd_recenter_top_bottom

    subroutine cmd_move_to_window_line(status)
        type(status_type), intent(out) :: status
        integer(int64) :: line
        call move_to_window_line(current_prefix_arg, line, status)
    end subroutine cmd_move_to_window_line

    subroutine cmd_move_to_window_line_top_bottom(status)
        type(status_type), intent(out) :: status
        call move_to_window_line_top_bottom(current_prefix_arg, status)
    end subroutine cmd_move_to_window_line_top_bottom

    module subroutine syms_of_window()
        commands%scroll_up%run => cmd_scroll_up
        commands%scroll_down%run => cmd_scroll_down
        commands%scroll_up_command%run => cmd_scroll_up_command
        commands%scroll_down_command%run => cmd_scroll_down_command
        commands%recenter%run => cmd_recenter
        commands%recenter_top_bottom%run => cmd_recenter_top_bottom
        commands%move_to_window_line%run => cmd_move_to_window_line
        commands%move_to_window_line_top_bottom%run => cmd_move_to_window_line_top_bottom
        call syms_of_window_cmds()
        call syms_of_window_display()
    end subroutine syms_of_window
end submodule window_scroll_fm
