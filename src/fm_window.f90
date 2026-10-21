! fm_window.f90 -- windows: a buffer shown in part of a frame.
! Ported from GNU Emacs 31.1 src/window.c and src/window.h (the parts in
! use).
!
! Copyright (C) 1985-1987, 1993-1998, 2000-2026 Free Software Foundation,
! Inc.
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

!> A frame has a root window (no splitting yet) and a one-line minibuffer
!> window, which shows the echo area.  A window's start and point are
!> markers, so they follow insertions and deletions in its buffer.
module fm_window
    use, intrinsic :: iso_fortran_env, only: int64
    use fm_error, only: status_type
    use fm_handle, only: handle_type, null_handle
    use fm_buffer, only: buffers, resolve_buffer
    use fm_marker, only: make_marker, free_marker, set_marker, marker_position
    implicit none(type, external)
    private

    public :: set_window_buffer, window_start, set_window_start, window_point, &
              set_window_point, window_body_lines, free_window

    !> Port of the parts of window.h:struct window in use.
    type, public :: window_type
        !> The buffer shown (contents); the null handle for none.
        type(handle_type) :: buffer
        !> Markers: where the display starts, and point when not selected.
        type(handle_type) :: start, pointm
        !> Position on the frame (0-based rows) and size, in lines and
        !> columns, including the mode line.
        integer :: top_line = 0, total_lines = 0, left_col = 0, total_cols = 0
        !> The minibuffer window: no mode line.
        logical :: mini = .false.
        !> Where redisplay put the cursor, relative to the window; -1: none.
        integer :: cursor_vpos = -1, cursor_hpos = -1
        !> The start was set explicitly; redisplay must keep it.
        logical :: force_start = .false.
        !> Last display ended at window end (for "Bot"/"All" in the mode line).
        integer(int64) :: window_end_pos = 0
        logical :: window_end_valid = .false.
    end type window_type

contains

    !> Show buffer `buffer` in `w`, starting at its accessible beginning,
    !> with the window's point at the buffer's point.  Port of
    !> window.c:set_window_buffer (no window-local history or hooks).
    subroutine set_window_buffer(w, buffer, status)
        type(window_type), intent(inout) :: w
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        integer(int64) :: i

        call resolve_buffer(buffer, i, status)
        if (.not. status%ok()) return
        if (w%start%index == 0) call make_marker(w%start)
        if (w%pointm%index == 0) call make_marker(w%pointm)
        w%buffer = buffer
        call set_marker(w%start, buffers(i)%begv, status, buffer)
        if (.not. status%ok()) return
        call set_marker(w%pointm, buffers(i)%pt, status, buffer)
        w%force_start = .false.
        w%window_end_valid = .false.
    end subroutine set_window_buffer

    !> Where the display of `w` starts (1 if it shows no buffer).  Port of
    !> window.c:Fwindow_start.
    integer(int64) function window_start(w) result(pos)
        type(window_type), intent(in) :: w
        type(status_type) :: status

        pos = 1
        if (w%start%index == 0) return
        call marker_position(w%start, pos, status)
        if (.not. status%ok()) pos = 1
    end function window_start

    !> Make `w` start at `pos`; with `noforce`, redisplay may move it to
    !> show point.  Port of window.c:Fset_window_start.
    subroutine set_window_start(w, pos, status, noforce)
        type(window_type), intent(inout) :: w
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noforce

        if (w%start%index == 0) call make_marker(w%start)
        call set_marker(w%start, pos, status, w%buffer)
        w%force_start = .true.
        if (present(noforce)) w%force_start = .not. noforce
        w%window_end_valid = .false.
    end subroutine set_window_start

    !> The window's point marker position.  (The selected window's point
    !> is its buffer's point; see window.c:Fwindow_point.)
    integer(int64) function window_point(w) result(pos)
        type(window_type), intent(in) :: w
        type(status_type) :: status

        pos = 1
        if (w%pointm%index == 0) return
        call marker_position(w%pointm, pos, status)
        if (.not. status%ok()) pos = 1
    end function window_point

    !> Port of window.c:Fset_window_point (for a non-selected window).
    subroutine set_window_point(w, pos, status)
        type(window_type), intent(inout) :: w
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status

        if (w%pointm%index == 0) call make_marker(w%pointm)
        call set_marker(w%pointm, pos, status, w%buffer)
    end subroutine set_window_point

    !> Lines for text: the total minus the mode line.  Port of
    !> window.c:window_body_height (in lines).
    integer function window_body_lines(w)
        type(window_type), intent(in) :: w

        window_body_lines = w%total_lines
        if (.not. w%mini) window_body_lines = max(0, w%total_lines - 1)
    end function window_body_lines

    !> Free the window's markers.  Not a port (upstream relies on GC).
    subroutine free_window(w)
        type(window_type), intent(inout) :: w
        type(status_type) :: status

        if (w%start%index /= 0) call free_marker(w%start, status)
        if (w%pointm%index /= 0) call free_marker(w%pointm, status)
        w%start = null_handle()
        w%pointm = null_handle()
        w%buffer = null_handle()
    end subroutine free_window

end module fm_window
