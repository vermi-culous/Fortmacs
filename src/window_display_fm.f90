! window_display_fm.f90 -- showing buffers in windows: set-window-buffer,
! the previous buffers of windows, display-buffer and quitting windows.
! Ported from GNU Emacs 31.1 src/window.c (set_window_buffer,
! unshow_buffer, window_discard_buffer_from_window) and lisp/window.el.
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

!> Part of window_fm: which buffer a window shows.  A window remembers
!> the buffers it showed before (`prev_buffers', with their start and
!> point) and how it came to show its buffer (`quit_restore'), so that
!> `quit-window' and `switch-to-prev-buffer' can go back.
!> `display_buffer' picks a window for a buffer the way window.el's
!> `display-buffer' does with its default actions: reuse a window
!> showing the buffer, split the largest or least recently used window
!> sensibly, use a window that showed the buffer before, or else use
!> some other window.
!>
!> Deferred: `display-buffer-alist', `display-buffer-base-action' and the
!> action alist entries other than `inhibit-same-window', other frames
!> and tabs (`pop-up-frames', `display-buffer-pop-up-frame'), side and
!> atomic windows, `window-size-fixed', frame buffer predicates,
!> `switch-to-prev-buffer-skip' and `switch-to-prev-buffer-skip-regexp'
!> other than nil, `quit-restore-window-no-switch',
!> `kill-buffer-quit-windows', `quit-window-hook' and
!> `quit-window-kill-buffer', `temp-buffer-resize-mode', the
!> `window-height' and `window-width' entries, nil for the split
!> thresholds, and windows resized by more than their next or previous
!> sibling can give.
submodule(window_fm) window_display_fm
    use commands_fm, only: commands
    use error_fm, only: set_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR
    use buffer_fm, only: cur, current_buffer, set_buffer, buffer_live_p, record_buffer, bury_buffer_internal, &
                         frame_buffer_list, get_buffer, get_buffer_create, kill_buffer, &
                         set_buffer_read_only, restore_buffer_modified_p, replace_buffer_in_windows_hook, &
                         buffer_handle
    use marker_fm, only: set_marker_restricted
    use frame_fm, only: selected_frame, ensure_frame_windows, selected_window
    use editfns_fm, only: goto_char
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use lisp_fm, only: nilp
    implicit none(type, external)

contains

    ! ------------------------------------------------------------------
    ! Window helpers
    ! ------------------------------------------------------------------

    !> Whether `a' and `b' are the same buffer (or window) handle.
    pure logical function same_handle(a, b)
        type(handle_type), intent(in) :: a, b
        same_handle = a%index == b%index .and. a%generation == b%generation
    end function same_handle

    !> Whether window `h' is the selected window.
    logical function selected_p(h)
        type(handle_type), intent(in) :: h
        selected_p = same_handle(h, selected_window())
    end function selected_p

    !> WINDOW's point: its buffer's point for the selected window, else
    !> its own.  Port of window.c:Fwindow_point.
    integer(int64) function point_of(h) result(pos)
        type(handle_type), intent(in) :: h
        integer(int64) :: bi
        type(status_type) :: status
        if (selected_p(h)) then
            call resolve_buffer(windows(h%index)%buffer, bi, status)
            if (status%ok()) then
                pos = buffers(bi)%pt
                return
            end if
        end if
        pos = window_point(windows(h%index))
    end function point_of

    !> The name of the buffer of `h' (a live buffer), or "".
    function buffer_name_of(b) result(name)
        type(handle_type), intent(in) :: b
        character(len=:), allocatable :: name
        integer(int64) :: bi
        type(status_type) :: status
        name = ""
        call resolve_buffer(b, bi, status)
        if (status%ok()) name = buffers(bi)%name
    end function buffer_name_of

    !> The live ordinary windows of the frame in cyclic order, starting
    !> with `first' (the selected window if absent or not ordinary).
    !> window.el:window-list-1 with MINIBUF `nomini'.
    function window_list_from(first) result(list)
        type(handle_type), intent(in), optional :: first
        type(handle_type), allocatable :: list(:)
        type(handle_type), allocatable :: leaves(:)
        type(handle_type) :: start
        integer :: k, at, n
        call ensure_frame_windows()
        call window_leaves(selected_frame%root_window, leaves)
        n = size(leaves)
        start = selected_frame%selected_window
        if (present(first)) start = first
        at = 1
        do k = 1, n
            if (same_handle(leaves(k), start)) at = k
        end do
        allocate (list(n))
        do k = 1, n
            list(k) = leaves(modulo(at - 1 + k - 1, n) + 1)
        end do
    end function window_list_from

    !> Whether `h' is combined with siblings above and below (or with
    !> HORIZONTAL, side by side).  Port of window.el:window-combined-p.
    logical function window_combined_p(h, horizontal)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: horizontal
        type(handle_type) :: parent
        window_combined_p = .false.
        parent = windows(h%index)%parent
        if (.not. window_valid_p(parent)) return
        window_combined_p = windows(parent%index)%horizontal .eqv. horizontal
    end function window_combined_p

    !> The window's total height if it is combined vertically, else its
    !> width (what display-buffer-record-window stores).
    integer function combined_size(h)
        type(handle_type), intent(in) :: h
        if (window_combined_p(h, .false.)) then
            combined_size = windows(h%index)%total_lines
        else
            combined_size = windows(h%index)%total_cols
        end if
    end function combined_size

    !> Whether `h' is as wide as its frame.  Port of
    !> window.el:window-full-width-p.
    logical function window_full_width_p(h)
        type(handle_type), intent(in) :: h
        window_full_width_p = windows(h%index)%total_cols == windows(selected_frame%root_window%index)%total_cols
    end function window_full_width_p

    !> Columns available for text: the total minus the vertical divider.
    !> Port of window.el:window-width (window-body-width).
    integer function body_cols(h)
        type(handle_type), intent(in) :: h
        body_cols = windows(h%index)%total_cols
        if (windows(h%index)%divider) body_cols = body_cols - 1
    end function body_cols

    ! ------------------------------------------------------------------
    ! set-window-buffer and the previous buffers
    ! ------------------------------------------------------------------

    !> Make window `h' show `buffer', at the start the buffer had in the
    !> last window that showed it and with the buffer's point.  Port of
    !> window.c:set_window_buffer.
    module subroutine set_window_buffer_internal(h, buffer, keep_margins)
        type(handle_type), intent(in) :: h
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: keep_margins
        integer(int64) :: bi
        logical :: samebuf
        type(status_type) :: status
        call resolve_buffer(buffer, bi, status)
        if (.not. status%ok()) return
        associate (w => windows(h%index))
            samebuf = same_handle(buffer, w%buffer)
            w%buffer = buffer
            if (selected_p(h)) buffers(bi)%last_selected_window = h
            w%window_end_pos = 0
            w%window_end_valid = .false.
            ! Discard BUFFER from WINDOW's previous and next buffers.
            call window_discard_buffer_from_window(buffer, h, .false.)
            if (w%start%index == 0) call make_marker(w%start)
            if (w%pointm%index == 0) call make_marker(w%pointm)
            if (.not. (keep_margins .and. samebuf)) then
                w%hscroll = 0
                call set_marker(w%pointm, buffers(bi)%pt, status, buffer)
                call set_marker_restricted(w%start, buffers(bi)%last_window_start, status, buffer)
                w%force_start = .false.
            end if
        end associate
    end subroutine set_window_buffer_internal

    !> Record info on the buffer window `h' is displaying when it is about
    !> to cease to display that buffer: its window start, and its point
    !> unless the buffer is the selected window's.  Port of
    !> window.c:unshow_buffer.
    subroutine unshow_buffer(h)
        type(handle_type), intent(in) :: h
        integer(int64) :: bi, pos
        type(status_type) :: status
        type(handle_type) :: sel, lsw
        logical :: other_shows
        call resolve_buffer(windows(h%index)%buffer, bi, status)
        if (.not. status%ok()) return
        buffers(bi)%last_window_start = window_start(windows(h%index))
        ! Point in the selected window's buffer is actually stored in that
        ! buffer, and the window's pointm isn't used.  So don't clobber
        ! point in that buffer.
        sel = selected_window()
        lsw = buffers(bi)%last_selected_window
        other_shows = .false.
        if (window_live_p(lsw)) then
            if (.not. same_handle(lsw, h)) other_shows = same_handle(windows(lsw%index)%buffer, windows(h%index)%buffer)
        end if
        if (.not. same_handle(windows(sel%index)%buffer, windows(h%index)%buffer) .and. .not. other_shows) then
            pos = min(max(window_point(windows(h%index)), buffers(bi)%begv), buffers(bi)%zv)
            call goto_in_buffer(bi, pos)
        end if
        if (same_handle(lsw, h)) buffers(bi)%last_selected_window = null_handle()
    end subroutine unshow_buffer

    !> Set point of buffer slot `bi' to POS (temp_set_point_both).
    subroutine goto_in_buffer(bi, pos)
        integer(int64), intent(in) :: bi, pos
        type(handle_type) :: saved
        type(status_type) :: status
        saved = current_buffer()
        call set_buffer(buffer_handle(bi), status)
        call goto_char(pos)
        if (buffer_live_p(saved)) call set_buffer(saved, status)
    end subroutine goto_in_buffer

    module subroutine set_window_buffer(h, buffer, status, keep_margins)
        type(handle_type), intent(in) :: h
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: keep_margins
        logical :: keep
        status = status_type()
        if (.not. window_live_p(h)) then
            call set_error(status, FM_GENERIC_ERROR, "Window is deleted")
            return
        end if
        if (.not. buffer_live_p(buffer)) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to display deleted buffer")
            return
        end if
        keep = .false.
        if (present(keep_margins)) keep = keep_margins
        associate (w => windows(h%index))
            if (w%buffer%index /= 0) then
                if (.not. same_handle(w%buffer, buffer)) then
                    if (w%dedicated == DEDICATED_STRONG) then
                        ! WINDOW is strongly dedicated to its buffer.
                        call set_error(status, FM_GENERIC_ERROR, format_message("Window is dedicated to `")// &
                                       buffer_name_of(w%buffer)//format_message("'"))
                        return
                    end if
                    ! WINDOW is weakly dedicated to its buffer, reset
                    ! dedication.
                    w%dedicated = DEDICATED_NONE
                    call record_window_buffer(h)
                end if
                call unshow_buffer(h)
            end if
        end associate
        call set_window_buffer_internal(h, buffer, keep)
    end subroutine set_window_buffer

    !> Remove BUFFER from the previous and next buffers of window `h',
    !> and with ALL from its `quit-restore' and `quit-restore-prev'
    !> parameters.  Port of window.c:window_discard_buffer_from_window.
    module subroutine window_discard_buffer_from_window(buffer, h, all)
        type(handle_type), intent(in) :: buffer, h
        logical, intent(in) :: all
        type(prev_buffer_type), allocatable :: kept(:)
        type(handle_type), allocatable :: knext(:)
        integer :: k, n
        associate (w => windows(h%index))
            if (allocated(w%prev_buffers)) then
                allocate (kept(size(w%prev_buffers)))
                n = 0
                do k = 1, size(w%prev_buffers)
                    if (same_handle(w%prev_buffers(k)%buffer, buffer)) then
                        call free_prev_buffer(w%prev_buffers(k))
                    else
                        n = n + 1
                        kept(n) = w%prev_buffers(k)
                    end if
                end do
                deallocate (w%prev_buffers)
                allocate (w%prev_buffers(n))
                w%prev_buffers(1:n) = kept(1:n)
            end if
            if (allocated(w%next_buffers)) then
                allocate (knext(size(w%next_buffers)))
                n = 0
                do k = 1, size(w%next_buffers)
                    if (same_handle(w%next_buffers(k), buffer)) cycle
                    n = n + 1
                    knext(n) = w%next_buffers(k)
                end do
                deallocate (w%next_buffers)
                allocate (w%next_buffers(n))
                w%next_buffers(1:n) = knext(1:n)
            end if
            if (.not. all) return
            if (mentions(w%quit_restore_prev, buffer)) call free_quit_restore(w%quit_restore_prev)
            if (mentions(w%quit_restore, buffer)) then
                call free_quit_restore(w%quit_restore)
                ! quit-restore becomes quit-restore-prev (which keeps its
                ! marker).
                w%quit_restore = w%quit_restore_prev
                w%quit_restore_prev = quit_restore_type()
            end if
        end associate
    end subroutine window_discard_buffer_from_window

    !> Whether a `quit-restore' parameter names BUFFER as its fourth
    !> element or as the buffer of its quadruple.
    logical function mentions(q, buffer)
        type(quit_restore_type), intent(in) :: q
        type(handle_type), intent(in) :: buffer
        mentions = .false.
        if (q%kind == QR_NONE) return
        mentions = same_handle(q%buffer, buffer)
        if (q%has_quad) mentions = mentions .or. same_handle(q%quad_buffer, buffer)
    end function mentions

    !> Record WINDOW's buffer.
    !> Add the buffer currently shown in WINDOW to the list of WINDOW's
    !> previous buffers.  WINDOW must be a live window and defaults to the
    !> selected one.
    !>
    !> If WINDOW is not a minibuffer window, do not record insignificant
    !> buffers (buffers whose name starts with a space).  If WINDOW is a
    !> minibuffer window, record its buffer if and only if that buffer is a
    !> live minibuffer.  Port of window.el:record-window-buffer.
    module subroutine record_window_buffer(h)
        type(handle_type), intent(in) :: h
        type(prev_buffer_type), allocatable :: grown(:)
        type(prev_buffer_type) :: entry
        type(handle_type) :: buffer
        character(len=:), allocatable :: name
        integer(int64) :: start, point
        integer :: k, n, found
        type(status_type) :: status
        logical :: record
        associate (w => windows(h%index))
            buffer = w%buffer
            if (.not. allocated(w%prev_buffers)) allocate (w%prev_buffers(0))
            found = 0
            do k = 1, size(w%prev_buffers)
                if (same_handle(w%prev_buffers(k)%buffer, buffer)) found = k
            end do
            ! Reset WINDOW's next buffers.  If needed, they are resurrected
            ! by `switch-to-prev-buffer' and `switch-to-next-buffer'.
            if (allocated(w%next_buffers)) deallocate (w%next_buffers)
            allocate (w%next_buffers(0))
            name = buffer_name_of(buffer)
            ! For minibuffer windows record live minibuffers only.  For
            ! normal windows do not record insignificant buffers.
            if (w%mini) then
                record = index(name, " *Minibuf-") == 1
            else
                record = len(name) > 0
                if (record) record = name(1:1) /= " "
            end if
            if (.not. record) then
                if (found > 0) call remove_entry(found)
                return
            end if
            start = window_start(w)
            point = point_of(h)
            if (found > 0) then
                ! We have an entry, update marker positions.
                entry = w%prev_buffers(found)
                call remove_entry(found)
            else
                entry%buffer = buffer
                call make_marker(entry%start)
                call make_marker(entry%point)
            end if
            call set_marker(entry%start, start, status, buffer)
            call set_marker(entry%point, point, status, buffer)
            n = size(w%prev_buffers)
            allocate (grown(n + 1))
            grown(1) = entry
            grown(2:n + 1) = w%prev_buffers
            call move_alloc(grown, w%prev_buffers)
        end associate
    contains
        !> Drop entry K from the list without freeing its markers.
        subroutine remove_entry(k)
            integer, intent(in) :: k
            type(prev_buffer_type), allocatable :: kept(:)
            integer :: m
            m = size(windows(h%index)%prev_buffers)
            allocate (kept(m - 1))
            kept(1:k - 1) = windows(h%index)%prev_buffers(1:k - 1)
            kept(k:m - 1) = windows(h%index)%prev_buffers(k + 1:m)
            call move_alloc(kept, windows(h%index)%prev_buffers)
        end subroutine remove_entry
    end subroutine record_window_buffer

    !> Unrecord BUFFER in WINDOW: make it disappear from the buffers
    !> previously shown there and, with ALL, from the `quit-restore' and
    !> `quit-restore-prev' parameters.  Port of
    !> window.el:unrecord-window-buffer.
    subroutine unrecord_window_buffer(h, buffer, all)
        type(handle_type), intent(in) :: h, buffer
        logical, intent(in) :: all
        if (buffer_live_p(buffer)) call window_discard_buffer_from_window(buffer, h, all)
    end subroutine unrecord_window_buffer

    !> Set WINDOW's buffer to BUFFER.
    !> Optional argument START non-nil means set WINDOW's start position
    !> to START.  Optional argument POINT non-nil means set WINDOW's
    !> point to POINT.  If WINDOW is selected this also sets BUFFER's
    !> `point' to POINT.  If WINDOW is selected and the buffer it showed
    !> before was current this also makes BUFFER the current buffer.
    !> Port of window.el:set-window-buffer-start-and-point.
    subroutine set_window_buffer_start_and_point(h, buffer, status, start, point)
        type(handle_type), intent(in) :: h, buffer
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, point
        logical :: selected, current
        type(handle_type) :: cb
        selected = selected_p(h)
        cb = current_buffer()
        current = same_handle(windows(h%index)%buffer, cb)
        call set_window_buffer(h, buffer, status)
        if (.not. status%ok()) return
        if (selected .and. current) call set_buffer(buffer, status)
        ! Don't force window-start here (even if POINT is nil).
        if (present(start)) call set_window_start(windows(h%index), start, status, noforce=.true.)
        if (present(point)) call set_point_of(h, point)
    end subroutine set_window_buffer_start_and_point

    !> Make POS the point of window `h': its buffer's point if it is the
    !> selected window.  Port of window.c:Fset_window_point.
    subroutine set_point_of(h, pos)
        type(handle_type), intent(in) :: h
        integer(int64), intent(in) :: pos
        type(handle_type) :: saved
        type(status_type) :: status
        if (selected_p(h)) then
            saved = current_buffer()
            call set_buffer(windows(h%index)%buffer, status)
            call goto_char(pos)
            if (buffer_live_p(saved)) call set_buffer(saved, status)
        else
            call set_window_point(windows(h%index), pos, status)
        end if
    end subroutine set_point_of

    !> The position of marker `m' (0 if it points nowhere).
    integer(int64) function marker_pos(m) result(pos)
        type(handle_type), intent(in) :: m
        type(status_type) :: status
        pos = 0
        if (m%index == 0) return
        call marker_position(m, pos, status)
        if (.not. status%ok()) pos = 0
    end function marker_pos

    ! ------------------------------------------------------------------
    ! Switching to previous and next buffers
    ! ------------------------------------------------------------------

    !> Whether `b' is in the list `list'.
    logical function member(list, b)
        type(handle_type), allocatable, intent(in) :: list(:)
        type(handle_type), intent(in) :: b
        integer :: k
        member = .false.
        if (.not. allocated(list)) return
        do k = 1, size(list)
            if (same_handle(list(k), b)) then
                member = .true.
                return
            end if
        end do
    end function member

    !> The index of BUFFER in the previous buffers of `h', or 0.
    integer function prev_entry(h, buffer) result(k)
        type(handle_type), intent(in) :: h, buffer
        integer :: i
        k = 0
        if (.not. allocated(windows(h%index)%prev_buffers)) return
        do i = 1, size(windows(h%index)%prev_buffers)
            if (same_handle(windows(h%index)%prev_buffers(i)%buffer, buffer)) then
                k = i
                return
            end if
        end do
    end function prev_entry

    !> In WINDOW switch to previous buffer.
    !> Return the buffer switched to, nil if no suitable buffer could be
    !> found.
    !>
    !> Optional argument BURY-OR-KILL non-nil means the buffer currently
    !> shown in WINDOW is about to be buried or killed and consequently
    !> shall not be switched to in future invocations of this command.
    !>
    !> As a special case, if BURY-OR-KILL equals `append', this means to
    !> move the buffer to the end of WINDOW's previous buffers list so a
    !> future invocation of `switch-to-prev-buffer' less likely switches
    !> to it.  Port of window.el:switch-to-prev-buffer
    !> (`switch-to-prev-buffer-skip' nil).
    module function switch_to_prev_buffer(window, bury_or_kill, status) result(new_buffer)
        type(handle_type), intent(in) :: window
        integer, intent(in) :: bury_or_kill
        type(status_type), intent(out) :: status
        type(handle_type) :: new_buffer
        type(handle_type) :: h, old_buffer, b
        type(handle_type), allocatable :: next_buffers(:), list(:)
        type(prev_buffer_type) :: entry
        integer :: k, e
        logical :: found
        status = status_type()
        new_buffer = null_handle()
        h = window
        if (windows(h%index)%mini) then
            ! Don't switch in minibuffer window.
            h = selected_frame%selected_window
            if (.not. window_live_p(h)) then
                call set_error(status, FM_GENERIC_ERROR, "Window is a minibuffer window")
                return
            end if
        end if
        old_buffer = windows(h%index)%buffer
        ! Save this since it's destroyed by `set-window-buffer'.
        if (allocated(windows(h%index)%next_buffers)) then
            next_buffers = windows(h%index)%next_buffers
        else
            allocate (next_buffers(0))
        end if
        if (windows(h%index)%dedicated /= DEDICATED_NONE) then
            ! Don't switch in dedicated window.
            call set_error(status, FM_GENERIC_ERROR, "Window is dedicated to buffer "//buffer_name_of(old_buffer))
            return
        end if
        found = .false.
        ! Scan WINDOW's previous buffers first, skipping entries of next
        ! buffers.
        if (allocated(windows(h%index)%prev_buffers)) then
            do k = 1, size(windows(h%index)%prev_buffers)
                entry = windows(h%index)%prev_buffers(k)
                b = entry%buffer
                if (same_handle(b, old_buffer)) cycle
                if (.not. buffer_live_p(b)) cycle
                if (bury_or_kill == BK_NONE .and. member(next_buffers, b)) cycle
                new_buffer = b
                call set_window_buffer_start_and_point(h, b, status, marker_pos(entry%start), marker_pos(entry%point))
                found = .true.
                exit
            end do
        end if
        ! Scan reverted buffer list of WINDOW's frame next, skipping
        ! entries of next buffers.
        if (.not. found) then
            list = frame_buffer_list()
            if (bury_or_kill == BK_NONE) list = list(size(list):1:-1)
            do k = 1, size(list)
                b = list(k)
                if (.not. buffer_live_p(b) .or. same_handle(b, old_buffer)) cycle
                ! Skip buffers whose names start with a space.
                if (index(buffer_name_of(b), " ") == 1) cycle
                if (bury_or_kill == BK_NONE .and. member(next_buffers, b)) cycle
                new_buffer = b
                call set_window_buffer_start_and_point(h, b, status)
                found = .true.
                exit
            end do
        end if
        if (.not. found .and. bury_or_kill == BK_NONE) then
            ! Scan reverted next buffers last.
            do k = size(next_buffers), 1, -1
                b = next_buffers(k)
                if (.not. buffer_live_p(b) .or. same_handle(b, old_buffer)) cycle
                e = prev_entry(h, b)
                if (e == 0) cycle
                entry = windows(h%index)%prev_buffers(e)
                new_buffer = b
                call set_window_buffer_start_and_point(h, b, status, marker_pos(entry%start), marker_pos(entry%point))
                exit
            end do
        end if
        if (.not. status%ok()) return
        if (bury_or_kill /= BK_NONE) then
            e = 0
            if (bury_or_kill == BK_APPEND) e = prev_entry(h, old_buffer)
            if (e > 0) then
                ! Keep old-buffer's entry to append it below.
                entry = windows(h%index)%prev_buffers(e)
                windows(h%index)%prev_buffers(e)%start = null_handle()
                windows(h%index)%prev_buffers(e)%point = null_handle()
            end if
            ! Remove `old-buffer' from WINDOW's previous and (restored list
            ! of) next buffers and also from its 'quit-restore' and
            ! 'quit-restore-prev' parameters.
            call unrecord_window_buffer(h, old_buffer, .true.)
            if (e > 0) then
                ! Append old-buffer's entry to list of WINDOW's previous
                ! buffers.
                call append_prev(h, entry)
            end if
        else
            ! Move `old-buffer' to head of WINDOW's restored list of next
            ! buffers.
            call push_front(next_buffers, old_buffer)
            windows(h%index)%next_buffers = next_buffers
        end if
    end function switch_to_prev_buffer

    !> Append ENTRY to the previous buffers of window `h'.
    subroutine append_prev(h, entry)
        type(handle_type), intent(in) :: h
        type(prev_buffer_type), intent(in) :: entry
        type(prev_buffer_type), allocatable :: grown(:)
        integer :: n
        if (.not. allocated(windows(h%index)%prev_buffers)) allocate (windows(h%index)%prev_buffers(0))
        n = size(windows(h%index)%prev_buffers)
        allocate (grown(n + 1))
        grown(1:n) = windows(h%index)%prev_buffers
        grown(n + 1) = entry
        call move_alloc(grown, windows(h%index)%prev_buffers)
    end subroutine append_prev

    !> (cons b (delq b list))
    subroutine push_front(list, b)
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), intent(in) :: b
        type(handle_type), allocatable :: grown(:)
        integer :: k, n
        n = 0
        allocate (grown(size(list) + 1))
        n = 1
        grown(1) = b
        do k = 1, size(list)
            if (same_handle(list(k), b)) cycle
            n = n + 1
            grown(n) = list(k)
        end do
        list = grown(1:n)
    end subroutine push_front

    !> In WINDOW switch to next buffer.
    !> Return the buffer switched to, nil if no suitable buffer could be
    !> found.  Port of window.el:switch-to-next-buffer
    !> (`switch-to-prev-buffer-skip' nil).
    module function switch_to_next_buffer(window, status) result(new_buffer)
        type(handle_type), intent(in) :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: new_buffer
        type(handle_type) :: h, old_buffer, b
        type(handle_type), allocatable :: next_buffers(:), list(:)
        type(prev_buffer_type) :: entry
        integer :: k, e
        logical :: found
        status = status_type()
        new_buffer = null_handle()
        h = window
        if (windows(h%index)%mini) then
            h = selected_frame%selected_window
            if (.not. window_live_p(h)) then
                call set_error(status, FM_GENERIC_ERROR, "Window is a minibuffer window")
                return
            end if
        end if
        old_buffer = windows(h%index)%buffer
        if (allocated(windows(h%index)%next_buffers)) then
            next_buffers = windows(h%index)%next_buffers
        else
            allocate (next_buffers(0))
        end if
        if (windows(h%index)%dedicated /= DEDICATED_NONE) then
            call set_error(status, FM_GENERIC_ERROR, "Window is dedicated to buffer "//buffer_name_of(old_buffer))
            return
        end if
        found = .false.
        ! Scan WINDOW's next buffers first.
        do k = 1, size(next_buffers)
            b = next_buffers(k)
            if (.not. buffer_live_p(b) .or. same_handle(b, old_buffer)) cycle
            e = prev_entry(h, b)
            if (e == 0) cycle
            entry = windows(h%index)%prev_buffers(e)
            new_buffer = b
            call set_window_buffer_start_and_point(h, b, status, marker_pos(entry%start), marker_pos(entry%point))
            found = .true.
            exit
        end do
        ! Scan the buffer list of WINDOW's frame next, skipping previous
        ! buffers entries.
        if (.not. found) then
            list = frame_buffer_list()
            do k = 1, size(list)
                b = list(k)
                if (.not. buffer_live_p(b) .or. same_handle(b, old_buffer)) cycle
                if (index(buffer_name_of(b), " ") == 1) cycle
                if (prev_entry(h, b) > 0) cycle
                new_buffer = b
                call set_window_buffer_start_and_point(h, b, status)
                found = .true.
                exit
            end do
        end if
        ! Scan WINDOW's reverted previous buffers last (must not use
        ! nreverse here!)
        if (.not. found .and. allocated(windows(h%index)%prev_buffers)) then
            do k = size(windows(h%index)%prev_buffers), 1, -1
                entry = windows(h%index)%prev_buffers(k)
                b = entry%buffer
                if (.not. buffer_live_p(b) .or. same_handle(b, old_buffer)) cycle
                new_buffer = b
                call set_window_buffer_start_and_point(h, b, status, marker_pos(entry%start), marker_pos(entry%point))
                exit
            end do
        end if
        ! Remove `new-buffer' from and restore WINDOW's next buffers.
        if (allocated(windows(h%index)%next_buffers)) deallocate (windows(h%index)%next_buffers)
        allocate (windows(h%index)%next_buffers(0))
        do k = 1, size(next_buffers)
            if (same_handle(next_buffers(k), new_buffer)) cycle
            windows(h%index)%next_buffers = [windows(h%index)%next_buffers, next_buffers(k)]
        end do
    end function switch_to_next_buffer

    ! ------------------------------------------------------------------
    ! Deleting and quitting windows
    ! ------------------------------------------------------------------

    !> Whether window `h' can be deleted: not its frame's only window or
    !> the minibuffer window (the frame, Fortmacs's only one, can't be
    !> deleted).  Port of window.el:window-deletable-p.
    logical function window_deletable_p(h)
        type(handle_type), intent(in) :: h
        window_deletable_p = .false.
        if (same_handle(h, selected_frame%root_window)) return
        if (windows(h%index)%mini) return
        window_deletable_p = .true.
    end function window_deletable_p

    !> Delete WINDOW if possible: with DEDICATED_ONLY, only if it is
    !> dedicated.  Return whether WINDOW got deleted.  Port of
    !> window.el:window--delete.
    logical function window_delete(h, dedicated_only) result(deleted)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: dedicated_only
        type(status_type) :: status
        deleted = .false.
        if (dedicated_only .and. windows(h%index)%dedicated == DEDICATED_NONE) return
        if (.not. window_deletable_p(h)) return
        call delete_window(h, status)
        deleted = status%ok()
    end function window_delete

    !> Select WINDOW after having quit another one.  Do not select an
    !> inactive minibuffer window.  Port of
    !> window.el:window--quit-restore-select-window.
    subroutine quit_restore_select_window(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: status
        if (.not. window_live_p(h)) return
        if (windows(h%index)%mini) return
        call select_window(h, status)
    end subroutine quit_restore_select_window

    !> Quit WINDOW and deal with its buffer.
    !> According to information stored in WINDOW's `quit-restore' and
    !> `quit-restore-prev' parameters either (1) delete WINDOW and its
    !> frame, (2) delete WINDOW but leave its frame alone, (3) restore the
    !> buffer previously shown in WINDOW, or (4) make WINDOW display some
    !> other buffer.  In case (3) set any of these parameters to nil if it
    !> has been used to restore the previously shown buffer.
    !>
    !> BURY-OR-KILL tells how to proceed with the buffer of WINDOW: nil
    !> means to not handle the buffer in a particular way; `append' moves it
    !> to the end of WINDOW's previous buffers; `bury' removes it from them
    !> and moves it to the end of the buffer list; `kill' kills it;
    !> `killing' and `burying' are like `kill' and `bury' but mean the
    !> buffer will be killed or buried elsewhere.  Port of
    !> window.el:quit-restore-window (no frames, tabs or side windows).
    module subroutine quit_restore_window(window, bury_or_kill, status)
        type(handle_type), intent(in) :: window
        integer, intent(in) :: bury_or_kill
        type(status_type), intent(out) :: status
        type(handle_type) :: h, buffer, prev_buffer, select_after, b
        type(quit_restore_type) :: qr, qrp, quad_owner
        type(prev_buffer_type) :: entry
        integer :: dedicated, k, e, delta
        logical :: reset_prev, use_prev, use_main, done
        integer(int64) :: start, point
        status = status_type()
        h = window
        if (.not. window_live_p(h)) h = selected_window()
        buffer = windows(h%index)%buffer
        qr = windows(h%index)%quit_restore
        qrp = windows(h%index)%quit_restore_prev
        select_after = qr%selected
        prev_buffer = null_handle()
        if (allocated(windows(h%index)%prev_buffers)) then
            do k = 1, size(windows(h%index)%prev_buffers)
                b = windows(h%index)%prev_buffers(k)%buffer
                if (.not. same_handle(b, buffer) .and. buffer_live_p(b)) then
                    prev_buffer = b
                    exit
                end if
            end do
        end if
        dedicated = windows(h%index)%dedicated
        reset_prev = .false.
        done = .false.
        if (dedicated /= DEDICATED_NONE) then
            ! First try to delete dedicated windows.
            if (window_delete(h, .true.)) then
                call quit_restore_select_window(select_after)
                done = .true.
            end if
        end if
        if (.not. done .and. prev_buffer%index == 0 .and. qr%what == QR_WINDOW .and. &
            .not. same_handle(h, selected_frame%root_window) .and. same_handle(qr%buffer, buffer)) then
            ! Delete WINDOW if possible.
            if (window_delete(h, .false.)) then
                call quit_restore_select_window(select_after)
                done = .true.
            end if
        end if
        if (.not. done) then
            use_prev = .false.
            use_main = .false.
            if (qrp%kind /= QR_NONE .and. qrp%has_quad) then
                if (buffer_live_p(qrp%quad_buffer) .and. same_handle(qrp%buffer, buffer)) use_prev = .true.
            end if
            if (.not. use_prev .and. qr%kind /= QR_NONE .and. qr%has_quad) then
                if (buffer_live_p(qr%quad_buffer) .and. same_handle(qr%buffer, buffer)) use_main = .true.
            end if
            if (use_prev .or. use_main) then
                if (use_prev) then
                    ! Use selected window from quit-restore-prev; we want to
                    ! reset quit-restore-prev only.
                    quad_owner = qrp
                    select_after = qrp%selected
                    reset_prev = .true.
                else
                    quad_owner = qr
                end if
                ! Show another buffer stored in quit-restore(-prev)
                ! parameter.
                if (quad_owner%quad_size >= 0) then
                    if (quad_owner%quad_size /= combined_size(h)) then
                        ! Try to resize WINDOW to its old height but don't
                        ! signal an error.
                        delta = quad_owner%quad_size - combined_size(h)
                        call window_resize(h, delta, .not. window_combined_p(h, .false.), status)
                        status = status_type()
                    end if
                end if
                windows(h%index)%dedicated = DEDICATED_NONE
                ! Restore WINDOW's previous buffer, start and point
                ! position.
                start = quad_owner%quad_start
                point = marker_pos(quad_owner%quad_point)
                call set_window_buffer_start_and_point(h, quad_owner%quad_buffer, status, start, point)
                if (.not. status%ok()) return
                ! Deal with the buffer we just removed from WINDOW.
                e = 0
                if (bury_or_kill == BK_APPEND) e = prev_entry(h, buffer)
                if (e > 0) then
                    entry = windows(h%index)%prev_buffers(e)
                    windows(h%index)%prev_buffers(e)%start = null_handle()
                    windows(h%index)%prev_buffers(e)%point = null_handle()
                    call window_discard_buffer_from_window(buffer, h, .false.)
                end if
                if (any(bury_or_kill == [BK_BURY, BK_BURYING, BK_KILL, BK_KILLING])) then
                    ! Remove buffer from WINDOW's previous and next buffers.
                    call unrecord_window_buffer(h, buffer, .false.)
                end if
                if (e > 0) call append_prev(h, entry)
                ! Reset the quit-restore(-prev) parameter.
                call free_quit_restore(windows(h%index)%quit_restore_prev)
                if (.not. reset_prev) call free_quit_restore(windows(h%index)%quit_restore)
                ! If WINDOW is the selected window and the previously
                ! selected window is still alive, try to select that window.
                if (selected_p(h) .or. .not. any(bury_or_kill == [BK_KILLING, BK_BURYING])) &
                    call quit_restore_select_window(select_after)
            else
                ! Show some other buffer in WINDOW and leave the
                ! quit-restore(-prev) parameters alone.  Make sure that
                ! WINDOW is no more dedicated.
                windows(h%index)%dedicated = DEDICATED_NONE
                ! Try to switch to a previous buffer.  Delete the window
                ! only if that is not possible.
                b = switch_to_prev_buffer(h, bury_or_kill, status)
                if (.not. status%ok()) return
                if (b%index == 0) then
                    if (window_delete(h, .false.)) continue
                end if
            end if
        end if
        ! Deal with the buffer.
        if (.not. buffer_live_p(buffer)) return
        if (bury_or_kill == BK_KILL) then
            call kill_buffer(buffer, status)
        else if (bury_or_kill == BK_BURY) then
            call bury_buffer_internal(buffer)
        end if
    end subroutine quit_restore_window

    !> Quit WINDOW and bury its buffer.
    !> WINDOW must be a live window and defaults to the selected one.
    !> With prefix argument KILL non-nil, kill the buffer instead of
    !> burying it.
    !>
    !> This calls the function `quit-restore-window' to delete WINDOW or
    !> show some other buffer in it.  Port of window.el:quit-window.
    module subroutine quit_window(kill, status, window)
        logical, intent(in) :: kill
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: window
        type(handle_type) :: h
        h = selected_window()
        if (present(window)) h = window
        if (kill) then
            call quit_restore_window(h, BK_KILL, status)
        else
            call quit_restore_window(h, BK_BURY, status)
        end if
    end subroutine quit_window

    !> Quit WINDOW and bury its buffer.
    !> WINDOW must be a live window and defaults to the selected one.
    !> With prefix argument KILL non-nil, kill the buffer instead of
    !> burying it.  If `quit-window-kill-buffer' is non-nil, perhaps
    !> kill the buffer even if KILL is nil; see the doc string of
    !> that variable for the details.
    !>
    !> This calls the function `quit-restore-window' to delete WINDOW or
    !> show some other buffer in it.  See Info node `(elisp) Quitting
    !> Windows' for more details.
    !>
    !> The functions in `quit-window-hook' will be run before doing
    !> anything else.  Port of window.el:quit-window (the command).
    subroutine cmd_quit_window(status)
        type(status_type), intent(out) :: status
        call quit_window(.not. nilp(current_prefix_arg), status)
    end subroutine cmd_quit_window

    !> Replace BUFFER-OR-NAME with some other buffer in all windows
    !> showing it.  With the exception of side windows, when a window
    !> showing BUFFER-OR-NAME is dedicated, delete that window.  In any
    !> other case, call `switch-to-prev-buffer' to display some other
    !> buffer in that window.  Remove the buffer from the lists of
    !> previous and next buffers of all windows and remove any
    !> `quit-restore' or `quit-restore-prev' parameters mentioning it.
    !> Port of window.el:replace-buffer-in-windows
    !> (`kill-buffer-quit-windows' nil).
    module subroutine replace_buffer_in_windows(buffer)
        type(handle_type), intent(in) :: buffer
        type(handle_type), allocatable :: list(:)
        type(handle_type) :: b
        type(status_type) :: status
        integer :: k
        if (.not. window_valid_p(selected_frame%root_window)) return
        list = window_list_from()
        do k = 1, size(list)
            if (.not. window_live_p(list(k))) cycle
            if (same_handle(windows(list(k)%index)%buffer, buffer)) then
                if (.not. window_delete(list(k), .true.)) then
                    ! Switch to another buffer in that window.
                    windows(list(k)%index)%dedicated = DEDICATED_NONE
                    b = switch_to_prev_buffer(list(k), BK_KILL, status)
                    if (b%index == 0) then
                        if (window_delete(list(k), .false.)) continue
                    end if
                end if
            end if
            if (window_live_p(list(k))) call unrecord_window_buffer(list(k), buffer, .true.)
        end do
        ! The minibuffer window too.
        if (window_live_p(selected_frame%minibuffer_window)) &
            call unrecord_window_buffer(selected_frame%minibuffer_window, buffer, .true.)
    end subroutine replace_buffer_in_windows

    !> Put BUFFER-OR-NAME at the end of the list of all buffers.
    !> There it is the least likely candidate for `other-buffer' to
    !> return; thus, the least likely buffer for \[switch-to-buffer] to
    !> select by default.
    !>
    !> If BUFFER-OR-NAME is nil or omitted, bury the current buffer.
    !> Also, if BUFFER-OR-NAME is nil or omitted, remove the current
    !> buffer from the selected window if it is displayed there.  Port of
    !> window.el:bury-buffer.
    module subroutine bury_buffer(status, buffer)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        type(handle_type) :: b, sw
        status = status_type()
        if (present(buffer)) then
            b = buffer
        else
            b = current_buffer()
        end if
        ! If `buffer-or-name' is not on the selected frame we unrecord it
        ! although it's not "here" (call it a feature).
        sw = selected_window()
        call bury_buffer_internal(b)
        ! Handle case where `buffer-or-name' is nil and the current buffer
        ! is shown in the selected window.
        if (.not. present(buffer) .and. same_handle(windows(sw%index)%buffer, b)) then
            if (windows(sw%index)%mini) return
            if (.not. window_delete(sw, .true.)) then
                ! (switch-to-prev-buffer nil 'bury)
                b = switch_to_prev_buffer(sw, BK_BURY, status)
            end if
        end if
    end subroutine bury_buffer

    !> Put BUFFER-OR-NAME at the end of the list of all buffers.
    !> There it is the least likely candidate for `other-buffer' to
    !> return; thus, the least likely buffer for \[switch-to-buffer] to
    !> select by default.
    !>
    !> You can specify a buffer name as BUFFER-OR-NAME, or an actual
    !> buffer object.  If BUFFER-OR-NAME is nil or omitted, bury the
    !> current buffer.  Also, if BUFFER-OR-NAME is nil or omitted,
    !> remove the current buffer from the selected window if it is
    !> displayed there.  Port of window.el:bury-buffer (the command).
    subroutine cmd_bury_buffer(status)
        type(status_type), intent(out) :: status
        call bury_buffer(status)
    end subroutine cmd_bury_buffer

    !> In selected window switch to ARGth previous buffer.
    !> Call `switch-to-prev-buffer' unless the selected window is the
    !> minibuffer window or is dedicated to its buffer.  Port of
    !> window.el:previous-buffer (`switch-to-prev-buffer-skip' nil).
    subroutine cmd_previous_buffer(status)
        type(status_type), intent(out) :: status
        call prev_or_next_buffer(.true., status)
    end subroutine cmd_previous_buffer

    !> In selected window switch to ARGth next buffer.
    !> Call `switch-to-next-buffer' unless the selected window is the
    !> minibuffer window or is dedicated to its buffer.  Port of
    !> window.el:next-buffer.
    subroutine cmd_next_buffer(status)
        type(status_type), intent(out) :: status
        call prev_or_next_buffer(.false., status)
    end subroutine cmd_next_buffer

    !> The common part of `previous-buffer' and `next-buffer'.
    subroutine prev_or_next_buffer(previous, status)
        logical, intent(in) :: previous
        type(status_type), intent(out) :: status
        type(handle_type) :: sw, b
        integer(int64) :: arg, k
        status = status_type()
        sw = selected_window()
        if (windows(sw%index)%mini) then
            if (previous) then
                call set_error(status, FM_USER_ERROR, "Cannot switch buffers in minibuffer window")
            else
                call set_error(status, FM_USER_ERROR, "Cannot switch buffers in minibuffer window")
            end if
            return
        end if
        if (windows(sw%index)%dedicated == DEDICATED_STRONG) then
            call set_error(status, FM_USER_ERROR, "Window is strongly dedicated to its buffer")
            return
        end if
        arg = prefix_numeric_value(current_prefix_arg)
        do k = 1, abs(arg)
            if ((arg > 0) .eqv. previous) then
                b = switch_to_prev_buffer(sw, BK_NONE, status)
                if (b%index == 0) then
                    call set_error(status, FM_USER_ERROR, "No previous buffer")
                    return
                end if
            else
                b = switch_to_next_buffer(sw, status)
                if (b%index == 0) then
                    call set_error(status, FM_USER_ERROR, "No next buffer")
                    return
                end if
            end if
            if (.not. status%ok()) return
        end do
    end subroutine prev_or_next_buffer

    ! ------------------------------------------------------------------
    ! display-buffer
    ! ------------------------------------------------------------------

    !> Record information for window used by `display-buffer': install
    !> or update the `quit-restore' parameter of WINDOW (or its
    !> `quit-restore-prev' parameter, when WINDOW is reused for another
    !> buffer while it has one).  TYPE is QR_OTHER for `reuse' or
    !> QR_WINDOW for a new window.  Port of
    !> window.el:display-buffer-record-window.
    module subroutine display_buffer_record_window(type, h, buffer)
        integer, intent(in) :: type
        type(handle_type), intent(in) :: h, buffer
        type(quit_restore_type) :: q
        type(handle_type) :: sw
        type(status_type) :: status
        sw = selected_window()
        associate (w => windows(h%index))
            if (type == QR_OTHER) then
                if (same_handle(w%buffer, buffer)) then
                    ! WINDOW shows BUFFER already.  Update WINDOW's
                    ! quit-restore parameter, if any.
                    if (w%quit_restore%kind /= QR_NONE) then
                        w%quit_restore%kind = QR_SAME
                        ! The selected-window might have changed in
                        ! between.
                        if (.not. same_handle(h, sw) .and. .not. same_handle(h, w%quit_restore%selected)) &
                            w%quit_restore%selected = sw
                    end if
                else
                    ! WINDOW shows another buffer: a quadruple of its
                    ! buffer, start, point and height.
                    q%kind = QR_OTHER
                    q%has_quad = .true.
                    q%quad_buffer = w%buffer
                    q%quad_start = window_start(w)
                    call make_marker(q%quad_point)
                    call set_marker(q%quad_point, point_of(h), status, w%buffer)
                    q%quad_size = combined_size(h)
                    q%selected = sw
                    q%buffer = buffer
                    if (w%quit_restore%kind /= QR_NONE) then
                        call free_quit_restore(w%quit_restore_prev)
                        w%quit_restore_prev = q
                    else
                        w%quit_restore = q
                    end if
                end if
            else
                ! WINDOW has been created on an existing frame.
                call free_quit_restore(w%quit_restore)
                w%quit_restore%kind = QR_WINDOW
                w%quit_restore%what = QR_WINDOW
                w%quit_restore%selected = sw
                w%quit_restore%buffer = buffer
            end if
        end associate
    end subroutine display_buffer_record_window

    !> Display BUFFER in WINDOW: TYPE is QR_OTHER when WINDOW existed
    !> already (`reuse'), QR_WINDOW when it is new.  Return WINDOW if
    !> BUFFER and WINDOW are live.  Port of window.el:window--display-buffer
    !> (no action alist entries).
    function window_display_buffer(buffer, h, type, status) result(window)
        type(handle_type), intent(in) :: buffer, h
        integer, intent(in) :: type
        type(status_type), intent(out) :: status
        type(handle_type) :: window
        status = status_type()
        window = null_handle()
        if (.not. buffer_live_p(buffer) .or. .not. window_live_p(h)) return
        call display_buffer_record_window(type, h, buffer)
        if (.not. same_handle(buffer, windows(h%index)%buffer)) then
            windows(h%index)%dedicated = DEDICATED_NONE
            call set_window_buffer(h, buffer, status)
            if (.not. status%ok()) return
        end if
        if (type == QR_WINDOW) then
            if (allocated(windows(h%index)%prev_buffers)) then
                call clear_prev_buffers(h)
            end if
        end if
        window = h
    end function window_display_buffer

    !> (set-window-prev-buffers h nil)
    subroutine clear_prev_buffers(h)
        type(handle_type), intent(in) :: h
        integer :: k
        do k = 1, size(windows(h%index)%prev_buffers)
            call free_prev_buffer(windows(h%index)%prev_buffers(k))
        end do
        deallocate (windows(h%index)%prev_buffers)
        allocate (windows(h%index)%prev_buffers(0))
    end subroutine clear_prev_buffers

    !> Display BUFFER in the selected window, unless INHIBIT_SAME_WINDOW,
    !> the selected window is a minibuffer window or it is dedicated.
    !> Port of window.el:display-buffer-same-window.
    function display_buffer_same_window(buffer, inhibit_same_window, status) result(window)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: inhibit_same_window
        type(status_type), intent(out) :: status
        type(handle_type) :: window, sw
        status = status_type()
        window = null_handle()
        sw = selected_window()
        if (inhibit_same_window .or. windows(sw%index)%mini .or. windows(sw%index)%dedicated /= DEDICATED_NONE) return
        window = window_display_buffer(buffer, sw, QR_OTHER, status)
    end function display_buffer_same_window

    !> Return a window that is already displaying BUFFER, preferably the
    !> selected one (unless INHIBIT_SAME_WINDOW).  Port of
    !> window.el:display-buffer-reuse-window.
    function display_buffer_reuse_window(buffer, inhibit_same_window, status) result(window)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: inhibit_same_window
        type(status_type), intent(out) :: status
        type(handle_type) :: window, sw
        type(handle_type), allocatable :: list(:)
        integer :: k
        status = status_type()
        window = null_handle()
        sw = selected_window()
        ! Avoid calling 'get-buffer-window-list' if the selected window
        ! already shows BUFFER and can be used.
        if (same_handle(windows(sw%index)%buffer, buffer) .and. .not. inhibit_same_window) then
            window = sw
        else
            list = window_list_from()
            do k = 1, size(list)
                if (inhibit_same_window .and. same_handle(list(k), sw)) cycle
                if (same_handle(windows(list(k)%index)%buffer, buffer)) then
                    window = list(k)
                    exit
                end if
            end do
        end if
        if (window_live_p(window)) window = window_display_buffer(buffer, window, QR_OTHER, status)
    end function display_buffer_reuse_window

    !> Display BUFFER by popping up a new window: split the largest or
    !> the least recently used window.  Port of
    !> window.el:display-buffer-pop-up-window.
    function display_buffer_pop_up_window(buffer, status) result(window)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: window
        status = status_type()
        ! Attempt to split largest or least recently used window.
        window = try_to_split_window(get_largest_window(.true., .false.))
        if (.not. window_live_p(window)) window = try_to_split_window(get_lru_window(.true., .false.))
        if (window_live_p(window)) then
            window = window_display_buffer(buffer, window, QR_WINDOW, status)
        else
            window = null_handle()
        end if
    end function display_buffer_pop_up_window

    !> Display BUFFER in a window previously showing it: such a window
    !> other than the selected one, else the selected one (unless
    !> INHIBIT_SAME_WINDOW).  A dedicated window is not usable.  Port of
    !> window.el:display-buffer-in-previous-window (no `previous-window'
    !> entry).
    function display_buffer_in_previous_window(buffer, inhibit_same_window, status) result(window)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: inhibit_same_window
        type(status_type), intent(out) :: status
        type(handle_type) :: window, best, second_best, sw
        type(handle_type), allocatable :: list(:)
        integer :: k
        status = status_type()
        window = null_handle()
        best = null_handle()
        second_best = null_handle()
        sw = selected_window()
        list = window_list_from(frame_first_window())
        do k = 1, size(list)
            if (prev_entry(list(k), buffer) == 0) cycle
            if (windows(list(k)%index)%dedicated /= DEDICATED_NONE) cycle
            if (same_handle(list(k), sw)) then
                if (.not. inhibit_same_window) second_best = list(k)
            else
                best = list(k)
                exit
            end if
        end do
        if (window_live_p(best)) then
            window = window_display_buffer(buffer, best, QR_OTHER, status)
        else if (window_live_p(second_best)) then
            window = window_display_buffer(buffer, second_best, QR_OTHER, status)
        end if
    end function display_buffer_in_previous_window

    !> The frame's first window (the top left one).  Port of
    !> window.c:Fframe_first_window.
    function frame_first_window() result(h)
        type(handle_type) :: h
        type(handle_type), allocatable :: leaves(:)
        call window_leaves(selected_frame%root_window, leaves)
        h = leaves(1)
    end function frame_first_window

    !> Return the least recently used window for display-buffer-use-
    !> some-window: not dedicated, preferring a full-width one that is not
    !> the selected window.  Port of window.el:display-buffer--lru-window
    !> (with `window-min-width' `full-width').
    function display_buffer_lru_window(inhibit_same_window) result(window)
        logical, intent(in) :: inhibit_same_window
        type(handle_type) :: window, best, second_best, sw
        type(handle_type), allocatable :: list(:)
        integer(int64) :: time, best_time, second_best_time
        integer :: k
        best = null_handle()
        second_best = null_handle()
        best_time = -1
        second_best_time = -1
        sw = selected_window()
        list = window_list_from()
        do k = 1, size(list)
            if (windows(list(k)%index)%dedicated /= DEDICATED_NONE) cycle
            if (inhibit_same_window .and. same_handle(list(k), sw)) cycle
            time = windows(list(k)%index)%use_time
            if (same_handle(list(k), sw) .or. .not. window_full_width_p(list(k))) then
                if (second_best_time < 0 .or. time < second_best_time) then
                    second_best_time = time
                    second_best = list(k)
                end if
            else
                if (best_time < 0 .or. time < best_time) then
                    best_time = time
                    best = list(k)
                end if
            end if
        end do
        if (best%index /= 0) then
            window = best
        else
            window = second_best
        end if
    end function display_buffer_lru_window

    !> Display BUFFER in an existing window: the least recently used one,
    !> or one already showing BUFFER, or the largest one.  Port of
    !> window.el:display-buffer-use-some-window (`some-window' nil).
    function display_buffer_use_some_window(buffer, inhibit_same_window, status) result(window)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: inhibit_same_window
        type(status_type), intent(out) :: status
        type(handle_type) :: window, sw
        integer :: delta
        status = status_type()
        sw = selected_window()
        ! Reuse an existing window.
        window = display_buffer_lru_window(inhibit_same_window)
        if (.not. window_live_p(window)) then
            window = get_buffer_window(buffer)
            if (inhibit_same_window .and. same_handle(window, sw)) window = null_handle()
        end if
        if (.not. window_live_p(window)) window = get_largest_window(.false., inhibit_same_window)
        if (.not. window_live_p(window)) then
            window = null_handle()
            return
        end if
        associate (q => windows(window%index)%quit_restore)
            if (q%kind /= QR_NONE .and. q%has_quad .and. q%quad_size >= 0) then
                if (q%quad_size > windows(window%index)%total_lines) then
                    delta = q%quad_size - windows(window%index)%total_lines
                    call window_resize(window, delta, .false., status)
                    status = status_type()
                end if
            end if
        end associate
        window = window_display_buffer(buffer, window, QR_OTHER, status)
        if (window_live_p(window)) call window_even_window_sizes(window)
    end function display_buffer_use_some_window

    !> Display BUFFER in some window and return that window, or the null
    !> handle if no window could be found.  The actions tried are, after
    !> those of ACTION: `display-buffer--maybe-same-window',
    !> `display-buffer-reuse-window', `display-buffer--maybe-pop-up-window',
    !> `display-buffer-in-previous-window' and
    !> `display-buffer-use-some-window' (`display-buffer-fallback-action').
    !> ACTION is DISPLAY_SAME_WINDOW for display-buffer--same-window-action
    !> or DISPLAY_OTHER_WINDOW for display-buffer--other-window-action
    !> (whose `inhibit-same-window' is t).  Port of window.el:display-buffer.
    module subroutine display_buffer(buffer, window, status, action, inhibit_same_window)
        type(handle_type), intent(in) :: buffer
        type(handle_type), intent(out) :: window
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: action
        logical, intent(in), optional :: inhibit_same_window
        integer :: act
        logical :: inhibit
        status = status_type()
        window = null_handle()
        if (.not. buffer_live_p(buffer)) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid buffer")
            return
        end if
        call ensure_frame_windows()
        act = DISPLAY_DEFAULT
        if (present(action)) act = action
        inhibit = .false.
        if (present(inhibit_same_window)) inhibit = inhibit_same_window
        ! The action alist: the first `inhibit-same-window' entry counts.
        if (act == DISPLAY_SAME_WINDOW) inhibit = .false.
        if (act == DISPLAY_OTHER_WINDOW) inhibit = .true.
        select case (act)
        case (DISPLAY_SAME_WINDOW)
            window = display_buffer_same_window(buffer, inhibit, status)
        case (DISPLAY_OTHER_WINDOW)
            window = display_buffer_reuse_window(buffer, inhibit, status)
            if (.not. status%ok()) return
            if (window%index == 0) window = display_buffer_pop_up_window(buffer, status)
        case default
            continue
        end select
        if (.not. status%ok() .or. window%index /= 0) return
        ! display-buffer-fallback-action.  `same-window-p' is nil for any
        ! buffer with the default `same-window-buffer-names' and
        ! `same-window-regexps'.
        window = display_buffer_reuse_window(buffer, inhibit, status)
        if (.not. status%ok() .or. window%index /= 0) return
        if (pop_up_windows) then
            window = display_buffer_pop_up_window(buffer, status)
            if (.not. status%ok() .or. window%index /= 0) return
        end if
        window = display_buffer_in_previous_window(buffer, inhibit, status)
        if (.not. status%ok() .or. window%index /= 0) return
        window = display_buffer_use_some_window(buffer, inhibit, status)
    end subroutine display_buffer

    !> Display buffer BUFFER-OR-NAME and select its window.  This uses
    !> `display-buffer' with ACTION (see `display_buffer') and then
    !> selects the window; unless NORECORD, BUFFER becomes the most
    !> recently selected buffer.  Return the buffer switched to.  Port of
    !> window.el:pop-to-buffer (one frame).
    module subroutine pop_to_buffer(buffer, status, action, norecord)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: action
        logical, intent(in), optional :: norecord
        type(handle_type) :: window
        call display_buffer(buffer, window, status, action)
        if (.not. status%ok()) return
        if (.not. window_live_p(window)) return
        ! Make sure the window is selected.
        call select_window(window, status, norecord)
    end subroutine pop_to_buffer

    !> Select buffer BUFFER in some window, preferably the same one.
    !> Port of window.el:pop-to-buffer-same-window.
    module subroutine pop_to_buffer_same_window(buffer, status, norecord)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: norecord
        call pop_to_buffer(buffer, status, DISPLAY_SAME_WINDOW, norecord)
    end subroutine pop_to_buffer_same_window

    !> Show temporary buffer BUFFER in a window: unmodified, read-only and
    !> at its beginning.  Return the window showing BUFFER.  Port of
    !> window.el:temp-buffer-window-show (no `temp-buffer-resize-mode').
    module subroutine temp_buffer_window_show(buffer, window, status)
        type(handle_type), intent(in) :: buffer
        type(handle_type), intent(out) :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        saved = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call restore_buffer_modified_p(.false.)
        call set_buffer_read_only(buffer, .true., status)
        call goto_char(buffers(cur)%begv)
        if (buffer_live_p(saved)) call set_buffer(saved, status)
        call display_buffer(buffer, window, status)
        if (.not. status%ok()) return
        if (window_live_p(window)) windows(window%index)%hscroll = 0
    end subroutine temp_buffer_window_show

    !> Return a window currently displaying BUFFER-OR-NAME, or nil if none:
    !> the selected window if it does, else the first in the cyclic order
    !> from it.  Port of window.c:Fget_buffer_window.
    module function get_buffer_window(buffer) result(window)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: window
        type(handle_type), allocatable :: list(:)
        integer :: k
        window = null_handle()
        list = window_list_from()
        do k = 1, size(list)
            if (same_handle(windows(list(k)%index)%buffer, buffer)) then
                window = list(k)
                return
            end if
        end do
    end function get_buffer_window

    ! ------------------------------------------------------------------
    ! Choosing and splitting windows
    ! ------------------------------------------------------------------

    !> Return the least recently used window on the frame: a full-width
    !> window if possible, avoiding the selected window if possible (never
    !> it with NOT_SELECTED).  A dedicated window is never a candidate
    !> unless DEDICATED.  Port of window.el:get-lru-window.
    module function get_lru_window(dedicated, not_selected) result(window)
        logical, intent(in) :: dedicated, not_selected
        type(handle_type) :: window, best, second_best, sw
        type(handle_type), allocatable :: list(:)
        integer(int64) :: time, best_time, second_best_time
        integer :: k
        best = null_handle()
        second_best = null_handle()
        best_time = -1
        second_best_time = -1
        sw = selected_window()
        list = window_list_from()
        do k = 1, size(list)
            if (.not. dedicated .and. windows(list(k)%index)%dedicated /= DEDICATED_NONE) cycle
            if (not_selected .and. same_handle(list(k), sw)) cycle
            time = windows(list(k)%index)%use_time
            if (same_handle(list(k), sw) .or. .not. window_full_width_p(list(k))) then
                if (second_best_time < 0 .or. time < second_best_time) then
                    second_best_time = time
                    second_best = list(k)
                end if
            else
                if (best_time < 0 .or. time < best_time) then
                    best_time = time
                    best = list(k)
                end if
            end if
        end do
        if (best%index /= 0) then
            window = best
        else
            window = second_best
        end if
    end function get_lru_window

    !> Return the most recently used window on the frame.  Port of
    !> window.el:get-mru-window.
    module function get_mru_window(dedicated, not_selected) result(window)
        logical, intent(in) :: dedicated, not_selected
        type(handle_type) :: window, sw
        type(handle_type), allocatable :: list(:)
        integer(int64) :: best_time
        integer :: k
        window = null_handle()
        best_time = -1
        sw = selected_window()
        list = window_list_from()
        do k = 1, size(list)
            if (.not. dedicated .and. windows(list(k)%index)%dedicated /= DEDICATED_NONE) cycle
            if (not_selected .and. same_handle(list(k), sw)) cycle
            if (best_time < 0 .or. windows(list(k)%index)%use_time > best_time) then
                best_time = windows(list(k)%index)%use_time
                window = list(k)
            end if
        end do
    end function get_mru_window

    !> Return the largest window on the frame.  A dedicated window is never
    !> a candidate unless DEDICATED; NOT_SELECTED means never return the
    !> selected window.  Port of window.el:get-largest-window.
    module function get_largest_window(dedicated, not_selected) result(window)
        logical, intent(in) :: dedicated, not_selected
        type(handle_type) :: window, sw
        type(handle_type), allocatable :: list(:)
        integer :: k, area, best_size
        window = null_handle()
        best_size = 0
        sw = selected_window()
        list = window_list_from()
        do k = 1, size(list)
            if (.not. dedicated .and. windows(list(k)%index)%dedicated /= DEDICATED_NONE) cycle
            if (not_selected .and. same_handle(list(k), sw)) cycle
            area = windows(list(k)%index)%total_lines*windows(list(k)%index)%total_cols
            if (area > best_size) then
                best_size = area
                window = list(k)
            end if
        end do
    end function get_largest_window

    !> Return non-nil if `split-window-sensibly' may split WINDOW: one at
    !> least `split-height-threshold' lines high and twice
    !> `window-min-height' (or 2), or with HORIZONTAL at least
    !> `split-width-threshold' columns wide and twice `window-min-width'
    !> (or 2).  THRESHOLD_ZERO stands for `split-height-threshold' bound
    !> to 0.  Port of window.el:window-splittable-p.
    logical function window_splittable_p(h, horizontal, threshold_zero)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: horizontal
        logical, intent(in), optional :: threshold_zero
        integer :: threshold
        window_splittable_p = .false.
        if (.not. window_live_p(h)) return
        if (horizontal) then
            window_splittable_p = body_cols(h) >= max(split_width_threshold, 2*max(window_min_width, 2))
        else
            threshold = split_height_threshold
            if (present(threshold_zero)) then
                if (threshold_zero) threshold = 0
            end if
            ! The window has a mode line.
            window_splittable_p = windows(h%index)%total_lines >= max(threshold, 2*max(window_min_height, 2))
        end if
    end function window_splittable_p

    !> Split WINDOW below (or with HORIZONTAL to the right) and return the
    !> new window, which gets WINDOW's `quit-restore' parameter.  Port of
    !> window.el:split-window-below and split-window-right (no SIZE,
    !> `split-window-keep-point' t).
    module subroutine split_window_and_copy(h, horizontal, new, status)
        type(handle_type), intent(in) :: h
        logical, intent(in) :: horizontal
        type(handle_type), intent(out) :: new
        type(status_type), intent(out) :: status
        call split_window(h, 0, .false., horizontal, new, status)
        if (.not. status%ok()) return
        ! Always copy quit-restore parameter in interactive use.
        call copy_quit_restore(windows(h%index)%quit_restore, windows(new%index)%quit_restore)
    end subroutine split_window_and_copy

    !> TO := FROM, with a marker of its own.
    module subroutine copy_quit_restore(from, to)
        type(quit_restore_type), intent(in) :: from
        type(quit_restore_type), intent(inout) :: to
        type(status_type) :: status
        integer(int64) :: pos
        if (from%kind == QR_NONE) return
        call free_quit_restore(to)
        to = from
        to%quad_point = null_handle()
        if (from%quad_point%index /= 0) then
            pos = marker_pos(from%quad_point)
            call make_marker(to%quad_point)
            call set_marker(to%quad_point, pos, status, from%quad_buffer)
        end if
    end subroutine copy_quit_restore

    !> Whether the frame is wider than it is tall (characters being about
    !> twice as tall as wide).  Port of window.el:window--frame-landscape-p.
    logical function frame_landscape_p()
        associate (r => windows(selected_frame%root_window%index), m => windows(selected_frame%minibuffer_window%index))
            frame_landscape_p = r%total_cols > 2*(r%total_lines + m%total_lines)
        end associate
    end function frame_landscape_p

    !> Split WINDOW in a way suitable for `display-buffer'.  Try the
    !> directions in the order `split-window-preferred-direction' says,
    !> each if `window-splittable-p' allows it; if both fail and WINDOW is
    !> the only window on its frame (or all others are dedicated), split
    !> it vertically disregarding `split-height-threshold'.  Return the new
    !> window or the null handle.  Port of window.el:split-window-sensibly.
    function split_window_sensibly(h) result(new)
        type(handle_type), intent(in) :: h
        type(handle_type) :: new
        type(handle_type), allocatable :: list(:)
        logical :: horizontal_first, others
        integer :: k
        horizontal_first = split_window_preferred_direction == SPLIT_HORIZONTAL .or. &
                           (split_window_preferred_direction == SPLIT_LONGEST .and. frame_landscape_p())
        if (horizontal_first) then
            new = try_split(h, .true.)
            if (new%index == 0) new = try_split(h, .false.)
        else
            new = try_split(h, .false.)
            if (new%index == 0) new = try_split(h, .true.)
        end if
        if (new%index /= 0) return
        others = .false.
        if (.not. same_handle(h, selected_frame%root_window)) then
            list = window_list_from()
            do k = 1, size(list)
                if (same_handle(list(k), h)) cycle
                if (windows(list(k)%index)%dedicated == DEDICATED_NONE) others = .true.
            end do
        end if
        if (others .or. windows(h%index)%mini) return
        if (window_splittable_p(h, .false., threshold_zero=.true.)) new = do_split(h, .false.)
    contains
        function try_split(w, horizontal) result(n)
            type(handle_type), intent(in) :: w
            logical, intent(in) :: horizontal
            type(handle_type) :: n
            n = null_handle()
            if (window_splittable_p(w, horizontal)) n = do_split(w, horizontal)
        end function try_split

        function do_split(w, horizontal) result(n)
            type(handle_type), intent(in) :: w
            logical, intent(in) :: horizontal
            type(handle_type) :: n
            type(status_type) :: status
            call split_window_and_copy(w, horizontal, n, status)
            if (.not. status%ok()) n = null_handle()
        end function do_split
    end function split_window_sensibly

    !> Try to split WINDOW: return the new window if
    !> `split-window-sensibly' made one, the null handle otherwise.  Port
    !> of window.el:window--try-to-split-window.
    function try_to_split_window(h) result(new)
        type(handle_type), intent(in) :: h
        type(handle_type) :: new
        new = null_handle()
        if (.not. window_live_p(h)) return
        new = split_window_sensibly(h)
        if (.not. window_live_p(new)) new = null_handle()
    end function try_to_split_window

    !> Even sizes of WINDOW and selected window, if these windows are the
    !> only children of their parent and the selected window is larger
    !> than WINDOW.  Port of window.el:window--even-window-sizes
    !> (`even-window-sizes' t).
    subroutine window_even_window_sizes(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: sw, parent
        type(status_type) :: status
        if (.not. even_window_sizes) return
        sw = selected_window()
        parent = windows(h%index)%parent
        if (.not. window_valid_p(parent)) return
        if (child_count(parent) /= 2) return
        if (.not. same_handle(windows(sw%index)%parent, parent)) return
        if (window_combined_p(h, .true.) .and. windows(sw%index)%total_cols > windows(h%index)%total_cols) then
            call window_resize(sw, (windows(h%index)%total_cols - windows(sw%index)%total_cols)/2, .true., status)
        else if (window_combined_p(h, .false.) .and. windows(sw%index)%total_lines > windows(h%index)%total_lines) then
            call window_resize(sw, (windows(h%index)%total_lines - windows(sw%index)%total_lines)/2, .false., status)
        end if
    end subroutine window_even_window_sizes

    !> The number of children of internal window `h'.
    integer function child_count(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: c
        child_count = 0
        c = windows(h%index)%child
        do while (window_valid_p(c))
            child_count = child_count + 1
            c = windows(c%index)%next
        end do
    end function child_count

    !> Resize window `h' by DELTA lines (or with HORIZONTAL columns),
    !> taking the space from (or giving it to) its next sibling, else its
    !> previous one; each window keeps at least `window-min-height' lines
    !> (or `window-min-width' columns).  Port of window.el:window-resize
    !> for a window with such a sibling (`window-combination-resize' nil).
    module subroutine window_resize(h, delta, horizontal, status)
        type(handle_type), intent(in) :: h
        integer, intent(in) :: delta
        logical, intent(in) :: horizontal
        type(status_type), intent(out) :: status
        type(handle_type) :: sib
        integer :: min_size
        status = status_type()
        if (delta == 0) return
        if (.not. window_combined_p(h, horizontal)) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot resize the root window of a frame")
            return
        end if
        sib = windows(h%index)%next
        if (.not. window_valid_p(sib)) sib = windows(h%index)%prev
        if (horizontal) then
            min_size = window_min_width
        else
            min_size = window_min_height
        end if
        if (size_of(sib) - delta < min_size .or. size_of(h) + delta < min_size) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot resize window")
            return
        end if
        if (same_handle(sib, windows(h%index)%next)) then
            call resize_window_tree(h, windows(h%index)%top_line, windows(h%index)%left_col, &
                                    lines_of(h, delta), cols_of(h, delta), frame_cols())
            if (horizontal) then
                call resize_window_tree(sib, windows(sib%index)%top_line, windows(sib%index)%left_col + delta, &
                                        lines_of(sib, 0), cols_of(sib, -delta), frame_cols())
            else
                call resize_window_tree(sib, windows(sib%index)%top_line + delta, windows(sib%index)%left_col, &
                                        lines_of(sib, -delta), cols_of(sib, 0), frame_cols())
            end if
        else
            call resize_window_tree(sib, windows(sib%index)%top_line, windows(sib%index)%left_col, &
                                    lines_of(sib, -delta), cols_of(sib, -delta), frame_cols())
            if (horizontal) then
                call resize_window_tree(h, windows(h%index)%top_line, windows(h%index)%left_col - delta, &
                                        lines_of(h, 0), cols_of(h, delta), frame_cols())
            else
                call resize_window_tree(h, windows(h%index)%top_line - delta, windows(h%index)%left_col, &
                                        lines_of(h, delta), cols_of(h, 0), frame_cols())
            end if
        end if
    contains
        integer function size_of(w)
            type(handle_type), intent(in) :: w
            if (horizontal) then
                size_of = windows(w%index)%total_cols
            else
                size_of = windows(w%index)%total_lines
            end if
        end function size_of

        integer function lines_of(w, d)
            type(handle_type), intent(in) :: w
            integer, intent(in) :: d
            lines_of = windows(w%index)%total_lines
            if (.not. horizontal) lines_of = lines_of + d
        end function lines_of

        integer function cols_of(w, d)
            type(handle_type), intent(in) :: w
            integer, intent(in) :: d
            cols_of = windows(w%index)%total_cols
            if (horizontal) cols_of = cols_of + d
        end function cols_of

        integer function frame_cols()
            frame_cols = windows(selected_frame%root_window%index)%left_col + &
                         windows(selected_frame%root_window%index)%total_cols
        end function frame_cols
    end subroutine window_resize

    !> Register the commands of this submodule.
    module subroutine syms_of_window_display()
        replace_buffer_in_windows_hook => replace_buffer_in_windows
        commands%quit_window%run => cmd_quit_window
        commands%bury_buffer%run => cmd_bury_buffer
        commands%previous_buffer%run => cmd_previous_buffer
        commands%next_buffer%run => cmd_next_buffer
    end subroutine syms_of_window_display
end submodule window_display_fm
