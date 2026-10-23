! window_cmds_fm.f90 -- selecting, splitting and deleting windows, and
! switching and killing buffers.
! Ported from GNU Emacs 31.1 src/window.c (Fselect_window), lisp/window.el
! (split-window, split-window-below, split-window-right, delete-window,
! delete-other-windows, other-window, switch-to-buffer,
! replace-buffer-in-windows), src/buffer.c (Fother_buffer, Fkill_buffer's
! interactive part) and lisp/simple.el (kill-buffer--possibly-save).
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

!> Part of window_fm: the commands on the window tree (C-x 2, C-x 3,
!> C-x 0, C-x 1, C-x o) and on buffers (C-x b, C-x k).  The selected
!> window shows the current buffer: selecting a window makes its buffer
!> current with point at the window's point, and the window it leaves
!> keeps its point in `pointm'.  These need the frame (frame_fm), which
!> depends on window_fm, so they live in a submodule.
!>
!> Deferred: window parameters, side and atomic windows, dedicated
!> windows, `window-combination-resize', `split-window-keep-point' nil,
!> `delete-window-choose-selected' other than `mru', other frames, the
!> window commands while the minibuffer is selected, buffer menus
!> (C-x C-b), `switch-to-buffer-preserve-window-point',
!> `kill-buffer-query-functions' and `kill-buffer-hook'.
submodule(window_fm) window_cmds_fm
    use commands_fm, only: commands
    use error_fm, only: format_message, set_error, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT
    use buffer_fm, only: cur, current_buffer, set_buffer, buffer_list, get_buffer, get_buffer_create, &
                         record_buffer, frame_seen_buffers, buffer_live_p, kill_buffer, buffer_modified_p, &
                         replace_buffer_in_windows_hook
    use editfns_fm, only: goto_char
    use frame_fm, only: selected_frame, ensure_frame_windows
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use lisp_fm, only: nilp, make_string, list
    use minibuf_fm, only: completion_list_table
    use minibuffer_fm, only: read_buffer, completing_read, REQUIRE_MATCH_T, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION
    use files_fm, only: save_buffer
    implicit none(type, external)


contains

    !> An error for a window that is not live.
    subroutine not_live(status)
        type(status_type), intent(out) :: status
        call set_error(status, FM_WRONG_TYPE_ARGUMENT, "window-live-p")
    end subroutine not_live

    !> The window commands can't run in the minibuffer yet.
    logical function in_minibuffer(status)
        type(status_type), intent(out) :: status
        in_minibuffer = selected_frame%minibuffer_selected
        if (in_minibuffer) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Window commands don't work in the minibuffer yet"))
        else
            status = status_type()
        end if
    end function in_minibuffer

    module subroutine select_window(h, status, norecord)
        type(handle_type), intent(in) :: h
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: norecord
        type(handle_type) :: old
        integer(int64) :: bi
        logical :: record
        if (.not. window_live_p(h)) then
            call not_live(status)
            return
        end if
        record = .true.
        if (present(norecord)) record = .not. norecord
        status = status_type()
        old = selected_frame%selected_window
        if (.not. same_window(old, h)) then
            ! The window left keeps its buffer's point.
            if (window_live_p(old)) then
                call resolve_buffer(windows(old%index)%buffer, bi, status)
                if (status%ok()) call set_window_point(windows(old%index), buffers(bi)%pt, status)
            end if
            selected_frame%selected_window = h
            call set_buffer(windows(h%index)%buffer, status)
            if (.not. status%ok()) return
            call goto_char(window_point(windows(h%index)))
        else
            call set_buffer(windows(h%index)%buffer, status)
            if (.not. status%ok()) return
        end if
        if (record) then
            window_select_count = window_select_count + 1
            windows(h%index)%use_time = window_select_count
            call record_buffer(windows(h%index)%buffer)
        end if
    end subroutine select_window

    !> "#<window N on BUFFER>" for window `h'.
    function window_description(h) result(s)
        type(handle_type), intent(in) :: h
        character(len=:), allocatable :: s
        character(len=20) :: num
        integer(int64) :: bi
        type(status_type) :: status
        write (num, "(i0)") h%index
        s = "#<window "//trim(num)
        call resolve_buffer(windows(h%index)%buffer, bi, status)
        if (status%ok()) s = s//" on "//buffers(bi)%name
        s = s//">"
    end function window_description

    module subroutine split_window(window, size, have_size, horizontal, new, status)
        type(handle_type), intent(in) :: window
        integer, intent(in) :: size
        logical, intent(in) :: have_size, horizontal
        type(handle_type), intent(out) :: new
        type(status_type), intent(out) :: status
        integer :: old_size, new_size, min_size, safe_size
        logical :: too_small
        integer(int64) :: bi
        if (.not. window_live_p(window)) then
            call not_live(status)
            return
        end if
        if (horizontal) then
            old_size = windows(window%index)%total_cols
            min_size = window_min_width
            safe_size = window_safe_min_width
        else
            old_size = windows(window%index)%total_lines
            min_size = window_min_height
            safe_size = window_safe_min_height
        end if
        if (.not. have_size) then
            ! Try to give the new window half the size of WINDOW.
            new_size = old_size/2
            too_small = new_size < min_size .or. old_size - new_size < min_size
        else if (size >= 0) then
            ! SIZE non-negative specifies the new size of WINDOW.
            new_size = old_size - size
            too_small = size >= old_size .or. new_size < safe_size .or. size < safe_size
        else
            ! SIZE negative specifies the size of the new window.
            new_size = -size
            too_small = new_size < safe_size .or. old_size - new_size < safe_size
        end if
        if (too_small) then
            call set_error(status, FM_GENERIC_ERROR, "Window "//window_description(window)//" too small for splitting")
            return
        end if
        ! The selected window's point is its buffer's point.
        if (same_window(window, selected_frame%selected_window)) then
            call resolve_buffer(windows(window%index)%buffer, bi, status)
            if (status%ok()) call set_window_point(windows(window%index), buffers(bi)%pt, status)
        end if
        call split_window_internal(window, horizontal, new_size, selected_frame%root_window, new, status)
    end subroutine split_window

    !> Split WINDOW-TO-SPLIT into two windows, one above the other.
    !> WINDOW-TO-SPLIT defaults to the selected window if omitted or nil.
    !> The newly created window will be below WINDOW-TO-SPLIT and will show
    !> the same buffer as WINDOW-TO-SPLIT.
    !>
    !> If optional argument SIZE is omitted or nil, both windows get the
    !> same height, or close to it.  If SIZE is positive, the upper
    !> (selected) window gets SIZE lines.  If SIZE is negative, the
    !> lower (new) window gets -SIZE lines.  Interactively, SIZE is
    !> the prefix numeric argument.
    !>
    !> Both windows get the same value of point as the WINDOW-TO-SPLIT.
    !> Port of window.el:split-window-below (the command;
    !> `split-window-keep-point' is t).
    subroutine cmd_split_window_below(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: new
        if (in_minibuffer(status)) return
        call split_window(selected_frame%selected_window, int(prefix_numeric_value(current_prefix_arg)), &
                          .not. nilp(current_prefix_arg), .false., new, status)
    end subroutine cmd_split_window_below

    !> Split WINDOW-TO-SPLIT into two side-by-side windows.
    !> WINDOW-TO-SPLIT defaults to the selected window if omitted or nil.
    !> The newly created window will be to the right of WINDOW-TO-SPLIT and
    !> will show the same buffer as WINDOW-TO-SPLIT.
    !>
    !> If optional argument SIZE is omitted or nil, both windows get the
    !> same width, or close to it.  If SIZE is positive, the left-hand
    !> (selected) window gets SIZE columns.  If SIZE is negative, the
    !> right-hand (new) window gets -SIZE columns.  Here, SIZE includes
    !> the width of the divider column to the window's right.
    !> Interactively, SIZE is the prefix numeric argument.
    !> Port of window.el:split-window-right (the command).
    subroutine cmd_split_window_right(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: new
        if (in_minibuffer(status)) return
        call split_window(selected_frame%selected_window, int(prefix_numeric_value(current_prefix_arg)), &
                          .not. nilp(current_prefix_arg), .true., new, status)
    end subroutine cmd_split_window_right

    module subroutine delete_window(window, status)
        type(handle_type), intent(in) :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: heir, mru
        type(handle_type), allocatable :: leaves(:)
        integer :: k, frame_cols
        logical :: was_selected
        status = status_type()
        if (.not. window_valid_p(window)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "window-valid-p")
            return
        end if
        if (same_window(window, selected_frame%root_window) .or. windows(window%index)%mini) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to delete minibuffer or sole ordinary window")
            return
        end if
        was_selected = same_window(window, selected_frame%selected_window)
        frame_cols = windows(selected_frame%root_window%index)%total_cols
        call remove_window_from_tree(window, selected_frame%root_window, heir, frame_cols)
        call delete_window_tree(window)
        if (.not. was_selected) return
        ! delete-window-choose-selected `mru': the most recently used window.
        call window_leaves(selected_frame%root_window, leaves)
        mru = leaves(1)
        do k = 2, size(leaves)
            if (windows(leaves(k)%index)%use_time > windows(mru%index)%use_time) mru = leaves(k)
        end do
        selected_frame%selected_window = null_handle()
        call select_window(mru, status)
    end subroutine delete_window

    !> Delete specified WINDOW.
    !> WINDOW must be a valid window and defaults to the selected one.
    !>
    !> Signal an error if WINDOW is the only window on its frame.
    !>
    !> If WINDOW is the selected window on its frame, choose some other
    !> window as that frame's selected window: the most recently used one.
    !> Port of window.el:delete-window (the command; no window parameters).
    subroutine cmd_delete_window(status)
        type(status_type), intent(out) :: status
        if (in_minibuffer(status)) return
        call delete_window(selected_frame%selected_window, status)
    end subroutine cmd_delete_window

    module subroutine delete_other_windows(window, status)
        type(handle_type), intent(in) :: window
        type(status_type), intent(out) :: status
        type(handle_type) :: old_root, parent
        integer :: top, left, lines, cols
        if (.not. window_valid_p(window)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "window-valid-p")
            return
        end if
        status = status_type()
        old_root = selected_frame%root_window
        if (same_window(window, old_root)) return
        associate (r => windows(old_root%index))
            top = r%top_line
            left = r%left_col
            lines = r%total_lines
            cols = r%total_cols
        end associate
        ! Take WINDOW out of its combination, then free the rest.
        associate (w => windows(window%index))
            parent = w%parent
            if (window_valid_p(w%prev)) then
                windows(w%prev%index)%next = w%next
            else
                windows(parent%index)%child = w%next
            end if
            if (window_valid_p(w%next)) windows(w%next%index)%prev = w%prev
            w%parent = null_handle()
            w%next = null_handle()
            w%prev = null_handle()
        end associate
        call delete_window_tree(old_root)
        selected_frame%root_window = window
        call resize_window_tree(window, top, left, lines, cols, left + cols)
        if (window_live_p(window)) call select_window(window, status)
    end subroutine delete_other_windows

    !> Make WINDOW fill its frame.
    !> WINDOW must be a valid window and defaults to the selected one.
    !> Port of window.el:delete-other-windows (the command; no window
    !> parameters).
    subroutine cmd_delete_other_windows(status)
        type(status_type), intent(out) :: status
        if (in_minibuffer(status)) return
        call delete_other_windows(selected_frame%selected_window, status)
    end subroutine cmd_delete_other_windows

    module subroutine other_window(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: leaves(:)
        integer :: k, n, at
        if (in_minibuffer(status)) return
        call window_leaves(selected_frame%root_window, leaves)
        n = size(leaves)
        if (n == 0) return
        at = 1
        do k = 1, n
            if (same_window(leaves(k), selected_frame%selected_window)) at = k
        end do
        call select_window(leaves(int(modulo(at - 1 + count, int(n, int64))) + 1), status)
    end subroutine other_window

    !> Select another window in cyclic ordering of windows.
    !> COUNT specifies the number of windows to skip, starting with the
    !> selected window, before making the selection.  If COUNT is
    !> positive, skip COUNT windows forwards.  If COUNT is negative,
    !> skip -COUNT windows backwards.  COUNT zero means do not skip any
    !> window, so select the selected window.  In an interactive call,
    !> COUNT is the numeric prefix argument.
    !> Port of window.el:other-window (the command; one frame).
    subroutine cmd_other_window(status)
        type(status_type), intent(out) :: status
        call other_window(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_other_window

    !> Whether buffer `b' is shown in a window of the frame.
    logical function buffer_visible_p(b)
        type(handle_type), intent(in) :: b
        type(handle_type), allocatable :: leaves(:)
        integer :: k
        buffer_visible_p = .false.
        if (.not. window_valid_p(selected_frame%root_window)) return
        call window_leaves(selected_frame%root_window, leaves)
        do k = 1, size(leaves)
            associate (wb => windows(leaves(k)%index)%buffer)
                if (wb%index == b%index .and. wb%generation == b%generation) then
                    buffer_visible_p = .true.
                    return
                end if
            end associate
        end do
    end function buffer_visible_p

    module function other_buffer(buffer) result(h)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: h
        type(handle_type), allocatable :: seen(:), all(:)
        type(handle_type) :: notsogood
        type(status_type) :: status
        integer :: k
        notsogood = null_handle()
        ! Consider buffers that have been seen in the frame first.
        seen = frame_seen_buffers()
        do k = 1, size(seen)
            if (.not. candidate_buffer(seen(k), buffer)) cycle
            if (.not. buffer_visible_p(seen(k))) then
                h = seen(k)
                return
            else if (notsogood%index == 0) then
                notsogood = seen(k)
            end if
        end do
        ! Consider alist of all buffers next.
        all = buffer_list()
        do k = 1, size(all)
            if (.not. candidate_buffer(all(k), buffer)) cycle
            if (.not. buffer_visible_p(all(k))) then
                h = all(k)
                return
            else if (notsogood%index == 0) then
                notsogood = all(k)
            end if
        end do
        if (notsogood%index /= 0) then
            h = notsogood
        else
            call get_buffer_create("*scratch*", h, status)
        end if
    end function other_buffer

    !> Whether `b' is live, not `buffer' and not internal (its name does
    !> not start with a space).  Port of buffer.c:candidate_buffer.
    logical function candidate_buffer(b, buffer)
        type(handle_type), intent(in) :: b, buffer
        candidate_buffer = .false.
        if (.not. buffer_live_p(b)) return
        if (b%index == buffer%index .and. b%generation == buffer%generation) return
        if (len(buffers(b%index)%name) > 0) then
            if (buffers(b%index)%name(1:1) == " ") return
        end if
        candidate_buffer = .true.
    end function candidate_buffer

    module subroutine switch_to_buffer(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        type(handle_type) :: buf
        type(window_type), pointer :: w
        buf = get_buffer(name)
        if (.not. buffer_live_p(buf)) then
            call get_buffer_create(name, buf, status)
            if (.not. status%ok()) return
        end if
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        call ensure_frame_windows()
        w => window_ref(selected_frame%selected_window)
        call set_window_buffer(w, buf, status)
        if (.not. status%ok()) return
        call record_buffer(buf)
    end subroutine switch_to_buffer

    !> Display buffer BUFFER-OR-NAME in the selected window.
    !>
    !> If called interactively, read the buffer name using `read-buffer'.
    !> The variable `confirm-nonexistent-file-or-buffer' determines
    !> whether to request confirmation before creating a new buffer.
    !>
    !> If BUFFER-OR-NAME is a string that does not identify an existing
    !> buffer, create a buffer with that name.  If BUFFER-OR-NAME is
    !> empty, switch to the buffer returned by `other-buffer'.
    !> Port of window.el:switch-to-buffer (the command, with
    !> read-buffer-to-switch; `confirm-nonexistent-file-or-buffer' is
    !> `after-completion').
    recursive subroutine cmd_switch_to_buffer(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(handle_type) :: other
        integer(int64) :: bi
        if (in_minibuffer(status)) return
        other = other_buffer(current_buffer())
        call resolve_buffer(other, bi, status)
        if (.not. status%ok()) return
        call read_buffer("Switch to buffer: ", name, status, def=buffers(bi)%name, &
                         require_match=REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION, except=current_buffer())
        if (.not. status%ok()) return
        if (len(name) == 0) name = buffers(bi)%name
        call switch_to_buffer(name, status)
    end subroutine cmd_switch_to_buffer

    !> Ask the user to confirm killing of a modified BUFFER.
    !> If the user confirms, optionally save BUFFER that is about to be
    !> killed.  `ok' is false if the user says no.  Port of
    !> simple.el:kill-buffer--possibly-save (read-multiple-choice with long
    !> answers).
    recursive subroutine kill_buffer_possibly_save(buffer, ok, status)
        type(handle_type), intent(in) :: buffer
        logical, intent(out) :: ok
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer
        type(handle_type) :: saved
        integer(int64) :: bi
        ok = .false.
        call resolve_buffer(buffer, bi, status)
        if (.not. status%ok()) return
        call completing_read("Buffer "//buffers(bi)%name//" modified; kill anyway? (yes, no, save and then kill) ", &
                             completion_list_table(list(make_string("yes"), make_string("no"), &
                                                        make_string("save and then kill"))), &
                             answer, status, require_match=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        if (answer == "no") return
        ok = .true.
        if (answer /= "yes") then
            saved = current_buffer()
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            call save_buffer(1_int64, .true., status)
            if (buffer_live_p(saved)) call set_buffer(saved, status)
        end if
    end subroutine kill_buffer_possibly_save

    !> Kill the buffer specified by BUFFER-OR-NAME.
    !> The argument may be a buffer or the name of an existing buffer.
    !> Argument nil or omitted means kill the current buffer.
    !>
    !> A modified buffer visiting a file is killed only after asking (and
    !> perhaps saving it).  Port of buffer.c:Fkill_buffer (the command:
    !> "bKill buffer: ", and kill-buffer--possibly-save).
    recursive subroutine cmd_kill_buffer(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(handle_type) :: buf
        integer(int64) :: bi
        logical :: modified, ok
        if (in_minibuffer(status)) return
        call read_buffer("Kill buffer: ", name, status, def=buffers(cur)%name, require_match=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        if (len(name) == 0) name = buffers(cur)%name
        buf = get_buffer(name)
        if (.not. buffer_live_p(buf)) then
            call set_error(status, FM_GENERIC_ERROR, "No such buffer "//name)
            return
        end if
        call resolve_buffer(buf, bi, status)
        call buffer_modified_p(buf, modified, status)
        if (modified .and. allocated(buffers(bi)%filename)) then
            call kill_buffer_possibly_save(buf, ok, status)
            if (.not. status%ok() .or. .not. ok) return
        end if
        call kill_buffer(buf, status)
    end subroutine cmd_kill_buffer

    module subroutine syms_of_window_cmds()
        commands%split_window_below%run => cmd_split_window_below
        commands%split_window_right%run => cmd_split_window_right
        commands%delete_window%run => cmd_delete_window
        commands%delete_other_windows%run => cmd_delete_other_windows
        commands%other_window%run => cmd_other_window
        commands%switch_to_buffer%run => cmd_switch_to_buffer
        commands%kill_buffer%run => cmd_kill_buffer
    end subroutine syms_of_window_cmds
end submodule window_cmds_fm
