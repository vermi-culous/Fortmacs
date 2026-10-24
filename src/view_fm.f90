! view_fm.f90 -- peruse file or buffer without editing.
! Ported from GNU Emacs 31.1 lisp/view.el.
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

!> View mode: a minor mode for viewing text but not editing it, with
!> `view_mode_map' (SPC, DEL, S-SPC, <, >, g, s, r, q, e, E, Q, c, C and
!> the rest), and the commands that enter it: `view_file',
!> `view_file_other_window', `view_buffer', `view_buffer_other_window'.
!> Upstream's commands with a key binding are named View-...; Fortran
!> names are not case-sensitive, so here they are `view_quit',
!> `view_scroll_page_forward' and so on.  The buffer-local variables
!> (`view_exit_action', the page sizes, the old read-only state, the last
!> regexp, the local value of `view_read_only') are kept per buffer here.
!>
!> Deferred: highlighting the match of a search (`view_highlight_face',
!> `view_overlay': overlays), `F' (View-revert-buffer-scroll-page-forward)
!> and `view_try_extend_at_buffer_end' (reverting a file-visiting buffer),
!> `H', `h' and `?' (describe-mode), the frame commands
!> (`view_file_other_frame', `view_buffer_other_frame'), the QUIT-RESTORE
!> argument of `view_mode_enter', `read_only_mode__state' (read only by
!> reverting file buffers), the obsolete `view_mode_disable'.  `''
!> runs `jump_to_register', as there is no alias `register_to_point'.
module view_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_ARGS_OUT_OF_RANGE, format_message
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, make_fixnum, make_string, list
    use lread_fm, only: sym
    use data_fm, only: add_local_variables_killer
    use eval_fm, only: hook_type, change_major_mode_hook
    use buffer_fm, only: buffers, cur, current_buffer, buffer_live_p, resolve_buffer, buffer_handle, &
                         buffer_modified_p, kill_buffer, get_file_buffer
    use editfns_fm, only: point, point_min, point_max, goto_char, eobp, line_end_position
    use cmds_fm, only: forward_line, beginning_of_line, end_of_line
    use search_fm, only: re_search_forward, looking_at, SEARCH_NOERROR
    use fileio_fm, only: file_exists_p, expand_file_name
    use files_fm, only: find_file_noselect, view_read_only
    use window_fm, only: windows, window_live_p, window_leaves, window_body_lines, scroll_up_command, &
                         scroll_down_command, recenter, pos_visible_in_window_p, quit_window, switch_to_buffer, &
                         pop_to_buffer, display_alist_type, next_screen_context_lines, scroll_error_top_bottom, &
                         other_buffer, same_handle, QR_NONE
    use frame_fm, only: selected_frame, selected_window
    use keymap_fm, only: keymap_set
    use keyboard_fm, only: this_command, sit_for
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use xdisp_fm, only: message
    use dispnew_fm, only: ding
    use simple_fm, only: push_mark, pop_mark, mark
    use minibuf_fm, only: read_string
    use minibuffer_fm, only: read_file_name, read_buffer, REQUIRE_MATCH_T
    use help_fm, only: substitute_command_keys, register_keymap_name
    use print_fm, only: fortran_string
    use derived_fm, only: major_mode_type, major_mode_named
    use modes_fm, only: minor_modes
    implicit none(type, external)
    private

    public :: syms_of_view, view_mode, view_mode_enter, view_mode_exit, view_file, view_file_other_window, &
              view_buffer, view_buffer_other_window, kill_buffer_if_not_modified, view_exit_action_proc, &
              view_mode_enabled, view_exit_and_edit, view_read_only_value, set_view_read_only_local, view_search

    abstract interface
        !> A `view_exit_action': called with the viewed BUFFER when finished
        !> viewing.
        recursive subroutine view_exit_action_proc(buffer, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
        end subroutine view_exit_action_proc
    end interface

    !> True means scrolling past the end of buffer exits View mode.
    !> False means attempting to scroll past the end of the buffer,
    !> only rings the bell and gives a message on how to leave.  Port of
    !> view.el view-scroll-auto-exit.
    logical, public :: view_scroll_auto_exit = .false.

    !> True means restore all windows used to view buffer.
    !> Commands that restore windows when finished viewing a buffer,
    !> apply to all windows that display the buffer and have restore
    !> information.  If `view_exits_all_viewing_windows' is false, only
    !> the selected window is considered for restoring.  Port of view.el
    !> view-exits-all-viewing-windows.
    logical, public :: view_exits_all_viewing_windows = .false.

    !> True inhibits the help message shown upon entering View mode.
    !> This setting takes effect only when View mode is entered via an
    !> interactive command; otherwise the help message is not shown.
    !> Port of view.el view-inhibit-help-message.
    logical, public :: view_inhibit_help_message = .false.

    !> Normal hook run when starting to view a buffer or file.  Port of
    !> view.el view-mode-hook.
    type(hook_type), target, public :: view_mode_hook = hook_type(name="view_mode_hook")

    !> If true, View mode "exit" commands don't actually disable View mode.
    !> Instead, these commands just switch buffers or windows.
    !> This is set in certain buffers by specialized features such as help commands
    !> that use View mode automatically.  Port of view.el
    !> view-no-disable-on-exit.
    logical, public :: view_no_disable_on_exit = .false.

    !> Keymap for `view_mode'.  Port of view.el view-mode-map.
    type(handle_type), public :: view_mode_map

    !> A buffer's View mode variables (buffer-local upstream), by buffer
    !> serial number.
    type :: view_local_type
        integer(int64) :: serial = 0
        logical :: set = .false.
        !> view-old-buffer-read-only
        logical :: old_read_only = .false.
        !> view-page-size and view-half-page-size (has_...: non-nil)
        logical :: has_page_size = .false., has_half_page_size = .false.
        integer(int64) :: page_size = 0, half_page_size = 0
        !> view-last-regexp: none, a string, or (if last_no) a list of it
        logical :: has_last_regexp = .false., last_no = .false.
        character(len=:), allocatable :: last_regexp
        !> view-exit-action
        procedure(view_exit_action_proc), pointer, nopass :: exit_action => null()
        !> The buffer-local value of `view_read_only'.
        logical :: has_view_read_only = .false., view_read_only = .false.
    end type view_local_type

    type(view_local_type), allocatable :: locals(:)

contains

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    !> Whether a live buffer has serial number SERIAL.
    logical function serial_live(serial)
        integer(int64), intent(in) :: serial
        integer(int64) :: k
        serial_live = .false.
        if (.not. allocated(buffers)) return
        do k = 1, size(buffers, kind=int64)
            if (buffers(k)%serial /= serial) cycle
            serial_live = buffer_live_p(buffer_handle(k))
            return
        end do
    end function serial_live

    !> The index of the current buffer's entry in `locals', made if MAKE
    !> (0 if none).
    integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(view_local_type), allocatable :: grown(:)
        integer :: i, n
        k = 0
        if (cur == 0) return
        if (.not. allocated(locals)) allocate (locals(0))
        do i = 1, size(locals)
            if (locals(i)%set .and. locals(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do
        if (.not. make) return
        ! Reuse the entry of a dead buffer.
        do i = 1, size(locals)
            if (locals(i)%set) then
                if (serial_live(locals(i)%serial)) cycle
            end if
            locals(i) = view_local_type()
            locals(i)%set = .true.
            locals(i)%serial = buffers(cur)%serial
            k = i
            return
        end do
        n = size(locals)
        allocate (grown(n + 1))
        grown(1:n) = locals
        call move_alloc(grown, locals)
        locals(n + 1) = view_local_type()
        locals(n + 1)%set = .true.
        locals(n + 1)%serial = buffers(cur)%serial
        k = n + 1
    end function local_index

    !> kill-all-local-variables: View mode's buffer-local variables.
    subroutine kill_view_locals()
        integer :: k
        k = local_index(.false.)
        if (k > 0) locals(k) = view_local_type()
    end subroutine kill_view_locals

    !> The value of `view_read_only' in the current buffer: its
    !> buffer-local value if it has one.
    logical function view_read_only_value()
        integer :: k
        view_read_only_value = view_read_only
        k = local_index(.false.)
        if (k == 0) return
        if (locals(k)%has_view_read_only) view_read_only_value = locals(k)%view_read_only
    end function view_read_only_value

    !> (setq-local view-read-only VALUE).
    subroutine set_view_read_only_local(value)
        logical, intent(in) :: value
        integer :: k
        k = local_index(.true.)
        if (k == 0) return
        locals(k)%has_view_read_only = .true.
        locals(k)%view_read_only = value
    end subroutine set_view_read_only_local

    !> Whether View mode is on in the current buffer (the variable
    !> `view_mode').
    logical function view_mode_enabled()
        view_mode_enabled = minor_modes%view_mode%enabled()
    end function view_mode_enabled

    ! ------------------------------------------------------------------
    ! Entering and exiting
    ! ------------------------------------------------------------------

    !> Like `kill_buffer', but does nothing if buffer BUF is modified.
    !> Port of view.el:kill-buffer-if-not-modified.
    recursive subroutine kill_buffer_if_not_modified(buf, status)
        type(handle_type), intent(in) :: buf
        type(status_type), intent(out) :: status
        logical :: modified
        status = status_type()
        if (.not. buffer_live_p(buf)) return
        call buffer_modified_p(buf, modified, status)
        if (.not. status%ok()) return
        if (.not. modified) call kill_buffer(buf, status)
    end subroutine kill_buffer_if_not_modified

    !> `kill_buffer' as an exit action.
    recursive subroutine kill_buffer_action(buf, status)
        type(handle_type), intent(in) :: buf
        type(status_type), intent(out) :: status
        call kill_buffer(buf, status)
    end subroutine kill_buffer_action

    !> Whether the current buffer's major mode is special (its
    !> `mode_class' property is `special').
    logical function special_major_mode()
        type(major_mode_type), pointer :: p
        special_major_mode = .false.
        if (.not. allocated(buffers(cur)%major_mode)) return
        p => major_mode_named(buffers(cur)%major_mode)
        if (associated(p)) special_major_mode = p%special
    end function special_major_mode

    !> View FILE in View mode, returning to previous buffer when done.
    !> The buffer is killed when done unless it was visited before (or is
    !> modified).  Port of view.el:view-file.
    recursive subroutine view_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: absname
        type(handle_type) :: had_a_buf, buffer
        status = status_type()
        if (.not. file_exists_p(file)) then
            call set_error(status, FM_GENERIC_ERROR, file//" does not exist")
            return
        end if
        call expand_file_name(file, absname, status)
        if (.not. status%ok()) return
        had_a_buf = get_file_buffer(absname)
        call find_file_noselect(file, buffer, status)
        if (.not. status%ok()) return
        if (had_a_buf%index == 0) then
            call view_buffer(buffer, status, kill_buffer_if_not_modified)
        else
            call view_buffer(buffer, status)
        end if
    end subroutine view_file

    !> View FILE in View mode, returning to previous buffer when done.
    !> Emacs commands editing the buffer contents are not available; instead, a
    !> special set of commands (mostly letters and punctuation) are defined for
    !> moving around in the buffer.
    !> Space scrolls forward, Delete scrolls backward.
    !> For a list of all View commands, type H or h while viewing.
    !>
    !> This command runs the normal hook `view_mode_hook'.  Port of
    !> view.el:view-file (interactive).
    recursive subroutine cmd_view_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        call read_file_name("View file: ", file, status, mustmatch=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        call view_file(file, status)
    end subroutine cmd_view_file

    !> View FILE in View mode in another window.
    !> When done, return that window to its previous buffer, and kill the
    !> buffer visiting FILE if unmodified and if it wasn't visited before.
    !> Port of view.el:view-file-other-window.
    recursive subroutine view_file_other_window(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: absname
        type(handle_type) :: had_a_buf, buf_to_view
        status = status_type()
        if (.not. file_exists_p(file)) then
            call set_error(status, FM_GENERIC_ERROR, file//" does not exist")
            return
        end if
        call expand_file_name(file, absname, status)
        if (.not. status%ok()) return
        had_a_buf = get_file_buffer(absname)
        call find_file_noselect(file, buf_to_view, status)
        if (.not. status%ok()) return
        if (had_a_buf%index == 0) then
            call view_buffer_other_window(buf_to_view, status, kill_buffer_if_not_modified)
        else
            call view_buffer_other_window(buf_to_view, status)
        end if
    end subroutine view_file_other_window

    !> View FILE in View mode in another window.
    !> When done, return that window to its previous buffer, and kill the
    !> buffer visiting FILE if unmodified and if it wasn't visited before.
    !>
    !> Emacs commands editing the buffer contents are not available; instead,
    !> a special set of commands (mostly letters and punctuation)
    !> are defined for moving around in the buffer.
    !> Space scrolls forward, Delete scrolls backward.
    !> For a list of all View commands, type H or h while viewing.
    !>
    !> This command runs the normal hook `view_mode_hook'.
    !>
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of view.el:view-file-other-window (interactive).
    recursive subroutine cmd_view_file_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        call read_file_name("In other window view file: ", file, status, mustmatch=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        call view_file_other_window(file, status)
    end subroutine cmd_view_file_other_window

    !> View BUFFER in View mode, returning to previous buffer when done.
    !> EXIT_ACTION, if present, is called with the buffer when finished
    !> viewing it.  This function does not enable View mode if the buffer's
    !> major mode has a `special' mode-class.  Port of view.el:view-buffer.
    recursive subroutine view_buffer(buffer, status, exit_action)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        procedure(view_exit_action_proc), optional :: exit_action
        integer(int64) :: b
        call resolve_buffer(buffer, b, status)
        if (.not. status%ok()) then
            return
        end if
        call switch_to_buffer(buffers(b)%name, status)
        if (.not. status%ok()) return
        if (special_major_mode()) then
            call message("Not using View mode because the major mode is special")
        else
            call view_mode_enter(status, exit_action)
        end if
    end subroutine view_buffer

    !> View BUFFER in View mode, returning to previous buffer when done.
    !> Emacs commands editing the buffer contents are not available; instead, a
    !> special set of commands (mostly letters and punctuation) are defined for
    !> moving around in the buffer.
    !> Space scrolls forward, Delete scrolls backward.
    !> For a list of all View commands, type H or h while viewing.
    !>
    !> This command runs the normal hook `view_mode_hook'.
    !>
    !> This function does not enable View mode if the buffer's major mode
    !> has a `special' mode-class, because such modes usually have their
    !> own View-like bindings.  Port of view.el:view-buffer (interactive).
    recursive subroutine cmd_view_buffer(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        character(len=:), allocatable :: name
        type(handle_type) :: other
        integer(int64) :: b
        other = other_buffer(current_buffer())
        call resolve_buffer(other, b, st)
        if (.not. st%ok()) b = 0
        if (b > 0) then
            call read_buffer("View buffer: ", name, status, def=buffers(b)%name, require_match=REQUIRE_MATCH_T)
        else
            call read_buffer("View buffer: ", name, status, require_match=REQUIRE_MATCH_T)
        end if
        if (.not. status%ok()) return
        call view_buffer(buffer_by_name(name), status)
    end subroutine cmd_view_buffer

    !> The live buffer named NAME (the null handle if none).
    function buffer_by_name(name) result(h)
        character(len=*), intent(in) :: name
        type(handle_type) :: h
        integer(int64) :: k
        h = handle_type()
        do k = 1, size(buffers, kind=int64)
            if (.not. buffer_live_p(buffer_handle(k))) cycle
            if (.not. allocated(buffers(k)%name)) cycle
            if (buffers(k)%name == name .and. len(buffers(k)%name) == len(name)) then
                h = buffer_handle(k)
                return
            end if
        end do
    end function buffer_by_name

    !> View BUFFER in View mode in another window.  EXIT_ACTION, if
    !> present, is called with the buffer when finished viewing it.  Port
    !> of view.el:view-buffer-other-window (NOT-RETURN is ignored upstream).
    recursive subroutine view_buffer_other_window(buffer, status, exit_action)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        procedure(view_exit_action_proc), optional :: exit_action
        type(display_alist_type) :: alist
        ! (let ((pop-up-windows t)) (pop-to-buffer buffer t))
        alist%inhibit_same_window = .true.
        call pop_to_buffer(buffer, status, alist=alist)
        if (.not. status%ok()) return
        if (special_major_mode()) then
            call message("Not using View mode because the major mode is special")
        else
            call view_mode_enter(status, exit_action)
        end if
    end subroutine view_buffer_other_window

    !> View BUFFER in View mode in another window.
    !> Emacs commands editing the buffer contents are not available;
    !> instead, a special set of commands (mostly letters and
    !> punctuation) are defined for moving around in the buffer.
    !> Space scrolls forward, Delete scrolls backward.
    !> For a list of all View commands, type H or h while viewing.
    !>
    !> This command runs the normal hook `view_mode_hook'.
    !>
    !> This function does not enable View mode if the buffer's major mode
    !> has a `special' mode-class, because such modes usually have their
    !> own View-like bindings.
    !>
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of view.el:view-buffer-other-window (interactive).
    recursive subroutine cmd_view_buffer_other_window(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        character(len=:), allocatable :: name
        type(handle_type) :: other
        integer(int64) :: b
        other = other_buffer(current_buffer())
        call resolve_buffer(other, b, st)
        if (.not. st%ok()) b = 0
        if (b > 0) then
            call read_buffer("In other window view buffer: ", name, status, def=buffers(b)%name, &
                             require_match=REQUIRE_MATCH_T)
        else
            call read_buffer("In other window view buffer: ", name, status, require_match=REQUIRE_MATCH_T)
        end if
        if (.not. status%ok()) return
        call view_buffer_other_window(buffer_by_name(name), status)
    end subroutine cmd_view_buffer_other_window

    !> Toggle View mode, a minor mode for viewing text but not editing it.
    !>
    !> When View mode is enabled, commands that do not change the buffer
    !> contents are available as usual.  Kill commands save text but
    !> do not delete it from the buffer.  Most other commands beep and
    !> tell the user that the buffer is read-only.
    !>
    !> \<view_mode_map>
    !>
    !> The following additional commands are provided.  Most commands
    !> take prefix arguments.  Page commands default to "page size"
    !> lines which is almost a whole window, or number of lines set by
    !> \[view_scroll_page_forward_set_page_size] or \[view_scroll_page_backward_set_page_size].
    !> Half page commands default to and set "half page size" lines
    !> which initially is half a window full.  Search commands default
    !> to a repeat count of one.
    !>
    !> H, h, ?         This message.
    !> Digits        provide prefix arguments.
    !> \[negative_argument]        negative prefix argument.
    !> \[beginning_of_buffer]        move to the beginning of buffer.
    !> >        move to the end of buffer.
    !> \[view_scroll_to_buffer_end]        scroll so that buffer end is at last line of window.
    !> SPC        scroll forward "page size" lines.
    !>           With prefix scroll forward prefix lines.
    !> DEL, S-SPC  scroll backward "page size" lines.
    !>               With prefix scroll backward prefix lines.
    !> \[view_scroll_page_forward_set_page_size]        like  \[view_scroll_page_forward]
    !>           but with prefix sets "page size" to prefix.
    !> \[view_scroll_page_backward_set_page_size]        like  \[view_scroll_page_backward]
    !>           but with prefix sets "page size" to prefix.
    !> \[view_scroll_half_page_forward]        scroll forward "half page size" lines.  With prefix, sets
    !>           "half page size" to prefix lines and scrolls forward that much.
    !> \[view_scroll_half_page_backward]        scroll backward "half page size" lines.  With prefix, sets
    !>           "half page size" to prefix lines and scrolls backward that much.
    !> RET, LFD  scroll forward one line.  With prefix scroll forward prefix line(s).
    !> y        scroll backward one line.  With prefix scroll backward prefix line(s).
    !> \[what_line]        prints the current line number.
    !> \[view_goto_percent]        goes prefix argument (default 100) percent into buffer.
    !> \[view_goto_line]        goes to line given by prefix argument (default first line).
    !> .        set the mark.
    !> x        exchanges point and mark.
    !> \[view_back_to_mark]        return to mark and pops mark ring.
    !>           Mark ring is pushed at start of every successful search and when
    !>           jump to line occurs.  The mark is set on jump to buffer start or end.
    !> \[point_to_register]        save current position in character register.
    !> '        go to position saved in character register.
    !> s        do forward incremental search.
    !> r        do reverse incremental search.
    !> \[view_search_regexp_forward]        searches forward for regular expression, starting after current page.
    !>           ! and @ have a special meaning at the beginning of the regexp.
    !>           ! means search for a line with no match for regexp.  @ means start
    !>           search at beginning (end for backward search) of buffer.
    !> \        searches backward for regular expression, starting before current page.
    !> \[view_search_last_regexp_forward]        searches forward for last regular expression.
    !> p        searches backward for last regular expression.
    !> \[view_quit]        quit View mode, restoring this window and buffer to previous state.
    !>           \[view_quit] is the normal way to leave view mode.
    !> \[view_exit]        exit View mode but stay in current buffer.  Use this if you started
    !>           viewing a buffer (file) and find out you want to edit it.
    !>           This command restores the previous read-only status of the buffer.
    !> \[view_exit_and_edit]        exit View mode, and make the current buffer editable
    !>           even if it was not editable before entry to View mode.
    !> \[view_quit_all]        quit View mode, restoring all windows to previous state.
    !> \[view_leave]        quit View mode and maybe switch buffers, but don't kill this buffer.
    !> \[view_kill_and_leave]        quit View mode, kill current buffer and go back to other buffer.
    !>
    !> The effect of \[view_leave], \[view_quit] and \[view_kill_and_leave] depends on how view_mode was entered.  If it was
    !> entered by view_file, view_file_other_window, or the Dired mode v command,
    !> then \[view_quit] will try to kill the current buffer.
    !> If view_mode was entered from another buffer, by \[view_buffer],
    !> \[view_buffer_other_window], \[view_file], or \[view_file_other_window],
    !> then \[view_leave], \[view_quit] and \[view_kill_and_leave] will return to that buffer.
    !>
    !> Entry to view_mode runs the normal hook `view_mode_hook'.  Port of
    !> view.el:view-mode.
    recursive subroutine view_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%view_mode%command(status)
    end subroutine view_mode

    !> The body of `view_mode': view--enable or view--disable.
    recursive subroutine view_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        status = status_type()
        if (enable) then
            call view_enable()
        else
            call view_disable(status)
        end if
    end subroutine view_mode_body

    !> Port of view.el:view--enable (`read_only_mode__state' is not kept).
    subroutine view_enable()
        integer :: k
        ! Always leave view mode before changing major mode.
        ! This is to guarantee that the buffer-read-only variable is restored.
        call change_major_mode_hook%add(view_disable, local=.true.)
        k = local_index(.true.)
        locals(k)%has_page_size = .false.
        locals(k)%has_half_page_size = .false.
        locals(k)%old_read_only = buffers(cur)%read_only
        buffers(cur)%read_only = .true.
    end subroutine view_enable

    !> Port of view.el:view--disable (no overlays; `read_only_mode__state'
    !> is not kept).
    recursive subroutine view_disable(status)
        type(status_type), intent(out) :: status
        integer :: k
        status = status_type()
        call change_major_mode_hook%remove(view_disable, local=.true.)
        ! Calling read-only-mode while View mode is enabled
        ! sets view-read-only to t as a buffer-local variable
        ! after exiting View mode.  That arranges that the next read-only-mode
        ! will reenable View mode.
        ! Canceling View mode in any other way should cancel that, too,
        ! so that View mode stays off if read-only-mode is called.
        k = local_index(.false.)
        if (k > 0) locals(k)%has_view_read_only = .false.
        if (buffers(cur)%read_only) then
            buffers(cur)%read_only = .false.
            if (k > 0) buffers(cur)%read_only = locals(k)%old_read_only
        end if
    end subroutine view_disable

    !> Enter View mode and set up exit from view mode depending on optional arguments.
    !>
    !> Optional argument EXIT_ACTION, if present, must specify a
    !> procedure that takes a buffer as argument.  This procedure will be
    !> called by `view_mode_exit'.
    !>
    !> For a list of all View commands, type H or h while viewing.
    !>
    !> This function runs the normal hook `view_mode_hook'.  Port of
    !> view.el:view-mode-enter (QUIT-RESTORE is deferred).
    recursive subroutine view_mode_enter(status, exit_action)
        type(status_type), intent(out) :: status
        procedure(view_exit_action_proc), optional :: exit_action
        integer :: k
        status = status_type()
        if (present(exit_action)) then
            k = local_index(.true.)
            locals(k)%exit_action => exit_action
        end if
        if (.not. view_mode_enabled()) then
            call minor_modes%view_mode%enable(status)
            if (.not. status%ok()) return
            ! Avoid spamming the echo area if `view_mode' is entered
            ! non-interactively, e.g., in a temporary buffer (bug#44629).
            if (.not. view_inhibit_help_message .and. .not. nilp(this_command)) then
                call message(substitute_command_keys("View mode: type \[help_command] for help, "// &
                                                     "\[describe_mode] for commands, \[view_quit] to quit."))
            end if
        end if
    end subroutine view_mode_enter

    !> Exit View mode in various ways.
    !> If all arguments are false, remove the current buffer from the
    !> selected window using the `quit_restore' information associated
    !> with the selected window.  If optional argument ALL_WINDOWS or
    !> `view_exits_all_viewing_windows' are true, remove the current
    !> buffer from all windows showing it.
    !>
    !> Optional argument EXIT_ONLY true means just exit `view_mode'
    !> (unless `view_no_disable_on_exit' is true) but do not change
    !> the associations of any windows with the current buffer.
    !>
    !> EXIT_ACTION, if present, must specify a procedure that is called
    !> with the current buffer as argument and is called after disabling
    !> `view_mode' and removing any associations of windows with the
    !> current buffer.  Port of view.el:view-mode-exit.
    recursive subroutine view_mode_exit(status, exit_only, exit_action, all_windows)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: exit_only, all_windows
        procedure(view_exit_action_proc), optional :: exit_action
        type(handle_type) :: buffer
        type(handle_type), allocatable :: wins(:)
        logical :: only, all, old_read_only
        integer :: k
        status = status_type()
        only = .false.
        if (present(exit_only)) only = exit_only
        all = .false.
        if (present(all_windows)) all = all_windows
        if (.not. view_mode_enabled()) return
        buffer = window_buffer(selected_window())
        old_read_only = .false.
        k = local_index(.false.)
        if (k > 0) old_read_only = locals(k)%old_read_only
        ! When `view_read_only' is true, we don't want to leave a buffer
        ! read-only but not in `view_mode'.
        if (.not. (view_no_disable_on_exit .or. (view_read_only_value() .and. old_read_only))) then
            call minor_modes%view_mode%disable(status)
            if (.not. status%ok()) return
        end if
        if (.not. only) then
            if (all .or. view_exits_all_viewing_windows) then
                wins = buffer_window_list(current_buffer())
                do k = 1, size(wins)
                    if (window_live_p(wins(k))) call quit_window(.false., status, wins(k))
                    if (.not. status%ok()) return
                end do
            else if (same_handle(window_buffer(selected_window()), current_buffer())) then
                call quit_window(.false., status)
                if (.not. status%ok()) return
            end if
            if (present(exit_action)) call exit_action(buffer, status)
        end if
    end subroutine view_mode_exit

    !> The buffer shown in WINDOW.
    function window_buffer(window) result(buffer)
        type(handle_type), intent(in) :: window
        type(handle_type) :: buffer
        buffer = handle_type()
        if (window_live_p(window)) buffer = windows(window%index)%buffer
    end function window_buffer

    !> The windows of the selected frame showing BUFFER, the selected
    !> window first, then in cyclic order.  Port of
    !> window.el:get-buffer-window-list (MINIBUF and ALL-FRAMES nil).
    function buffer_window_list(buffer) result(list)
        type(handle_type), intent(in) :: buffer
        type(handle_type), allocatable :: list(:)
        type(handle_type), allocatable :: leaves(:)
        integer :: k, n, first, j
        call window_leaves(selected_frame%root_window, leaves)
        allocate (list(0))
        n = size(leaves)
        first = 1
        do k = 1, n
            if (same_handle(leaves(k), selected_window())) first = k
        end do
        do j = 0, n - 1
            k = mod(first - 1 + j, n) + 1
            if (same_handle(windows(leaves(k)%index)%buffer, buffer)) list = [list, leaves(k)]
        end do
    end function buffer_window_list

    !> Exit View mode but stay in current buffer.  Port of
    !> view.el:View-exit.
    recursive subroutine view_exit(status)
        type(status_type), intent(out) :: status
        logical :: saved_no_disable, saved_global, had_local, saved_local
        integer :: k
        ! (let ((view-read-only nil) (view-no-disable-on-exit nil)) ...)
        saved_no_disable = view_no_disable_on_exit
        view_no_disable_on_exit = .false.
        k = local_index(.false.)
        had_local = .false.
        saved_local = .false.
        if (k > 0) then
            had_local = locals(k)%has_view_read_only
            saved_local = locals(k)%view_read_only
        end if
        saved_global = view_read_only
        if (had_local) then
            locals(k)%view_read_only = .false.
        else
            view_read_only = .false.
        end if
        call view_mode_exit(status, exit_only=.true.)
        view_no_disable_on_exit = saved_no_disable
        if (had_local) then
            k = local_index(.false.)
            if (k > 0) then
                if (locals(k)%has_view_read_only) locals(k)%view_read_only = saved_local
            end if
        else
            view_read_only = saved_global
        end if
    end subroutine view_exit

    !> Exit View mode and make the current buffer editable.  Port of
    !> view.el:View-exit-and-edit.
    recursive subroutine view_exit_and_edit(status)
        type(status_type), intent(out) :: status
        logical :: saved_no_disable, saved_old
        integer :: k
        ! (let ((view-old-buffer-read-only nil) (view-no-disable-on-exit nil)) ...)
        saved_no_disable = view_no_disable_on_exit
        view_no_disable_on_exit = .false.
        k = local_index(.true.)
        saved_old = locals(k)%old_read_only
        locals(k)%old_read_only = .false.
        call view_mode_exit(status, exit_only=.true.)
        view_no_disable_on_exit = saved_no_disable
        k = local_index(.false.)
        if (k > 0) locals(k)%old_read_only = saved_old
    end subroutine view_exit_and_edit

    !> Quit View mode and maybe switch buffers, but don't kill this buffer.
    !> Port of view.el:View-leave.
    recursive subroutine view_leave(status)
        type(status_type), intent(out) :: status
        call view_mode_exit(status)
    end subroutine view_leave

    !> Quit View mode, trying to restore window and buffer to previous state.
    !> Maybe kill this buffer.  Try to restore selected window to previous state
    !> and go to previous buffer or window.  Port of view.el:View-quit.
    recursive subroutine view_quit(status)
        type(status_type), intent(out) :: status
        procedure(view_exit_action_proc), pointer :: action
        integer :: k
        action => null()
        k = local_index(.false.)
        if (k > 0) action => locals(k)%exit_action
        if (associated(action)) then
            call view_mode_exit(status, exit_action=action)
        else
            call view_mode_exit(status)
        end if
    end subroutine view_quit

    !> Quit View mode, trying to restore windows and buffers to previous state.
    !> Maybe kill current buffer.  Try to restore all windows viewing buffer to
    !> previous state and go to previous buffer or window.  Port of
    !> view.el:View-quit-all.
    recursive subroutine view_quit_all(status)
        type(status_type), intent(out) :: status
        procedure(view_exit_action_proc), pointer :: action
        integer :: k
        action => null()
        k = local_index(.false.)
        if (k > 0) action => locals(k)%exit_action
        if (associated(action)) then
            call view_mode_exit(status, exit_action=action, all_windows=.true.)
        else
            call view_mode_exit(status, all_windows=.true.)
        end if
    end subroutine view_quit_all

    !> Quit View mode, kill current buffer and return to previous buffer.
    !> Port of view.el:View-kill-and-leave.
    recursive subroutine view_kill_and_leave(status)
        type(status_type), intent(out) :: status
        procedure(view_exit_action_proc), pointer :: action
        integer :: k
        action => kill_buffer_action
        k = local_index(.false.)
        if (k > 0) then
            if (associated(locals(k)%exit_action)) action => locals(k)%exit_action
        end if
        call view_mode_exit(status, exit_action=action, all_windows=.true.)
    end subroutine view_kill_and_leave

    ! ------------------------------------------------------------------
    ! Some help routines
    ! ------------------------------------------------------------------

    !> The height of the current window, excluding the mode line.  Port
    !> of view.el:view-window-size.
    integer(int64) function view_window_size()
        type(handle_type) :: w
        view_window_size = 1
        w = selected_window()
        if (window_live_p(w)) view_window_size = window_body_lines(windows(w%index))
    end function view_window_size

    !> Recenter point in window and redisplay normally.  Port of
    !> view.el:view-recenter.
    subroutine view_recenter()
        type(status_type) :: st
        call recenter(list(make_fixnum(1_int64)), .false., st)
    end subroutine view_recenter

    !> If LINES (HAS_LINES false for nil) is nil, 0, or larger than
    !> `view_window_size', return nil (HAS false).  Otherwise, return
    !> LINES.  Port of view.el:view-page-size-default.
    subroutine view_page_size_default(has_lines, lines, has, n)
        logical, intent(in) :: has_lines
        integer(int64), intent(in) :: lines
        logical, intent(out) :: has
        integer(int64), intent(out) :: n
        has = .false.
        n = 0
        if (.not. has_lines) return
        if (lines == 0) return
        if (abs(lines) > abs(view_window_size() - next_screen_context_lines)) return
        has = .true.
        n = abs(lines)
    end subroutine view_page_size_default

    !> Get and maybe set half page size.  Port of
    !> view.el:view-set-half-page-size-default.
    subroutine view_set_half_page_size_default(has_lines, lines, has, n)
        logical, intent(in) :: has_lines
        integer(int64), intent(in) :: lines
        logical, intent(out) :: has
        integer(int64), intent(out) :: n
        integer :: k
        k = local_index(.true.)
        if (.not. has_lines) then
            has = .true.
            if (locals(k)%has_half_page_size) then
                n = locals(k)%half_page_size
            else
                n = view_window_size()/2
            end if
            return
        end if
        if (lines == 0) then
            has = .true.
            n = view_window_size()/2
        else
            call view_page_size_default(.true., lines, has, n)
        end if
        locals(k)%has_half_page_size = has
        locals(k)%half_page_size = n
    end subroutine view_set_half_page_size_default

    !> The prefix argument as "P" gives it: HAS false for nil, else its
    !> numeric value.
    subroutine raw_prefix(has, n)
        logical, intent(out) :: has
        integer(int64), intent(out) :: n
        has = .not. nilp(current_prefix_arg)
        n = prefix_numeric_value(current_prefix_arg)
    end subroutine raw_prefix

    ! ------------------------------------------------------------------
    ! Commands for moving around in the buffer
    ! ------------------------------------------------------------------

    !> Move to end (or prefix PERCENT) of buffer in View mode.
    !> Display is centered at point.
    !> Also set the mark at the position where point was.  Port of
    !> view.el:View-goto-percent.
    subroutine view_goto_percent(status)
        type(status_type), intent(out) :: status
        logical :: has
        integer(int64) :: percent
        status = status_type()
        call raw_prefix(has, percent)
        call push_mark()
        if (has) then
            call goto_char(point_min() + floor(real(point_max() - point_min(), real64)*0.01_real64* &
                                               real(max(0_int64, min(100_int64, percent)), real64), int64))
        else
            call goto_char(point_max())
        end if
        call view_recenter()
    end subroutine view_goto_percent

    !> Move to first (or prefix LINE) line in View mode.
    !> Display is centered at LINE.
    !> Also set the mark at the position where point was.  Port of
    !> view.el:View-goto-line.
    subroutine view_goto_line(status)
        type(status_type), intent(out) :: status
        integer(int64) :: line, shortage
        status = status_type()
        line = prefix_numeric_value(current_prefix_arg)
        call push_mark()
        call goto_char(point_min())
        shortage = forward_line(line - 1)
        call view_recenter()
    end subroutine view_goto_line

    !> Return to last mark set in View mode, else beginning of file.
    !> Display that line at the center of the window.
    !> This command pops the mark ring, so that successive
    !> invocations return to earlier marks.  Port of
    !> view.el:View-back-to-mark.
    subroutine view_back_to_mark(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: pos
        status = status_type()
        call mark(pos, st, force=.true.)
        if (.not. st%ok() .or. pos <= 0) pos = point_min()
        call goto_char(pos)
        call pop_mark()
        call view_recenter()
    end subroutine view_back_to_mark

    !> This function does the job for all the scrolling commands.
    !> Scroll forward LINES lines.  If BACKWARD is true, scroll backwards.
    !> If LINES is negative scroll in the other direction.
    !> If LINES is 0 or nil, scroll DEFAULT lines (if DEFAULT is nil, scroll
    !> by one page).  If MAXDEFAULT is true, scroll no more than a window.
    !> Port of view.el:view-scroll-lines.
    recursive subroutine view_scroll_lines(has_lines_in, lines_in, backward_in, has_default, default, maxdefault, &
                                           status)
        logical, intent(in) :: has_lines_in, backward_in, has_default, maxdefault
        integer(int64), intent(in) :: lines_in, default
        type(status_type), intent(out) :: status
        logical :: has_lines, backward
        integer(int64) :: lines
        type(value_type) :: arg
        has_lines = has_lines_in
        lines = lines_in
        backward = backward_in
        if (.not. has_lines .or. lines == 0) then
            has_lines = has_default
            lines = default
        end if
        if (has_lines .and. lines < 0) then
            backward = .not. backward
            lines = -lines
        end if
        if (maxdefault .and. has_lines .and. lines > view_window_size()) has_lines = .false.
        if (has_lines) then
            arg = make_fixnum(lines)
        else
            arg = nil()
        end if
        if (backward) then
            call scroll_down_command(arg, status)
        else if (view_really_at_end()) then
            if (view_scroll_auto_exit) then
                call view_quit(status)
            else
                status = status_type()
                call ding()
                call view_end_message()
            end if
        else
            call scroll_up_command(arg, status)
            if (.not. status%ok()) return
            if (view_really_at_end()) call view_end_message()
        end if
    end subroutine view_scroll_lines

    !> Return true if buffer end visible.  Port of
    !> view.el:view-really-at-end (`view_try_extend_at_buffer_end' is
    !> deferred: false).
    logical function view_really_at_end()
        view_really_at_end = .false.
        if (.not. (.not. scroll_error_top_bottom .or. eobp())) return
        view_really_at_end = pos_visible_in_window_p(point_max(), selected_window())
    end function view_really_at_end

    !> Tell that we are at end of buffer.  Port of
    !> view.el:view-end-message.
    subroutine view_end_message()
        type(handle_type) :: w
        logical :: quit_restore
        call goto_char(point_max())
        w = selected_window()
        quit_restore = .false.
        if (window_live_p(w)) quit_restore = windows(w%index)%quit_restore%kind /= QR_NONE
        if (quit_restore) then
            if (view_scroll_auto_exit) then
                call message("End of buffer.  Type "//substitute_command_keys("\[view_scroll_page_forward]")// &
                             " to quit viewing.")
            else
                call message("End of buffer.  Type "//substitute_command_keys("\[view_quit]")//" to quit viewing.")
            end if
        else
            call message("End of buffer")
        end if
    end subroutine view_end_message

    !> Scroll backward or forward so that buffer end is at last line of
    !> window.  Port of view.el:View-scroll-to-buffer-end.
    subroutine view_scroll_to_buffer_end(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p
        logical :: visible
        visible = pos_visible_in_window_p(point_max(), selected_window())
        p = point()
        call goto_char(point_max())
        call recenter(make_fixnum(-1_int64), .false., status)
        if (visible) call goto_char(p)
    end subroutine view_scroll_to_buffer_end

    !> Scroll "page size" or prefix LINES lines forward in View mode.
    !> Exit if end of text is visible and `view_scroll_auto_exit' is true.
    !> "page size" is whole window full, or number of lines set by
    !> \[view_scroll_page_forward_set_page_size] or
    !> \[view_scroll_page_backward_set_page_size].
    !> If LINES is more than a window-full, only the last window-full is shown.
    !> Port of view.el:View-scroll-page-forward.
    recursive subroutine view_scroll_page_forward(status)
        type(status_type), intent(out) :: status
        call page_scroll(.false., .false., status)
    end subroutine view_scroll_page_forward

    !> Scroll "page size" or prefix LINES lines backward in View mode.
    !> See also `view_scroll_page_forward'.  Port of
    !> view.el:View-scroll-page-backward.
    recursive subroutine view_scroll_page_backward(status)
        type(status_type), intent(out) :: status
        call page_scroll(.true., .false., status)
    end subroutine view_scroll_page_backward

    !> Scroll forward LINES lines in View mode, setting the "page size".
    !> This is the number of lines which \[view_scroll_page_forward] and
    !> \[view_scroll_page_backward] scroll by default.
    !> If LINES is omitted or = 0, sets "page size" to window height and
    !> scrolls forward that much, otherwise scrolls forward LINES lines and sets
    !> "page size" to the minimum of window height and the absolute value of LINES.
    !> See also `view_scroll_page_forward'.  Port of
    !> view.el:View-scroll-page-forward-set-page-size.
    recursive subroutine view_scroll_page_forward_set_page_size(status)
        type(status_type), intent(out) :: status
        call page_scroll(.false., .true., status)
    end subroutine view_scroll_page_forward_set_page_size

    !> Scroll backward prefix LINES lines in View mode, setting the "page size".
    !> See also `view_scroll_page_forward_set_page_size'.  Port of
    !> view.el:View-scroll-page-backward-set-page-size.
    recursive subroutine view_scroll_page_backward_set_page_size(status)
        type(status_type), intent(out) :: status
        call page_scroll(.true., .true., status)
    end subroutine view_scroll_page_backward_set_page_size

    !> The page scrolling commands: BACKWARD, and with SET_SIZE setting
    !> `view_page_size' from the prefix argument.
    recursive subroutine page_scroll(backward, set_size, status)
        logical, intent(in) :: backward, set_size
        type(status_type), intent(out) :: status
        logical :: has_lines, has_default
        integer(int64) :: lines, default
        integer :: k
        call raw_prefix(has_lines, lines)
        k = local_index(.true.)
        if (set_size) then
            call view_page_size_default(has_lines, lines, has_default, default)
            locals(k)%has_page_size = has_default
            locals(k)%page_size = default
        else
            call view_page_size_default(locals(k)%has_page_size, locals(k)%page_size, has_default, default)
        end if
        call view_scroll_lines(has_lines, lines, backward, has_default, default, .false., status)
    end subroutine page_scroll

    !> Scroll forward one line (or prefix LINES lines) in View mode.
    !> See also `view_scroll_page_forward', but note that scrolling is limited
    !> to minimum of LINES and one window-full.  Port of
    !> view.el:View-scroll-line-forward.
    recursive subroutine view_scroll_line_forward(status)
        type(status_type), intent(out) :: status
        logical :: has_lines
        integer(int64) :: lines
        call raw_prefix(has_lines, lines)
        call view_scroll_lines(has_lines, lines, .false., .true., 1_int64, .true., status)
    end subroutine view_scroll_line_forward

    !> Scroll backward one line (or prefix LINES lines) in View mode.
    !> See also `view_scroll_line_forward'.  Port of
    !> view.el:View-scroll-line-backward.
    recursive subroutine view_scroll_line_backward(status)
        type(status_type), intent(out) :: status
        logical :: has_lines
        integer(int64) :: lines
        call raw_prefix(has_lines, lines)
        call view_scroll_lines(has_lines, lines, .true., .true., 1_int64, .true., status)
    end subroutine view_scroll_line_backward

    !> Scroll forward a "half page" (or prefix LINES) lines in View mode.
    !> If LINES is not omitted, the "half page size" is set to the minimum of
    !> window height and the absolute value of LINES.
    !> LINES=0 resets "half page size" to half window height.  Port of
    !> view.el:View-scroll-half-page-forward.
    recursive subroutine view_scroll_half_page_forward(status)
        type(status_type), intent(out) :: status
        logical :: has_lines, has_default
        integer(int64) :: lines, default
        call raw_prefix(has_lines, lines)
        call view_set_half_page_size_default(has_lines, lines, has_default, default)
        call view_scroll_lines(has_lines, lines, .false., has_default, default, .true., status)
    end subroutine view_scroll_half_page_forward

    !> Scroll backward a "half page" (or prefix LINES) lines in View mode.
    !> See also `view_scroll_half_page_forward'.  Port of
    !> view.el:View-scroll-half-page-backward.
    recursive subroutine view_scroll_half_page_backward(status)
        type(status_type), intent(out) :: status
        logical :: has_lines, has_default
        integer(int64) :: lines, default
        call raw_prefix(has_lines, lines)
        call view_set_half_page_size_default(has_lines, lines, has_default, default)
        call view_scroll_lines(has_lines, lines, .true., has_default, default, .true., status)
    end subroutine view_scroll_half_page_backward

    ! ------------------------------------------------------------------
    ! Searching
    ! ------------------------------------------------------------------

    !> Search forward for first (or prefix Nth) occurrence of REGEXP in View mode.
    !>
    !> Displays line found at center of window.  Sets mark at starting position and
    !> pushes mark ring.
    !>
    !> Characters @ and ! are special at the beginning of REGEXP.  They modify
    !> the search rather than become part of the pattern searched for.
    !> @ means search all the buffer i.e. start search at the beginning of buffer.
    !> ! means search for a line that contains no match for the pattern.
    !> If REGEXP is empty or only consist of these control characters, then
    !> an earlier remembered REGEXP is used, otherwise REGEXP is remembered
    !> for use by later search commands.  Port of
    !> view.el:View-search-regexp-forward (the match is not highlighted).
    recursive subroutine view_search_regexp_forward(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer(int64) :: n
        n = prefix_numeric_value(current_prefix_arg)
        call read_string("Search forward (regexp): ", regexp, status)
        if (.not. status%ok()) return
        call view_search(n, status, regexp)
    end subroutine view_search_regexp_forward

    !> Search backward for first (or prefix Nth) occurrence of REGEXP in View mode.
    !>
    !> Displays line found at center of window.  Sets mark at starting position and
    !> pushes mark ring.
    !>
    !> Characters @ and ! are special at the beginning of REGEXP.  They modify
    !> the search rather than become part of the pattern searched for.
    !> @ means search all the buffer i.e. start search at the end of buffer.
    !> ! means search for a line that contains no match for the pattern.
    !> If REGEXP is empty or only consist of these control characters, then
    !> an earlier remembered REGEXP is used, otherwise REGEXP is remembered
    !> for use by later search commands.  Port of
    !> view.el:View-search-regexp-backward (the match is not highlighted).
    recursive subroutine view_search_regexp_backward(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer(int64) :: n
        n = prefix_numeric_value(current_prefix_arg)
        call read_string("Search backward (regexp): ", regexp, status)
        if (.not. status%ok()) return
        call view_search(-n, status, regexp)
    end subroutine view_search_regexp_backward

    !> Search forward for first (or prefix Nth) instance of last regexp in View mode.
    !> Displays line found at center of window.  Sets mark at starting position and
    !> pushes mark ring.  Port of view.el:View-search-last-regexp-forward
    !> (the match is not highlighted).
    recursive subroutine view_search_last_regexp_forward(status)
        type(status_type), intent(out) :: status
        call view_search(prefix_numeric_value(current_prefix_arg), status)
    end subroutine view_search_last_regexp_forward

    !> Search backward for first (or prefix Nth) instance of last regexp in View mode.
    !> Displays line found at center of window.  Sets mark at starting position and
    !> pushes mark ring.  Port of view.el:View-search-last-regexp-backward
    !> (the match is not highlighted).
    recursive subroutine view_search_last_regexp_backward(status)
        type(status_type), intent(out) :: status
        call view_search(-prefix_numeric_value(current_prefix_arg), status)
    end subroutine view_search_last_regexp_backward

    !> This function does the job for all the View-search- commands.
    !> Search for the TIMESth match for REGEXP.  If TIMES is negative
    !> search backwards.  If REGEXP is absent use `view_last_regexp'.
    !> Characters "!" and "@" have a special meaning at the beginning of
    !> REGEXP and are removed from REGEXP before the search "!" means
    !> search for lines with no match for REGEXP.  "@" means search in
    !> the whole buffer, don't start searching from the present point.
    !> Port of view.el:view-search (no overlay highlighting).
    recursive subroutine view_search(times, status, regexp_in)
        integer(int64), intent(in) :: times
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: regexp_in
        character(len=:), allocatable :: regexp
        character(len=24) :: timestr
        integer(int64) :: where, opoint, found
        type(status_type) :: st
        logical :: no, end, have, first
        integer :: k, ln
        status = status_type()
        no = .false.
        end = .false.
        k = local_index(.true.)
        first = .false.
        if (present(regexp_in)) then
            regexp = regexp_in
            if (len(regexp) > 0) then
                if (regexp(1:1) /= "!" .and. regexp(1:1) /= "@") then
                    first = .true.
                else
                    if (len(regexp) < 2) then
                        call set_error(status, FM_ARGS_OUT_OF_RANGE, fortran_string(regexp)//", 0, 2")
                        return
                    end if
                    if (regexp(1:2) == "!@" .or. regexp(1:2) == "@!") then
                        end = .true.
                        no = .true.
                        ln = 2
                    else
                        end = regexp(1:1) == "@"
                        no = .not. end
                        ln = 1
                    end if
                    regexp = regexp(ln + 1:)
                    first = len(regexp) > 0
                end if
            end if
        end if
        have = .true.
        if (first) then
            locals(k)%has_last_regexp = .true.
            locals(k)%last_regexp = regexp
            locals(k)%last_no = no
        else if (locals(k)%has_last_regexp .and. locals(k)%last_no) then
            regexp = locals(k)%last_regexp
            no = .not. no
            if (.not. no) locals(k)%last_no = .false.
        else if (locals(k)%has_last_regexp) then
            regexp = locals(k)%last_regexp
            if (no) locals(k)%last_no = .true.
        else
            have = .false.
        end if
        if (.not. have) then
            call set_error(status, FM_GENERIC_ERROR, "No previous View-mode search")
            return
        end if
        where = 0
        opoint = point()
        if (end) then
            if (times < 0) then
                call goto_char(point_max())
            else
                call goto_char(point_min())
            end if
        else if (times < 0) then
            call beginning_of_line()
        else
            call end_of_line()
        end if
        if (no) then
            if (view_search_no_match_lines(times, regexp, st)) where = point()
        else
            call re_search_forward(regexp, found, st, noerror=SEARCH_NOERROR, count=times)
            if (st%ok() .and. found /= 0) where = point()
        end if
        call goto_char(opoint)
        if (.not. st%ok()) then
            status = st
            return
        end if
        if (where /= 0) then
            call push_mark()
            call goto_char(where)
            call beginning_of_line()
            call view_recenter()
        else
            write (timestr, "(i0)") times
            if (no) then
                call message("Can't find occurrence "//trim(timestr)//" of no "//regexp)
            else
                call message("Can't find occurrence "//trim(timestr)//" of "//regexp)
            end if
            call sit_for(4000)
        end if
    end subroutine view_search

    !> Search for the TIMESth occurrence of a line with no match for REGEXP.
    !> If such a line is found, return true and set the match data to that line.
    !> If TIMES is negative, search backwards.  Port of
    !> view.el:view-search-no-match-lines.
    recursive logical function view_search_no_match_lines(times_in, regexp, status) result(found_line)
        integer(int64), intent(in) :: times_in
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer(int64) :: times, step, found
        status = status_type()
        times = times_in
        if (times >= 0) then
            step = 1
        else
            times = -times
            step = -1
        end if
        ! Note that we do not check the current line.
        do while (times > 0)
            if (forward_line(step) /= 0) exit
            ! (forward-line 1) returns 0 on moving within the last line.
            if (eobp()) then
                times = -1
            else
                call re_search_forward(regexp, found, status, bound=line_end_position(), noerror=SEARCH_NOERROR)
                if (.not. status%ok()) then
                    found_line = .false.
                    return
                end if
                if (found == 0) times = times - 1
            end if
        end do
        found_line = .false.
        if (times == 0) then
            found_line = looking_at(".*", status)
            found_line = found_line .and. status%ok()
        end if
    end function view_search_no_match_lines

    ! ------------------------------------------------------------------
    ! Initialization
    ! ------------------------------------------------------------------

    !> Define View mode, its keymap and commands.
    subroutine syms_of_view()
        type(status_type) :: st
        integer :: d
        character(len=1) :: digit
        call minor_modes%view_mode%define("view_mode", " View", hook=view_mode_hook, keymap=.true., &
                                          body=view_mode_body)
        view_mode_map = minor_modes%view_mode%map
        call register_keymap_name("view_mode_map", view_mode_map)
        call add_local_variables_killer(kill_view_locals)
        commands%view_mode%run => view_mode
        commands%view_file%run => cmd_view_file
        commands%view_file_other_window%run => cmd_view_file_other_window
        commands%view_buffer%run => cmd_view_buffer
        commands%view_buffer_other_window%run => cmd_view_buffer_other_window
        commands%view_exit%run => view_exit
        commands%view_exit_and_edit%run => view_exit_and_edit
        commands%view_leave%run => view_leave
        commands%view_quit%run => view_quit
        commands%view_quit_all%run => view_quit_all
        commands%view_kill_and_leave%run => view_kill_and_leave
        commands%view_goto_percent%run => view_goto_percent
        commands%view_goto_line%run => view_goto_line
        commands%view_back_to_mark%run => view_back_to_mark
        commands%view_scroll_to_buffer_end%run => view_scroll_to_buffer_end
        commands%view_scroll_page_forward%run => view_scroll_page_forward
        commands%view_scroll_page_backward%run => view_scroll_page_backward
        commands%view_scroll_page_forward_set_page_size%run => view_scroll_page_forward_set_page_size
        commands%view_scroll_page_backward_set_page_size%run => view_scroll_page_backward_set_page_size
        commands%view_scroll_line_forward%run => view_scroll_line_forward
        commands%view_scroll_line_backward%run => view_scroll_line_backward
        commands%view_scroll_half_page_forward%run => view_scroll_half_page_forward
        commands%view_scroll_half_page_backward%run => view_scroll_half_page_backward
        commands%view_search_regexp_forward%run => view_search_regexp_forward
        commands%view_search_regexp_backward%run => view_search_regexp_backward
        commands%view_search_last_regexp_forward%run => view_search_last_regexp_forward
        commands%view_search_last_regexp_backward%run => view_search_last_regexp_backward
        call keymap_set(view_mode_map, "C", "view_kill_and_leave", st)
        call keymap_set(view_mode_map, "c", "view_leave", st)
        call keymap_set(view_mode_map, "Q", "view_quit_all", st)
        call keymap_set(view_mode_map, "E", "view_exit_and_edit", st)
        call keymap_set(view_mode_map, "e", "view_exit", st)
        call keymap_set(view_mode_map, "q", "view_quit", st)
        call keymap_set(view_mode_map, "p", "view_search_last_regexp_backward", st)
        call keymap_set(view_mode_map, "n", "view_search_last_regexp_forward", st)
        call keymap_set(view_mode_map, "\", "view_search_regexp_backward", st)
        call keymap_set(view_mode_map, "/", "view_search_regexp_forward", st)
        call keymap_set(view_mode_map, "r", "isearch_backward", st)
        call keymap_set(view_mode_map, "s", "isearch_forward", st)
        call keymap_set(view_mode_map, "m", "point_to_register", st)
        call keymap_set(view_mode_map, "'", "jump_to_register", st)
        call keymap_set(view_mode_map, "x", "exchange_point_and_mark", st)
        call keymap_set(view_mode_map, "@", "view_back_to_mark", st)
        call keymap_set(view_mode_map, ".", "set_mark_command", st)
        call keymap_set(view_mode_map, "%", "view_goto_percent", st)
        call keymap_set(view_mode_map, "g", "view_goto_line", st)
        call keymap_set(view_mode_map, "=", "what_line", st)
        call keymap_set(view_mode_map, "y", "view_scroll_line_backward", st)
        call keymap_set(view_mode_map, "C-j", "view_scroll_line_forward", st)
        call keymap_set(view_mode_map, "RET", "view_scroll_line_forward", st)
        call keymap_set(view_mode_map, "u", "view_scroll_half_page_backward", st)
        call keymap_set(view_mode_map, "d", "view_scroll_half_page_forward", st)
        call keymap_set(view_mode_map, "z", "view_scroll_page_forward_set_page_size", st)
        call keymap_set(view_mode_map, "w", "view_scroll_page_backward_set_page_size", st)
        call keymap_set(view_mode_map, "DEL", "view_scroll_page_backward", st)
        call keymap_set(view_mode_map, "SPC", "view_scroll_page_forward", st)
        call keymap_set(view_mode_map, "S-SPC", "view_scroll_page_backward", st)
        call keymap_set(view_mode_map, "o", "view_scroll_to_buffer_end", st)
        call keymap_set(view_mode_map, ">", "end_of_buffer", st)
        call keymap_set(view_mode_map, "<", "beginning_of_buffer", st)
        call keymap_set(view_mode_map, "-", "negative_argument", st)
        do d = 9, 0, -1
            write (digit, "(i1)") d
            call keymap_set(view_mode_map, digit, "digit_argument", st)
        end do
    end subroutine syms_of_view
end module view_fm
