! window_fm.f90 -- windows: a buffer shown in part of a frame.
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
module window_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type, null_handle, slot_table_type
    use buffer_fm, only: buffers, resolve_buffer
    use marker_fm, only: make_marker, free_marker, set_marker, marker_position
    use lisp_fm, only: value_type
    implicit none(type, external)
    private

    public :: set_window_buffer, set_window_buffer_internal, window_discard_buffer_from_window, record_window_buffer, &
              switch_to_prev_buffer, switch_to_next_buffer, quit_restore_window, bury_buffer, &
              display_buffer_record_window, display_buffer, pop_to_buffer, pop_to_buffer_same_window, &
              temp_buffer_window_show, get_buffer_window, get_lru_window, get_mru_window, get_largest_window, &
              split_window_and_copy, copy_quit_restore, window_resize, syms_of_window_display
    public :: window_start, set_window_start, window_point, &
              set_window_point, window_body_lines, free_window, free_prev_buffer, free_quit_restore
    public :: make_window, delete_window_object, window_ref, window_live_p, window_valid_p, same_window, &
              window_leaves, resize_window_tree, window_text_cols, split_window_internal, &
              remove_window_from_tree, replace_window, delete_window_tree
    public :: scroll_up, scroll_down, recenter, move_to_window_line, &
              scroll_up_command, scroll_down_command, recenter_top_bottom, &
              move_to_window_line_top_bottom, quit_window, syms_of_window
    public :: select_window, split_window, delete_window, delete_other_windows, other_window, other_buffer, &
              replace_buffer_in_windows, switch_to_buffer, syms_of_window_cmds

    !> Number of lines of continuity when scrolling by screenfuls.
    !> Port of window.c next-screen-context-lines.
    integer, public :: next_screen_context_lines = 2

    !> Move point on scroll error if non-nil.
    !> If nil, scroll commands signal an error when no more scrolling is
    !> possible.  If non-nil, they move point to the beginning or end of
    !> the buffer instead (and then signal at the edge).  Port of
    !> window.el scroll-error-top-bottom.
    logical, public :: scroll_error_top_bottom = .false.

    !> A buffer a window showed before (window.h prev_buffers element):
    !> its window start and point when it was removed, as markers.
    type, public :: prev_buffer_type
        type(handle_type) :: buffer, start, point
    end type prev_buffer_type

    !> What a window's `quit-restore' parameter records (see
    !> window.el:display-buffer-record-window): how the window came to
    !> show `buffer' (QR_WINDOW, QR_FRAME, QR_SAME or QR_OTHER; QR_NONE
    !> for no parameter), what to restore (QR_WINDOW, QR_FRAME, or with
    !> `has_quad' the buffer shown before with its start, point and
    !> size), and the window selected at the time.
    type, public :: quit_restore_type
        integer :: kind = 0
        integer :: what = 0
        logical :: has_quad = .false.
        type(handle_type) :: quad_buffer, quad_point
        integer(int64) :: quad_start = 0
        !> The window's height or width then; -1 for none (nil).
        integer :: quad_size = -1
        type(handle_type) :: selected, buffer
    end type quit_restore_type

    !> Kinds of `quit-restore' parameter.
    integer, parameter, public :: QR_NONE = 0, QR_WINDOW = 1, QR_FRAME = 2, QR_SAME = 3, QR_OTHER = 4

    !> Dedication of a window to its buffer: nil, weak (non-nil), strong
    !> (t).
    integer, parameter, public :: DEDICATED_NONE = 0, DEDICATED_WEAK = 1, DEDICATED_STRONG = 2

    !> What `quit-restore-window' and `switch-to-prev-buffer' do with the
    !> window's buffer (BURY-OR-KILL): nothing, `append', `bury', `kill',
    !> `killing' or `burying'.
    integer, parameter, public :: BK_NONE = 0, BK_APPEND = 1, BK_BURY = 2, BK_KILL = 3, BK_KILLING = 4, &
                                  BK_BURYING = 5

    !> Actions for `display_buffer': none, display-buffer--same-window-action
    !> and display-buffer--other-window-action.
    integer, parameter, public :: DISPLAY_DEFAULT = 0, DISPLAY_SAME_WINDOW = 1, DISPLAY_OTHER_WINDOW = 2

    !> Values of `split_window_preferred_direction'.
    integer, parameter, public :: SPLIT_LONGEST = 0, SPLIT_VERTICAL = 1, SPLIT_HORIZONTAL = 2

    !> Minimum height for splitting windows sensibly.
    !> If this is an integer, `split-window-sensibly' may split a window
    !> vertically only if it has at least this many lines.  If, however, a
    !> window is the only window on its frame, or all the other ones are
    !> dedicated, `split-window-sensibly' may split it vertically
    !> disregarding the value of this variable.  Port of window.el
    !> split-height-threshold.
    integer, public :: split_height_threshold = 80

    !> Minimum width for splitting windows sensibly.
    !> If this is an integer, `split-window-sensibly' may split a window
    !> horizontally only if it has at least this many columns.  Port of
    !> window.el split-width-threshold.
    integer, public :: split_width_threshold = 150

    !> The first direction tried when Emacs needs to split a window.
    !> This variable controls in which order `split-window-sensibly' will
    !> try to split the window.  If set to `vertical' (SPLIT_VERTICAL),
    !> `split-window-sensibly' tries to split vertically first and then
    !> horizontally.  If set to `horizontal' (SPLIT_HORIZONTAL) it does the
    !> opposite.  If set to `longest' (SPLIT_LONGEST, the default), the
    !> first direction tried depends on the frame shape: in landscape
    !> orientation it will be like `horizontal', but in portrait it will be
    !> like `vertical'.  Port of window.el split-window-preferred-direction.
    integer, public :: split_window_preferred_direction = SPLIT_LONGEST

    !> Non-nil means `display-buffer' should make a new window.  Port of
    !> window.el pop-up-windows.
    logical, public :: pop_up_windows = .true.

    !> If non-nil `display-buffer' will try to even window sizes.
    !> Otherwise `display-buffer' will leave the window configuration
    !> alone.  Port of window.el even-window-sizes (`height-only' and
    !> `width-only' are deferred).
    logical, public :: even_window_sizes = .true.

    !> The minimum total height, in lines, of any window.
    !> The value has to accommodate one text line, a mode and header line,
    !> a horizontal scroll bar and a bottom divider, if present.  A value
    !> less than `window-safe-min-height' is ignored.  Port of window.el
    !> window-min-height.
    integer, public :: window_min_height = 4

    !> The minimum total width, in columns, of any window.
    !> The value has to accommodate two text columns as well as margins,
    !> fringes, a scroll bar and a right divider, if present.  A value less
    !> than `window-safe-min-width' is ignored.  Port of window.el
    !> window-min-width.
    integer, public :: window_min_width = 10

    !> window-safe-min-height and the safe minimum width: the least a
    !> window may get when split with a SIZE.
    integer, parameter :: window_safe_min_height = 2, window_safe_min_width = 2

    !> The use time of the most recently selected window (window.c
    !> window_select_count).
    integer(int64) :: window_select_count = 0

    !> Port of the parts of window.h:struct window in use.
    type, public :: window_type
        !> The buffer shown (contents); the null handle for none.
        type(handle_type) :: buffer
        !> Markers: where the display starts, and point when not selected.
        type(handle_type) :: start, pointm
        !> Position on the frame (0-based rows) and size, in lines and
        !> columns, including the mode line.
        integer :: top_line = 0, total_lines = 0, left_col = 0, total_cols = 0
        !> The buffers shown before, most recently removed first
        !> (`window-prev-buffers').
        type(prev_buffer_type), allocatable :: prev_buffers(:)
        !> Buffers recently replaced by `switch-to-prev-buffer'
        !> (`window-next-buffers').
        type(handle_type), allocatable :: next_buffers(:)
        !> The `quit-restore' and `quit-restore-prev' parameters.
        type(quit_restore_type) :: quit_restore, quit_restore_prev
        !> Whether the window is dedicated to its buffer.
        integer :: dedicated = DEDICATED_NONE
        !> The window tree: the parent (internal) window, the next and
        !> previous siblings, and for an internal window its first child;
        !> null handles for none.  Port of window.h parent, next, prev and
        !> contents (a window with a child shows no buffer).
        type(handle_type) :: parent, next, prev, child
        !> An internal window whose children are side by side (a horizontal
        !> combination); else they are one above the other.
        logical :: horizontal = .false.
        !> When the window was last selected (window.h use_time).
        integer(int64) :: use_time = 0
        !> The window ends before the frame's right edge, so its last column
        !> holds the vertical border (a terminal's window divider).
        logical :: divider = .false.
        !> The minibuffer window: no mode line.
        logical :: mini = .false.
        !> Where redisplay put the cursor, relative to the window; -1: none.
        integer :: cursor_vpos = -1, cursor_hpos = -1
        !> Columns scrolled off the left (only the minibuffer window
        !> scrolls horizontally so far: auto-hscroll).
        integer :: hscroll = 0
        !> The start was set explicitly; redisplay must keep it.
        logical :: force_start = .false.
        !> Last display ended at window end (for "Bot"/"All" in the mode line).
        integer(int64) :: window_end_pos = 0
        logical :: window_end_valid = .false.
    end type window_type

    !> The window registry, indexed by handle (windows are objects with
    !> explicit lifetimes: make_window, delete_window_object).
    type(window_type), allocatable, target, public :: windows(:)
    type(slot_table_type) :: wtable

    ! Scrolling and recentering, in submodule window_scroll_fm: they
    ! move by screen lines (indent_fm), which depends on this module.
    interface
        !> Scroll text of selected window upward ARG lines.
        !> If ARG is omitted or nil, scroll upward by a near full screen.
        !> A near full screen is `next-screen-context-lines' less than a
        !> full screen.  Negative ARG means scroll downward.  If ARG is the
        !> atom `-', scroll downward by nearly full screen.  When calling
        !> from a program, supply as argument a number, nil, or `-'.
        !> Port of window.c:Fscroll_up.
        module subroutine scroll_up(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_up

        !> Scroll text of selected window down ARG lines.
        !> If ARG is omitted or nil, scroll down by a near full screen.
        !> A near full screen is `next-screen-context-lines' less than a
        !> full screen.  Negative ARG means scroll upward.  If ARG is the
        !> atom `-', scroll upward by nearly full screen.  When calling
        !> from a program, supply as argument a number, nil, or `-'.
        !> Port of window.c:Fscroll_down.
        module subroutine scroll_down(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_down

        !> Center point in selected window and maybe redisplay frame.
        !> With a numeric prefix argument ARG, recenter putting point on
        !> screen line ARG relative to the selected window.  If ARG is
        !> negative, it counts up from the bottom of the window.  (ARG
        !> should be less than the height of the window.)
        !>
        !> If ARG is omitted or nil, then recenter with point on the middle
        !> line of the selected window; if REDISPLAY is true, also erase the
        !> entire frame and redraw it.  Interactively, REDISPLAY is always
        !> true.
        !>
        !> Just C-u as prefix means put point in the center of the window
        !> and redisplay normally--don't erase and redraw the frame.
        !> Port of window.c:Frecenter (`recenter-redisplay' is `tty').
        module subroutine recenter(arg, redisplay, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            logical, intent(in) :: redisplay
            type(status_type), intent(out) :: status
        end subroutine recenter

        !> Position point relative to window.
        !> ARG nil means position point at center of window.
        !> Else, ARG specifies vertical position within the window;
        !> zero means top of window, negative means relative to bottom
        !> of window, -1 meaning the last fully visible display line
        !> of the window.
        !>
        !> `line' is the screen line of the window point moved to, counting
        !> from the top of the window.  Port of window.c:Fmove_to_window_line.
        module subroutine move_to_window_line(arg, line, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            integer(int64), intent(out) :: line
            type(status_type), intent(out) :: status
        end subroutine move_to_window_line

        !> Scroll text of selected window upward ARG lines; or near full
        !> screen if no ARG.  Interactively, giving this command a numerical
        !> prefix will scroll up by that many lines (and down by that many
        !> lines if the number is negative).  Without a prefix, scroll up by
        !> a full screen.  If given a `C-u -' prefix, scroll a full page
        !> down instead.
        !>
        !> If `scroll-error-top-bottom' is non-nil and `scroll-up' cannot
        !> scroll window further, move cursor to the bottom line.
        !> When point is already on that position, then signal an error.
        !> A near full screen is `next-screen-context-lines' less than a
        !> full screen.  Negative ARG means scroll downward.
        !>
        !> If ARG is the atom `-', scroll downward by nearly full screen.
        !>
        !> The command \[set-goal-column] can be used to create a
        !> semipermanent goal column for this command.  Port of
        !> window.el:scroll-up-command.
        recursive module subroutine scroll_up_command(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_up_command

        !> Scroll text of selected window down ARG lines; or near full
        !> screen if no ARG.  Interactively, giving this command a numerical
        !> prefix will scroll down by that many lines (and up by that many
        !> lines if the number is negative).  Without a prefix, scroll down
        !> by a full screen.  If given a `C-u -' prefix, scroll a full page
        !> up instead.
        !>
        !> If `scroll-error-top-bottom' is non-nil and `scroll-down' cannot
        !> scroll window further, move cursor to the top line.
        !> When point is already on that position, then signal an error.
        !> A near full screen is `next-screen-context-lines' less than a
        !> full screen.  Negative ARG means scroll upward.
        !>
        !> If ARG is the atom `-', scroll upward by nearly full screen.
        !>
        !> The command \[set-goal-column] can be used to create a
        !> semipermanent goal column for this command.  Port of
        !> window.el:scroll-down-command.
        recursive module subroutine scroll_down_command(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine scroll_down_command

        !> Scroll the window so that current line is in the middle of the
        !> window.  Successive invocations scroll the window in a cyclical
        !> order to put the current line at certain places within the
        !> window, as determined by `recenter-positions'.  By default, the
        !> second invocation puts the current line at the top-most window
        !> line, the third invocation puts it on the bottom-most window
        !> line, and then the order is reused in a cyclical manner.
        !>
        !> With numeric prefix ARG, move current line ARG lines below the
        !> window top.  With plain \[universal-argument], move current line
        !> to window center.  Port of window.el:recenter-top-bottom.
        module subroutine recenter_top_bottom(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine recenter_top_bottom

        !> Position point relative to window.
        !>
        !> With a prefix argument ARG, acts like `move-to-window-line'.
        !>
        !> With no argument, positions point at center of window.
        !> Successive calls position point at positions defined
        !> by `recenter-positions'.  Port of
        !> window.el:move-to-window-line-top-bottom.
        module subroutine move_to_window_line_top_bottom(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine move_to_window_line_top_bottom

        !> Register the scrolling commands and `quit-window'.
        module subroutine syms_of_window()
            implicit none(type, external)
        end subroutine syms_of_window

        ! The window tree and buffer display, in submodule window_cmds_fm.

        !> Select WINDOW which must be a live window.  Also make WINDOW's
        !> buffer current and make WINDOW the frame's selected window; point
        !> becomes WINDOW's point.  Unless `norecord', record the buffer as
        !> most recently selected.  Port of window.c:Fselect_window.
        module subroutine select_window(h, status, norecord)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: norecord
        end subroutine select_window

        !> Make a new window adjacent to WINDOW: below it, or with
        !> `horizontal' to its right.  Without `have_size' both get half its
        !> size (or close to it); a nonnegative SIZE is the new size of
        !> WINDOW, a negative one minus the size of the new window, in lines
        !> or columns.  The new window shows WINDOW's buffer, start and
        !> point.  Port of window.el:split-window (no SIDE above or left,
        !> no resizing of the combination).
        module subroutine split_window(window, size, have_size, horizontal, new, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            integer, intent(in) :: size
            logical, intent(in) :: have_size, horizontal
            type(handle_type), intent(out) :: new
            type(status_type), intent(out) :: status
        end subroutine split_window

        !> Delete WINDOW: its space goes to a neighbour, and if it was
        !> selected the most recently used window is selected.  Port of
        !> window.el:delete-window.
        module subroutine delete_window(window, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            type(status_type), intent(out) :: status
        end subroutine delete_window

        !> Make WINDOW fill its frame.  Port of
        !> window.el:delete-other-windows.
        module subroutine delete_other_windows(window, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            type(status_type), intent(out) :: status
        end subroutine delete_other_windows

        !> Select another window in cyclic ordering of windows.
        !> COUNT specifies the number of windows to skip, starting with the
        !> selected window, before making the selection.  If COUNT is
        !> positive, skip COUNT windows forwards.  If COUNT is negative,
        !> skip -COUNT windows backwards.  Port of window.el:other-window.
        module subroutine other_window(count, status)
            implicit none(type, external)
            integer(int64), intent(in) :: count
            type(status_type), intent(out) :: status
        end subroutine other_window

        !> Return most recently selected buffer other than BUFFER.
        !> Buffers not visible in windows are preferred to visible buffers.
        !> Ignore internal buffers (names starting with a space).  If no
        !> other buffer exists, return the buffer `*scratch*' (creating it
        !> if necessary).  Port of buffer.c:Fother_buffer (VISIBLE-OK nil).
        module function other_buffer(buffer) result(h)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(handle_type) :: h
        end function other_buffer

        !> Display buffer BUFFER-OR-NAME in the selected window, making it
        !> current; a name of no live buffer makes a new buffer of that name.
        !> Port of window.el:switch-to-buffer.
        module subroutine switch_to_buffer(name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
        end subroutine switch_to_buffer

        !> Register the window and buffer commands.
        module subroutine syms_of_window_cmds()
            implicit none(type, external)
        end subroutine syms_of_window_cmds
        ! Showing buffers in windows, in submodule window_display_fm.

        !> Make WINDOW display BUFFER-OR-NAME.
        !> WINDOW must be a live window.  Optional third argument
        !> KEEP-MARGINS non-nil means that WINDOW's current display margins,
        !> fringe widths, and scroll bar settings are preserved.  This
        !> function throws an error when WINDOW is strongly dedicated to its
        !> buffer and does not already display BUFFER-OR-NAME.  Port of
        !> window.c:Fset_window_buffer.
        module subroutine set_window_buffer(h, buffer, status, keep_margins)
            implicit none(type, external)
            type(handle_type), intent(in) :: h, buffer
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: keep_margins
        end subroutine set_window_buffer

        !> Port of window.c:set_window_buffer.
        module subroutine set_window_buffer_internal(h, buffer, keep_margins)
            implicit none(type, external)
            type(handle_type), intent(in) :: h, buffer
            logical, intent(in) :: keep_margins
        end subroutine set_window_buffer_internal

        !> Port of window.c:window_discard_buffer_from_window.
        module subroutine window_discard_buffer_from_window(buffer, h, all)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer, h
            logical, intent(in) :: all
        end subroutine window_discard_buffer_from_window

        !> Port of window.el:record-window-buffer.
        module subroutine record_window_buffer(h)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
        end subroutine record_window_buffer

        !> Port of window.el:switch-to-prev-buffer.
        module function switch_to_prev_buffer(window, bury_or_kill, status) result(new_buffer)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            integer, intent(in) :: bury_or_kill
            type(status_type), intent(out) :: status
            type(handle_type) :: new_buffer
        end function switch_to_prev_buffer

        !> Port of window.el:switch-to-next-buffer.
        module function switch_to_next_buffer(window, status) result(new_buffer)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            type(status_type), intent(out) :: status
            type(handle_type) :: new_buffer
        end function switch_to_next_buffer

        !> Port of window.el:quit-restore-window.
        module subroutine quit_restore_window(window, bury_or_kill, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: window
            integer, intent(in) :: bury_or_kill
            type(status_type), intent(out) :: status
        end subroutine quit_restore_window

        !> Quit WINDOW (the selected one by default) and bury its buffer, or
        !> with KILL kill it.  Port of window.el:quit-window.
        module subroutine quit_window(kill, status, window)
            implicit none(type, external)
            logical, intent(in) :: kill
            type(status_type), intent(out) :: status
            type(handle_type), intent(in), optional :: window
        end subroutine quit_window

        !> Port of window.el:replace-buffer-in-windows.
        module subroutine replace_buffer_in_windows(buffer)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
        end subroutine replace_buffer_in_windows

        !> Put BUFFER (the current buffer by default) at the end of the
        !> list of all buffers.  Port of window.el:bury-buffer.
        module subroutine bury_buffer(status, buffer)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            type(handle_type), intent(in), optional :: buffer
        end subroutine bury_buffer

        !> Port of window.el:display-buffer-record-window.
        module subroutine display_buffer_record_window(type, h, buffer)
            implicit none(type, external)
            integer, intent(in) :: type
            type(handle_type), intent(in) :: h, buffer
        end subroutine display_buffer_record_window

        !> Display BUFFER in some window and return that window (the null
        !> handle if none could be found).  Port of window.el:display-buffer.
        module subroutine display_buffer(buffer, window, status, action, inhibit_same_window)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(handle_type), intent(out) :: window
            type(status_type), intent(out) :: status
            integer, intent(in), optional :: action
            logical, intent(in), optional :: inhibit_same_window
        end subroutine display_buffer

        !> Display BUFFER and select its window.  Port of
        !> window.el:pop-to-buffer.
        module subroutine pop_to_buffer(buffer, status, action, norecord)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
            integer, intent(in), optional :: action
            logical, intent(in), optional :: norecord
        end subroutine pop_to_buffer

        !> Port of window.el:pop-to-buffer-same-window.
        module subroutine pop_to_buffer_same_window(buffer, status, norecord)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: norecord
        end subroutine pop_to_buffer_same_window

        !> Port of window.el:temp-buffer-window-show.
        module subroutine temp_buffer_window_show(buffer, window, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(handle_type), intent(out) :: window
            type(status_type), intent(out) :: status
        end subroutine temp_buffer_window_show

        !> Port of window.c:Fget_buffer_window.
        module function get_buffer_window(buffer) result(window)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(handle_type) :: window
        end function get_buffer_window

        !> Port of window.el:get-lru-window.
        module function get_lru_window(dedicated, not_selected) result(window)
            implicit none(type, external)
            logical, intent(in) :: dedicated, not_selected
            type(handle_type) :: window
        end function get_lru_window

        !> Port of window.el:get-mru-window.
        module function get_mru_window(dedicated, not_selected) result(window)
            implicit none(type, external)
            logical, intent(in) :: dedicated, not_selected
            type(handle_type) :: window
        end function get_mru_window

        !> Port of window.el:get-largest-window.
        module function get_largest_window(dedicated, not_selected) result(window)
            implicit none(type, external)
            logical, intent(in) :: dedicated, not_selected
            type(handle_type) :: window
        end function get_largest_window

        !> Split a window as split-window-below or split-window-right do.
        module subroutine split_window_and_copy(h, horizontal, new, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
            logical, intent(in) :: horizontal
            type(handle_type), intent(out) :: new
            type(status_type), intent(out) :: status
        end subroutine split_window_and_copy

        !> Copy a `quit-restore' parameter.
        module subroutine copy_quit_restore(from, to)
            implicit none(type, external)
            type(quit_restore_type), intent(in) :: from
            type(quit_restore_type), intent(inout) :: to
        end subroutine copy_quit_restore

        !> Port of window.el:window-resize (a window with a sibling).
        module subroutine window_resize(h, delta, horizontal, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: h
            integer, intent(in) :: delta
            logical, intent(in) :: horizontal
            type(status_type), intent(out) :: status
        end subroutine window_resize

        !> Register `quit-window', `bury-buffer', `previous-buffer' and
        !> `next-buffer'.
        module subroutine syms_of_window_display()
            implicit none(type, external)
        end subroutine syms_of_window_display
    end interface

contains

    !> Where the display of `w' starts (1 if it shows no buffer).  Port of
    !> window.c:Fwindow_start.
    integer(int64) function window_start(w) result(pos)
        type(window_type), intent(in) :: w
        type(status_type) :: status
        pos = 1
        if (w%start%index == 0) return
        call marker_position(w%start, pos, status)
        if (.not. status%ok()) pos = 1
    end function window_start

    !> Make `w' start at `pos'; with `noforce', redisplay may move it to
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
        integer :: k
        if (w%start%index /= 0) call free_marker(w%start, status)
        if (w%pointm%index /= 0) call free_marker(w%pointm, status)
        if (allocated(w%prev_buffers)) then
            do k = 1, size(w%prev_buffers)
                call free_prev_buffer(w%prev_buffers(k))
            end do
            deallocate (w%prev_buffers)
        end if
        if (allocated(w%next_buffers)) deallocate (w%next_buffers)
        call free_quit_restore(w%quit_restore)
        call free_quit_restore(w%quit_restore_prev)
        w%start = null_handle()
        w%pointm = null_handle()
        w%buffer = null_handle()
    end subroutine free_window

    !> Free the markers of a previous-buffer entry.
    subroutine free_prev_buffer(e)
        type(prev_buffer_type), intent(inout) :: e
        type(status_type) :: status
        if (e%start%index /= 0) call free_marker(e%start, status)
        if (e%point%index /= 0) call free_marker(e%point, status)
        e = prev_buffer_type()
    end subroutine free_prev_buffer

    !> Clear a `quit-restore' parameter (set it to nil), freeing its
    !> marker.
    subroutine free_quit_restore(q)
        type(quit_restore_type), intent(inout) :: q
        type(status_type) :: status
        if (q%quad_point%index /= 0) call free_marker(q%quad_point, status)
        q = quit_restore_type()
    end subroutine free_quit_restore

    ! ------------------------------------------------------------------
    ! The window registry and tree
    ! ------------------------------------------------------------------

    !> A new window, in no tree and showing no buffer.  Port of
    !> window.c:make_window.
    subroutine make_window(h)
        type(handle_type), intent(out) :: h
        type(window_type), allocatable :: grown(:)
        call wtable%acquire(h)
        if (.not. allocated(windows)) then
            allocate (windows(wtable%capacity()))
        else if (size(windows, kind=int64) < wtable%capacity()) then
            allocate (grown(wtable%capacity()))
            grown(1:size(windows)) = windows
            call move_alloc(grown, windows)
        end if
        windows(h%index) = window_type()
    end subroutine make_window

    !> Free window `h': its markers and its slot.  Not a port (upstream
    !> relies on GC).
    subroutine delete_window_object(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: status
        if (.not. wtable%is_live(h)) return
        call free_window(windows(h%index))
        windows(h%index) = window_type()
        call wtable%release(h, status)
    end subroutine delete_window_object

    !> Window `h', or a null pointer if it doesn't exist.  Valid until the
    !> next make_window.
    function window_ref(h) result(w)
        type(handle_type), intent(in) :: h
        type(window_type), pointer :: w
        w => null()
        if (wtable%is_live(h)) w => windows(h%index)
    end function window_ref

    !> Whether `h' is a valid window (live or internal).  Port of
    !> window.c:Fwindow_valid_p.
    logical function window_valid_p(h)
        type(handle_type), intent(in) :: h
        window_valid_p = wtable%is_live(h)
    end function window_valid_p

    !> Whether `h' is a live window: one that shows a buffer (a leaf).
    !> Port of window.c:Fwindow_live_p.
    logical function window_live_p(h)
        type(handle_type), intent(in) :: h
        window_live_p = wtable%is_live(h)
        if (window_live_p) window_live_p = windows(h%index)%child%index == 0
    end function window_live_p

    pure logical function same_window(a, b)
        type(handle_type), intent(in) :: a, b
        same_window = a%index == b%index .and. a%generation == b%generation
    end function same_window

    !> The live windows of the tree under `root', in cyclic order (top to
    !> bottom, left to right).  window.c:window_list_1 for one frame.
    recursive subroutine window_leaves(root, list)
        type(handle_type), intent(in) :: root
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), allocatable :: grown(:)
        type(handle_type) :: c
        if (.not. allocated(list)) allocate (list(0))
        if (.not. wtable%is_live(root)) return
        if (windows(root%index)%child%index == 0) then
            allocate (grown(size(list) + 1))
            grown(1:size(list)) = list
            grown(size(grown)) = root
            call move_alloc(grown, list)
            return
        end if
        c = windows(root%index)%child
        do while (wtable%is_live(c))
            call window_leaves(c, list)
            c = windows(c%index)%next
        end do
    end subroutine window_leaves

    !> Columns for text in `w': its width less the vertical border.
    integer function window_text_cols(w)
        type(window_type), intent(in) :: w
        window_text_cols = w%total_cols
        if (w%divider) window_text_cols = max(1, w%total_cols - 1)
    end function window_text_cols

    !> Give the tree under `h' the place `top', `left', `lines' by `cols'
    !> on a frame `frame_cols' wide.  Children share the size along their
    !> combination in proportion to their sizes before (equally if they
    !> had none), the last taking what is left.  The terminal part of
    !> window.c:window_resize_apply.
    recursive subroutine resize_window_tree(h, top, left, lines, cols, frame_cols)
        type(handle_type), intent(in) :: h
        integer, intent(in) :: top, left, lines, cols, frame_cols
        type(handle_type) :: c
        integer :: n, old_total, k, pos, size_k, given, old_k
        if (.not. wtable%is_live(h)) return
        associate (w => windows(h%index))
            if (w%horizontal) then
                old_total = w%total_cols
            else
                old_total = w%total_lines
            end if
            w%top_line = top
            w%left_col = left
            w%total_lines = lines
            w%total_cols = cols
            w%divider = left + cols < frame_cols
            if (w%child%index == 0) return
        end associate
        ! Count the children and the old sizes along the combination.
        n = 0
        c = windows(h%index)%child
        do while (wtable%is_live(c))
            n = n + 1
            c = windows(c%index)%next
        end do
        pos = 0
        given = 0
        k = 0
        c = windows(h%index)%child
        do while (wtable%is_live(c))
            k = k + 1
            if (windows(h%index)%horizontal) then
                old_k = windows(c%index)%total_cols
                size_k = new_size(cols)
                call resize_window_tree(c, top, left + pos, lines, size_k, frame_cols)
            else
                old_k = windows(c%index)%total_lines
                size_k = new_size(lines)
                call resize_window_tree(c, top + pos, left, size_k, cols, frame_cols)
            end if
            pos = pos + size_k
            c = windows(c%index)%next
        end do

    contains

        !> The new size of the k-th child out of `total'.
        integer function new_size(total)
            integer, intent(in) :: total
            if (k == n) then
                new_size = total - given
            else if (old_total > 0 .and. old_k > 0) then
                new_size = max(1, (old_k*total)/old_total)
            else
                new_size = total/n
            end if
            given = given + new_size
        end function new_size
    end subroutine resize_window_tree

    !> Free window `h' and the windows under it.  Not a port (GC upstream).
    recursive subroutine delete_window_tree(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: c, nxt
        if (.not. wtable%is_live(h)) return
        c = windows(h%index)%child
        do while (wtable%is_live(c))
            nxt = windows(c%index)%next
            call delete_window_tree(c)
            c = nxt
        end do
        call delete_window_object(h)
    end subroutine delete_window_tree

    !> Put window `new' in `old''s place in the tree (same parent and
    !> siblings), leaving `old' out of it.  Port of window.c:replace_window.
    subroutine replace_window(old, new)
        type(handle_type), intent(in) :: old, new
        associate (o => windows(old%index), n => windows(new%index))
            n%parent = o%parent
            n%next = o%next
            n%prev = o%prev
            n%top_line = o%top_line
            n%left_col = o%left_col
            n%total_lines = o%total_lines
            n%total_cols = o%total_cols
            n%divider = o%divider
            if (wtable%is_live(o%next)) windows(o%next%index)%prev = new
            if (wtable%is_live(o%prev)) then
                windows(o%prev%index)%next = new
            else if (wtable%is_live(o%parent)) then
                windows(o%parent%index)%child = new
            end if
            o%parent = null_handle()
            o%next = null_handle()
            o%prev = null_handle()
        end associate
    end subroutine replace_window

    !> Split live window `old' in two: make `new' (a new window showing the
    !> same buffer, with the same start and point) below it or, with
    !> `horizontal', to its right.  `new_size' is the size of the new
    !> window in lines or columns.  If `old' isn't already in a combination
    !> of that kind, an internal window replaces it there, with `old' and
    !> `new' as its children.  `root' is the frame's root window, updated
    !> if it was `old'.  Port of window.c:Fsplit_window_internal (the
    !> sizes are worked out by the caller, window.el:split-window).
    subroutine split_window_internal(old, horizontal, new_size, root, new, status)
        type(handle_type), intent(in) :: old
        logical, intent(in) :: horizontal
        integer, intent(in) :: new_size
        type(handle_type), intent(inout) :: root
        type(handle_type), intent(out) :: new
        type(status_type), intent(out) :: status
        type(handle_type) :: parent, nxt
        integer :: old_size
        logical :: combine
        status = status_type()
        parent = windows(old%index)%parent
        combine = .false.
        if (wtable%is_live(parent)) combine = windows(parent%index)%horizontal .eqv. horizontal
        if (.not. combine) then
            ! A new internal window takes old's place, with old as its child.
            call make_window(parent)
            call replace_window(old, parent)
            windows(parent%index)%horizontal = horizontal
            windows(parent%index)%child = old
            windows(old%index)%parent = parent
            if (same_window(root, old)) root = parent
        end if
        call make_window(new)
        associate (o => windows(old%index), n => windows(new%index))
            n%parent = parent
            nxt = o%next
            n%next = nxt
            n%prev = old
            o%next = new
            if (wtable%is_live(nxt)) windows(nxt%index)%prev = new
            n%top_line = o%top_line
            n%left_col = o%left_col
            n%total_lines = o%total_lines
            n%total_cols = o%total_cols
            n%divider = o%divider
            if (horizontal) then
                old_size = o%total_cols - new_size
                o%total_cols = old_size
                n%left_col = o%left_col + old_size
                n%total_cols = new_size
                o%divider = .true.
            else
                old_size = o%total_lines - new_size
                o%total_lines = old_size
                n%top_line = o%top_line + old_size
                n%total_lines = new_size
            end if
        end associate
        call set_window_buffer_internal(new, windows(old%index)%buffer, .true.)
    end subroutine split_window_internal

    !> Take window `h' out of the tree: its space goes to the sibling
    !> before it (or else after it), and a parent left with one child is
    !> replaced by that child.  `root' is the frame's root window, updated
    !> as needed.  `heir' is the sibling that got the space.  The tree part
    !> of window.c:Fdelete_window_internal; `h' itself is not freed.
    subroutine remove_window_from_tree(h, root, heir, frame_cols)
        type(handle_type), intent(in) :: h
        type(handle_type), intent(inout) :: root
        type(handle_type), intent(out) :: heir
        integer, intent(in) :: frame_cols
        type(handle_type) :: parent, only
        integer :: top, left, lines, cols
        heir = null_handle()
        parent = windows(h%index)%parent
        if (.not. wtable%is_live(parent)) return
        associate (w => windows(h%index))
            if (wtable%is_live(w%prev)) then
                heir = w%prev
            else
                heir = w%next
            end if
            ! Unlink.
            if (wtable%is_live(w%prev)) then
                windows(w%prev%index)%next = w%next
            else
                windows(parent%index)%child = w%next
            end if
            if (wtable%is_live(w%next)) windows(w%next%index)%prev = w%prev
            ! The heir grows over the space.
            associate (e => windows(heir%index))
                top = min(e%top_line, w%top_line)
                left = min(e%left_col, w%left_col)
                if (windows(parent%index)%horizontal) then
                    lines = e%total_lines
                    cols = e%total_cols + w%total_cols
                else
                    lines = e%total_lines + w%total_lines
                    cols = e%total_cols
                end if
            end associate
            w%parent = null_handle()
            w%next = null_handle()
            w%prev = null_handle()
        end associate
        call resize_window_tree(heir, top, left, lines, cols, frame_cols)
        ! A combination of one is no combination.
        only = windows(parent%index)%child
        if (.not. wtable%is_live(windows(only%index)%next)) then
            call replace_window(parent, only)
            if (same_window(root, parent)) root = only
            windows(parent%index)%child = null_handle()
            call delete_window_object(parent)
        end if
    end subroutine remove_window_from_tree
end module window_fm
