! xdisp_fm.f90 -- redisplay: turning buffer text into screen rows.
! Ported from GNU Emacs 31.1 src/xdisp.c (with the terminal glyph
! production of term.c and the basic faces of xfaces.c).
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

!> `redisplay' lays out the root window's buffer from its window start,
!> recentering when point is not visible, adds the mode line and the echo
!> area, and updates the terminal (dispnew_fm).
!>
!> Text appears as Emacs shows it by default: TAB to the next multiple of
!> tab-width, control characters as ^X, raw bytes as \ooo, wide
!> characters in two cells, and long lines continued with \ in the last
!> column.
!>
!> Simplified for now: the mode line has a fixed format (no
!> mode-line-format), character widths follow a built-in table of wide
!> ranges (no char-width-table), there are no text properties, overlays,
!> faces from text, display tables, bidi or truncate-lines (only the
!> minibuffer line scrolls horizontally), and
!> the incremental redisplay shortcuts (try_window_reusing_current_matrix
!> and friends) are not ported: each redisplay lays out the whole window
!> and dispnew_fm sends only the differences.
module xdisp_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, resolve_buffer, current_buffer, buf_fetch_char, &
                         buf_fetch_byte, buf_next_char_len, buffer_modified_p, transient_mark_mode
    use marker_fm, only: buf_charpos_to_bytepos, buf_bytepos_to_charpos, markers
    use character_fm, only: char_byte8_p, char_to_byte8, string_char_and_length
    use dispextern_fm, only: glyph_type, glyph_row_type, face_type, &
                             DEFAULT_FACE_ID, MODE_LINE_ACTIVE_FACE_ID, REGION_FACE_ID, &
                             FACE_TTY_DEFAULT_FG_COLOR, FACE_TTY_DEFAULT_BG_COLOR
    use frame_fm, only: frame_type
    use window_fm, only: window_type, set_window_buffer, window_start, &
                         set_window_start, window_body_lines, window_point, window_text_cols, windows, &
                         window_valid_p, make_window, resize_window_tree, window_leaves, same_window, &
                         set_window_point
    use dispnew_fm, only: adjust_frame_glyphs, update_frame
    use keyboard_fm, only: last_message, minibuffer_message_text
    use lisp_fm, only: value_type, nilp
    use lread_fm, only: sym
    use data_fm, only: buffer_local_value, integer_setting
    implicit none(type, external)
    private

    public :: add_minor_mode_lighter, mode_line_modes
    public :: redisplay, init_frame_windows, realize_basic_faces, glyph_width, &
              mode_line_text, layout_window, message, glyph_columns

    !> xdisp.c cursor-in-echo-area: put the cursor after the echo area
    !> text (while a prompt there waits for an answer).
    logical, public :: cursor_in_echo_area = .false.

    !> Distance between tab stops (for display of tab characters), in columns.
    !> This controls the width of a TAB character on display.
    !> The value should be a positive integer.  Port of buffer.c tab-width
    !> (one value for all buffers until buffer-local variables exist).
    type(integer_setting), public :: tab_width = integer_setting(name="tab-width", value=8)

    !> An entry of minor-mode-alist.
    type :: lighter_type
        character(len=:), allocatable :: variable, lighter
    end type lighter_type

    type(lighter_type), allocatable :: lighters(:)
    integer :: n_lighters = 0

    !> The most cells one character can take: a TAB at the largest sane
    !> tab-width (SANE_TAB_WIDTH).
    integer, parameter :: MAX_CHAR_CELLS = 1000

contains

    ! ------------------------------------------------------------------
    ! Frames, windows and faces
    ! ------------------------------------------------------------------

    !> Lay out the frame's windows, making them if need be: the window
    !> tree above (at first one window, selected), the one-line minibuffer
    !> window at the bottom.  The window part of frame.c:make_terminal_frame
    !> and window.c:make_frame_without_minibuffer.
    subroutine init_frame_windows(f)
        type(frame_type), intent(inout) :: f
        integer :: rows, cols
        rows = f%tty%wcm%cm_rows
        cols = f%tty%wcm%cm_cols
        if (.not. window_valid_p(f%root_window)) then
            call make_window(f%root_window)
            f%selected_window = f%root_window
        end if
        if (.not. window_valid_p(f%minibuffer_window)) then
            call make_window(f%minibuffer_window)
            windows(f%minibuffer_window%index)%mini = .true.
        end if
        call resize_window_tree(f%root_window, 0, 0, rows - 1, cols, cols)
        call resize_window_tree(f%minibuffer_window, rows - 1, 0, 1, cols, cols)
    end subroutine init_frame_windows

    !> The default face and the mode-line face (inverse video) of a
    !> terminal frame.  Port of the terminal part of
    !> xfaces.c:realize_basic_faces.
    subroutine realize_basic_faces(f)
        type(frame_type), intent(inout) :: f
        if (allocated(f%faces)) deallocate (f%faces)
        allocate (f%faces(0:REGION_FACE_ID))
        f%faces(DEFAULT_FACE_ID)%foreground = FACE_TTY_DEFAULT_FG_COLOR
        f%faces(DEFAULT_FACE_ID)%background = FACE_TTY_DEFAULT_BG_COLOR
        ! xfaces.c:realize_tty_face swaps the colors of an inverse face.
        f%faces(MODE_LINE_ACTIVE_FACE_ID)%tty_reverse_p = .true.
        f%faces(MODE_LINE_ACTIVE_FACE_ID)%foreground = FACE_TTY_DEFAULT_BG_COLOR
        f%faces(MODE_LINE_ACTIVE_FACE_ID)%background = FACE_TTY_DEFAULT_FG_COLOR
        ! The region: faces.el's tty fallback, inverse video (the colors of
        ! its other specs need tty-colors.el).
        f%faces(REGION_FACE_ID)%tty_reverse_p = .true.
        f%faces(REGION_FACE_ID)%foreground = FACE_TTY_DEFAULT_BG_COLOR
        f%faces(REGION_FACE_ID)%background = FACE_TTY_DEFAULT_FG_COLOR
    end subroutine realize_basic_faces

    ! ------------------------------------------------------------------
    ! Producing glyphs
    ! ------------------------------------------------------------------

    !> Columns character `c' takes: 2 for East Asian wide and fullwidth
    !> characters and emoji, else 1.  An approximation of
    !> character.c:char_width with characters.el's char-width-table.
    elemental integer function glyph_width(c)
        integer(int32), intent(in) :: c
        glyph_width = 1
        if ((c >= int(z'1100', int32) .and. c <= int(z'115F', int32)) .or. &
            (c >= int(z'2E80', int32) .and. c <= int(z'303E', int32)) .or. &
            (c >= int(z'3041', int32) .and. c <= int(z'33FF', int32)) .or. &
            (c >= int(z'3400', int32) .and. c <= int(z'4DBF', int32)) .or. &
            (c >= int(z'4E00', int32) .and. c <= int(z'9FFF', int32)) .or. &
            (c >= int(z'A000', int32) .and. c <= int(z'A4CF', int32)) .or. &
            (c >= int(z'AC00', int32) .and. c <= int(z'D7A3', int32)) .or. &
            (c >= int(z'F900', int32) .and. c <= int(z'FAFF', int32)) .or. &
            (c >= int(z'FE30', int32) .and. c <= int(z'FE4F', int32)) .or. &
            (c >= int(z'FF00', int32) .and. c <= int(z'FF60', int32)) .or. &
            (c >= int(z'FFE0', int32) .and. c <= int(z'FFE6', int32)) .or. &
            (c >= int(z'1F300', int32) .and. c <= int(z'1F64F', int32)) .or. &
            (c >= int(z'1F900', int32) .and. c <= int(z'1F9FF', int32)) .or. &
            (c >= int(z'20000', int32) .and. c <= int(z'3FFFD', int32))) glyph_width = 2
    end function glyph_width

    !> The cells that show character `c' at logical column `x' (the column
    !> counted from the start of the line, continuation lines included).
    !> The display-element part of xdisp.c:get_next_display_element with
    !> term.c:produce_glyphs: TAB, ^X, \ooo, wide characters.
    subroutine produce_char(c, x, cells, n)
        integer(int32), intent(in) :: c
        integer, intent(in) :: x
        type(glyph_type), intent(out) :: cells(:)
        integer, intent(out) :: n
        integer(int32) :: byte
        integer :: tw
        n = 1
        if (c == 9) then
            ! SANE_TAB_WIDTH
            tw = 8
            if (tab_width%value > 0 .and. tab_width%value <= 1000) tw = int(tab_width%value)
            n = tw - mod(x, tw)
            cells(1:n)%ch = 32
        else if (c < 32 .or. c == 127) then
            ! ctl-arrow: ^@ .. ^_, ^?
            n = 2
            cells(1)%ch = iachar("^")
            cells(2)%ch = ieor(c, 64_int32)
        else if (char_byte8_p(c)) then
            byte = char_to_byte8(c)
            n = 4
            cells(1)%ch = iachar("\")
            cells(2)%ch = iachar("0") + byte/64
            cells(3)%ch = iachar("0") + mod(byte/8, 8)
            cells(4)%ch = iachar("0") + mod(byte, 8)
        else if (glyph_width(c) == 2) then
            n = 2
            cells(1)%ch = c
            cells(2)%ch = c
            cells(2)%padding = .true.
        else
            cells(1)%ch = c
        end if
    end subroutine produce_char

    !> Columns character `c' takes when displayed at column `x' (for
    !> TAB, the distance to the next tab stop).
    integer function glyph_columns(c, x) result(n)
        integer(int32), intent(in) :: c
        integer, intent(in) :: x
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        call produce_char(c, x, cells, n)
    end function glyph_columns

    !> Lay out buffer slot `bi' from `start' in `rows' (one per window
    !> body line) `width' columns wide.  Gives the cursor position of `pt'
    !> (cursor_row -1 if it is not shown), where the display ends, and
    !> whether it reached the end of the accessible text.  The core of
    !> xdisp.c:try_window with display_line.
    subroutine layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, &
                             end_pos, reached_end, region_beg, region_end)
        integer(int64), intent(in) :: bi, start, pt
        integer, intent(in) :: width
        type(glyph_row_type), intent(inout) :: rows(:)
        integer, intent(out) :: cursor_row, cursor_col
        integer(int64), intent(out) :: end_pos
        logical, intent(out) :: reached_end
        !> The region to highlight with the region face, [region_beg, region_end).
        integer(int64), intent(in), optional :: region_beg, region_end
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        integer(int64) :: rb, re
        integer(int64) :: pos, pos_byte, zv
        integer(int32) :: c
        integer :: r, col, cont_width, n
        cursor_row = -1
        cursor_col = 0
        reached_end = .false.
        zv = buffers(bi)%zv
        pos = start
        pos_byte = buf_charpos_to_bytepos(bi, start)
        rb = 0
        re = 0
        if (present(region_beg) .and. present(region_end)) then
            rb = region_beg
            re = region_end
        end if
        cont_width = 0
        do r = 1, size(rows)
            rows(r)%glyphs = glyph_type()
            rows(r)%used = 0
            rows(r)%enabled_p = .true.
            if (reached_end) cycle
            col = 0
            do
                if (pos >= zv) then
                    if (pos == pt .and. cursor_row < 0) then
                        cursor_row = r - 1
                        cursor_col = col
                    end if
                    reached_end = .true.
                    exit
                end if
                c = buf_fetch_char(buffers(bi), pos_byte)
                if (c == 10) then
                    if (pos == pt) then
                        cursor_row = r - 1
                        cursor_col = col
                    end if
                    ! The region face has :extend t: a newline in the region
                    ! highlights the rest of the row.
                    if (pos >= rb .and. pos < re .and. col < width - 1) then
                        rows(r)%glyphs(col + 1:width - 1)%face_id = REGION_FACE_ID
                        col = width - 1
                    end if
                    pos = pos + 1
                    pos_byte = pos_byte + 1
                    cont_width = 0
                    exit
                end if
                cells = glyph_type()
                call produce_char(c, col + cont_width, cells, n)
                if (col + n > width - 1) then
                    ! Continue on the next row, \ in the last column.
                    rows(r)%glyphs(width)%ch = iachar("\")
                    col = width
                    cont_width = cont_width + width - 1
                    exit
                end if
                if (pos == pt) then
                    cursor_row = r - 1
                    cursor_col = col
                end if
                rows(r)%glyphs(col + 1:col + n) = cells(1:n)
                if (pos >= rb .and. pos < re) rows(r)%glyphs(col + 1:col + n)%face_id = REGION_FACE_ID
                col = col + n
                pos = pos + 1
                pos_byte = pos_byte + buf_next_char_len(buffers(bi), pos_byte)
            end do
            rows(r)%used = col
        end do
        end_pos = pos
        if (pos >= zv) reached_end = .true.
    end subroutine layout_window

    !> The start of the line `nlines' lines above the one holding `pos'
    !> in buffer slot `bi' (not before the accessible start).  The
    !> logical-line part of xdisp.c's recentering (move_it_vertically_backward).
    integer(int64) function line_start_above(bi, pos, nlines) result(start)
        integer(int64), intent(in) :: bi, pos
        integer, intent(in) :: nlines
        integer(int64) :: byte, begv_byte
        integer :: k
        begv_byte = buffers(bi)%begv_byte
        byte = buf_charpos_to_bytepos(bi, pos)
        do k = 0, nlines
            ! Back to the start of this line.
            do while (byte > begv_byte)
                if (buf_fetch_byte(buffers(bi), byte - 1) == 10) exit
                byte = byte - 1
            end do
            if (k == nlines .or. byte <= begv_byte) exit
            byte = byte - 1
        end do
        start = buf_bytepos_to_charpos(bi, byte)
    end function line_start_above

    ! ------------------------------------------------------------------
    ! Windows, mode line, echo area
    ! ------------------------------------------------------------------

    !> Make row `vpos' of the desired matrix a full-width blank row, to be
    !> filled by the windows on it.
    subroutine blank_row(f, vpos)
        type(frame_type), intent(inout) :: f
        integer, intent(in) :: vpos
        integer :: cols
        cols = f%desired_matrix%ncols
        associate (m => f%desired_matrix%rows(vpos))
            if (allocated(m%glyphs)) then
                if (size(m%glyphs) /= cols) deallocate (m%glyphs)
            end if
            if (.not. allocated(m%glyphs)) allocate (m%glyphs(cols))
            m%glyphs = glyph_type()
            m%used = cols
            m%enabled_p = .true.
        end associate
    end subroutine blank_row

    !> Copy window rows into the frame's desired matrix, from column
    !> `left' (0-based), with the vertical border after them if `divider'.
    subroutine put_rows(f, top, left, rows, divider)
        type(frame_type), intent(inout) :: f
        integer, intent(in) :: top, left
        type(glyph_row_type), intent(in) :: rows(:)
        logical, intent(in) :: divider
        integer :: r, n, cols
        cols = f%desired_matrix%ncols
        do r = 1, size(rows)
            associate (m => f%desired_matrix%rows(top + r - 1))
                n = min(size(rows(r)%glyphs), cols - left)
                if (n > 0) m%glyphs(left + 1:left + n) = rows(r)%glyphs(1:n)
                ! The border between side-by-side windows (vertical-border).
                if (divider .and. left + n + 1 <= cols) then
                    m%glyphs(left + n + 1) = glyph_type()
                    m%glyphs(left + n + 1)%ch = iachar("|")
                end if
            end associate
        end do
    end subroutine put_rows

    !> Display window `w': from its start if that shows point (or the
    !> start is forced), else recentered around point; then its mode line.
    !> `pt' is the window's point; `selected' says whether it is the
    !> selected window.  Port of the core of xdisp.c:redisplay_window.
    subroutine redisplay_window(f, w, pt, selected)
        type(frame_type), intent(inout) :: f
        type(window_type), intent(inout) :: w
        integer(int64), intent(in) :: pt
        logical, intent(in) :: selected
        type(glyph_row_type), allocatable :: rows(:)
        type(status_type) :: status
        integer(int64) :: bi, start, end_pos
        integer(int64) :: rb, re, m
        integer :: body, width, cursor_row, cursor_col, r
        logical :: reached_end
        call resolve_buffer(w%buffer, bi, status)
        if (.not. status%ok()) return
        body = window_body_lines(w)
        width = window_text_cols(w)
        allocate (rows(body))
        do r = 1, body
            allocate (rows(r)%glyphs(width))
        end do
        ! Transient Mark mode highlights an active region, in the selected
        ! window only (highlight-nonselected-windows nil).
        rb = 0
        re = 0
        if (transient_mark_mode%value .and. buffers(bi)%mark_active .and. .not. f%minibuffer_selected .and. &
            .not. w%mini .and. selected) then
            m = buffers(bi)%mark%index
            if (m > 0) then
                if (markers(m)%buffer == bi) then
                    rb = min(pt, markers(m)%charpos)
                    re = max(pt, markers(m)%charpos)
                end if
            end if
        end if
        start = max(buffers(bi)%begv, min(window_start(w), buffers(bi)%zv))
        call layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, end_pos, &
                           reached_end, rb, re)
        if (cursor_row < 0 .and. .not. w%force_start) then
            ! Point is not visible: recenter (scroll-conservatively 0).
            start = line_start_above(bi, pt, (body - 1)/2)
            call layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, end_pos, &
                               reached_end, rb, re)
        end if
        call set_window_start(w, start, status, noforce=.true.)
        w%window_end_pos = end_pos
        w%window_end_valid = reached_end
        w%cursor_vpos = cursor_row
        w%cursor_hpos = cursor_col
        call put_rows(f, w%top_line, w%left_col, rows, w%divider .and. .not. w%mini)
        if (.not. w%mini) call display_mode_line(f, w, bi, start, pt)
    end subroutine redisplay_window

    !> 1 + the number of newlines before `pos' in buffer slot `bi'.
    integer(int64) function line_number(bi, pos) result(n)
        integer(int64), intent(in) :: bi, pos
        integer(int64) :: byte, end_byte
        n = 1
        byte = buffers(bi)%begv_byte
        end_byte = buf_charpos_to_bytepos(bi, pos)
        do while (byte < end_byte)
            if (buf_fetch_byte(buffers(bi), byte) == 10) n = n + 1
            byte = byte + 1
        end do
    end function line_number

    !> The mode line of window `w', padded with dashes to `width'.  A
    !> fixed rendering of the default mode-line-format: coding and
    !> modified flags, frame name, buffer name, position, line number and
    !> major mode.  (xdisp.c:display_mode_line with bindings.el's
    !> mode-line-format, simplified.)
    !> MODES is the text in parentheses, the major mode's name and the
    !> minor modes' lighters (default "Fundamental").
    function mode_line_text(name, modified, read_only, position, line, width, modes) result(s)
        character(len=*), intent(in) :: name, position
        logical, intent(in) :: modified, read_only
        integer(int64), intent(in) :: line
        integer, intent(in) :: width
        character(len=*), intent(in), optional :: modes
        character(len=:), allocatable :: s
        character(len=2) :: flags
        character(len=20) :: num
        if (read_only) then
            flags = "%%"
            if (modified) flags = "%*"
        else if (modified) then
            flags = "**"
        else
            flags = "--"
        end if
        write (num, "(i0)") line
        s = "-UUU:"//flags//"--F1  "//name
        if (len(name) < 12) s = s//repeat(" ", 12 - len(name))
        s = s//"   "//position//"   L"//trim(num)//"     ("
        if (present(modes)) then
            s = s//modes//") "
        else
            s = s//"Fundamental) "
        end if
        if (len(s) < width) then
            s = s//repeat("-", width - len(s))
        else
            s = s(1:width)
        end if
    end function mode_line_text

    !> Show LIGHTER in the mode line while VARIABLE is non-nil (replacing
    !> VARIABLE's earlier lighter).  Port of the mode-line part of
    !> subr.el:add-minor-mode (minor-mode-alist).
    subroutine add_minor_mode_lighter(variable, lighter)
        character(len=*), intent(in) :: variable, lighter
        type(lighter_type), allocatable :: grown(:)
        integer :: i
        do i = 1, n_lighters
            if (lighters(i)%variable == variable) then
                lighters(i)%lighter = lighter
                return
            end if
        end do
        if (.not. allocated(lighters)) allocate (lighters(8))
        if (n_lighters == size(lighters)) then
            allocate (grown(2*n_lighters))
            grown(1:n_lighters) = lighters
            call move_alloc(grown, lighters)
        end if
        n_lighters = n_lighters + 1
        lighters(n_lighters)%variable = variable
        lighters(n_lighters)%lighter = lighter
    end subroutine add_minor_mode_lighter

    !> The modes part of buffer BI's mode line: `mode-name' and the
    !> lighters of `minor-mode-alist' whose variables are non-nil there.
    function mode_line_modes(bi) result(s)
        integer(int64), intent(in) :: bi
        character(len=:), allocatable :: s
        type(value_type) :: v
        type(status_type) :: st
        integer :: i
        if (allocated(buffers(bi)%mode_name)) then
            s = buffers(bi)%mode_name
        else
            s = "Fundamental"
        end if
        do i = 1, n_lighters
            call buffer_local_value(sym(lighters(i)%variable), bi, v, st)
            if (.not. st%ok()) cycle
            if (.not. nilp(v)) s = s//lighters(i)%lighter
        end do
    end function mode_line_modes

    !> Port of the terminal part of xdisp.c:display_mode_line.
    subroutine display_mode_line(f, w, bi, start, pt)
        type(frame_type), intent(inout) :: f
        type(window_type), intent(in) :: w
        integer(int64), intent(in) :: bi, start, pt
        character(len=:), allocatable :: text, position
        type(glyph_type), allocatable :: row(:)
        type(status_type) :: status
        logical :: modified, at_top, at_bottom
        integer(int64) :: total
        character(len=4) :: pct
        integer :: k
        call buffer_modified_p(w%buffer, modified, status)
        at_top = start <= buffers(bi)%begv
        at_bottom = w%window_end_valid
        if (at_top .and. at_bottom) then
            position = "All"
        else if (at_top) then
            position = "Top"
        else if (at_bottom) then
            position = "Bot"
        else
            total = max(1_int64, buffers(bi)%zv - buffers(bi)%begv)
            write (pct, '(i2,"%")') int(((start - buffers(bi)%begv)*100)/total)
            position = trim(adjustl(pct))
        end if
        text = mode_line_text(buffers(bi)%name, modified, buffers(bi)%read_only, position, &
                              line_number(bi, pt), w%total_cols, mode_line_modes(bi))
        allocate (row(w%total_cols))
        do k = 1, w%total_cols
            row(k)%ch = iachar(text(k:k))
            row(k)%face_id = MODE_LINE_ACTIVE_FACE_ID
        end do
        associate (m => f%desired_matrix%rows(w%top_line + w%total_lines - 1))
            m%glyphs(w%left_col + 1:w%left_col + w%total_cols) = row
        end associate
    end subroutine display_mode_line

    !> Show the last message in the minibuffer window, or blank it.  Port
    !> of the display part of xdisp.c:echo_area_display.
    subroutine display_echo_area(f, endcol)
        type(frame_type), intent(inout) :: f
        !> Column after the text.
        integer, intent(out) :: endcol
        integer :: n, cols
        integer(int64) :: pos
        integer(int32) :: ch, length
        cols = windows(f%minibuffer_window%index)%total_cols
        endcol = 0
        associate (m => f%desired_matrix%rows(windows(f%minibuffer_window%index)%top_line))
            m%glyphs = glyph_type()
            m%used = 0
            m%enabled_p = .true.
            if (.not. allocated(last_message)) return
            n = 0
            pos = 1
            do while (pos <= len(last_message, kind=int64) .and. n < cols)
                ch = ichar(last_message(pos:pos), int32)
                length = 1
                if (ch >= 128) call string_char_and_length(last_message, pos, ch, length)
                n = n + 1
                m%glyphs(n)%ch = ch
                pos = pos + length
            end do
            m%used = n
            endcol = n
        end associate
    end subroutine display_echo_area

    !> Show the active minibuffer's text on the minibuffer line, scrolled
    !> horizontally to keep point visible ("$" marks text cut off at
    !> either edge).  `cursor_col' is point's column on the screen line.
    !> The one-line case of xdisp.c:display_line for a minibuffer window
    !> with auto-hscroll.
    subroutine display_minibuffer(f, cursor_col)
        type(frame_type), intent(inout) :: f
        integer, intent(out) :: cursor_col
        type(glyph_type), allocatable :: line(:), grown(:)
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        type(status_type) :: status
        integer(int64) :: bi, pos, byte
        integer(int32) :: c
        integer :: x, n, cols, col_of_point
        integer(int64) :: mpos
        integer(int32) :: clen
        cursor_col = 0
        call resolve_buffer(windows(f%minibuffer_window%index)%buffer, bi, status)
        if (.not. status%ok()) return
        cols = windows(f%minibuffer_window%index)%total_cols
        allocate (line(max(2*cols, 16)))
        ! Lay out the whole accessible text as one long line.
        x = 0
        col_of_point = -1
        pos = buffers(bi)%begv
        byte = buffers(bi)%begv_byte
        do while (pos < buffers(bi)%zv)
            if (pos == buffers(bi)%pt) col_of_point = x
            c = buf_fetch_char(buffers(bi), byte)
            call put(c)
            byte = byte + buf_next_char_len(buffers(bi), byte)
            pos = pos + 1
        end do
        if (col_of_point < 0) col_of_point = x
        ! A minibuffer-message after the text (its overlay at the end).
        if (allocated(minibuffer_message_text)) then
            mpos = 1
            do while (mpos <= len(minibuffer_message_text, kind=int64))
                call string_char_and_length(minibuffer_message_text, mpos, c, clen)
                call put(c)
                mpos = mpos + clen
            end do
        end if
        ! auto-hscroll: recenter point when it would leave the line.
        associate (w => windows(f%minibuffer_window%index))
            if (col_of_point < w%hscroll .or. col_of_point - w%hscroll >= cols - 1) then
                w%hscroll = max(0, col_of_point - cols/2)
            end if
            associate (m => f%desired_matrix%rows(w%top_line))
                m%glyphs = glyph_type()
                m%enabled_p = .true.
                n = min(cols, max(0, x - w%hscroll))
                if (n > 0) m%glyphs(1:n) = line(w%hscroll + 1:w%hscroll + n)
                m%used = n
                ! Truncation glyphs.
                if (w%hscroll > 0) then
                    m%glyphs(1) = glyph_type()
                    m%glyphs(1)%ch = iachar("$")
                end if
                if (x - w%hscroll > cols) then
                    m%glyphs(cols) = glyph_type()
                    m%glyphs(cols)%ch = iachar("$")
                end if
                ! A wide character cut by the left edge shows as a space.
                if (n > 0) then
                    if (m%glyphs(1)%padding) then
                        m%glyphs(1) = glyph_type()
                        m%glyphs(1)%ch = 32
                    end if
                end if
            end associate
            cursor_col = col_of_point - w%hscroll
        end associate
    contains

        !> Lay out character `c' at column x.
        subroutine put(c)
            integer(int32), intent(in) :: c
            call produce_char(c, x, cells, n)
            if (x + n > size(line)) then
                allocate (grown(2*size(line) + n))
                grown(1:x) = line(1:x)
                call move_alloc(grown, line)
            end if
            line(x + 1:x + n) = cells(1:n)
            x = x + n
        end subroutine put
    end subroutine display_minibuffer

    !> Show `text' in the echo area.  Port of editfns.c:Fmessage (with a
    !> plain string; no *Messages* log yet).
    subroutine message(text)
        character(len=*), intent(in) :: text
        last_message = text
    end subroutine message

    !> Redisplay the frame: each window with its mode line (the selected
    !> one showing the current buffer unless the minibuffer is selected),
    !> and the echo area or active minibuffer, then update the terminal.
    !> Port of the core of xdisp.c:redisplay_internal.
    subroutine redisplay(f, status)
        type(frame_type), intent(inout) :: f
        type(status_type), intent(out) :: status
        type(handle_type) :: cb
        type(handle_type), allocatable :: leaves(:)
        integer(int64) :: bi, pt
        integer :: endcol, mini_col, k, r
        logical :: showing_message, mini_active, selected
        if (.not. f%glyphs_initialized_p) call adjust_frame_glyphs(f)
        if (.not. window_valid_p(f%root_window)) call init_frame_windows(f)
        if (windows(f%root_window%index)%total_lines == 0) call init_frame_windows(f)
        if (.not. allocated(f%faces)) call realize_basic_faces(f)
        if (size(f%faces) < REGION_FACE_ID + 1) call realize_basic_faces(f)
        do r = 0, windows(f%minibuffer_window%index)%top_line - 1
            call blank_row(f, r)
        end do
        ! Each live window, at its point: the buffer's point for the
        ! selected one, its own (pointm) for the others.
        call window_leaves(f%root_window, leaves)
        do k = 1, size(leaves)
            associate (w => windows(leaves(k)%index))
                call resolve_buffer(w%buffer, bi, status)
                if (.not. status%ok()) cycle
                selected = same_window(leaves(k), f%selected_window)
                if (selected) then
                    pt = buffers(bi)%pt
                    ! Keep the window's own point current too.
                    call set_window_point(w, pt, status)
                else
                    pt = window_point(w)
                end if
                call redisplay_window(f, w, pt, selected)
            end associate
        end do
        status = status_type()
        ! The minibuffer line: a message if there is one, else the active
        ! minibuffer, else blank.
        showing_message = .false.
        if (allocated(last_message)) showing_message = len(last_message) > 0
        associate (mw => windows(f%minibuffer_window%index), sw => windows(f%selected_window%index))
            mini_active = f%minibuffer_selected .and. mw%buffer%index /= 0
            mini_col = 0
            if (mini_active .and. .not. showing_message) then
                call display_minibuffer(f, mini_col)
            else
                call display_echo_area(f, endcol)
            end if
            f%cursor_vpos = -1
            if (mini_active .and. .not. showing_message) then
                f%cursor_vpos = mw%top_line
                f%cursor_hpos = min(mini_col, mw%total_cols - 1)
            else if (cursor_in_echo_area .or. (mini_active .and. showing_message)) then
                f%cursor_vpos = mw%top_line
                f%cursor_hpos = min(endcol, mw%total_cols - 1)
            else if (sw%cursor_vpos >= 0) then
                f%cursor_vpos = sw%top_line + sw%cursor_vpos
                f%cursor_hpos = sw%left_col + sw%cursor_hpos
            end if
        end associate
        call update_frame(f, status)
    end subroutine redisplay
end module xdisp_fm
