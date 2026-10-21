! fm_xdisp.f90 -- redisplay: turning buffer text into screen rows.
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

!> `redisplay` lays out the root window's buffer from its window start,
!> recentering when point is not visible, adds the mode line and the echo
!> area, and updates the terminal (fm_dispnew).
!>
!> Text appears as Emacs shows it by default: TAB to the next multiple of
!> tab-width, control characters as ^X, raw bytes as \ooo, wide
!> characters in two cells, and long lines continued with \ in the last
!> column.
!>
!> Simplified for now: the mode line has a fixed format (no
!> mode-line-format), character widths follow a built-in table of wide
!> ranges (no char-width-table), there are no text properties, overlays,
!> faces from text, display tables, bidi, hscroll or truncate-lines, and
!> the incremental redisplay shortcuts (try_window_reusing_current_matrix
!> and friends) are not ported: each redisplay lays out the whole window
!> and fm_dispnew sends only the differences.
module fm_xdisp
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use fm_error, only: status_type
    use fm_handle, only: handle_type
    use fm_buffer, only: buffers, resolve_buffer, current_buffer, buf_fetch_char, &
                         buf_fetch_byte, buf_next_char_len, buffer_modified_p
    use fm_marker, only: buf_charpos_to_bytepos, buf_bytepos_to_charpos
    use fm_character, only: char_byte8_p, char_to_byte8, string_char_and_length
    use fm_dispextern, only: glyph_type, glyph_row_type, face_type, &
                             DEFAULT_FACE_ID, MODE_LINE_ACTIVE_FACE_ID, &
                             FACE_TTY_DEFAULT_FG_COLOR, FACE_TTY_DEFAULT_BG_COLOR
    use fm_frame, only: frame_type
    use fm_window, only: window_type, set_window_buffer, window_start, &
                         set_window_start, window_body_lines
    use fm_dispnew, only: adjust_frame_glyphs, update_frame
    use fm_keyboard, only: last_message
    implicit none(type, external)
    private

    public :: redisplay, init_frame_windows, realize_basic_faces, glyph_width, &
              mode_line_text, layout_window

    !> buffer.c BVAR tab_width (the default).
    integer, public :: tab_width = 8

contains

    ! ------------------------------------------------------------------
    ! Frames, windows and faces
    ! ------------------------------------------------------------------

    !> Lay out the frame's windows: the root window above, the one-line
    !> minibuffer window at the bottom.  The window part of
    !> frame.c:make_terminal_frame and window.c:make_frame_without_minibuffer.
    subroutine init_frame_windows(f)
        type(frame_type), intent(inout) :: f
        integer :: rows, cols

        rows = f%tty%wcm%cm_rows
        cols = f%tty%wcm%cm_cols
        f%root_window%top_line = 0
        f%root_window%total_lines = rows - 1
        f%root_window%total_cols = cols
        f%minibuffer_window%top_line = rows - 1
        f%minibuffer_window%total_lines = 1
        f%minibuffer_window%total_cols = cols
        f%minibuffer_window%mini = .true.
    end subroutine init_frame_windows

    !> The default face and the mode-line face (inverse video) of a
    !> terminal frame.  Port of the terminal part of
    !> xfaces.c:realize_basic_faces.
    subroutine realize_basic_faces(f)
        type(frame_type), intent(inout) :: f

        if (allocated(f%faces)) deallocate (f%faces)
        allocate (f%faces(0:MODE_LINE_ACTIVE_FACE_ID))
        f%faces(DEFAULT_FACE_ID)%foreground = FACE_TTY_DEFAULT_FG_COLOR
        f%faces(DEFAULT_FACE_ID)%background = FACE_TTY_DEFAULT_BG_COLOR
        ! xfaces.c:realize_tty_face swaps the colors of an inverse face.
        f%faces(MODE_LINE_ACTIVE_FACE_ID)%tty_reverse_p = .true.
        f%faces(MODE_LINE_ACTIVE_FACE_ID)%foreground = FACE_TTY_DEFAULT_BG_COLOR
        f%faces(MODE_LINE_ACTIVE_FACE_ID)%background = FACE_TTY_DEFAULT_FG_COLOR
    end subroutine realize_basic_faces

    ! ------------------------------------------------------------------
    ! Producing glyphs
    ! ------------------------------------------------------------------

    !> Columns character `c` takes: 2 for East Asian wide and fullwidth
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

    !> The cells that show character `c` at logical column `x` (the column
    !> counted from the start of the line, continuation lines included).
    !> The display-element part of xdisp.c:get_next_display_element with
    !> term.c:produce_glyphs: TAB, ^X, \ooo, wide characters.
    subroutine produce_char(c, x, cells, n)
        integer(int32), intent(in) :: c
        integer, intent(in) :: x
        type(glyph_type), intent(out) :: cells(:)
        integer, intent(out) :: n
        integer(int32) :: byte

        n = 1
        if (c == 9) then
            n = tab_width - mod(x, tab_width)
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

    !> Lay out buffer slot `bi` from `start` in `rows` (one per window
    !> body line) `width` columns wide.  Gives the cursor position of `pt`
    !> (cursor_row -1 if it is not shown), where the display ends, and
    !> whether it reached the end of the accessible text.  The core of
    !> xdisp.c:try_window with display_line.
    subroutine layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, &
                             end_pos, reached_end)
        integer(int64), intent(in) :: bi, start, pt
        integer, intent(in) :: width
        type(glyph_row_type), intent(inout) :: rows(:)
        integer, intent(out) :: cursor_row, cursor_col
        integer(int64), intent(out) :: end_pos
        logical, intent(out) :: reached_end
        type(glyph_type) :: cells(8)
        integer(int64) :: pos, pos_byte, zv
        integer(int32) :: c
        integer :: r, col, cont_width, n

        cursor_row = -1
        cursor_col = 0
        reached_end = .false.
        zv = buffers(bi)%zv
        pos = start
        pos_byte = buf_charpos_to_bytepos(bi, start)
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
                col = col + n
                pos = pos + 1
                pos_byte = pos_byte + buf_next_char_len(buffers(bi), pos_byte)
            end do
            rows(r)%used = col
        end do
        end_pos = pos
        if (pos >= zv) reached_end = .true.
    end subroutine layout_window

    !> The start of the line `nlines` lines above the one holding `pos`
    !> in buffer slot `bi` (not before the accessible start).  The
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

    !> Copy window rows into the frame's desired matrix.
    subroutine put_rows(f, top, rows)
        type(frame_type), intent(inout) :: f
        integer, intent(in) :: top
        type(glyph_row_type), intent(in) :: rows(:)
        integer :: r

        do r = 1, size(rows)
            f%desired_matrix%rows(top + r - 1)%glyphs = rows(r)%glyphs
            f%desired_matrix%rows(top + r - 1)%used = rows(r)%used
            f%desired_matrix%rows(top + r - 1)%enabled_p = .true.
        end do
    end subroutine put_rows

    !> Display window `w`: from its start if that shows point (or the
    !> start is forced), else recentered around point; then its mode line.
    !> Port of the core of xdisp.c:redisplay_window.
    subroutine redisplay_window(f, w, pt)
        type(frame_type), intent(inout) :: f
        type(window_type), intent(inout) :: w
        integer(int64), intent(in) :: pt
        type(glyph_row_type), allocatable :: rows(:)
        type(status_type) :: status
        integer(int64) :: bi, start, end_pos
        integer :: body, width, cursor_row, cursor_col, r
        logical :: reached_end

        call resolve_buffer(w%buffer, bi, status)
        if (.not. status%ok()) return
        body = window_body_lines(w)
        width = w%total_cols
        allocate (rows(body))
        do r = 1, body
            allocate (rows(r)%glyphs(width))
        end do
        start = max(buffers(bi)%begv, min(window_start(w), buffers(bi)%zv))
        call layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, end_pos, &
                           reached_end)
        if (cursor_row < 0 .and. .not. w%force_start) then
            ! Point is not visible: recenter (scroll-conservatively 0).
            start = line_start_above(bi, pt, (body - 1)/2)
            call layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, end_pos, &
                               reached_end)
        end if
        call set_window_start(w, start, status, noforce=.true.)
        w%window_end_pos = end_pos
        w%window_end_valid = reached_end
        w%cursor_vpos = cursor_row
        w%cursor_hpos = cursor_col
        call put_rows(f, w%top_line, rows)
        if (.not. w%mini) call display_mode_line(f, w, bi, start, pt)
    end subroutine redisplay_window

    !> 1 + the number of newlines before `pos` in buffer slot `bi`.
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

    !> The mode line of window `w`, padded with dashes to `width`.  A
    !> fixed rendering of the default mode-line-format: coding and
    !> modified flags, frame name, buffer name, position, line number and
    !> major mode.  (xdisp.c:display_mode_line with bindings.el's
    !> mode-line-format, simplified.)
    function mode_line_text(name, modified, read_only, position, line, width) result(s)
        character(len=*), intent(in) :: name, position
        logical, intent(in) :: modified, read_only
        integer(int64), intent(in) :: line
        integer, intent(in) :: width
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
        s = s//"   "//position//"   L"//trim(num)//"     (Fundamental) "
        if (len(s) < width) then
            s = s//repeat("-", width - len(s))
        else
            s = s(1:width)
        end if
    end function mode_line_text

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
                              line_number(bi, pt), w%total_cols)
        allocate (row(w%total_cols))
        do k = 1, w%total_cols
            row(k)%ch = iachar(text(k:k))
            row(k)%face_id = MODE_LINE_ACTIVE_FACE_ID
        end do
        associate (m => f%desired_matrix%rows(w%top_line + w%total_lines - 1))
            m%glyphs = row
            m%used = w%total_cols
            m%enabled_p = .true.
        end associate
    end subroutine display_mode_line

    !> Show the last message in the minibuffer window, or blank it.  Port
    !> of the display part of xdisp.c:echo_area_display.
    subroutine display_echo_area(f)
        type(frame_type), intent(inout) :: f
        integer :: k, n, cols
        integer(int64) :: pos
        integer(int32) :: ch, length

        cols = f%minibuffer_window%total_cols
        associate (m => f%desired_matrix%rows(f%minibuffer_window%top_line))
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
        end associate
    end subroutine display_echo_area

    !> Redisplay the frame: the root window (showing the current buffer),
    !> its mode line and the echo area, then update the terminal.  Port of
    !> the core of xdisp.c:redisplay_internal.
    subroutine redisplay(f, status)
        type(frame_type), intent(inout) :: f
        type(status_type), intent(out) :: status
        type(handle_type) :: cb
        integer(int64) :: bi

        if (.not. f%glyphs_initialized_p) call adjust_frame_glyphs(f)
        if (f%root_window%total_lines == 0) call init_frame_windows(f)
        if (.not. allocated(f%faces)) call realize_basic_faces(f)
        if (size(f%faces) < 2) call realize_basic_faces(f)
        ! The selected window shows the current buffer.
        cb = current_buffer()
        if (cb%index /= f%root_window%buffer%index .or. &
            cb%generation /= f%root_window%buffer%generation) then
            call set_window_buffer(f%root_window, cb, status)
            if (.not. status%ok()) return
        end if
        call resolve_buffer(cb, bi, status)
        if (.not. status%ok()) return
        call redisplay_window(f, f%root_window, buffers(bi)%pt)
        call display_echo_area(f)
        f%cursor_vpos = -1
        if (f%root_window%cursor_vpos >= 0) then
            f%cursor_vpos = f%root_window%top_line + f%root_window%cursor_vpos
            f%cursor_hpos = f%root_window%cursor_hpos
        end if
        call update_frame(f, status)
    end subroutine redisplay

end module fm_xdisp
