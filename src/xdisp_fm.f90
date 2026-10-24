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
!> column (a wide character that does not fit moves whole to the next
!> row; a TAB, ^X or \ooo is split between the rows).  Recentering puts
!> point's screen line half the window's height from the top.
!>
!> Faces (xfaces_fm): text in the face of its `face' property, an active
!> region in the region face (extended to the end of a line where the
!> face has `extend'), ^X and \ooo in escape_glyph, non-ASCII spaces and
!> hyphens in nobreak_space and nobreak_hyphen, the minibuffer prompt in
!> minibuffer_prompt, mode lines in mode_line_active or
!> mode_line_inactive with the buffer name in mode_line_buffer_id, and
!> borders between windows in vertical_border.
!>
!> Simplified for now: the mode line is the standard mode-line-format
!> expanded by fixed code (no mode-line-format interpretation; coding
!> systems shown as UUU:, no base-line cache for %l), character widths
!> follow a built-in table of wide ranges (no char-width-table), there
!> are no overlays, display or invisible properties, display tables,
!> faces in echo-area messages, bidi or truncate-lines
!> (truncate-partial-width-windows is Deferred: narrow side-by-side
!> windows continue lines instead of truncating them, as that needs
!> auto-hscroll of windows; only the minibuffer line scrolls
!> horizontally), the echo area shows the first line of a message
!> (resize-mini-windows is not ported), and
!> the incremental redisplay shortcuts (try_window_reusing_current_matrix
!> and friends) are not ported: each redisplay lays out the whole window
!> and dispnew_fm sends only the differences.
!>
!> Sizes are pixels, as upstream's: on a terminal frame every glyph is
!> one pixel (column) wide and every row one pixel (line) high, so pixels
!> and cells coincide.  A window-system frame (frame_window_p: one with a
!> redisplay interface; so far only the tests' fake device) lays out text
!> in the fonts of its faces (xdisp.c:gui_produce_glyphs): each character
!> is one glyph as wide as the font says, a TAB a stretch glyph to the
!> next tab stop (tab-width times the font's space width), rows as high as
!> their tallest font, the window as many rows as fit its height in
!> pixels; the continuation glyph \ follows the last glyph that fits
!> (there are no fringes), a TAB that does not fit fills the row.  Its
!> rows are drawn through the interface (dispnew_fm:update_window_line),
!> with the border between side-by-side windows drawn as a line
!> (gui_draw_vertical_border) and the cursor as a filled box.  Deferred
!> on window-system frames: recentering and vertical motion in pixels
!> (point outside the window leaves the start as it is), the active
!> minibuffer and the cursor in the echo area, face extension to the end
!> of a line (fill_line_p), fringes, scroll bars, margins, dividers,
!> boxes, overlines and the rest of realize_gui_face, line-height and
!> line-spacing, partially visible cursor rows, mode lines taller than
!> their face's font, faces without a font (glyphless display), fonts
!> too high for their size (FONT_TOO_HIGH), phys_ascent.
module xdisp_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use eval_fm, only: position_hook_type
    use textprop_fm, only: get_text_property, next_single_property_change
    use buffer_fm, only: cur, set_buffer_internal
    use buffer_fm, only: buffers, resolve_buffer, current_buffer, buf_fetch_char, &
                         buf_fetch_byte, buf_next_char_len, buffer_modified_p, transient_mark_mode
    use marker_fm, only: buf_charpos_to_bytepos, buf_bytepos_to_charpos, markers
    use character_fm, only: char_byte8_p, char_to_byte8, string_char_and_length, multibyte_length, &
                            byte8_to_char
    use dispextern_fm, only: glyph_type, glyph_row_type, DEFAULT_FACE_ID, MODE_LINE_ACTIVE_FACE_ID, &
                             MODE_LINE_INACTIVE_FACE_ID, VERTICAL_BORDER_FACE_ID, STRETCH_GLYPH, FILLED_BOX_CURSOR
    use font_fm, only: font_type, font_metrics_type
    use xfaces_fm, only: face_at_buffer_position, merge_faces, realize_basic_faces, TEXT_PROP_DISTANCE_LIMIT
    use frame_fm, only: frame_type, selected_frame, frame_window_p, frame_lines, frame_cols, window_pixel_left, &
                        window_pixel_top, window_pixel_width, window_pixel_height, window_body_pixel_width
    use window_fm, only: window_type, set_window_buffer, window_start, &
                         set_window_start, window_body_lines, window_point, window_text_cols, windows, &
                         window_valid_p, make_window, resize_window_tree, window_leaves, same_window, &
                         set_window_point
    use dispnew_fm, only: adjust_frame_glyphs, update_frame, update_window_line, redraw_frame
    use keyboard_fm, only: last_message, minibuffer_message_text, set_minibuffer_message_text
    use lisp_fm, only: value_type, nilp
    use lread_fm, only: sym
    use data_fm, only: buffer_local_value, integer_setting, add_local_variables_killer
    implicit none(type, external)
    private

    public :: add_minor_mode_lighter, mode_line_modes, mode_line_modes_faces
    public :: set_mode_line_process, clear_mode_line_process, mode_line_process_text, add_mode_line_modes_entry, &
              mode_line_modes_prefix
    public :: redisplay, init_frame_windows, glyph_width, &
              mode_line_text, layout_window, message, glyph_columns, display_line_rows, display_row_of, &
              mode_line_row, recenter_start, mode_line_percent

    !> List of functions to call to fontify regions of text.
    !> Each function is called with one argument POS.  Functions must
    !> fontify a region starting at POS in the current buffer, and give
    !> fontified regions the property `fontified' with a true value.
    !> Port of xdisp.c fontification-functions (buffer-local when set).
    type(position_hook_type), target, public :: fontification_functions = &
                                                position_hook_type(name="fontification_functions")

    !> fontification-functions is bound to nil while its functions run.
    logical :: fontifying = .false.

    !> xdisp.c cursor-in-echo-area: put the cursor after the echo area
    !> text (while a prompt there waits for an answer).
    logical, public :: cursor_in_echo_area = .false.

    !> Distance between tab stops (for display of tab characters), in columns.
    !> This controls the width of a TAB character on display.
    !> The value should be a positive integer.  Port of buffer.c tab-width
    !> (one value for all buffers until buffer-local variables exist).
    type(integer_setting), public :: tab_width = integer_setting(name="tab_width", value=8)

    !> A nonzero value means truncate lines in windows narrower than the frame.
    !> For a value other than huge(0), truncate lines in each window narrower than the
    !> full frame width, provided the total window width in column units is less
    !> than that integer; otherwise, respect the value of `truncate_lines'.
    !> The total width of the window is as returned by `window_total_width', it
    !> includes the fringes, the continuation and truncation glyphs, the
    !> display margins (if any), and the scroll bar
    !>
    !> For huge(0), truncate lines in all windows that do
    !> not span the full frame width.
    !>
    !> A value of 0 means to respect the value of `truncate_lines'.
    !>
    !> Port of
    !> xdisp.c truncate-partial-width-windows (0 for nil, huge(0) for any
    !> other non-nil value); the truncation itself is
    !> not ported yet (Deferred), only commands that test the variable.
    integer, public :: truncate_partial_width_windows = 50

    abstract interface
        !> Whether a minor-mode-alist entry's variable is true, for a
        !> variable kept in Fortran (such as `defining_kbd_macro').
        logical function lighter_active_proc()
            implicit none(type, external)
        end function lighter_active_proc
    end interface

    public :: lighter_active_proc

    !> An entry of minor-mode-alist: VARIABLE and its LIGHTER, shown in
    !> FACE (none if empty); ACTIVE, if associated, tells whether the
    !> variable is true.
    type :: lighter_type
        character(len=:), allocatable :: variable, lighter, face
        procedure(lighter_active_proc), pointer, nopass :: active => null()
    end type lighter_type

    !> A part of a mode line shown in another face: characters FIRST to
    !> LAST in face FACE_ID.
    type, public :: face_span_type
        integer :: first = 0, last = 0
        integer(int32) :: face_id = 0
    end type face_span_type

    type(lighter_type), allocatable :: lighters(:)
    integer :: n_lighters = 0

    !> One part of a `mode_line_process' construct: TEXT, in which "%s"
    !> stands for the status of the buffer's process (decode_mode_spec's
    !> %s) and "%%" for "%", shown in FACE (none if empty).  Upstream
    !> the parts are the strings and (:propertize STRING face FACE)
    !> elements of the construct.
    type, public :: mode_line_part_type
        character(len=:), allocatable :: text
        character(len=:), allocatable :: face
    end type mode_line_part_type

    !> A buffer's own `mode_line_process'.
    type :: mode_line_process_entry_type
        integer(int64) :: serial = 0
        type(mode_line_part_type), allocatable :: parts(:)
    end type mode_line_process_entry_type

    type(mode_line_process_entry_type), allocatable :: process_entries(:)

    abstract interface
        !> The status of the process of buffer slot BI, as %s in a mode
        !> line shows it ("no process" if it has none).
        subroutine process_status_proc(bi, status_name)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: bi
            character(len=:), allocatable, intent(out) :: status_name
        end subroutine process_status_proc
    end interface

    !> What %s shows: installed by process_fm (xdisp.c calls
    !> Fprocess_status); "no process" while it is null.
    procedure(process_status_proc), pointer, public :: mode_line_process_status => null()

    !> An element (VARIABLE TEXT) of `mode_line_modes', shown before its
    !> other parts while ACTIVE says VARIABLE is true.
    type :: modes_entry_type
        character(len=:), allocatable :: variable, text
        procedure(lighter_active_proc), pointer, nopass :: active => null()
    end type modes_entry_type

    type(modes_entry_type), allocatable :: modes_entries(:)

    !> The most cells one character can take: a TAB at the largest sane
    !> tab-width (SANE_TAB_WIDTH).
    integer, parameter :: MAX_CHAR_CELLS = 1000

    !> How display_row ended a screen row: at the end of the accessible
    !> text, after a newline, or continued on the next row.
    integer, parameter :: ROW_ENDS_AT_ZV = 0, ROW_ENDS_IN_NEWLINE = 1, ROW_CONTINUED = 2

    !> Where a screen row starts: the state of xdisp.c's iterator at the
    !> start of a display line.
    type, public :: row_start_type
        !> The first character shown (charpos, bytepos).
        integer(int64) :: pos = 1, byte = 1
        !> it->continuation_lines_width: the columns of the rows before
        !> this one on the same line (TABs count from it).
        integer :: cont = 0
        !> it->starts_in_middle_of_char_p: the row starts with the rest of
        !> a ^X, \ooo or TAB begun on the row before ...
        logical :: mid = .false.
        !> ... whose first `skip' cells were shown there.
        integer :: skip = 0
    end type row_start_type

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
        rows = frame_lines(f)
        cols = frame_cols(f)
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
    !> term.c:produce_glyphs: TAB, ^X, \ooo, wide characters.  With `gui'
    !> (a window-system frame), a TAB is one stretch glyph and a wide
    !> character one glyph, their widths given later by
    !> gui_produce_glyph; ^X and \ooo are display vectors, one glyph an
    !> element, as on a terminal.
    subroutine produce_char(c, x, cells, n, gui)
        integer(int32), intent(in) :: c
        integer, intent(in) :: x
        type(glyph_type), intent(out) :: cells(:)
        integer, intent(out) :: n
        logical, intent(in), optional :: gui
        integer(int32) :: byte
        integer :: tw
        logical :: window_system
        window_system = .false.
        if (present(gui)) window_system = gui
        n = 1
        if (c == 9 .and. window_system) then
            cells(1)%type = STRETCH_GLYPH
        else if (c == 9) then
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
        else if (glyph_width(c) == 2 .and. .not. window_system) then
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

    !> The size of glyph `g' (its character or stretch, and face) at
    !> logical x `x' (pixels from the start of the line, continuation rows
    !> included) on window-system frame `f', from the font of its face: a
    !> character as wide as the font's glyph for it (the font's space width
    !> if it has none, at least one pixel), a TAB stretch to the next tab
    !> stop (tab-width times the space width; the stop after that if the
    !> next is closer than a space), the ascent and descent the font's.
    !> Port of the IT_CHARACTER part of xdisp.c:gui_produce_glyphs.
    subroutine gui_produce_glyph(f, g, x)
        type(frame_type), intent(in) :: f
        type(glyph_type), intent(inout) :: g
        integer, intent(in) :: x
        type(font_type) :: ft
        type(font_metrics_type) :: pcm
        integer :: id, boff, tw, next_tab_x
        logical :: found
        id = f%faces(g%face_id)%font
        ! (A face without a font is shown as a glyphless character:
        ! Deferred.)
        if (id <= 0 .or. .not. associated(f%font_driver)) return
        ft = f%font_driver%font(id)
        boff = ft%baseline_offset
        g%ascent = ft%ascent + boff
        g%descent = ft%descent - boff
        if (g%type == STRETCH_GLYPH) then
            g%pixel_width = 0
            if (ft%space_width > 0) then
                ! SANE_TAB_WIDTH
                tw = 8
                if (tab_width%value > 0 .and. tab_width%value <= 1000) tw = int(tab_width%value)
                tw = tw*ft%space_width
                next_tab_x = ((x + tw)/tw)*tw
                if (next_tab_x - x < ft%space_width) next_tab_x = next_tab_x + tw
                g%pixel_width = next_tab_x - x
            end if
            return
        end if
        call f%font_driver%text_extents(id, g%ch, pcm, found)
        if (found) then
            if (pcm%width == 0 .and. pcm%rbearing == 0 .and. pcm%lbearing == 0) found = .false.
        end if
        if (found) then
            g%pixel_width = pcm%width
        else
            g%pixel_width = ft%space_width
        end if
        ! All visible glyphs have at least 1-pixel width.
        if (g%pixel_width == 0) g%pixel_width = 1
    end subroutine gui_produce_glyph

    !> The ascent and descent of the font of face `face_id' on
    !> window-system frame `f'.  Port of xdisp.c:normal_char_ascent_descent
    !> (for a font not too high).
    subroutine normal_char_ascent_descent(f, face_id, ascent, descent)
        type(frame_type), intent(in) :: f
        integer(int32), intent(in) :: face_id
        integer, intent(out) :: ascent, descent
        type(font_type) :: ft
        integer :: id
        ascent = 0
        descent = 0
        id = f%faces(face_id)%font
        if (id <= 0 .or. .not. associated(f%font_driver)) return
        ft = f%font_driver%font(id)
        ascent = ft%ascent + ft%baseline_offset
        descent = ft%descent - ft%baseline_offset
    end subroutine normal_char_ascent_descent

    !> The continuation glyph: \ in the default face, sized for frame `f'.
    !> The IT_CONTINUATION part of xdisp.c:produce_special_glyphs (no
    !> display table).
    function continuation_glyph(f) result(g)
        type(frame_type), intent(in) :: f
        type(glyph_type) :: g
        g = glyph_type(face_id=DEFAULT_FACE_ID)
        g%ch = iachar("\")
        if (frame_window_p(f)) call gui_produce_glyph(f, g, 0)
    end function continuation_glyph

    !> The width of the continuation glyph, which the text area leaves
    !> room for at the right of a window without a right fringe
    !> (it->continuation_pixel_width, set in xdisp.c:init_iterator).
    integer function continuation_pixel_width(f) result(w)
        type(frame_type), intent(in) :: f
        type(glyph_type) :: g
        g = continuation_glyph(f)
        w = g%pixel_width
    end function continuation_pixel_width

    !> The height of a mode line in face `face_id' on frame `f': 1 on a
    !> terminal, else the height of the face's font (the frame's line
    !> height if it has none).  Port of xdisp.c:estimate_mode_line_height
    !> (faces have no box).
    integer function estimate_mode_line_height(f, face_id) result(height)
        type(frame_type), intent(in) :: f
        integer(int32), intent(in) :: face_id
        type(font_type) :: ft
        integer :: id
        height = 1
        if (.not. frame_window_p(f)) return
        height = f%line_height
        if (.not. allocated(f%faces) .or. .not. associated(f%font_driver)) return
        if (face_id > ubound(f%faces, 1)) return
        id = f%faces(face_id)%font
        if (id <= 0) return
        ft = f%font_driver%font(id)
        height = ft%height
    end function estimate_mode_line_height

    !> Lay out one screen row of buffer slot `bi' from `s' (updated to the
    !> start of the next row), with the continuation glyph \ at column
    !> `last_x' + 1 (it->last_visible_x is `last_x').  A character that
    !> does not fit continues on the next row: a wide character whole,
    !> after \ in every cell left (cont width grows by the row's columns
    !> before it); the cells of others (TAB, ^X, \ooo) are split, those
    !> that fit staying on this row (cont width grows by `last_x').
    !> `ends' says how the row ended.  With `glyphs', the cells are drawn
    !> there, and with frame `f' too in their faces: the face of the text
    !> (its `face' property and the region [rb, re)), `escape_glyph' merged
    !> into it for ^X and \ooo, `nobreak_space' and `nobreak_hyphen' for
    !> non-ASCII spaces and hyphens; after the text of a line a space in
    !> the face of the newline, and the rest of the row in the face the
    !> newline's face extends with (append_space_for_newline and
    !> extend_face_to_end_of_line).  `cursor_col' is the column of `pt' if
    !> it is shown on this row, else -1; `used' the columns used.  The
    !> terminal part of xdisp.c:display_line with
    !> xdisp.c:get_next_display_element and term.c:produce_glyphs.  On a
    !> window-system frame (with `glyphs' and `f'), `last_x' and
    !> `cursor_col' are pixels and display_row_gui lays the row out;
    !> `cursor_hpos' is the glyph of the cursor, `ascent' and `descent'
    !> the row's (0 on a terminal).
    subroutine display_row(bi, s, last_x, pt, ends, cursor_col, used, glyphs, rb, re, f, cursor_hpos, ascent, descent)
        integer(int64), intent(in) :: bi
        type(row_start_type), intent(inout) :: s
        integer, intent(in) :: last_x
        integer(int64), intent(in) :: pt
        integer, intent(out) :: ends, cursor_col, used
        type(glyph_type), intent(inout), optional :: glyphs(:)
        integer(int64), intent(in), optional :: rb, re
        type(frame_type), intent(inout), optional :: f
        integer, intent(out), optional :: cursor_hpos, ascent, descent
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        integer(int64) :: zv, stop, rbeg, rend, endpos, fstop
        integer(int32) :: c, face_id, extend_face_id
        integer :: col, n, first, ncells, k, hpos, asc, desc
        logical :: faces
        zv = buffers(bi)%zv
        col = 0
        cursor_col = -1
        faces = present(glyphs) .and. present(f)
        rbeg = 0
        rend = 0
        if (present(rb) .and. present(re)) then
            rbeg = rb
            rend = re
        end if
        if (present(cursor_hpos)) cursor_hpos = 0
        if (present(ascent)) ascent = 0
        if (present(descent)) descent = 0
        if (faces) then
            if (frame_window_p(f)) then
                call display_row_gui(bi, s, last_x, pt, ends, cursor_col, hpos, used, glyphs, rbeg, rend, f, asc, desc)
                if (present(cursor_hpos)) cursor_hpos = hpos
                if (present(ascent)) ascent = asc
                if (present(descent)) descent = desc
                return
            end if
        end if
        face_id = DEFAULT_FACE_ID
        stop = -1
        do
            if (s%pos >= zv) then
                if (s%pos == pt) cursor_col = col
                ends = ROW_ENDS_AT_ZV
                exit
            end if
            ! handle_fontified_prop and handle_face_prop at the next stop.
            if (faces .and. s%pos >= stop) then
                call handle_fontified_prop(bi, s%pos, s%pos + TEXT_PROP_DISTANCE_LIMIT, fstop)
                face_id = face_at_buffer_position(f, bi, s%pos, stop, s%pos + TEXT_PROP_DISTANCE_LIMIT, rbeg, rend)
                stop = min(stop, fstop)
            end if
            c = buf_fetch_char(buffers(bi), s%byte)
            if (c == 10) then
                if (s%pos == pt) cursor_col = col
                if (faces .and. face_id /= DEFAULT_FACE_ID .and. col + 1 <= size(glyphs)) then
                    ! A space for the cursor at the end of the line, in the
                    ! newline's face.
                    glyphs(col + 1) = glyph_type(face_id=face_id)
                    col = col + 1
                    ! The rest of the row in the face that extends.
                    extend_face_id = face_at_buffer_position(f, bi, s%pos, endpos, s%pos + TEXT_PROP_DISTANCE_LIMIT, &
                                                             rbeg, rend, extend_only=.true.)
                    if (extend_face_id /= DEFAULT_FACE_ID .and. col < last_x + 1) then
                        glyphs(col + 1:last_x + 1) = glyph_type(face_id=extend_face_id)
                        col = last_x + 1
                    end if
                end if
                s = row_start_type(pos=s%pos + 1, byte=s%byte + 1)
                ends = ROW_ENDS_IN_NEWLINE
                exit
            end if
            cells = glyph_type()
            call produce_char(c, col + s%cont, cells, n)
            if (faces) cells(1:n)%face_id = char_face(f, c, face_id)
            ! The rest of a ^X or \ooo begun on the row before (a TAB's
            ! rest follows from the column).
            first = 1
            if (s%mid .and. c /= 9) first = min(n, s%skip + 1)
            ncells = n - first + 1
            if (col + ncells > last_x .and. col > 0) then
                ends = ROW_CONTINUED
                if (n == 2 .and. cells(2)%padding) then
                    ! A wide character is unbreakable: fill the rest of the
                    ! row with continuation glyphs, like in 20.x.
                    if (present(glyphs)) then
                        glyphs(col + 1:last_x + 1) = glyph_type()
                        glyphs(col + 1:last_x + 1)%ch = iachar("\")
                    end if
                    s%cont = s%cont + col
                else
                    k = last_x - col
                    if (k > 0) then
                        if (present(glyphs)) glyphs(col + 1:col + k) = cells(first:first + k - 1)
                        if (s%pos == pt .and. .not. s%mid) cursor_col = col
                        s%skip = first - 1 + k
                        s%mid = .true.
                    end if
                    if (present(glyphs)) then
                        glyphs(last_x + 1) = glyph_type()
                        glyphs(last_x + 1)%ch = iachar("\")
                    end if
                    s%cont = s%cont + last_x
                end if
                col = last_x + 1
                exit
            end if
            if (present(glyphs)) then
                ! (Only a character alone on a too narrow row is clipped.)
                k = min(ncells, size(glyphs) - col)
                if (k > 0) glyphs(col + 1:col + k) = cells(first:first + k - 1)
            end if
            if (s%pos == pt .and. .not. s%mid) cursor_col = col
            col = col + ncells
            s%pos = s%pos + 1
            s%byte = s%byte + buf_next_char_len(buffers(bi), s%byte)
            s%mid = .false.
            s%skip = 0
        end do
        used = min(col, last_x + 1)
        if (present(cursor_hpos)) cursor_hpos = cursor_col + 1
    end subroutine display_row

    !> display_row on window-system frame `f': lay out one screen row of
    !> buffer slot `bi' from `s' in `glyphs', glyphs as wide as their
    !> fonts make them, up to pixel `last_x' (the text area less the
    !> continuation glyph).  A glyph that does not fit ends the row with
    !> the continuation glyph after the last that fits, and its character
    !> begins the next row (the rest of a ^X or \ooo display vector after
    !> the elements shown); a TAB that does not fit fills the row instead,
    !> the next row starting in its middle; a glyph alone on a row too
    !> narrow for it stays there.  After the text of a line, and at the end
    !> of the accessible text, a space for the cursor (in the newline's
    !> face, or the default face).  `cursor_x' is the x of `pt' if shown
    !> on this row, else -1, and `cursor_hpos' its glyph; `used' the
    !> glyphs used; `ascent' and `descent' the row's (the tallest fonts of
    !> the glyphs shown and the newline; for an empty row the space's).
    !> Port of the window-system part of xdisp.c:display_line with
    !> gui_produce_glyphs and append_space_for_newline.
    subroutine display_row_gui(bi, s, last_x, pt, ends, cursor_x, cursor_hpos, used, glyphs, rbeg, rend, f, &
                               ascent, descent)
        integer(int64), intent(in) :: bi, pt, rbeg, rend
        type(row_start_type), intent(inout) :: s
        integer, intent(in) :: last_x
        integer, intent(out) :: ends, cursor_x, cursor_hpos, used, ascent, descent
        type(glyph_type), intent(inout) :: glyphs(:)
        type(frame_type), intent(inout) :: f
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        integer(int64) :: zv, stop, fstop
        integer(int32) :: c, face_id
        integer :: x, n, first, k, a, d
        logical :: shown, continued
        zv = buffers(bi)%zv
        x = 0
        used = 0
        cursor_x = -1
        cursor_hpos = 0
        ascent = 0
        descent = 0
        face_id = DEFAULT_FACE_ID
        stop = -1
        do
            if (s%pos >= zv) then
                if (s%pos == pt) call set_cursor()
                call append_space(DEFAULT_FACE_ID)
                ends = ROW_ENDS_AT_ZV
                exit
            end if
            ! handle_fontified_prop and handle_face_prop at the next stop.
            if (s%pos >= stop) then
                call handle_fontified_prop(bi, s%pos, s%pos + TEXT_PROP_DISTANCE_LIMIT, fstop)
                face_id = face_at_buffer_position(f, bi, s%pos, stop, s%pos + TEXT_PROP_DISTANCE_LIMIT, rbeg, rend)
                stop = min(stop, fstop)
            end if
            c = buf_fetch_char(buffers(bi), s%byte)
            if (c == 10) then
                if (s%pos == pt) call set_cursor()
                ! A newline has no width, but the height of its face's font.
                call normal_char_ascent_descent(f, face_id, a, d)
                ascent = max(ascent, a)
                descent = max(descent, d)
                call append_space(face_id)
                s = row_start_type(pos=s%pos + 1, byte=s%byte + 1)
                ends = ROW_ENDS_IN_NEWLINE
                exit
            end if
            cells = glyph_type()
            call produce_char(c, x + s%cont, cells, n, gui=.true.)
            cells(1:n)%face_id = char_face(f, c, face_id)
            ! The rest of a display vector begun on the row before.
            first = 1
            if (s%mid .and. c /= 9) first = min(n, s%skip + 1)
            shown = .false.
            continued = .false.
            do k = first, n
                call gui_produce_glyph(f, cells(k), x + s%cont)
                if (x + cells(k)%pixel_width > last_x) then
                    continued = .true.
                    exit
                end if
                call show(cells(k))
            end do
            if (continued) then
                ends = ROW_CONTINUED
                if (used == 0) then
                    ! The glyph is the only one on the row: it stays, and
                    ! the line continues after it.
                    call show(cells(k))
                    s%cont = s%cont + x
                    if (k == n) then
                        call next_char()
                    else
                        s%mid = .true.
                        s%skip = k
                    end if
                else if (cells(k)%type == STRETCH_GLYPH) then
                    ! A TAB that extends past the right edge fills the row
                    ! without being consumed.
                    cells(k)%pixel_width = last_x - x
                    call show(cells(k))
                    call put(continuation_glyph(f), .true.)
                    s%cont = s%cont + last_x
                    s%mid = .true.
                    s%skip = 0
                else
                    ! Something else draws past the right edge: it goes to
                    ! the next row (its height not counted on this one).
                    call put(continuation_glyph(f), .false.)
                    s%cont = s%cont + x
                    if (k > 1) then
                        s%mid = .true.
                        s%skip = k - 1
                    else
                        s%mid = .false.
                        s%skip = 0
                    end if
                end if
                exit
            end if
            call next_char()
        end do
    contains

        !> The cursor goes before the next glyph.
        subroutine set_cursor()
            cursor_x = x
            cursor_hpos = used + 1
        end subroutine set_cursor

        !> Show glyph `g' of the current character, the cursor on its first
        !> one if it is at `pt'.
        subroutine show(g)
            type(glyph_type), intent(in) :: g
            if (.not. shown .and. s%pos == pt .and. .not. s%mid) call set_cursor()
            shown = .true.
            call put(g, .true.)
            x = x + g%pixel_width
        end subroutine show

        !> Append `g' if there is room, its height counted if `counted'.
        subroutine put(g, counted)
            type(glyph_type), intent(in) :: g
            logical, intent(in) :: counted
            if (used >= size(glyphs)) return
            used = used + 1
            glyphs(used) = g
            if (counted) then
                ascent = max(ascent, g%ascent)
                descent = max(descent, g%descent)
            end if
        end subroutine put

        !> Go on to the next character.
        subroutine next_char()
            s%pos = s%pos + 1
            s%byte = s%byte + buf_next_char_len(buffers(bi), s%byte)
            s%mid = .false.
            s%skip = 0
        end subroutine next_char

        !> append_space_for_newline: a space in face `id' for the cursor;
        !> an empty row gets the height of its font.
        subroutine append_space(id)
            integer(int32), intent(in) :: id
            type(glyph_type) :: g
            if (used >= size(glyphs)) return
            g = glyph_type(face_id=id)
            call gui_produce_glyph(f, g, x + s%cont)
            if (used == 0) call normal_char_ascent_descent(f, id, ascent, descent)
            g%ascent = ascent
            g%descent = descent
            used = used + 1
            glyphs(used) = g
        end subroutine append_space
    end subroutine display_row_gui

    !> If the text at POS in buffer slot BI is not fontified (its
    !> `fontified' property is false), call `fontification_functions' with
    !> POS, in that buffer; NEXT gets the next position (at most LIMIT)
    !> where `fontified' changes, if there are such functions.  Port of
    !> xdisp.c:handle_fontified_prop.
    subroutine handle_fontified_prop(bi, pos, limit, next)
        integer(int64), intent(in) :: bi, pos, limit
        integer(int64), intent(out) :: next
        type(value_type) :: prop
        type(status_type) :: st
        integer(int64) :: saved
        logical :: found
        next = limit
        if (fontifying) return
        saved = cur
        call set_buffer_internal(bi)
        if (.not. fontification_functions%empty()) then
            ! (The nil value always present at the end of the buffer is
            ! ignored: no amount of fontifying will change it.)
            if (pos < buffers(bi)%text%z) then
                call get_text_property(pos, sym("fontified"), prop, st)
                if (nilp(prop)) then
                    fontifying = .true.
                    call fontification_functions%run(pos, st, safe=.true.)
                    fontifying = .false.
                    ! (A function may have switched buffers: a fig leaf.)
                    call set_buffer_internal(bi)
                end if
            end if
            call next_single_property_change(pos, sym("fontified"), next, found, st, limit)
            if (.not. found) next = limit
        end if
        if (saved > 0) call set_buffer_internal(saved)
    end subroutine handle_fontified_prop

    !> The face of character C in text whose face is FACE_ID on frame F:
    !> control characters and raw bytes shown as ^X or \ooo in the
    !> escape-glyph face merged in, non-ASCII spaces and hyphens with
    !> nobreak-space and nobreak-hyphen (nobreak-char-display t).  The
    !> face part of xdisp.c:get_next_display_element.
    integer(int32) function char_face(f, c, face_id) result(id)
        type(frame_type), intent(inout) :: f
        integer(int32), intent(in) :: c, face_id
        id = face_id
        if (c /= 9 .and. c /= 10 .and. (c < 32 .or. c == 127 .or. char_byte8_p(c))) then
            ! merge_escape_glyph_face.
            id = merge_faces(f, "escape_glyph", 0_int32, face_id)
        else if (nonascii_blank_p(c)) then
            ! Merge `nobreak-space' into the current face.
            id = merge_faces(f, "nobreak_space", 0_int32, face_id)
        else if (c == int(z'AD', int32) .or. c == int(z'2010', int32) .or. c == int(z'2011', int32)) then
            ! SOFT_HYPHEN, HYPHEN, NON_BREAKING_HYPHEN.
            id = merge_faces(f, "nobreak_hyphen", 0_int32, face_id)
        end if
    end function char_face

    !> Whether C is a non-ASCII blank (Unicode general category Zs), as
    !> character.c:blankp tells for the characters `nobreak_char_display'
    !> highlights.
    elemental logical function nonascii_blank_p(c)
        integer(int32), intent(in) :: c
        nonascii_blank_p = c == int(z'A0', int32) .or. c == int(z'1680', int32) .or. &
                           (c >= int(z'2000', int32) .and. c <= int(z'200A', int32)) .or. &
                           c == int(z'202F', int32) .or. c == int(z'205F', int32) .or. c == int(z'3000', int32)
    end function nonascii_blank_p

    !> The screen rows of the line starting at `bol' in buffer slot `bi',
    !> rows(1:nrows) where each starts, and `eol' where the line ends (its
    !> newline, or the end of the accessible text).  `last_x' as for
    !> display_row; on window-system frame `f', pixels.
    subroutine display_line_rows(bi, bol, last_x, rows, nrows, eol, f)
        integer(int64), intent(in) :: bi, bol
        integer, intent(in) :: last_x
        type(row_start_type), allocatable, intent(out) :: rows(:)
        integer, intent(out) :: nrows
        integer(int64), intent(out) :: eol
        type(frame_type), intent(inout), optional :: f
        type(row_start_type), allocatable :: grown(:)
        type(row_start_type) :: s
        type(glyph_type), allocatable :: scratch(:)
        integer :: ends, cursor_col, used
        logical :: gui
        gui = .false.
        if (present(f)) gui = frame_window_p(f)
        if (gui) allocate (scratch(last_x/max(1, f%smallest_char_width) + 4))
        allocate (rows(8))
        nrows = 0
        s%pos = bol
        s%byte = buf_charpos_to_bytepos(bi, bol)
        do
            nrows = nrows + 1
            if (nrows > size(rows)) then
                allocate (grown(2*size(rows)))
                grown(1:size(rows)) = rows
                call move_alloc(grown, rows)
            end if
            rows(nrows) = s
            if (gui) then
                call display_row(bi, s, last_x, -1_int64, ends, cursor_col, used, scratch, f=f)
            else
                call display_row(bi, s, last_x, -1_int64, ends, cursor_col, used)
            end if
            if (ends /= ROW_CONTINUED) exit
        end do
        eol = s%pos
        if (ends == ROW_ENDS_IN_NEWLINE) eol = s%pos - 1
    end subroutine display_line_rows

    !> The row among rows(1:nrows) on which the display of `pos' begins.
    pure integer function display_row_of(pos, rows, nrows) result(k)
        integer(int64), intent(in) :: pos
        type(row_start_type), intent(in) :: rows(:)
        integer, intent(in) :: nrows
        k = nrows
        do while (k > 1)
            if (rows(k)%pos < pos) exit
            if (rows(k)%pos == pos .and. .not. rows(k)%mid) exit
            k = k - 1
        end do
    end function display_row_of

    !> The start of the line holding `pos' in buffer slot `bi'.
    integer(int64) function line_beginning(bi, pos) result(bol)
        integer(int64), intent(in) :: bi, pos
        integer(int64) :: byte, begv_byte
        begv_byte = buffers(bi)%begv_byte
        byte = buf_charpos_to_bytepos(bi, pos)
        do while (byte > begv_byte)
            if (buf_fetch_byte(buffers(bi), byte - 1) == 10) exit
            byte = byte - 1
        end do
        bol = buf_bytepos_to_charpos(bi, byte)
    end function line_beginning

    !> Where display starts for window start `start': the continuation
    !> width of the row it begins.  Port of the line-start part of
    !> xdisp.c:start_display.
    function window_start_row(bi, start, last_x, f) result(s)
        integer(int64), intent(in) :: bi, start
        integer, intent(in) :: last_x
        type(frame_type), intent(inout), optional :: f
        type(row_start_type) :: s
        type(row_start_type), allocatable :: rows(:)
        integer(int64) :: bol, eol
        integer :: nrows, k
        s%pos = start
        s%byte = buf_charpos_to_bytepos(bi, start)
        bol = line_beginning(bi, start)
        if (bol == start) return
        call display_line_rows(bi, bol, last_x, rows, nrows, eol, f)
        k = nrows
        do while (k > 1)
            if (rows(k)%pos <= start) exit
            k = k - 1
        end do
        s%cont = rows(k)%cont
    end function window_start_row

    !> Lay out buffer slot `bi' from `start' in `rows' (one per window
    !> body line) `width' columns wide.  Gives the cursor position of `pt'
    !> (cursor_row -1 if it is not shown), where the display ends, and
    !> whether it reached the end of the accessible text.  On a
    !> window-system frame `f', `width' and `cursor_col' are pixels, and
    !> rows are laid out while they start above `last_y' (the height of
    !> the text area in pixels); `nrows' tells how many, `cursor_hpos' the
    !> glyph of the cursor.  Rows get their pixel position and metrics
    !> (xdisp.c:compute_line_metrics).  The core of xdisp.c:try_window
    !> with display_line.
    subroutine layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, &
                             end_pos, reached_end, region_beg, region_end, f, last_y, nrows, cursor_hpos)
        integer(int64), intent(in) :: bi, start, pt
        integer, intent(in) :: width
        type(glyph_row_type), intent(inout) :: rows(:)
        integer, intent(out) :: cursor_row, cursor_col
        integer(int64), intent(out) :: end_pos
        logical, intent(out) :: reached_end
        !> The region to highlight with the region face, [region_beg, region_end).
        integer(int64), intent(in), optional :: region_beg, region_end
        !> The frame whose faces the glyphs get (without it, all are in the
        !> default face).
        type(frame_type), intent(inout), optional :: f
        integer, intent(in), optional :: last_y
        integer, intent(out), optional :: nrows, cursor_hpos
        type(row_start_type) :: s
        integer(int64) :: rb, re
        integer :: r, ends, col, used, last_x, y, ylimit, hpos, asc, desc, k
        logical :: gui
        cursor_row = -1
        cursor_col = 0
        if (present(cursor_hpos)) cursor_hpos = 1
        reached_end = .false.
        gui = .false.
        if (present(f)) gui = frame_window_p(f)
        rb = 0
        re = 0
        if (present(region_beg) .and. present(region_end)) then
            rb = region_beg
            re = region_end
        end if
        ! Room for the continuation glyph at the right edge.
        last_x = width - 1
        if (gui) last_x = width - continuation_pixel_width(f)
        ylimit = size(rows)
        if (present(last_y)) ylimit = last_y
        if (gui) then
            s = window_start_row(bi, start, last_x, f)
        else
            s = window_start_row(bi, start, last_x)
        end if
        y = 0
        if (present(nrows)) nrows = 0
        do r = 1, size(rows)
            rows(r)%glyphs = glyph_type()
            rows(r)%used = 0
            rows(r)%enabled_p = .true.
            rows(r)%x = 0
            rows(r)%y = y
            rows(r)%ascent = 0
            rows(r)%height = 1
            rows(r)%visible_height = 1
            rows(r)%pixel_width = 0
            rows(r)%continued_p = .false.
            rows(r)%ends_at_zv_p = reached_end
            if (gui) then
                ! try_window: rows down to the bottom of the text area.
                if (y >= ylimit) then
                    rows(r)%enabled_p = .false.
                    cycle
                end if
            else if (reached_end) then
                y = y + 1
                if (present(nrows)) nrows = r
                cycle
            end if
            if (present(f)) then
                call display_row(bi, s, last_x, pt, ends, col, used, rows(r)%glyphs, rb, re, f, hpos, asc, desc)
            else
                call display_row(bi, s, last_x, pt, ends, col, used, rows(r)%glyphs, rb, re, cursor_hpos=hpos)
            end if
            rows(r)%used = used
            rows(r)%continued_p = ends == ROW_CONTINUED
            rows(r)%ends_at_zv_p = ends == ROW_ENDS_AT_ZV
            ! compute_line_metrics.
            if (gui) then
                if (asc + desc == 0) desc = f%line_height
                rows(r)%ascent = asc
                rows(r)%height = asc + desc
                rows(r)%pixel_width = rows(r)%x
                do k = 1, used
                    rows(r)%pixel_width = rows(r)%pixel_width + rows(r)%glyphs(k)%pixel_width
                end do
                rows(r)%visible_height = rows(r)%height
                if (y + rows(r)%height > ylimit) rows(r)%visible_height = ylimit - y
            else
                rows(r)%pixel_width = used
                if (rows(r)%continued_p) rows(r)%pixel_width = used - 1
            end if
            if (col >= 0 .and. cursor_row < 0) then
                cursor_row = r - 1
                cursor_col = col
                if (present(cursor_hpos)) cursor_hpos = hpos
            end if
            if (ends == ROW_ENDS_AT_ZV) reached_end = .true.
            y = y + rows(r)%height
            if (present(nrows)) nrows = r
        end do
        end_pos = s%pos
        if (s%pos >= buffers(bi)%zv) reached_end = .true.
    end subroutine layout_window

    !> The window start that puts point's screen line `dy' screen lines
    !> from the top: the start of the screen line `dy' above the one
    !> holding `pt' (not before the accessible start).  The terminal part
    !> of xdisp.c:move_it_vertically_backward as redisplay_window's
    !> recentering uses it.
    integer(int64) function recenter_start(bi, pt, last_x, dy) result(start)
        integer(int64), intent(in) :: bi, pt
        integer, intent(in) :: last_x, dy
        type(row_start_type), allocatable :: rows(:)
        integer(int64) :: bol, eol
        integer :: nrows, target
        bol = line_beginning(bi, pt)
        call display_line_rows(bi, bol, last_x, rows, nrows, eol)
        target = display_row_of(pt, rows, nrows) - dy
        do while (target < 1)
            if (bol <= buffers(bi)%begv) then
                target = 1
                exit
            end if
            bol = line_beginning(bi, bol - 1)
            call display_line_rows(bi, bol, last_x, rows, nrows, eol)
            target = target + nrows
        end do
        start = rows(target)%pos
    end function recenter_start

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
                    m%glyphs(left + n + 1) = glyph_type(face_id=VERTICAL_BORDER_FACE_ID)
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
        integer :: body, width, cursor_row, cursor_col, r, last_y, nrows, cursor_hpos, left, top, capacity
        integer(int32) :: mode_line_face
        logical :: reached_end, gui
        call resolve_buffer(w%buffer, bi, status)
        if (.not. status%ok()) return
        gui = frame_window_p(f)
        body = window_body_lines(w)
        width = window_text_cols(w)
        capacity = width
        if (gui) then
            ! The text area in pixels, the mode line's height off the
            ! bottom; as many rows and glyphs as the smallest font allows
            ! (required_matrix_height and required_matrix_width).
            width = window_body_pixel_width(f, w)
            mode_line_face = MODE_LINE_INACTIVE_FACE_ID
            if (selected) mode_line_face = MODE_LINE_ACTIVE_FACE_ID
            last_y = window_pixel_height(f, w)
            if (.not. w%mini) last_y = last_y - estimate_mode_line_height(f, mode_line_face)
            body = last_y/max(1, f%smallest_font_height) + 2
            capacity = width/max(1, f%smallest_char_width) + 4
        end if
        allocate (rows(body))
        do r = 1, body
            allocate (rows(r)%glyphs(capacity))
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
        if (gui) then
            call layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, end_pos, &
                               reached_end, rb, re, f, last_y, nrows, cursor_hpos)
            ! (Recentering in pixels is Deferred: the start stays.)
            call set_window_start(w, start, status, noforce=.true.)
            w%window_end_pos = end_pos
            w%window_end_valid = reached_end
            ! (w->cursor: the row and the glyph, 0-based.)
            w%cursor_vpos = cursor_row
            w%cursor_hpos = cursor_hpos - 1
            ! update_window: the rows, the mode line, then the cursor's
            ! place and the borders (gui_update_window_end).
            left = window_pixel_left(f, w)
            top = window_pixel_top(f, w)
            do r = 1, nrows
                if (rows(r)%enabled_p) call update_window_line(f, rows(r), left, top, left + width)
            end do
            if (.not. w%mini) call display_mode_line(f, w, bi, start, pt, selected)
            if (cursor_row >= 0) call get_phys_cursor_geometry(f, w, rows(cursor_row + 1), cursor_col, cursor_hpos, last_y)
            if (.not. w%mini) call gui_draw_vertical_border(f, w, last_y)
            return
        end if
        call layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, end_pos, &
                           reached_end, rb, re, f)
        if (cursor_row < 0 .and. .not. w%force_start) then
            ! Point is not visible: recenter (scroll-conservatively 0),
            ! the window start half the height of the window backward
            ! from point.
            start = recenter_start(bi, pt, width - 1, body/2)
            call layout_window(bi, start, pt, width, rows, cursor_row, cursor_col, end_pos, &
                               reached_end, rb, re, f)
        end if
        call set_window_start(w, start, status, noforce=.true.)
        w%window_end_pos = end_pos
        w%window_end_valid = reached_end
        w%cursor_vpos = cursor_row
        w%cursor_hpos = cursor_col
        call put_rows(f, w%top_line, w%left_col, rows, w%divider .and. .not. w%mini)
        if (.not. w%mini) call display_mode_line(f, w, bi, start, pt, selected)
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

    !> The characters of `text' (internal multibyte form), a byte that is
    !> not part of a valid form taken as a raw byte.
    subroutine decode_string(text, chars, n)
        character(len=*), intent(in) :: text
        integer(int32), allocatable, intent(out) :: chars(:)
        integer, intent(out) :: n
        integer(int64) :: pos
        integer(int32) :: length
        allocate (chars(len(text)))
        n = 0
        pos = 1
        do while (pos <= len(text, kind=int64))
            n = n + 1
            length = multibyte_length(text, pos, .true.)
            if (length == 0) then
                chars(n) = byte8_to_char(ichar(text(pos:pos), int32))
                length = 1
            else
                call string_char_and_length(text, pos, chars(n), length)
            end if
            pos = pos + length
        end do
    end subroutine decode_string

    !> `text' padded with spaces to `width' characters (display_string's
    !> FIELD_WIDTH).
    function pad_chars(text, width) result(s)
        character(len=*), intent(in) :: text
        integer, intent(in) :: width
        character(len=:), allocatable :: s
        integer(int32), allocatable :: chars(:)
        integer :: n
        call decode_string(text, chars, n)
        s = text//repeat(" ", max(0, width - n))
    end function pad_chars

    !> The first `width' characters of `text' (display_string's PRECISION).
    function truncate_chars(text, width) result(s)
        character(len=*), intent(in) :: text
        integer, intent(in) :: width
        character(len=:), allocatable :: s
        integer(int64) :: pos
        integer(int32) :: length
        integer :: n
        pos = 1
        n = 0
        do while (pos <= len(text, kind=int64) .and. n < width)
            length = max(1_int32, multibyte_length(text, pos, .true.))
            pos = pos + length
            n = n + 1
        end do
        s = text(1:pos - 1)
    end function truncate_chars

    !> The default mode line of a buffer named NAME on a text terminal,
    !> before it is cut to the window: bindings.el's
    !> standard-mode-line-format as xdisp.c:display_mode_element and
    !> decode_mode_spec expand it ("%e", mode-line-front-space "-",
    !> mode-line-mule-info "%z" with the end-of-line mnemonic,
    !> mode-line-modified "%1*%1+", mode-line-remote "%1@",
    !> mode-line-frame-identification " %F  ", "%12b", "   ",
    !> mode-line-position: (-3 "%p") and (6 " L%l"), "  ",
    !> mode-line-modes "%[(" MODES ")%] ", mode-line-end-spaces "-%-").
    !> POSITION is what %p gives (`All', `Top', `Bottom' or ` 5%'), LINE
    !> what %l gives, MODES the text in parentheses (mode_line_modes,
    !> default "Fundamental").  The dashes of %- are WIDTH long.
    !> Simplified: all coding systems are UTF-8 with Unix line ends
    !> (UUU:), the buffers are local (-) and the frame is F1.  On a
    !> window-system frame (`graphic', display-graphic-p) the front space
    !> is " ", the frame identification "  " (mode-line-frame-control) and
    !> there are no end dashes.  PREFIX is what `mode_line_modes' shows
    !> before its "(".
    function mode_line_text(name, modified, read_only, position, line, width, modes, graphic, prefix) result(s)
        character(len=*), intent(in) :: name, position, line
        logical, intent(in) :: modified, read_only
        integer, intent(in) :: width
        character(len=*), intent(in), optional :: modes
        logical, intent(in), optional :: graphic
        !> The elements of `mode_line_modes' before its "(" (mode_line_modes_prefix).
        character(len=*), intent(in), optional :: prefix
        character(len=:), allocatable :: s
        character(len=1) :: star, plus
        logical :: gui
        gui = .false.
        if (present(graphic)) gui = graphic
        ! %*: % if read-only, else * if modified; %+: * if modified, else
        ! % if read-only.
        star = "-"
        plus = "-"
        if (modified) then
            star = "*"
            plus = "*"
        end if
        if (read_only) then
            star = "%"
            if (.not. modified) plus = "%"
        end if
        if (gui) then
            s = " UUU:"//star//plus//"-"//"  "
        else
            s = "-UUU:"//star//plus//"-"//" F1  "
        end if
        s = s//pad_chars(name, 12)//"   "//truncate_chars(position, 3)//pad_chars(" L"//line, 6)//"  "
        if (present(prefix)) s = s//prefix
        s = s//"("
        if (present(modes)) then
            s = s//modes
        else
            s = s//"Fundamental"
        end if
        s = s//") "
        if (.not. gui) s = s//repeat("-", max(0, width))
    end function mode_line_text

    !> What %p shows: the percentage of the accessible text above window
    !> start `start', or All, Top or Bottom.  `at_end' says the window
    !> shows the end of the accessible text.  Port of the %p case of
    !> xdisp.c:decode_mode_spec, with percent99.
    function mode_line_percent(bi, start, at_end) result(s)
        integer(int64), intent(in) :: bi, start
        logical, intent(in) :: at_end
        character(len=:), allocatable :: s
        character(len=8) :: buf
        integer(int64) :: begv, d
        integer :: percent
        begv = buffers(bi)%begv
        if (at_end) then
            if (start <= begv) then
                s = "All"
            else
                s = "Bottom"
            end if
        else if (start <= begv) then
            s = "Top"
        else
            ! percent99: rounded up, at most 99.
            d = buffers(bi)%zv - begv
            percent = int(min(99_int64, (d - 1 + 100*(start - begv))/d))
            write (buf, '(i2,"%")') percent
            s = trim(buf)
        end if
    end function mode_line_percent

    !> What %l shows for a window `height' lines high starting at
    !> `start': point's line number, or ?? when the lines before the
    !> window start are too long to count (line-number-display-limit-width
    !> 200).  The counting part of the %l case of xdisp.c:decode_mode_spec
    !> (no base line cache, line-number-display-limit nil).
    function mode_line_line(bi, start, pt, height) result(s)
        integer(int64), intent(in) :: bi, start, pt
        integer, intent(in) :: height
        character(len=:), allocatable :: s
        character(len=20) :: buf
        integer(int64), parameter :: limit_width = 200
        integer(int64) :: distance, limit, byte, nlines, wanted
        if (start > buffers(bi)%begv .and. start <= buffers(bi)%zv) then
            ! Look back for height * 2 + 30 lines from the window start,
            ! within line-number-display-limit-width characters a line.
            wanted = 2_int64*height + 30
            distance = limit_width*wanted
            limit = buffers(bi)%begv
            if (start - distance > limit) then
                limit = start - distance
                byte = buf_charpos_to_bytepos(bi, start)
                nlines = 0
                do while (byte > buf_charpos_to_bytepos(bi, limit) .and. nlines < wanted)
                    byte = byte - 1
                    if (buf_fetch_byte(buffers(bi), byte) == 10) nlines = nlines + 1
                end do
                if (nlines < wanted) then
                    s = "??"
                    return
                end if
            end if
        end if
        write (buf, "(i0)") line_number(bi, pt)
        s = trim(buf)
    end function mode_line_line

    !> Lay out `text' as a mode line `width' columns wide in `row':
    !> truncated at the right edge (a TAB or wide character cut there
    !> shows as -), filled with spaces, in face `face_id' (default
    !> mode_line_active), characters `span_first' to `span_last' in face
    !> `span_face' (a propertized part of the mode line), and those of
    !> each of `spans' in its face.  The terminal part of
    !> xdisp.c:display_string for mode lines, with pad_mode_line.  On a
    !> window-system frame `f', `width' is pixels: glyphs are produced
    !> while they start left of it (the last may be cut off there), then
    !> spaces fill the rest ("Fill up with spaces" in
    !> xdisp.c:display_mode_line); `used' gives the number of glyphs.
    subroutine mode_line_row(text, width, row, face_id, span_first, span_last, span_face, spans, f, used)
        character(len=*), intent(in) :: text
        integer, intent(in) :: width
        type(glyph_type), intent(out) :: row(:)
        integer(int32), intent(in), optional :: face_id, span_face
        integer, intent(in), optional :: span_first, span_last
        type(face_span_type), intent(in), optional :: spans(:)
        type(frame_type), intent(in), optional :: f
        integer, intent(out), optional :: used
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        type(glyph_type) :: g
        integer(int32), allocatable :: chars(:)
        integer(int32) :: base
        integer :: nchars, i, x, n, k, j, nglyphs
        logical :: gui
        base = MODE_LINE_ACTIVE_FACE_ID
        if (present(face_id)) base = face_id
        gui = .false.
        if (present(f)) gui = frame_window_p(f)
        row = glyph_type()
        call decode_string(text, chars, nchars)
        if (gui) then
            x = 0
            nglyphs = 0
            do i = 1, nchars
                if (x >= width) exit
                cells = glyph_type()
                call produce_char(chars(i), x, cells, n, gui=.true.)
                cells(1:n)%face_id = span_face_of(i)
                ! (Each element of a display vector is a display element.)
                do j = 1, n
                    if (x >= width .or. nglyphs >= size(row)) exit
                    call gui_produce_glyph(f, cells(j), x)
                    nglyphs = nglyphs + 1
                    row(nglyphs) = cells(j)
                    x = x + cells(j)%pixel_width
                end do
            end do
            do while (x < width .and. nglyphs < size(row))
                g = glyph_type(face_id=base)
                call gui_produce_glyph(f, g, x)
                nglyphs = nglyphs + 1
                row(nglyphs) = g
                x = x + g%pixel_width
            end do
            if (present(used)) used = nglyphs
            return
        end if
        if (present(used)) used = width
        row(1:width)%face_id = base
        x = 0
        do i = 1, nchars
            if (x >= width) exit
            call produce_char(chars(i), x, cells, n)
            cells(1:n)%face_id = base
            if (present(span_first) .and. present(span_last) .and. present(span_face)) then
                if (i >= span_first .and. i <= span_last) cells(1:n)%face_id = span_face
            end if
            if (present(spans)) then
                do k = 1, size(spans)
                    if (i >= spans(k)%first .and. i <= spans(k)%last) cells(1:n)%face_id = spans(k)%face_id
                end do
            end if
            if (chars(i) == 9 .or. n == 1 .or. (n == 2 .and. cells(min(n, 2))%padding)) then
                ! One display element.
                if (x + n > width) then
                    row(x + 1:width) = cells(1:width - x)
                    row(width) = glyph_type(face_id=base)
                    row(width)%ch = iachar("-")
                    x = width
                    exit
                end if
                row(x + 1:x + n) = cells(1:n)
            else
                ! ^X and \ooo are display vectors: one element a cell.
                n = min(n, width - x)
                row(x + 1:x + n) = cells(1:n)
            end if
            x = x + n
        end do
    contains

        !> The face of character `i' of the text.
        integer(int32) function span_face_of(i) result(id)
            integer, intent(in) :: i
            integer :: m
            id = base
            if (present(span_first) .and. present(span_last) .and. present(span_face)) then
                if (i >= span_first .and. i <= span_last) id = span_face
            end if
            if (present(spans)) then
                do m = 1, size(spans)
                    if (i >= spans(m)%first .and. i <= spans(m)%last) id = spans(m)%face_id
                end do
            end if
        end function span_face_of
    end subroutine mode_line_row

    !> Show LIGHTER in the mode line while VARIABLE is non-nil (replacing
    !> VARIABLE's earlier lighter), in FACE if present (a propertized
    !> lighter).  ACTIVE, if present, tells whether VARIABLE is true (a
    !> variable kept in Fortran).  Port of the mode-line part of
    !> subr.el:add-minor-mode (minor-mode-alist).
    subroutine add_minor_mode_lighter(variable, lighter, face, active)
        character(len=*), intent(in) :: variable, lighter
        character(len=*), intent(in), optional :: face
        procedure(lighter_active_proc), optional :: active
        type(lighter_type), allocatable :: grown(:)
        integer :: i, k
        k = 0
        do i = 1, n_lighters
            if (lighters(i)%variable == variable) then
                k = i
                exit
            end if
        end do
        if (k == 0) then
            if (.not. allocated(lighters)) allocate (lighters(8))
            if (n_lighters == size(lighters)) then
                allocate (grown(2*n_lighters))
                grown(1:n_lighters) = lighters
                call move_alloc(grown, lighters)
            end if
            n_lighters = n_lighters + 1
            k = n_lighters
            lighters(k)%variable = variable
        end if
        lighters(k)%lighter = lighter
        lighters(k)%face = ""
        if (present(face)) lighters(k)%face = face
        lighters(k)%active => null()
        if (present(active)) lighters(k)%active => active
    end subroutine add_minor_mode_lighter

    !> The entry of buffer slot BI in `process_entries' (0 for none).
    integer function process_entry_of(bi) result(k)
        integer(int64), intent(in) :: bi
        if (allocated(process_entries) .and. bi > 0) then
            do k = 1, size(process_entries)
                if (process_entries(k)%serial == buffers(bi)%serial) return
            end do
        end if
        k = 0
    end function process_entry_of

    !> Give the current buffer the `mode_line_process' PARTS (setq
    !> mode-line-process, a buffer-local variable).
    !>
    !> Mode line construct for displaying info on process status.
    !> Normally unallocated in most modes, since there is no process to
    !> display.  Port of bindings.el mode-line-process.
    subroutine set_mode_line_process(parts)
        type(mode_line_part_type), intent(in) :: parts(:)
        type(mode_line_process_entry_type), allocatable :: grown(:)
        integer :: k, n
        if (cur == 0) return
        if (.not. allocated(process_entries)) allocate (process_entries(0))
        call add_local_variables_killer(clear_mode_line_process)
        k = process_entry_of(cur)
        if (k == 0) then
            n = size(process_entries)
            allocate (grown(n + 1))
            do k = 1, n
                call move_alloc(process_entries(k)%parts, grown(k)%parts)
                grown(k)%serial = process_entries(k)%serial
            end do
            call move_alloc(grown, process_entries)
            k = n + 1
            process_entries(k)%serial = buffers(cur)%serial
        end if
        process_entries(k)%parts = parts
    end subroutine set_mode_line_process

    !> Make the current buffer's `mode_line_process' empty (nil): when its
    !> local variables are killed, as kill_all_local_variables does.
    subroutine clear_mode_line_process()
        integer :: k
        if (cur == 0) return
        k = process_entry_of(cur)
        if (k == 0) return
        deallocate (process_entries(k)%parts)
        allocate (process_entries(k)%parts(0))
    end subroutine clear_mode_line_process

    !> TEXT of a `mode_line_process' part as buffer slot BI's mode line
    !> shows it: %s is the status of its process, %% a percent sign.
    function expand_process_part(bi, text) result(s)
        integer(int64), intent(in) :: bi
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: s, status_name
        integer :: k
        s = ""
        k = 1
        do while (k <= len(text))
            if (text(k:k) == "%" .and. k < len(text)) then
                if (text(k + 1:k + 1) == "s") then
                    if (associated(mode_line_process_status)) then
                        call mode_line_process_status(bi, status_name)
                    else
                        status_name = "no process"
                    end if
                    s = s//status_name
                    k = k + 2
                    cycle
                else if (text(k + 1:k + 1) == "%") then
                    s = s//"%"
                    k = k + 2
                    cycle
                end if
            end if
            s = s//text(k:k)
            k = k + 1
        end do
    end function expand_process_part

    !> The text `mode_line_process' adds to buffer slot BI's mode line
    !> (for tests and for help).
    function mode_line_process_text(bi) result(s)
        integer(int64), intent(in) :: bi
        character(len=:), allocatable :: s
        integer :: e, i
        s = ""
        e = process_entry_of(bi)
        if (e == 0) return
        do i = 1, size(process_entries(e)%parts)
            s = s//expand_process_part(bi, process_entries(e)%parts(i)%text)
        end do
    end function mode_line_process_text

    !> Show TEXT in every mode line, before its modes, while ACTIVE tells
    !> that VARIABLE is true: the element (VARIABLE TEXT) added at the
    !> front of `mode_line_modes', as compile.el adds
    !> (compilation-in-progress "[Compiling] ").  Port of the
    !> mode-line-modes part of an `add-to-list'.
    subroutine add_mode_line_modes_entry(variable, text, active)
        character(len=*), intent(in) :: variable, text
        procedure(lighter_active_proc) :: active
        type(modes_entry_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(modes_entries)) allocate (modes_entries(0))
        do k = 1, size(modes_entries)
            if (modes_entries(k)%variable == variable) return
        end do
        n = size(modes_entries)
        allocate (grown(n + 1))
        grown(1)%variable = variable
        grown(1)%text = text
        grown(1)%active => active
        do k = 1, n
            call move_alloc(modes_entries(k)%variable, grown(k + 1)%variable)
            call move_alloc(modes_entries(k)%text, grown(k + 1)%text)
            grown(k + 1)%active => modes_entries(k)%active
        end do
        call move_alloc(grown, modes_entries)
    end subroutine add_mode_line_modes_entry

    !> The text the active entries of `mode_line_modes' added with
    !> `add_mode_line_modes_entry' show before its "(".
    function mode_line_modes_prefix() result(s)
        character(len=:), allocatable :: s
        integer :: k
        s = ""
        if (.not. allocated(modes_entries)) return
        do k = 1, size(modes_entries)
            if (modes_entries(k)%active()) s = s//modes_entries(k)%text
        end do
    end function mode_line_modes_prefix

    !> The modes part of buffer BI's mode line: `mode_name', " Narrow"
    !> if the buffer is narrowed (%n), and the lighters of
    !> `minor_mode_alist' whose variables are non-nil there.
    function mode_line_modes(bi) result(s)
        integer(int64), intent(in) :: bi
        character(len=:), allocatable :: s
        type(face_span_type), allocatable :: spans(:)
        character(len=64), allocatable :: faces(:)
        call mode_line_modes_faces(bi, s, spans, faces)
    end function mode_line_modes

    !> MODES, the modes part of buffer BI's mode line (mode_line_modes),
    !> with the characters SPANS(K)%FIRST to SPANS(K)%LAST of the
    !> lighters that have a face in face FACES(K).
    subroutine mode_line_modes_faces(bi, modes, spans, faces)
        integer(int64), intent(in) :: bi
        character(len=:), allocatable, intent(out) :: modes
        type(face_span_type), allocatable, intent(out) :: spans(:)
        character(len=64), allocatable, intent(out) :: faces(:)
        type(face_span_type), allocatable :: all_spans(:)
        character(len=64), allocatable :: all_faces(:)
        integer(int32), allocatable :: chars(:)
        type(value_type) :: v
        type(status_type) :: st
        integer :: i, n, nchars, before
        logical :: on
        type(mode_line_part_type), allocatable :: parts(:)
        integer :: e
        if (allocated(buffers(bi)%mode_name)) then
            modes = buffers(bi)%mode_name
        else
            modes = "Fundamental"
        end if
        ! mode-line-process.
        e = process_entry_of(bi)
        n = 0
        if (e > 0) then
            parts = process_entries(e)%parts
        else
            allocate (parts(0))
        end if
        allocate (all_spans(max(1, n_lighters + size(parts))), all_faces(max(1, n_lighters + size(parts))))
        do i = 1, size(parts)
            call decode_string(modes, chars, before)
            modes = modes//expand_process_part(bi, parts(i)%text)
            if (len(parts(i)%face) > 0) then
                call decode_string(modes, chars, nchars)
                n = n + 1
                all_spans(n) = face_span_type(first=before + 1, last=nchars)
                all_faces(n) = parts(i)%face
            end if
        end do
        if (buffers(bi)%begv > 1 .or. buffers(bi)%zv < buffers(bi)%text%z) modes = modes//" Narrow"
        do i = 1, n_lighters
            if (associated(lighters(i)%active)) then
                on = lighters(i)%active()
            else
                call buffer_local_value(sym(lighters(i)%variable), bi, v, st)
                if (.not. st%ok()) cycle
                on = .not. nilp(v)
            end if
            if (.not. on) cycle
            call decode_string(modes, chars, before)
            modes = modes//lighters(i)%lighter
            if (len(lighters(i)%face) > 0) then
                call decode_string(modes, chars, nchars)
                n = n + 1
                all_spans(n) = face_span_type(first=before + 1, last=nchars)
                all_faces(n) = lighters(i)%face
            end if
        end do
        spans = all_spans(1:n)
        allocate (faces(n))
        if (n > 0) faces = all_faces(1:n)
    end subroutine mode_line_modes_faces

    !> Draw the mode line of window `w' (showing buffer slot `bi' from
    !> `start', point `pt'): mode_line_text cut to the window, less the
    !> column of the border glyph of a window with one to its right.  The
    !> mode line is in the mode_line_active face if `w' is the selected
    !> window (or was when the minibuffer was entered), else in
    !> mode_line_inactive (mode-line-in-non-selected-windows t); the
    !> buffer name (mode-line-buffer-identification, "%12b") has the
    !> mode_line_buffer_id face merged in.  Port of the terminal part of
    !> xdisp.c:display_mode_line (CURRENT_MODE_LINE_ACTIVE_FACE_ID_3),
    !> with the border of dispnew.c:build_frame_matrix_from_leaf_window.
    subroutine display_mode_line(f, w, bi, start, pt, selected)
        type(frame_type), intent(inout) :: f
        type(window_type), intent(in) :: w
        integer(int64), intent(in) :: bi, start, pt
        logical, intent(in) :: selected
        character(len=:), allocatable :: text, modes
        type(glyph_type), allocatable :: row(:)
        integer(int32), allocatable :: chars(:)
        type(face_span_type), allocatable :: spans(:)
        character(len=64), allocatable :: faces(:)
        type(status_type) :: status
        type(glyph_row_type) :: line
        logical :: modified, gui
        integer :: width, nname, nmodes, ntext, k, offset, name_at, asc, desc, height
        integer(int32) :: face_id, id_face
        call buffer_modified_p(w%buffer, modified, status)
        gui = frame_window_p(f)
        width = window_text_cols(w)
        call mode_line_modes_faces(bi, modes, spans, faces)
        text = mode_line_text(buffers(bi)%name, modified, buffers(bi)%read_only, &
                              mode_line_percent(bi, start, w%window_end_valid), &
                              mode_line_line(bi, start, pt, w%total_lines), width, modes, gui, &
                              mode_line_modes_prefix())
        face_id = MODE_LINE_INACTIVE_FACE_ID
        if (selected) face_id = MODE_LINE_ACTIVE_FACE_ID
        id_face = merge_faces(f, "mode_line_buffer_id", 0_int32, face_id)
        ! The lighters with faces: the text ends with MODES, ") " and the
        ! dashes of %-.
        call decode_string(modes, chars, nmodes)
        call decode_string(text, chars, ntext)
        if (gui) then
            offset = ntext - 2 - nmodes
        else
            offset = ntext - max(0, width) - 2 - nmodes
        end if
        do k = 1, size(spans)
            spans(k)%first = spans(k)%first + offset
            spans(k)%last = spans(k)%last + offset
            spans(k)%face_id = merge_faces(f, trim(faces(k)), 0_int32, face_id)
        end do
        ! The buffer name, padded to 12, after "-UUU:**- F1  " (" UUU:**-  "
        ! on a window-system frame).
        call decode_string(buffers(bi)%name, chars, nname)
        if (gui) then
            ! A row spanning the whole window, the mode line's height off
            ! its bottom (compute_line_metrics).
            name_at = 11
            width = window_pixel_width(f, w)
            allocate (line%glyphs(width/max(1, f%smallest_char_width) + 4))
            call mode_line_row(text, width, line%glyphs, face_id, name_at, name_at - 1 + max(12, nname), id_face, &
                               spans, f, line%used)
            asc = 0
            desc = 0
            line%pixel_width = 0
            do k = 1, line%used
                asc = max(asc, line%glyphs(k)%ascent)
                desc = max(desc, line%glyphs(k)%descent)
                line%pixel_width = line%pixel_width + line%glyphs(k)%pixel_width
            end do
            if (asc + desc == 0) desc = f%line_height
            line%ascent = asc
            line%height = asc + desc
            line%enabled_p = .true.
            height = window_pixel_height(f, w)
            line%y = height - estimate_mode_line_height(f, face_id)
            line%visible_height = min(line%height, height - line%y)
            call update_window_line(f, line, window_pixel_left(f, w), window_pixel_top(f, w), &
                                    window_pixel_left(f, w) + width)
            return
        end if
        allocate (row(w%total_cols))
        call mode_line_row(text, width, row, face_id, 14, 13 + max(12, nname), id_face, spans)
        if (width < w%total_cols) then
            row(width + 1:) = glyph_type(face_id=VERTICAL_BORDER_FACE_ID)
            row(w%total_cols)%ch = iachar("|")
        end if
        associate (m => f%desired_matrix%rows(w%top_line + w%total_lines - 1))
            m%glyphs(w%left_col + 1:w%left_col + w%total_cols) = row
        end associate
    end subroutine display_mode_line

    !> Where and how large the cursor of window `w' on window-system frame
    !> `f' is drawn: on glyph `hpos' of `row' at pixel `x' of the text
    !> area (less wide on a stretch glyph, x-stretch-cursor nil), as high
    !> as the glyph within the row, kept above `text_bottom' (the bottom of
    !> the text area).  Port of xdisp.c:get_phys_cursor_geometry (no tab or
    !> header line), stored as w->phys_cursor.
    subroutine get_phys_cursor_geometry(f, w, row, x, hpos, text_bottom)
        type(frame_type), intent(in) :: f
        type(window_type), intent(inout) :: w
        type(glyph_row_type), intent(in) :: row
        integer, intent(in) :: x, hpos, text_bottom
        type(glyph_type) :: g
        integer :: wd, cx, y, h, h0, y0, ascent
        if (hpos >= 1 .and. hpos <= row%used) then
            g = row%glyphs(hpos)
        else
            g = glyph_type(face_id=DEFAULT_FACE_ID)
            call gui_produce_glyph(f, g, x)
        end if
        wd = g%pixel_width
        cx = x
        if (cx < 0) then
            wd = wd + cx
            cx = 0
        end if
        if (g%type == STRETCH_GLYPH) wd = min(f%column_width, wd)
        y = row%y
        ascent = row%ascent
        if (.not. row%ends_at_zv_p .and. row%ascent < g%ascent) then
            y = y - (g%ascent - row%ascent)
            ascent = g%ascent
        end if
        ! If y is below window bottom, ensure that we still see a cursor.
        h0 = min(f%line_height, row%visible_height)
        h = max(h0, ascent + g%descent)
        h = min(h, row%height)
        h0 = min(h0, ascent + g%descent)
        y0 = 0
        if (y < y0) then
            h = max(h - (y0 - y) + 1, h0)
            y = y0 - 1
        else
            y0 = text_bottom - h0
            if (y > y0) then
                h = h + y - y0
                y = y0
            end if
        end if
        w%phys_cursor_x = window_pixel_left(f, w) + cx
        w%phys_cursor_y = window_pixel_top(f, w) + y
        w%phys_cursor_width = wd
        w%phys_cursor_height = h
    end subroutine get_phys_cursor_geometry

    !> Draw the borders of window `w' on window-system frame `f' with the
    !> windows beside it, over its last pixel column (and its left
    !> neighbor's), down the text area (`text_bottom' pixels high).  Port
    !> of xdisp.c:gui_draw_vertical_border (no scroll bars, fringes or
    !> dividers).
    subroutine gui_draw_vertical_border(f, w, text_bottom)
        type(frame_type), intent(inout) :: f
        type(window_type), intent(in) :: w
        integer, intent(in) :: text_bottom
        integer :: x0, x1, y0, y1
        ! window_box_edges of the text area.
        x0 = window_pixel_left(f, w)
        x1 = x0 + window_body_pixel_width(f, w)
        y0 = window_pixel_top(f, w)
        y1 = y0 + text_bottom - 1
        if (w%divider) call f%rif%draw_vertical_window_border(x1 - 1, y0, y1, VERTICAL_BORDER_FACE_ID)
        if (w%left_col > 0) call f%rif%draw_vertical_window_border(x0 - 1, y0, y1, VERTICAL_BORDER_FACE_ID)
    end subroutine gui_draw_vertical_border

    !> Show the last message in the minibuffer window, or blank it: its
    !> first screen line, a line that does not fit continued with \ in the
    !> last column.  Port of the display part of xdisp.c:echo_area_display
    !> (one line: resize-mini-windows is not ported).
    subroutine display_echo_area(f, endcol)
        type(frame_type), intent(inout) :: f
        !> Column after the text.
        integer, intent(out) :: endcol
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        integer(int32), allocatable :: chars(:)
        integer :: nchars, i, x, n, k, cols, last_x
        if (frame_window_p(f)) then
            call display_echo_area_gui(f, endcol)
            return
        end if
        cols = windows(f%minibuffer_window%index)%total_cols
        last_x = cols - 1
        endcol = 0
        associate (m => f%desired_matrix%rows(windows(f%minibuffer_window%index)%top_line))
            m%glyphs = glyph_type()
            m%used = 0
            m%enabled_p = .true.
            if (.not. allocated(last_message)) return
            call decode_string(last_message, chars, nchars)
            x = 0
            do i = 1, nchars
                if (chars(i) == 10) exit
                cells = glyph_type()
                call produce_char(chars(i), x, cells, n)
                if (x + n > last_x .and. x > 0) then
                    if (n == 2 .and. cells(min(n, 2))%padding) then
                        m%glyphs(x + 1:cols)%ch = iachar("\")
                    else
                        k = last_x - x
                        if (k > 0) m%glyphs(x + 1:x + k) = cells(1:k)
                        m%glyphs(cols)%ch = iachar("\")
                    end if
                    x = cols
                    exit
                end if
                k = min(n, cols - x)
                m%glyphs(x + 1:x + k) = cells(1:k)
                x = x + n
            end do
            m%used = min(x, cols)
            endcol = m%used
        end associate
    end subroutine display_echo_area

    !> display_echo_area on window-system frame `f': the first line of the
    !> message in the default face, glyphs as wide as the font makes them,
    !> the continuation glyph after the last that fits, a space for the
    !> cursor at the end; `endcol' is the x after the text.  Port of the
    !> display part of xdisp.c:echo_area_display with display_line.
    subroutine display_echo_area_gui(f, endcol)
        type(frame_type), intent(inout) :: f
        integer, intent(out) :: endcol
        type(glyph_type) :: cells(MAX_CHAR_CELLS)
        type(glyph_type) :: g
        type(glyph_row_type) :: line
        integer(int32), allocatable :: chars(:)
        integer :: nchars, i, j, x, n, width, last_x, asc, desc, left, top
        logical :: continued
        associate (mw => windows(f%minibuffer_window%index))
            width = window_pixel_width(f, mw)
            left = window_pixel_left(f, mw)
            top = window_pixel_top(f, mw)
        end associate
        last_x = width - continuation_pixel_width(f)
        allocate (line%glyphs(width/max(1, f%smallest_char_width) + 4))
        line%used = 0
        x = 0
        asc = 0
        desc = 0
        continued = .false.
        if (allocated(last_message)) then
            call decode_string(last_message, chars, nchars)
            do i = 1, nchars
                if (chars(i) == 10) exit
                cells = glyph_type()
                call produce_char(chars(i), x, cells, n, gui=.true.)
                do j = 1, n
                    call gui_produce_glyph(f, cells(j), x)
                    if (x + cells(j)%pixel_width > last_x .and. line%used > 0) then
                        continued = .true.
                        exit
                    end if
                    call put(cells(j))
                    x = x + cells(j)%pixel_width
                end do
                if (continued) exit
            end do
        end if
        endcol = x
        if (continued) then
            call put(continuation_glyph(f))
        else if (line%used < size(line%glyphs)) then
            ! append_space_for_newline at the end of the text.
            g = glyph_type(face_id=DEFAULT_FACE_ID)
            call gui_produce_glyph(f, g, x)
            if (line%used == 0) call normal_char_ascent_descent(f, DEFAULT_FACE_ID, asc, desc)
            line%used = line%used + 1
            line%glyphs(line%used) = g
        end if
        if (asc + desc == 0) desc = f%line_height
        line%ascent = asc
        line%height = asc + desc
        line%visible_height = line%height
        line%enabled_p = .true.
        call update_window_line(f, line, left, top, left + width)
    contains

        !> Append glyph `g', its height counted.
        subroutine put(g)
            type(glyph_type), intent(in) :: g
            if (line%used >= size(line%glyphs)) return
            line%used = line%used + 1
            line%glyphs(line%used) = g
            asc = max(asc, g%ascent)
            desc = max(desc, g%descent)
        end subroutine put
    end subroutine display_echo_area_gui

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
        integer(int64) :: mpos, stop, fstop
        integer(int32) :: clen, face_id
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
        face_id = DEFAULT_FACE_ID
        stop = -1
        do while (pos < buffers(bi)%zv)
            if (pos == buffers(bi)%pt) col_of_point = x
            ! The faces of the text (the prompt's minibuffer_prompt).
            if (pos >= stop) then
                call handle_fontified_prop(bi, pos, pos + TEXT_PROP_DISTANCE_LIMIT, fstop)
                face_id = face_at_buffer_position(f, bi, pos, stop, pos + TEXT_PROP_DISTANCE_LIMIT)
                stop = min(stop, fstop)
            end if
            c = buf_fetch_char(buffers(bi), byte)
            call put(c)
            byte = byte + buf_next_char_len(buffers(bi), byte)
            pos = pos + 1
        end do
        if (col_of_point < 0) col_of_point = x
        face_id = DEFAULT_FACE_ID
        ! The messages of minibuffer-message and set-minibuffer-message
        ! after the text (their overlays at the end).
        if (allocated(minibuffer_message_text)) then
            mpos = 1
            do while (mpos <= len(minibuffer_message_text, kind=int64))
                call string_char_and_length(minibuffer_message_text, mpos, c, clen)
                call put(c)
                mpos = mpos + clen
            end do
        end if
        if (allocated(set_minibuffer_message_text)) then
            mpos = 1
            do while (mpos <= len(set_minibuffer_message_text, kind=int64))
                call string_char_and_length(set_minibuffer_message_text, mpos, c, clen)
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
            cells(1:n)%face_id = char_face(f, c, face_id)
            if (x + n > size(line)) then
                allocate (grown(2*size(line) + n))
                grown(1:x) = line(1:x)
                call move_alloc(grown, line)
            end if
            line(x + 1:x + n) = cells(1:n)
            x = x + n
        end subroutine put
    end subroutine display_minibuffer

    !> Show `text' in the echo area; an empty `text' clears it.  While the
    !> minibuffer is active the text goes after the minibuffer's contents
    !> instead, enclosed in [...] unless it already is, until the next
    !> input event.  Port of editfns.c:Fmessage (with a plain string; no
    !> *Messages* log yet) with xdisp.c:set_message and clear_message
    !> calling minibuffer.el's set-minibuffer-message and
    !> clear-minibuffer-message (set-message-functions,
    !> clear-message-function).
    subroutine message(text)
        character(len=*), intent(in) :: text
        if (len(text) == 0) then
            if (allocated(set_minibuffer_message_text)) deallocate (set_minibuffer_message_text)
            last_message = ""
        else if (minibuffer_active_p()) then
            if (bracketed_message_p(text)) then
                set_minibuffer_message_text = text
            else
                set_minibuffer_message_text = " ["//text//"]"
            end if
        else
            last_message = text
        end if
    end subroutine message

    !> Whether `text' matches "\` *\[.+\]\'".
    pure logical function bracketed_message_p(text) result(p)
        character(len=*), intent(in) :: text
        integer :: k
        p = .false.
        k = verify(text, " ")
        if (k == 0 .or. len(text) - k < 2) return
        if (text(k:k) /= "[" .or. text(len(text):len(text)) /= "]") return
        p = index(text(k + 1:len(text) - 1), achar(10)) == 0
    end function bracketed_message_p

    !> Whether the selected frame's minibuffer is active (the minibuffer
    !> window shows a minibuffer): window.c:Factive_minibuffer_window.
    logical function minibuffer_active_p() result(p)
        integer(int64) :: bi
        type(status_type) :: status
        p = .false.
        if (.not. window_valid_p(selected_frame%minibuffer_window)) return
        call resolve_buffer(windows(selected_frame%minibuffer_window%index)%buffer, bi, status)
        p = status%ok()
    end function minibuffer_active_p

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
        logical :: showing_message, mini_active, selected, gui
        gui = frame_window_p(f)
        if (.not. f%glyphs_initialized_p) call adjust_frame_glyphs(f)
        if (.not. window_valid_p(f%root_window)) call init_frame_windows(f)
        if (windows(f%root_window%index)%total_lines == 0) call init_frame_windows(f)
        if (.not. allocated(f%faces)) call realize_basic_faces(f)
        if (gui) then
            ! clear_garbaged_frames; a window-system frame is drawn as it
            ! is laid out.
            if (f%garbaged) call redraw_frame(f)
        else
            do r = 0, windows(f%minibuffer_window%index)%top_line - 1
                call blank_row(f, r)
            end do
        end if
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
                ! (On a window-system frame: Deferred.)
                if (.not. gui) call display_minibuffer(f, mini_col)
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
                ! display_and_set_cursor: the selected window's cursor (in
                ! the echo area or minibuffer: Deferred).
                if (gui) call f%rif%draw_window_cursor(sw%phys_cursor_x, sw%phys_cursor_y, sw%phys_cursor_width, &
                                                       sw%phys_cursor_height, FILLED_BOX_CURSOR, .true., .true.)
            end if
        end associate
        call update_frame(f, status)
    end subroutine redisplay
end module xdisp_fm
