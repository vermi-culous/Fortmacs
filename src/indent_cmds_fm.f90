! indent_cmds_fm.f90 -- indentation commands, a submodule of indent_fm.
! Ported from GNU Emacs 31.1 lisp/indent.el.
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

!> indent.el: `indent_for_tab_command' (TAB), `indent_according_to_mode',
!> `indent_rigidly' (C-x TAB, with its transient map), `indent_region'
!> (C-M-\), `indent_relative', `tab_to_tab_stop' (M-i), `indent_line_to',
!> the left margin and `beginning_of_line_text'.  A submodule because
!> these use simple_fm and fill_fm, which use indent_fm.
!>
!> Not ported: `tab_always_indent' `complete' and `tab_first_completion',
!> `indent_for_tab_command' with a prefix argument (needs sexps),
!> `edit_tab_stops', right-to-left paragraphs, the margin text-property
!> commands (`set_left_margin' and friends).
submodule(indent_fm) indent_cmds_fm
    use commands_fm, only: commands
    use error_fm, only: set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: nil, nilp, fixnump, xfixnum, make_keymap_ref
    use lread_fm, only: sym
    use data_fm, only: value_eq, default_value
    use lisp_fm, only: value_type, symbolp, xsymbol, symbols, t_value
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use editfns_fm, only: point_min, point_max, bobp, eobp, bolp, eolp, delete_region, buffer_substring, &
                          char_before, widen, narrow_to_region, region_beginning, region_end, line_end_position, &
                          preceding_char, following_char
    use insdel_fm, only: insert
    use cmds_fm, only: forward_line, beginning_of_line
    use textprop_fm, only: get_text_property
    use syntax_fm, only: skip_chars_forward, skip_chars_backward, char_syntax
    use abbrev_fm, only: abbrev_mode_p, expand_abbrev
    use search_fm, only: looking_at, re_search_backward, re_search_forward, regexp_quote, SEARCH_NOERROR
    use keymap_fm, only: make_sparse_keymap, keymap_set_key => define_key, key_parse
    use keyboard_fm, only: this_command, last_command, set_transient_map
    use callint_fm, only: current_prefix_arg, prefix_numeric_value, funcall
    use simple_fm, only: fill_prefix, use_region_p, delete_horizontal_space, back_to_indentation, deactivate_mark
    use keymap_fm, only: overriding_terminal_local_map
    use buffer_fm, only: UE_BOUNDARY
    use buffer_fm, only: barf_if_buffer_read_only, vdeactivate_mark, left_margin
    use minibuf_fm, only: minibuffer_prompt_end, minibuf_level
    use fill_fm, only: adaptive_fill_mode, adaptive_fill_regexp, current_justification, JUSTIFY_CENTER, &
                       JUSTIFY_RIGHT, prefix_at_point, nchars
    use xdisp_fm, only: message, tab_width
    use subr_fm, only: read_number, progress_reporter_type, make_progress_reporter, progress_reporter_update, &
                       progress_reporter_done
    implicit none(type, external)

    !> Transient keymap for adjusting indentation interactively.
    !> It is activated by calling `indent_rigidly' interactively.  Port of
    !> indent.el indent-rigidly-map.
    type(handle_type) :: indent_rigidly_map

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

contains

    ! The default indent-line-function, `indent-relative`.
    subroutine indent_relative_line(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent
        call indent_relative(status, noindent=noindent)
    end subroutine indent_relative_line

    ! The current indent-line-function.
    subroutine call_indent_line_function(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent
        if (associated(indent_line_function%value)) then
            call indent_line_function%value(status, noindent)
        else
            call indent_relative_line(status, noindent)
        end if
    end subroutine call_indent_line_function

    module procedure indent_line_function_name
    procedure(indent_line_proc), pointer :: q
    name = ""
    if (.not. associated(p)) then
        name = "indent_relative"
        return
    end if
    q => indent_relative_line
    if (associated(p, q)) name = "indent_relative"
    q => indent_relative_first_indent_point
    if (associated(p, q)) name = "indent_relative_first_indent_point"
    q => indent_to_left_margin
    if (associated(p, q)) name = "indent_to_left_margin"
    end procedure indent_line_function_name

    module procedure indent_line_function_named
    select case (name)
    case ("indent_relative")
        p => indent_relative_line
    case ("indent_relative_first_indent_point")
        p => indent_relative_first_indent_point
    case ("indent_to_left_margin")
        p => indent_to_left_margin
    case default
        p => indent_line_by_name
    end select
    end procedure indent_line_function_named

    ! An indent-line-function that is not one of the above: call the
    ! function named by the variable's value.
    recursive subroutine indent_line_by_name(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent
        noindent = .false.
        call funcall(indent_line_function%exact, status)
    end subroutine indent_line_by_name

    ! Whether the indent-line-function is one of
    ! `indent-line-ignored-functions` (the indent-relative family).
    logical function ignored_indent_line_function()
        procedure(indent_line_proc), pointer :: p
        ignored_indent_line_function = .not. associated(indent_line_function%value)
        if (ignored_indent_line_function) return
        p => indent_relative_line
        ignored_indent_line_function = associated(indent_line_function%value, p)
        if (ignored_indent_line_function) return
        p => indent_relative_first_indent_point
        ignored_indent_line_function = associated(indent_line_function%value, p)
    end function ignored_indent_line_function

    !> Indent line in proper way for current major mode.
    !> Normally, this is done by calling the function specified by the
    !> variable `indent_line_function'.  However, if the value of that
    !> variable is present in the `indent_line_ignored_functions' variable,
    !> handle it specially (since those functions are used for tabbing);
    !> in that case, indent by aligning to the previous non-blank line.
    !>
    !> Ignore restriction, unless the optional argument INHIBIT_WIDEN is
    !> true.  Port of indent.el:indent-according-to-mode.
    module procedure indent_according_to_mode
    integer(int64) :: savedb, savedz, column, opoint, shortage
    type(status_type) :: st
    logical :: widened, noindent
    status = status_type()
    savedb = buffers(cur)%begv
    savedz = buffers(cur)%text%z - buffers(cur)%zv
    widened = .true.
    if (present(inhibit_widen)) widened = .not. inhibit_widen
    if (widened) call widen()
    if (ignored_indent_line_function()) then
        ! These functions are used for tabbing, but can't be used for
        ! indenting.  Replace with something ad-hoc.
        opoint = point()
        call beginning_of_line()
        if (bobp()) then
            column = 0
        else
            call beginning_of_line(0_int64)
            if (looking_at("[ "//TAB//"]*$", st)) then
                column = 0
            else
                column = current_indentation()
            end if
        end if
        call goto_char(opoint)
        if (current_column() <= current_indentation()) then
            call indent_line_to(column, status)
        else
            call save_excursion_indent_line_to(column, status)
        end if
    else
        ! The normal case.
        call indent_line_function%value(status, noindent)
    end if
    if (widened) then
        call widen()
        call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
    end if
    end procedure indent_according_to_mode

    ! (save-excursion (indent-line-to COLUMN)), point kept with a marker.
    subroutine save_excursion_indent_line_to(column, status)
        integer(int64), intent(in) :: column
        type(status_type), intent(out) :: status
        type(handle_type) :: m
        type(status_type) :: st
        integer(int64) :: pos
        call copy_marker_at(point(), m, .false., st)
        call indent_line_to(column, status)
        call marker_position(m, pos, st)
        call goto_char(pos)
        call free_marker(m, st)
    end subroutine save_excursion_indent_line_to

    !> Indent the current line or region, or insert a tab, as appropriate.
    !> This function either inserts a tab, or indents the current line,
    !> or performs symbol completion, depending on `tab_always_indent'.
    !> The function called to actually indent the line or insert a tab
    !> is given by the variable `indent_line_function'.
    !>
    !> In most major modes, if point was in the current line's
    !> indentation, it is moved to the first non-whitespace character
    !> after indenting; otherwise it stays at the same position relative
    !> to the text.
    !>
    !> If `transient_mark_mode' is turned on and the region is active,
    !> this function instead calls `indent_region'.  In this case, any
    !> prefix argument is ignored.  Port of
    !> indent.el:indent-for-tab-command (no prefix-argument sexp
    !> reindentation, no completion).
    module procedure indent_for_tab_command
    integer(int64) :: b, e
    logical :: noindent, to_left_margin
    procedure(indent_line_proc), pointer :: p
    status = status_type()
    to_left_margin = .false.
    if (associated(indent_line_function%value)) then
        p => indent_to_left_margin
        to_left_margin = associated(indent_line_function%value, p)
    end if
    if (use_region_p()) then
        ! The region is active, indent it.
        call region_beginning(b, status)
        call region_end(e, status)
        call indent_region(b, e, status)
    else if (to_left_margin .or. (.not. tab_always_indent .and. &
                                  (current_column() > current_indentation() .or. &
                                   value_eq(this_command, last_command)))) then
        ! indent-to-left-margin is only meant for indenting, so we force it
        ! to always insert a tab here.
        call insert_tab(arg, status)
    else
        ! Indent the line.  When the indent-line-function returns
        ! `noindent', fall back on the default one
        ! (indent--default-inside-comment is nil: syntax-ppss is not
        ! ported; tab_always_indent is never `complete').
        call widened_indent_line(status, noindent)
        if (status%ok() .and. noindent) call widened_default_indent_line(status)
    end if
    end procedure indent_for_tab_command

    ! (indent--funcall-widened (default-value 'indent-line-function)).  A
    ! default that is a procedure without a name is called through the
    ! current value.
    subroutine widened_default_indent_line(status)
        type(status_type), intent(out) :: status
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: savedb, savedz
        logical :: noindent
        savedb = buffers(cur)%begv
        savedz = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        call default_value(sym("indent_line_function"), v, st)
        status = status_type()
        if (.not. st%ok() .or. .not. symbolp(v) .or. value_eq(v, t_value())) then
            call call_indent_line_function(status, noindent)
        else
            select case (symbols(xsymbol(v))%name)
            case ("indent_relative")
                call indent_relative_line(status, noindent)
            case ("indent_relative_first_indent_point")
                call indent_relative_first_indent_point(status, noindent)
            case ("indent_to_left_margin")
                call indent_to_left_margin(status, noindent)
            case default
                call funcall(v, status)
            end select
        end if
        call widen()
        call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
    end subroutine widened_default_indent_line

    ! indent--funcall-widened with the indent-line-function.
    subroutine widened_indent_line(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent
        integer(int64) :: savedb, savedz
        type(status_type) :: st
        savedb = buffers(cur)%begv
        savedz = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        call call_indent_line_function(status, noindent)
        call widen()
        call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
    end subroutine widened_indent_line

    !> Insert a tab, or indent to the next tab column, ARG times.  Port of
    !> indent.el:insert-tab.
    module procedure insert_tab
    integer(int64) :: count, col
    count = prefix_numeric_value(arg)
    status = status_type()
    if (abbrev_mode_p()) then
        if (char_syntax(int(preceding_char())) == "w") call expand_abbrev(status)
        if (.not. status%ok()) return
    end if
    if (indent_tabs_mode%value) then
        call insert_char_n(9, count, status)
    else
        col = indent_to(sane_tab_width()*(count + current_column()/sane_tab_width()), status)
    end if
    end procedure insert_tab

    !> Return the smallest indentation in range from BEG to END.
    !> Blank lines are ignored.  Port of
    !> indent.el:indent-rigidly--current-indentation (-1 for nil).
    integer(int64) function rigidly_current_indentation(beg, end) result(indent)
        integer(int64), intent(in) :: beg, end
        integer(int64) :: opoint, pos
        type(status_type) :: st
        opoint = point()
        call goto_char(beg)
        call goto_char(pos_bol())
        indent = -1
        do
            call re_search_forward("^\s-*[[:print:]]", pos, st, bound=end, noerror=SEARCH_NOERROR)
            if (pos == 0) exit
            if (indent < 0) then
                indent = current_indentation()
            else
                indent = min(indent, current_indentation())
            end if
        end do
        call goto_char(opoint)
    end function rigidly_current_indentation

    !> Indent all lines starting in the region.
    !> If called interactively with no prefix argument, activate a
    !> transient mode in which the indentation can be adjusted interactively
    !> by typing <left>, <right>, S-<left>, or S-<right>.
    !> In addition, TAB is also bound (and calls `indent_rigidly_right').
    !>
    !> Typing any other key exits this mode, and this key is then
    !> acted upon as normally.  If `transient_mark_mode' is enabled,
    !> exiting also deactivates the mark.
    !>
    !> If called from a program, or interactively with prefix ARG,
    !> indent all lines starting in the region forward by ARG columns.
    !> If called from a program, START and END specify the beginning and
    !> end of the text to act on, in place of the region.
    !>
    !> Negative values of ARG indent backward, so you can remove all
    !> indentation by specifying a large negative ARG.  Port of
    !> indent.el:indent-rigidly (the program path; the command is
    !> cmd_indent_rigidly).
    module procedure indent_rigidly
    type(handle_type) :: end_marker, opoint_marker
    type(status_type) :: st
    integer(int64) :: opoint, endpos, indent, shortage, moved, from, col
    logical :: eol_flag
    status = status_type()
    ! (save-excursion ...)
    call copy_marker_at(point(), opoint_marker, .false., st)
    call copy_marker_at(end, end_marker, .false., st)
    call goto_char(start)
    if (.not. bolp()) shortage = forward_line(1_int64)
    do
        call marker_position(end_marker, endpos, st)
        if (point() >= endpos) exit
        indent = current_indentation()
        from = point()
        moved = skip_chars_forward(" "//TAB, st)
        eol_flag = eolp()
        call goto_char(from)
        if (.not. eol_flag) col = indent_to(max(0_int64, indent + arg), status, 0_int64)
        if (.not. status%ok()) exit
        from = point()
        moved = skip_chars_forward(" "//TAB, st)
        call delete_region(from, point(), status)
        if (.not. status%ok()) exit
        shortage = forward_line(1_int64)
    end do
    call free_marker(end_marker, st)
    call marker_position(opoint_marker, opoint, st)
    call goto_char(opoint)
    call free_marker(opoint_marker, st)
    ! Keep the active region in transient mode.
    if (overriding_terminal_local_map%index == indent_rigidly_map%index .and. &
        overriding_terminal_local_map%generation == indent_rigidly_map%generation) vdeactivate_mark = .false.
    end procedure indent_rigidly

    !> Indent all lines between BEG and END leftward by one space.  Port of
    !> indent.el:indent-rigidly-left.
    module procedure indent_rigidly_left
    call indent_rigidly(beg, end, -1_int64, status)
    end procedure indent_rigidly_left

    !> Indent all lines between BEG and END rightward by one space.  Port
    !> of indent.el:indent-rigidly-right.
    module procedure indent_rigidly_right
    call indent_rigidly(beg, end, 1_int64, status)
    end procedure indent_rigidly_right

    !> Indent all lines between BEG and END leftward to a tab stop.  Port
    !> of indent.el:indent-rigidly-left-to-tab-stop.
    module procedure indent_rigidly_left_to_tab_stop
    integer(int64) :: current
    current = max(0_int64, rigidly_current_indentation(beg, end))
    call indent_rigidly(beg, end, indent_next_tab_stop(current, .true.) - current, status)
    end procedure indent_rigidly_left_to_tab_stop

    !> Indent all lines between BEG and END rightward to a tab stop.  Port
    !> of indent.el:indent-rigidly-right-to-tab-stop.
    module procedure indent_rigidly_right_to_tab_stop
    integer(int64) :: current
    current = max(0_int64, rigidly_current_indentation(beg, end))
    call indent_rigidly(beg, end, indent_next_tab_stop(current) - current, status)
    end procedure indent_rigidly_right_to_tab_stop

    !> Indent current line to COLUMN.
    !> This function removes or adds spaces and tabs at beginning of line
    !> only if necessary.  It leaves point at end of indentation.  Port of
    !> indent.el:indent-line-to.
    module procedure indent_line_to
    integer(int64) :: cur_col, moved, from, col, tw
    type(status_type) :: st
    status = status_type()
    tw = sane_tab_width()
    call beginning_of_line(1_int64)
    moved = skip_chars_forward(" "//TAB, st)
    cur_col = current_column()
    if (cur_col < column) then
        if (column - (cur_col/tw)*tw >= tw) then
            from = point()
            moved = skip_chars_backward(" ", st)
            call delete_region(point(), from, status)
            if (.not. status%ok()) return
        end if
        col = indent_to(column, status)
    else if (cur_col > column) then
        ! too far right (after tab?)
        call move_to_column(column, col, .true., status)
        if (.not. status%ok()) return
        from = point()
        ! The `move_to_column' call may replace tabs with spaces, so we
        ! can't reuse the previous start point.
        call beginning_of_line(1_int64)
        moved = skip_chars_forward(" "//TAB, st)
        call delete_region(from, point(), status)
    end if
    end procedure indent_line_to

    !> Return the left margin to use for this line.
    !> This is the value of the buffer-local variable `left_margin' plus the value
    !> of the `left_margin' text-property at the start of the line.  Port of
    !> indent.el:current-left-margin.
    module procedure current_left_margin
    integer(int64) :: opoint, pos
    type(value_type) :: v
    type(status_type) :: st
    opoint = point()
    call back_to_indentation()
    pos = point()
    if (eobp() .and. .not. bobp()) pos = pos - 1
    call get_text_property(pos, sym("left_margin"), v, st)
    current_left_margin = left_margin%value
    if (fixnump(v)) current_left_margin = left_margin%value + xfixnum(v)
    current_left_margin = max(0_int64, current_left_margin)
    call goto_char(opoint)
    end procedure current_left_margin

    !> Move to the left margin of the current line.
    !> With optional argument, move forward N-1 lines first.
    !> The column moved to is the one given by the `current_left_margin' function.
    !> If the line's indentation appears to be wrong, and this command is called
    !> interactively or with optional argument FORCE, it will be fixed.  Port
    !> of indent.el:move-to-left-margin.
    module procedure move_to_left_margin
    integer(int64) :: lm, cc, moved, col
    type(status_type) :: st
    logical :: forcing, noindent
    status = status_type()
    forcing = .false.
    if (present(force)) forcing = force
    call beginning_of_line(n)
    moved = skip_chars_forward(" "//TAB, st)
    if (minibuf_level > 0 .and. buffers(cur)%begv /= 0 .and. is_minibuffer()) then
        if (pos_bol() == point_min()) then
            call goto_char(minibuffer_prompt_end())
        else
            call beginning_of_line()
        end if
        return
    end if
    lm = current_left_margin()
    cc = current_column()
    if (cc > lm) then
        call move_to_column(lm, col, forcing, status)
        ! If lm is in a tab and we are not forcing, move before tab
        if (col > lm) call goto_char(point() - 1)
    else if (forcing .and. cc < lm) then
        call indent_to_left_margin(status, noindent)
    end if
    end procedure move_to_left_margin

    ! (minibufferp): the current buffer is a minibuffer.
    logical function is_minibuffer()
        is_minibuffer = minibuffer_prompt_end() > point_min()
    end function is_minibuffer

    !> Indent current line to the column given by `current_left_margin'.
    !> Port of indent.el:indent-to-left-margin.
    module procedure indent_to_left_margin
    integer(int64) :: moved, opoint
    type(status_type) :: st
    noindent = .false.
    call save_excursion_indent_line_to(current_left_margin(), status)
    ! If we are within the indentation, move past it.
    opoint = point()
    moved = skip_chars_backward(" "//TAB, st)
    if (bolp()) then
        moved = skip_chars_forward(" "//TAB, st)
    else
        call goto_char(opoint)
    end if
    end procedure indent_to_left_margin

    !> Remove left margin indentation from a region.
    !> This deletes to the column given by `current_left_margin'.
    !> In no case will it delete non-whitespace.
    !> Args FROM and TO are optional; default is the whole buffer.  Port of
    !> indent.el:delete-to-left-margin.
    module procedure delete_to_left_margin
    type(handle_type) :: to_marker
    type(status_type) :: st
    integer(int64) :: opoint, endpos, shortage, from_pos
    status = status_type()
    opoint = point()
    if (present(to)) then
        call copy_marker_at(to, to_marker, .false., st)
    else
        call copy_marker_at(point_max(), to_marker, .false., st)
    end if
    if (present(from)) then
        call goto_char(from)
    else
        call goto_char(point_min())
    end if
    if (.not. bolp()) shortage = forward_line(1_int64)
    do
        call marker_position(to_marker, endpos, st)
        if (point() >= endpos) exit
        from_pos = point()
        call move_to_left_margin(status, force=.true.)
        if (.not. status%ok()) exit
        call delete_region(from_pos, point(), status)
        if (.not. status%ok()) exit
        shortage = forward_line(1_int64)
    end do
    call free_marker(to_marker, st)
    call goto_char(min(opoint, point_max()))
    end procedure delete_to_left_margin

    !> Move to the beginning of the text on this line.
    !>
    !> With optional argument N, move forward N-1 lines first.
    !>
    !> From the beginning of the line, moves past the `left_margin'
    !> indentation, the `fill_prefix', and any indentation used for
    !> centering or right-justifying the line, but does not move past
    !> any whitespace that was explicitly inserted (such as a tab used
    !> to indent the first line of a paragraph).  Port of
    !> indent.el:beginning-of-line-text.
    module procedure beginning_of_line_text
    integer(int64) :: moved
    type(status_type) :: st
    logical :: have_prefix
    call beginning_of_line(n)
    moved = skip_chars_forward(" "//TAB, st)
    ! Skip over fill-prefix.
    have_prefix = allocated(fill_prefix%value)
    if (have_prefix) have_prefix = len(fill_prefix%value) > 0
    if (have_prefix) then
        if (prefix_at_point()) call goto_char(point() + nchars(fill_prefix%value))
    else if (adaptive_fill_mode%value .and. allocated(adaptive_fill_regexp)) then
        if (looking_at(adaptive_fill_regexp, st)) call goto_char(match_end_0())
    end if
    ! Skip centering or flushright indentation
    select case (current_justification())
    case (JUSTIFY_CENTER, JUSTIFY_RIGHT)
        moved = skip_chars_forward(" "//TAB, st)
    case default
        continue
    end select
    end procedure beginning_of_line_text

    integer(int64) function match_end_0()
        use search_fm, only: match_end
        type(status_type) :: st
        match_end_0 = match_end(0, st)
    end function match_end_0

    !> Indent each nonblank line in the region.
    !> A numeric prefix argument specifies a column: indent each line to that column.
    !>
    !> With no prefix argument, the command chooses one of these methods and
    !> indents all the lines with it:
    !>
    !>   1) If `fill_prefix' is allocated, insert `fill_prefix' at the
    !>      beginning of each line in the region that does not already begin
    !>      with it.
    !>   2) If `indent_region_function' is associated, call that function
    !>      to indent the region.
    !>   3) Indent each line via `indent_according_to_mode'.
    !>
    !> Called from a program, START and END specify the region to indent.
    !> If the third argument COLUMN is an integer, it specifies the
    !> column to indent to; if it is absent, use one of the three methods above.
    !> Port of indent.el:indent-region (no progress reporter).
    module procedure indent_region
    type(handle_type) :: end_marker, opoint_marker
    type(status_type) :: st
    integer(int64) :: opoint, endpos, shortage, moved, from, col, savedb, savedz
    character(len=:), allocatable :: regexp
    status = status_type()
    if (present(column)) then
        ! If a numeric prefix is given, indent to that column.
        ! (save-excursion ...)
        call copy_marker_at(point(), opoint_marker, .false., st)
        call copy_marker_at(end, end_marker, .false., st)
        call goto_char(start)
        if (.not. bolp()) shortage = forward_line(1_int64)
        do
            call marker_position(end_marker, endpos, st)
            if (point() >= endpos) exit
            from = point()
            moved = skip_chars_forward(" "//TAB, st)
            call delete_region(from, point(), status)
            if (.not. status%ok()) exit
            if (.not. eolp()) col = indent_to(column, status, 0_int64)
            if (.not. status%ok()) exit
            shortage = forward_line(1_int64)
        end do
        call free_marker(end_marker, st)
        call marker_position(opoint_marker, opoint, st)
        call goto_char(opoint)
        call free_marker(opoint_marker, st)
    else if (allocated(fill_prefix%value)) then
        ! If a fill-prefix is specified, use it.
        ! (save-excursion ...)
        call copy_marker_at(point(), opoint_marker, .false., st)
        call copy_marker_at(end, end_marker, .false., st)
        call goto_char(start)
        regexp = regexp_quote(fill_prefix%value)
        do
            call marker_position(end_marker, endpos, st)
            if (point() >= endpos) exit
            if (.not. looking_at(regexp, st) .and. .not. (bolp() .and. eolp())) then
                call insert(fill_prefix%value, status)
                if (.not. status%ok()) exit
            end if
            shortage = forward_line(1_int64)
        end do
        call free_marker(end_marker, st)
        call marker_position(opoint_marker, opoint, st)
        call goto_char(opoint)
        call free_marker(opoint_marker, st)
    else
        savedb = buffers(cur)%begv
        savedz = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        if (associated(indent_region_function)) then
            call indent_region_function(start, end, status)
        else
            call indent_region_line_by_line(start, end, status)
        end if
        call widen()
        call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
    end if
    ! In most cases, reindenting modifies the buffer, but it may also
    ! leave it unmodified, in which case we have to deactivate the mark by
    ! hand.
    vdeactivate_mark = .true.
    end procedure indent_region

    !> Indent each nonblank line from START to END with
    !> `indent_according_to_mode', reporting progress (not in a
    !> minibuffer).  Port of indent.el:indent-region-line-by-line.
    module procedure indent_region_line_by_line
    type(handle_type) :: end_marker, opoint_marker
    type(status_type) :: st
    type(progress_reporter_type) :: pr
    integer(int64) :: endpos, shortage, pos
    logical :: report
    status = status_type()
    call copy_marker_at(point(), opoint_marker, .false., st)
    call copy_marker_at(end, end_marker, .false., st)
    call goto_char(start)
    report = .not. is_minibuffer()
    if (report) then
        call marker_position(end_marker, endpos, st)
        call make_progress_reporter(pr, "Indenting region...", point(), endpos)
    end if
    do
        call marker_position(end_marker, endpos, st)
        if (point() >= endpos) exit
        if (.not. (bolp() .and. eolp())) then
            call indent_according_to_mode(status, .true.)
            if (.not. status%ok()) exit
        end if
        shortage = forward_line(1_int64)
        if (report) call progress_reporter_update(pr, point())
    end do
    if (report .and. status%ok()) call progress_reporter_done(pr)
    call free_marker(end_marker, st)
    call marker_position(opoint_marker, pos, st)
    call goto_char(pos)
    call free_marker(opoint_marker, st)
    end procedure indent_region_line_by_line

    !> Indent the current line like the previous nonblank line.
    !> Indent to the first indentation position in the previous nonblank
    !> line if that position is greater than the current column.
    !>
    !> See also `indent_relative'.  Port of
    !> indent.el:indent-relative-first-indent-point.
    module procedure indent_relative_first_indent_point
    call indent_relative(status, first_only=.true., noindent=noindent)
    end procedure indent_relative_first_indent_point

    !> Space out to under next indent point in previous nonblank line.
    !> An indent point is a non-whitespace character following whitespace.
    !> The following line shows the indentation points in this line.
    !>     ^         ^    ^     ^   ^           ^      ^  ^    ^
    !> If FIRST_ONLY is true, then only the first indent point is
    !> considered.
    !>
    !> If the previous nonblank line has no indent points beyond the
    !> column point starts at, then `tab_to_tab_stop' is done, if both
    !> FIRST_ONLY and UNINDENTED_OK are false, otherwise nothing is done.
    !> If there isn't a previous nonblank line and UNINDENTED_OK is false,
    !> call `tab_to_tab_stop'.
    !>
    !> See also `indent_relative_first_indent_point'.  Port of
    !> indent.el:indent-relative.
    module procedure indent_relative
    integer(int64) :: start_column, opoint, pos, end, col, moved, indent, shortage
    type(handle_type) :: opoint_marker
    type(status_type) :: st
    logical :: first, unindented, have_indent
    status = status_type()
    if (present(noindent)) noindent = .false.
    first = .false.
    if (present(first_only)) first = first_only
    unindented = .false.
    if (present(unindented_ok)) unindented = unindented_ok
    if (abbrev_mode_p()) then
        if (char_syntax(int(preceding_char())) == "w") call expand_abbrev(status)
        if (.not. status%ok()) return
    end if
    start_column = current_column()
    have_indent = .false.
    indent = 0
    opoint = point()
    call beginning_of_line()
    call re_search_backward("^[^"//LF//"]", pos, st, noerror=SEARCH_NOERROR)
    if (pos > 0) then
        shortage = forward_line(1_int64)
        end = point()
        call goto_char(pos)
        call move_to_column(start_column, col)
        ! Is start-column inside a tab on this line?
        if (current_column() > start_column) call goto_char(point() - 1)
        if (.not. looking_at("[ "//TAB//"]", st) .and. .not. first) moved = skip_chars_forward("^ "//TAB, st, end)
        moved = skip_chars_forward(" "//TAB, st, end)
        if (point() /= end) then
            indent = current_column()
            have_indent = .true.
        end if
    end if
    call goto_char(opoint)
    if (have_indent) then
        call copy_marker_at(point(), opoint_marker, .false., st)
        col = indent_to(indent, status, 0_int64)
        call marker_position(opoint_marker, pos, st)
        if (pos > point()) call goto_char(pos)
        call free_marker(opoint_marker, st)
    else if (.not. unindented) then
        call tab_to_tab_stop(status)
    end if
    end procedure indent_relative

    !> Return the next tab stop after COLUMN.
    !> If PREV is non-nil, return the previous one instead.  Port of
    !> indent.el:indent-next-tab-stop.
    module procedure indent_next_tab_stop
    integer(int64) :: step, last
    integer :: k, n
    logical :: before
    before = .false.
    if (present(prev)) before = prev
    n = 0
    if (allocated(tab_stop_list)) n = size(tab_stop_list)
    k = 1
    do while (k <= n)
        if (column < tab_stop_list(k)) exit
        k = k + 1
    end do
    if (k <= n) then
        if (.not. before) then
            indent_next_tab_stop = tab_stop_list(k)
        else if (k == 1) then
            indent_next_tab_stop = 0
        else if (column == tab_stop_list(k - 1)) then
            if (k >= 3) then
                indent_next_tab_stop = tab_stop_list(k - 2)
            else
                indent_next_tab_stop = 0
            end if
        else
            indent_next_tab_stop = tab_stop_list(k - 1)
        end if
        return
    end if
    ! We passed the end of tab-stop-list: guess a continuation.
    if (n >= 2) then
        step = tab_stop_list(n) - tab_stop_list(n - 1)
        last = tab_stop_list(n)
    else
        step = sane_tab_width()
        last = 0
        if (n == 1) last = tab_stop_list(1)
    end if
    ! Repeat the last tab's length.
    if (before) then
        if (column <= last) then
            indent_next_tab_stop = last - step
        else
            indent_next_tab_stop = last + step*((column - last - 1)/step)
        end if
    else
        indent_next_tab_stop = last + step*(1 + (column - last)/step)
    end if
    end procedure indent_next_tab_stop

    !> Get a list of tab stops before LIMIT (inclusive).  Port of
    !> indent.el:indent-accumulate-tab-stops.
    module procedure indent_accumulate_tab_stops
    integer(int64) :: tab
    integer(int64), allocatable :: grown(:)
    allocate (stops(0))
    tab = 0
    do
        tab = indent_next_tab_stop(tab)
        if (tab > limit) exit
        allocate (grown(size(stops) + 1))
        grown(1:size(stops)) = stops
        grown(size(grown)) = tab
        call move_alloc(grown, stops)
    end do
    end procedure indent_accumulate_tab_stops

    !> Insert spaces or tabs to next defined tab-stop column.
    !> The variable `tab_stop_list' is an array of columns at which there are tab stops.
    !> Whether this inserts tabs or spaces depends on `indent_tabs_mode'.
    !> Port of indent.el:tab-to-tab-stop.
    module procedure tab_to_tab_stop
    integer(int64) :: nexttab, col
    status = status_type()
    if (abbrev_mode_p()) then
        if (char_syntax(int(preceding_char())) == "w") call expand_abbrev(status)
        if (.not. status%ok()) return
    end if
    nexttab = indent_next_tab_stop(current_column())
    call delete_horizontal_space(.true., status)
    if (status%ok()) col = indent_to(nexttab, status)
    end procedure tab_to_tab_stop

    !> Move point to next defined tab-stop column.
    !> The variable `tab_stop_list' is an array of columns at which there are tab stops.
    !> Port of indent.el:move-to-tab-stop.
    module procedure move_to_tab_stop
    integer(int64) :: nexttab, before, col, tabend, pos
    type(handle_type) :: saved
    type(status_type) :: st
    nexttab = indent_next_tab_stop(current_column())
    before = point()
    call move_to_column(nexttab, col, .true., status)
    if (.not. status%ok()) return
    ! (save-excursion ...)
    call copy_marker_at(point(), saved, .false., st)
    call goto_char(before)
    ! If we just added a tab, or moved over one, delete any superfluous
    ! spaces before the old point.
    if (preceding_char() == 32 .and. following_char() == 9) then
        tabend = (current_column()/tab_width%value)*tab_width%value
        do while (current_column() > tabend)
            if (preceding_char() /= 32) exit
            call goto_char(point() - 1)
        end do
        call delete_region(point(), before, status)
    end if
    call marker_position(saved, pos, st)
    call goto_char(pos)
    call free_marker(saved, st)
    end procedure move_to_tab_stop

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    subroutine cmd_indent_for_tab_command(status)
        type(status_type), intent(out) :: status
        call indent_for_tab_command(current_prefix_arg, status)
    end subroutine cmd_indent_for_tab_command

    subroutine cmd_indent_according_to_mode(status)
        type(status_type), intent(out) :: status
        call indent_according_to_mode(status)
    end subroutine cmd_indent_according_to_mode

    ! indent-rigidly: with no prefix argument, the transient map.
    subroutine cmd_indent_rigidly(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e
        call region_beginning(b, status)
        if (.not. status%ok()) return
        call region_end(e, status)
        if (nilp(current_prefix_arg)) then
            call set_transient_map(indent_rigidly_map, .true., rigidly_exit, "Type %k to indent region interactively")
        else
            call indent_rigidly(b, e, prefix_numeric_value(current_prefix_arg), status)
        end if
    end subroutine cmd_indent_rigidly

    subroutine region_command(status, which)
        type(status_type), intent(out) :: status
        integer, intent(in) :: which
        integer(int64) :: b, e
        call region_beginning(b, status)
        if (.not. status%ok()) return
        call region_end(e, status)
        call rigidly_pop_undo()
        select case (which)
        case (1)
            call indent_rigidly_left(b, e, status)
        case (2)
            call indent_rigidly_right(b, e, status)
        case (3)
            call indent_rigidly_left_to_tab_stop(b, e, status)
        case default
            call indent_rigidly_right_to_tab_stop(b, e, status)
        end select
    end subroutine region_command

    ! The ON-EXIT of indent-rigidly's transient map: `deactivate_mark'.
    subroutine rigidly_exit()
        call deactivate_mark()
    end subroutine rigidly_exit

    ! Undo the boundary between consecutive commands of indent-rigidly's
    ! transient map, so they undo together.  Port of
    ! indent.el:indent-rigidly--pop-undo.
    subroutine rigidly_pop_undo()
        if (.not. (value_eq(last_command, sym("indent_rigidly_left")) .or. &
                   value_eq(last_command, sym("indent_rigidly_right")) .or. &
                   value_eq(last_command, sym("indent_rigidly_left_to_tab_stop")) .or. &
                   value_eq(last_command, sym("indent_rigidly_right_to_tab_stop")))) return
        associate (b => buffers(cur))
            if (.not. b%undo_enabled .or. b%undo_count == 0) return
            if (b%undo_list(b%undo_count)%kind == UE_BOUNDARY) b%undo_count = b%undo_count - 1
        end associate
    end subroutine rigidly_pop_undo

    subroutine cmd_indent_rigidly_left(status)
        type(status_type), intent(out) :: status
        call region_command(status, 1)
    end subroutine cmd_indent_rigidly_left

    subroutine cmd_indent_rigidly_right(status)
        type(status_type), intent(out) :: status
        call region_command(status, 2)
    end subroutine cmd_indent_rigidly_right

    subroutine cmd_indent_rigidly_left_to_tab_stop(status)
        type(status_type), intent(out) :: status
        call region_command(status, 3)
    end subroutine cmd_indent_rigidly_left_to_tab_stop

    subroutine cmd_indent_rigidly_right_to_tab_stop(status)
        type(status_type), intent(out) :: status
        call region_command(status, 4)
    end subroutine cmd_indent_rigidly_right_to_tab_stop

    subroutine cmd_indent_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e
        call region_beginning(b, status)
        if (.not. status%ok()) return
        call region_end(e, status)
        if (nilp(current_prefix_arg)) then
            call indent_region(b, e, status)
        else
            call indent_region(b, e, status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_indent_region

    subroutine cmd_indent_relative(status)
        type(status_type), intent(out) :: status
        call indent_relative(status, first_only=.not. nilp(current_prefix_arg))
    end subroutine cmd_indent_relative

    subroutine cmd_indent_relative_first_indent_point(status)
        type(status_type), intent(out) :: status
        logical :: noindent
        call indent_relative_first_indent_point(status, noindent)
    end subroutine cmd_indent_relative_first_indent_point

    subroutine cmd_tab_to_tab_stop(status)
        type(status_type), intent(out) :: status
        call tab_to_tab_stop(status)
    end subroutine cmd_tab_to_tab_stop

    subroutine cmd_move_to_tab_stop(status)
        type(status_type), intent(out) :: status
        call move_to_tab_stop(status)
    end subroutine cmd_move_to_tab_stop

    subroutine cmd_move_to_left_margin(status)
        type(status_type), intent(out) :: status
        call move_to_left_margin(status, prefix_numeric_value(current_prefix_arg), .true.)
    end subroutine cmd_move_to_left_margin

    subroutine cmd_beginning_of_line_text(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call beginning_of_line_text(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_beginning_of_line_text

    ! Interactive spec "NIndent to column: ": the numeric prefix argument,
    ! or else a number read in the minibuffer.
    recursive subroutine cmd_indent_to(status)
        type(status_type), intent(out) :: status
        integer(int64) :: col, column
        if (nilp(current_prefix_arg)) then
            call read_number("Indent to column: ", column, status)
            if (.not. status%ok()) return
        else
            column = prefix_numeric_value(current_prefix_arg)
        end if
        col = indent_to(column, status)
    end subroutine cmd_indent_to

    !> Register the indentation commands and make indent-rigidly-map.
    module procedure syms_of_indent
    type(value_type), allocatable :: key(:)
    type(status_type) :: st
    commands%indent_for_tab_command%run => cmd_indent_for_tab_command
    commands%indent_according_to_mode%run => cmd_indent_according_to_mode
    commands%indent_rigidly%run => cmd_indent_rigidly
    commands%indent_rigidly_left%run => cmd_indent_rigidly_left
    commands%indent_rigidly_right%run => cmd_indent_rigidly_right
    commands%indent_rigidly_left_to_tab_stop%run => cmd_indent_rigidly_left_to_tab_stop
    commands%indent_rigidly_right_to_tab_stop%run => cmd_indent_rigidly_right_to_tab_stop
    commands%indent_region%run => cmd_indent_region
    commands%indent_relative%run => cmd_indent_relative
    commands%indent_relative_first_indent_point%run => cmd_indent_relative_first_indent_point
    commands%tab_to_tab_stop%run => cmd_tab_to_tab_stop
    commands%move_to_tab_stop%run => cmd_move_to_tab_stop
    commands%move_to_left_margin%run => cmd_move_to_left_margin
    commands%beginning_of_line_text%run => cmd_beginning_of_line_text
    commands%indent_to%run => cmd_indent_to
    if (indent_rigidly_map%index == 0) then
        call make_sparse_keymap(indent_rigidly_map)
        call key_parse("TAB", key, st)
        call keymap_set_key(indent_rigidly_map, key, sym("indent_rigidly_right"), st)
        call key_parse("<left>", key, st)
        call keymap_set_key(indent_rigidly_map, key, sym("indent_rigidly_left"), st)
        call key_parse("<right>", key, st)
        call keymap_set_key(indent_rigidly_map, key, sym("indent_rigidly_right"), st)
        call key_parse("S-<left>", key, st)
        call keymap_set_key(indent_rigidly_map, key, sym("indent_rigidly_left_to_tab_stop"), st)
        call key_parse("S-<right>", key, st)
        call keymap_set_key(indent_rigidly_map, key, sym("indent_rigidly_right_to_tab_stop"), st)
    end if
    end procedure syms_of_indent
end submodule indent_cmds_fm
