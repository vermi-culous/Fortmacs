! simple_electric_fm.f90 -- blinking parens and newline-and-indent, a submodule of simple_fm.
! Ported from GNU Emacs 31.1 lisp/simple.el.
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

!> Blinking matching parens (`blink_matching_open' from
!> `post_self_insert_hook') and `newline_and_indent': a submodule because
!> they read paren.el's and electric.el's modes (paren_fm, modes_fm),
!> which use simple_fm.
!>
!> Deferred: `blink_matching_paren_highlight_offscreen' and the face
!> `blink_matching_paren_offscreen' (strings carry no text properties),
!> the `shadow' face of "Matches " in the echo area,
!> `syntax_propertize', and `noninteractive' (no batch mode).
submodule(simple_fm) simple_electric_fm
    use syntax_fm, only: syntax_entry, syntax_descriptor_type, skip_syntax_backward, parse_sexp_ignore_comments, &
                         char_syntax, SCLOSE, SQUOTE
    use lisp_cmds_fm, only: forward_sexp
    use editfns_fm, only: narrow_to_region, widen
    use window_fm, only: window_pos_visible_p => pos_visible_in_window_p
    use frame_fm, only: selected_window
    use itree_fm, only: make_overlay, move_overlay, delete_overlay, overlay_put, overlayp
    use paren_fm, only: show_paren_context_when_offscreen
    use modes_fm, only: minor_modes
    use minibuffer_fm, only: minibuffer_message
    use font_lock_fm, only: font_lock_ensure
    use cmds_fm, only: post_self_insert_hook
    use indent_fm, only: indent_according_to_mode
    use keyboard_fm, only: executing_kbd_macro_p, sit_for
    implicit none(type, external)

    !> Overlay used to highlight the matching paren.  Port of simple.el
    !> blink-matching--overlay (made on first use).
    type(handle_type) :: blink_matching_overlay

contains

    module procedure syms_of_simple_electric
    commands%blink_matching_open%run => cmd_blink_matching_open
    commands%newline_and_indent%run => cmd_newline_and_indent
    commands%backward_delete_char_untabify%run => cmd_backward_delete_char_untabify
    if (.not. associated(blink_matching_check_function)) blink_matching_check_function => blink_matching_check_mismatch
    if (.not. associated(blink_paren_function)) blink_paren_function => blink_matching_open
    ! Most likely, this hook is nil, so this arg doesn't matter, but I use
    ! it as a reminder that this function usually likes to be run after
    ! others since it does `sit_for'.  That's also the reason it get a
    ! `priority' prop of 100.
    call post_self_insert_hook%add(blink_paren_post_self_insert_function, depth=90)
    end procedure syms_of_simple_electric

    !> The syntax descriptor of the character after POS (syntax-after):
    !> FOUND false outside the accessible region (nil).
    subroutine syntax_after(pos, d, found)
        integer(int64), intent(in) :: pos
        type(syntax_descriptor_type), intent(out) :: d
        logical, intent(out) :: found
        integer(int32) :: c
        found = .false.
        if (pos < point_min() .or. pos >= point_max()) return
        call char_after(c, found, pos)
        if (found) d = syntax_entry(int(c))
    end subroutine syntax_after

    module procedure blink_matching_check_mismatch
    type(syntax_descriptor_type) :: end_syntax, start_syntax
    integer(int32) :: c
    integer :: matching_paren
    logical :: found, have_start
    mismatch = .false.
    call syntax_after(end - 1, end_syntax, found)
    matching_paren = -1
    if (found) then
        if (iand(end_syntax%code, 255) == SCLOSE) matching_paren = end_syntax%match
    end if
    ! For self-matched chars like " and $, we can't know when they're
    ! mismatched or unmatched, so we can do it only for parens.
    if (matching_paren < 0) return
    mismatch = .true.
    if (start == 0) return
    call char_after(c, found, start)
    if (found .and. c == matching_paren) mismatch = .false.
    ! The cdr might hold a new paren-class info rather than a
    ! matching-char info, in which case the two CDRs should match.
    call syntax_after(start, start_syntax, have_start)
    if (have_start) then
        if (start_syntax%match == matching_paren) mismatch = .false.
    end if
    end procedure blink_matching_check_mismatch

    !> Momentarily highlight the beginning of the sexp before point.
    !> Port of simple.el:blink-matching-open (the command).
    subroutine cmd_blink_matching_open(status)
        type(status_type), intent(out) :: status
        call blink_matching_open(status)
    end subroutine cmd_blink_matching_open

    module procedure blink_matching_open
    integer(int64) :: oldpos, blinkpos, savedb, savedz, opoint
    type(syntax_descriptor_type) :: code
    type(status_type) :: st
    logical :: saved_ignore, found, mismatch, visible
    status = status_type()
    if (bobp() .or. blink_matching_paren == BLINK_MATCHING_PAREN_NIL) return
    oldpos = point()
    ! (save-excursion (save-restriction ...))
    savedb = buffers(cur)%begv
    savedz = buffers(cur)%text%z - buffers(cur)%zv
    if (blink_matching_paren_distance >= 0) then
        ! (point-min) unless minibuf.
        call narrow_to_region(max(minibuffer_prompt_end(), point() - blink_matching_paren_distance), oldpos, st)
    end if
    saved_ignore = parse_sexp_ignore_comments%value
    parse_sexp_ignore_comments%value = parse_sexp_ignore_comments%value .and. &
                                       .not. blink_matching_paren_dont_ignore_comments
    blinkpos = 0
    call forward_sexp(-1_int64, st)
    if (st%ok()) then
        ! backward-sexp skips backward over prefix chars, so move back to
        ! the matching paren.
        do while (point() < oldpos - 1)
            call syntax_after(point(), code, found)
            if (.not. found) exit
            if (iand(code%code, 255) /= SQUOTE .and. iand(code%code, 1048576) /= 1048576) exit
            call goto_char(point() + 1)
        end do
        blinkpos = point()
    end if
    parse_sexp_ignore_comments%value = saved_ignore
    call widen()
    call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
    call goto_char(oldpos)
    mismatch = .false.
    if (associated(blink_matching_check_function)) mismatch = blink_matching_check_function(blinkpos, oldpos)
    if (mismatch) then
        if (blinkpos /= 0) then
            if (minibufferp()) then
                call minibuffer_message("Mismatched parentheses")
            else
                call message("Mismatched parentheses")
            end if
        else
            if (minibufferp()) then
                call minibuffer_message("No matching parenthesis found")
            else
                call message("No matching parenthesis found")
            end if
        end if
        return
    end if
    if (blinkpos == 0) return
    visible = blink_matching_paren == BLINK_MATCHING_PAREN_JUMP_OFFSCREEN
    if (.not. visible) visible = window_pos_visible_p(blinkpos, selected_window())
    if (visible) then
        ! Matching open within window, temporarily move to or highlight
        ! char after blinkpos but only if `blink_matching_paren_on_screen'
        ! is true.
        if (.not. blink_matching_paren_on_screen .or. minor_modes%show_paren_mode%state%value) return
        if (blink_matching_paren == BLINK_MATCHING_PAREN_JUMP .or. &
            blink_matching_paren == BLINK_MATCHING_PAREN_JUMP_OFFSCREEN) then
            opoint = point()
            call goto_char(blinkpos)
            call sit_for(int(blink_matching_delay*1000, int32))
            call goto_char(opoint)
        else
            if (.not. overlayp(blink_matching_overlay)) then
                call make_overlay(blink_matching_overlay, point(), point(), st, front_advance=.true.)
                call overlay_put(blink_matching_overlay, "face", sym("show_paren_match"), st)
                call delete_overlay(blink_matching_overlay)
            end if
            call move_overlay(blink_matching_overlay, blinkpos, blinkpos + 1, st)
            call sit_for(int(blink_matching_delay*1000, int32))
            call delete_overlay(blink_matching_overlay)
        end if
    else if (.not. show_paren_context_when_offscreen) then
        call minibuffer_message("Matches "//blink_paren_open_paren_line_string(blinkpos))
    end if
    end procedure blink_matching_open

    module procedure blink_paren_open_paren_line_string
    integer(int64) :: opoint, bol, moved, first_beg, first_end, last_end
    character(len=:), allocatable :: a
    type(status_type) :: st
    logical :: not_bol, not_eol, not_bob
    integer(int64), allocatable :: regions(:, :)
    integer :: k
    opoint = point()
    call goto_char(pos)
    ! Capture the regions in terms of (beg . end) pairs whose
    ! buffer-substrings we want to show as a context string.
    moved = skip_chars_backward(" "//TAB, st)
    not_bol = .not. bolp()
    call goto_char(pos)
    if (not_bol) then
        ! Show what precedes the open in its line, if anything.
        bol = line_beginning_position()
        allocate (regions(2, 1))
        regions(:, 1) = [bol, pos + 1]
    else
        call goto_char(pos + 1)
        moved = skip_chars_forward(" "//TAB, st)
        not_eol = .not. eolp()
        call goto_char(pos)
        if (not_eol) then
            ! Show what follows the open in its line, if anything.
            allocate (regions(2, 1))
            regions(:, 1) = [pos, line_end_position()]
        else
            moved = skip_chars_backward(LF//" "//TAB, st)
            not_bob = .not. bobp()
            call goto_char(pos)
            if (not_bob) then
                ! Otherwise show the previous nonblank line, if there is
                ! one.
                moved = skip_chars_backward(LF//" "//TAB, st)
                first_beg = line_beginning_position()
                call end_of_line()
                moved = skip_chars_backward(" "//TAB, st)
                first_end = point()
                allocate (regions(2, 2))
                regions(:, 1) = [first_beg, first_end]
                regions(:, 2) = [pos, pos + 1]
            else
                ! There is nothing to show except the char itself.
                allocate (regions(2, 1))
                regions(:, 1) = [pos, pos + 1]
            end if
        end if
    end if
    ! Ensure we've font-locked the context region.
    last_end = regions(2, size(regions, 2))
    call font_lock_ensure(st, regions(1, 1), last_end)
    line_string = ""
    do k = 1, size(regions, 2)
        if (k > 1) line_string = line_string//"..."
        call buffer_substring(regions(1, k), regions(2, k), a, st)
        line_string = line_string//a
    end do
    call goto_char(opoint)
    end procedure blink_paren_open_paren_line_string

    module procedure blink_paren_post_self_insert_function
    integer(int32) :: c
    integer(int64) :: opoint, start, ev
    logical :: found, odd
    status = status_type()
    ! Sanity check.
    if (.not. fixnump(last_command_event)) return
    ev = xfixnum(last_command_event)
    call char_before(c, found)
    if (.not. found .or. int(c, int64) /= ev) return
    if (ev < 0 .or. ev > int(huge(0_int32), int64)) return
    if (char_syntax(int(ev)) /= ")" .and. char_syntax(int(ev)) /= "$") return
    if (.not. associated(blink_paren_function)) return
    if (executing_kbd_macro_p()) return
    ! Verify an even number of quoting characters precede the close.
    opoint = point()
    call goto_char(opoint - 1)
    start = skip_syntax_backward("/\")
    start = point()
    call goto_char(opoint)
    odd = modulo(opoint - start, 2_int64) == 1
    if (.not. odd) return
    call blink_paren_function(status)
    end procedure blink_paren_post_self_insert_function

    !> Insert a newline, then indent according to major mode.
    !> Indentation is done using the value of `indent_line_function'.
    !> In programming language modes, this is the same as TAB.
    !> In some text modes, where TAB inserts a tab, this command indents to the
    !> column specified by the function `current_left_margin'.
    !>
    !> With ARG, perform this action that many times.  Port of
    !> simple.el:newline-and-indent (the command, "*p").
    subroutine cmd_newline_and_indent(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call newline_and_indent(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_newline_and_indent

    module procedure newline_and_indent
    integer(int64) :: n, k
    logical :: saved
    call delete_horizontal_space(.true., status)
    if (.not. status%ok()) return
    n = 1
    if (present(arg)) n = arg
    saved = minor_modes%electric_indent_mode%state%value
    minor_modes%electric_indent_mode%state%value = .false.
    do k = 1, n
        call newline(status, interactive=.true.)
        if (.not. status%ok()) exit
        call indent_according_to_mode(status)
        if (.not. status%ok()) exit
    end do
    minor_modes%electric_indent_mode%state%value = saved
    end procedure newline_and_indent

    !> Delete characters backward, changing tabs into spaces.
    !> The exact behavior depends on `backward_delete_char_untabify_method'.
    !>
    !> Delete ARG chars, and kill (save in kill ring) if KILLP is true.
    !>
    !> If Transient Mark mode is enabled, the mark is active, and ARG is 1,
    !> delete the text in the region and deactivate the mark instead.
    !> To disable this, set option `delete_active_region' to false.
    !>
    !> Interactively, ARG is the prefix arg (default 1)
    !> and KILLP is true if a prefix arg was specified.  Port of
    !> simple.el:backward-delete-char-untabify (the command, "*p\nP").
    subroutine cmd_backward_delete_char_untabify(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call backward_delete_char_untabify(prefix_numeric_value(current_prefix_arg), status, &
                                           .not. nilp(current_prefix_arg))
    end subroutine cmd_backward_delete_char_untabify

    module procedure backward_delete_char_untabify
    integer(int64) :: count, col, opoint, oldpt, wh, n, moved, b, e
    integer(int32) :: c
    logical :: found, kill
    character(len=:), allocatable :: skip
    type(status_type) :: st
    type(handle_type) :: here
    kill = .false.
    if (present(killp)) kill = killp
    if (use_region_p() .and. delete_active_region .and. arg == 1) then
        ! Delete the text of the region and deactivate the mark, in the
        ! same way that `delete_backward_char' does.
        if (kill) then
            call delete_backward_char(1_int64, status, kill)
        else
            ! The default region-extract-function with METHOD `delete_only'.
            call region_beginning(b, status)
            if (status%ok()) call region_end(e, status)
            if (status%ok()) call delete_region(b, e, status)
        end if
        return
    end if
    if (backward_delete_char_untabify_method == UNTABIFY_METHOD_UNTABIFY) then
        count = arg
        call copy_marker_at(point(), here, .false., st)
        do while (count > 0 .and. .not. bobp())
            call char_before(c, found)
            if (found .and. c == 9) then
                col = current_column()
                call goto_char(point() - 1)
                col = col - current_column()
                call insert_char_n(32_int32, col, status)
                if (status%ok()) call delete_char(1_int64, status)
                if (.not. status%ok()) then
                    call free_marker(here, st)
                    return
                end if
            end if
            call goto_char(point() - 1)
            count = count - 1
        end do
        call marker_position(here, opoint, st)
        call goto_char(opoint)
        call free_marker(here, st)
    end if
    skip = ""
    if (backward_delete_char_untabify_method == UNTABIFY_METHOD_HUNGRY) skip = " "//TAB
    if (backward_delete_char_untabify_method == UNTABIFY_METHOD_ALL) skip = " "//TAB//LF//achar(13)
    n = arg
    if (len(skip) > 0) then
        oldpt = point()
        moved = skip_chars_backward(skip, st)
        call goto_char(constrain_to_field(point(), oldpt, .false., .false.))
        wh = oldpt - point()
        call goto_char(oldpt)
        if (wh /= 0) n = arg + wh - 1
    end if
    call delete_backward_char(n, status, kill)
    end procedure backward_delete_char_untabify
end submodule simple_electric_fm
