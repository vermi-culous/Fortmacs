! paren_fm.f90 -- highlight matching paren.
! Ported from GNU Emacs 31.1 lisp/paren.el.
!
! Copyright (C) 1993, 1996, 2001-2026 Free Software Foundation, Inc.
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

!> Show Paren mode: after `show_paren_delay' seconds of idle time, an
!> idle timer (timer_fm) highlights the paren near point and its match
!> with two overlays (itree_fm) in the faces `show_paren_match',
!> `show_paren_match_expression' and `show_paren_mismatch' (faces_fm).
!> The mode is on by default, as upstream (`:init-value t'): bindings_fm
!> turns it on at startup.
!>
!> Deferred: `show_paren_context_when_offscreen' shown in an overlay or a
!> child frame (needs the `display' overlay property and child frames;
!> the echo area is ported), the `shadow' face of "Matches " in the echo
!> area (messages carry no text properties), `syntax_propertize'
!> (syntax_fm has no `syntax_table' properties), and buffer predicates
!> other than a procedure (buffer-match-p's conditions).
module paren_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_fm, only: status_type
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, nilp, make_fixnum, fixnump, xfixnum
    use lread_fm, only: sym
    use data_fm, only: local_variable_p, kill_local_variable, default_value, set_default, make_local_variable, &
                       symbol_value, set
    use commands_fm, only: commands
    use eval_fm, only: hook_type
    use buffer_fm, only: buffers, cur
    use editfns_fm, only: point, point_min, point_max, goto_char, narrow_to_region, widen, char_after, char_before, &
                          line_end_position
    use syntax_fm, only: syntax_entry, syntax_descriptor_type, parse_state_type, syntax_ppss, scan_sexps, &
                         skip_syntax_backward, skip_chars_backward, parse_sexp_ignore_comments, SOPEN, SCLOSE, SMATH
    use itree_fm, only: make_overlay, move_overlay, delete_overlay, overlay_put, overlayp
    use timer_fm, only: run_with_idle_timer, free_timer, timerp
    use modes_fm, only: minor_modes
    use derived_fm, only: derived_mode_p
    use simple_fm, only: blink_matching_paren_distance, blink_paren_open_paren_line_string, back_to_indentation
    use minibuffer_fm, only: minibuffer_message
    use window_fm, only: pos_visible_in_window_p
    use frame_fm, only: selected_window
    use dispnew_fm, only: ding
    implicit none(type, external)
    private

    public :: syms_of_paren, show_paren_mode, show_paren_local_mode, show_paren_function, show_paren_default, &
              show_paren_unescaped_p, show_paren_categorize_paren, show_paren_locate_near_paren, &
              show_paren_enabled_p, show_paren_default_predicate, show_paren_data_proc, show_paren_predicate_proc

    !> Values of `show_paren_style'.
    integer, parameter, public :: SHOW_PAREN_PARENTHESIS = 0, SHOW_PAREN_EXPRESSION = 1, SHOW_PAREN_MIXED = 2

    !> Values of `show_paren_not_in_comments_or_strings': never highlight
    !> parens inside comments and strings (`all'), don't highlight
    !> mismatched parens there (`on-mismatch'), always highlight parens
    !> (nil).
    integer, parameter, public :: SHOW_PAREN_COMMENTS_STRINGS_ALWAYS = 0, SHOW_PAREN_COMMENTS_STRINGS_ALL = 1, &
                                  SHOW_PAREN_COMMENTS_STRINGS_ON_MISMATCH = 2

    !> Style used when showing a matching paren.
    !> Valid styles are `SHOW_PAREN_PARENTHESIS' (meaning show the matching
    !> paren), `SHOW_PAREN_EXPRESSION' (meaning show the entire expression
    !> enclosed by the paren) and `SHOW_PAREN_MIXED' (meaning show the
    !> matching paren if it is visible, and the expression otherwise).
    !> Port of paren.el show-paren-style.
    integer, public :: show_paren_style = SHOW_PAREN_PARENTHESIS

    !> Time in seconds to delay before showing a matching paren.
    !> If you change this while `show_paren_mode' is active, you must
    !> toggle the mode off and on again for this to take effect.  Port of
    !> paren.el show-paren-delay.
    real, public :: show_paren_delay = 0.125

    !> Priority of paren highlighting overlays.  Port of paren.el
    !> show-paren-priority.
    integer, public :: show_paren_priority = 1000

    !> If true, beep if mismatched paren is detected.  Port of paren.el
    !> show-paren-ring-bell-on-mismatch.
    logical, public :: show_paren_ring_bell_on_mismatch = .false.

    !> If true, show parens when point is just inside one.
    !> This will only be done when point isn't also just outside a paren.
    !> Port of paren.el show-paren-when-point-inside-paren.
    logical, public :: show_paren_when_point_inside_paren = .false.

    !> If true, show parens when point is in the line's periphery.
    !> The periphery is at the beginning or end of a line or in any
    !> whitespace there.  Port of paren.el show-paren-when-point-in-periphery.
    logical, public :: show_paren_when_point_in_periphery = .false.

    !> True turns on openparen highlighting when matching forward.
    !> When false, and point stands just before an open paren, the paren
    !> is not highlighted, the cursor being regarded as adequate to mark
    !> its position.  Port of paren.el show-paren-highlight-openparen.
    logical, public :: show_paren_highlight_openparen = .true.

    !> If true, show context around the opening paren if it is offscreen.
    !> The context is usually the line that contains the openparen,
    !> except if the openparen is on its own line, in which case the
    !> context includes the previous nonblank line.
    !>
    !> The context is shown in the echo area.  Port of paren.el
    !> show-paren-context-when-offscreen (the values `overlay' and
    !> `child-frame' are not ported).
    logical, public :: show_paren_context_when_offscreen = .false.

    !> If `SHOW_PAREN_COMMENTS_STRINGS_ALL', never highlight parens inside
    !> comments and strings.  If `SHOW_PAREN_COMMENTS_STRINGS_ON_MISMATCH',
    !> do not highlight mismatched parens inside comments and strings.  If
    !> `SHOW_PAREN_COMMENTS_STRINGS_ALWAYS' (the default), always highlight
    !> parens wherever they are.  Port of paren.el
    !> show-paren-not-in-comments-or-strings.
    integer, public :: show_paren_not_in_comments_or_strings = SHOW_PAREN_COMMENTS_STRINGS_ALWAYS

    !> What `show_paren_data_function' finds: the opener/closer "near"
    !> point from HERE_BEG to HERE_END, its match from THERE_BEG to
    !> THERE_END (0 for none, upstream's nil) and whether they mismatch.
    type, public :: show_paren_data_type
        integer(int64) :: here_beg = 0, here_end = 0, there_beg = 0, there_end = 0
        logical :: mismatch = .false.
    end type show_paren_data_type

    abstract interface
        !> A `show_paren_data_function': FOUND is false if there's no
        !> opener/closer near point (upstream's nil), else DATA describes it.
        subroutine show_paren_data_proc(found, data)
            import :: show_paren_data_type
            implicit none(type, external)
            logical, intent(out) :: found
            type(show_paren_data_type), intent(out) :: data
        end subroutine show_paren_data_proc

        !> A `show_paren_predicate': whether to use `show_paren_mode' in the
        !> current buffer.
        logical function show_paren_predicate_proc()
            implicit none(type, external)
        end function show_paren_predicate_proc
    end interface

    !> Function to find the opener/closer "near" point and its match.
    !> The function is called with no argument and should give either
    !> FOUND false if there's no opener/closer near point, or the
    !> positions HERE_BEG HERE_END THERE_BEG THERE_END and MISMATCH,
    !> where HERE_BEG..HERE_END is expected to be near point.  Port of
    !> paren.el show-paren-data-function.
    procedure(show_paren_data_proc), pointer, public :: show_paren_data_function => null()

    !> Whether to use `show_paren_mode' in a buffer.
    !> The default is to enable the mode in all buffers that don't
    !> derive from `special_mode', which means that it's on (by default)
    !> in all editing buffers.
    !> The predicate is called in the buffer.  Port of paren.el
    !> show-paren-predicate (a procedure; buffer-match-p's conditions are
    !> not ported).
    procedure(show_paren_predicate_proc), pointer, public :: show_paren_predicate => null()

    !> Hook run after entering or leaving `show_paren_mode'.  Port of the
    !> show-paren-mode-hook define-minor-mode defines.
    type(hook_type), target, public :: show_paren_mode_hook = hook_type(name="show_paren_mode_hook")

    !> Hook run after entering or leaving `show_paren_local_mode'.  Port
    !> of the show-paren-local-mode-hook define-minor-mode defines.
    type(hook_type), target, public :: show_paren_local_mode_hook = hook_type(name="show_paren_local_mode_hook")

    !> show-paren--idle-timer, show-paren--overlay (the matching paren)
    !> and show-paren--overlay-1 (the paren at point).
    type(handle_type) :: idle_timer, overlay, overlay_1

contains

    !> Define Show Paren mode and Show Paren Local mode (as loading
    !> paren.el does).
    subroutine syms_of_paren()
        call minor_modes%show_paren_mode%define("show_paren_mode", global=.true., body=show_paren_mode_body, &
                                                hook=show_paren_mode_hook)
        commands%show_paren_mode%run => show_paren_mode
        call minor_modes%show_paren_local_mode%define("show_paren_local_mode", body=show_paren_local_mode_body, &
                                                      hook=show_paren_local_mode_hook, getter=show_paren_enabled_p)
        commands%show_paren_local_mode%run => show_paren_local_mode
        if (.not. associated(show_paren_data_function)) show_paren_data_function => show_paren_default
        if (.not. associated(show_paren_predicate)) show_paren_predicate => show_paren_default_predicate
    end subroutine syms_of_paren

    !> Toggle visualization of matching parens (Show Paren mode).
    !>
    !> When enabled, any matching parenthesis is highlighted in `show_paren_style'
    !> after `show_paren_delay' seconds of Emacs idle time.
    !>
    !> Also see `show_paren_predicate', which controls which buffers
    !> this mode is enabled in.
    !>
    !> This is a global minor mode.  To toggle the mode in a single buffer,
    !> use `show_paren_local_mode'.  Port of paren.el:show-paren-mode.
    subroutine show_paren_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%show_paren_mode%command(status)
    end subroutine show_paren_mode

    !> The body of Show Paren mode: enable or disable the mechanism.
    subroutine show_paren_mode_body(on, status)
        logical, intent(in) :: on
        type(status_type), intent(out) :: status
        status = status_type()
        if (on) continue
        ! First get rid of the old idle timer.
        if (timerp(idle_timer)) call free_timer(idle_timer)
        idle_timer = null_handle()
        call run_with_idle_timer(idle_timer, real(show_paren_delay, real64), .true., show_paren_function_timer, &
                                 status)
        if (.not. minor_modes%show_paren_mode%state%value) call show_paren_delete_overlays()
    end subroutine show_paren_mode_body

    !> Port of paren.el:show-paren--delete-overlays.
    subroutine show_paren_delete_overlays()
        call delete_overlay(overlay)
        call delete_overlay(overlay_1)
    end subroutine show_paren_delete_overlays

    !> Toggle `show_paren_mode' only in this buffer.  Port of
    !> paren.el:show-paren-local-mode.
    subroutine show_paren_local_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%show_paren_local_mode%command(status)
    end subroutine show_paren_local_mode

    !> The body of Show Paren Local mode with its new state VAL: the
    !> setter (setq-local show-paren-mode VAL), then the body.
    subroutine show_paren_local_mode_body(val, status)
        logical, intent(in) :: val
        type(status_type), intent(out) :: status
        type(value_type) :: default
        type(status_type) :: st
        status = status_type()
        call minor_modes%show_paren_mode%state%set_local(val)
        call default_value(sym("show_paren_mode"), default, st)
        if (minor_modes%show_paren_mode%state%value .eqv. .not. nilp(default)) then
            if (.not. minor_modes%show_paren_mode%state%value) then
                call show_paren_delete_overlays()
                call kill_local_variable(sym("show_paren_mode"), st)
            end if
        else if (nilp(default)) then
            ! Locally enabled, but globally disabled.
            call minor_modes%show_paren_mode%enable(status)
            ! But keep it globally disabled.
            call set_default(sym("show_paren_mode"), nil(), st)
        else
            ! Locally disabled only.
            call show_paren_delete_overlays()
        end if
    end subroutine show_paren_local_mode_body

    !> Determine whether the paren after POS is unescaped.  Port of
    !> paren.el:show-paren--unescaped-p.
    logical function show_paren_unescaped_p(pos) result(unescaped)
        integer(int64), intent(in) :: pos
        integer(int64) :: opoint, moved
        opoint = point()
        call goto_char(pos)
        moved = skip_syntax_backward("/\")
        unescaped = iand(abs(moved), 1_int64) == 0
        call goto_char(opoint)
    end function show_paren_unescaped_p

    !> The syntax class of the character after POS, or -1 if there is none
    !> (syntax-class of syntax-after).
    integer function syntax_class_after(pos) result(syntax_class)
        integer(int64), intent(in) :: pos
        integer :: c
        logical :: found
        type(syntax_descriptor_type) :: d
        syntax_class = -1
        if (pos < point_min() .or. pos >= point_max()) return
        call char_after(c, found, pos)
        if (.not. found) return
        d = syntax_entry(c)
        syntax_class = iand(d%code, 255)
    end function syntax_class_after

    !> The matching character of the syntax of the character after POS
    !> (the cdr of syntax-after), or -1 for nil.
    integer function syntax_match_after(pos) result(m)
        integer(int64), intent(in) :: pos
        integer :: c
        logical :: found
        type(syntax_descriptor_type) :: d
        m = -1
        if (pos < point_min() .or. pos >= point_max()) return
        call char_after(c, found, pos)
        if (found) then
            d = syntax_entry(c)
            m = d%match
        end if
    end function syntax_match_after

    !> Determine whether the character after POS has paren syntax,
    !> and if so, give DIR 1 for an open paren, -1 for a close paren, and
    !> OUTSIDE the buffer position of the outside of the paren.  If the
    !> character isn't a paren, or it is an escaped paren, DIR is 0
    !> (upstream's nil).  Port of paren.el:show-paren--categorize-paren.
    subroutine show_paren_categorize_paren(pos, dir, outside)
        integer(int64), intent(in) :: pos
        integer, intent(out) :: dir
        integer(int64), intent(out) :: outside
        dir = 0
        outside = 0
        if (syntax_class_after(pos) == SOPEN) then
            if (show_paren_unescaped_p(pos)) then
                dir = 1
                outside = pos
            end if
        else if (syntax_class_after(pos) == SCLOSE) then
            if (show_paren_unescaped_p(pos)) then
                dir = -1
                outside = pos + 1
            end if
        end if
    end subroutine show_paren_categorize_paren

    !> Locate an unescaped paren "near" point to show.
    !> If one is found, give DIR 1 for an open paren, -1 for a close
    !> paren, and OUTSIDE the buffer position of the outside of the paren.
    !> Otherwise DIR is 0.  Port of paren.el:show-paren--locate-near-paren.
    subroutine show_paren_locate_near_paren(dir, outside)
        integer, intent(out) :: dir
        integer(int64), intent(out) :: outside
        integer(int64) :: ind_pos, eol_pos, opoint, moved, before_outside, after_outside
        integer :: before_dir, after_dir
        type(status_type) :: st
        opoint = point()
        call back_to_indentation()
        ind_pos = point()
        call goto_char(line_end_position())
        moved = skip_chars_backward(" "//achar(9), st, ind_pos)
        eol_pos = point()
        call goto_char(opoint)
        call show_paren_categorize_paren(opoint - 1, before_dir, before_outside)
        call show_paren_categorize_paren(opoint, after_dir, after_outside)
        dir = 0
        outside = 0
        if (before_dir == -1) then
            ! Point is immediately outside a paren.
            dir = before_dir
            outside = before_outside
        else if (after_dir == 1) then
            dir = after_dir
            outside = after_outside
        else if (show_paren_when_point_inside_paren .and. before_dir /= 0) then
            ! Point is immediately inside a paren.
            dir = before_dir
            outside = before_outside
        else if (show_paren_when_point_inside_paren .and. after_dir /= 0) then
            dir = after_dir
            outside = after_outside
        else if (show_paren_when_point_in_periphery .and. opoint <= ind_pos) then
            ! Point is in the whitespace before the code.
            call show_paren_categorize_paren(ind_pos, dir, outside)
            if (dir == 0) call show_paren_categorize_paren(eol_pos - 1, dir, outside)
        else if (show_paren_when_point_in_periphery .and. opoint >= eol_pos) then
            ! Point is in the whitespace after the code.
            call show_paren_categorize_paren(eol_pos - 1, dir, outside)
        end if
    end subroutine show_paren_locate_near_paren

    !> Find the opener/closer near point and its match.
    !>
    !> It is the default value of `show_paren_data_function'.  Port of
    !> paren.el:show-paren--default.
    subroutine show_paren_default(found, data)
        logical, intent(out) :: found
        type(show_paren_data_type), intent(out) :: data
        type(parse_state_type) :: ppss
        type(status_type) :: st
        integer(int64) :: outside, pos, back, beg, end, opoint, savedb, savedz
        integer :: dir, cb, ca
        logical :: saved_ignore, mismatch, have_pos, fb, fa
        found = .false.
        call show_paren_locate_near_paren(dir, outside)
        ! If we're inside a comment, then we probably want to blink a
        ! matching parentheses in the comment.  So don't ignore comments
        ! in that case.
        opoint = point()
        call syntax_ppss(ppss)
        call goto_char(opoint)
        saved_ignore = parse_sexp_ignore_comments%value
        if (ppss%incomment /= 0) parse_sexp_ignore_comments%value = .false.
        ! Find the other end of the sexp.
        if (dir /= 0) then
            if (dir == 1) then
                data%here_beg = outside
                data%here_end = outside + 1
            else
                data%here_beg = outside - 1
                data%here_end = outside
            end if
            savedb = buffers(cur)%begv
            savedz = buffers(cur)%text%z - buffers(cur)%zv
            ! Determine the range within which to look for a match.
            if (blink_matching_paren_distance >= 0) then
                beg = max(point_min(), point() - blink_matching_paren_distance)
                call narrow_to_region(beg, min(point_max(), point() + blink_matching_paren_distance), st)
            end if
            ! Scan across one sexp within that range.  Errors or nil mean
            ! there is a mismatch.
            mismatch = .false.
            have_pos = .false.
            pos = scan_sexps(outside, int(dir, int64), st)
            if (.not. st%ok()) then
                mismatch = .true.
            else if (pos /= 0) then
                have_pos = .true.
            end if
            ! Move back the other way and verify we get back to the
            ! starting point.  If not, these two parens don't really
            ! match.  Maybe the one at point is escaped and doesn't really
            ! count, or one is inside a comment.
            if (have_pos) then
                back = scan_sexps(pos, int(-dir, int64), st)
                if (.not. st%ok() .or. back /= outside) have_pos = .false.
            end if
            ! If found a "matching" paren, see if it is the right kind of
            ! paren to match the one we started at.
            if (.not. have_pos) then
                if (mismatch) then
                    found = .true.
                    data%mismatch = .true.
                end if
            else
                beg = min(pos, outside)
                end = max(pos, outside)
                if (syntax_class_after(beg) /= SMATH) then
                    call char_before(cb, fb, end)
                    call char_after(ca, fa, beg)
                    mismatch = .true.
                    ! (The cdrs can give nil.)
                    if (fb .and. syntax_match_after(beg) >= 0) then
                        if (cb == syntax_match_after(beg)) mismatch = .false.
                    end if
                    if (fa .and. syntax_match_after(end - 1) >= 0) then
                        if (ca == syntax_match_after(end - 1)) mismatch = .false.
                    end if
                    ! The cdr might hold a new paren-class info rather
                    ! than a matching-char info, in which case the two
                    ! CDRs should match.
                    if (syntax_match_after(end - 1) == syntax_match_after(beg)) mismatch = .false.
                end if
                found = .true.
                if (dir == 1) then
                    data%there_beg = pos - 1
                    data%there_end = pos
                else
                    data%there_beg = pos
                    data%there_end = pos + 1
                end if
                data%mismatch = mismatch
            end if
            call widen()
            call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
        end if
        parse_sexp_ignore_comments%value = saved_ignore
    end subroutine show_paren_default

    !> Whether Show Paren mode is on in the current buffer.  Port of
    !> paren.el:show-paren--enabled-p.
    logical function show_paren_enabled_p() result(enabled)
        enabled = minor_modes%show_paren_mode%state%value
        if (.not. enabled) return
        ! If we're using `show_paren_local_mode', then always heed the
        ! value.
        if (local_variable_p(sym("show_paren_mode"))) return
        ! If not, check that the predicate matches.
        if (associated(show_paren_predicate)) enabled = show_paren_predicate()
    end function show_paren_enabled_p

    !> The default `show_paren_predicate': true unless the major mode
    !> derives from `special_mode' (paren.el's (not (derived-mode
    !> . special-mode))).
    logical function show_paren_default_predicate() result(usable)
        usable = .not. derived_mode_p(["special_mode"])
    end function show_paren_default_predicate

    !> Whether position POS is in a string or comment: the context of
    !> syntax-ppss-context (syntax-ppss POS), keeping point.
    logical function in_string_or_comment(pos)
        integer(int64), intent(in) :: pos
        type(parse_state_type) :: ppss
        integer(int64) :: opoint
        opoint = point()
        call syntax_ppss(ppss, pos)
        call goto_char(opoint)
        in_string_or_comment = ppss%instring >= 0 .or. ppss%incomment /= 0
    end function in_string_or_comment

    !> The overlays, made (empty and deleted) on first use, with
    !> front-advance as upstream's (make-overlay (point) (point) nil t).
    subroutine ensure_overlays()
        type(status_type) :: st
        if (.not. overlayp(overlay)) then
            call make_overlay(overlay, point(), point(), st, front_advance=.true.)
            call delete_overlay(overlay)
        end if
        if (.not. overlayp(overlay_1)) then
            call make_overlay(overlay_1, point(), point(), st, front_advance=.true.)
            call delete_overlay(overlay_1)
        end if
    end subroutine ensure_overlays

    !> `show_paren_function' as the idle timer's function.
    subroutine show_paren_function_timer(status)
        type(status_type), intent(out) :: status
        call show_paren_function(status)
    end subroutine show_paren_function_timer

    !> The last position of point for which `show_paren_function' was
    !> called (show-paren--last-pos, buffer-local), or 0 for nil.
    integer(int64) function last_pos()
        type(value_type) :: v
        type(status_type) :: st
        last_pos = 0
        if (.not. local_variable_p(sym("show_paren__last_pos"))) return
        call symbol_value(sym("show_paren__last_pos"), v, st)
        if (fixnump(v)) last_pos = xfixnum(v)
    end function last_pos

    subroutine set_last_pos(pos)
        integer(int64), intent(in) :: pos
        type(status_type) :: st
        call make_local_variable(sym("show_paren__last_pos"), st)
        call set(sym("show_paren__last_pos"), make_fixnum(pos), st)
    end subroutine set_last_pos

    !> Highlight the parentheses until the next input arrives.  Port of
    !> paren.el:show-paren-function.
    subroutine show_paren_function(status)
        type(status_type), intent(out) :: status
        type(show_paren_data_type) :: data
        character(len=:), allocatable :: face, context
        integer(int64) :: closest, openparen
        logical :: found, highlight_expression
        type(status_type) :: st
        status = status_type()
        if (cur == 0) return
        call ensure_overlays()
        found = .false.
        if (show_paren_enabled_p() .and. associated(show_paren_data_function)) call show_paren_data_function(found, data)
        if (.not. found) then
            ! If show-paren-mode is nil in this buffer or if not at a paren
            ! that has a match, turn off any previous paren highlighting.
            call delete_overlay(overlay)
            call delete_overlay(overlay_1)
            call set_last_pos(point())
            return
        end if
        ! Found something to highlight.
        highlight_expression = show_paren_style == SHOW_PAREN_EXPRESSION
        if (.not. highlight_expression .and. data%there_beg /= 0 .and. show_paren_style == SHOW_PAREN_MIXED) then
            if (data%there_beg < data%here_beg) then
                closest = data%there_end - 1
            else
                closest = data%there_beg + 1
            end if
            highlight_expression = .not. pos_visible_in_window_p(closest, selected_window())
        end if
        if (data%mismatch) then
            if (show_paren_not_in_comments_or_strings == SHOW_PAREN_COMMENTS_STRINGS_ON_MISMATCH) then
                if (in_string_or_comment(data%here_beg)) return
            end if
            if (show_paren_ring_bell_on_mismatch) call ding()
            face = "show_paren_mismatch"
        else if (highlight_expression) then
            face = "show_paren_match_expression"
        else
            face = "show_paren_match"
        end if
        if (show_paren_not_in_comments_or_strings == SHOW_PAREN_COMMENTS_STRINGS_ALL) then
            if (in_string_or_comment(data%here_beg)) return
        end if
        ! If matching backwards, highlight the closeparen before point as
        ! well as its matching open.  If matching forward, and the
        ! openparen is unbalanced, highlight the paren at point to
        ! indicate misbalance.  Otherwise, turn off any such highlighting.
        if (data%here_beg == 0 .or. (.not. show_paren_highlight_openparen .and. data%here_end > point() .and. &
                                     data%here_beg <= point() .and. data%there_beg /= 0)) then
            call delete_overlay(overlay_1)
        else
            call move_overlay(overlay_1, data%here_beg, data%here_end, st)
            ! Always set the overlay face, since it varies.
            call overlay_put(overlay_1, "priority", make_fixnum(int(show_paren_priority, int64)), st)
            call overlay_put(overlay_1, "face", sym(face), st)
        end if
        ! Turn on highlighting for the matching paren, if found.  If it's
        ! an unmatched paren, turn off any such highlighting.
        if (data%there_beg == 0) then
            call delete_overlay(overlay)
            return
        end if
        if (highlight_expression) then
            if (data%there_beg < data%here_beg) then
                call move_overlay(overlay, data%here_end, data%there_beg, st)
            else
                call move_overlay(overlay, data%here_beg, data%there_end, st)
            end if
        else
            call move_overlay(overlay, data%there_beg, data%there_end, st)
        end if
        ! If `show_paren_context_when_offscreen' is true and point is at a
        ! closing paren, show the context around the opening paren.
        openparen = min(data%here_beg, data%there_beg)
        if (show_paren_context_when_offscreen .and. last_pos() /= point() .and. data%there_beg < data%here_beg) then
            if (.not. pos_visible_in_window_p(openparen, selected_window())) then
                context = blink_paren_open_paren_line_string(openparen)
                call minibuffer_message("Matches "//context)
            end if
        end if
        call set_last_pos(point())
        ! Always set the overlay face, since it varies.
        call overlay_put(overlay, "priority", make_fixnum(int(show_paren_priority, int64)), st)
        call overlay_put(overlay, "face", sym(face), st)
    end subroutine show_paren_function
end module paren_fm
