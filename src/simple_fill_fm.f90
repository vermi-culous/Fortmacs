! simple_fill_fm.f90 -- auto-fill and fill-column, a submodule of simple_fm.
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

!> Auto Fill mode (`do_auto_fill', `internal_auto_fill',
!> `auto_fill_mode'), `default_indent_new_line' (M-j) and
!> `set_fill_column' (C-x f): a submodule because they use fill_fm,
!> which uses simple_fm.
submodule(simple_fm) simple_fill_fm
    use commands_fm, only: commands
    use buffer_fm, only: fill_column, auto_fill_function
    use cmds_fm, only: internal_auto_fill_hook, end_of_line, post_self_insert_hook
    use eval_fm, only: hook_type
    use indent_fm, only: current_column, move_to_column, indent_to_left_margin, indent_according_to_mode
    use editfns_fm, only: point_marker
    use subr_fm, only: read_number
    use fill_fm, only: current_justification, current_fill_column, fill_context_prefix, &
                       fill_move_to_break_point, justify_current_line, unjustify_current_line, string_width, &
                       adaptive_fill_mode, fill_indent_according_to_mode, JUSTIFY_NONE, JUSTIFY_LEFT, &
                       JUSTIFY_FULL, JUSTIFY_CENTER, JUSTIFY_RIGHT
    use paragraphs_fm, only: forward_paragraph, page_delimiter, init_paragraph_variables
    use indent_fm, only: current_left_margin, move_to_left_margin, indent_to
    use search_fm, only: string_match
    use insdel_fm, only: insert_before_markers
    use modes_fm, only: minor_modes
    implicit none(type, external)

    ! The state newline_postproc works from: where point was before the
    ! newline, and whether it was at the start of a page.
    integer(int64) :: newline_beforepos = 0
    logical :: newline_was_page_start = .false.

contains

    module procedure do_auto_fill
    integer(int64) :: fc, opoint, after_prefix, fill_point, prev_column, col, moved, from, to, left
    integer :: justify
    character(len=:), allocatable :: prefix, saved_prefix
    logical :: give_up, have_prefix, have_saved, matched, no_good, at_fill_point
    type(status_type) :: st
    type(handle_type) :: m
    status = status_type()
    ! Returns t if it really did any work.
    auto_fill_result = .false.
    justify = current_justification()
    if (justify == JUSTIFY_NONE) return
    fc = current_fill_column()
    if (justify == JUSTIFY_LEFT .and. current_column() <= fc) return
    if (allocated(auto_fill_inhibit_regexp%value)) then
        opoint = point()
        call beginning_of_line()
        matched = looking_at(auto_fill_inhibit_regexp%value, st)
        call goto_char(opoint)
        if (matched) return
    end if
    ! (let ((fill-prefix fill-prefix)) ...)
    have_saved = allocated(fill_prefix%value)
    if (have_saved) saved_prefix = fill_prefix%value
    if (justify == JUSTIFY_FULL .or. justify == JUSTIFY_CENTER .or. justify == JUSTIFY_RIGHT) then
        opoint = point()
        call unjustify_current_line(st)
        call goto_char(min(opoint, point_max()))
    end if
    ! Choose a fill-prefix automatically.
    have_prefix = allocated(fill_prefix%value)
    if (have_prefix) have_prefix = len(fill_prefix%value) > 0
    if (adaptive_fill_mode%value .and. .not. have_prefix) then
        opoint = point()
        left = forward_paragraph(-1_int64)
        from = point()
        call goto_char(opoint)
        left = forward_paragraph(1_int64)
        to = point()
        call goto_char(opoint)
        call fill_context_prefix(from, to, prefix, have_prefix)
        if (have_prefix .and. len(prefix) > 0) then
            ! Use auto-indentation rather than a guessed empty prefix.
            if (.not. (fill_indent_according_to_mode .and. &
                       string_match("\`[ "//TAB//"]*\'", prefix, st) > 0)) fill_prefix%value = prefix
        end if
    end if
    give_up = .false.
    do while (.not. give_up .and. current_column() > fc)
        ! Determine where to split the line.
        opoint = point()
        call beginning_of_line()
        ! Don't split earlier in the line than the length of the fill
        ! prefix, since the resulting line would be longer.
        if (allocated(fill_prefix%value)) call move_to_column(string_width(fill_prefix%value), col)
        after_prefix = point()
        call move_to_column(fc + 1, col)
        call fill_move_to_break_point(after_prefix)
        fill_point = point()
        call goto_char(opoint)
        ! See whether the place we found is any good.
        call goto_char(fill_point)
        no_good = bolp()
        if (.not. no_good) then
            ! There is no use breaking at end of line.
            moved = skip_chars_forward(" ", st)
            no_good = eolp()
        end if
        call goto_char(opoint)
        if (no_good) then
            ! No good place to break => stop trying.
            give_up = .true.
        else
            ! Ok, we have a useful place to break the line.  Do it.
            prev_column = current_column()
            ! If point is at the fill-point, do not `save_excursion'.
            ! Otherwise, if a comment prefix or fill-prefix is inserted,
            ! point will end up before it rather than after it.
            moved = skip_chars_backward(" "//TAB, st)
            at_fill_point = point() == fill_point
            call goto_char(opoint)
            if (at_fill_point) then
                call default_indent_new_line(status, .true.)
            else
                call save_excursion_newline_at(fill_point, status)
            end if
            if (.not. status%ok()) exit
            ! Now do justification, if required
            if (justify /= JUSTIFY_LEFT) then
                ! (save-excursion)
                call copy_marker_at(point(), m, .false., st)
                call end_of_line(0_int64)
                call justify_current_line(st, justify, .false., .true.)
                call marker_position(m, opoint, st)
                call goto_char(opoint)
                call free_marker(m, st)
            end if
            ! If making the new line didn't reduce the hpos of the end of
            ! the line, then give up now; trying again will not help.
            if (current_column() >= prev_column) give_up = .true.
        end if
    end do
    ! Justify last line.
    if (status%ok()) call justify_current_line(status, justify, .true., .true.)
    if (status%ok()) auto_fill_result = .true.
    if (allocated(fill_prefix%value)) deallocate (fill_prefix%value)
    if (have_saved) fill_prefix = saved_prefix
    end procedure do_auto_fill

    ! (save-excursion (goto-char FILL-POINT) (default-indent-new-line t)),
    ! point kept with a marker.
    subroutine save_excursion_newline_at(fill_point, status)
        integer(int64), intent(in) :: fill_point
        type(status_type), intent(out) :: status
        type(handle_type) :: m
        type(status_type) :: st
        integer(int64) :: pos
        call copy_marker_at(point(), m, .false., st)
        call goto_char(fill_point)
        call default_indent_new_line(status, .true.)
        call marker_position(m, pos, st)
        call goto_char(pos)
        call free_marker(m, st)
    end subroutine save_excursion_newline_at

    module procedure default_indent_new_line
    integer(int64) :: opoint
    logical :: noindent
    status = status_type()
    ! Insert the newline before removing empty space so that markers get
    ! preserved better.
    if (present(soft)) then
        if (soft) then
            call insert(LF, status)
        else
            call newline(status, 1_int64)
        end if
    else
        call newline(status, 1_int64)
    end if
    if (.not. status%ok()) return
    opoint = point()
    call goto_char(point() - 1)
    call delete_horizontal_space(.false., status)
    if (.not. status%ok()) return
    call goto_char(point() + 1)
    call delete_horizontal_space(.false., status)
    if (.not. status%ok()) return
    if (allocated(fill_prefix%value) .and. .not. adaptive_fill_mode%value) then
        ! Blindly trust a non-adaptive fill-prefix.
        call indent_to_left_margin(status, noindent)
        if (status%ok()) call insert_before_markers(fill_prefix%value, status)
    else if (allocated(fill_prefix%value)) then
        ! If there's an adaptive prefix, use it unless we're inside a
        ! comment and the prefix is not a comment starter.
        call indent_to_left_margin(status, noindent)
        if (status%ok()) call insert(fill_prefix%value, status)
    else
        ! If we're not inside a comment, just try to indent.
        call indent_according_to_mode(status)
    end if
    end procedure default_indent_new_line

    module procedure newline
    logical :: saved_inhibit, is_interactive
    integer(int64) :: n
    type(status_type) :: st
    type(value_type) :: saved_event
    type(hook_type) :: saved_hook
    call barf_if_buffer_read_only(status)
    if (.not. status%ok()) return
    if (present(arg)) then
        if (arg < 0) then
            call set_error(status, FM_GENERIC_ERROR, "Repetition argument has to be non-negative")
            return
        end if
    end if
    is_interactive = .false.
    if (present(interactive)) is_interactive = interactive
    ! Call self-insert so that auto-fill, abbrev expansion etc. happen.
    ! Set last-command-event to tell self-insert what to insert.
    call init_paragraph_variables()
    newline_was_page_start = .false.
    if (bolp()) newline_was_page_start = looking_at(page_delimiter, st)
    newline_beforepos = point()
    saved_event = last_command_event
    last_command_event = make_fixnum(10_int64)
    ! Don't auto-fill if we have a prefix argument.
    saved_inhibit = inhibit_auto_fill
    if (present(arg)) inhibit_auto_fill = .true.
    n = 1
    if (present(arg)) n = arg
    if (.not. is_interactive) then
        ! For non-interactive uses, the rest is the only function of
        ! post-self-insert-hook (it runs last, after auto-fill).
        saved_hook = post_self_insert_hook
        call post_self_insert_hook%clear()
        call self_insert_command(n, 10_int64, status)
        post_self_insert_hook = saved_hook
        if (status%ok()) call newline_postproc(status)
    else
        ! Do the rest in post-self-insert-hook, because we want to do it
        ! *before* other functions on that hook.
        call post_self_insert_hook%add(newline_postproc, local=.true.)
        call self_insert_command(n, 10_int64, status)
        call post_self_insert_hook%remove(newline_postproc, local=.true.)
    end if
    inhibit_auto_fill = saved_inhibit
    last_command_event = saved_event
    end procedure newline

    !> The rest of `newline', run first in `post_self_insert_hook':
    !> delete the left margin from a line the newline left blank, and
    !> indent the new line.  The newline-postproc of simple.el:newline
    !> (`use_hard_newlines' is not ported).
    recursive subroutine newline_postproc(status)
        type(status_type), intent(out) :: status
        logical :: blank
        integer(int64) :: pos
        type(handle_type) :: m
        type(status_type) :: st
        status = status_type()
        call post_self_insert_hook%remove(newline_postproc, local=.true.)
        ! If the newline leaves the previous line blank, and we have a left
        ! margin, delete that from the blank line.
        call point_marker(m)
        call goto_char(newline_beforepos)
        call beginning_of_line()
        blank = looking_at("[ "//TAB//"]+$", st)
        if (blank) blank = current_left_margin() > 0
        if (blank) call delete_region(point(), line_end_position(), status)
        call marker_position(m, pos, st)
        call goto_char(pos)
        call free_marker(m, st)
        if (.not. status%ok()) return
        ! Indent the line after the newline, except in one case: when we
        ! added the newline at the beginning of a line that starts a page.
        if (.not. newline_was_page_start) call move_to_left_margin(status, force=.true.)
    end subroutine newline_postproc

    module procedure open_line
    logical :: do_fill_prefix, do_left_margin
    integer(int64) :: k, pos, col
    type(handle_type) :: loc
    type(status_type) :: st
    do_fill_prefix = allocated(fill_prefix%value) .and. bolp()
    do_left_margin = bolp()
    if (do_left_margin) do_left_margin = current_left_margin() > 0
    call point_marker(loc)
    call newline(status, n)
    if (status%ok()) then
        call marker_position(loc, pos, st)
        call goto_char(pos)
        k = n
        do while (k > 0)
            if (bolp()) then
                if (do_left_margin) col = indent_to(current_left_margin(), status)
                if (do_fill_prefix .and. status%ok()) call insert(fill_prefix%value, status)
                if (.not. status%ok()) exit
            end if
            if (forward_line(1_int64) /= 0) continue
            k = k - 1
        end do
        call marker_position(loc, pos, st)
        call goto_char(pos)
        ! Necessary in case a margin or prefix was inserted.
        if (status%ok()) call end_of_line()
    end if
    call free_marker(loc, st)
    end procedure open_line

    module procedure internal_auto_fill
    status = status_type()
    filled = .false.
    if (inhibit_auto_fill) return
    if (associated(auto_fill_function%value)) then
        auto_fill_result = .false.
        call auto_fill_function%value(status)
        filled = auto_fill_result
    end if
    end procedure internal_auto_fill

    module procedure auto_fill_mode
    call minor_modes%auto_fill_mode%command(status)
    end procedure auto_fill_mode

    module procedure turn_on_auto_fill
    call minor_modes%auto_fill_mode%enable(status)
    end procedure turn_on_auto_fill

    module procedure turn_off_auto_fill
    call minor_modes%auto_fill_mode%disable(status)
    end procedure turn_off_auto_fill

    module procedure set_fill_column
    integer(int64) :: n
    character(len=24) :: a, b
    status = status_type()
    if (consp(arg)) then
        n = current_column()
    else if (fixnump(arg)) then
        n = xfixnum(arg)
    else if (nilp(arg)) then
        ! We used to use current-column silently, but C-x f is too easily
        ! typed as a typo for C-x C-f, so we turned it into an error and
        ! now an interactive prompt.
        write (a, "(i0)") fill_column%value
        call read_number("Change fill-column from "//trim(a)//" to: ", n, status, current_column())
        if (.not. status%ok()) return
    else
        ! Disallow missing argument; it's probably a typo for C-x C-f.
        call set_error(status, FM_GENERIC_ERROR, "set_fill_column requires an explicit argument")
        return
    end if
    write (a, "(i0)") n
    write (b, "(i0)") fill_column%value
    call message("Fill column set to "//trim(a)//" (was "//trim(b)//")")
    fill_column = n
    end procedure set_fill_column

    subroutine cmd_set_fill_column(status)
        type(status_type), intent(out) :: status
        call set_fill_column(current_prefix_arg, status)
    end subroutine cmd_set_fill_column

    subroutine cmd_default_indent_new_line(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call default_indent_new_line(status)
    end subroutine cmd_default_indent_new_line

    !> auto-fill-mode's :variable setter: set `auto_fill_function'
    !> (buffer-locally) to `normal_auto_fill_function' or nil.
    subroutine auto_fill_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        status = status_type()
        call auto_fill_function%make_local()
        if (.not. enable) then
            auto_fill_function%value => null()
        else if (associated(normal_auto_fill_function)) then
            auto_fill_function%value => normal_auto_fill_function
        else
            auto_fill_function%value => do_auto_fill
        end if
    end subroutine auto_fill_body

    module procedure syms_of_simple_fill
    call minor_modes%auto_fill_mode%define("auto_fill_mode", " Fill", variable="auto_fill_function", hook=auto_fill_mode_hook, &
                                           body=auto_fill_body)
    internal_auto_fill_hook => internal_auto_fill
    commands%auto_fill_mode%run => auto_fill_mode
    ! turn-on-auto-fill, turn-off-auto-fill and do-auto-fill are
    ! functions, not commands.
    commands%turn_on_auto_fill%run => turn_on_auto_fill
    commands%turn_on_auto_fill%interactive = .false.
    commands%turn_off_auto_fill%run => turn_off_auto_fill
    commands%turn_off_auto_fill%interactive = .false.
    commands%do_auto_fill%run => do_auto_fill
    commands%do_auto_fill%interactive = .false.
    commands%set_fill_column%run => cmd_set_fill_column
    commands%default_indent_new_line%run => cmd_default_indent_new_line
    end procedure syms_of_simple_fill
end submodule simple_fill_fm
