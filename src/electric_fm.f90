! electric_fm.f90 -- electric keys: automatic reindentation.
! Ported from GNU Emacs 31.1 lisp/electric.el.
!
! Copyright (C) 1985-1986, 1995, 2001-2026 Free Software Foundation,
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

!> Electric Indent mode: self-inserting one of `electric_indent_chars'
!> (by default a newline) reindents the line, from
!> `post_self_insert_hook'.  The mode is on by default, as upstream
!> (`:init-value t'): bindings_fm turns it on at startup.  C-j is
!> `electric_newline_and_maybe_indent'.
!>
!> Deferred: `electric_indent_functions' (run-hook-with-args-until-success
!> is not ported), `electric_indent_actions' (advice on `yank' and
!> `before_save_hook'), buffer-local values of `electric_indent_chars'
!> (one global value), `electric_layout_mode' and
!> `electric_layout_local_mode' with `electric_layout_rules',
!> `electric_quote_mode' and `electric_quote_local_mode' with their
!> variables, and the electric command loop and pop-up window
!> (`Electric_command_loop', `Electric_pop_up_window').
module electric_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, fixnump, xfixnum
    use lread_fm, only: sym
    use data_fm, only: logical_setting, default_value, set_default, kill_local_variable, buffer_local_value
    use commands_fm, only: commands
    use eval_fm, only: hook_type
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use buffer_fm, only: buffers, cur, buffer_list, resolve_buffer, barf_if_buffer_read_only
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use editfns_fm, only: point, goto_char, bolp, eolp, char_before, line_beginning_position
    use syntax_fm, only: skip_chars_backward
    use cmds_fm, only: post_self_insert_hook
    use keyboard_fm, only: last_command_event
    use indent_fm, only: indent_according_to_mode, indent_line_function, indent_line_function_name
    use simple_fm, only: newline, newline_and_indent, delete_horizontal_space
    use keymap_fm, only: define_key, key_parse
    use modes_fm, only: minor_modes
    implicit none(type, external)
    private

    public :: syms_of_electric, electric_indent_mode, electric_indent_local_mode, electric_after_char_pos, &
              electric_indent_can_reindent_p, electric_indent_post_self_insert_function, &
              electric_indent_just_newline, electric_newline_and_maybe_indent

    !> Characters that should cause automatic reindentation.  Port of
    !> electric.el electric-indent-chars (one global value; `syms_of_electric'
    !> gives it the newline unless the configuration set it).
    integer, allocatable, public :: electric_indent_chars(:)

    !> If true, reindentation is not appropriate for this buffer.
    !> This should be set by major modes such as `python_mode' since
    !> Python does not lend itself to fully automatic indentation.  Port of
    !> electric.el electric-indent-inhibit (buffer-local).
    type(logical_setting), public :: electric_indent_inhibit = &
                                     logical_setting(name="electric_indent_inhibit", value=.false.)

    !> List of indent functions that can't reindent.
    !> If `indent_line_function' is one of those, then `electric_indent_mode' will
    !> not try to reindent lines.  It is normally better to make the major
    !> mode set `electric_indent_inhibit', but this can be used as a workaround.
    !> Port of electric.el electric-indent-functions-without-reindent (by
    !> name).
    character(len=40), allocatable, public :: electric_indent_functions_without_reindent(:)

    !> Hook run after entering or leaving `electric_indent_mode'.  Port of
    !> the electric-indent-mode-hook define-minor-mode defines.
    type(hook_type), target, public :: electric_indent_mode_hook = hook_type(name="electric_indent_mode_hook")

    !> Hook run after entering or leaving `electric_indent_local_mode'.
    !> Port of the electric-indent-local-mode-hook define-minor-mode
    !> defines.
    type(hook_type), target, public :: electric_indent_local_mode_hook = &
                                       hook_type(name="electric_indent_local_mode_hook")

contains

    !> Define Electric Indent mode and its local mode, and bind C-j in
    !> GLOBAL_MAP (as loading electric.el does).
    subroutine syms_of_electric(global_map)
        type(handle_type), intent(in) :: global_map
        type(value_type), allocatable :: key(:)
        type(status_type) :: st
        call minor_modes%electric_indent_mode%define("electric_indent_mode", global=.true., &
                                                     body=electric_indent_mode_body, hook=electric_indent_mode_hook)
        commands%electric_indent_mode%run => electric_indent_mode
        call minor_modes%electric_indent_local_mode%define("electric_indent_local_mode", &
                                                           body=electric_indent_local_mode_body, &
                                                           hook=electric_indent_local_mode_hook, &
                                                           getter=electric_indent_local_mode_p)
        commands%electric_indent_local_mode%run => electric_indent_local_mode
        commands%electric_indent_just_newline%run => electric_indent_just_newline
        commands%electric_newline_and_maybe_indent%run => electric_newline_and_maybe_indent
        if (.not. allocated(electric_indent_chars)) electric_indent_chars = [10]
        if (.not. allocated(electric_indent_functions_without_reindent)) then
            electric_indent_functions_without_reindent = [character(len=40) :: "indent_relative", &
                                                          "indent_to_left_margin", "indent_relative_maybe", &
                                                          "indent_relative_first_indent_point", "yaml_indent_line"]
        end if
        call key_parse("C-j", key, st)
        call define_key(global_map, key, sym("electric_newline_and_maybe_indent"), st)
    end subroutine syms_of_electric

    !> The character `last_command_event' is, or -1.
    integer function event_char()
        event_char = -1
        if (fixnump(last_command_event)) event_char = int(xfixnum(last_command_event))
    end function event_char

    !> Return the position after the char we just inserted.
    !> Returns 0 (upstream's nil) when we can't find this char.  Port of
    !> electric.el:electric--after-char-pos.
    integer(int64) function electric_after_char_pos() result(pos)
        integer(int64) :: opoint, moved
        integer :: c
        logical :: found, ok
        type(status_type) :: st
        opoint = point()
        pos = opoint
        call char_before(c, found)
        ok = found .and. c == event_char()
        if (.not. ok) then
            moved = skip_chars_backward(" "//achar(9), st)
            pos = point()
            call char_before(c, found)
            ok = found .and. c == event_char()
            if (.not. ok) then
                moved = skip_chars_backward(" "//achar(10)//achar(9), st)
                pos = point()
                call char_before(c, found)
                ok = found .and. c == event_char()
            end if
            call goto_char(opoint)
        end if
        if (.not. ok) pos = 0
    end function electric_after_char_pos

    !> Return true if `electric_indent_mode' can performs reindentation.
    !> Port of electric.el:electric-indent-can-reindent-p.
    logical function electric_indent_can_reindent_p() result(can)
        character(len=:), allocatable :: name
        integer :: k
        can = .not. electric_indent_inhibit%value
        if (.not. can) return
        name = indent_line_function_name(indent_line_function%value)
        do k = 1, size(electric_indent_functions_without_reindent)
            if (name == trim(electric_indent_functions_without_reindent(k))) can = .false.
        end do
    end function electric_indent_can_reindent_p

    !> Function that `electric_indent_mode' adds to `post_self_insert_hook'.
    !> This indents if the hook `electric_indent_functions' returns true,
    !> or if a member of `electric_indent_chars' was typed; but not in a string
    !> or comment.  Port of electric.el:electric-indent-post-self-insert-function
    !> (no `electric_indent_functions').
    subroutine electric_indent_post_self_insert_function(status)
        type(status_type), intent(out) :: status
        integer(int64) :: pos, opoint, moved, bpos
        integer :: ev
        logical :: go, at_newline
        type(handle_type) :: before, saved
        type(status_type) :: st
        status = status_type()
        if (.not. minor_modes%electric_indent_mode%state%value) return
        ev = event_char()
        ! Don't reindent while inserting spaces at beginning of line.
        if (ev == 32 .or. ev == 9) then
            opoint = point()
            moved = skip_chars_backward(" "//achar(9), st)
            go = .not. bolp()
            call goto_char(opoint)
            if (.not. go) return
        end if
        pos = electric_after_char_pos()
        if (pos == 0) return
        if (.not. any(electric_indent_chars == ev)) return
        ! If we error during indent, silently give up since this is an
        ! automatic action that the user didn't explicitly request.
        ! For newline, we want to reindent both lines and basically behave
        ! like reindent-then-newline-and-indent (whose code we hence
        ! copied).
        at_newline = pos <= line_beginning_position()
        if (at_newline) then
            call copy_marker_at(pos - 1, before, .true., st)
            call copy_marker_at(point(), saved, .false., st)
            if (electric_indent_can_reindent_p()) then
                ! Don't reindent the previous line if the indentation
                ! function is not a real one.
                call marker_position(before, bpos, st)
                call goto_char(bpos)
                call indent_according_to_mode(st)
                if (.not. st%ok()) then
                    call restore()
                    return
                end if
            end if
            ! The goal here will be to remove the trailing whitespace after
            ! reindentation of the previous line because that may have
            ! (re)introduced it.
            call marker_position(before, bpos, st)
            call goto_char(bpos)
            ! We were at EOL in marker `before' before the call to
            ! `indent_according_to_mode' but after we may not be
            ! (Bug#15767).
            if (eolp()) call delete_horizontal_space(.true., status)
            call restore()
            if (.not. status%ok()) return
        end if
        if (electric_indent_inhibit%value .and. .not. at_newline) return
        call indent_according_to_mode(st)
    contains
        !> End the save-excursion and free the markers.
        subroutine restore()
            integer(int64) :: p
            call marker_position(saved, p, st)
            call goto_char(p)
            call free_marker(saved, st)
            call free_marker(before, st)
        end subroutine restore
    end subroutine electric_indent_post_self_insert_function

    !> Insert just a newline, without any auto-indentation.  Port of
    !> electric.el:electric-indent-just-newline ("*P").
    subroutine electric_indent_just_newline(status)
        type(status_type), intent(out) :: status
        logical :: saved
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        saved = minor_modes%electric_indent_mode%state%value
        minor_modes%electric_indent_mode%state%value = .false.
        if (nilp(current_prefix_arg)) then
            call newline(status, interactive=.true.)
        else
            call newline(status, prefix_numeric_value(current_prefix_arg), interactive=.true.)
        end if
        minor_modes%electric_indent_mode%state%value = saved
    end subroutine electric_indent_just_newline

    !> Insert a newline.
    !> If `electric_indent_mode' is enabled, that's that, but if it
    !> is *disabled* then additionally indent according to major mode.
    !> Indentation is done using the value of `indent_line_function'.
    !> In programming language modes, this is the same as TAB.
    !> In some text modes, where TAB inserts a tab, this command indents to the
    !> column specified by the function `current_left_margin'.  Port of
    !> electric.el:electric-newline-and-maybe-indent.
    subroutine electric_newline_and_maybe_indent(status)
        type(status_type), intent(out) :: status
        type(value_type) :: saved
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        if (minor_modes%electric_indent_mode%state%value) then
            saved = current_prefix_arg
            current_prefix_arg = nil()
            call electric_indent_just_newline(status)
            current_prefix_arg = saved
        else
            call newline_and_indent(status)
        end if
    end subroutine electric_newline_and_maybe_indent

    !> Toggle on-the-fly reindentation of text lines (Electric Indent mode).
    !>
    !> When enabled, this reindents whenever the hook `electric_indent_functions'
    !> returns true, or if you insert one of the "electric characters".
    !> The electric characters normally include the newline, but can
    !> also include other characters as needed by the major mode; see
    !> `electric_indent_chars' for the actual list.
    !>
    !> By "reindent" we mean remove any existing indentation, and then
    !> indent the line according to context and rules of the major mode.
    !>
    !> This is a global minor mode.  To toggle the mode in a single buffer,
    !> use `electric_indent_local_mode'.  Port of
    !> electric.el:electric-indent-mode.
    subroutine electric_indent_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%electric_indent_mode%command(status)
    end subroutine electric_indent_mode

    !> The body of Electric Indent mode.
    subroutine electric_indent_mode_body(on, status)
        logical, intent(in) :: on
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: all(:)
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: b
        integer :: k
        logical :: found
        status = status_type()
        if (on) continue
        if (.not. minor_modes%electric_indent_mode%state%value) then
            found = .false.
            all = buffer_list()
            do k = 1, size(all)
                call resolve_buffer(all(k), b, st)
                if (.not. st%ok()) cycle
                call buffer_local_value(sym("electric_indent_mode"), b, v, st)
                if (st%ok() .and. .not. nilp(v)) then
                    found = .true.
                    exit
                end if
            end do
            if (.not. found) call post_self_insert_hook%remove(electric_indent_post_self_insert_function)
        else
            call post_self_insert_hook%add(electric_indent_post_self_insert_function, depth=60)
        end if
    end subroutine electric_indent_mode_body

    !> Toggle `electric_indent_mode' only in this buffer.  Port of
    !> electric.el:electric-indent-local-mode.
    subroutine electric_indent_local_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%electric_indent_local_mode%command(status)
    end subroutine electric_indent_local_mode

    !> The state of Electric Indent Local mode: `electric_indent_mode' in
    !> the current buffer.
    logical function electric_indent_local_mode_p() result(on)
        on = minor_modes%electric_indent_mode%state%value
    end function electric_indent_local_mode_p

    !> The body of Electric Indent Local mode with its new state VAL: the
    !> setter (setq-local electric-indent-mode VAL), then the body.
    subroutine electric_indent_local_mode_body(val, status)
        logical, intent(in) :: val
        type(status_type), intent(out) :: status
        type(value_type) :: default
        type(status_type) :: st
        status = status_type()
        call minor_modes%electric_indent_mode%state%set_local(val)
        call default_value(sym("electric_indent_mode"), default, st)
        if (minor_modes%electric_indent_mode%state%value .eqv. .not. nilp(default)) then
            call kill_local_variable(sym("electric_indent_mode"), st)
        else if (nilp(default)) then
            ! Locally enabled, but globally disabled.
            ! Setup the hooks.
            call minor_modes%electric_indent_mode%enable(status)
            ! But keep it globally disabled.
            call set_default(sym("electric_indent_mode"), nil(), st)
        end if
    end subroutine electric_indent_local_mode_body
end module electric_fm
