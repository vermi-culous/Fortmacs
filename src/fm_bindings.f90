! fm_bindings.f90 -- the default global key bindings.
! Ported from GNU Emacs 31.1 lisp/bindings.el, with the global keymap
! tree from lisp/subr.el and the self-inserting range from
! lisp/international/mule-conf.el.
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

!> `init_bindings` registers the ported commands and builds global-map,
!> esc-map, ctl-x-map and function-key-map with the bindings upstream
!> gives them, as far as their commands exist.  Bindings to commands not
!> ported yet are left out rather than failing with void-function.
!> Until `move-beginning-of-line` and `move-end-of-line` are ported,
!> <home> and <end> use `beginning-of-line` and `end-of-line`.
module fm_bindings
    use, intrinsic :: iso_fortran_env, only: int64
    use fm_error, only: status_type
    use fm_handle, only: handle_type
    use fm_lisp, only: value_type, make_fixnum, make_keymap_ref, list
    use fm_lread, only: sym
    use fm_character, only: MAX_CHAR, CHAR_CTL, CHAR_META
    use fm_keymap, only: make_keymap, make_sparse_keymap, define_key, &
                         define_key_range, use_global_map, set_keymap_parent, &
                         key_parse, keymap_live_p, free_keymap
    use fm_callint, only: defcommand, current_prefix_arg, prefix_numeric_value
    use fm_keyboard, only: local_function_key_map
    use fm_cmds, only: syms_of_cmds, forward_char, backward_char
    use fm_simple, only: syms_of_simple
    use fm_files, only: syms_of_files
    implicit none(type, external)
    private

    public :: init_bindings

    integer(int64), parameter :: M_CTL = int(CHAR_CTL, int64), &
                                 M_META = int(CHAR_META, int64)

    !> subr.el:global-map, esc-map, ctl-x-map; keymap.c:Vfunction_key_map.
    type(handle_type), public :: global_map, esc_map, ctl_x_map, &
                                 function_key_map

contains

    !> Port of bindings.el:right-char (left-to-right paragraphs only).
    subroutine cmd_right_char(status)
        type(status_type), intent(out) :: status

        call forward_char(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_right_char

    !> Port of bindings.el:left-char (left-to-right paragraphs only).
    subroutine cmd_left_char(status)
        type(status_type), intent(out) :: status

        call backward_char(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_left_char

    !> Bind `keys` (kbd syntax) to command `cmd` in `map`.
    subroutine bind(map, keys, cmd)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call key_parse(keys, key, status)
        call define_key(map, key, sym(cmd), status)
    end subroutine bind

    !> Bind the single event `c` to command `cmd` in `map`.
    subroutine bind_char(map, c, cmd)
        type(handle_type), intent(in) :: map
        integer(int64), intent(in) :: c
        character(len=*), intent(in) :: cmd
        type(value_type) :: key(1)
        type(status_type) :: status

        key(1) = make_fixnum(c)
        call define_key(map, key, sym(cmd), status)
    end subroutine bind_char

    !> In `map`, translate `from` (kbd syntax) to the event `to`.
    subroutine translate(map, from, to)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: from
        type(value_type), intent(in) :: to
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call key_parse(from, key, status)
        call define_key(map, key, list(to), status)
    end subroutine translate

    !> Register the commands and install the default keymaps.  Safe to
    !> call again (the maps are rebuilt).
    subroutine init_bindings()
        type(status_type) :: status
        type(value_type) :: key(1)
        integer(int64) :: c

        ! Rebuilding: drop the old maps.
        if (keymap_live_p(global_map)) call free_keymap(global_map, status)
        if (keymap_live_p(esc_map)) call free_keymap(esc_map, status)
        if (keymap_live_p(ctl_x_map)) call free_keymap(ctl_x_map, status)
        if (keymap_live_p(function_key_map)) call free_keymap(function_key_map, status)

        call syms_of_cmds()
        call syms_of_simple()
        call defcommand("right-char", cmd_right_char)
        call defcommand("left-char", cmd_left_char)

        ! subr.el: esc-map, ctl-x-map, global-map.
        call make_keymap(esc_map)
        call make_keymap(ctl_x_map)
        call make_keymap(global_map)
        key(1) = make_fixnum(27_int64)
        call define_key(global_map, key, make_keymap_ref(esc_map), status)
        key(1) = make_fixnum(24_int64)
        call define_key(global_map, key, make_keymap_ref(ctl_x_map), status)
        call bind_char(global_map, 9_int64, "self-insert-command")
        do c = 32, 126
            call bind_char(global_map, c, "self-insert-command")
        end do
        ! mule-conf.el: all non-ASCII characters self-insert (this covers
        ! subr.el's 0240..0377 loop too).
        call define_key_range(global_map, 128_int64, int(MAX_CHAR, int64), &
                              sym("self-insert-command"), status)
        call bind(global_map, "C-a", "beginning-of-line")
        call bind(global_map, "C-b", "backward-char")
        call bind(global_map, "C-e", "end-of-line")
        call bind(global_map, "C-f", "forward-char")

        ! bindings.el
        call bind(global_map, "C-g", "keyboard-quit")
        call bind(global_map, "C-m", "newline")
        call bind(global_map, "C-o", "open-line")
        call bind(global_map, "C-q", "quoted-insert")
        call bind(global_map, "C-u", "universal-argument")
        do c = 0, 9
            call bind_char(esc_map, iachar("0") + c, "digit-argument")
            call bind_char(global_map, M_CTL + iachar("0") + c, "digit-argument")
            call bind_char(global_map, M_CTL + M_META + iachar("0") + c, "digit-argument")
        end do
        call bind(esc_map, "-", "negative-argument")
        call bind_char(global_map, M_CTL + iachar("-"), "negative-argument")
        call bind_char(global_map, M_CTL + M_META + iachar("-"), "negative-argument")
        call bind(global_map, "DEL", "delete-backward-char")
        call bind(global_map, "C-d", "delete-char")
        call bind(global_map, "<home>", "beginning-of-line")
        call bind(global_map, "<end>", "end-of-line")
        call bind(global_map, "<left>", "left-char")
        call bind(global_map, "<right>", "right-char")
        call bind(global_map, "<deletechar>", "delete-forward-char")
        call use_global_map(global_map, status)
        call syms_of_files(ctl_x_map)

        ! bindings.el: function-key-map, the parent of every terminal's
        ! local-function-key-map.
        call make_sparse_keymap(function_key_map)
        call translate(function_key_map, "<backspace>", make_fixnum(127_int64))
        call translate(function_key_map, "<delete>", make_fixnum(127_int64))
        call translate(function_key_map, "<kp-delete>", make_fixnum(127_int64))
        if (.not. keymap_live_p(local_function_key_map)) then
            call make_sparse_keymap(local_function_key_map)
        end if
        call set_keymap_parent(local_function_key_map, function_key_map, status)
    end subroutine init_bindings

end module fm_bindings
