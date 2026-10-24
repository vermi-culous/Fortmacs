! bindings_fm.f90 -- the default global key bindings.
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

!> `init_bindings' registers the ported commands and builds global-map,
!> esc-map, ctl-x-map and function-key-map with the bindings upstream
!> gives them, as far as their commands exist.  Bindings to commands not
!> ported yet are left out rather than failing with void-function.
module bindings_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use commands_fm, only: commands
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_fixnum, make_keymap_ref, list
    use lread_fm, only: sym
    use character_fm, only: MAX_CHAR, CHAR_CTL, CHAR_META
    use keymap_fm, only: make_keymap, make_sparse_keymap, define_key, &
                         define_key_range, use_global_map, set_keymap_parent, &
                         key_parse, keymap_live_p, free_keymap
    use callint_fm, only: current_prefix_arg, prefix_numeric_value, intern_command_names
    use keyboard_fm, only: local_function_key_map, syms_of_macros
    use kmacro_fm, only: syms_of_kmacro, kmacro_keymap
    use cmds_fm, only: syms_of_cmds, forward_char, backward_char
    use simple_fm, only: syms_of_simple, backward_word
    use syntax_fm, only: syms_of_syntax, forward_word
    use paragraphs_fm, only: syms_of_paragraphs
    use lisp_cmds_fm, only: syms_of_lisp_cmds
    use search_fm, only: syms_of_search
    use replace_fm, only: syms_of_replace
    use casefiddle_fm, only: syms_of_casefiddle
    use novice_fm, only: syms_of_novice
    use indent_fm, only: syms_of_indent
    use fill_fm, only: syms_of_fill
    use isearch_fm, only: syms_of_isearch
    use window_fm, only: syms_of_window
    use files_fm, only: syms_of_files
    use help_fm, only: syms_of_help, register_keymap_name
    use isearch_fm, only: isearch_mode_map
    use text_mode_fm, only: text_mode_map
    use prog_mode_fm, only: prog_mode_map
    use simple_fm, only: special_mode_map
    use minibuffer_fm, only: minibuffer_local_completion_map, minibuffer_local_must_match_map, &
                             minibuffer_local_filename_completion_map, minibuffer_local_filename_must_match_map
    use help_fns_fm, only: syms_of_help_fns
    use minibuf_fm, only: init_minibuf, minibuffer_local_map
    use minibuffer_fm, only: syms_of_minibuffer
    use subr_fm, only: syms_of_subr
    use text_mode_fm, only: syms_of_text_mode
    use prog_mode_fm, only: syms_of_prog_mode
    use font_core_fm, only: syms_of_font_core
    use font_lock_fm, only: syms_of_font_lock
    use jit_lock_fm, only: syms_of_jit_lock
    use dired_mode_fm, only: syms_of_dired_mode, dired_mode_map
    use process_fm, only: syms_of_process
    use compile_fm, only: syms_of_compile, compilation_mode_map
    use comint_fm, only: syms_of_comint, comint_mode_map
    use shell_fm, only: syms_of_shell, shell_mode_map
    use rect_fm, only: syms_of_rect
    use register_fm, only: syms_of_register
    implicit none(type, external)
    private

    public :: init_bindings

    integer(int64), parameter :: M_CTL = int(CHAR_CTL, int64), &
                                 M_META = int(CHAR_META, int64)

    !> subr.el:global-map, esc-map, ctl-x-map; keymap.c:Vfunction_key_map;
    !> bindings.el:ctl-x-x-map (keymap for subcommands of `C-x x');
    !> bindings.el:ctl-x-r-map (keymap for subcommands of `C-x r');
    !> subr.el:ctl-x-4-map (keymap for subcommands of `C-x 4').
    type(handle_type), public :: global_map, esc_map, ctl_x_map, &
                                 function_key_map, ctl_x_x_map, ctl_x_r_map, ctl_x_4_map
    !> bindings.el:goto-map, keymap for navigation commands (M-g).
    type(handle_type), public :: goto_map

contains

    !> Move point N characters to the right (to the left if N is negative).
    !> On reaching beginning or end of buffer, stop and signal error.
    !>
    !> In left-to-right text this moves forward in the buffer, like
    !> \[forward_char].  Port of bindings.el:right-char (left-to-right
    !> paragraphs only).
    subroutine cmd_right_char(status)
        type(status_type), intent(out) :: status
        call forward_char(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_right_char

    !> Move point N characters to the left (to the right if N is negative).
    !> On reaching beginning or end of buffer, stop and signal error.
    !>
    !> In left-to-right text this moves backward in the buffer, like
    !> \[backward_char].  Port of bindings.el:left-char (left-to-right
    !> paragraphs only).
    subroutine cmd_left_char(status)
        type(status_type), intent(out) :: status
        call backward_char(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_left_char

    !> Move point N words to the right (to the left if N is negative).
    !>
    !> In left-to-right text this moves forward in the buffer, like
    !> \[forward_word].
    !>
    !> The word boundaries are normally determined by the buffer's syntax
    !> table and character script (according to `char_script_table').
    !>
    !> If an edge of the buffer or a field boundary is reached, point is left there.
    !> Field boundaries are not noticed if `inhibit_field_text_motion' is
    !> true.  Port of bindings.el:right-word (left-to-right paragraphs
    !> only).
    subroutine cmd_right_word(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call forward_word(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_right_word

    !> Move point N words to the left (to the right if N is negative).
    !>
    !> In left-to-right text this moves backward in the buffer, like
    !> \[backward_word].
    !>
    !> The word boundaries are normally determined by the buffer's syntax
    !> table and character script (according to `char_script_table').
    !>
    !> If an edge of the buffer or a field boundary is reached, point is left there.
    !> Field boundaries are not noticed if `inhibit_field_text_motion' is
    !> true.  Port of bindings.el:left-word (left-to-right paragraphs
    !> only).
    subroutine cmd_left_word(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call backward_word(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_left_word

    !> Bind `keys' (kbd syntax) to command `cmd' in `map'.
    subroutine bind(map, keys, cmd)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: status
        call key_parse(keys, key, status)
        call define_key(map, key, sym(cmd), status)
    end subroutine bind

    !> Bind the single event `c' to command `cmd' in `map'.
    subroutine bind_char(map, c, cmd)
        type(handle_type), intent(in) :: map
        integer(int64), intent(in) :: c
        character(len=*), intent(in) :: cmd
        type(value_type) :: key(1)
        type(status_type) :: status
        key(1) = make_fixnum(c)
        call define_key(map, key, sym(cmd), status)
    end subroutine bind_char

    !> In `map', translate `from' (kbd syntax) to the event `to'.
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
        call intern_command_names()
        ! Rebuilding: drop the old maps.
        if (keymap_live_p(global_map)) call free_keymap(global_map, status)
        if (keymap_live_p(esc_map)) call free_keymap(esc_map, status)
        if (keymap_live_p(ctl_x_map)) call free_keymap(ctl_x_map, status)
        if (keymap_live_p(ctl_x_x_map)) call free_keymap(ctl_x_x_map, status)
        if (keymap_live_p(ctl_x_4_map)) call free_keymap(ctl_x_4_map, status)
        if (keymap_live_p(ctl_x_r_map)) call free_keymap(ctl_x_r_map, status)
        if (keymap_live_p(function_key_map)) call free_keymap(function_key_map, status)
        if (keymap_live_p(goto_map)) call free_keymap(goto_map, status)
        call syms_of_cmds()
        call syms_of_simple()
        call syms_of_window()
        commands%right_char%run => cmd_right_char
        commands%left_char%run => cmd_left_char
        commands%right_word%run => cmd_right_word
        commands%left_word%run => cmd_left_word
        call syms_of_syntax()
        call syms_of_paragraphs()
        call syms_of_lisp_cmds()
        call syms_of_search()
        call syms_of_replace()
        call syms_of_casefiddle()
        call syms_of_indent()
        call syms_of_fill()
        call syms_of_novice()
        call syms_of_text_mode()
        call syms_of_prog_mode()
        call syms_of_jit_lock()
        call syms_of_font_core()
        call syms_of_font_lock()
        call syms_of_process()
        call syms_of_comint()
        call syms_of_shell()
        call syms_of_compile()
        call syms_of_rect()
        call syms_of_register()
        call syms_of_macros()
        call syms_of_kmacro()
        ! subr.el: esc-map, ctl-x-map, global-map.
        call make_keymap(esc_map)
        call make_keymap(ctl_x_map)
        call make_keymap(global_map)
        key(1) = make_fixnum(27_int64)
        call define_key(global_map, key, make_keymap_ref(esc_map), status)
        key(1) = make_fixnum(24_int64)
        call define_key(global_map, key, make_keymap_ref(ctl_x_map), status)
        call bind_char(global_map, 9_int64, "self_insert_command")
        do c = 32, 126
            call bind_char(global_map, c, "self_insert_command")
        end do
        ! mule-conf.el: all non-ASCII characters self-insert (this covers
        ! subr.el's 0240..0377 loop too).
        call define_key_range(global_map, 128_int64, int(MAX_CHAR, int64), &
                              sym("self_insert_command"), status)
        call bind(global_map, "C-a", "move_beginning_of_line")
        call bind(global_map, "C-b", "backward_char")
        call bind(global_map, "C-e", "move_end_of_line")
        call bind(global_map, "C-f", "forward_char")
        call bind(global_map, "C-n", "next_line")
        call bind(global_map, "C-p", "previous_line")
        ! bindings.el
        call bind(global_map, "C-g", "keyboard_quit")
        call bind(global_map, "C-m", "newline")
        call bind(global_map, "C-o", "open_line")
        call bind(global_map, "C-q", "quoted_insert")
        call bind(global_map, "C-u", "universal_argument")
        do c = 0, 9
            call bind_char(esc_map, iachar("0") + c, "digit_argument")
            call bind_char(global_map, M_CTL + iachar("0") + c, "digit_argument")
            call bind_char(global_map, M_CTL + M_META + iachar("0") + c, "digit_argument")
        end do
        call bind(esc_map, "-", "negative_argument")
        call bind_char(global_map, M_CTL + iachar("-"), "negative_argument")
        call bind_char(global_map, M_CTL + M_META + iachar("-"), "negative_argument")
        call bind(global_map, "DEL", "delete_backward_char")
        call bind(global_map, "C-d", "delete_char")
        call bind(global_map, "<home>", "move_beginning_of_line")
        call bind(global_map, "<end>", "move_end_of_line")
        call bind(global_map, "<left>", "left_char")
        call bind(global_map, "<right>", "right_char")
        call bind(global_map, "<down>", "next_line")
        call bind(global_map, "<up>", "previous_line")
        call bind(global_map, "<deletechar>", "delete_forward_char")
        call bind(global_map, "<menu>", "execute_extended_command")
        call bind(global_map, "<find>", "search_forward")
        call bind(global_map, "<begin>", "beginning_of_buffer")
        call bind(global_map, "<execute>", "execute_extended_command")
        call bind(global_map, "C-<insert>", "kill_ring_save")
        call bind(global_map, "S-<insert>", "yank")
        call bind(global_map, "C-<insertchar>", "kill_ring_save")
        call bind(global_map, "S-<insertchar>", "yank")
        call bind(global_map, "<copy>", "kill_ring_save")
        call bind(global_map, "<paste>", "yank")
        call bind(global_map, "<cut>", "kill_region")
        call bind(global_map, "<undo>", "undo")
        call bind(global_map, "<open>", "find_file")
        call bind(global_map, "<insertline>", "open_line")
        call bind(global_map, "<deleteline>", "kill_line")
        call bind(global_map, "S-<delete>", "kill_region")
        call bind(esc_map, "x", "execute_extended_command")
        call bind(global_map, "<next>", "scroll_up_command")
        call bind(global_map, "<prior>", "scroll_down_command")
        call bind(global_map, "C-v", "scroll_up_command")
        call bind(esc_map, "v", "scroll_down_command")
        call bind(esc_map, "C-v", "scroll_other_window")
        call bind(global_map, "M-<next>", "scroll_other_window")
        call bind(esc_map, "<next>", "scroll_other_window")
        call bind(global_map, "M-<prior>", "scroll_other_window_down")
        call bind(esc_map, "<prior>", "scroll_other_window_down")
        call bind(esc_map, "C-S-v", "scroll_other_window_down")
        ! window.el
        call bind(global_map, "C-l", "recenter_top_bottom")
        call bind(esc_map, "r", "move_to_window_line_top_bottom")
        ! Undo (bindings.el).
        commands%undo%advertised_binding = "C-x u"
        call bind(global_map, "C-_", "undo")
        call bind(global_map, "C-/", "undo")
        call bind(ctl_x_map, "u", "undo")
        ! bindings.el: ctl-x-x-map.
        call make_sparse_keymap(ctl_x_x_map)
        call bind(ctl_x_x_map, "f", "font_lock_update")
        key(1) = make_fixnum(int(iachar("x"), int64))
        call define_key(ctl_x_map, key, make_keymap_ref(ctl_x_x_map), status)
        ! bindings.el: ctl-x-r-map (rect.el and register.el; frameset-to-register
        ! on f and the bookmark keys are not ported).
        call make_sparse_keymap(ctl_x_r_map)
        call bind(ctl_x_r_map, "c", "clear_rectangle")
        call bind(ctl_x_r_map, "k", "kill_rectangle")
        call bind(ctl_x_r_map, "d", "delete_rectangle")
        call bind(ctl_x_r_map, "y", "yank_rectangle")
        call bind(ctl_x_r_map, "o", "open_rectangle")
        call bind(ctl_x_r_map, "t", "string_rectangle")
        call bind(ctl_x_r_map, "N", "rectangle_number_lines")
        call bind(ctl_x_r_map, "M-w", "copy_rectangle_as_kill")
        call bind(ctl_x_r_map, "C-@", "point_to_register")
        call bind(ctl_x_r_map, "C-SPC", "point_to_register")
        call bind(ctl_x_r_map, "SPC", "point_to_register")
        call bind(ctl_x_r_map, "j", "jump_to_register")
        call bind(ctl_x_r_map, "s", "copy_to_register")
        call bind(ctl_x_r_map, "x", "copy_to_register")
        call bind(ctl_x_r_map, "i", "insert_register")
        call bind(ctl_x_r_map, "g", "insert_register")
        call bind(ctl_x_r_map, "r", "copy_rectangle_to_register")
        call bind(ctl_x_r_map, "n", "number_to_register")
        call bind(ctl_x_r_map, "+", "increment_register")
        call bind(ctl_x_r_map, "w", "window_configuration_to_register")
        call bind(ctl_x_r_map, "F", "file_to_register")
        call bind(ctl_x_r_map, "B", "buffer_to_register")
        key(1) = make_fixnum(int(iachar("r"), int64))
        call define_key(ctl_x_map, key, make_keymap_ref(ctl_x_r_map), status)
        ! kmacro.el (autoloads): keyboard macros.
        call bind(ctl_x_map, "(", "kmacro_start_macro")
        call bind(ctl_x_map, ")", "kmacro_end_macro")
        call bind(ctl_x_map, "e", "kmacro_end_and_call_macro")
        call bind(global_map, "<f3>", "kmacro_start_macro_or_insert_counter")
        call bind(global_map, "<f4>", "kmacro_end_or_call_macro")
        key(1) = make_fixnum(11_int64)
        call define_key(ctl_x_map, key, make_keymap_ref(kmacro_keymap), status)
        call bind(global_map, "C-?", "undo_redo")
        call bind(global_map, "C-M-_", "undo_redo")
        ! The mark and killing (bindings.el).
        commands%set_mark_command%advertised_binding = "C-SPC"
        call bind(global_map, "C-@", "set_mark_command")
        call bind(global_map, "C-SPC", "set_mark_command")
        call bind(ctl_x_map, "C-x", "exchange_point_and_mark")
        call bind(ctl_x_map, "h", "mark_whole_buffer")
        ! Windows (window.el) and buffers (subr.el ctl-x-map).
        call bind(ctl_x_map, "0", "delete_window")
        call bind(ctl_x_map, "1", "delete_other_windows")
        call bind(ctl_x_map, "2", "split_window_below")
        call bind(ctl_x_map, "3", "split_window_right")
        call bind(ctl_x_map, "o", "other_window")
        call bind(ctl_x_map, "b", "switch_to_buffer")
        call bind(ctl_x_map, "<right>", "next_buffer")
        call bind(ctl_x_map, "C-<right>", "next_buffer")
        call bind(global_map, "<XF86Forward>", "next_buffer")
        call bind(ctl_x_map, "<left>", "previous_buffer")
        call bind(ctl_x_map, "C-<left>", "previous_buffer")
        call bind(global_map, "<XF86Back>", "previous_buffer")
        call bind(ctl_x_map, "k", "kill_buffer")
        call bind(global_map, "C-w", "kill_region")
        call bind(esc_map, "w", "kill_ring_save")
        call bind(global_map, "C-k", "kill_line")
        call bind(global_map, "C-y", "yank")
        call bind(esc_map, "y", "yank_pop")
        call bind(global_map, "<C-S-backspace>", "kill_whole_line")
        ! Words and paragraphs (bindings.el).
        call bind(esc_map, "@", "mark_word")
        call bind(esc_map, "f", "forward_word")
        call bind(esc_map, "b", "backward_word")
        call bind(esc_map, "d", "kill_word")
        call bind(esc_map, "DEL", "backward_kill_word")
        call bind(global_map, "M-<right>", "right_word")
        call bind(esc_map, "<right>", "forward_word")
        call bind(global_map, "M-<left>", "left_word")
        call bind(esc_map, "<left>", "backward_word")
        call bind(global_map, "C-<right>", "right_word")
        call bind(global_map, "C-<left>", "left_word")
        call bind(global_map, "C-<delete>", "kill_word")
        call bind(global_map, "C-<backspace>", "backward_kill_word")
        call bind(global_map, "C-<up>", "backward_paragraph")
        call bind(global_map, "C-<down>", "forward_paragraph")
        call bind(esc_map, "{", "backward_paragraph")
        call bind(esc_map, "}", "forward_paragraph")
        call bind(esc_map, "h", "mark_paragraph")
        call bind(esc_map, "a", "backward_sentence")
        call bind(esc_map, "e", "forward_sentence")
        call bind(esc_map, "k", "kill_sentence")
        call bind(ctl_x_map, "DEL", "backward_kill_sentence")
        ! Balanced expressions (bindings.el).
        call bind(global_map, "C-M-<left>", "backward_sexp")
        call bind(esc_map, "C-<left>", "backward_sexp")
        call bind(global_map, "C-M-<right>", "forward_sexp")
        call bind(esc_map, "C-<right>", "forward_sexp")
        call bind(global_map, "C-M-<up>", "backward_up_list")
        call bind(esc_map, "C-<up>", "backward_up_list")
        call bind(global_map, "C-M-<down>", "down_list")
        call bind(esc_map, "C-<down>", "down_list")
        call bind(esc_map, "C-f", "forward_sexp")
        call bind(esc_map, "C-b", "backward_sexp")
        call bind(esc_map, "C-u", "backward_up_list")
        call bind(esc_map, "C-@", "mark_sexp")
        call bind(esc_map, "C-SPC", "mark_sexp")
        call bind(esc_map, "C-d", "down_list")
        call bind(esc_map, "C-k", "kill_sexp")
        call bind(global_map, "C-M-<delete>", "backward_kill_sexp")
        call bind(global_map, "C-M-<backspace>", "backward_kill_sexp")
        call bind(esc_map, "C-<delete>", "backward_kill_sexp")
        call bind(esc_map, "C-<backspace>", "backward_kill_sexp")
        call bind(esc_map, "C-n", "forward_list")
        call bind(esc_map, "C-p", "backward_list")
        call bind(esc_map, "%", "query_replace")
        call bind(esc_map, "C-%", "query_replace_regexp")
        call bind(esc_map, "!", "shell_command")
        call bind(esc_map, "|", "shell_command_on_region")
        call bind(esc_map, "&", "async_shell_command")
        call bind(ctl_x_map, "`", "next_error")
        ! bindings.el: goto-map (the keys of ported commands).
        call make_sparse_keymap(goto_map)
        call bind(goto_map, "n", "next_error")
        call bind(goto_map, "M-n", "next_error")
        call bind(goto_map, "p", "previous_error")
        call bind(goto_map, "M-p", "previous_error")
        key(1) = make_fixnum(int(iachar("g"), int64))
        call define_key(esc_map, key, make_keymap_ref(goto_map), status)
        call bind(global_map, "C-t", "transpose_chars")
        call bind(esc_map, "t", "transpose_words")
        call bind(ctl_x_map, "C-t", "transpose_lines")
        call bind(esc_map, "SPC", "cycle_spacing")
        call bind(esc_map, "\", "delete_horizontal_space")
        call bind(ctl_x_map, "C-o", "delete_blank_lines")
        call bind(esc_map, "^", "delete_indentation")
        call bind(esc_map, "u", "upcase_word")
        call bind(esc_map, "l", "downcase_word")
        call bind(esc_map, "c", "capitalize_word")
        call bind(ctl_x_map, "C-u", "upcase_region")
        call bind(ctl_x_map, "C-l", "downcase_region")
        call bind(global_map, "TAB", "indent_for_tab_command")
        call bind(esc_map, "C-\", "indent_region")
        call bind(ctl_x_map, "TAB", "indent_rigidly")
        call bind(esc_map, "i", "tab_to_tab_stop")
        call bind(esc_map, "m", "back_to_indentation")
        call bind(esc_map, "q", "fill_paragraph")
        call bind(ctl_x_map, "f", "set_fill_column")
        call bind(ctl_x_map, ".", "set_fill_prefix")
        call bind(esc_map, "j", "default_indent_new_line")
        call bind(esc_map, "C-j", "default_indent_new_line")
        call bind(esc_map, "<", "beginning_of_buffer")
        call bind(esc_map, ">", "end_of_buffer")
        call bind(global_map, "C-<home>", "beginning_of_buffer")
        call bind(global_map, "C-<end>", "end_of_buffer")
        call use_global_map(global_map, status)
        ! subr.el: (define-key ctl-x-map "4" 'ctl-x-4-prefix)
        call make_sparse_keymap(ctl_x_4_map)
        key(1) = make_fixnum(int(iachar("4"), int64))
        call define_key(ctl_x_map, key, make_keymap_ref(ctl_x_4_map), status)
        call syms_of_files(ctl_x_map, ctl_x_4_map)
        call syms_of_dired_mode(ctl_x_map, ctl_x_4_map)
        call syms_of_isearch(global_map)
        ! The minibuffer: minibuffer-local-map's keys from minibuffer.el,
        ! then its keys from bindings.el.
        if (keymap_live_p(minibuffer_local_map)) call free_keymap(minibuffer_local_map, status)
        call init_minibuf()
        call syms_of_minibuffer()
        call syms_of_subr()
        call bind(minibuffer_local_map, "M-n", "next_history_element")
        call bind(minibuffer_local_map, "<next>", "next_history_element")
        call bind(minibuffer_local_map, "<down>", "next_line_or_history_element")
        call bind(minibuffer_local_map, "M-p", "previous_history_element")
        call bind(minibuffer_local_map, "<prior>", "previous_history_element")
        call bind(minibuffer_local_map, "<up>", "previous_line_or_history_element")
        ! Override the global binding (which calls indent-relative via
        ! indent-for-tab-command).  The alignment that indent-relative
        ! tries to do doesn't make much sense here since the prompt messes
        ! it up.
        call bind(minibuffer_local_map, "TAB", "self_insert_command")
        ! Help (help.el, help-fns.el), after the maps it names exist.
        call syms_of_help(global_map)
        call syms_of_help_fns()
        ! The other keymap variables, for \<MAP> and help--binding-locus.
        call register_keymap_name("esc_map", esc_map)
        call register_keymap_name("ctl_x_map", ctl_x_map)
        call register_keymap_name("ctl_x_4_map", ctl_x_4_map)
        call register_keymap_name("dired_mode_map", dired_mode_map)
        call register_keymap_name("ctl_x_r_map", ctl_x_r_map)
        call register_keymap_name("kmacro_keymap", kmacro_keymap)
        call register_keymap_name("isearch_mode_map", isearch_mode_map)
        call register_keymap_name("text_mode_map", text_mode_map)
        call register_keymap_name("prog_mode_map", prog_mode_map)
        call register_keymap_name("special_mode_map", special_mode_map)
        call register_keymap_name("compilation_mode_map", compilation_mode_map)
        call register_keymap_name("comint_mode_map", comint_mode_map)
        call register_keymap_name("shell_mode_map", shell_mode_map)
        call register_keymap_name("goto_map", goto_map)
        call register_keymap_name("minibuffer_local_completion_map", minibuffer_local_completion_map)
        call register_keymap_name("minibuffer_local_must_match_map", minibuffer_local_must_match_map)
        call register_keymap_name("minibuffer_local_filename_completion_map", minibuffer_local_filename_completion_map)
        call register_keymap_name("minibuffer_local_filename_must_match_map", minibuffer_local_filename_must_match_map)
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
end module bindings_fm
