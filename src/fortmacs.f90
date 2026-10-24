! fortmacs.f90 -- the configuration API: what init.f90 uses.
!
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

!> The configuration API.  A user's init.f90 is a module named
!> `user_config' with a public subroutine `configure', which runs once at
!> startup (where upstream loads the init file):
!>
!>     module user_config
!>         use fortmacs
!>         implicit none(type, external)
!>         private
!>         public :: configure
!>         type(command_type), target :: insert_date_stamp_command
!>     contains
!>         subroutine configure(status)
!>             type(status_type), intent(out) :: status
!>
!>             tab_width = 4
!>             make_backup_files = .false.
!>             insert_date_stamp_command = command_type("insert_date_stamp", insert_date_stamp)
!>             call insert_date_stamp_command%define()
!>             call keymap_global_set("C-c d", "insert_date_stamp", status)
!>         end subroutine configure
!>
!>         subroutine insert_date_stamp(status)
!>             type(status_type), intent(out) :: status
!>             character(len=8) :: date
!>
!>             call date_and_time(date=date)
!>             call insert(date, status)
!>         end subroutine insert_date_stamp
!>     end module user_config
!>
!> Settings are the variables below; assign them (that sets the default
!> value; `call fill_column%set_local(60)' sets the current buffer's own
!> value).  Commands are subroutines taking a `status_type', defined as
!> `command_type' objects: built-in ones are the components of
!> `commands', so that `commands%upcase_region%disabled = .false.'
!> enables C-x C-u, and `call commands%yank%run(status)' runs one.  Keys
!> are bound with `keymap_global_set'.  Modes are customized from
!> their hooks and keymaps, named after their Emacs variables: `call text_mode_hook%add(turn_on_auto_fill)'.  New modes
!> are `major_mode_type' and `minor_mode_type' objects made known with
!> their `define'.  A `configure' that fails (returns a
!> status that is not ok) leaves the editor running and shows the error.
!> `fortmacs -q' starts without running `configure'.
!>
!> This module only gathers names from the modules that define them, so
!> the API stays one `use' away.
module fortmacs
    use, intrinsic :: iso_fortran_env, only: int64
    ! Errors and values.
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR
    ! Commands and keys.
    use commands_fm, only: commands
    use callint_fm, only: command_proc, command_type, set_command_disabled
    use keymap_fm, only: keymap_global_set, keymap_set, key_valid_p, current_global_map
    use subr_fm, only: global_set_key, y_or_n_p
    ! Buffers and editing.
    use handle_fm, only: handle_type
    use buffer_fm, only: current_buffer, get_buffer_create, set_buffer, buffer_name
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, goto_char, buffer_substring, delete_region, bobp, eobp, bolp, &
                          eolp, region_beginning, region_end
    use cmds_fm, only: forward_char, backward_char, forward_line, beginning_of_line, end_of_line
    use files_fm, only: find_file, save_buffer
    use fns_fm, only: yes_or_no_p
    use minibuf_fm, only: read_string
    use xdisp_fm, only: message
    use search_fm, only: search_forward, search_backward, match_beginning, match_end, SEARCH_NOERROR, SEARCH_MOVE
    use isearch_fm, only: isearch_forward_command, isearch_backward_command
    use simple_fm, only: undo_only, undo_redo
    use undo_fm, only: undo_boundary
    use simple_fm, only: kill_new, current_kill, kill_region, push_mark, set_mark, mark, region_active_p, &
                         deactivate_mark
    use minibuffer_fm, only: read_file_name
    use syntax_fm, only: forward_word, modify_syntax_entry, char_syntax, standard_syntax_table, make_syntax_table, &
                         copy_syntax_table, set_syntax_table, syntax_table
    use simple_fm, only: backward_word, kill_word, backward_kill_word, mark_word
    use paragraphs_fm, only: forward_paragraph, backward_paragraph, mark_paragraph, kill_paragraph, &
                             backward_kill_paragraph
    use help_fns_fm, only: describe_function, describe_variable
    use help_fm, only: substitute_command_keys
    use window_fm, only: split_window, delete_window, delete_other_windows, other_window, select_window, &
                         other_buffer, switch_to_buffer, quit_window
    use frame_fm, only: selected_window
    use buffer_fm, only: kill_buffer
    use minibuffer_fm, only: read_buffer
    use search_fm, only: re_search_forward, re_search_backward, posix_search_forward, posix_search_backward, &
                         looking_at, posix_looking_at, string_match, posix_string_match, match_data, set_match_data, &
                         regexp_quote
    use subr_fm, only: match_string, match_string_no_properties, string_match_p, looking_at_p, looking_back
    use isearch_fm, only: isearch_forward_regexp_command, isearch_backward_regexp_command
    use paragraphs_fm, only: forward_sentence, backward_sentence, kill_sentence, backward_kill_sentence, &
                             mark_end_of_sentence, sentence_end_regexp
    use search_fm, only: replace_match
    use subr_fm, only: match_substitute_replacement
    use casefiddle_fm, only: upcase_string, downcase_string, capitalize_string, upcase_initials_string, upcase_char, &
                             downcase_char, capitalize_char, upcase_initials_char, upcase_region, downcase_region, &
                             capitalize_region, upcase_initials_region, upcase_word, downcase_word, capitalize_word
    use simple_fm, only: transpose_chars, transpose_words, transpose_lines, delete_horizontal_space, &
                         delete_all_space, just_one_space, fixup_whitespace, delete_blank_lines, delete_indentation
    use paragraphs_fm, only: transpose_sentences, transpose_paragraphs
    use syntax_fm, only: skip_chars_forward, skip_chars_backward, skip_syntax_forward, skip_syntax_backward
    use novice_fm, only: enable_command, disable_command
    use rmc_fm, only: rmc_choice_type, read_multiple_choice
    use indent_fm, only: indent_to, current_indentation, indent_according_to_mode, indent_rigidly, indent_line_to, &
                         current_left_margin, move_to_left_margin, indent_region, indent_relative, tab_to_tab_stop, &
                         move_to_tab_stop, beginning_of_line_text, indent_line_proc, indent_region_proc
    use fill_fm, only: fill_paragraph, fill_region, fill_region_as_paragraph, unfill_paragraph, set_fill_prefix, &
                       justify_current_line, canonically_space_region, current_fill_column, current_justification, &
                       fill_single_word_nobreak_p, fill_french_nobreak_p, fill_polish_nobreak_p, &
                       fill_single_char_nobreak_p, fill_nobreak_predicate_type, JUSTIFY_NONE, JUSTIFY_LEFT, &
                       JUSTIFY_RIGHT, JUSTIFY_FULL, JUSTIFY_CENTER, JUSTIFY_T
    use simple_fm, only: back_to_indentation, do_auto_fill, default_indent_new_line
    use subr_fm, only: read_number
    use replace_fm, only: query_replace, query_replace_regexp, map_query_replace_regexp, replace_string, &
                          replace_regexp, replacement_function
    use simple_fm, only: count_lines, line_number_at_pos
    use rect_fm, only: apply_on_rectangle, rectangle_line_action, rectangle_line_type, rectangle_position_as_coordinates, &
                       rectangle_intersect_p, rectangle_dimensions, delete_rectangle, delete_extract_rectangle, &
                       extract_rectangle, extract_rectangle_bounds, kill_rectangle, copy_rectangle_as_kill, &
                       yank_rectangle, insert_rectangle, open_rectangle, delete_whitespace_rectangle, string_rectangle, &
                       string_insert_rectangle, clear_rectangle, rectangle_number_lines, spaces_string
    use register_fm, only: register_type, register_value_type, get_register, set_register, point_to_register, &
                           window_configuration_to_register, jump_to_register, file_to_register, buffer_to_register, &
                           number_to_register, increment_register, view_register, list_registers, insert_register, &
                           copy_to_register, append_to_register, prepend_to_register, copy_rectangle_to_register, &
                           register_read_with_preview, REGISTER_EMPTY, REGISTER_MARKER, REGISTER_TEXT, &
                           REGISTER_RECTANGLE, REGISTER_NUMBER, REGISTER_WINDOW_CONFIGURATION, REGISTER_FILE, &
                           REGISTER_FILE_QUERY, REGISTER_BUFFER
    ! Buffer-local variables, hooks and modes.
    use subr_fm, only: run_mode_hooks
    use simple_fm, only: fundamental_mode, special_mode
    use text_mode_fm, only: text_mode
    use prog_mode_fm, only: prog_mode
    use files_fm, only: normal_mode, set_auto_mode, add_auto_mode, file_name_sans_versions
    ! Settings.
    use xdisp_fm, only: tab_width, truncate_partial_width_windows
    use files_fm, only: make_backup_files, change_major_mode_with_file_name
    use simple_fm, only: line_move_visual, goal_column
    use subr_fm, only: history_length, history_delete_duplicates, y_or_n_p_use_read_key
    use fns_fm, only: use_short_answers
    use rmc_fm, only: read_char_choice_use_read_key
    use keyboard_fm, only: minibuffer_message_timeout
    use minibuffer_fm, only: read_file_name_completion_ignore_case, read_buffer_completion_ignore_case
    use minibuf_fm, only: enable_recursive_minibuffers, history_add_new_input
    use minibuffer_fm, only: insert_default_directory
    use editfns_fm, only: inhibit_field_text_motion
    use buffer_fm, only: transient_mark_mode
    use callint_fm, only: mark_even_if_inactive
    use simple_fm, only: kill_ring_max, mark_ring_max, kill_whole_line_option, delete_active_region, kill_read_only_ok
    use rect_fm, only: killed_rectangle
    use register_fm, only: register_alist, register_separator
    use term_fm, only: visible_bell
    use minibuf_fm, only: completion_ignore_case
    use minibuffer_fm, only: completion_auto_help, completion_show_inline_help, completion_fail_discreetly
    use simple_fm, only: suggest_key_bindings
    use dired_fm, only: completion_ignored_extensions
    use search_fm, only: case_fold_search
    ! Font Lock.
    use font_core_fm, only: font_lock_defaults, font_lock_defaults_setting, font_lock_global_modes, &
                            font_lock_global_modes_type, font_lock_mode_hook, global_font_lock_mode_hook, &
                            turn_on_font_lock, font_lock_keyword_type, font_lock_highlight_type, &
                            font_lock_match_highlight_type, font_lock_keywords_type, font_lock_syntax_entry_type, &
                            FONT_LOCK_OVERRIDE_NIL, FONT_LOCK_OVERRIDE_T, FONT_LOCK_OVERRIDE_KEEP, &
                            FONT_LOCK_OVERRIDE_PREPEND, FONT_LOCK_OVERRIDE_APPEND, font_lock_matcher_proc, &
                            font_lock_face_proc, font_lock_form_proc
    use font_lock_fm, only: font_lock_maximum_decoration, FONT_LOCK_DECORATION_MAX, font_lock_verbose, &
                            font_lock_dont_widen, font_lock_support_mode, FONT_LOCK_SUPPORT_NONE, &
                            FONT_LOCK_SUPPORT_JIT_LOCK, font_lock_extra_managed_props, font_lock_syntactic_face_function, &
                            font_lock_keyword, font_lock_highlight, font_lock_anchored, font_lock_add_keywords, &
                            FONT_LOCK_ADD_BEGINNING, FONT_LOCK_ADD_END, FONT_LOCK_ADD_SET
    use jit_lock_fm, only: jit_lock_chunk_size
    use xdisp_fm, only: fontification_functions
    use eval_fm, only: position_hook_type, position_function
    use isearch_fm, only: search_upper_case, search_exit_option, search_nonincremental_instead, search_ring_max, &
                          UPPER_CASE_NIL, UPPER_CASE_T, UPPER_CASE_NOT_YANKS
    use window_fm, only: next_screen_context_lines, scroll_error_top_bottom
    use syntax_fm, only: words_include_escapes
    use search_fm, only: search_spaces_regexp
    use isearch_fm, only: regexp_search_ring_max, search_whitespace_regexp, isearch_lax_whitespace, &
                          isearch_regexp_lax_whitespace
    use paragraphs_fm, only: paragraph_start, paragraph_separate, sentence_end, sentence_end_base, &
                             sentence_end_double_space, sentence_end_without_period, sentence_end_without_space
    use casefiddle_fm, only: case_symbols_as_words
    use simple_fm, only: fill_prefix, cycle_spacing_actions, cycle_spacing_action_type, CYCLE_JUST_ONE_SPACE, &
                         CYCLE_DELETE_SPACE_AFTER, CYCLE_DELETE_SPACE_BEFORE, CYCLE_DELETE_ALL_SPACE, CYCLE_RESTORE, &
                         CYCLE_ARG_NONE, CYCLE_ARG_FIXED, CYCLE_ARG_INVERTED, CYCLE_ARG_MINUS
    use novice_fm, only: disabled_command_function_p
    use buffer_fm, only: fill_column, left_margin, auto_fill_function
    use indent_fm, only: indent_tabs_mode, standard_indent, tab_always_indent, tab_stop_list, indent_line_function, &
                         indent_region_function
    use fill_fm, only: colon_double_space, fill_separate_heterogeneous_words_with_space, adaptive_fill_mode, &
                       adaptive_fill_regexp, adaptive_fill_first_line_regexp, fill_indent_according_to_mode, &
                       default_justification, fill_nobreak_predicate
    use simple_fm, only: auto_fill_inhibit_regexp, inhibit_auto_fill, normal_auto_fill_function
    use replace_fm, only: case_replace, replace_lax_whitespace, replace_regexp_lax_whitespace, &
                          query_replace_skip_read_only, query_replace_show_replacement, query_replace_from_to_separator
    ! Settings, hooks and modes as objects.
    use data_fm, only: setting_type, integer_setting, logical_setting, string_setting, function_setting
    use eval_fm, only: hook_type, change_major_mode_hook, change_hook_type, after_change_hook_type, change_function, &
                       after_change_function
    use insdel_fm, only: before_change_functions, after_change_functions, first_change_hook, inhibit_modification_hooks
    use subr_fm, only: change_major_mode_after_body_hook, after_change_major_mode_hook
    use derived_fm, only: major_mode_type, derived_mode_p, provided_mode_derived_p, major_mode_named
    use easy_mmode_fm, only: minor_mode_type, minor_mode_body_proc
    use text_mode_fm, only: text_mode_hook, text_mode_map, text_mode_syntax_table
    use prog_mode_fm, only: prog_mode_hook, prog_mode_map
    use simple_fm, only: special_mode_hook, special_mode_map, transient_mark_mode_hook, indent_tabs_mode_hook, &
                         auto_fill_mode_hook
    use help_mode_fm, only: help_mode_hook, help_mode_map
    use simple_fm, only: turn_on_auto_fill, turn_off_auto_fill
    use text_mode_fm, only: text_mode_variant
    use syntax_fm, only: parse_sexp_ignore_comments, comment_end_can_be_escaped, multibyte_syntax_as_symbol, &
                         open_paren_in_column_0_is_defun_start, comment_use_syntax_ppss
    use syntax_fm, only: forward_comment, scan_lists, scan_sexps, parse_partial_sexp, parse_state_type, &
                         ST_COMMENT_STYLE, ST_STRING_STYLE, COMMENTSTOP_NIL, COMMENTSTOP_COMMENT, COMMENTSTOP_SYNTAX_TABLE
    use lisp_cmds_fm, only: forward_sexp, backward_sexp, forward_list, backward_list, down_list, up_list, &
                            backward_up_list, kill_sexp, backward_kill_sexp, mark_sexp
    ! Keyboard macros.
    use keyboard_fm, only: kbd_macro_termination_hook
    use kmacro_fm, only: kmacro_ring_max, kmacro_execute_before_append, kmacro_repeat_no_prefix, kmacro_call_repeat_key, &
                         kmacro_call_repeat_with_arg, KMACRO_CALL_REPEAT_DISABLED, KMACRO_CALL_REPEAT_LAST_KEY
    use edmacro_fm, only: edmacro_eight_bits
    use files_fm, only: require_final_newline, mode_require_final_newline, FINAL_NEWLINE_NEVER, FINAL_NEWLINE_SAVE, &
                        FINAL_NEWLINE_VISIT, FINAL_NEWLINE_VISIT_SAVE, FINAL_NEWLINE_ASK
    ! Dired.
    use dired_mode_fm, only: dired, dired_other_window, dired_noselect, dired_mode, dired_mode_map, dired_mode_hook, &
                             dired_before_readin_hook, dired_after_readin_hook, dired_initial_position_hook, &
                             dired_listing_switches, dired_use_ls_dired, DIRED_USE_LS_DIRED_NIL, DIRED_USE_LS_DIRED_T, &
                             DIRED_USE_LS_DIRED_UNSPECIFIED, dired_ls_f_marks_symlinks, dired_trivial_filenames, &
                             dired_free_space, DIRED_FREE_SPACE_NIL, DIRED_FREE_SPACE_SEPARATE, DIRED_FREE_SPACE_FIRST, &
                             dired_mark_region, DIRED_MARK_REGION_NIL, DIRED_MARK_REGION_FILE, DIRED_MARK_REGION_LINE, &
                             dired_movement_style, DIRED_MOVEMENT_STYLE_NIL, DIRED_MOVEMENT_STYLE_CYCLE, &
                             DIRED_MOVEMENT_STYLE_BOUNDED, DIRED_MOVEMENT_STYLE_CYCLE_FILES, &
                             DIRED_MOVEMENT_STYLE_BOUNDED_FILES, dired_auto_revert_buffer, dired_check_symlinks, &
                             dired_recursive_deletes, DIRED_RECURSIVE_NEVER, DIRED_RECURSIVE_ALWAYS, DIRED_RECURSIVE_TOP, &
                             DIRED_RECURSIVE_ASK, dired_no_confirm, dired_no_confirm_type, dired_switches_in_mode_line, &
                             DIRED_SWITCHES_SORT_ORDER, DIRED_SWITCHES_AS_IS, dired_sort_inhibit, &
                             dired_garbage_files_regexp, dired_always_read_filesystem, dired_deletion_confirmer, &
                             dired_header_face, dired_mark_face, dired_marked_face, dired_flagged_face, &
                             dired_warning_face, dired_perm_write_face, dired_directory_face, dired_symlink_face, &
                             dired_ignored_face
    use files_fm, only: insert_directory_program, find_file_run_dired, find_file_other_window, revert_buffer
    ! Processes and shell commands.
    use process_fm, only: process_arg_type, process_filter_proc, process_sentinel_proc, make_process, start_process, &
                          delete_process, free_process, processp, process_live_p, get_process, get_buffer_process, &
                          process_list, process_status, process_exit_status, process_id, process_name, &
                          process_command, process_buffer, set_process_buffer, process_mark, set_process_filter, &
                          set_process_sentinel, process_tty_name, set_process_query_on_exit_flag, &
                          process_query_on_exit_flag, interrupt_process, kill_process, quit_process, signal_process, &
                          process_send_string, process_send_region, process_send_eof, accept_process_output, &
                          internal_default_process_filter, internal_default_process_sentinel, &
                          process_connection_type, delete_exited_processes, PROCESS_CONNECTION_DEFAULT, &
                          PROCESS_CONNECTION_PIPE, PROCESS_CONNECTION_PTY, num_processors
    use compile_fm, only: compile, recompile, compilation_start, compilation_mode, kill_compilation, &
                          compile_command, compilation_read_command, compilation_ask_about_save, &
                          compilation_always_kill, compilation_disable_input, compilation_scroll_output, &
                          COMPILATION_SCROLL_NIL, COMPILATION_SCROLL_T, COMPILATION_SCROLL_FIRST_ERROR, &
                          compilation_filter_hook, compilation_filter_start, compilation_mode_hook, &
                          compilation_mode_map, compilation_directory
    use compile_fm, only: compilation_rule_type, compilation_highlight_type, compilation_directory_matcher_type, &
                          compilation_transform_type, compilation_number_function, compilation_file_function, &
                          compilation_face_function, compilation_minor_mode_hook, compilation_shell_minor_mode_hook, &
                          compilation_minor_mode_map, compilation_shell_minor_mode_map, &
                          compilation_error_regexp_alist_alist, compilation_error_regexp_alist, &
                          compilation_error_case_fold_search, compilation_directory_matcher, &
                          compilation_transform_file_match_alist, compilation_error_screen_columns, &
                          compilation_first_column, compilation_skip_threshold, compilation_skip_visited, &
                          compilation_skip_to_next_location, compilation_search_all_directories, &
                          compilation_context_lines, COMPILATION_CONTEXT_NIL, COMPILATION_CONTEXT_T, &
                          compilation_message_face, compilation_error_face, compilation_warning_face, &
                          compilation_info_face, compilation_line_face, compilation_column_face, &
                          compilation_enter_directory_face, compilation_leave_directory_face, compilation_error, &
                          compilation_warning, compilation_info, compilation_mode_line_fail, compilation_mode_line_run, &
                          compilation_mode_line_exit, compilation_line_number, compilation_column_number, &
                          compilation_minor_mode, compilation_shell_minor_mode, compile_goto_error, &
                          compilation_next_error, compilation_previous_error, compilation_next_file, &
                          compilation_previous_file
    use comint_fm, only: comint_inhibit_carriage_motion, comint_mode, make_comint, make_comint_in_buffer, comint_run, &
                         comint_exec, comint_send_input, comint_send_string, comint_prompt_regexp, &
                         comint_prompt_read_only, comint_highlight_input, comint_input_ignoredups, &
                         comint_input_ring_file_name, comint_pager, comint_input_ring_file_prefix, &
                         comint_move_point_for_output, comint_move_point_for_matching_input, COMINT_MOVE_NIL, &
                         COMINT_MOVE_T, COMINT_MOVE_ALL, COMINT_MOVE_THIS, COMINT_MOVE_OTHERS, &
                         COMINT_MATCHING_AFTER_INPUT, COMINT_MATCHING_END_OF_LINE, comint_scroll_show_maximum_output, &
                         comint_buffer_maximum_size, comint_input_ring_size, comint_input_ring_separator, &
                         comint_input_history_ignore, comint_process_echoes, comint_eol_on_send, &
                         comint_use_prompt_regexp, comint_terminfo_terminal, comint_delimiter_argument_list, &
                         comint_mode_hook, comint_exec_hook, comint_output_filter_functions, &
                         comint_preoutput_filter_functions, comint_input_filter_functions, comint_mode_map, &
                         comint_highlight_input_face, comint_highlight_prompt, comint_truncate_buffer, &
                         comint_strip_ctrl_m, comint_postoutput_scroll_to_bottom, comint_show_maximum_output
    use shell_fm, only: shell, shell_mode, shell_dumb_shell_regexp, shell_prompt_pattern, shell_delimiter_argument_list, &
                        shell_command_regexp, shell_command_separator_regexp, shell_popd_regexp, shell_pushd_regexp, &
                        shell_pushd_tohome, shell_pushd_dextract, shell_pushd_dunique, shell_cd_regexp, &
                        shell_dirtrack_verbose, explicit_shell_file_name, explicit_csh_args, explicit_bash_args, &
                        shell_has_auto_cd, shell_get_old_input_include_continuation_lines, shell_kill_buffer_on_exit, &
                        shell_history_file_name, shell_dirstack_query, shell_mode_hook, shell_command_mode_hook, &
                        shell_dirtrack_mode_hook, shell_mode_map, shell_command_mode_map, shell_dirtrack_mode
    use simple_fm, only: async_shell_command, async_shell_command_buffer, async_shell_command_display_buffer, &
                         async_shell_command_width, async_shell_command_mode, ASYNC_CONFIRM_KILL_PROCESS, &
                         ASYNC_CONFIRM_NEW_BUFFER, ASYNC_NEW_BUFFER, ASYNC_CONFIRM_RENAME_BUFFER, ASYNC_RENAME_BUFFER, &
                         next_error, next_error_hook, next_error_verbose, next_error_last_buffer
    use callproc_fm, only: shell_file_name
    use simple_fm, only: shell_command, shell_command_on_region, read_shell_command, display_message_or_buffer, &
                         shell_command_switch, shell_command_default_error_buffer, shell_command_prompt_show_cwd, &
                         shell_command_dont_erase_buffer, shell_command_buffer_name, DONT_ERASE_NIL, DONT_ERASE_ERASE, &
                         DONT_ERASE_BEG_LAST_OUT, DONT_ERASE_END_LAST_OUT, DONT_ERASE_SAVE_POINT
    implicit none(type, external)
    private

    public :: int64
    public :: process_arg_type, process_filter_proc, process_sentinel_proc, make_process, start_process, &
              delete_process, free_process, processp, process_live_p, get_process, get_buffer_process, process_list, &
              process_status, process_exit_status, process_id, process_name, process_command, process_buffer, &
              set_process_buffer, process_mark, set_process_filter, set_process_sentinel, process_tty_name, &
              set_process_query_on_exit_flag, process_query_on_exit_flag, interrupt_process, kill_process, &
              quit_process, signal_process, process_send_string, process_send_region, process_send_eof, &
              accept_process_output, internal_default_process_filter, internal_default_process_sentinel, &
              process_connection_type, delete_exited_processes, PROCESS_CONNECTION_DEFAULT, PROCESS_CONNECTION_PIPE, &
              PROCESS_CONNECTION_PTY, shell_file_name, shell_command, shell_command_on_region, read_shell_command, &
              display_message_or_buffer, shell_command_switch, shell_command_default_error_buffer, &
              shell_command_prompt_show_cwd, shell_command_dont_erase_buffer, shell_command_buffer_name, &
              DONT_ERASE_NIL, DONT_ERASE_ERASE, DONT_ERASE_BEG_LAST_OUT, DONT_ERASE_END_LAST_OUT, DONT_ERASE_SAVE_POINT, &
              num_processors, compile, recompile, compilation_start, compilation_mode, kill_compilation, compile_command, &
              compilation_read_command, compilation_ask_about_save, compilation_always_kill, compilation_disable_input, &
              compilation_scroll_output, COMPILATION_SCROLL_NIL, COMPILATION_SCROLL_T, COMPILATION_SCROLL_FIRST_ERROR, &
              compilation_filter_hook, compilation_filter_start, compilation_mode_hook, compilation_mode_map, &
              compilation_directory, comint_inhibit_carriage_motion
    public :: compilation_rule_type, compilation_highlight_type, compilation_directory_matcher_type, &
              compilation_transform_type, compilation_number_function, compilation_file_function, &
              compilation_face_function, compilation_minor_mode_hook, compilation_shell_minor_mode_hook, &
              compilation_minor_mode_map, compilation_shell_minor_mode_map, compilation_error_regexp_alist_alist, &
              compilation_error_regexp_alist, compilation_error_case_fold_search, compilation_directory_matcher, &
              compilation_transform_file_match_alist, compilation_error_screen_columns, compilation_first_column, &
              compilation_skip_threshold, compilation_skip_visited, compilation_skip_to_next_location, &
              compilation_search_all_directories, compilation_context_lines, COMPILATION_CONTEXT_NIL, &
              COMPILATION_CONTEXT_T, compilation_message_face, compilation_error_face, compilation_warning_face, &
              compilation_info_face, compilation_line_face, compilation_column_face, compilation_enter_directory_face, &
              compilation_leave_directory_face, compilation_error, compilation_warning, compilation_info, &
              compilation_mode_line_fail, compilation_mode_line_run, compilation_mode_line_exit, &
              compilation_line_number, compilation_column_number, compilation_minor_mode, &
              compilation_shell_minor_mode, compile_goto_error, compilation_next_error, compilation_previous_error, &
              compilation_next_file, compilation_previous_file
    public :: comint_mode, make_comint, make_comint_in_buffer, comint_run, comint_exec, comint_send_input, &
              comint_send_string, comint_prompt_regexp, comint_prompt_read_only, comint_highlight_input, &
              comint_input_ignoredups, comint_input_ring_file_name, comint_pager, comint_input_ring_file_prefix, &
              comint_move_point_for_output, comint_move_point_for_matching_input, COMINT_MOVE_NIL, COMINT_MOVE_T, &
              COMINT_MOVE_ALL, COMINT_MOVE_THIS, COMINT_MOVE_OTHERS, COMINT_MATCHING_AFTER_INPUT, &
              COMINT_MATCHING_END_OF_LINE, comint_scroll_show_maximum_output, comint_buffer_maximum_size, &
              comint_input_ring_size, comint_input_ring_separator, comint_input_history_ignore, comint_process_echoes, &
              comint_eol_on_send, comint_use_prompt_regexp, comint_terminfo_terminal, comint_delimiter_argument_list, &
              comint_mode_hook, comint_exec_hook, comint_output_filter_functions, comint_preoutput_filter_functions, &
              comint_input_filter_functions, comint_mode_map, comint_highlight_input_face, comint_highlight_prompt, &
              comint_truncate_buffer, comint_strip_ctrl_m, comint_postoutput_scroll_to_bottom, &
              comint_show_maximum_output
    public :: shell, shell_mode, shell_dumb_shell_regexp, shell_prompt_pattern, shell_delimiter_argument_list, &
              shell_command_regexp, shell_command_separator_regexp, shell_popd_regexp, shell_pushd_regexp, &
              shell_pushd_tohome, shell_pushd_dextract, shell_pushd_dunique, shell_cd_regexp, shell_dirtrack_verbose, &
              explicit_shell_file_name, explicit_csh_args, explicit_bash_args, shell_has_auto_cd, &
              shell_get_old_input_include_continuation_lines, shell_kill_buffer_on_exit, shell_history_file_name, &
              shell_dirstack_query, shell_mode_hook, shell_command_mode_hook, shell_dirtrack_mode_hook, shell_mode_map, &
              shell_command_mode_map, shell_dirtrack_mode
    public :: async_shell_command, async_shell_command_buffer, async_shell_command_display_buffer, &
              async_shell_command_width, async_shell_command_mode, ASYNC_CONFIRM_KILL_PROCESS, ASYNC_CONFIRM_NEW_BUFFER, &
              ASYNC_NEW_BUFFER, ASYNC_CONFIRM_RENAME_BUFFER, ASYNC_RENAME_BUFFER, next_error, next_error_hook, &
              next_error_verbose, next_error_last_buffer
    public :: status_type, set_error, format_error, FM_GENERIC_ERROR
    public :: command_proc, command_type, commands, set_command_disabled
    public :: keymap_global_set, keymap_set, key_valid_p, current_global_map, global_set_key
    public :: handle_type, current_buffer, get_buffer_create, set_buffer, buffer_name
    public :: insert, point, point_min, point_max, goto_char, buffer_substring, delete_region, bobp, eobp, bolp, eolp
    public :: forward_char, backward_char, forward_line, beginning_of_line, end_of_line
    public :: find_file, save_buffer, yes_or_no_p, y_or_n_p, read_string, message
    public :: search_forward, search_backward, match_beginning, match_end, SEARCH_NOERROR, SEARCH_MOVE, &
              isearch_forward_command, isearch_backward_command
    public :: undo_only, undo_redo, undo_boundary
    public :: region_beginning, region_end, kill_new, current_kill, kill_region, push_mark, set_mark, mark, &
              region_active_p, deactivate_mark
    public :: read_file_name
    public :: forward_word, backward_word, kill_word, backward_kill_word, mark_word, modify_syntax_entry, &
              char_syntax, standard_syntax_table, make_syntax_table, copy_syntax_table, set_syntax_table, &
              syntax_table, forward_paragraph, backward_paragraph, mark_paragraph, kill_paragraph, &
              backward_kill_paragraph, describe_function, describe_variable, substitute_command_keys
    public :: split_window, delete_window, delete_other_windows, other_window, select_window, selected_window, &
              other_buffer, switch_to_buffer, quit_window, kill_buffer, read_buffer
    public :: re_search_forward, re_search_backward, posix_search_forward, posix_search_backward, looking_at, &
              posix_looking_at, string_match, posix_string_match, match_data, set_match_data, regexp_quote, &
              match_string, match_string_no_properties, string_match_p, looking_at_p, looking_back, &
              isearch_forward_regexp_command, isearch_backward_regexp_command, forward_sentence, backward_sentence, &
              kill_sentence, backward_kill_sentence, mark_end_of_sentence, sentence_end_regexp, replace_match, &
              match_substitute_replacement, upcase_string, downcase_string, capitalize_string, &
              upcase_initials_string, upcase_char, downcase_char, capitalize_char, upcase_initials_char, &
              upcase_region, downcase_region, capitalize_region, upcase_initials_region, upcase_word, downcase_word, &
              capitalize_word, query_replace, query_replace_regexp, map_query_replace_regexp, replace_string, &
              replace_regexp, replacement_function, transpose_chars, transpose_words, transpose_lines, &
              transpose_sentences, transpose_paragraphs, delete_horizontal_space, delete_all_space, just_one_space, &
              fixup_whitespace, delete_blank_lines, delete_indentation, skip_chars_forward, skip_chars_backward, &
              skip_syntax_forward, skip_syntax_backward, forward_comment, scan_lists, scan_sexps, &
              parse_partial_sexp, parse_state_type, ST_COMMENT_STYLE, ST_STRING_STYLE, COMMENTSTOP_NIL, &
              COMMENTSTOP_COMMENT, COMMENTSTOP_SYNTAX_TABLE, forward_sexp, backward_sexp, forward_list, backward_list, &
 down_list, up_list, backward_up_list, kill_sexp, backward_kill_sexp, mark_sexp, enable_command, disable_command, rmc_choice_type, &
              read_multiple_choice, indent_to, current_indentation, indent_according_to_mode, indent_rigidly, &
              indent_line_to, current_left_margin, move_to_left_margin, indent_region, indent_relative, &
              tab_to_tab_stop, move_to_tab_stop, beginning_of_line_text, indent_line_proc, indent_region_proc, &
              fill_paragraph, fill_region, fill_region_as_paragraph, unfill_paragraph, set_fill_prefix, &
              justify_current_line, canonically_space_region, current_fill_column, current_justification, &
              fill_single_word_nobreak_p, fill_french_nobreak_p, fill_polish_nobreak_p, fill_single_char_nobreak_p, &
              fill_nobreak_predicate_type, JUSTIFY_NONE, JUSTIFY_LEFT, JUSTIFY_RIGHT, JUSTIFY_FULL, JUSTIFY_CENTER, &
              JUSTIFY_T, back_to_indentation, do_auto_fill, default_indent_new_line, read_number
    public :: count_lines, line_number_at_pos, apply_on_rectangle, rectangle_line_action, rectangle_line_type, &
              rectangle_position_as_coordinates, rectangle_intersect_p, rectangle_dimensions, delete_rectangle, &
              delete_extract_rectangle, extract_rectangle, extract_rectangle_bounds, kill_rectangle, &
              copy_rectangle_as_kill, yank_rectangle, insert_rectangle, open_rectangle, delete_whitespace_rectangle, &
              string_rectangle, string_insert_rectangle, clear_rectangle, rectangle_number_lines, spaces_string, &
              killed_rectangle, register_type, register_value_type, register_alist, register_separator, get_register, &
              set_register, point_to_register, window_configuration_to_register, jump_to_register, file_to_register, &
              buffer_to_register, number_to_register, increment_register, view_register, list_registers, &
              insert_register, copy_to_register, append_to_register, prepend_to_register, copy_rectangle_to_register, &
              register_read_with_preview, REGISTER_EMPTY, REGISTER_MARKER, REGISTER_TEXT, REGISTER_RECTANGLE, &
              REGISTER_NUMBER, REGISTER_WINDOW_CONFIGURATION, REGISTER_FILE, REGISTER_FILE_QUERY, REGISTER_BUFFER
    public :: run_mode_hooks, fundamental_mode, special_mode, text_mode, prog_mode, normal_mode, set_auto_mode, &
              add_auto_mode, file_name_sans_versions
    public :: history_delete_duplicates, y_or_n_p_use_read_key, use_short_answers, minibuffer_message_timeout, &
              read_file_name_completion_ignore_case, read_buffer_completion_ignore_case, read_char_choice_use_read_key
    public :: tab_width, make_backup_files, change_major_mode_with_file_name, line_move_visual, goal_column, history_length, &
              truncate_partial_width_windows, enable_recursive_minibuffers, history_add_new_input, insert_default_directory, &
              inhibit_field_text_motion, visible_bell, transient_mark_mode, mark_even_if_inactive, kill_ring_max, &
              mark_ring_max, kill_whole_line_option, delete_active_region, kill_read_only_ok, case_fold_search, &
              search_upper_case, search_exit_option, search_nonincremental_instead, search_ring_max, UPPER_CASE_NIL, &
              UPPER_CASE_T, UPPER_CASE_NOT_YANKS, next_screen_context_lines, scroll_error_top_bottom, &
              completion_ignore_case, completion_auto_help, completion_show_inline_help, completion_fail_discreetly, &
              suggest_key_bindings, completion_ignored_extensions, words_include_escapes, search_spaces_regexp, &
              regexp_search_ring_max, search_whitespace_regexp, isearch_lax_whitespace, &
              isearch_regexp_lax_whitespace, paragraph_start, paragraph_separate, sentence_end, sentence_end_base, &
              sentence_end_double_space, sentence_end_without_period, sentence_end_without_space, &
              case_symbols_as_words, case_replace, replace_lax_whitespace, replace_regexp_lax_whitespace, &
              query_replace_skip_read_only, query_replace_show_replacement, query_replace_from_to_separator, &
              fill_prefix, cycle_spacing_actions, cycle_spacing_action_type, CYCLE_JUST_ONE_SPACE, &
              CYCLE_DELETE_SPACE_AFTER, CYCLE_DELETE_SPACE_BEFORE, CYCLE_DELETE_ALL_SPACE, CYCLE_RESTORE, &
              CYCLE_ARG_NONE, CYCLE_ARG_FIXED, CYCLE_ARG_INVERTED, CYCLE_ARG_MINUS, disabled_command_function_p, &
              fill_column, left_margin, auto_fill_function, indent_tabs_mode, standard_indent, tab_always_indent, &
              tab_stop_list, indent_line_function, indent_region_function, colon_double_space, &
              fill_separate_heterogeneous_words_with_space, adaptive_fill_mode, adaptive_fill_regexp, &
              adaptive_fill_first_line_regexp, fill_indent_according_to_mode, default_justification, &
              fill_nobreak_predicate, auto_fill_inhibit_regexp, inhibit_auto_fill, normal_auto_fill_function
    public :: setting_type, integer_setting, logical_setting, string_setting, function_setting, hook_type, &
              change_major_mode_hook, change_major_mode_after_body_hook, after_change_major_mode_hook, &
              major_mode_type, derived_mode_p, provided_mode_derived_p, minor_mode_type, minor_mode_body_proc, &
              major_mode_named, text_mode_hook, text_mode_map, text_mode_syntax_table, prog_mode_hook, &
              prog_mode_map, special_mode_hook, special_mode_map, transient_mark_mode_hook, indent_tabs_mode_hook, &
              auto_fill_mode_hook, help_mode_hook, help_mode_map, turn_on_auto_fill, turn_off_auto_fill, text_mode_variant, &
              parse_sexp_ignore_comments, comment_end_can_be_escaped, multibyte_syntax_as_symbol, &
              open_paren_in_column_0_is_defun_start, comment_use_syntax_ppss, require_final_newline, &
              mode_require_final_newline, FINAL_NEWLINE_NEVER, &
              FINAL_NEWLINE_SAVE, FINAL_NEWLINE_VISIT, FINAL_NEWLINE_VISIT_SAVE, FINAL_NEWLINE_ASK, change_hook_type, &
              after_change_hook_type, change_function, after_change_function, before_change_functions, &
              after_change_functions, first_change_hook, inhibit_modification_hooks
    public :: font_lock_defaults, font_lock_defaults_setting, font_lock_global_modes, font_lock_global_modes_type, &
              font_lock_mode_hook, global_font_lock_mode_hook, turn_on_font_lock, font_lock_keyword_type, &
              font_lock_highlight_type, font_lock_match_highlight_type, font_lock_keywords_type, &
              font_lock_syntax_entry_type, FONT_LOCK_OVERRIDE_NIL, FONT_LOCK_OVERRIDE_T, FONT_LOCK_OVERRIDE_KEEP, &
              FONT_LOCK_OVERRIDE_PREPEND, FONT_LOCK_OVERRIDE_APPEND, font_lock_matcher_proc, font_lock_face_proc, &
              font_lock_form_proc, font_lock_maximum_decoration, FONT_LOCK_DECORATION_MAX, font_lock_verbose, &
              font_lock_dont_widen, font_lock_support_mode, FONT_LOCK_SUPPORT_NONE, FONT_LOCK_SUPPORT_JIT_LOCK, &
              font_lock_extra_managed_props, font_lock_syntactic_face_function, font_lock_keyword, &
              font_lock_highlight, font_lock_anchored, font_lock_add_keywords, FONT_LOCK_ADD_BEGINNING, &
              FONT_LOCK_ADD_END, FONT_LOCK_ADD_SET, jit_lock_chunk_size, fontification_functions, &
              position_hook_type, position_function
    public :: kbd_macro_termination_hook, kmacro_ring_max, kmacro_execute_before_append, kmacro_repeat_no_prefix, &
              kmacro_call_repeat_key, kmacro_call_repeat_with_arg, KMACRO_CALL_REPEAT_DISABLED, &
              KMACRO_CALL_REPEAT_LAST_KEY, edmacro_eight_bits
    public :: dired, dired_other_window, dired_noselect, dired_mode, dired_mode_map, dired_mode_hook, &
              dired_before_readin_hook, dired_after_readin_hook, dired_initial_position_hook, dired_listing_switches, &
              dired_use_ls_dired, DIRED_USE_LS_DIRED_NIL, DIRED_USE_LS_DIRED_T, DIRED_USE_LS_DIRED_UNSPECIFIED, &
              dired_ls_f_marks_symlinks, dired_trivial_filenames, dired_free_space, DIRED_FREE_SPACE_NIL, &
              DIRED_FREE_SPACE_SEPARATE, DIRED_FREE_SPACE_FIRST, dired_mark_region, DIRED_MARK_REGION_NIL, &
              DIRED_MARK_REGION_FILE, DIRED_MARK_REGION_LINE, dired_movement_style, DIRED_MOVEMENT_STYLE_NIL, &
              DIRED_MOVEMENT_STYLE_CYCLE, DIRED_MOVEMENT_STYLE_BOUNDED, DIRED_MOVEMENT_STYLE_CYCLE_FILES, &
              DIRED_MOVEMENT_STYLE_BOUNDED_FILES, dired_auto_revert_buffer, dired_check_symlinks, &
              dired_recursive_deletes, DIRED_RECURSIVE_NEVER, DIRED_RECURSIVE_ALWAYS, DIRED_RECURSIVE_TOP, &
              DIRED_RECURSIVE_ASK, dired_no_confirm, dired_no_confirm_type, dired_switches_in_mode_line, &
              DIRED_SWITCHES_SORT_ORDER, DIRED_SWITCHES_AS_IS, dired_sort_inhibit, dired_garbage_files_regexp, &
              dired_always_read_filesystem, dired_deletion_confirmer, dired_header_face, dired_mark_face, &
              dired_marked_face, dired_flagged_face, dired_warning_face, dired_perm_write_face, &
              dired_directory_face, dired_symlink_face, dired_ignored_face, insert_directory_program, &
              find_file_run_dired, find_file_other_window, revert_buffer
end module fortmacs
