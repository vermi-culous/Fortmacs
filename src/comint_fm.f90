! comint_fm.f90 -- general command interpreter in a window stuff.
! Ported from GNU Emacs 31.1 lisp/comint.el.
!
! Copyright (C) 1988, 1990, 1992-2026 Free Software Foundation, Inc.
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

!> Comint mode: a buffer talking to an interactive subprocess.  Output
!> goes in through `comint_output_filter' (carriage motion, the prompt
!> highlighted and marked as output fields); RET (`comint_send_input')
!> sends what follows the process mark, keeping it in the input ring
!> (M-p, M-n, C-c M-r, C-c M-s); C-c C-c, C-c C-\ and C-c C-d interrupt,
!> quit and end the input of the current subjob; C-c C-o, C-c C-r and
!> C-c C-e work on the last output.
!>
!> A comint buffer's own state (the input ring, its markers, its input
!> sender and old-input function) is kept by buffer here; the input ring
!> and its indices are permanent, as upstream's are.
!>
!> Not ported yet (Deferred): input history expansion
!> (`comint_input_autoexpand', `comint_replace_by_expanded_history',
!> `comint_magic_space', `comint_arguments' and
!> `comint_insert_previous_argument' (C-c .)); completion
!> (`comint_completion_at_point', file name completion, the
!> `comint_dynamic_*' commands, `comint_dynamic_list_input_ring' (C-c C-l),
!> `comint_complete_input_ring' (C-x <up>)); Isearch in the input
!> history (`comint_history_isearch_backward_regexp' on M-r,
!> `comint_history_isearch'); password prompts
!> (`comint_watch_for_password_prompt', `comint_send_invisible',
!> `comint_password_prompt_regexp'), which upstream has in
!> `comint_output_filter_functions'; ANSI colors
!> (`ansi_color_process_output', also there); stopped processes
!> (`comint_stop_subjob' on C-c C-z, `comint_continue_subjob'), which
!> need process_fm's stopped processes; `comint_scroll_to_bottom_on_input'
!> and `comint_preinput_scroll_to_bottom' (a buffer's own
!> `pre_command_hook'); redirection (`comint_redirect_*'); network
!> connections and ptys without a program as PROGRAM; `comint_fontify_input_mode'
!> and indirect buffers; the mouse (`comint_insert_input'), menus and
!> `comint_repeat_map'; `insert_in_front_hooks' on output (the
!> property is set, text property hooks are not ported);
!> `filter_buffer_substring_function'; `window_point_insertion_type';
!> `comint_osc_process_output'; source file commands
!> (`comint_get_source' and friends); `comint_run' with switches read
!> after C-u (an error says so); coding systems.
module comint_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, format_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, nilp, t_value, list, consp, symbolp, symbols, xsymbol, list_elements, &
                       command_proc
    use lread_fm, only: sym
    use data_fm, only: string_setting, logical_setting, integer_setting, symbol_value, set, value_eq, &
                       add_local_variables_killer, forward_setting
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, buffer_live_p, &
                         barf_if_buffer_read_only
    use marker_fm, only: make_marker, free_marker, marker_buffer, marker_position, set_marker, copy_marker_at, &
                         detach_marker
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, goto_char, widen, narrow_to_region, delete_region, &
                          buffer_substring, line_beginning_position, line_end_position, following_char, eobp, &
                          bobp, bolp, field_beginning, field_end, field_string, constrain_to_field, &
                          inhibit_field_text_motion
    use textprop_fm, only: put_text_property, get_text_property, add_text_properties, remove_list_of_text_properties, &
                           remove_text_properties, next_single_char_property_change, previous_single_char_property_change
    use cmds_fm, only: delete_char, forward_char, forward_line, beginning_of_line, end_of_line
    use syntax_fm, only: skip_chars_forward
    use search_fm, only: looking_at, string_match, re_search_backward, match_beginning, match_end, regexp_quote, &
                         SEARCH_NOERROR
    use subr_fm, only: silent_modifications_type, silent_modifications_begin, silent_modifications_end, match_string
    use eval_fm, only: hook_type, string_hook_type, string_filter_hook_type
    use keymap_fm, only: keymap_set, lookup_key, key_description, command_remapping, current_global_map
    use modes_fm, only: major_modes
    use simple_fm, only: kill_region, copy_region_as_kill, push_mark, kill_whole_line
    use process_fm, only: process_arg_type, make_process, get_buffer_process, process_status, process_mark, &
                          process_buffer, delete_process, set_process_filter, process_send_string, process_send_region, &
                          process_send_eof, interrupt_process, kill_process, quit_process, accept_process_output, &
                          processp, process_connection_type, PROCESS_CONNECTION_PIPE, PROCESS_CONNECTION_PTY
    use window_fm, only: windows, window_type, window_point, set_window_point, set_window_start, recenter, &
                         window_leaves, window_text_cols, select_window, same_handle
    use frame_fm, only: selected_frame
    use xdisp_fm, only: message, set_mode_line_process, mode_line_part_type
    use font_core_fm, only: font_lock_defaults
    use font_lock_fm, only: font_lock_append_text_property, font_lock_remove_face_from_text_property
    use faces_fm, only: named_face_type, display_when, face_attrs
    use fileio_fm, only: file_readable_p, file_writable_p, file_accessible_directory_p, insert_file_contents, &
                         write_region, expand_file_name
    use minibuf_fm, only: read_from_minibuffer, read_string
    use minibuffer_fm, only: read_file_name
    use paragraphs_fm, only: paragraph_start, forward_paragraph
    use callint_fm, only: current_prefix_arg, prefix_numeric_value, call_interactively
    use keyboard_fm, only: last_command, this_command_keys
    use dispnew_fm, only: ding
    use commands_fm, only: commands
    use ring_fm, only: ring_type
    use version_fm, only: upstream_version
    implicit none(type, external)
    private

    public :: comint_mode, syms_of_comint, comint_check_proc, make_comint_in_buffer, make_comint, comint_run, &
              comint_exec, comint_term_environment, comint_nonblank_p, comint_output_filter, comint_carriage_motion, &
              comint_send_input, comint_simple_send, comint_send_string, comint_send_region, comint_snapshot_last_prompt, &
              comint_get_old_input_default, comint_copy_old_input, comint_skip_prompt, comint_after_pmark_p, &
              comint_line_beginning_position, comint_bol, comint_add_to_input_history, comint_previous_input, &
              comint_next_input, comint_previous_matching_input, comint_next_matching_input, &
              comint_previous_matching_input_from_input, comint_next_matching_input_from_input, comint_restore_input, &
              comint_delete_input, comint_delete_output, comint_write_output, comint_append_output_to_file, &
              comint_show_output, comint_show_maximum_output, comint_clear_buffer, comint_truncate_buffer, &
              comint_strip_ctrl_m, comint_interrupt_subjob, comint_kill_subjob, comint_quit_subjob, comint_kill_input, &
              comint_delchar_or_maybe_eof, comint_send_eof, comint_backward_matching_input, &
              comint_forward_matching_input, comint_next_prompt, comint_previous_prompt, comint_update_fence, &
              comint_kill_whole_line, comint_kill_region, comint_get_next_from_history, comint_accumulate, &
              comint_goto_process_mark, comint_bol_or_process_mark, comint_set_process_mark, comint_read_input_ring, &
              comint_write_input_ring, comint_postoutput_scroll_to_bottom, comint_adjust_window_point, &
              comint_input_ring_item, comint_input_ring_length, comint_last_input_start, comint_last_input_end, &
              comint_last_output_start, set_comint_get_old_input, set_comint_input_sender, set_comint_input_filter, &
              comint_ptyp, comint_arguments, comint_delim_arg, comint_substitute_in_file_name
    public :: comint_input_sender_proc, comint_get_old_input_proc, comint_input_filter_proc

    abstract interface
        !> A `comint_input_sender': send to PROCESS the input STRING.
        subroutine comint_input_sender_proc(process, string, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: process
            character(len=*), intent(in) :: string
            type(status_type), intent(out) :: status
        end subroutine comint_input_sender_proc

        !> A `comint_get_old_input' function: the old text at point, as
        !> INPUT.
        subroutine comint_get_old_input_proc(input, status)
            import :: status_type
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: input
            type(status_type), intent(out) :: status
        end subroutine comint_get_old_input_proc

        !> A `comint_input_filter': whether INPUT may be saved on the input
        !> history.
        logical function comint_input_filter_proc(input)
            implicit none(type, external)
            character(len=*), intent(in) :: input
        end function comint_input_filter_proc
    end interface

    !> Values of `comint_move_point_for_output'.
    integer, parameter, public :: COMINT_MOVE_NIL = 0, COMINT_MOVE_T = 1, COMINT_MOVE_ALL = 2, &
                                  COMINT_MOVE_THIS = 3, COMINT_MOVE_OTHERS = 4

    !> Values of `comint_move_point_for_matching_input'.
    integer, parameter, public :: COMINT_MATCHING_AFTER_INPUT = 0, COMINT_MATCHING_END_OF_LINE = 1

    !> Regexp to recognize prompts in the inferior process.
    !> Defaults to "^", the null string at BOL.
    !>
    !> This variable is only used if the variable
    !> `comint_use_prompt_regexp' is true.
    !>
    !> Good choices:
    !>   Canonical Lisp: "^[^> \n]*>+:? *" (Lucid, franz, kcl, T, cscheme, oaklisp)
    !>   Lucid Common Lisp: "^\\(>\\|\\(->\\)+\\) *"
    !>   franz: "^\\(->\\|<[0-9]*>:\\) *"
    !>   kcl: "^>+ *"
    !>   shell: "^[^#$%>\n]*[#$%>] *"
    !>   T: "^>+ *"
    !>
    !> This is a good thing to set in mode hooks.  Port of comint.el
    !> comint-prompt-regexp.
    type(string_setting), target, public :: comint_prompt_regexp = string_setting(name="comint_prompt_regexp")

    !> If true, the comint prompt is read only.
    !> The read only region includes the newline before the prompt.
    !> This does not affect existing prompts.
    !> Certain derived modes may override this option.
    !>
    !> If you set this option to true, then the safe way to temporarily
    !> override the read-only-ness of comint prompts is to call
    !> `comint_kill_whole_line' or `comint_kill_region' with no
    !> narrowing in effect.  This way you will be certain that none of
    !> the remaining prompts will be accidentally messed up.  Port of
    !> comint.el comint-prompt-read-only.
    type(logical_setting), target, public :: comint_prompt_read_only = &
                                             logical_setting(name="comint_prompt_read_only", value=.false.)

    !> If true, highlight input with `comint_highlight_input' face.
    !> Otherwise keep the original highlighting untouched.  Port of
    !> comint.el comint-highlight-input.
    type(logical_setting), target, public :: comint_highlight_input = &
                                             logical_setting(name="comint_highlight_input", value=.true.)

    !> If true, don't add input matching the last on the input ring.
    !> This mirrors the optional behavior of bash.
    !>
    !> This variable is buffer-local.  Port of comint.el
    !> comint-input-ignoredups.
    type(logical_setting), target, public :: comint_input_ignoredups = &
                                             logical_setting(name="comint_input_ignoredups", value=.false.)

    !> If allocated, name of the file to read/write input history.
    !> See also `comint_read_input_ring' and `comint_write_input_ring'.
    !> `comint_mode' makes this a buffer-local variable.  You probably want
    !> to set this in a mode hook, rather than customize the default
    !> value.  Port of comint.el comint-input-ring-file-name.
    type(string_setting), target, public :: comint_input_ring_file_name = &
                                            string_setting(name="comint_input_ring_file_name")

    !> If allocated, the program to use for pagination of program output.
    !> If unallocated, use the default pager.
    !>
    !> Some programs produce large amounts of output, and have provision
    !> for pagination of their output through a filter program, commonly
    !> known as a "pager".  The pager limits the amount of output produced
    !> and allows the user to interactively browse the output one page at
    !> a time.  Some programs paginate their output by default, by always
    !> starting a pager.  The program they use as the pager is specified
    !> by the environment variable PAGER; if that variable is not defined,
    !> they use some fixed default, such as "less".
    !>
    !> The interactive browsing aspects of pagination are not needed, and
    !> get in the way, when the output of the program is directed to an
    !> Emacs buffer, so in those cases pagination might need to be
    !> disabled.  Disabling pagination means that some programs will
    !> produce large amounts of output, but most such programs have other
    !> ways to limit their output, such as additional arguments or Emacs
    !> interfaces.  To disable pagination, this variable's value should be
    !> a string that names a program, such as "cat", which passes through
    !> all of the output without any filtering or delays.  Comint will then
    !> set the PAGER variable to name that program, when it invokes
    !> external programs.  Port of comint.el comint-pager.
    type(string_setting), target, public :: comint_pager = string_setting(name="comint_pager")

    !> The prefix to skip when parsing the input ring file.
    !> This is useful in Zsh when the extended_history option is on.  Port
    !> of comint.el comint-input-ring-file-prefix.
    type(string_setting), target, public :: comint_input_ring_file_prefix = &
                                            string_setting(name="comint_input_ring_file_prefix")

    !> Controls whether interpreter output moves point to the end of the
    !> output.
    !> If COMINT_MOVE_NIL, then output never moves point to the output.
    !>  (If the output occurs at point, it is inserted before point.)
    !> If COMINT_MOVE_T or COMINT_MOVE_ALL, move point in all windows
    !> showing the buffer.
    !> If COMINT_MOVE_THIS, move point only the selected window.
    !> If COMINT_MOVE_OTHERS, move point only in other windows, not in the
    !> selected window.
    !>
    !> The default is COMINT_MOVE_NIL.
    !>
    !> See the variable `comint_scroll_show_maximum_output' and the
    !> function `comint_postoutput_scroll_to_bottom'.
    !> This variable is buffer-local in all Comint buffers.  Port of
    !> comint.el comint-move-point-for-output.
    type(integer_setting), target, public :: comint_move_point_for_output = &
                                             integer_setting(name="comint_move_point_for_output", value=COMINT_MOVE_NIL)

    !> Controls where to place point after matching input.
    !> \<comint_mode_map>This influences the commands
    !> \[comint_previous_matching_input_from_input] and
    !> \[comint_next_matching_input_from_input].
    !> If COMINT_MATCHING_AFTER_INPUT, point will be positioned after the
    !> input typed by the user, but before the rest of the history entry
    !> that has been inserted.  If COMINT_MATCHING_END_OF_LINE, point will
    !> be positioned at the end of the current logical (not visual) line
    !> after insertion.  Port of comint.el
    !> comint-move-point-for-matching-input.
    type(integer_setting), target, public :: comint_move_point_for_matching_input = &
                                             integer_setting(name="comint_move_point_for_matching_input", &
                                                             value=COMINT_MATCHING_AFTER_INPUT)

    !> Controls how to scroll due to interpreter output.
    !> This variable applies when point is at the end of the buffer
    !> (either because it was originally there, or because
    !> `comint_move_point_for_output' said to move it there)
    !> and output from the subprocess is inserted.
    !>
    !> True means scroll so that the window is full of text
    !> and point is on the last line.  A value of false
    !> means don't do anything special--scroll normally.
    !>
    !> See also the variable `comint_move_point_for_output' and the function
    !> `comint_postoutput_scroll_to_bottom'.
    !> This variable is buffer-local in all Comint buffers.  Port of
    !> comint.el comint-scroll-show-maximum-output.
    type(logical_setting), target, public :: comint_scroll_show_maximum_output = &
                                             logical_setting(name="comint_scroll_show_maximum_output", value=.true.)

    !> The maximum size in lines for Comint buffers.
    !> Comint buffers are truncated from the top to be no greater than this
    !> number, if the function `comint_truncate_buffer' is on
    !> `comint_output_filter_functions'.  Port of comint.el
    !> comint-buffer-maximum-size.
    type(integer_setting), target, public :: comint_buffer_maximum_size = &
                                             integer_setting(name="comint_buffer_maximum_size", value=1024)

    !> Size of the input history ring in `comint_mode'.  Port of comint.el
    !> comint-input-ring-size.
    type(integer_setting), target, public :: comint_input_ring_size = &
                                             integer_setting(name="comint_input_ring_size", value=500)

    !> Separator between commands in the history file.  Port of comint.el
    !> comint-input-ring-separator.
    type(string_setting), target, public :: comint_input_ring_separator = &
                                            string_setting(name="comint_input_ring_separator")

    !> Regexp for history entries that should be ignored when Comint
    !> initializes.  Port of comint.el comint-input-history-ignore.
    type(string_setting), target, public :: comint_input_history_ignore = &
                                            string_setting(name="comint_input_history_ignore")

    !> If true, assume that the subprocess echoes any input.
    !> If so, delete one copy of the input so that only one copy eventually
    !> appears in the buffer.
    !>
    !> This variable is buffer-local.  Port of comint.el
    !> comint-process-echoes.
    type(logical_setting), target, public :: comint_process_echoes = &
                                             logical_setting(name="comint_process_echoes", value=.false.)

    !> True means go to the end of the line before sending input.
    !> See `comint_send_input'.  Port of comint.el comint-eol-on-send.
    type(logical_setting), target, public :: comint_eol_on_send = &
                                             logical_setting(name="comint_eol_on_send", value=.true.)

    !> If true, use `comint_prompt_regexp' to recognize prompts.
    !> If false, then program output and user-input are given different
    !> `field' properties, which Emacs commands can use to distinguish them
    !> (in particular, common movement commands such as
    !> `beginning_of_line' respect field boundaries in a natural way).
    !> Port of comint.el comint-use-prompt-regexp.
    type(logical_setting), target, public :: comint_use_prompt_regexp = &
                                             logical_setting(name="comint_use_prompt_regexp", value=.false.)

    !> Value to use for TERM when the system uses terminfo.
    !> If the system's terminfo database contains a definition for the
    !> "dumb-emacs-ansi" terminal (as all recent versions of terminfo do),
    !> set this to "dumb-emacs-ansi" and then some terminfo-aware programs
    !> will send colorized output when run under Comint.  Port of comint.el
    !> comint-terminfo-terminal.
    type(string_setting), target, public :: comint_terminfo_terminal = &
                                            string_setting(name="comint_terminfo_terminal")

    !> If false, Comint will interpret `carriage control' characters in
    !> output.  See `comint_carriage_motion' for details.  Port of
    !> comint.el comint-inhibit-carriage-motion.
    type(logical_setting), target, public :: comint_inhibit_carriage_motion = &
                                             logical_setting(name="comint_inhibit_carriage_motion", value=.false.)

    !> The characters to recognize as separate arguments in input.
    !> Strings comprising a character in this list will separate the
    !> arguments surrounding them, and also be regarded as arguments in
    !> their own right (unlike whitespace).  See `comint_arguments'.
    !> Defaults to the empty string.
    !>
    !> For shells, a good value is "|&<>();".
    !>
    !> This is a good thing to set in mode hooks.  Port of comint.el
    !> comint-delimiter-argument-list (the characters as a string).
    type(string_setting), target, public :: comint_delimiter_argument_list = &
                                            string_setting(name="comint_delimiter_argument_list")

    !> True directs the `comint_input_sender' function not to send a
    !> newline.  Port of comint.el comint-input-sender-no-newline.
    logical, public :: comint_input_sender_no_newline = .false.

    !> Hook run upon entry to `comint_mode'.
    !> This is run before the process is cranked up.  Port of comint.el
    !> comint-mode-hook.
    type(hook_type), target, public :: comint_mode_hook = hook_type(name="comint_mode_hook")

    !> Hook run each time a process is exec'd by `comint_exec'.
    !> This is called after the process is cranked up.  It is useful for
    !> things that must be done each time a process is executed in a
    !> Comint mode buffer (e.g., `set_process_query_on_exit_flag').  In
    !> contrast, `comint_mode_hook' is only executed once, when the buffer
    !> is created.  Port of comint.el comint-exec-hook.
    type(hook_type), target, public :: comint_exec_hook = hook_type(name="comint_exec_hook")

    !> Functions to call after output is inserted into the buffer.
    !> One possible function is `comint_postoutput_scroll_to_bottom'.
    !> These functions get one argument, a string containing the text as
    !> originally inserted.  Note that this might not be the same as the
    !> buffer contents between `comint_last_output_start' and the buffer's
    !> `process_mark', if other filter functions have already modified the
    !> buffer.
    !>
    !> See also `comint_preoutput_filter_functions'.
    !>
    !> You can use `add' to add functions to this hook either globally or
    !> locally.  Port of comint.el comint-output-filter-functions (its
    !> default value without the Deferred `ansi_color_process_output' and
    !> `comint_watch_for_password_prompt').
    type(string_hook_type), target, public :: comint_output_filter_functions = &
                                              string_hook_type(name="comint_output_filter_functions", &
                                                               permanent_local=.true.)

    !> Hook of functions to call before inserting Comint output into the
    !> buffer.  Each function gets one argument, a string containing the
    !> text received from the subprocess.  It should return the string to
    !> insert, perhaps the same string that was received, or perhaps a
    !> modified or transformed string.
    !>
    !> The functions are called sequentially, and each one is given the
    !> string returned by the previous one.  The string returned by the
    !> last function is the text that is actually inserted in the
    !> redirection buffer.
    !>
    !> You can use `add' to add functions to this hook either globally or
    !> locally.  Port of comint.el comint-preoutput-filter-functions.
    type(string_filter_hook_type), target, public :: comint_preoutput_filter_functions = &
                                                     string_filter_hook_type(name="comint_preoutput_filter_functions", &
                                                                             permanent_local=.true.)

    !> Abnormal hook run before input is sent to the process.
    !> These functions get one argument, a string containing the text to
    !> send.  Port of comint.el comint-input-filter-functions.
    type(string_hook_type), target, public :: comint_input_filter_functions = &
                                              string_hook_type(name="comint_input_filter_functions", &
                                                               permanent_local=.true.)

    !> Keymap for Comint mode.  Port of comint.el comint-mode-map (the
    !> keys of ported commands).
    type(handle_type), public :: comint_mode_map

    !> Face to use to highlight user input: the face `comint_highlight_input'
    !> (the variable of that name is the setting).  Port of
    !> comint.el:comint-highlight-input (the face).
    type(named_face_type), target, public :: comint_highlight_input_face

    !> Face to use to highlight prompts.  Port of
    !> comint.el:comint-highlight-prompt.
    type(named_face_type), target, public :: comint_highlight_prompt

    !> A Comint buffer's own state: upstream's buffer-local variables
    !> comint-input-ring, comint-input-ring-index,
    !> comint-save-input-ring-index, comint-ptyp (permanent), and
    !> comint-last-input-start, comint-last-input-end,
    !> comint-last-output-start, comint-accum-marker, comint-last-prompt,
    !> comint-stored-incomplete-input,
    !> comint-matching-input-from-input-string, comint-get-old-input,
    !> comint-input-sender and comint-input-filter.
    type :: comint_buffer_type
        integer(int64) :: serial = 0
        type(handle_type) :: buffer
        type(ring_type) :: input_ring
        logical :: has_ring = .false.
        !> -1 for nil.
        integer :: input_ring_index = -1, save_input_ring_index = -1
        logical :: ptyp = .true.
        !> The variables comint-mode sets (killed with the local variables).
        logical :: local = .false.
        type(handle_type) :: last_input_start, last_input_end, last_output_start, accum_marker
        logical :: has_last_prompt = .false.
        type(handle_type) :: last_prompt_start, last_prompt_end
        character(len=:), allocatable :: stored_incomplete_input
        character(len=:), allocatable :: matching_input_from_input_string
        procedure(comint_get_old_input_proc), pointer, nopass :: get_old_input => null()
        procedure(comint_input_sender_proc), pointer, nopass :: input_sender => null()
        procedure(comint_input_filter_proc), pointer, nopass :: input_filter => null()
    end type comint_buffer_type

    type(comint_buffer_type), allocatable :: comint_buffers(:)

    !> comint--prompt-rear-nonsticky: text properties set on the prompt
    !> that are not to leak past it.
    character(len=*), parameter :: PROMPT_REAR_NONSTICKY(5) = [character(len=31) :: "field", &
                                                                                   "inhibit_line_move_field_capture", "read_only", &
                                                                                          "font_lock_face", "insert_in_front_hooks"]

contains

    ! ------------------------------------------------------------------
    ! The buffers' own state
    ! ------------------------------------------------------------------

    !> The entry of buffer slot BI (0 for none).
    integer function entry_of(bi) result(k)
        integer(int64), intent(in) :: bi
        if (allocated(comint_buffers) .and. bi > 0) then
            do k = 1, size(comint_buffers)
                if (comint_buffers(k)%serial == buffers(bi)%serial) return
            end do
        end if
        k = 0
    end function entry_of

    !> The entry of the current buffer, made (empty) if needed.  Entries of
    !> dead buffers are dropped then.
    integer function current_entry() result(k)
        type(comint_buffer_type), allocatable :: kept(:)
        integer :: i, n
        k = entry_of(cur)
        if (k /= 0) return
        if (.not. allocated(comint_buffers)) allocate (comint_buffers(0))
        n = 0
        allocate (kept(size(comint_buffers) + 1))
        do i = 1, size(comint_buffers)
            if (buffer_live_p(comint_buffers(i)%buffer)) then
                n = n + 1
                call move_entry(comint_buffers(i), kept(n))
            else
                call free_locals(comint_buffers(i))
            end if
        end do
        n = n + 1
        kept(n)%serial = buffers(cur)%serial
        kept(n)%buffer = current_buffer()
        call move_alloc(kept, comint_buffers)
        comint_buffers = comint_buffers(1:n)
        k = n
    end function current_entry

    subroutine move_entry(from, to)
        type(comint_buffer_type), intent(inout) :: from
        type(comint_buffer_type), intent(out) :: to
        to = from
    end subroutine move_entry

    !> Free the markers of entry E and forget its local variables.
    subroutine free_locals(e)
        type(comint_buffer_type), intent(inout) :: e
        type(status_type) :: st
        if (.not. e%local) return
        call free_marker(e%last_input_start, st)
        call free_marker(e%last_input_end, st)
        call free_marker(e%last_output_start, st)
        call free_marker(e%accum_marker, st)
        call forget_last_prompt(e)
        if (allocated(e%stored_incomplete_input)) deallocate (e%stored_incomplete_input)
        e%matching_input_from_input_string = ""
        e%get_old_input => null()
        e%input_sender => null()
        e%input_filter => null()
        e%local = .false.
    end subroutine free_locals

    subroutine forget_last_prompt(e)
        type(comint_buffer_type), intent(inout) :: e
        type(status_type) :: st
        if (.not. e%has_last_prompt) return
        call free_marker(e%last_prompt_start, st)
        call free_marker(e%last_prompt_end, st)
        e%has_last_prompt = .false.
    end subroutine forget_last_prompt

    !> Forget the current buffer's local Comint variables (not the
    !> permanent ones), as `kill_all_local_variables' does.
    subroutine kill_comint_locals()
        integer :: k
        k = entry_of(cur)
        if (k /= 0) call free_locals(comint_buffers(k))
    end subroutine kill_comint_locals

    !> The current buffer's entry with its local variables (made as
    !> comint-mode makes them if the buffer has none).
    integer function local_entry() result(k)
        k = current_entry()
        if (.not. comint_buffers(k)%local) call make_locals(k)
    end function local_entry

    !> Make the local variables of entry K, as comint-mode does.
    subroutine make_locals(k)
        integer, intent(in) :: k
        type(status_type) :: st
        associate (e => comint_buffers(k))
            call copy_marker_at(point_min(), e%last_input_start, .false., st)
            call copy_marker_at(point_min(), e%last_input_end, .false., st)
            call make_marker(e%last_output_start)
            call make_marker(e%accum_marker)
            e%has_last_prompt = .false.
            e%matching_input_from_input_string = ""
            e%get_old_input => comint_get_old_input_default
            e%input_sender => comint_simple_send
            e%input_filter => comint_nonblank_p
            e%local = .true.
        end associate
    end subroutine make_locals

    !> Whether the current buffer has a local input ring (comint-input-ring).
    logical function has_input_ring()
        integer :: k
        has_input_ring = .false.
        k = entry_of(cur)
        if (k /= 0) has_input_ring = comint_buffers(k)%has_ring
    end function has_input_ring

    !> The number of items of the current buffer's input ring.
    integer function comint_input_ring_length()
        integer :: k
        comint_input_ring_length = 0
        k = entry_of(cur)
        if (k == 0) return
        if (comint_buffers(k)%has_ring) comint_input_ring_length = comint_buffers(k)%input_ring%count()
    end function comint_input_ring_length

    !> Item INDEX (0 the newest) of the current buffer's input ring ("" if
    !> it has none).
    function comint_input_ring_item(index) result(item)
        integer, intent(in) :: index
        character(len=:), allocatable :: item
        type(status_type) :: st
        integer :: k
        item = ""
        k = entry_of(cur)
        if (k == 0) return
        if (comint_buffers(k)%has_ring) call comint_buffers(k)%input_ring%ref(index, item, st)
    end function comint_input_ring_item

    !> Position of a marker of the current buffer's state (0 if unset).
    integer(int64) function marker_pos(m) result(pos)
        type(handle_type), intent(in) :: m
        type(status_type) :: st
        pos = 0
        if (m%index == 0) return
        call marker_position(m, pos, st)
        if (.not. st%ok()) pos = 0
    end function marker_pos

    !> The position of comint-last-input-start in the current buffer.
    integer(int64) function comint_last_input_start()
        integer :: k
        comint_last_input_start = 0
        k = entry_of(cur)
        if (k /= 0) comint_last_input_start = marker_pos(comint_buffers(k)%last_input_start)
    end function comint_last_input_start

    !> The position of comint-last-input-end in the current buffer.
    integer(int64) function comint_last_input_end()
        integer :: k
        comint_last_input_end = 0
        k = entry_of(cur)
        if (k /= 0) comint_last_input_end = marker_pos(comint_buffers(k)%last_input_end)
    end function comint_last_input_end

    !> The position of comint-last-output-start in the current buffer.
    integer(int64) function comint_last_output_start()
        integer :: k
        comint_last_output_start = 0
        k = entry_of(cur)
        if (k /= 0) comint_last_output_start = marker_pos(comint_buffers(k)%last_output_start)
    end function comint_last_output_start

    !> True if communications via pty; false if by pipe.  Buffer local.
    !> This is to work around a bug in Emacs process signaling.  Port of
    !> comint.el comint-ptyp.
    logical function comint_ptyp()
        integer :: k
        comint_ptyp = .true.
        k = entry_of(cur)
        if (k /= 0) comint_ptyp = comint_buffers(k)%ptyp
    end function comint_ptyp

    !> Give the current buffer its own `comint_get_old_input': the
    !> function that returns old text in Comint mode.
    !> This function is called when return is typed while the point is in
    !> old text.  It returns the text to be submitted as process input.
    !> The default is `comint_get_old_input_default', which either grabs
    !> the current input field or grabs the current line and strips off
    !> leading text matching `comint_prompt_regexp', depending on the value
    !> of `comint_use_prompt_regexp'.  Port of the setq-local of comint.el
    !> comint-get-old-input.
    subroutine set_comint_get_old_input(fn)
        procedure(comint_get_old_input_proc) :: fn
        integer :: k
        if (cur == 0) return
        k = local_entry()
        comint_buffers(k)%get_old_input => fn
    end subroutine set_comint_get_old_input

    !> Give the current buffer its own `comint_input_sender': the
    !> function to actually send to PROCESS the STRING submitted by user.
    !> Usually this is just `comint_simple_send', but if your mode needs
    !> to massage the input string, put a different function here.
    !> `comint_simple_send' just sends the string plus a newline.
    !> (If `comint_input_sender_no_newline' is true, it omits the newline.)
    !> This is called from the user command `comint_send_input'.  Port of
    !> the setq-local of comint.el comint-input-sender.
    subroutine set_comint_input_sender(fn)
        procedure(comint_input_sender_proc) :: fn
        integer :: k
        if (cur == 0) return
        k = local_entry()
        comint_buffers(k)%input_sender => fn
    end subroutine set_comint_input_sender

    !> Give the current buffer its own `comint_input_filter': the
    !> predicate for filtering additions to input history.
    !> Takes one argument, the input.  If true, the input may be saved on
    !> the input history list.  Default is to save anything that isn't all
    !> whitespace.  Port of the setq-local of comint.el comint-input-filter.
    subroutine set_comint_input_filter(fn)
        procedure(comint_input_filter_proc) :: fn
        integer :: k
        if (cur == 0) return
        k = local_entry()
        comint_buffers(k)%input_filter => fn
    end subroutine set_comint_input_filter

    ! ------------------------------------------------------------------
    ! Small helpers
    ! ------------------------------------------------------------------

    !> Bind `inhibit_read_only' to true, keeping its value in SAVED.
    subroutine inhibit_read_only_begin(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st
        call symbol_value(sym("inhibit_read_only"), saved, st)
        call set(sym("inhibit_read_only"), t_value(), st)
    end subroutine inhibit_read_only_begin

    !> End the binding of `inhibit_read_only_begin'.
    subroutine inhibit_read_only_end(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st
        call set(sym("inhibit_read_only"), saved, st)
    end subroutine inhibit_read_only_end

    !> The current buffer's process, or an error: "Current buffer has no
    !> process".
    subroutine current_process(proc, status)
        type(handle_type), intent(out) :: proc
        type(status_type), intent(out) :: status
        status = status_type()
        proc = get_buffer_process(current_buffer())
        if (proc%index == 0) call set_error(status, FM_USER_ERROR, "Current buffer has no process")
    end subroutine current_process

    !> The position of the process mark of PROC (0 if it points nowhere).
    integer(int64) function pmark_pos(proc)
        type(handle_type), intent(in) :: proc
        pmark_pos = marker_pos(process_mark(proc))
    end function pmark_pos

    !> The property list (rear-nonsticky comint--prompt-rear-nonsticky).
    function rear_nonsticky_plist() result(v)
        type(value_type) :: v
        v = list(sym("rear_nonsticky"), list(sym(trim(PROMPT_REAR_NONSTICKY(1))), &
                                             sym(trim(PROMPT_REAR_NONSTICKY(2))), &
                                             sym(trim(PROMPT_REAR_NONSTICKY(3))), &
                                             sym(trim(PROMPT_REAR_NONSTICKY(4))), &
                                             sym(trim(PROMPT_REAR_NONSTICKY(5)))))
    end function rear_nonsticky_plist

    !> Whether the symbol V is named NAME.
    logical function symbol_is(v, name)
        type(value_type), intent(in) :: v
        character(len=*), intent(in) :: name
        symbol_is = .false.
        if (.not. symbolp(v)) return
        if (nilp(v)) return
        symbol_is = symbols(xsymbol(v))%name == name
    end function symbol_is

    !> The `field' property of the character at POS (get-char-property).
    function field_at(pos) result(v)
        integer(int64), intent(in) :: pos
        type(value_type) :: v
        type(status_type) :: st
        call get_text_property(pos, sym("field"), v, st)
    end function field_at

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Major mode for interacting with an inferior interpreter.
    !> Interpreter name is same as buffer name, sans the asterisks.
    !> Return at end of buffer sends line as input.
    !> Return not at end copies rest of line to end and sends it.
    !> Setting variable `comint_eol_on_send' means jump to the end of the
    !> line before submitting new input.
    !>
    !> This mode is customized to create major modes such as Inferior Lisp
    !> mode, Shell mode, etc.  This can be done by setting the hooks
    !> `comint_input_filter_functions', `comint_input_filter',
    !> `comint_input_sender' and `comint_get_old_input' to appropriate
    !> functions, and the variable `comint_prompt_regexp' to the
    !> appropriate regular expression.
    !>
    !> The mode maintains an input history of size `comint_input_ring_size'.
    !> You can access this with the commands \[comint_next_input] and
    !> \[comint_previous_input].
    !> Addition is controlled by the variable `comint_input_ignoredups'.
    !>
    !> Input to, and output from, the subprocess can cause the window to
    !> scroll to the end of the buffer.  See variables
    !> `comint_output_filter_functions', `comint_preoutput_filter_functions'
    !> and `comint_move_point_for_output'.
    !>
    !> Entry to this mode runs the hooks on `comint_mode_hook'.  Port of
    !> comint.el:comint-mode.
    recursive subroutine comint_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        if (.not. associated(major_modes%comint_mode%run)) call syms_of_comint()
        call major_modes%comint_mode%begin(saved, status)
        if (.not. status%ok()) return
        call comint_mode_body()
        call major_modes%comint_mode%finish(saved, status)
    end subroutine comint_mode

    !> The body of comint-mode (also run by the modes derived from it,
    !> through their parent).
    subroutine comint_mode_body()
        type(mode_line_part_type) :: parts(1)
        integer :: k
        parts(1)%text = ":%s"
        parts(1)%face = ""
        call set_mode_line_process(parts)
        k = current_entry()
        call make_locals(k)
        call comint_prompt_regexp%make_local()
        call comint_delimiter_argument_list%make_local()
        call comint_input_ring_size%make_local()
        call comint_input_ring_file_name%make_local()
        call comint_input_ignoredups%make_local()
        call comint_eol_on_send%make_local()
        call comint_move_point_for_output%make_local()
        call comint_scroll_show_maximum_output%make_local()
        call comint_process_echoes%make_local()
        k = current_entry()
        if (.not. comint_buffers(k)%has_ring) then
            call comint_buffers(k)%input_ring%make(int(comint_input_ring_size%value))
            comint_buffers(k)%has_ring = .true.
        end if
        call font_lock_defaults%set_local(keywords_only=.true.)
    end subroutine comint_mode_body

    !> Return true if there is a living process associated with BUFFER.
    !> Living means the status is `run' (`open' and `stop' are not
    !> ported).  Port of comint.el:comint-check-proc.
    logical function comint_check_proc(buffer)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: proc
        comint_check_proc = .false.
        proc = get_buffer_process(buffer)
        if (proc%index /= 0) comint_check_proc = process_status(proc) == "run"
    end function comint_check_proc

    !> Make a Comint process NAME in BUFFER, running PROGRAM.
    !> If BUFFER_NAME is absent, it defaults to NAME surrounded by `*'s.
    !> If there is a running process in BUFFER, it is not restarted.
    !>
    !> Optional fourth arg STARTFILE is the name of a file, whose
    !> contents are sent to the process as its initial input.
    !>
    !> SWITCHES are arguments to PROGRAM.
    !>
    !> RESULT is the (possibly newly created) process buffer.  Port of
    !> comint.el:make-comint-in-buffer (PROGRAM a string).
    recursive subroutine make_comint_in_buffer(name, program, result, status, buffer_name, startfile, switches)
        character(len=*), intent(in) :: name, program
        type(handle_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: buffer_name, startfile
        type(process_arg_type), intent(in), optional :: switches(:)
        type(handle_type) :: old
        type(status_type) :: st
        type(process_arg_type), allocatable :: args(:)
        if (present(buffer_name)) then
            call get_buffer_create(buffer_name, result, status)
        else
            call get_buffer_create("*"//name//"*", result, status)
        end if
        if (.not. status%ok()) return
        ! If no process, or nuked process, crank up a new one and put
        ! buffer in comint mode.  Otherwise, leave buffer and existing
        ! process alone.
        if (comint_check_proc(result)) return
        old = current_buffer()
        call set_buffer(result, status)
        if (.not. status%ok()) return
        if (.not. derived_from_comint()) call comint_mode(status)
        if (buffer_live_p(old)) call set_buffer(old, st)
        if (.not. status%ok()) return
        if (present(switches)) then
            args = switches
        else
            allocate (args(0))
        end if
        call comint_exec(result, name, program, args, status, startfile)
    end subroutine make_comint_in_buffer

    !> Whether the current buffer's major mode is derived from Comint mode.
    logical function derived_from_comint()
        use derived_fm, only: derived_mode_p
        derived_from_comint = derived_mode_p("comint_mode")
    end function derived_from_comint

    !> Make a Comint process NAME in a buffer, running PROGRAM.
    !> The name of the buffer is made by surrounding NAME with `*'s.
    !> If there is already a running process in that buffer, it is not
    !> restarted.  Optional third arg STARTFILE is the name of a file,
    !> whose contents are sent to the process as its initial input.
    !>
    !> SWITCHES are arguments to PROGRAM.
    !>
    !> RESULT is the (possibly newly created) process buffer.  Port of
    !> comint.el:make-comint.
    recursive subroutine make_comint(name, program, result, status, startfile, switches)
        character(len=*), intent(in) :: name, program
        type(handle_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: startfile
        type(process_arg_type), intent(in), optional :: switches(:)
        call make_comint_in_buffer(name, program, result, status, startfile=startfile, switches=switches)
    end subroutine make_comint

    !> Run PROGRAM in a Comint buffer and switch to that buffer.
    !>
    !> If SWITCHES are supplied, they are passed to PROGRAM.
    !>
    !> The buffer name is made by surrounding the file name of PROGRAM with
    !> `*'s.
    !>
    !> See `make_comint' and `comint_exec'.  Port of comint.el:comint-run
    !> (the hooks `comint-NAME-hook' are not applicable: hooks are named
    !> objects).
    recursive subroutine comint_run(program, status, switches)
        use fileio_fm, only: file_name_nondirectory
        use window_fm, only: switch_to_buffer
        character(len=*), intent(in) :: program
        type(status_type), intent(out) :: status
        type(process_arg_type), intent(in), optional :: switches(:)
        type(handle_type) :: buf
        character(len=:), allocatable :: name
        name = file_name_nondirectory(program)
        call make_comint(name, program, buf, status, switches=switches)
        if (.not. status%ok()) return
        call switch_to_buffer(buffers(buf%index)%name, status)
    end subroutine comint_run

    !> The command: read PROGRAM and run it.  The interactive form of
    !> comint.el:comint-run.
    !>
    !> Run PROGRAM in a Comint buffer and switch to that buffer.
    recursive subroutine cmd_comint_run(status)
        character(len=:), allocatable :: program
        type(status_type), intent(out) :: status
        call read_string("Run program: ", program, status)
        if (.not. status%ok()) return
        if (consp(current_prefix_arg)) then
            call set_error(status, FM_GENERIC_ERROR, "Reading switches for comint_run is not supported yet")
            return
        end if
        call comint_run(program, status)
    end subroutine cmd_comint_run

    !> Start up a process named NAME in buffer BUFFER for Comint modes.
    !> Runs the given COMMAND with SWITCHES, and initial input from
    !> STARTFILE.
    !>
    !> This function blasts any old process running in the buffer, and
    !> does not set the buffer mode.  You can use this to cheaply run a
    !> series of processes in the same Comint buffer.  The hook
    !> `comint_exec_hook' is run after each exec.  ENVIRONMENT, if
    !> present, are "NAME=VALUE" entries put after Comint's own in the
    !> process's environment (as binding `process_environment' around the
    !> call).  Port of comint.el:comint-exec (COMMAND a program).
    recursive subroutine comint_exec(buffer, name, command, switches, status, startfile, environment)
        type(handle_type), intent(in) :: buffer
        character(len=*), intent(in) :: name, command
        type(process_arg_type), intent(in) :: switches(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: startfile
        type(process_arg_type), intent(in), optional :: environment(:)
        type(handle_type) :: old, proc
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer :: k
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        ! Blast any old process.
        proc = get_buffer_process(buffer)
        if (proc%index /= 0) call delete_process(proc, st)
        ! Crank up a new process.
        call comint_exec_1(name, buffer, command, switches, proc, status, environment)
        if (.not. status%ok()) then
            if (buffer_live_p(old)) call set_buffer(old, st)
            return
        end if
        call set_process_filter(proc, st, comint_output_filter)
        ! t if pty, nil if pipe.
        k = current_entry()
        comint_buffers(k)%ptyp = process_connection_type%value
        ! Jump to the end, and set the process mark.
        call goto_char(point_max())
        call set_marker(process_mark(proc), point(), st)
        ! Feed it the startfile.
        if (present(startfile)) then
            call temp_buffer_file_contents(startfile, text, status)
            if (.not. status%ok()) return
            call comint_send_string(proc, text, status)
            if (.not. status%ok()) return
        end if
        call comint_exec_hook%run(status)
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine comint_exec

    !> The contents of FILE (with-temp-buffer, insert-file-contents,
    !> buffer-string).
    subroutine temp_buffer_file_contents(file, text, status)
        use buffer_fm, only: kill_buffer, generate_new_buffer_name
        use editfns_fm, only: buffer_string
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: old, tmp
        type(status_type) :: st
        integer(int64) :: inserted
        text = ""
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, status)
        if (.not. status%ok()) return
        call set_buffer(tmp, st)
        call insert_file_contents(file, .false., inserted, status)
        if (status%ok()) text = buffer_string()
        call set_buffer(old, st)
        call kill_buffer(tmp, st)
    end subroutine temp_buffer_file_contents

    !> Crank up the process for comint-exec in the appropriate environment.
    !> Port of comint.el:comint-exec-1.
    recursive subroutine comint_exec_1(name, buffer, command, switches, proc, status, environment)
        character(len=*), intent(in) :: name, command
        type(handle_type), intent(in) :: buffer
        type(process_arg_type), intent(in) :: switches(:)
        type(handle_type), intent(out) :: proc
        type(status_type), intent(out) :: status
        type(process_arg_type), intent(in), optional :: environment(:)
        type(process_arg_type), allocatable :: env(:), argv(:)
        character(len=:), allocatable :: saved_dir
        logical :: had_dir
        integer :: k
        call comint_term_environment(env)
        call append_arg(env, "INSIDE_EMACS="//upstream_version//",comint")
        if (allocated(comint_pager%value)) call append_arg(env, "PAGER="//comint_pager%value)
        if (present(environment)) then
            do k = 1, size(environment)
                call append_arg(env, environment(k)%s)
            end do
        end if
        had_dir = allocated(buffers(cur)%directory)
        if (had_dir) saved_dir = buffers(cur)%directory
        if (had_dir) then
            if (.not. file_accessible_directory_p(saved_dir)) buffers(cur)%directory = "/"
        else
            buffers(cur)%directory = "/"
        end if
        allocate (argv(size(switches) + 1))
        argv(1)%s = command
        do k = 1, size(switches)
            argv(k + 1)%s = switches(k)%s
        end do
        call make_process(name, argv, proc, status, buffer=buffer, environment=env)
        if (had_dir) then
            buffers(cur)%directory = saved_dir
        else
            deallocate (buffers(cur)%directory)
        end if
        ! (Coding systems are not ported.)
    end subroutine comint_exec_1

    !> Return an environment variable list for terminal configuration.
    !> Port of comint.el:comint-term-environment (a system using
    !> terminfo).
    subroutine comint_term_environment(env)
        type(process_arg_type), allocatable, intent(out) :: env(:)
        character(len=16) :: cols
        integer :: width
        width = 80
        if (selected_frame%selected_window%index /= 0) width = window_text_cols(windows(selected_frame%selected_window%index))
        write (cols, "(i0)") width
        allocate (env(3))
        env(1)%s = "TERM="//comint_terminfo_terminal%value
        env(2)%s = "TERMCAP="
        env(3)%s = "COLUMNS="//trim(cols)
    end subroutine comint_term_environment

    !> Return true if STR contains non-whitespace syntax.  Port of
    !> comint.el:comint-nonblank-p.
    logical function comint_nonblank_p(str)
        character(len=*), intent(in) :: str
        type(status_type) :: st
        comint_nonblank_p = string_match("\`\s *\'", str, st, inhibit_modify=.true.) == 0
    end function comint_nonblank_p

    ! ------------------------------------------------------------------
    ! The input history
    ! ------------------------------------------------------------------

    !> Set the buffer's `comint_input_ring' from a history file.
    !> The name of the file is given by the variable
    !> `comint_input_ring_file_name'.
    !> The history ring is of size `comint_input_ring_size', regardless of
    !> file size.  If `comint_input_ring_file_name' is unallocated this
    !> function does nothing.
    !>
    !> If the optional argument SILENT is true, we say nothing about a
    !> failure to read the history file.
    !>
    !> This function is useful for major mode commands and mode hooks.
    !>
    !> The commands stored in the history file are separated by the
    !> `comint_input_ring_separator', and entries that match
    !> `comint_input_history_ignore' are ignored.  The most recent command
    !> comes last.
    !>
    !> See also `comint_input_ignoredups' and `comint_write_input_ring'.
    !> Port of comint.el:comint-read-input-ring.
    subroutine comint_read_input_ring(silent)
        use buffer_fm, only: kill_buffer, generate_new_buffer_name
        logical, intent(in), optional :: silent
        type(ring_type) :: ring
        type(handle_type) :: old, tmp
        type(status_type) :: st
        character(len=:), allocatable :: file, separator, prefix, ignore, history, last
        integer(int64) :: inserted, found, start, end
        integer :: count, ring_size, ring_max_size, k
        logical :: quiet, ignoredups, have_prefix, keep
        quiet = .false.
        if (present(silent)) quiet = silent
        if (.not. allocated(comint_input_ring_file_name%value)) return
        if (len(comint_input_ring_file_name%value) == 0) return
        file = comint_input_ring_file_name%value
        if (.not. file_readable_p(file)) then
            if (.not. quiet) call message("Cannot read history file "//file)
            return
        end if
        count = 0
        ! Some users set HISTSIZE or `comint-input-ring-size' to huge
        ! numbers.  Don't allocate a huge ring right away; there might not
        ! be that much history.
        ring_size = int(min(1500_int64, comint_input_ring_size%value))
        call ring%make(ring_size)
        ! Use possibly buffer-local values of these variables.
        ring_max_size = int(comint_input_ring_size%value)
        separator = comint_input_ring_separator%value
        have_prefix = allocated(comint_input_ring_file_prefix%value)
        if (have_prefix) prefix = comint_input_ring_file_prefix%value
        ignore = comint_input_history_ignore%value
        ignoredups = comint_input_ignoredups%value
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, st)
        call set_buffer(tmp, st)
        call insert_file_contents(file, .false., inserted, st)
        call goto_char(point_max())
        do while (count < ring_max_size)
            call re_search_backward(separator, found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            end = match_beginning(0, st)
            call re_search_backward(separator, found, st, noerror=SEARCH_NOERROR)
            if (found /= 0) then
                call goto_char(match_end(0, st))
            else
                call goto_char(point_min())
            end if
            if (have_prefix) then
                ! Skip zsh extended_history stamps.
                if (looking_at(prefix, st)) call goto_char(match_end(0, st))
            end if
            start = point()
            call buffer_substring(start, end, history, st)
            keep = string_match(ignore, history, st) == 0
            if (keep .and. ignoredups .and. .not. ring%empty_p()) then
                call ring%ref(ring%count() - 1, last, st)
                keep = .not. (last == history .and. len(last) == len(history))
            end if
            if (keep) then
                if (count == ring_size) then
                    call ring%extend(min(ring_max_size - ring_size, ring_size))
                    ring_size = ring%size()
                end if
                call ring%insert_at_beginning(history)
                count = count + 1
            end if
        end do
        call set_buffer(old, st)
        call kill_buffer(tmp, st)
        k = current_entry()
        comint_buffers(k)%input_ring = ring
        comint_buffers(k)%has_ring = .true.
        comint_buffers(k)%input_ring_index = -1
    end subroutine comint_read_input_ring

    !> Writes the buffer's `comint_input_ring' to a history file.
    !> The name of the file is given by the variable
    !> `comint_input_ring_file_name'.  The original contents of the file
    !> are lost if `comint_input_ring' is not empty.  If
    !> `comint_input_ring_file_name' is unallocated this function does
    !> nothing.
    !>
    !> Useful within process sentinels.
    !>
    !> See also `comint_read_input_ring'.  Port of
    !> comint.el:comint-write-input-ring.
    subroutine comint_write_input_ring()
        use buffer_fm, only: kill_buffer, generate_new_buffer_name
        type(handle_type) :: old, tmp
        type(status_type) :: st
        type(ring_type) :: ring
        character(len=:), allocatable :: file, separator, item, msg
        integer :: k, index
        k = entry_of(cur)
        if (.not. allocated(comint_input_ring_file_name%value)) return
        if (len(comint_input_ring_file_name%value) == 0) return
        if (k == 0) return
        if (.not. comint_buffers(k)%has_ring) return
        if (comint_buffers(k)%input_ring%empty_p()) return
        file = comint_input_ring_file_name%value
        if (.not. file_writable_p(file)) then
            call message("Cannot write history file "//file)
            return
        end if
        ring = comint_buffers(k)%input_ring
        separator = comint_input_ring_separator%value
        index = ring%count()
        ! Write it all out into a buffer first.  Much faster, but messier,
        ! than writing it one line at a time.
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, st)
        call set_buffer(tmp, st)
        do while (index > 0)
            index = index - 1
            call ring%ref(index, item, st)
            call insert(item//separator, st)
        end do
        call write_region(file, .false., st, message=msg)
        call set_buffer(old, st)
        call kill_buffer(tmp, st)
    end subroutine comint_write_input_ring

    !> Read a regexp with PROMPT: REGEXP, and the prefix argument as N.
    !> Port of comint.el:comint-regexp-arg.
    subroutine comint_regexp_arg(prompt, regexp, n, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: regexp
        integer, intent(out) :: n
        type(status_type), intent(out) :: status
        type(value_type) :: saved_last, history
        type(value_type), allocatable :: elts(:)
        type(status_type) :: st
        ! Don't clobber this.
        saved_last = last_command
        call read_from_minibuffer(prompt, regexp, status, history="minibuffer_history_search_history")
        last_command = saved_last
        if (.not. status%ok()) return
        ! If the user didn't enter anything, nothing is added to m-h-s-h.
        ! Use the previous search regexp, if there is one.
        if (len(regexp) == 0) then
            call symbol_value(sym("minibuffer_history_search_history"), history, st)
            if (st%ok() .and. consp(history)) then
                call list_elements(history, elts)
                if (size(elts) > 0) then
                    block
                        use lisp_fm, only: stringp, xstring
                        if (stringp(elts(1))) regexp = xstring(elts(1))
                    end block
                end if
            end if
        end if
        n = int(prefix_numeric_value(current_prefix_arg))
    end subroutine comint_regexp_arg

    !> First make sure there is a ring and that we are after the process
    !> mark; ARG is the count (1 for 0, which restarts from the
    !> beginning).  Port of comint.el:comint-search-arg.
    subroutine comint_search_arg(arg, n, status)
        integer, intent(in) :: arg
        integer, intent(out) :: n
        type(status_type), intent(out) :: status
        integer :: k
        status = status_type()
        n = arg
        if (.not. comint_after_pmark_p()) then
            call set_error(status, FM_USER_ERROR, "Not at command line")
            return
        end if
        k = entry_of(cur)
        if (k == 0) then
            call set_error(status, FM_USER_ERROR, "Empty input ring")
            return
        end if
        if (.not. comint_buffers(k)%has_ring) then
            call set_error(status, FM_USER_ERROR, "Empty input ring")
        else if (comint_buffers(k)%input_ring%empty_p()) then
            call set_error(status, FM_USER_ERROR, "Empty input ring")
        else if (arg == 0) then
            ! arg of zero resets search from beginning, and uses arg of 1.
            comint_buffers(k)%input_ring_index = -1
            n = 1
        end if
    end subroutine comint_search_arg

    !> Restore unfinished input.  Port of comint.el:comint-restore-input.
    subroutine comint_restore_input(status)
        type(status_type), intent(out) :: status
        integer :: k
        status = status_type()
        k = entry_of(cur)
        if (k == 0) return
        if (comint_buffers(k)%input_ring_index < 0) return
        call comint_delete_input(status)
        if (.not. status%ok()) return
        if (allocated(comint_buffers(k)%stored_incomplete_input)) then
            if (len(comint_buffers(k)%stored_incomplete_input) > 0) then
                call insert(comint_buffers(k)%stored_incomplete_input, status)
                call message("Input restored")
            end if
        end if
        comint_buffers(k)%input_ring_index = -1
    end subroutine comint_restore_input

    !> Index to start a directional search, starting at
    !> `comint_input_ring_index'.  Port of comint.el:comint-search-start.
    integer function comint_search_start(k, arg) result(index)
        integer, intent(in) :: k, arg
        associate (e => comint_buffers(k))
            if (e%input_ring_index >= 0) then
                ! If a search is running, offset by 1 in direction of arg.
                index = modulo(e%input_ring_index + merge(1, -1, arg > 0), e%input_ring%count())
            else if (arg >= 0) then
                ! For a new search, start from beginning or end, as
                ! appropriate.
                index = 0
            else
                index = e%input_ring%count() - 1
            end if
        end associate
    end function comint_search_start

    !> Cycle backwards through input history, saving input.  Port of
    !> comint.el:comint-previous-input.
    subroutine comint_previous_input(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status
        integer :: k
        logical :: leaving
        k = entry_of(cur)
        leaving = .false.
        if (k /= 0) then
            associate (e => comint_buffers(k))
                if (e%input_ring_index >= 0 .and. allocated(e%stored_incomplete_input) .and. e%has_ring) then
                    ! Leaving the "end" of the ring.
                    leaving = (arg < 0 .and. e%input_ring_index == 0) .or. &
                              (arg > 0 .and. e%input_ring_index == e%input_ring%count() - 1)
                end if
            end associate
        end if
        if (leaving) then
            call comint_restore_input(status)
        else
            call comint_previous_matching_input(".", arg, status)
        end if
    end subroutine comint_previous_input

    !> Cycle forwards through input history.  Port of
    !> comint.el:comint-next-input.
    subroutine comint_next_input(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status
        call comint_previous_input(-arg, status)
    end subroutine comint_next_input

    !> Return the index matching REGEXP ARG places along the input ring
    !> (-1 for none).  Moves relative to START, or
    !> `comint_input_ring_index'.  Port of
    !> comint.el:comint-previous-matching-input-string-position.
    subroutine comint_previous_matching_input_string_position(regexp, arg_in, pos, status, start)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: arg_in
        integer, intent(out) :: pos
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: start
        character(len=:), allocatable :: item
        integer :: k, length, motion, n, prev, arg
        logical :: tried_each_ring_item
        type(status_type) :: st
        pos = -1
        status = status_type()
        k = entry_of(cur)
        if (k == 0) then
            call set_error(status, FM_USER_ERROR, "No history")
            return
        end if
        if (.not. comint_buffers(k)%has_ring) then
            call set_error(status, FM_USER_ERROR, "No history")
            return
        end if
        if (comint_buffers(k)%input_ring%empty_p()) then
            call set_error(status, FM_USER_ERROR, "No history")
            return
        end if
        arg = arg_in
        length = comint_buffers(k)%input_ring%count()
        motion = merge(1, -1, arg > 0)
        if (present(start)) then
            n = modulo(start - motion, length)
        else
            n = modulo(comint_search_start(k, arg) - motion, length)
        end if
        tried_each_ring_item = .false.
        ! Do the whole search as many times as the argument says.
        do while (arg /= 0 .and. .not. tried_each_ring_item)
            ! Step once.
            prev = n
            n = modulo(n + motion, length)
            ! If we haven't reached a match, step some more.
            do while (n < length .and. .not. tried_each_ring_item)
                call comint_buffers(k)%input_ring%ref(n, item, st)
                if (string_match(regexp, item, status) /= 0) exit
                if (.not. status%ok()) return
                n = modulo(n + motion, length)
                ! If we have gone all the way around in this search.
                tried_each_ring_item = n == prev
            end do
            if (arg > 0) then
                arg = arg - 1
            else
                arg = arg + 1
            end if
        end do
        ! Now that we know which ring element to use, if we found it,
        ! return that.
        call comint_buffers(k)%input_ring%ref(n, item, st)
        if (string_match(regexp, item, status) /= 0) pos = n
    end subroutine comint_previous_matching_input_string_position

    !> Delete all input between accumulation or process mark and point.
    !> Port of comint.el:comint-delete-input.
    subroutine comint_delete_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        integer(int64) :: from
        integer :: k
        status = status_type()
        from = 0
        k = entry_of(cur)
        if (k /= 0) then
            if (comint_buffers(k)%local) from = marker_pos(comint_buffers(k)%accum_marker)
        end if
        if (from == 0) then
            call current_process(proc, status)
            if (.not. status%ok()) return
            from = pmark_pos(proc)
        end if
        ! Can't use kill-region as it sets this-command.
        call delete_region(from, point_max(), status)
    end subroutine comint_delete_input

    !> Search backwards through input history for match for REGEXP.
    !> (Previous history elements are earlier commands.)
    !> With prefix argument N, search for Nth previous match.
    !> If N is negative, find the next or Nth next match.  Port of
    !> comint.el:comint-previous-matching-input.
    subroutine comint_previous_matching_input(regexp, n_in, status, restore)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: n_in
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: restore
        character(len=:), allocatable :: item, old
        character(len=16) :: num
        integer :: n, pos, k
        logical :: wrap
        call comint_search_arg(n_in, n, status)
        if (.not. status%ok()) return
        call comint_previous_matching_input_string_position(regexp, n, pos, status)
        if (.not. status%ok()) return
        ! Has a match been found?
        if (pos < 0) then
            call set_error(status, FM_USER_ERROR, "Not found")
            return
        end if
        k = entry_of(cur)
        wrap = .false.
        if (comint_buffers(k)%input_ring_index >= 0 .and. present(restore)) then
            if (restore) wrap = (n < 0 .and. comint_buffers(k)%input_ring_index < pos) .or. &
                                (n > 0 .and. comint_buffers(k)%input_ring_index > pos)
        end if
        if (wrap) then
            ! We have a wrap; restore contents.
            call comint_restore_input(status)
            return
        end if
        ! If leaving the edit line, save partial input.
        if (comint_buffers(k)%input_ring_index < 0) then
            call call_get_old_input(old, status)
            if (.not. status%ok()) return
            k = entry_of(cur)
            comint_buffers(k)%stored_incomplete_input = old
        end if
        comint_buffers(k)%input_ring_index = pos
        write (num, "(i0)") pos + 1
        call message("History item: "//trim(num))
        call comint_delete_input(status)
        if (.not. status%ok()) return
        call comint_input_ring_ref(pos, item)
        call insert(item, status)
    end subroutine comint_previous_matching_input

    !> Item POS of the current buffer's input ring.
    subroutine comint_input_ring_ref(pos, item)
        integer, intent(in) :: pos
        character(len=:), allocatable, intent(out) :: item
        type(status_type) :: st
        integer :: k
        item = ""
        k = entry_of(cur)
        if (k /= 0) call comint_buffers(k)%input_ring%ref(pos, item, st)
    end subroutine comint_input_ring_ref

    !> Run the current buffer's `comint_get_old_input'.
    subroutine call_get_old_input(input, status)
        character(len=:), allocatable, intent(out) :: input
        type(status_type), intent(out) :: status
        integer :: k
        k = local_entry()
        call comint_buffers(k)%get_old_input(input, status)
    end subroutine call_get_old_input

    !> Search forwards through input history for match for REGEXP.
    !> (Later history elements are more recent commands.)
    !> With prefix argument N, search for Nth following match.
    !> If N is negative, find the previous or Nth previous match.  Port of
    !> comint.el:comint-next-matching-input.
    subroutine comint_next_matching_input(regexp, n, status)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        call comint_previous_matching_input(regexp, -n, status)
    end subroutine comint_next_matching_input

    !> Search backwards through input history for match for current input.
    !> (Previous history elements are earlier commands.)
    !> With prefix argument N, search for Nth previous match.
    !> If N is negative, search forwards for the -Nth following match.
    !> Port of comint.el:comint-previous-matching-input-from-input.
    subroutine comint_previous_matching_input_from_input(n, status)
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        integer(int64) :: opoint, from
        integer :: k
        character(len=:), allocatable :: text
        opoint = point()
        k = local_entry()
        if (.not. (symbol_is(last_command, "comint_previous_matching_input_from_input") .or. &
                   symbol_is(last_command, "comint_next_matching_input_from_input"))) then
            ! Starting a new search.
            from = marker_pos(comint_buffers(k)%accum_marker)
            if (from == 0) then
                call current_process(proc, status)
                if (.not. status%ok()) return
                from = pmark_pos(proc)
            end if
            call buffer_substring(from, point(), text, status)
            if (.not. status%ok()) return
            comint_buffers(k)%matching_input_from_input_string = text
            comint_buffers(k)%input_ring_index = -1
        end if
        call comint_previous_matching_input("^"//regexp_quote(comint_buffers(k)%matching_input_from_input_string), &
                                            n, status, .true.)
        if (.not. status%ok()) return
        if (comint_move_point_for_matching_input%value == COMINT_MATCHING_AFTER_INPUT) call goto_char(opoint)
    end subroutine comint_previous_matching_input_from_input

    !> Search forwards through input history for match for current input.
    !> (Following history elements are more recent commands.)
    !> With prefix argument N, search for Nth following match.
    !> If N is negative, search backwards for the -Nth previous match.
    !> Port of comint.el:comint-next-matching-input-from-input.
    subroutine comint_next_matching_input_from_input(n, status)
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        call comint_previous_matching_input_from_input(-n, status)
    end subroutine comint_next_matching_input_from_input

    !> Add CMD to the input history.
    !> Ignore duplicates if `comint_input_ignoredups' is true.  Port of
    !> comint.el:comint-add-to-input-history.
    subroutine comint_add_to_input_history(cmd)
        character(len=*), intent(in) :: cmd
        character(len=:), allocatable :: last
        type(status_type) :: st
        integer :: k, size
        logical :: add
        k = local_entry()
        if (.not. comint_buffers(k)%has_ring) then
            call comint_buffers(k)%input_ring%make(int(comint_input_ring_size%value))
            comint_buffers(k)%has_ring = .true.
        end if
        add = comint_buffers(k)%input_filter(cmd)
        if (add .and. comint_input_ignoredups%value .and. .not. comint_buffers(k)%input_ring%empty_p()) then
            call comint_buffers(k)%input_ring%ref(0, last, st)
            add = .not. (last == cmd .and. len(last) == len(cmd))
        end if
        if (.not. add) return
        ! If `comint-input-ring' is full, maybe grow it.
        size = comint_buffers(k)%input_ring%size()
        if (size == comint_buffers(k)%input_ring%count() .and. size < comint_input_ring_size%value) then
            call comint_buffers(k)%input_ring%extend(int(min(int(size, int64), comint_input_ring_size%value - size)))
        end if
        call comint_buffers(k)%input_ring%insert(cmd)
    end subroutine comint_add_to_input_history

    !> Return an array of arguments from ARG, backwards.
    !> Break it up at the delimiters in `comint_delimiter_argument_list'.
    !> Characters marked LITERAL (e.g., backslash-escaped characters) are
    !> not considered to be delimiters.  Port of comint.el:comint-delim-arg
    !> (the `literal' text property of the string as an array).
    subroutine comint_delim_arg(arg, literal, args)
        character(len=*), intent(in) :: arg
        logical, intent(in) :: literal(:)
        type(process_arg_type), allocatable, intent(out) :: args(:)
        character(len=:), allocatable :: delims
        integer :: pos, start, n
        character :: ch
        delims = ""
        if (allocated(comint_delimiter_argument_list%value)) delims = comint_delimiter_argument_list%value
        if (len(delims) == 0) then
            allocate (args(1))
            args(1)%s = arg
            return
        end if
        allocate (args(0))
        n = len(arg)
        pos = 1
        do while (pos <= n)
            ch = arg(pos:pos)
            start = pos
            if (index(delims, ch) > 0 .and. .not. literal(pos)) then
                do while (pos <= n)
                    if (arg(pos:pos) /= ch) exit
                    pos = pos + 1
                end do
            else
                do while (pos <= n)
                    if (index(delims, arg(pos:pos)) > 0 .and. .not. literal(pos)) exit
                    pos = pos + 1
                end do
            end if
            call prepend_arg(args, arg(start:pos - 1))
        end do
    end subroutine comint_delim_arg

    !> Return from STRING the NTH to MTH arguments.
    !> NTH and/or MTH can be absent, which means the last argument.
    !> NTH and MTH can be negative to count from the end; -1 means
    !> the last argument.
    !> Returned arguments are separated by single spaces.  We assume
    !> whitespace separates arguments, except within quotes and except
    !> for a space or tab that immediately follows a backslash.  Also, a
    !> run of one or more of a single character in
    !> `comint_delimiter_argument_list' is a separate argument.
    !> Argument 0 is the command name.  Port of comint.el:comint-arguments.
    function comint_arguments(string, nth, mth) result(res)
        character(len=*), intent(in) :: string
        integer, intent(in), optional :: nth, mth
        character(len=:), allocatable :: res
        ! The first line handles ordinary characters and
        ! backslash-sequences.  The second matches "-quoted strings.  The
        ! third matches '-quoted strings.  The fourth matches `-quoted
        ! strings.  This seems to fit the syntax of BASH 2.0.
        character(len=*), parameter :: ARGPART = "[^ "//achar(10)//achar(9)//"""'`\]\|\(\\.\)\|"// &
                                       "\(""\([^""\]\|\\.\)*""\|'[^']*'\|`[^`]*`\)"
        type(process_arg_type), allocatable :: args(:), parts(:)
        logical, allocatable :: literal(:)
        type(status_type) :: st
        integer(int64) :: idx, mb, me, b1, e1, q
        integer :: pos, count, beg, n, m, k
        logical :: quotes, have_beg
        allocate (args(0))
        allocate (literal(len(string)))
        literal = .false.
        pos = 1
        count = 0
        have_beg = .false.
        beg = 0
        quotes = .false.
        ! Build a list of all the args until we have as many as we want.
        do
            if (present(mth)) then
                if (mth >= 0 .and. count > mth) exit
            end if
            idx = string_match(ARGPART, string, st, start=int(pos, int64))
            if (idx == 0) exit
            mb = match_beginning(0, st)
            me = match_end(0, st)
            ! Apply the `literal' text property to backslash-escaped
            ! characters, so that `comint-delim-arg' won't break them up.
            b1 = match_beginning(1, st)
            e1 = match_end(1, st)
            if (b1 > 0) literal(b1:e1 - 1) = .true.
            q = match_beginning(2, st)
            if (have_beg .and. pos == mb) then
                ! It's contiguous, part of the same arg.
                pos = int(me)
                quotes = quotes .or. q > 0
            else
                ! It's a new separate arg.
                if (have_beg) call push_arg()
                count = size(args)
                quotes = q > 0
                beg = int(mb)
                have_beg = .true.
                pos = int(me)
            end if
        end do
        if (have_beg) call push_arg()
        count = size(args)
        if (.not. present(nth)) then
            n = count - 1
        else if (nth >= 0) then
            n = nth
        else
            n = count + nth
        end if
        if (.not. present(mth)) then
            m = 0
        else if (mth >= 0) then
            m = count - mth - 1
        else
            m = -mth - 1
        end if
        ! (mapconcat #'identity (nthcdr n (nreverse (nthcdr m args))) " "):
        ! ARGS is backwards, the J'th argument (from 0) being
        ! ARGS(COUNT - J).
        res = ""
        do k = max(n, 0), count - 1 - m
            if (k > max(n, 0)) res = res//" "
            res = res//args(count - k)%s
        end do
    contains

        subroutine push_arg()
            character(len=:), allocatable :: str
            str = string(beg:pos - 1)
            if (quotes) then
                call prepend_arg(args, str)
            else
                call comint_delim_arg(str, literal(beg:pos - 1), parts)
                args = [parts, args]
            end if
        end subroutine push_arg
    end function comint_arguments

    !> Return FILENAME with environment variables substituted.
    !> Supports additional environment variable syntax of the command
    !> interpreter.  Port of comint.el:comint-substitute-in-file-name.
    function comint_substitute_in_file_name(filename) result(name)
        use fileio_fm, only: substitute_in_file_name
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: name
        name = substitute_in_file_name(filename)
    end function comint_substitute_in_file_name

    ! ------------------------------------------------------------------
    ! Sending input
    ! ------------------------------------------------------------------

    !> Send input to process.
    !> After the process output mark, sends all text from the process mark
    !> to point as input to the process.  Before the process output mark,
    !> calls value of variable `comint_get_old_input' to retrieve old
    !> input, copies it to the process mark, and sends it.
    !>
    !> This command also sends and inserts a final newline, unless
    !> NO_NEWLINE is true.
    !>
    !> The functions of `comint_input_filter_functions' are called on the
    !> input before sending it.  The input is entered into the input
    !> history ring, if the value of variable `comint_input_filter' returns
    !> true when called on the input.
    !>
    !> If variable `comint_eol_on_send' is true, then point is moved to
    !> the end of line before sending the input.
    !>
    !> After the input has been sent, if `comint_process_echoes' is true,
    !> then `comint_send_input' waits to see if the process outputs a
    !> string matching the input, and if so, deletes that part of the
    !> output.  If ARTIFICIAL is true, it inhibits such deletion.  Callers
    !> sending input not from the user should use ARTIFICIAL = true.
    !>
    !> The values of `comint_get_old_input', `comint_input_filter_functions',
    !> and `comint_input_filter' are chosen according to the command
    !> interpreter running in the buffer.  Port of
    !> comint.el:comint-send-input (input history expansion is Deferred).
    recursive subroutine comint_send_input(status, no_newline, artificial, sender, run_filters)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_newline, artificial
        !> Send with SENDER instead of `comint_input_sender' (the binding
        !> comint-skip-input makes).
        procedure(comint_input_sender_proc), optional :: sender
        !> False to run no `comint_input_filter_functions' (the binding
        !> comint-skip-input makes).
        logical, intent(in), optional :: run_filters
        type(handle_type) :: proc, pmark
        type(value_type) :: saved, props
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        character(len=:), allocatable :: intxt, input, history, copy
        integer(int64) :: pm, beg, end, echo_len, lie, lis
        logical :: nonl, art, changed, got, filters
        integer :: k
        nonl = .false.
        if (present(no_newline)) nonl = no_newline
        art = .false.
        if (present(artificial)) art = artificial
        filters = .true.
        if (present(run_filters)) filters = run_filters
        ! Note that the input string does not include its terminal newline.
        call current_process(proc, status)
        if (.not. status%ok()) return
        k = local_entry()
        call widen()
        pmark = process_mark(proc)
        pm = pmark_pos(proc)
        if (point() >= pm) then
            if (comint_eol_on_send%value) then
                if (comint_use_prompt_regexp%value) then
                    call end_of_line()
                else
                    call goto_char(field_end(point(), .false.))
                end if
            end if
            call buffer_substring(pm, point(), intxt, status)
            if (.not. status%ok()) return
        else
            call call_get_old_input(copy, status)
            if (.not. status%ok()) return
            call goto_char(pm)
            call insert(copy, status)
            if (.not. status%ok()) return
            intxt = copy
        end if
        ! (comint-input-autoexpand is Deferred: the input as it is.)
        input = intxt
        history = input
        ! Delete and reinsert input.  This seems like a no-op, except for
        ! the resulting entries in the undo list: undoing this insertion
        ! will delete the region, moving the process mark back to its
        ! original position.
        call inhibit_read_only_begin(saved)
        call delete_region(pm, point(), st)
        call insert(input, st)
        call inhibit_read_only_end(saved)
        if (.not. nonl) call insert(achar(10), status)
        call comint_add_to_input_history(history)
        if (filters) then
            if (nonl) then
                call comint_input_filter_functions%run(input, status)
            else
                call comint_input_filter_functions%run(input//achar(10), status)
            end if
            if (.not. status%ok()) return
        end if
        k = local_entry()
        beg = marker_pos(pmark)
        if (nonl) then
            end = point()
        else
            end = point() - 1
        end if
        call silent_modifications_begin(silent)
        if (end > beg) then
            if (comint_highlight_input%value) then
                props = list(sym("font_lock_face"), sym("comint_highlight_input"), &
                             sym("comint__fontify_input_inhibit_fontification"), t_value(), &
                             sym("front_sticky"), t_value())
                call add_text_properties(beg, end, props, changed, st)
            end if
            if (.not. comint_use_prompt_regexp%value) then
                ! Give old user input a field property of `input', to
                ! distinguish it from both process output and unsent input.
                ! The terminating newline is put into a special `boundary'
                ! field to make cursor movement between input and output
                ! fields smoother.
                props = list(sym("mouse_face"), sym("highlight"), sym("help_echo"), &
                             make_str("mouse-2: insert after prompt as new input"))
                call add_text_properties(beg, end, props, changed, st)
            end if
        end if
        if (.not. (nonl .or. comint_use_prompt_regexp%value)) then
            ! Cover the terminating newline.
            props = rear_nonsticky_plist()
            call add_text_properties(end, end + 1, props, changed, st)
            props = list(sym("field"), sym("boundary"), sym("inhibit_line_move_field_capture"), t_value())
            call add_text_properties(end, end + 1, props, changed, st)
        end if
        call silent_modifications_end(silent)
        call comint_snapshot_last_prompt()
        k = local_entry()
        comint_buffers(k)%save_input_ring_index = comint_buffers(k)%input_ring_index
        comint_buffers(k)%input_ring_index = -1
        ! Update the markers before we send the input in case we get
        ! output amidst sending the input.
        call set_marker(comint_buffers(k)%last_input_start, pm, st)
        call set_marker(comint_buffers(k)%last_input_end, point(), st)
        call set_marker(pmark, point(), st)
        ! Clear the "accumulation" marker.
        call detach_marker(comint_buffers(k)%accum_marker, st)
        comint_input_sender_no_newline = nonl
        if (present(sender)) then
            call sender(proc, input, status)
        else
            call comint_buffers(k)%input_sender(proc, input, status)
        end if
        comint_input_sender_no_newline = .false.
        if (.not. status%ok()) return
        ! Optionally delete echoed input (after checking it).
        k = local_entry()
        if (comint_process_echoes%value .and. .not. art) then
            lis = marker_pos(comint_buffers(k)%last_input_start)
            lie = marker_pos(comint_buffers(k)%last_input_end)
            echo_len = lie - lis
            ! Wait for all input to be echoed.
            do while (lie + echo_len > point_max())
                call accept_process_output(got, st, proc)
                if (.not. got) exit
                if (.not. same_text(lis, point_max() - echo_len, lie, point_max())) exit
            end do
            if (lie + echo_len <= point_max()) then
                if (same_text(lis, lie, lie, lie + echo_len)) then
                    ! Certain parts of the text to be deleted may have been
                    ! mistaken for prompts.  We have to prevent problems
                    ! when `comint-prompt-read-only' is non-nil.
                    call inhibit_read_only_begin(saved)
                    call delete_region(lie, lie + echo_len, st)
                    if (comint_prompt_read_only%value) then
                        block
                            integer(int64) :: opoint
                            opoint = point()
                            call goto_char(lie)
                            call comint_update_fence()
                            call goto_char(opoint)
                        end block
                    end if
                    call inhibit_read_only_end(saved)
                end if
            end if
        end if
        ! This used to call comint-output-filter-functions, but that
        ! scrolled the buffer in undesirable ways.
        k = local_entry()
        call set_marker(comint_buffers(k)%last_output_start, marker_pos(pmark), st)
        call comint_output_filter_functions%run("", status)
    contains

        !> Whether the text from B1 to E1 is the text from B2 to E2
        !> (compare-buffer-substrings gives 0).
        logical function same_text(b1, e1, b2, e2)
            integer(int64), intent(in) :: b1, e1, b2, e2
            character(len=:), allocatable :: t1, t2
            type(status_type) :: s
            call buffer_substring(max(b1, point_min()), min(e1, point_max()), t1, s)
            call buffer_substring(max(b2, point_min()), min(e2, point_max()), t2, s)
            same_text = t1 == t2 .and. len(t1) == len(t2)
        end function same_text
    end subroutine comint_send_input

    function make_str(s) result(v)
        use lisp_fm, only: make_string
        character(len=*), intent(in) :: s
        type(value_type) :: v
        v = make_string(s)
    end function make_str

    !> The command: send input to process.  The interactive form of
    !> comint.el:comint-send-input.
    !>
    !> Send input to process.
    recursive subroutine cmd_comint_send_input(status)
        type(status_type), intent(out) :: status
        call comint_send_input(status)
    end subroutine cmd_comint_send_input

    !> Snapshot the current `comint_last_prompt'.
    !> Freezes the `font_lock_face' text property in place.  Port of
    !> comint.el:comint-snapshot-last-prompt.
    subroutine comint_snapshot_last_prompt()
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        integer :: k
        k = entry_of(cur)
        if (k == 0) return
        if (.not. comint_buffers(k)%has_last_prompt) return
        call silent_modifications_begin(silent)
        call font_lock_append_text_property(marker_pos(comint_buffers(k)%last_prompt_start), &
                                            marker_pos(comint_buffers(k)%last_prompt_end), &
                                            sym("font_lock_face"), sym("comint_highlight_prompt"), st)
        call silent_modifications_end(silent)
        ! Reset comint-last-prompt so later on comint-output-filter does
        ! not remove the font-lock-face text property of the previous
        ! (this) prompt.
        call forget_last_prompt(comint_buffers(k))
    end subroutine comint_snapshot_last_prompt

    !> Default function for sending to PROC input STRING.
    !> This just sends STRING plus a newline.  To override this, set the
    !> hook `comint_input_sender'.  Port of comint.el:comint-simple-send.
    recursive subroutine comint_simple_send(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        if (comint_input_sender_no_newline) then
            call comint_send_string(process, string, status)
        else
            ! Sending as two separate strings does not work on Windows, so
            ! concat the \n before sending.
            call comint_send_string(process, string//achar(10), status)
        end if
        if (.not. status%ok()) return
        if (comint_input_sender_no_newline .and. len(string) > 0) call process_send_eof(status)
    end subroutine comint_simple_send

    !> The `comint_input_sender' comint-skip-input binds: send nothing.
    subroutine ignore_input(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        ! (Nothing is sent: PROCESS and STRING are not used.)
        status = status_type()
        if (process%index < 0 .or. len(string) < 0) return
    end subroutine ignore_input

    !> Like `process_send_string', but also does extra bookkeeping for
    !> Comint mode.  Port of comint.el:comint-send-string.
    recursive subroutine comint_send_string(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        call snapshot_in_buffer_of(process)
        call process_send_string(string, status, process)
    end subroutine comint_send_string

    !> Like `process_send_region', but also does extra bookkeeping for
    !> Comint mode.  Port of comint.el:comint-send-region.
    subroutine comint_send_region(process, start, end, status)
        type(handle_type), intent(in) :: process
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        call snapshot_in_buffer_of(process)
        call process_send_region(start, end, status, process)
    end subroutine comint_send_region

    !> comint-snapshot-last-prompt in the buffer of PROCESS.
    subroutine snapshot_in_buffer_of(process)
        type(handle_type), intent(in) :: process
        type(handle_type) :: old, buf
        type(status_type) :: st
        buf = process_buffer(process)
        if (.not. buffer_live_p(buf)) return
        old = current_buffer()
        call set_buffer(buf, st)
        call comint_snapshot_last_prompt()
        call set_buffer(old, st)
    end subroutine snapshot_in_buffer_of

    ! ------------------------------------------------------------------
    ! Output
    ! ------------------------------------------------------------------

    !> Interpret carriage control characters in the region from START to
    !> END.  Translate carriage return/linefeed sequences to linefeeds.
    !> Make single carriage returns delete to the beginning of the line.
    !> Make backspaces delete the previous character.  Port of
    !> comint.el:comint-carriage-motion.
    recursive subroutine comint_carriage_motion(start, end)
        integer(int64), intent(in) :: start, end
        type(value_type) :: v, inhibit
        type(handle_type) :: delete_end, saved_point
        type(status_type) :: st
        integer(int64) :: opoint, lbeg, de, moved
        logical :: changed
        integer(int32) :: ch
        logical :: have_delete_end
        call copy_marker_at(point(), saved_point, .false., st)
        call goto_char(start)
        call inhibit_read_only_begin(inhibit)
        lbeg = line_beginning_position()
        have_delete_end = .false.
        ! If the preceding text is marked as "must-overwrite", record it in
        ! delete-end.
        if (start > point_min()) then
            call get_text_property(start - 1, sym("comint_must_overwrite"), v, st)
            if (.not. nilp(v)) then
                call copy_marker_at(point(), delete_end, .false., st)
                have_delete_end = .true.
                call remove_text_properties(lbeg, start, list(sym("comint_must_overwrite"), nil()), changed, st)
            end if
        end if
        call narrow_to_region(lbeg, end, st)
        ! Handle BS, LF, and CR specially.
        do
            moved = skip_chars_forward("^"//achar(8)//achar(10)//achar(13), st)
            if (eobp()) exit
            ch = following_char()
            if (ch == 8) then
                ! CH = BS
                call delete_char(1_int64, st)
                if (point() > lbeg) call delete_char(-1_int64, st)
            else if (ch == 10) then
                ! CH = LF
                if (have_delete_end) then
                    call marker_position(delete_end, de, st)
                    if (de < point()) call delete_region(lbeg, de, st)
                    call free_marker(delete_end, st)
                    have_delete_end = .false.
                end if
                call forward_char(st, 1_int64)
                lbeg = point()
            else
                ! CH = CR
                call delete_char(1_int64, st)
                if (have_delete_end) then
                    call marker_position(delete_end, de, st)
                    if (de < point()) then
                        call set_marker(delete_end, point(), st)
                    end if
                else
                    call copy_marker_at(point(), delete_end, .false., st)
                    have_delete_end = .true.
                end if
            end if
        end do
        if (have_delete_end) then
            call marker_position(delete_end, de, st)
            if (de < point()) then
                ! As there's a text after the last CR, make the current
                ! line contain only that text.
                call delete_region(lbeg, de, st)
            else
                ! Remember that the process output ends with CR, and thus
                ! the next output should overwrite the current line.
                call put_text_property(lbeg, de, sym("comint_must_overwrite"), t_value(), st)
            end if
            call free_marker(delete_end, st)
        end if
        call widen()
        ! (save-excursion restores point.)
        call marker_position(saved_point, opoint, st)
        call goto_char(opoint)
        call free_marker(saved_point, st)
        call inhibit_read_only_end(inhibit)
    end subroutine comint_carriage_motion

    !> The filter of Comint processes: insert STRING (after the
    !> `comint_preoutput_filter_functions') at the process mark, with
    !> carriage motion, run `comint_output_filter_functions', mark the text
    !> as output and the last line as the prompt.  Port of
    !> comint.el:comint-output-filter.
    recursive subroutine comint_output_filter(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(handle_type) :: oprocbuf, saved_point, rbeg, rend, pmark
        type(value_type) :: inhibit
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: start, prompt_start, b, e, shortage
        logical :: narrowed, changed
        type(value_type) :: v
        integer :: k
        status = status_type()
        oprocbuf = process_buffer(process)
        ! First check for killed buffer or no input.
        if (.not. buffer_live_p(oprocbuf)) return
        call set_buffer(oprocbuf, status)
        if (.not. status%ok()) return
        k = local_entry()
        ! Run preoutput filters.
        call comint_preoutput_filter_functions%run(string, text, status)
        if (.not. status%ok()) return
        ! Insert STRING.
        call inhibit_read_only_begin(inhibit)
        ! The point should float after any insertion we do.
        call copy_marker_at(point(), saved_point, .true., st)
        ! We temporarily remove any buffer narrowing, in case the process
        ! mark is outside of the restriction.
        narrowed = buffers(cur)%begv > 1 .or. buffers(cur)%zv < buffers(cur)%text%z
        call copy_marker_at(point_min(), rbeg, .false., st)
        call copy_marker_at(point_max(), rend, .true., st)
        call widen()
        pmark = process_mark(process)
        call goto_char(marker_pos(pmark))
        k = local_entry()
        call set_marker(comint_buffers(k)%last_output_start, point(), st)
        start = point()
        ! insert-before-markers is a bad thing.  (The text is marked as
        ! output once it is inserted, the property being on the string
        ! upstream.)
        call insert(text, status)
        if (status%ok()) then
            call put_text_property(start, point(), sym("field"), sym("output"), st)
            ! Advance process-mark.
            call set_marker(pmark, point(), st)
            if (.not. comint_inhibit_carriage_motion%value) then
                ! Interpret any carriage motion characters (newline,
                ! backspace).
                call comint_carriage_motion(marker_pos(comint_buffers(k)%last_output_start), point())
            end if
            ! Run these hooks with point where the user had it.
            call goto_char(marker_pos(saved_point))
            call comint_output_filter_functions%run(text, status)

            call set_marker(saved_point, point(), st)
            ! In case a filter moved it.
            call goto_char(marker_pos(pmark))
            k = local_entry()
            if (.not. comint_use_prompt_regexp%value) then
                call comint_mark_as_output(marker_pos(comint_buffers(k)%last_output_start), point())
            end if
            ! Highlight the prompt, where we define `prompt' to mean the
            ! most recent output that doesn't end with a newline.
            b = point()
            shortage = forward_line(0_int64)
            prompt_start = point()
            call goto_char(b)
            if (comint_prompt_read_only%value) then
                call silent_modifications_begin(silent)
                if (point_min() /= prompt_start) then
                    call get_text_property(prompt_start - 1, sym("read_only"), v, st)
                    if (nilp(v)) call put_text_property(prompt_start - 1, prompt_start, sym("read_only"), &
                                                        sym("fence"), st)
                end if
                call add_text_properties(prompt_start, point(), list(sym("read_only"), t_value(), sym("front_sticky"), &
                                                                     list(sym("read_only"))), changed, st)
                call silent_modifications_end(silent)
            end if
            k = local_entry()
            if (comint_buffers(k)%has_last_prompt) then
                ! There might be some keywords here waiting for
                ! fontification, so no `with-silent-modifications'.
                call font_lock_remove_face_from_text_property(marker_pos(comint_buffers(k)%last_prompt_start), &
                                                              marker_pos(comint_buffers(k)%last_prompt_end), &
                                                              sym("font_lock_face"), sym("comint_highlight_prompt"), st)
                call forget_last_prompt(comint_buffers(k))
            end if
            call copy_marker_at(prompt_start, comint_buffers(k)%last_prompt_start, .false., st)
            call copy_marker_at(point(), comint_buffers(k)%last_prompt_end, .false., st)
            comint_buffers(k)%has_last_prompt = .true.
            e = point()
            call font_lock_append_text_property(prompt_start, e, sym("font_lock_face"), sym("comint_highlight_prompt"), st)
            call add_text_properties(prompt_start, e, rear_nonsticky_plist(), changed, st)
        end if
        ! save-restriction.
        if (narrowed) then
            call narrow_to_region(marker_pos(rbeg), marker_pos(rend), st)
        end if
        call free_marker(rbeg, st)
        call free_marker(rend, st)
        call goto_char(marker_pos(saved_point))
        call free_marker(saved_point, st)
        call inhibit_read_only_end(inhibit)
    end subroutine comint_output_filter

    !> Mark the text from BEG to END as process output.  Port of
    !> comint.el:comint--mark-as-output.
    subroutine comint_mark_as_output(beg, end)
        integer(int64), intent(in) :: beg, end
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        logical :: changed
        call silent_modifications_begin(silent)
        call add_text_properties(beg, end, rear_nonsticky_plist(), changed, st)
        call add_text_properties(beg, end, list(sym("front_sticky"), &
                                                list(sym("field"), sym("inhibit_line_move_field_capture")), &
                                                sym("field"), sym("output"), &
                                                sym("inhibit_line_move_field_capture"), t_value()), changed, st)
        ! Text inserted by a user in the middle of process output should be
        ! marked as output.  (Text property hooks are not ported: the
        ! property is set as upstream sets it.)
        call add_text_properties(beg, end, list(sym("insert_in_front_hooks"), &
                                                list(sym("comint__mark_as_output"), &
                                                     sym("comint__mark_yanked_as_output"))), changed, st)
        call silent_modifications_end(silent)
    end subroutine comint_mark_as_output

    !> Go to the end of buffer in some or all windows showing it.
    !> Do not scroll if the current line is the last line in the buffer.
    !> Depends on the value of `comint_move_point_for_output' and
    !> `comint_scroll_show_maximum_output'.
    !>
    !> This function should be in the hook `comint_output_filter_functions'.
    !> Port of comint.el:comint-postoutput-scroll-to-bottom.
    recursive subroutine comint_postoutput_scroll_to_bottom(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(handle_type) :: current, process, selected
        type(handle_type), allocatable :: leaves(:)
        type(status_type) :: st
        integer :: i
        status = status_type()
        ! (STRING is not used.)
        if (len(string) < 0) return
        current = current_buffer()
        process = get_buffer_process(current)
        if (process%index == 0) return
        call window_leaves(selected_frame%root_window, leaves)
        selected = selected_frame%selected_window
        do i = 1, size(leaves)
            if (.not. same_handle(windows(leaves(i)%index)%buffer, current)) cycle
            call comint_adjust_window_point(leaves(i), process)
            ! Optionally scroll to the bottom of the window.
            if (comint_scroll_show_maximum_output%value) then
                if (window_point_of(leaves(i)) == point_max()) then
                    call select_window(leaves(i), st, norecord=.true.)
                    call recenter(make_int(-1_int64), .false., st)
                    call select_window(selected, st, norecord=.true.)
                end if
            end if
            call set_buffer(current, st)
        end do
    end subroutine comint_postoutput_scroll_to_bottom

    function make_int(n) result(v)
        use lisp_fm, only: make_fixnum
        integer(int64), intent(in) :: n
        type(value_type) :: v
        v = make_fixnum(n)
    end function make_int

    !> The point of window W: point, for the selected window showing the
    !> current buffer.
    integer(int64) function window_point_of(w) result(pos)
        type(handle_type), intent(in) :: w
        if (same_handle(w, selected_frame%selected_window) .and. same_handle(windows(w%index)%buffer, current_buffer())) then
            pos = point()
        else
            pos = window_point(windows(w%index))
        end if
    end function window_point_of

    !> Move point in WINDOW based on Comint settings.
    !> For point adjustment use the process-mark of PROCESS.  Port of
    !> comint.el:comint-adjust-window-point.
    subroutine comint_adjust_window_point(window, process)
        type(handle_type), intent(in) :: window, process
        type(status_type) :: st
        integer(int64) :: wp, pm, los
        logical :: move
        integer :: k
        wp = window_point_of(window)
        pm = pmark_pos(process)
        if (.not. (wp < pm)) return
        select case (comint_move_point_for_output%value)
        case (COMINT_MOVE_T, COMINT_MOVE_ALL)
            move = .true.
        case (COMINT_MOVE_THIS)
            ! Maybe user wants point to jump to end.
            move = same_handle(window, selected_frame%selected_window)
        case (COMINT_MOVE_OTHERS)
            move = .not. same_handle(window, selected_frame%selected_window)
        case default
            move = .false.
        end select
        if (.not. move) then
            ! If point was at the end, keep it at end.
            k = entry_of(cur)
            los = 0
            if (k /= 0) los = marker_pos(comint_buffers(k)%last_output_start)
            move = los /= 0 .and. wp >= los
        end if
        if (.not. move) return
        if (same_handle(window, selected_frame%selected_window) .and. &
            same_handle(windows(window%index)%buffer, current_buffer())) then
            call goto_char(pm)
        else
            call set_window_point(windows(window%index), pm, st)
        end if
    end subroutine comint_adjust_window_point

    !> Truncate the buffer to `comint_buffer_maximum_size'.
    !> This function could be on `comint_output_filter_functions' or bound
    !> to a key (STRING is not used).  Port of comint.el:comint-truncate-buffer.
    recursive subroutine comint_truncate_buffer(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        status = status_type()
        if (len(string) < 0) return
        call truncate_to(comint_buffer_maximum_size%value, status)
    end subroutine comint_truncate_buffer

    !> The command.  Truncate the buffer to `comint_buffer_maximum_size'.
    recursive subroutine cmd_comint_truncate_buffer(status)
        type(status_type), intent(out) :: status
        call comint_truncate_buffer("", status)
    end subroutine cmd_comint_truncate_buffer

    !> comint-truncate-buffer with comint-buffer-maximum-size MAXIMUM.
    subroutine truncate_to(maximum, status)
        integer(int64), intent(in) :: maximum
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        type(value_type) :: inhibit
        integer(int64) :: opoint, shortage
        call current_process(proc, status)
        if (.not. status%ok()) return
        opoint = point()
        call goto_char(pmark_pos(proc))
        shortage = forward_line(-maximum)
        call beginning_of_line()
        call inhibit_read_only_begin(inhibit)
        call delete_region(point_min(), point(), status)
        call inhibit_read_only_end(inhibit)
        call goto_char(min(opoint, point_max()))
    end subroutine truncate_to

    !> Strip trailing `^M' characters from the current output group.
    !> This function could be on `comint_output_filter_functions' or bound
    !> to a key (STRING is not used).  Port of comint.el:comint-strip-ctrl-m.
    subroutine comint_strip_ctrl_m(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        status = status_type()
        if (len(string) < 0) return
        call strip_ctrl_m(status, .false.)
    end subroutine comint_strip_ctrl_m

    !> comint-strip-ctrl-m, called interactively if INTERACTIVE.
    subroutine strip_ctrl_m(status, interactive)
        use search_fm, only: re_search_forward, replace_match
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(handle_type) :: proc
        type(status_type) :: st
        integer(int64) :: opoint, found, pm, from
        logical :: inter
        integer :: k
        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        proc = get_buffer_process(current_buffer())
        if (proc%index == 0) then
            ! This function may be used in `comint-output-filter-functions',
            ! and in that case, if there's no process, then we should do
            ! nothing.  If interactive, report an error.
            if (inter) call set_error(status, FM_GENERIC_ERROR, "No process in the current buffer")
            return
        end if
        pm = pmark_pos(proc)
        opoint = point()
        k = entry_of(cur)
        from = 0
        if (k /= 0) then
            if (inter) then
                from = marker_pos(comint_buffers(k)%last_input_end)
            else
                from = marker_pos(comint_buffers(k)%last_output_start)
            end if
        end if
        if (from > 0) call goto_char(from)
        do
            call re_search_forward(achar(13)//"+$", found, st, bound=pm, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            call replace_match("", st, fixedcase=.true., literal=.true.)
        end do
        call goto_char(opoint)
    end subroutine strip_ctrl_m

    !> The command.  Strip trailing `^M' characters from the current
    !> output group.
    subroutine cmd_comint_strip_ctrl_m(status)
        type(status_type), intent(out) :: status
        call strip_ctrl_m(status, .true.)
    end subroutine cmd_comint_strip_ctrl_m

    !> Put the end of the buffer at the bottom of the window.  Port of
    !> comint.el:comint-show-maximum-output.
    subroutine comint_show_maximum_output(status)
        type(status_type), intent(out) :: status
        call goto_char(point_max())
        call recenter(make_int(-1_int64), .false., status)
    end subroutine comint_show_maximum_output

    ! ------------------------------------------------------------------
    ! Old input
    ! ------------------------------------------------------------------

    !> Default for `comint_get_old_input'.
    !> If `comint_use_prompt_regexp' is false, then either return the
    !> current input field, if point is on an input field, or the current
    !> line, if point is on an output field.  If `comint_use_prompt_regexp'
    !> is true, then return the current line with any initial string
    !> matching the regexp `comint_prompt_regexp' removed.  Port of
    !> comint.el:comint-get-old-input-default.
    recursive subroutine comint_get_old_input_default(input, status)
        character(len=:), allocatable, intent(out) :: input
        type(status_type), intent(out) :: status
        type(value_type) :: field_prop
        integer(int64) :: bof, start, end
        status = status_type()
        field_prop = nil()
        if (.not. comint_use_prompt_regexp%value) then
            bof = field_beginning(point(), .false.)
            field_prop = field_at(bof)
            ! Make sure we're in an input rather than output field.
            if (nilp(field_prop)) then
                input = field_string(bof)
                return
            end if
        end if
        call comint_bol(.false.)
        start = point()
        if (comint_use_prompt_regexp%value .or. symbol_is(field_prop, "output")) then
            end = line_end_position()
        else
            end = field_end(point(), .false.)
        end if
        call buffer_substring(start, end, input, status)
    end subroutine comint_get_old_input_default

    !> Insert after prompt old input at point as new input to be edited.
    !> Calls `comint_get_old_input' to get old input.  Port of
    !> comint.el:comint-copy-old-input.
    subroutine comint_copy_old_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        character(len=:), allocatable :: input
        call call_get_old_input(input, status)
        if (.not. status%ok()) return
        call current_process(proc, status)
        if (.not. status%ok()) return
        call goto_char(pmark_pos(proc))
        call insert(input, status)
    end subroutine comint_copy_old_input

    !> Skip past the text matching regexp `comint_prompt_regexp'.
    !> If this takes us past the end of the current line, don't skip at
    !> all.  Port of comint.el:comint-skip-prompt.
    subroutine comint_skip_prompt()
        type(status_type) :: st
        integer(int64) :: e
        if (looking_at(comint_prompt_regexp%value, st)) then
            e = match_end(0, st)
            if (e <= line_end_position()) call goto_char(e)
        end if
    end subroutine comint_skip_prompt

    !> Return true if point is after the process output marker.  Port of
    !> comint.el:comint-after-pmark-p.
    logical function comint_after_pmark_p()
        type(handle_type) :: proc
        comint_after_pmark_p = .false.
        proc = get_buffer_process(current_buffer())
        if (proc%index /= 0) comint_after_pmark_p = pmark_pos(proc) <= point()
    end function comint_after_pmark_p

    !> Return the buffer position of the beginning of the line, after any
    !> prompt.  If `comint_use_prompt_regexp' is true, then the prompt
    !> skip is done by skipping text matching the regular expression
    !> `comint_prompt_regexp', a buffer local variable.  Port of
    !> comint.el:comint-line-beginning-position.
    integer(int64) function comint_line_beginning_position() result(pos)
        integer(int64) :: opoint, bol
        if (comint_use_prompt_regexp%value) then
            ! Use comint-prompt-regexp.
            opoint = point()
            call beginning_of_line()
            call comint_skip_prompt()
            pos = point()
            call goto_char(opoint)
        else
            ! Use input fields.  Note that, unlike the behavior of
            ! `line-beginning-position' inside a field, this function will
            ! return the position of the end of a prompt, even if the point
            ! is already inside the prompt.  In order to do this, it assumes
            ! that if there are two fields on a line, then the first one is
            ! the prompt, and the second one is an input field, and is
            ! front-sticky (as input fields should be).
            if (symbol_is(field_at_pos(point()), "output")) then
                bol = line_beginning_position()
            else
                bol = field_beginning(point(), .false.)
            end if
            pos = constrain_to_field(bol, line_end_position(), .false., .false.)
        end if
    end function comint_line_beginning_position

    !> The field at POS, as editfns.c:Ffield_at_pos gives it (the field
    !> of the text before POS unless the field after it is front-sticky).
    function field_at_pos(pos) result(v)
        use editfns_fm, only: get_pos_property
        integer(int64), intent(in) :: pos
        type(value_type) :: v
        v = get_pos_property(pos, sym("field"))
    end function field_at_pos

    !> Go to the beginning of line, then skip past the prompt, if any.
    !> If prefix argument is given (\[universal_argument]) the prompt is
    !> not skipped.  If `comint_use_prompt_regexp' is true, then the prompt
    !> skip is done by skipping text matching the regular expression
    !> `comint_prompt_regexp', a buffer local variable.  Port of
    !> comint.el:comint-bol.
    subroutine comint_bol(arg)
        logical, intent(in) :: arg
        integer(int64) :: shortage
        if (arg) then
            ! Unlike `beginning-of-line', forward-line ignores field
            ! boundaries.
            shortage = forward_line(0_int64)
        else
            call goto_char(comint_line_beginning_position())
        end if
    end subroutine comint_bol

    ! ------------------------------------------------------------------
    ! Output commands
    ! ------------------------------------------------------------------

    !> Delete all output from interpreter since last input.
    !> If KILL (interactively, the prefix), save the killed text in the
    !> kill ring.
    !>
    !> This command does not delete the prompt.  Port of
    !> comint.el:comint-delete-output.
    recursive subroutine comint_delete_output(status, kill)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: kill
        type(handle_type) :: proc
        type(value_type) :: inhibit
        type(status_type) :: st
        character(len=:), allocatable :: replacement, prompt
        integer(int64) :: opoint, pm, lie, shortage
        integer :: k
        call current_process(proc, status)
        if (.not. status%ok()) return
        k = local_entry()
        call inhibit_read_only_begin(inhibit)
        opoint = point()
        call goto_char(pmark_pos(proc))
        shortage = forward_line(0_int64)
        pm = point()
        lie = marker_pos(comint_buffers(k)%last_input_end)
        if (present(kill)) then
            if (kill) call copy_region_as_kill(lie, pm, st)
        end if
        call delete_region(lie, pm, st)
        pm = lie
        call goto_char(pmark_pos(proc))
        call buffer_substring(pm, point(), prompt, st)
        replacement = "*** output flushed ***"//achar(10)//prompt
        call delete_region(pm, point(), st)
        if (opoint <= point_max()) then
            call goto_char(opoint)
        else
            call goto_char(point_max())
        end if
        call inhibit_read_only_end(inhibit)
        ! Output message and put back prompt.
        call comint_output_filter(proc, replacement, status)
    end subroutine comint_delete_output

    !> The command: with a prefix argument, kill the output.
    !>
    !> Delete all output from interpreter since last input.
    recursive subroutine cmd_comint_delete_output(status)
        type(status_type), intent(out) :: status
        call comint_delete_output(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_comint_delete_output

    !> Write output from interpreter since last input to FILENAME.
    !> Any prompt at the end of the output is not written.
    !>
    !> If the optional argument APPEND (the prefix argument when
    !> interactive) is true, the output is appended to the file instead
    !> (Deferred: write_region has no APPEND yet, an error says so).  Port
    !> of comint.el:comint-write-output (MUSTBENEW is Deferred).
    subroutine comint_write_output(filename, status, append)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: append
        type(handle_type) :: proc
        integer(int64) :: opoint, shortage, e
        character(len=:), allocatable :: msg
        integer :: k
        if (present(append)) then
            if (append) then
                call set_error(status, FM_GENERIC_ERROR, "Appending output to a file is not supported yet")
                return
            end if
        end if
        call current_process(proc, status)
        if (.not. status%ok()) return
        k = local_entry()
        opoint = point()
        call goto_char(pmark_pos(proc))
        shortage = forward_line(0_int64)
        e = point()
        call goto_char(opoint)
        call write_region(filename, .false., status, start=marker_pos(comint_buffers(k)%last_input_end), end=e, &
                          message=msg)
        if (status%ok() .and. allocated(msg)) call message(msg)
    end subroutine comint_write_output

    !> The command: read the file name.
    !>
    !> Write output from interpreter since last input to FILENAME.
    subroutine cmd_comint_write_output(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        logical :: append
        append = .not. nilp(current_prefix_arg)
        if (append) then
            call read_file_name("Append output to file: ", file, status)
        else
            call read_file_name("Write output to file: ", file, status)
        end if
        if (.not. status%ok()) return
        call comint_write_output(file, status, append)
    end subroutine cmd_comint_write_output

    !> Append output from interpreter since last input to FILENAME.
    !> Any prompt at the end of the output is not written.  Port of
    !> comint.el:comint-append-output-to-file.
    subroutine comint_append_output_to_file(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        call comint_write_output(filename, status, .true.)
    end subroutine comint_append_output_to_file

    !> Display start of this batch of interpreter output at top of window.
    !> Sets mark to the value of point when this command is run.  Port of
    !> comint.el:comint-show-output.
    subroutine comint_show_output(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: pos
        integer :: k
        status = status_type()
        call push_mark()
        pos = 0
        k = entry_of(cur)
        if (k /= 0) pos = marker_pos(comint_buffers(k)%last_input_end)
        if (pos == 0) pos = point_max()
        if (comint_use_prompt_regexp%value) then
            call goto_char(pos)
            call beginning_of_line(0_int64)
            call set_window_start(windows(selected_frame%selected_window%index), point(), st)
            call comint_skip_prompt()
        else
            call goto_char(field_beginning(pos, .false.))
            call set_window_start(windows(selected_frame%selected_window%index), point(), st)
        end if
    end subroutine comint_show_output

    !> Clear the comint buffer.  Port of comint.el:comint-clear-buffer.
    subroutine comint_clear_buffer(status)
        type(status_type), intent(out) :: status
        call truncate_to(0_int64, status)
    end subroutine comint_clear_buffer

    !> Interrupt the current subjob.  Port of
    !> comint.el:comint-interrupt-subjob.
    recursive subroutine comint_interrupt_subjob(status)
        type(status_type), intent(out) :: status
        call comint_skip_input(status)
        if (.not. status%ok()) return
        call interrupt_process(status, current_group=comint_ptyp())
    end subroutine comint_interrupt_subjob

    !> Send kill signal to the current subjob.  Port of
    !> comint.el:comint-kill-subjob.
    recursive subroutine comint_kill_subjob(status)
        type(status_type), intent(out) :: status
        call comint_skip_input(status)
        if (.not. status%ok()) return
        call kill_process(status, current_group=comint_ptyp())
    end subroutine comint_kill_subjob

    !> Send quit signal to the current subjob.  Port of
    !> comint.el:comint-quit-subjob.
    recursive subroutine comint_quit_subjob(status)
        type(status_type), intent(out) :: status
        call comint_skip_input(status)
        if (.not. status%ok()) return
        call quit_process(status, current_group=comint_ptyp())
    end subroutine comint_quit_subjob

    !> Skip all pending input, from last stuff output by interpreter to
    !> point.  This means mark it as if it had been sent as input, without
    !> sending it.  The command keys used to trigger the command that
    !> called this function are inserted into the buffer.  Port of
    !> comint.el:comint-skip-input.
    recursive subroutine comint_skip_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc, marker
        type(value_type) :: inhibit
        type(status_type) :: st
        integer(int64) :: pos
        character(len=:), allocatable :: keys
        call comint_send_input(status, .true., .true., ignore_input, .false.)
        if (.not. status%ok()) return
        call end_of_line()
        pos = point()
        call current_process(proc, status)
        if (.not. status%ok()) return
        marker = process_mark(proc)
        call inhibit_read_only_begin(inhibit)
        keys = ""
        if (allocated(this_command_keys)) keys = key_description(this_command_keys)
        call insert("  "//keys, st)
        if (marker_pos(marker) == pos) call set_marker(marker, point(), st)
        call inhibit_read_only_end(inhibit)
    end subroutine comint_skip_input

    !> Kill all text from last stuff output by interpreter to point.  Port
    !> of comint.el:comint-kill-input.
    subroutine comint_kill_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        integer(int64) :: pm
        call current_process(proc, status)
        if (.not. status%ok()) return
        pm = pmark_pos(proc)
        if (point() > pm) call kill_region(pm, point(), status)
    end subroutine comint_kill_input

    !> Behave like the global binding or send an EOF to subprocess.
    !> Sends an EOF only if point is at the end of the buffer and there is
    !> no input.  Port of comint.el:comint-delchar-or-maybe-eof.
    recursive subroutine comint_delchar_or_maybe_eof(status)
        use keymap_fm, only: keymapp
        use lisp_fm, only: make_keymap_ref
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        type(value_type) :: cmd, remap
        logical :: eof
        proc = get_buffer_process(current_buffer())
        eof = .false.
        if (eobp() .and. proc%index /= 0) eof = point() == pmark_pos(proc)
        if (eof) then
            call comint_send_eof(status)
        else
            call lookup_key(make_keymap_ref(current_global_map()), this_command_keys, cmd, status)
            if (.not. status%ok()) return
            call command_remapping(cmd, remap)
            if (.not. nilp(remap)) cmd = remap
            call call_interactively(cmd, status)
        end if
    end subroutine comint_delchar_or_maybe_eof

    !> Send an EOF to the current buffer's process.  Port of
    !> comint.el:comint-send-eof.
    recursive subroutine comint_send_eof(status)
        type(status_type), intent(out) :: status
        call comint_send_input(status, .true., .true.)
        if (.not. status%ok()) return
        call process_send_eof(status)
    end subroutine comint_send_eof

    !> Search backward through buffer for input fields that match REGEXP.
    !> If `comint_use_prompt_regexp' is true, then input fields are
    !> identified by lines that match `comint_prompt_regexp'.
    !>
    !> With prefix argument N, search for Nth previous match.
    !> If N is negative, find the next or Nth next match.  Port of
    !> comint.el:comint-backward-matching-input.
    subroutine comint_backward_matching_input(regexp, n_in, status)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: n_in
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, found, pos
        integer :: n, dir
        type(value_type) :: f
        status = status_type()
        n = n_in
        opoint = point()
        if (comint_use_prompt_regexp%value) then
            ! Use comint-prompt-regexp.
            if (n > 0) then
                call end_of_line(0_int64)
            else
                call end_of_line(1_int64)
            end if
            call re_search_backward(comint_prompt_regexp%value//".*"//regexp, found, status, noerror=SEARCH_NOERROR, &
                                    count=int(n, int64))
            pos = point()
            call goto_char(opoint)
            if (found == 0) then
                call message("Not found")
                call ding(status=status)
            else
                call goto_char(pos)
                call comint_bol(.false.)
            end if
        else
            ! Use input fields.
            dir = merge(-1, 1, n < 0)
            do while (n /= 0)
                call re_search_backward(regexp, found, status, noerror=SEARCH_NOERROR, count=int(dir, int64))
                if (found == 0) then
                    call goto_char(opoint)
                    call set_error(status, FM_USER_ERROR, "Not found")
                    return
                end if
                f = field_at(point())
                if (nilp(f)) n = n - dir
            end do
            pos = field_beginning(point(), .false.)
            call goto_char(pos)
        end if
    end subroutine comint_backward_matching_input

    !> Search forward through buffer for input fields that match REGEXP.
    !> If `comint_use_prompt_regexp' is true, then input fields are
    !> identified by lines that match `comint_prompt_regexp'.
    !>
    !> With prefix argument N, search for Nth following match.
    !> If N is negative, find the previous or Nth previous match.  Port of
    !> comint.el:comint-forward-matching-input.
    subroutine comint_forward_matching_input(regexp, n, status)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        call comint_backward_matching_input(regexp, -n, status)
    end subroutine comint_forward_matching_input

    !> Move to end of Nth next prompt in the buffer.
    !> If `comint_use_prompt_regexp' is false, then this means the
    !> beginning of the Nth next `input' field, otherwise, it means the Nth
    !> occurrence of text matching `comint_prompt_regexp'.  Port of
    !> comint.el:comint-next-prompt.
    subroutine comint_next_prompt(n_in)
        integer, intent(in) :: n_in
        character(len=:), allocatable :: saved
        integer(int64) :: pos, input_pos, prev_pos, left
        logical :: have_saved, have_input
        integer :: n
        n = n_in
        if (comint_use_prompt_regexp%value) then
            ! Use comint-prompt-regexp.
            have_saved = allocated(paragraph_start%value)
            if (have_saved) saved = paragraph_start%value
            paragraph_start%value = comint_prompt_regexp%value
            if (n > 0) then
                call end_of_line(1_int64)
            else
                call end_of_line(0_int64)
            end if
            left = forward_paragraph(int(n, int64))
            call comint_skip_prompt()
            if (have_saved) then
                paragraph_start%value = saved
            else
                deallocate (paragraph_start%value)
            end if
        else
            ! Use input fields.
            pos = point()
            have_input = .false.
            input_pos = 0
            do while (n /= 0)
                prev_pos = pos
                if (n > 0) then
                    pos = next_single_char_property_change(pos, sym("field"))
                else
                    pos = previous_single_char_property_change(pos, sym("field"))
                end if
                if (pos == prev_pos) then
                    ! Ran off the end of the buffer.
                    if (n > 0) then
                        ! There's always an input field at the end of the
                        ! buffer, but it has a `field' property of nil.
                        input_pos = point_max()
                        have_input = .true.
                    end if
                    ! Stop iterating.
                    n = 0
                else if (nilp(field_at(pos))) then
                    if (n < 0) then
                        n = n + 1
                    else
                        n = n - 1
                    end if
                    input_pos = pos
                    have_input = .true.
                end if
            end do
            if (have_input) call goto_char(input_pos)
        end if
    end subroutine comint_next_prompt

    !> Move to end of Nth previous prompt in the buffer.
    !> If `comint_use_prompt_regexp' is false, then this means the
    !> beginning of the Nth previous `input' field, otherwise, it means the
    !> Nth occurrence of text matching `comint_prompt_regexp'.  Port of
    !> comint.el:comint-previous-prompt.
    subroutine comint_previous_prompt(n)
        integer, intent(in) :: n
        call comint_next_prompt(-n)
    end subroutine comint_previous_prompt

    !> Update read-only status of newline before point.
    !> The `fence' read-only property is used to indicate that a newline
    !> is read-only for no other reason than to "fence off" a following
    !> front-sticky read-only region.  This is used to implement comint
    !> read-only prompts.  If the text after a newline changes, the
    !> read-only status of that newline may need updating.  That is what
    !> this function does.
    !>
    !> This function does nothing if point is not at the beginning of a
    !> line, or is at the beginning of the accessible portion of the
    !> buffer.  Otherwise, if the character after point has a front-sticky
    !> read-only property, then the preceding newline is given a read-only
    !> property of `fence', unless it already is read-only.  If the
    !> character after point does not have a front-sticky read-only
    !> property, any read-only property of `fence' on the preceding newline
    !> is removed.  Port of comint.el:comint-update-fence.
    subroutine comint_update_fence()
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        type(value_type) :: lst, ro, prev_ro
        integer(int64) :: pt
        logical :: sticky, changed
        pt = point()
        if (.not. bolp() .or. bobp()) return
        call get_text_property(pt, sym("front_sticky"), lst, st)
        call get_text_property(pt, sym("read_only"), ro, st)
        call silent_modifications_begin(silent)
        sticky = .false.
        if (.not. nilp(ro)) sticky = front_sticky_read_only(lst)
        call get_text_property(pt - 1, sym("read_only"), prev_ro, st)
        if (sticky) then
            if (nilp(prev_ro)) call put_text_property(pt - 1, pt, sym("read_only"), sym("fence"), st)
        else if (symbol_is(prev_ro, "fence")) then
            call remove_list_of_text_properties(pt - 1, pt, list(sym("read_only")), changed, st)
        end if
        call silent_modifications_end(silent)
    end subroutine comint_update_fence

    !> Whether a front-sticky value LST covers `read_only': (if (listp lst)
    !> (memq 'read-only lst) t).
    logical function front_sticky_read_only(lst) result(yes)
        type(value_type), intent(in) :: lst
        type(value_type), allocatable :: elts(:)
        integer :: i
        if (nilp(lst)) then
            yes = .false.
        else if (consp(lst)) then
            yes = .false.
            call list_elements(lst, elts)
            do i = 1, size(elts)
                if (symbol_is(elts(i), "read_only")) yes = .true.
            end do
        else
            yes = .true.
        end if
    end function front_sticky_read_only

    !> Kill current line, ignoring read-only and field properties.
    !> With prefix arg COUNT, kill that many lines starting from the
    !> current line.  If COUNT is negative, kill backward.  Also kill the
    !> preceding newline, instead of the trailing one.
    !> If COUNT is zero, kill current line but exclude the trailing
    !> newline.  The read-only status of newlines is updated with
    !> `comint_update_fence', if necessary.  Port of
    !> comint.el:comint-kill-whole-line.
    subroutine comint_kill_whole_line(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        type(value_type) :: inhibit
        logical :: saved
        call inhibit_read_only_begin(inhibit)
        saved = inhibit_field_text_motion
        inhibit_field_text_motion = .true.
        call kill_whole_line(count, status)
        if (count >= 0) call comint_update_fence()
        inhibit_field_text_motion = saved
        call inhibit_read_only_end(inhibit)
    end subroutine comint_kill_whole_line

    !> Like `kill_region', but ignore read-only properties, if safe.
    !> This command assumes that the buffer contains read-only "prompts"
    !> which are regions with front-sticky read-only properties at the
    !> beginning of a line, with the preceding newline being read-only to
    !> protect the prompt.  This is true of the comint prompts if
    !> `comint_prompt_read_only' is true.  This command will not delete the
    !> region if this would create mutilated or out of place prompts.  That
    !> is, if any part of a prompt is deleted, the entire prompt must be
    !> deleted and all remaining prompts should stay at the beginning of a
    !> line.  If this is not the case, this command just calls
    !> `kill_region' with all read-only properties intact.  The read-only
    !> status of newlines is updated using `comint_update_fence', if
    !> necessary.  Port of comint.el:comint-kill-region.
    subroutine comint_kill_region(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        type(value_type) :: inhibit, lst, ro
        type(status_type) :: st
        integer(int64) :: opoint, true_beg, true_end
        logical :: beg_bolp, end_bolp, beg_bad, end_bad
        opoint = point()
        true_beg = min(beg, end)
        true_end = max(beg, end)
        call goto_char(true_beg)
        beg_bolp = bolp()
        call get_text_property(true_beg, sym("front_sticky"), lst, st)
        call get_text_property(true_beg, sym("read_only"), ro, st)
        beg_bad = .false.
        if (.not. nilp(ro)) beg_bad = front_sticky_read_only(lst)
        call goto_char(true_end)
        end_bolp = bolp()
        call get_text_property(true_end, sym("front_sticky"), lst, st)
        call get_text_property(true_end, sym("read_only"), ro, st)
        end_bad = .false.
        if (.not. nilp(ro)) end_bad = front_sticky_read_only(lst)
        call goto_char(opoint)
        if ((.not. beg_bolp .and. (beg_bad .or. end_bad)) .or. (.not. end_bolp .and. end_bad)) then
            call kill_region(beg, end, status)
        else
            call inhibit_read_only_begin(inhibit)
            call kill_region(beg, end, status)
            opoint = point()
            call comint_update_fence()
            call goto_char(opoint)
            call inhibit_read_only_end(inhibit)
        end if
    end subroutine comint_kill_region

    !> After fetching a line from input history, this fetches the following
    !> line.  In other words, this recalls the input line after the line
    !> you recalled last.  You can use this to repeat a sequence of input
    !> lines.  Port of comint.el:comint-get-next-from-history.
    subroutine comint_get_next_from_history(status)
        type(status_type), intent(out) :: status
        integer :: k
        status = status_type()
        k = entry_of(cur)
        if (k /= 0) then
            if (comint_buffers(k)%save_input_ring_index >= 0) then
                comint_buffers(k)%input_ring_index = comint_buffers(k)%save_input_ring_index + 1
                call comint_next_input(1, status)
                return
            end if
        end if
        call message("No previous history command")
    end subroutine comint_get_next_from_history

    !> Accumulate a line to send as input along with more lines.
    !> This inserts a newline so that you can enter more text to be sent
    !> along with this line.  Use \[comint_send_input] to send all the
    !> accumulated input, at once.  The entire accumulated text becomes one
    !> item in the input history when you send it.  Port of
    !> comint.el:comint-accumulate.
    subroutine comint_accumulate(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        type(value_type) :: inhibit
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: pm, am
        integer :: k
        status = status_type()
        k = local_entry()
        proc = get_buffer_process(current_buffer())
        if (proc%index /= 0) then
            pm = pmark_pos(proc)
            am = marker_pos(comint_buffers(k)%accum_marker)
            if (am == 0) then
                call set_marker(comint_buffers(k)%accum_marker, pm, st)
                am = pm
            end if
            if (point() >= am .and. am >= pm) then
                ! Delete and reinsert input.  This seems like a no-op,
                ! except for the resulting entries in the undo list:
                ! undoing this insertion will delete the region, moving the
                ! accumulation marker back to its original position.
                call buffer_substring(am, point(), text, st)
                call inhibit_read_only_begin(inhibit)
                call delete_region(am, point(), st)
                call insert(text, st)
                call inhibit_read_only_end(inhibit)
            end if
        end if
        call insert(achar(10), status)
        if (.not. status%ok()) return
        k = local_entry()
        call set_marker(comint_buffers(k)%accum_marker, point(), st)
        if (comint_buffers(k)%input_ring_index >= 0) then
            comint_buffers(k)%save_input_ring_index = comint_buffers(k)%input_ring_index - 1
        end if
    end subroutine comint_accumulate

    !> Move point to the process mark.
    !> The process mark separates output, and input already sent, from
    !> input that has not yet been sent.  Port of
    !> comint.el:comint-goto-process-mark.
    subroutine comint_goto_process_mark(status, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(handle_type) :: proc
        call current_process(proc, status)
        if (.not. status%ok()) return
        call goto_char(pmark_pos(proc))
        if (present(interactive)) then
            if (interactive) call message("Point is now at the process mark")
        end if
    end subroutine comint_goto_process_mark

    !> The command.  Move point to the process mark.
    subroutine cmd_comint_goto_process_mark(status)
        type(status_type), intent(out) :: status
        call comint_goto_process_mark(status, .true.)
    end subroutine cmd_comint_goto_process_mark

    !> Move point to beginning of line (after prompt) or to the process
    !> mark.  The first time you use this command, it moves to the
    !> beginning of the line (but after the prompt, if any).  If you repeat
    !> it again immediately, it moves point to the process mark.
    !>
    !> The process mark separates the process output, along with input
    !> already sent, from input that has not yet been sent.  Ordinarily,
    !> the process mark is at the beginning of the current input line; but
    !> if you have used \[comint_accumulate] to send multiple lines at once,
    !> the process mark is at the beginning of the accumulated input.  Port
    !> of comint.el:comint-bol-or-process-mark.
    subroutine comint_bol_or_process_mark(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. symbol_is(last_command, "comint_bol_or_process_mark")) then
            call comint_bol(.false.)
        else
            call comint_goto_process_mark(status)
        end if
    end subroutine comint_bol_or_process_mark

    !> Set the process mark at point.  Port of
    !> comint.el:comint-set-process-mark.
    subroutine comint_set_process_mark(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        type(status_type) :: st
        call current_process(proc, status)
        if (.not. status%ok()) return
        call set_marker(process_mark(proc), point(), st)
        call message("Process mark set")
    end subroutine comint_set_process_mark

    ! ------------------------------------------------------------------
    ! The commands' interactive forms
    ! ------------------------------------------------------------------

    !> The count of the prefix argument.
    integer function prefix_count()
        prefix_count = int(prefix_numeric_value(current_prefix_arg))
    end function prefix_count

    !> Cycle backwards through input history, saving input.
    subroutine cmd_comint_previous_input(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call comint_previous_input(prefix_count(), status)
    end subroutine cmd_comint_previous_input

    !> Cycle forwards through input history.
    subroutine cmd_comint_next_input(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call comint_next_input(prefix_count(), status)
    end subroutine cmd_comint_next_input

    !> Search backwards through input history for match for REGEXP.
    subroutine cmd_comint_previous_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer :: n
        call comint_regexp_arg("Previous input matching (regexp): ", regexp, n, status)
        if (status%ok()) call comint_previous_matching_input(regexp, n, status)
    end subroutine cmd_comint_previous_matching_input

    !> Search forwards through input history for match for REGEXP.
    subroutine cmd_comint_next_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer :: n
        call comint_regexp_arg("Next input matching (regexp): ", regexp, n, status)
        if (status%ok()) call comint_next_matching_input(regexp, n, status)
    end subroutine cmd_comint_next_matching_input

    !> Search backwards through input history for match for current input.
    subroutine cmd_comint_previous_matching_input_from_input(status)
        type(status_type), intent(out) :: status
        call comint_previous_matching_input_from_input(prefix_count(), status)
    end subroutine cmd_comint_previous_matching_input_from_input

    !> Search forwards through input history for match for current input.
    subroutine cmd_comint_next_matching_input_from_input(status)
        type(status_type), intent(out) :: status
        call comint_next_matching_input_from_input(prefix_count(), status)
    end subroutine cmd_comint_next_matching_input_from_input

    !> Search backward through buffer for input fields that match REGEXP.
    subroutine cmd_comint_backward_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer :: n
        call comint_regexp_arg("Backward input matching (regexp): ", regexp, n, status)
        if (status%ok()) call comint_backward_matching_input(regexp, n, status)
    end subroutine cmd_comint_backward_matching_input

    !> Search forward through buffer for input fields that match REGEXP.
    subroutine cmd_comint_forward_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer :: n
        call comint_regexp_arg("Forward input matching (regexp): ", regexp, n, status)
        if (status%ok()) call comint_forward_matching_input(regexp, n, status)
    end subroutine cmd_comint_forward_matching_input

    !> Move to end of Nth next prompt in the buffer.
    subroutine cmd_comint_next_prompt(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call comint_next_prompt(prefix_count())
    end subroutine cmd_comint_next_prompt

    !> Move to end of Nth previous prompt in the buffer.
    subroutine cmd_comint_previous_prompt(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call comint_previous_prompt(prefix_count())
    end subroutine cmd_comint_previous_prompt

    !> Go to the beginning of line, then skip past the prompt, if any.
    subroutine cmd_comint_bol(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call comint_bol(.not. nilp(current_prefix_arg))
    end subroutine cmd_comint_bol

    !> Kill current line, ignoring read-only and field properties.
    subroutine cmd_comint_kill_whole_line(status)
        type(status_type), intent(out) :: status
        call comint_kill_whole_line(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_comint_kill_whole_line

    !> Like `kill_region', but ignore read-only properties, if safe.
    subroutine cmd_comint_kill_region(status)
        use editfns_fm, only: region_beginning, region_end
        type(status_type), intent(out) :: status
        integer(int64) :: b, e
        call region_beginning(b, status)
        if (.not. status%ok()) return
        call region_end(e, status)
        if (.not. status%ok()) return
        call comint_kill_region(b, e, status)
    end subroutine cmd_comint_kill_region

    !> Append output from interpreter since last input to FILENAME.
    subroutine cmd_comint_append_output_to_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        call read_file_name("Append output to file: ", file, status)
        if (status%ok()) call comint_append_output_to_file(file, status)
    end subroutine cmd_comint_append_output_to_file

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Define Comint mode, its map, faces and settings, and register the
    !> commands.
    subroutine syms_of_comint()
        type(status_type) :: st
        if (associated(major_modes%comint_mode%run)) return
        call major_modes%comint_mode%define("comint_mode", "Comint", comint_mode, comint_mode_hook)
        major_modes%comint_mode%special = .true.
        comint_mode_map = major_modes%comint_mode%map
        call define_faces()
        if (.not. allocated(comint_prompt_regexp%value)) comint_prompt_regexp = "^"
        if (.not. allocated(comint_delimiter_argument_list%value)) comint_delimiter_argument_list = ""
        if (.not. allocated(comint_input_ring_separator%value)) comint_input_ring_separator = achar(10)
        if (.not. allocated(comint_input_history_ignore%value)) comint_input_history_ignore = "^#"
        if (.not. allocated(comint_terminfo_terminal%value)) comint_terminfo_terminal = "dumb"
        call forward_setting(comint_prompt_regexp)
        call forward_setting(comint_delimiter_argument_list)
        call forward_setting(comint_prompt_read_only)
        call forward_setting(comint_highlight_input)
        call forward_setting(comint_input_ignoredups)
        call forward_setting(comint_input_ring_file_name)
        call forward_setting(comint_pager)
        call forward_setting(comint_input_ring_file_prefix)
        call forward_setting(comint_move_point_for_output)
        call forward_setting(comint_move_point_for_matching_input)
        call forward_setting(comint_scroll_show_maximum_output)
        call forward_setting(comint_buffer_maximum_size)
        call forward_setting(comint_input_ring_size)
        call forward_setting(comint_input_ring_separator)
        call forward_setting(comint_input_history_ignore)
        call forward_setting(comint_process_echoes)
        call forward_setting(comint_eol_on_send)
        call forward_setting(comint_use_prompt_regexp)
        call forward_setting(comint_terminfo_terminal)
        call forward_setting(comint_inhibit_carriage_motion)
        call comint_output_filter_functions%add(comint_postoutput_scroll_to_bottom)
        call add_local_variables_killer(kill_comint_locals)
        ! Keys:
        call keymap_set(comint_mode_map, "M-p", "comint_previous_input", st)
        call keymap_set(comint_mode_map, "M-n", "comint_next_input", st)
        call keymap_set(comint_mode_map, "C-<up>", "comint_previous_input", st)
        call keymap_set(comint_mode_map, "C-<down>", "comint_next_input", st)
        call keymap_set(comint_mode_map, "C-c M-r", "comint_previous_matching_input_from_input", st)
        call keymap_set(comint_mode_map, "C-c M-s", "comint_next_matching_input_from_input", st)
        call keymap_set(comint_mode_map, "C-M-l", "comint_show_output", st)
        call keymap_set(comint_mode_map, "RET", "comint_send_input", st)
        call keymap_set(comint_mode_map, "C-d", "comint_delchar_or_maybe_eof", st)
        ! The following two are standardly bound to delete-forward-char, but
        ! they should never do EOF, just delete.
        call keymap_set(comint_mode_map, "<delete>", "delete_forward_char", st)
        call keymap_set(comint_mode_map, "<kp-delete>", "delete_forward_char", st)
        call keymap_set(comint_mode_map, "C-c SPC", "comint_accumulate", st)
        call keymap_set(comint_mode_map, "C-c C-x", "comint_get_next_from_history", st)
        call keymap_set(comint_mode_map, "C-c C-a", "comint_bol_or_process_mark", st)
        call keymap_set(comint_mode_map, "C-c C-u", "comint_kill_input", st)
        call keymap_set(comint_mode_map, "C-c C-w", "backward_kill_word", st)
        call keymap_set(comint_mode_map, "C-c C-c", "comint_interrupt_subjob", st)
        call keymap_set(comint_mode_map, "C-c C-\", "comint_quit_subjob", st)
        call keymap_set(comint_mode_map, "C-c RET", "comint_copy_old_input", st)
        call keymap_set(comint_mode_map, "C-c C-o", "comint_delete_output", st)
        call keymap_set(comint_mode_map, "C-c M-o", "comint_clear_buffer", st)
        call keymap_set(comint_mode_map, "C-c C-r", "comint_show_output", st)
        call keymap_set(comint_mode_map, "C-c C-e", "comint_show_maximum_output", st)
        call keymap_set(comint_mode_map, "C-c C-n", "comint_next_prompt", st)
        call keymap_set(comint_mode_map, "C-c C-p", "comint_previous_prompt", st)
        call keymap_set(comint_mode_map, "C-c C-d", "comint_send_eof", st)
        call keymap_set(comint_mode_map, "C-c C-s", "comint_write_output", st)
        commands%comint_mode%run => comint_mode
        commands%comint_run%run => cmd_comint_run
        commands%comint_send_input%run => cmd_comint_send_input
        commands%comint_previous_input%run => cmd_comint_previous_input
        commands%comint_next_input%run => cmd_comint_next_input
        commands%comint_previous_matching_input%run => cmd_comint_previous_matching_input
        commands%comint_next_matching_input%run => cmd_comint_next_matching_input
        commands%comint_previous_matching_input_from_input%run => cmd_comint_previous_matching_input_from_input
        commands%comint_next_matching_input_from_input%run => cmd_comint_next_matching_input_from_input
        commands%comint_restore_input%run => comint_restore_input
        commands%comint_backward_matching_input%run => cmd_comint_backward_matching_input
        commands%comint_forward_matching_input%run => cmd_comint_forward_matching_input
        commands%comint_show_output%run => comint_show_output
        commands%comint_show_maximum_output%run => comint_show_maximum_output
        commands%comint_delchar_or_maybe_eof%run => comint_delchar_or_maybe_eof
        commands%comint_send_eof%run => comint_send_eof
        commands%comint_accumulate%run => comint_accumulate
        commands%comint_get_next_from_history%run => comint_get_next_from_history
        commands%comint_bol_or_process_mark%run => comint_bol_or_process_mark
        commands%comint_bol%run => cmd_comint_bol
        commands%comint_goto_process_mark%run => cmd_comint_goto_process_mark
        commands%comint_set_process_mark%run => comint_set_process_mark
        commands%comint_kill_input%run => comint_kill_input
        commands%comint_interrupt_subjob%run => comint_interrupt_subjob
        commands%comint_kill_subjob%run => comint_kill_subjob
        commands%comint_quit_subjob%run => comint_quit_subjob
        commands%comint_copy_old_input%run => comint_copy_old_input
        commands%comint_delete_output%run => cmd_comint_delete_output
        commands%comint_write_output%run => cmd_comint_write_output
        commands%comint_append_output_to_file%run => cmd_comint_append_output_to_file
        commands%comint_clear_buffer%run => comint_clear_buffer
        commands%comint_truncate_buffer%run => cmd_comint_truncate_buffer
        commands%comint_strip_ctrl_m%run => cmd_comint_strip_ctrl_m
        commands%comint_next_prompt%run => cmd_comint_next_prompt
        commands%comint_previous_prompt%run => cmd_comint_previous_prompt
        commands%comint_kill_whole_line%run => cmd_comint_kill_whole_line
        commands%comint_kill_region%run => cmd_comint_kill_region
    end subroutine syms_of_comint

    !> The faces of comint.el.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        f => comint_highlight_input_face
        call f%define("comint_highlight_input", "Face to use to highlight user input.")
        call f%spec_entry(display_when(), face_attrs(weight="bold"))
        f => comint_highlight_prompt
        call f%define("comint_highlight_prompt", "Face to use to highlight prompts.")
        call f%spec_entry(display_when(), face_attrs(inherit="minibuffer_prompt"))
    end subroutine define_faces

    !> Add S at the end of LIST.
    subroutine append_arg(list, s)
        type(process_arg_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(process_arg_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))
        do k = 1, n
            if (allocated(list(k)%s)) call move_alloc(list(k)%s, grown(k)%s)
        end do
        grown(n + 1)%s = s
        call move_alloc(grown, list)
    end subroutine append_arg

    !> Add S at the start of LIST.
    subroutine prepend_arg(list, s)
        type(process_arg_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(process_arg_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))
        grown(1)%s = s
        do k = 1, n
            if (allocated(list(k)%s)) call move_alloc(list(k)%s, grown(k + 1)%s)
        end do
        call move_alloc(grown, list)
    end subroutine prepend_arg
end module comint_fm
