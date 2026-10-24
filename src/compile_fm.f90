! compile_fm.f90 -- run compiler as inferior of Emacs, parse error messages.
! Ported from GNU Emacs 31.1 lisp/progmodes/compile.el, with
! lisp/files.el cd, lisp/simple.el forward-to-indentation and
! lisp/startup.el command-line-normalize-file-name.
!
! Copyright (C) 1985-1987, 1993-1999, 2001-2026 Free Software
! Foundation, Inc.
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

!> Compilation: `compile' runs a shell command asynchronously with its
!> output in *compilation*, in Compilation mode (or, with C-u, in Comint
!> mode with `compilation_shell_minor_mode'), with the "Compilation
!> started at ..." header and the "Compilation finished at ..., duration
!> ..." line when it ends; `recompile' (g) runs it again and
!> `kill_compilation' (C-c C-k) interrupts it.  While it runs the mode
!> line shows ":run" and "[Compiling] ", and the counts of errors,
!> warnings and informational messages found so far.
!>
!> The output is parsed with the rules of `compilation_error_regexp_alist'
!> (all of `compilation_error_regexp_alist_alist' but `omake'), following
!> the "Entering directory" messages of `compilation_directory_matcher';
!> each message gets a `compilation_message' text property and faces.
!> `next_error' (M-g n, C-x `) and `compile_goto_error' (RET) visit the
!> source of a message, `compilation_next_error' (M-n, TAB) and friends
!> move between messages.  `compilation_minor_mode' and
!> `compilation_shell_minor_mode' do the same in other buffers.
!>
!> Not ported yet (Deferred): `compilation_auto_jump_to_first_error' and
!> the `first-error' value of `compilation_scroll_output' beyond scrolling
!> (need timers), highlighting the locus (`next_error_highlight',
!> `compilation_highlight_regexp': need overlays), the overlay arrow and
!> margins (`compilation_context_lines' true, the fringe), the `keymap'
!> text property (`compilation_button_map'), the mouse, menus and tool
!> bar, `compilation_goto_in_progress_buffer'; elements of
!> `compilation_error_regexp_alist' that are rule specifications rather
!> than names (add the rule to `compilation_error_regexp_alist_alist'),
!> FILE given as (FILE FORMAT...), HIGHLIGHT faces given as property
!> lists, `compilation_parse_errors_filename_function',
!> `comint_file_name_prefix' (remote files), `compilation_search_path'
!> and `compilation_search_extra_path' (only the directory of the message
!> and, with `compilation_search_all_directories', the directories
!> entered before it are searched), `compilation_fake_loc',
!> `compilation_debug'; the parts (face nil compilation_message nil ...)
!> of the font-lock keywords of the "Compilation finished" and
!> "Compilation exited" lines, and removing the font-lock keywords when
!> the minor modes are turned off (`font_lock_remove_keywords'); visiting
!> a directory message with `compile_goto_error' (`dired_other_window');
!> `selective_display' in `compilation_beginning_of_line';
!> `compilation_max_output_line_length' (long lines are not abbreviated:
!> needs buttons and display properties), `compilation_hidden_output',
!> `compilation_window_height', `compilation_page_delimiter'
!> (`page_delimiter' is not buffer-local), `compilation_environment',
!> `compilation_buffer_name_function',
!> `compilation_process_setup_function', `compilation_start_hook',
!> `compilation_finish_functions', `compilation_exit_message_function',
!> `compilation_save_buffers_predicate' (all modified file buffers are
!> offered), the NAME-FUNCTION, HIGHLIGHT-REGEXP and CONTINUE arguments
!> of `compilation_start', `revert_buffer_function' (g is `recompile'
!> itself), wildcards in a leading "cd" of the command
!> (`file-expand-wildcards'), `define_compilation_mode' and grep,
!> `next_error_follow_minor_mode' (C-c C-f) and `recenter_current_error'
!> (l).
module compile_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nilp, consp, t_value, make_string, list, nil, make_fixnum, fixnump, xfixnum
    use lread_fm, only: sym
    use data_fm, only: string_setting, logical_setting, integer_setting, symbol_value, set, &
                       add_local_variables_killer, forward_setting
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, buffer_live_p, &
                         buffer_list, erase_buffer, restore_buffer_modified_p
    use marker_fm, only: copy_marker_at, free_marker, marker_position, set_marker, marker_buffer
    use insdel_fm, only: insert, before_change_functions
    use editfns_fm, only: point, point_min, point_max, goto_char, widen, narrow_to_region, &
                          line_beginning_position, line_end_position, eobp, bobp, bolp, buffer_size, &
                          buffer_substring, point_marker
    use textprop_fm, only: put_text_property, get_text_property, add_text_properties, &
                           remove_list_of_text_properties, next_single_property_change, &
                           previous_single_property_change, text_property_not_all
    use cmds_fm, only: forward_line, beginning_of_line, end_of_line
    use indent_fm, only: move_to_column, current_column
    use syntax_fm, only: skip_chars_forward
    use search_fm, only: string_match, re_search_forward, match_end, match_data, set_match_data, replace_match, &
                         case_fold_search, SEARCH_NOERROR
    use subr_fm, only: match_string, silent_modifications_type, silent_modifications_begin, silent_modifications_end
    use env_fm, only: substitute_env_vars, getenv
    use fileio_fm, only: expand_file_name, file_name_as_directory, file_name_absolute_p, file_directory_p, &
                         file_exists_p, file_accessible_directory_p, file_name_directory, file_name_nondirectory
    use files_fm, only: abbreviate_file_name, save_some_buffers, find_file_noselect, file_truename
    use print_fm, only: prin1_to_string
    use fns_fm, only: yes_or_no_p
    use eval_fm, only: hook_type, change_major_mode_hook
    use keyboard_fm, only: sit_for
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use xdisp_fm, only: message, mode_line_part_type, set_mode_line_process, add_mode_line_modes_entry, tab_width
    use window_fm, only: windows, display_buffer, set_window_start, set_window_point, pop_to_buffer, switch_to_buffer, &
                         get_buffer_window, select_window, display_alist_type, pop_up_windows
    use frame_fm, only: selected_frame
    use keymap_fm, only: set_keymap_parent, keymap_set
    use modes_fm, only: major_modes, minor_modes
    use simple_fm, only: special_mode_map, read_shell_command, shell_command_switch, next_error_last_buffer, &
                         set_next_error_function, next_error_find_buffer, next_error_internal, next_error, push_mark
    use callproc_fm, only: shell_file_name, init_callproc
    use process_fm, only: process_arg_type, make_process, get_buffer_process, process_status, &
                          process_query_on_exit_flag, interrupt_process, delete_process, process_buffer, &
                          process_exit_status, set_process_buffer, set_process_query_on_exit_flag, process_mark, &
                          process_send_eof, num_processors
    use sysdep_fm, only: emacs_time, emacs_localtime_wday
    use version_fm, only: upstream_version
    use comint_fm, only: comint_carriage_motion, comint_inhibit_carriage_motion, comint_mode, comint_exec
    use faces_fm, only: named_face_type, display_when, face_attrs, FACE_CLASS_COLOR
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                            FONT_LOCK_OVERRIDE_T
    use font_lock_fm, only: font_lock_append_text_property, font_lock_add_keywords, font_lock_flush
    implicit none(type, external)
    private

    public :: compile, recompile, compilation_start, compilation_mode, kill_compilation, compilation_filter, &
              compilation_sentinel, compilation_handle_exit, comint_carriage_motion, syms_of_compile, cd
    public :: compilation_parse_errors, compilation_ensure_parse, compilation_next_error, compilation_previous_error, &
              compilation_next_file, compilation_previous_file, compilation_display_error, compile_goto_error, &
              compilation_next_error_function, compilation_find_buffer, compilation_forget_errors, &
              compilation_set_skip_threshold, compilation_minor_mode, compilation_shell_minor_mode, &
              compilation_setup, compilation_face, compilation_buffer_internal_p, compilation_buffer_p, &
              compilation_message_info, compilation_message_counts, compilation_find_file

    abstract interface
        !> A rule's LINE, END-LINE, COLUMN or END-COLUMN given as a function
        !> of no arguments: the number, or FOUND false for none.  It can
        !> assume the rule's regexp has just been matched (point is at the
        !> end of the match), and should preserve the match data.
        subroutine compilation_number_function(number, found)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(out) :: number
            logical, intent(out) :: found
        end subroutine compilation_number_function

        !> A rule's FILE given as a function: FILENAME (relative to
        !> DIRECTORY if that is allocated), or FOUND false for none.
        subroutine compilation_file_function(filename, directory, found)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: filename, directory
            logical, intent(out) :: found
        end subroutine compilation_file_function

        !> A HIGHLIGHT's FACE given as a function evaluated after each
        !> match: the face's name (FACE unallocated for none).
        recursive subroutine compilation_face_function(face, status)
            import :: status_type
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: face
            type(status_type), intent(out) :: status
        end subroutine compilation_face_function
    end interface

    public :: compilation_number_function, compilation_file_function, compilation_face_function

    !> A HIGHLIGHT of a rule: the text of SUBMATCH gets the face FACE (a
    !> face name), or the one FACE_FUNCTION gives.
    type, public :: compilation_highlight_type
        integer :: submatch = 0
        character(len=:), allocatable :: face
        procedure(compilation_face_function), pointer, nopass :: face_function => null()
    end type compilation_highlight_type

    !> A rule for matching messages: an element of
    !> `compilation_error_regexp_alist_alist' (see
    !> `compilation_error_regexp_alist').  Subexpression numbers are 0 for
    !> none (nil); WARNING and INFO, if either is not 0, make the type
    !> depend on which of them matched ((WARNING . INFO)), otherwise TYPE
    !> is the type (2 error, 1 warning, 0 info).
    type, public :: compilation_rule_type
        !> The rule's name (`compilation_error_regexp_alist' refers to it).
        character(len=:), allocatable :: name
        character(len=:), allocatable :: regexp
        integer :: file = 0
        procedure(compilation_file_function), pointer, nopass :: file_function => null()
        integer :: line = 0, end_line = 0, column = 0, end_column = 0
        procedure(compilation_number_function), pointer, nopass :: line_function => null()
        procedure(compilation_number_function), pointer, nopass :: end_line_function => null()
        procedure(compilation_number_function), pointer, nopass :: column_function => null()
        procedure(compilation_number_function), pointer, nopass :: end_column_function => null()
        integer :: type = 2
        integer :: warning = 0, info = 0
        integer :: hyperlink = 0
        type(compilation_highlight_type), allocatable :: highlights(:)
    end type compilation_rule_type

    !> The value of `compilation_directory_matcher': REGEXP (unallocated
    !> for nil) and the subexpressions DIR(i), with LEAVE(i) (0 for nil).
    type, public :: compilation_directory_matcher_type
        character(len=:), allocatable :: regexp
        integer, allocatable :: dir(:), leave(:)
    end type compilation_directory_matcher_type

    !> An element of `compilation_transform_file_match_alist': REGEXP and
    !> REPLACEMENT (unallocated for nil).
    type, public :: compilation_transform_type
        character(len=:), allocatable :: regexp, replacement
    end type compilation_transform_type

    !> What `compilation_message_info' tells about a message: its RULE
    !> (unallocated for an anonymous rule), TYPE, and the location (FILE,
    !> DIRECTORY, LINE, COLUMN; HAS_LOC false for none) and end location.
    type, public :: compilation_message_info_type
        character(len=:), allocatable :: rule
        integer :: type = 2
        logical :: has_loc = .false.
        character(len=:), allocatable :: file, directory
        logical :: has_line = .false., has_column = .false.
        integer(int64) :: line = 0, column = 0
        logical :: has_end = .false., has_end_line = .false., has_end_column = .false.
        integer(int64) :: end_line = 0, end_column = 0
    end type compilation_message_info_type

    !> Last shell command used to do a compilation; default for next
    !> compilation.
    !>
    !> Sometimes it is useful for files to supply local values for this
    !> variable.  You might also use mode hooks to specify it in certain
    !> modes.
    !>
    !> It's often useful to leave a space at the end of the value.  Port of
    !> compile.el compile-command ("make -k -jN " with N the number of
    !> processors divided by 1.5, rounded up).
    type(string_setting), public :: compile_command = string_setting(name="compile_command")

    !> True means \[compile] reads the compilation command to use.
    !> Otherwise, \[compile] just uses the value of `compile_command'.
    !>
    !> Note that changing this to false may be a security risk, because a
    !> file might define a malicious `compile_command' as a file local
    !> variable, and you might not notice.  Therefore, `compile_command'
    !> is considered unsafe if this variable is false.  Port of compile.el
    !> compilation-read-command.
    type(logical_setting), public :: compilation_read_command = &
                                     logical_setting(name="compilation_read_command", value=.true.)

    !> True means \[compile] asks which buffers to save before compiling.
    !> Otherwise, it saves all modified buffers without asking.  Port of
    !> compile.el compilation-ask-about-save.
    type(logical_setting), public :: compilation_ask_about_save = &
                                     logical_setting(name="compilation_ask_about_save", value=.true.)

    !> If true, always kill a running compilation process before starting
    !> a new one.  If false, ask to kill it.  Port of compile.el
    !> compilation-always-kill.
    type(logical_setting), public :: compilation_always_kill = &
                                     logical_setting(name="compilation_always_kill", value=.false.)

    !> If true, send end-of-file as compilation process input.
    !> This only affects platforms that support asynchronous processes (see
    !> `start_process'); synchronous compilation processes never accept
    !> input.  Port of compile.el compilation-disable-input.
    type(logical_setting), public :: compilation_disable_input = &
                                     logical_setting(name="compilation_disable_input", value=.false.)

    !> Values of `compilation_scroll_output'.
    integer, parameter, public :: COMPILATION_SCROLL_NIL = 0, COMPILATION_SCROLL_T = 1, &
                                  COMPILATION_SCROLL_FIRST_ERROR = 2

    !> True to scroll the *compilation* buffer window as output appears.
    !>
    !> Setting it causes the Compilation mode commands to put point at the
    !> end of their output window so that the end of the output is always
    !> visible rather than the beginning.
    !>
    !> COMPILATION_SCROLL_FIRST_ERROR stops scrolling at the first error,
    !> and leaves point on its location in the *compilation* buffer.  Port of
    !> compile.el compilation-scroll-output (COMPILATION_SCROLL_NIL is nil,
    !> COMPILATION_SCROLL_T t, COMPILATION_SCROLL_FIRST_ERROR `first-error').
    type(integer_setting), public :: compilation_scroll_output = &
                                     integer_setting(name="compilation_scroll_output", value=COMPILATION_SCROLL_NIL)

    !> Hook run after `compilation_filter' has inserted a string into the
    !> buffer.  It is called with the variable `compilation_filter_start'
    !> bound to the position of the start of the inserted text, and point at
    !> its end.
    !>
    !> If Emacs lacks asynchronous process support, this hook is run
    !> after `call_process' inserts the grep output into the buffer.  Port
    !> of compile.el compilation-filter-hook.
    type(hook_type), target, public :: compilation_filter_hook = hook_type(name="compilation_filter_hook")

    !> Position of the start of the text inserted by `compilation_filter'.
    !> This is bound before running `compilation_filter_hook'.  Port of
    !> compile.el compilation-filter-start.
    integer(int64), public :: compilation_filter_start = 0

    !> Hook run after entering Compilation mode.  Port of compile.el
    !> compilation-mode-hook.
    type(hook_type), target, public :: compilation_mode_hook = hook_type(name="compilation_mode_hook")

    !> Hook run after entering or leaving `compilation_minor_mode'.  Port
    !> of compile.el compilation-minor-mode-hook.
    type(hook_type), target, public :: compilation_minor_mode_hook = hook_type(name="compilation_minor_mode_hook")

    !> Hook run after entering or leaving `compilation_shell_minor_mode'.
    !> Port of compile.el compilation-shell-minor-mode-hook.
    type(hook_type), target, public :: compilation_shell_minor_mode_hook = &
                                       hook_type(name="compilation_shell_minor_mode_hook")

    !> Keymap for compilation log buffers.  Port of compile.el
    !> compilation-mode-map.
    type(handle_type), public :: compilation_mode_map

    !> Keymap for `compilation_minor_mode'.  Port of compile.el
    !> compilation-minor-mode-map.
    type(handle_type), public :: compilation_minor_mode_map

    !> Keymap for `compilation_shell_minor_mode'.  Port of compile.el
    !> compilation-shell-minor-mode-map.
    type(handle_type), public :: compilation_shell_minor_mode_map

    !> Directory to restore to when doing `recompile'.  Port of compile.el
    !> compilation-directory (this is its default value; a compilation
    !> buffer has a value of its own, the directory it was started in).
    type(string_setting), public :: compilation_directory = string_setting(name="compilation_directory")

    !> Alist of values for `compilation_error_regexp_alist'.  Port of
    !> compile.el compilation-error-regexp-alist-alist (the rules are named
    !> with underscores for hyphens: `gcc_include', `edg_1').
    type(compilation_rule_type), allocatable, public :: compilation_error_regexp_alist_alist(:)

    !> Alist that specifies how to match errors in compiler output.
    !> On GNU and Unix, any string is a valid filename, so these
    !> matchers must make some common sense assumptions, which catch
    !> normal cases.  A shorter list will be lighter on resource usage.
    !>
    !> Each element is the name of a rule of
    !> `compilation_error_regexp_alist_alist'.
    !>
    !> A rule has the form (REGEXP FILE [LINE COLUMN TYPE HYPERLINK
    !> HIGHLIGHT...]).  If REGEXP matches, the FILE'th subexpression
    !> gives the file name, and the LINE'th subexpression gives the line
    !> number.  The COLUMN'th subexpression gives the column number on
    !> that line.
    !>
    !> If FILE, LINE or COLUMN are 0 or that index didn't match, that
    !> information is not present on the matched line.  In that case the
    !> file name is assumed to be the same as the previous one in the
    !> buffer, line number defaults to 1 and column defaults to
    !> beginning of line's indentation.
    !>
    !> FILE can also be a function that returns FILENAME or
    !> RELATIVE-FILENAME and DIRNAME.
    !>
    !> LINE can also be given with END-LINE, meaning a range of lines.
    !> COLUMN can also be given with END-COLUMN, meaning a range of columns
    !> starting on LINE and ending on END-LINE, if that matched.
    !>
    !> LINE, END-LINE, COL, and END-COL can also be functions of no
    !> argument that return the corresponding line or column number.  They
    !> can assume REGEXP has just been matched, and should correspondingly
    !> preserve this match data.
    !>
    !> TYPE is 2 for a real error or 1 for warning or 0 for info.
    !> TYPE can also be given as WARNING and INFO.  In that case this will
    !> be equivalent to 1 if the WARNING'th subexpression matched or else
    !> equivalent to 0 if the INFO'th subexpression matched, or else
    !> equivalent to 2 if neither of them matched.  See
    !> `compilation_error_face', `compilation_warning_face',
    !> `compilation_info_face' and `compilation_skip_threshold'.
    !>
    !> What matched the HYPERLINK'th subexpression has `mouse_face' and
    !> `compilation_message_face' applied.  If this is 0, the text
    !> matched by the whole REGEXP becomes the hyperlink.
    !>
    !> Additional HIGHLIGHTs take the shape (SUBMATCH FACE), where
    !> SUBMATCH is the number of a submatch and FACE is a face name, or a
    !> function giving one.
    !>
    !> After identifying compilation errors and warnings determined by this
    !> variable, the `compilation_transform_file_match_alist' variable
    !> is then consulted.  It allows further transformations of the
    !> matched file names, and ignoring false positives.  Port of
    !> compile.el compilation-error-regexp-alist.
    character(len=64), allocatable, public :: compilation_error_regexp_alist(:)

    !> If true, use case-insensitive matching of compilation errors.
    !> If false, matching is case-sensitive.
    !>
    !> Compilation errors are given by the regexps in
    !> `compilation_error_regexp_alist' and
    !> `compilation_error_regexp_alist_alist'.
    !>
    !> This variable should only be set for backward compatibility as a
    !> temporary measure.  The proper solution is to use a regexp that
    !> matches the messages without case-folding.  Port of compile.el
    !> compilation-error-case-fold-search.
    type(logical_setting), public :: compilation_error_case_fold_search = &
                                     logical_setting(name="compilation_error_case_fold_search", value=.false.)

    !> A value for tracking when directories are entered or left.
    !> If its regexp is unallocated, do not track directories, e.g. if all
    !> file names are absolute.  The REGEXP matches these messages.  It can
    !> match any number of variants, e.g. different languages.  The pairs
    !> DIR and LEAVE say: if the DIR'th subexpression matches, that is a
    !> directory name.  If LEAVE is 0 or the corresponding LEAVE'th
    !> subexpression doesn't match, this message is about going into
    !> another directory.  If it does match anything, this message is
    !> about going back to the directory we were in before the last
    !> entering message.  Port of compile.el compilation-directory-matcher.
    type(compilation_directory_matcher_type), public :: compilation_directory_matcher

    !> Alist of regexp/replacements to alter file names in compiler
    !> messages.  If the replacement is unallocated (nil), the matching
    !> message will not be considered an error or warning.  If not, it
    !> should be a replacement string for the matched regexp.
    !>
    !> If a replacement is specified, the value of the matched file name
    !> used to locate the warning or error is modified using the
    !> replacement, but the compilation buffer still displays the original
    !> value.  Port of compile.el compilation-transform-file-match-alist.
    type(compilation_transform_type), allocatable, public :: compilation_transform_file_match_alist(:)

    !> If true, column numbers in error messages are screen columns.
    !> Otherwise they are interpreted as character positions, with
    !> each character occupying one column.
    !> The default is to use screen columns, which requires that the
    !> compilation program and Emacs agree about the display width of the
    !> characters, especially the TAB character.
    !> If this is buffer-local in the destination buffer, Emacs obeys
    !> that value, otherwise it uses the value in the *compilation*
    !> buffer.  This enables a major mode to specify its own value.  Port
    !> of compile.el compilation-error-screen-columns.
    type(logical_setting), public :: compilation_error_screen_columns = &
                                     logical_setting(name="compilation_error_screen_columns", value=.true.)

    !> This is how compilers number the first column, usually 1 or 0.
    !> If this is buffer-local in the destination buffer, Emacs obeys
    !> that value, otherwise it uses the value in the *compilation*
    !> buffer.  This enables a major mode to specify its own value.  Port
    !> of compile.el compilation-first-column.
    type(integer_setting), public :: compilation_first_column = &
                                     integer_setting(name="compilation_first_column", value=1)

    !> Compilation motion commands skip less important messages.
    !> The value can be either 2 -- skip anything less than error, 1 --
    !> skip anything less than warning or 0 -- don't skip any messages.
    !> Note that all messages not positively identified as warning or
    !> info, are considered errors.  Port of compile.el
    !> compilation-skip-threshold.
    type(integer_setting), public :: compilation_skip_threshold = &
                                     integer_setting(name="compilation_skip_threshold", value=1)

    !> Compilation motion commands skip visited messages if this is true.
    !> Visited messages are ones for which the file, line and column have
    !> been jumped to from the current content in the current compilation
    !> buffer, even if it was from a different message.  Port of
    !> compile.el compilation-skip-visited.
    type(logical_setting), public :: compilation_skip_visited = &
                                     logical_setting(name="compilation_skip_visited", value=.false.)

    !> If true, skip multiple error messages for the same source location.
    !> Port of compile.el compilation-skip-to-next-location.
    type(logical_setting), public :: compilation_skip_to_next_location = &
                                     logical_setting(name="compilation_skip_to_next_location", value=.true.)

    !> True means that `compilation_find_file' searches all the
    !> directories that were entered before the message, not only its
    !> own.  Port of compile.el compilation-search-all-directories.
    type(logical_setting), public :: compilation_search_all_directories = &
                                     logical_setting(name="compilation_search_all_directories", value=.true.)

    !> Values of `compilation_context_lines' other than a number of lines.
    integer, parameter, public :: COMPILATION_CONTEXT_NIL = -1, COMPILATION_CONTEXT_T = -2

    !> Display this many lines of leading context before the current
    !> message.  If COMPILATION_CONTEXT_NIL or COMPILATION_CONTEXT_T, and
    !> the left fringe is displayed, don't scroll the compilation output
    !> window; an arrow in the left fringe points to the current message.
    !> With no left fringe, if COMPILATION_CONTEXT_NIL, the message scrolls
    !> to the top of the window; there is no arrow.  If
    !> COMPILATION_CONTEXT_T, don't scroll the compilation output window at
    !> all; an arrow before column zero points to the current message.
    !> Port of compile.el compilation-context-lines.
    type(integer_setting), public :: compilation_context_lines = &
                                     integer_setting(name="compilation_context_lines", value=COMPILATION_CONTEXT_NIL)

    !> Face name to use for whole messages.
    !> Faces `compilation_error_face', `compilation_warning_face',
    !> `compilation_info_face', `compilation_line_face' and
    !> `compilation_column_face' get prepended to this, when applicable.
    !> Port of compile.el compilation-message-face.
    type(string_setting), public :: compilation_message_face = string_setting(name="compilation_message_face")

    !> Face name to use for file name in error messages.  Port of
    !> compile.el compilation-error-face.
    type(string_setting), public :: compilation_error_face = string_setting(name="compilation_error_face")

    !> Face name to use for file name in warning messages.  Port of
    !> compile.el compilation-warning-face.
    type(string_setting), public :: compilation_warning_face = string_setting(name="compilation_warning_face")

    !> Face name to use for file name in informational messages.  Port of
    !> compile.el compilation-info-face.
    type(string_setting), public :: compilation_info_face = string_setting(name="compilation_info_face")

    !> Face name to use for line numbers in compiler messages.  Port of
    !> compile.el compilation-line-face.
    type(string_setting), public :: compilation_line_face = string_setting(name="compilation_line_face")

    !> Face name to use for column numbers in compiler messages.  Port of
    !> compile.el compilation-column-face.
    type(string_setting), public :: compilation_column_face = string_setting(name="compilation_column_face")

    !> Face name to use for entering directory messages.  Port of
    !> compile.el compilation-enter-directory-face.
    type(string_setting), public :: compilation_enter_directory_face = &
                                    string_setting(name="compilation_enter_directory_face")

    !> Face name to use for leaving directory messages.  Port of
    !> compile.el compilation-leave-directory-face.
    type(string_setting), public :: compilation_leave_directory_face = &
                                    string_setting(name="compilation_leave_directory_face")

    !> The number of errors found so far (the value of buffers without a
    !> count of their own; compilation buffers have one).  Port of
    !> compile.el compilation-num-errors-found.
    integer(int64), public :: compilation_num_errors_found = 0

    !> The number of warnings found so far.  Port of compile.el
    !> compilation-num-warnings-found.
    integer(int64), public :: compilation_num_warnings_found = 0

    !> The number of informational messages found so far.  Port of
    !> compile.el compilation-num-infos-found.
    integer(int64), public :: compilation_num_infos_found = 0

    !> Face used to highlight compiler errors.  Port of
    !> compile.el:compilation-error.
    type(named_face_type), target, public :: compilation_error

    !> Face used to highlight compiler warnings.  Port of
    !> compile.el:compilation-warning.
    type(named_face_type), target, public :: compilation_warning

    !> Face used to highlight compiler information.  Port of
    !> compile.el:compilation-info.
    type(named_face_type), target, public :: compilation_info

    !> Face for Compilation mode's "error" mode line indicator.  Port of
    !> compile.el:compilation-mode-line-fail.
    type(named_face_type), target, public :: compilation_mode_line_fail

    !> Face for Compilation mode's "running" mode line indicator.  Port of
    !> compile.el:compilation-mode-line-run.
    type(named_face_type), target, public :: compilation_mode_line_run

    !> Face for Compilation mode's "exit" mode line indicator.  Port of
    !> compile.el:compilation-mode-line-exit.
    type(named_face_type), target, public :: compilation_mode_line_exit

    !> Face for displaying line numbers in compiler messages.  Port of
    !> compile.el:compilation-line-number.
    type(named_face_type), target, public :: compilation_line_number

    !> Face for displaying column numbers in compiler messages.  Port of
    !> compile.el:compilation-column-number.
    type(named_face_type), target, public :: compilation_column_number

    ! The stem of messages when no matches are found (compile.el
    ! compilation-error).
    character(len=*), parameter :: COMPILATION_ERROR_STEM = "error"

    ! A location (compile.el's LOC): COLUMN and LINE of FILE (a file
    ! structure), the MARKER pointing there once it is known, and whether
    ! it was VISITED.  FILLED is false while only the column is known (the
    ! loc just made by compilation-assq).
    type :: loc_type
        logical :: has_col = .false.
        integer(int64) :: col = 0
        logical :: filled = .false.
        logical :: has_line = .false.
        integer(int64) :: line = 0
        integer :: file = 0
        type(handle_type) :: marker
        logical :: visited = .false.
    end type loc_type

    ! The locations of a line of a file structure (LINE LOC...), the locs
    ! ordered by column, none first and then decreasing.
    type :: line_node_type
        logical :: has_line = .false.
        integer(int64) :: line = 0
        integer, allocatable :: locs(:)
    end type line_node_type

    ! A file structure ((FILENAME DIRECTORY) FORMATS (LINE LOC...) ...),
    ! the lines ordered by number, none first and then decreasing.
    type :: file_struct_type
        character(len=:), allocatable :: filename, directory
        type(line_node_type), allocatable :: lines(:)
    end type file_struct_type

    ! A key of compilation-locs: FILENAME and DIRECTORY as given (directory
    ! unallocated for none), and its file structure.
    type :: file_key_type
        character(len=:), allocatable :: filename, directory
        integer :: file = 0
    end type file_key_type

    ! A message (compilation--message): LOC, TYPE, END_LOC (0 for none)
    ! and RULE (unallocated for none).  `compilation_message' text
    ! properties hold its index.
    type :: message_type
        integer :: loc = 0
        integer :: type = 2
        integer :: end_loc = 0
        character(len=:), allocatable :: rule
    end type message_type

    ! An element of the stack of directories a `compilation_directory'
    ! text property holds: DIR (unallocated for nil) and the rest of the
    ! stack, PARENT (0 for the empty stack).
    type :: dir_node_type
        character(len=:), allocatable :: dir
        integer :: parent = 0
    end type dir_node_type

    ! What a compilation buffer keeps (its buffer-local variables): forgotten
    ! when its local variables are killed.
    type :: compilation_buffer_type
        type(handle_type) :: buffer
        ! compilation-arguments.
        logical :: has_arguments = .false.
        character(len=:), allocatable :: command
        logical :: comint = .false.
        ! compilation--start-time.
        integer(int64) :: start_count = 0
        ! compilation-directory, if the buffer has a value of its own.
        character(len=:), allocatable :: directory
        ! mode-line-process: 0 none, 1 running, 2 exited (EXIT_TEXT in
        ! EXIT_FACE).
        integer :: mode_line = 0
        character(len=:), allocatable :: exit_text, exit_face
        ! compilation-locs is local (compilation-setup).
        logical :: setup = .false.
        ! Its own compilation-num-*-found.
        logical :: has_counts = .false.
        integer(int64) :: num_errors = 0, num_warnings = 0, num_infos = 0
        ! compilation--parsed: a marker, or none (-1).
        type(handle_type) :: parsed
        ! compilation-current-error: a marker, a position, or none.
        type(handle_type) :: current_error_marker
        logical :: has_current_error = .false.
        integer(int64) :: current_error_pos = 0
        ! compilation-messages-start: a marker, a position, or none.
        type(handle_type) :: messages_start_marker
        logical :: has_messages_start = .false.
        integer(int64) :: messages_start_pos = 0
        ! compilation-auto-jump-to-next.
        logical :: auto_jump_to_next = .false.
        ! The messages, locations, file structures, the keys of
        ! compilation-locs and the directory stacks.
        type(message_type), allocatable :: messages(:)
        integer :: nmessages = 0
        type(loc_type), allocatable :: locs(:)
        integer :: nlocs = 0
        type(file_struct_type), allocatable :: files(:)
        integer :: nfiles = 0
        type(file_key_type), allocatable :: keys(:)
        integer :: nkeys = 0
        type(dir_node_type), allocatable :: dirs(:)
        integer :: ndirs = 0
        ! The stack '(nil).
        integer :: nil_dir = 0
    end type compilation_buffer_type

    type(compilation_buffer_type), allocatable :: compilation_buffers(:)

    ! compile.el compilation-in-progress: the running compilations.
    type(handle_type), allocatable :: compilation_in_progress(:)

    ! A rule spec read during a match: what compilation-error-properties
    ! computes.
    type :: error_spec_type
        character(len=:), allocatable :: filename, directory
        logical :: has_line = .false., has_end_line = .false., has_col = .false., has_end_col = .false.
        integer(int64) :: line = 0, end_line = 0, col = 0, end_col = 0
        integer :: type = 2
        character(len=:), allocatable :: rule
    end type error_spec_type

    character(len=3), parameter :: DAY_NAMES(0:6) = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
    character(len=3), parameter :: MONTH_NAMES(12) = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", &
                                                      "Sep", "Oct", "Nov", "Dec"]
    character(len=*), parameter :: NL = achar(10), TB = achar(9), CR = achar(13)

contains

    ! ------------------------------------------------------------------
    ! The rules
    ! ------------------------------------------------------------------

    !> Add a rule NAME to `compilation_error_regexp_alist_alist'.
    subroutine add_rule(name, regexp, file, line, end_line, column, end_column, type, warning, info, hyperlink)
        character(len=*), intent(in) :: name, regexp
        integer, intent(in), optional :: file, line, end_line, column, end_column, type, warning, info, hyperlink
        type(compilation_rule_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(compilation_error_regexp_alist_alist)) allocate (compilation_error_regexp_alist_alist(0))
        n = size(compilation_error_regexp_alist_alist)
        allocate (grown(n + 1))
        do k = 1, n
            grown(k) = compilation_error_regexp_alist_alist(k)
        end do
        grown(n + 1)%name = name
        grown(n + 1)%regexp = regexp
        if (present(file)) grown(n + 1)%file = file
        if (present(line)) grown(n + 1)%line = line
        if (present(end_line)) grown(n + 1)%end_line = end_line
        if (present(column)) grown(n + 1)%column = column
        if (present(end_column)) grown(n + 1)%end_column = end_column
        if (present(type)) grown(n + 1)%type = type
        if (present(warning)) grown(n + 1)%warning = warning
        if (present(info)) grown(n + 1)%info = info
        if (present(hyperlink)) grown(n + 1)%hyperlink = hyperlink
        allocate (grown(n + 1)%highlights(0))
        call move_alloc(grown, compilation_error_regexp_alist_alist)
    end subroutine add_rule

    !> Add a HIGHLIGHT to RULE: SUBMATCH in FACE (a name) or the face
    !> FACE_FUNCTION gives.
    subroutine add_highlight(rule, submatch, face, face_function)
        type(compilation_rule_type), intent(inout) :: rule
        integer, intent(in) :: submatch
        character(len=*), intent(in), optional :: face
        procedure(compilation_face_function), optional :: face_function
        type(compilation_highlight_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(rule%highlights)) allocate (rule%highlights(0))
        n = size(rule%highlights)
        allocate (grown(n + 1))
        do k = 1, n
            grown(k) = rule%highlights(k)
        end do
        grown(n + 1)%submatch = submatch
        if (present(face)) grown(n + 1)%face = face
        if (present(face_function)) grown(n + 1)%face_function => face_function
        call move_alloc(grown, rule%highlights)
    end subroutine add_highlight

    !> The last rule added.
    integer function last_rule()
        last_rule = size(compilation_error_regexp_alist_alist)
    end function last_rule

    !> Set `compilation_error_regexp_alist_alist' to the built-in rules, in
    !> the default matching order.  Port of the value of compile.el
    !> compilation-error-regexp-alist-alist (the `rx' forms written as
    !> regexps).
    subroutine init_compilation_error_regexp_alist_alist()
        if (allocated(compilation_error_regexp_alist_alist)) deallocate (compilation_error_regexp_alist_alist)
        call add_rule("absoft", "^\(?:[Ee]rror on \|[Ww]arning on\( \)\)?[Ll]ine["//TB//" ]+\([0-9]+\)["//TB// &
                      " ]+of["//TB//' ]+"?\([a-zA-Z]?:?[^":'//NL//']+\)"?:', 3, 2, warning=1)
        call add_rule("ada", "\(warning: .*\)? at \([^ "//NL//"]+\):\([0-9]+\)$", 2, 3, warning=1)
        call add_rule("aix", " in line \([0-9]+\) of file \([^ "//NL//"]+[^. "//NL//"]\)\.? ", 2, 1)
        call add_rule("ant", "^[ "//TB//"]*\(?:\[[^] "//NL//"]+\][ "//TB//"]*\)\{1,2\}\(\(?:[A-Za-z]:\)?[^: "//NL// &
                      "]+\):\([0-9]+\):\(?:\([0-9]+\):\([0-9]+\):\([0-9]+\):\)?\( warning\)?", 1, 2, 4, 3, 5, warning=6)
        call add_rule("bash", "^\([^: "//NL//TB//"]+\): line \([0-9]+\):", 1, 2)
        call add_rule("borland", "^\(?:Error\|Warnin\(g\)\) \(?:[FEW][0-9]+ \)?\([a-zA-Z]?:?[^:( "//TB//NL// &
                      "]+\) \([0-9]+\)\(?:[) "//TB//"]\|:[^0-9"//NL//"]\)", 2, 3, warning=1)
        call add_rule("python_tracebacks_and_caml", "^[ "//TB//']*File \("?\)\([^," '//NL//TB// &
                      "<>]+\)\1, lines? \([0-9]+\)-?\([0-9]+\)?\(?:$\|,\(?: characters? \([0-9]+\)-?\([0-9]+\)?:\)?\([ " &
                      //NL//"]Warning\(?: [0-9]+\)?:\)?\)", 2, 3, 4, 5, 6, warning=7)
        call add_rule("cmake", "^CMake \(?:Error\|\(Warning\)\) at \(.*\):\([1-9][0-9]*\) ([^)]+):$", 2, 3, warning=1)
        call add_rule("cmake_info", "^  \(?: \*\)?\(.*\):\([1-9][0-9]*\) ([^)]+)$", 1, 2, type=0)
        call add_rule("comma", '^"\([^," '//NL//TB//']+\)", line \([0-9]+\)\(?:[(. pos]+\([0-9]+\))?\)?[:.,; (-]\( ' &
                      //"warning:\|[-0-9 ]*(W)\)?", 1, 2, column=3, warning=4)
        call add_rule("msft", "^ *\([0-9]+>\)?\(\(?:[a-zA-Z]:\)?[^ :("//TB//NL//"][^:("//TB//NL// &
                      "]*\)(\([0-9]+\)\(?:,\([0-9]+\)\)?) ?: \(?:see declaration\|\(?:warnin\(g\)\|[a-z ]+\) C[0-9]+:\)", &
                      2, 3, column=4, warning=5)
        call add_rule("edg_1", "^\([^ "//NL//"]+\)(\([0-9]+\)): \(?:error\|warnin\(g\)\|remar\(k\)\)", 1, 2, &
                      warning=3, info=4)
        call add_rule("edg_2", 'at line \([0-9]+\) of "\([^ '//NL//']+\)"$', 2, 1, type=0)
        call add_rule("epc", "^Error [0-9]+ at (\([0-9]+\):\([^)"//NL//"]+\))", 2, 1)
        call add_rule("ftnchek", "\(^Warning .*\)? line[ "//NL//"]\([0-9]+\)[ "//NL//"]\(?:col \([0-9]+\)[ "//NL// &
                      "]\)?file \([^ :;"//NL//"]+\)", 4, 2, column=3, warning=1)
        call add_rule("gradle_kotlin", "^\(?:\(w\)\|\([iv]\)\|e\): file://\(\(?:[A-Za-z]:\)?[^"//NL// &
                      ":]+\):\([[:digit:]]+\):\([[:digit:]]+\) ", 3, 4, column=5, warning=1, info=2)
        call add_rule("gradle_kotlin_legacy", "^\(?:\(w\)\|\([iv]\)\|e\): \(\(?:[A-Za-z]:\)?[^"//NL// &
                      ":]+\): (\([[:digit:]]+\), \([[:digit:]]+\)): ", 3, 4, column=5, warning=1, info=2)
        call add_rule("gradle_android", "^ *ERROR:\(?1:[^:"//NL//"]+\):\(?2:[[:digit:]]+\): ", 1, 2)
        call add_rule("iar", '^"\(.*\)",\([0-9]+\)\s-+\(?:Error\|Warnin\(g\)\)\[[0-9]+\]:', 1, 2, warning=3)
        call add_rule("ibm", "^\([^( "//NL//TB//"]+\)(\([0-9]+\):\([0-9]+\)) : \(?:warnin\(g\)\|informationa\(l\)\)?", &
                      1, 2, column=3, warning=4, info=5)
        call add_rule("irix", "^[-[:alnum:]_/ ]+: \(?:\(?:[sS]evere\|[eE]rror\|[wW]arnin\(g\)\|[iI]nf\(o\)\)[0-9 ]*: " &
                      //'\)?\([^," '//NL//TB//"]+\)\(?:, line\|:\) \([0-9]+\):", 3, 4, warning=1, info=2)
        call add_rule("java", "^\(?:[ "//TB//"]+at \|==[0-9]+== +\(?:at\|b\(y\)\)\).+(\([^()"//NL// &
                      "]+\):\([0-9]+\))$", 2, 3, warning=1)
        call add_rule("javac", "^\(\(?:[A-Za-z]:\)?[^"//NL//":]+\):\([0-9]+\): \(warning: \)?.*"//NL//".*"//NL// &
                      " *\^$", 1, 2, warning=3)
        compilation_error_regexp_alist_alist(last_rule())%column_function => current_column_function
        call add_rule("jikes_file", '^\(?:Found\|Issued\) .* compiling "\(.+\)":$', 1, type=0)
        call add_rule("maven", "^\(?:\[\(?:ERROR\|\(?1:WARNING\)\|\(?2:INFO\)\)] \)?\(?3:[^"//NL//" []\(?:[^"//NL// &
                      " :]\| [^"//NL//"/-]\|:[^"//NL//" []\)*\):\[\(?4:[[:digit:]]+\),\(?5:[[:digit:]]+\)] ", 3, 4, &
                      column=5, warning=1, info=2)
        call add_rule("jikes_line", "^ *\([0-9]+\)\.[ "//TB//"]+.*"//NL//" +\(<-*>"//NL// &
                      "\*\*\* \(?:Error\|Warnin\(g\)\)\)", 0, 1, type=2, hyperlink=0)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 2, face_function=face_of_type_3)
        call add_rule("clang_include", "^In file included from \([^"//NL//":]+\):\([0-9]+\):$", 1, 2, type=0)
        call add_rule("gcc_include", "^\(?:In file included \|                 \|"//TB//"\)from \([0-9]*[^0-9"//NL// &
                      "]\(?:[^"//NL//" :]\| [^-/"//NL//"]\|:[^ "//NL//"]\)*?\):\([0-9]+\)\(?::\([0-9]+\)\)?\(?:\([:,]\|$\)\)?", &
                      1, 2, column=3, info=4)
        call add_rule("ruby_Test::Unit", "^    [[ ]?\([^ (].*\):\([1-9][0-9]*\)\(\]\)?:in ", 1, 2)
        call add_rule("rust_panic", "^\(?:    \)?thread '.+' \(?:([[:digit:]]+) \)?\(?1:panicked\) at \(?2:.+\):" &
                      //"\(?3:[[:digit:]]+\):\(?4:[[:digit:]]+\):$", 2, 3, column=4)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 1, face_function=error_face_value)
        call add_rule("rust", "^\(?:\(?1:error\)\|\(?2:warning\)\|\(?3:note\)\)\(?:\[[0-9A-Z]+]\)?:.*"//NL// &
                      " +--> \(?4:.+\):\(?5:[0-9]+\):\(?6:[0-9]+\)", 4, 5, column=6, warning=2, info=3)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 1, face_function=error_face_value)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 2, face_function=warning_face_value)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 3, face_function=info_face_value)
        call add_rule("lua", "^[^"//TB//NL//"]+?: \([^"//TB//NL//"]+?\):\([0-9]+\): .+"//NL//"stack traceback:"//NL//TB, &
                      1, 2, type=2, hyperlink=1)
        call add_rule("lua_stack", "^"//TB//"\(?:\[C]:\|\([^"//TB//NL//"]+?\):\(?:\([0-9]+\):\)?\) in ", 1, 2, type=0, &
                      hyperlink=1)
        call add_rule("gmake", ": \*\*\* \[\(\(.+?\):\([0-9]+\): .+\)\]", 2, 3, type=0, hyperlink=1)
        call add_rule("gnu", "^\(?:[[:alpha:]][.[:alnum:]-]+: ?\| +|?\)?\(?1:\(?:[^0-9"//NL//TB//" ]\|[0-9]+[^0-9"// &
                      NL//"]\)\(?:[^"//NL//" :]\| [^/"//NL//"-]\|:[^ "//NL//"]\)*?\): ?\(?2:[0-9]+\)\(?:-\(?4:[0-9]+\)" &
                      //"\(?:\.\(?5:[0-9]+\)\)?\|[.:]\(?3:[0-9]+\)\(?:-\(?:\(?4:[0-9]+\)\.\)?\(?5:[0-9]+\)\)?\)?:\(?: *" &
                      //"\(?6:FutureWarning\|RuntimeWarning\|Warning\|warning\|W:\)\| *\(?7:Informational\|" &
                      //"informational\|Information\|information\|Info\|info\|I:\|instantiated from\|required from\|" &
                      //"Note\|note\|\[ skipping .+ ]\)\| *\(?:Error\|error\)\|[0-9]?\(?:[^0-9"//NL//"]\|$\)\|" &
                      //"[0-9][0-9][0-9]\)", 1, 2, 4, 3, 5, warning=6, info=7)
        call add_rule("cucumber", "\(?:^\(?:cucumber\(?: -p [^[:space:]]+\)?\|     \)\|#\) \([^(].*\):" &
                      //"\([1-9][0-9]*\)", 1, 2)
        call add_rule("lcc", "^\(?:E\|\(W\)\), \([^("//NL//"]+\)(\([0-9]+\),["//TB//" ]*\([0-9]+\)", 2, 3, column=4, &
                      warning=1)
        call add_rule("makepp", "^makepp\(?:\(?:: warning\(:\).*?\|\(: Scanning\|: [LR]e?l?oading makefile\|: " &
                      //"Imported\|log:.*?\) \|: .*?\)`\(\(\S +?\)\(?::\([0-9]+\)\)?\)['(]\)", 4, 5, warning=1, &
                      info=2, hyperlink=3)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 0, face_function=makepp_highlight)
        call add_rule("mips_1", " (\([0-9]+\)) in \([^ "//NL//"]+\)", 2, 1)
        call add_rule("mips_2", " in \([^()"//NL//" ]+\)(\([0-9]+\))$", 1, 2)
        call add_rule("omake", "^\*\*\* omake: file \(.*\) changed", 1)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 0, face_function=omake_highlight)
        call add_rule("oracle", "^\(?:Semantic error\|Error\|PCC-[0-9]+:\).* line \([0-9]+\)\(?:\(?:,\| at\)? column " &
                      //"\([0-9]+\)\)?\(?:,\| in\| of\)? file \(.*?\):?$", 3, 1, column=2)
        call add_rule("perl", " at \([^ "//NL//"]+\) line \([0-9]+\)\(?:[,.]\|$\| during global destruction\.$\)", 1, 2)
        call add_rule("php", "\(?:Parse\|Fatal\) error: \(.*\) in \(.*\) on line \([0-9]+\)", 2, 3)
        call add_rule("rxp", "^\(?:Error\|Warnin\(g\)\):.*"//NL//".* line \([0-9]+\) char \([0-9]+\) of " &
                      //"file://\(.+\)", 4, 2, column=3, warning=1)
        call add_rule("shellcheck", "^In \(.+\) line \([0-9]+\):", 1, 2)
        call add_rule("sparc_pascal_file", "^\w\w\w \w\w\w +[0-3]?[0-9] +[0-2][0-9]:[0-5][0-9]:[0-5][0-9] " &
                      //"[12][09][0-9][0-9] +\(.*\):$", 1, type=0)
        call add_rule("sparc_pascal_line", "^\(\(?:E\|\(w\)\) +[0-9]+\) line \([0-9]+\) -  ", 0, 3, warning=2)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 1, face_function=face_of_type_2)
        call add_rule("sparc_pascal_example", "^ +\([0-9]+\) +.*"//NL//"\(\(?:e\|\(w\)\) [0-9]+\)-+", 0, 1, warning=3)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 2, face_function=face_of_type_3)
        call add_rule("sun", ": \(?:ERROR\|WARNIN\(G\)\|REMAR\(K\)\) \(?:[[:alnum:] ]+, \)?File = \(.+\), Line = " &
                      //"\([0-9]+\)\(?:, Column = \([0-9]+\)\)?", 3, 4, column=5, warning=1, info=2)
        call add_rule("sun_ada", "^\([^, "//NL//TB//"]+\), line \([0-9]+\), char \([0-9]+\)[:., (-]", 1, 2, column=3)
        call add_rule("watcom", "^[ "//TB//"]*\(\(?:[a-zA-Z]:\)?[^ :("//TB//NL//"][^:("//TB//NL// &
                      "]*\)(\([0-9]+\)): ?\(?:\(Error! E[0-9]+\)\|\(Warning! W[0-9]+\)\):", 1, 2, warning=4)
        call add_rule("4bsd", "\(?:^\|::  \|\S ( \)\(/[^ "//NL//TB//"()]+\)(\([0-9]+\))\(?:: \(warning:\)?\|$\| ),\)", &
                      1, 2, warning=3)
        call add_rule("gcov_file", "^ *-: *\(0\):Source:\(.+\)$", 2, 1, type=0)
        call add_rule("gcov_header", "^ *-: *\(0\):\(?:Object\|Graph\|Data\|Runs\|Programs\):.+$", 0, 1, type=0)
        call add_rule("gcov_nomark", "^ *-: *\([1-9]\|[0-9]\{2,\}\):.*$", 0, 1, type=0)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 0, face="default")
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 1, face_function=line_face_value)
        call add_rule("gcov_called_line", "^ *\([0-9]+\): *\([0-9]+\):.*$", 0, 2, type=0)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 0, face="default")
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 1, face_function=info_face_value)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 2, face_function=line_face_value)
        call add_rule("gcov_never_called", "^ *\(#####\): *\([0-9]+\):.*$", 0, 2, type=2)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 0, face="default")
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 1, face_function=error_face_value)
        call add_highlight(compilation_error_regexp_alist_alist(last_rule()), 2, face_function=line_face_value)
        call add_rule("perl__Pod::Checker", "^\*\*\* \(?:ERROR\|\(WARNING\)\).* \(?:at\|on\) line \([0-9]+\) " &
                      //"\(?:.* \)?in file \([^ "//TB//NL//"]+\)", 3, 2, warning=1)
        call add_rule("perl__Test", "^# Failed test [0-9]+ in \([^ "//TB//CR//NL//"]+\) at line \([0-9]+\)", 1, 2)
        call add_rule("perl__Test2", "^\(.*NOK.*\)?# Test [0-9]+ got:.* (\([^ "//TB//CR//NL// &
                      "]+\) at line \([0-9]+\)\( fail #[0-9]+\)?)", 2, 3)
        call add_rule("perl__Test::Harness", "^.*NOK.* \([^ "//TB//CR//NL//"]+\) at line \([0-9]+\)", 1, 2)
        call add_rule("weblint", "^\([^ "//TB//CR//NL//"(]+\) (\([0-9]+\):\([0-9]+\)) ", 1, 2, column=3)
        call add_rule("guile_file", "^In \(.+\..+\):"//NL, 1, type=0)
        call add_rule("guile_line", "^ *\([0-9]+\): *\([0-9]+\)", 0, 1, column=2)
        call add_rule("typescript_tsc_plain", "^\([^ "//TB//NL//"()][^"//NL//"()]*\)(\([0-9]+\),\([0-9]+\)): error " &
                      //"[0-9A-Z]+: ", 1, 2, column=3, type=2)
        call add_rule("typescript_tsc_pretty", "^\([^ "//TB//NL//"()][^"//NL//"()]*\):\([0-9]+\):\([0-9]+\) - error " &
                      //"[0-9A-Z]+: ", 1, 2, column=3, type=2)
    end subroutine init_compilation_error_regexp_alist_alist

    !> The default `compilation_error_regexp_alist': every rule but
    !> `omake' (its mere presence triggers special processing and modifies
    !> the regexps of other rules, see `compilation_parse_errors').
    subroutine init_compilation_error_regexp_alist()
        integer :: k, n
        n = 0
        do k = 1, size(compilation_error_regexp_alist_alist)
            if (compilation_error_regexp_alist_alist(k)%name /= "omake") n = n + 1
        end do
        if (allocated(compilation_error_regexp_alist)) deallocate (compilation_error_regexp_alist)
        allocate (compilation_error_regexp_alist(n))
        n = 0
        do k = 1, size(compilation_error_regexp_alist_alist)
            if (compilation_error_regexp_alist_alist(k)%name == "omake") cycle
            n = n + 1
            compilation_error_regexp_alist(n) = compilation_error_regexp_alist_alist(k)%name
        end do
    end subroutine init_compilation_error_regexp_alist

    !> The column of point (the javac rule's COLUMN: #'current-column).
    subroutine current_column_function(number, found)
        integer(int64), intent(out) :: number
        logical, intent(out) :: found
        number = current_column()
        found = .true.
    end subroutine current_column_function

    !> (compilation-face '(2)) for a HIGHLIGHT.
    recursive subroutine face_of_type_2(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status
        status = status_type()
        face = compilation_face(2, 0)
    end subroutine face_of_type_2

    !> (compilation-face '(3)) for a HIGHLIGHT.
    recursive subroutine face_of_type_3(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status
        status = status_type()
        face = compilation_face(3, 0)
    end subroutine face_of_type_3

    !> `compilation_error_face' for a HIGHLIGHT.
    recursive subroutine error_face_value(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status
        status = status_type()
        if (allocated(compilation_error_face%value)) face = compilation_error_face%value
    end subroutine error_face_value

    !> `compilation_warning_face' for a HIGHLIGHT.
    recursive subroutine warning_face_value(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status
        status = status_type()
        if (allocated(compilation_warning_face%value)) face = compilation_warning_face%value
    end subroutine warning_face_value

    !> `compilation_info_face' for a HIGHLIGHT.
    recursive subroutine info_face_value(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status
        status = status_type()
        if (allocated(compilation_info_face%value)) face = compilation_info_face%value
    end subroutine info_face_value

    !> `compilation_line_face' for a HIGHLIGHT.
    recursive subroutine line_face_value(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status
        status = status_type()
        if (allocated(compilation_line_face%value)) face = compilation_line_face%value
    end subroutine line_face_value

    !> The HIGHLIGHT of the makepp rule: parse the other file names of the
    !> line (all but the first), then go to the end of the line; no face.
    recursive subroutine makepp_highlight(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status
        type(compilation_rule_type) :: rules(1)
        integer(int64), allocatable :: saved(:)
        type(status_type) :: st
        integer(int64) :: from
        saved = match_data()
        rules(1)%regexp = "`\(\(\S +?\)\(?::\([0-9]+\)\)?\)['(]"
        rules(1)%file = 2
        rules(1)%line = 3
        if (match_end(1, st) /= 0) then
            rules(1)%type = 1
        else if (match_end(2, st) /= 0) then
            rules(1)%type = 0
        else
            rules(1)%type = 2
        end if
        rules(1)%hyperlink = 1
        allocate (rules(1)%highlights(0))
        from = match_end(0, st)
        call compilation_parse_errors(from, line_end_position(), status, rules)
        call set_match_data(saved)
        call end_of_line()
    end subroutine makepp_highlight

    !> The HIGHLIGHT of the omake rule: forget the locations of the file
    !> about to be recompiled; no face.
    recursive subroutine omake_highlight(face, status)
        character(len=:), allocatable, intent(out) :: face
        type(status_type), intent(out) :: status
        status = status_type()
        call compilation_flush_file_structure(match_string(1))
    end subroutine omake_highlight

    ! ------------------------------------------------------------------
    ! Compilation buffers
    ! ------------------------------------------------------------------

    logical function same(a, b)
        type(handle_type), intent(in) :: a, b
        same = a%index == b%index .and. a%generation == b%generation
    end function same

    !> The entry of buffer `h' in `compilation_buffers' (0 for none).
    integer function entry_of(h) result(k)
        type(handle_type), intent(in) :: h
        if (allocated(compilation_buffers) .and. h%index /= 0) then
            do k = 1, size(compilation_buffers)
                if (same(compilation_buffers(k)%buffer, h)) return
            end do
        end if
        k = 0
    end function entry_of

    !> The entry of the current buffer, made if needed (in the slot of a
    !> dead buffer if there is one).
    integer function current_entry() result(k)
        type(compilation_buffer_type), allocatable :: grown(:)
        integer :: n
        k = entry_of(current_buffer())
        if (k /= 0) return
        if (.not. allocated(compilation_buffers)) allocate (compilation_buffers(0))
        do k = 1, size(compilation_buffers)
            if (.not. buffer_live_p(compilation_buffers(k)%buffer)) then
                call reset_entry(k)
                compilation_buffers(k)%buffer = current_buffer()
                return
            end if
        end do
        n = size(compilation_buffers)
        allocate (grown(n + 1))
        do k = 1, n
            grown(k) = compilation_buffers(k)
        end do
        grown(n + 1)%buffer = current_buffer()
        call move_alloc(grown, compilation_buffers)
        k = n + 1
    end function current_entry

    !> Free what entry K holds and make it unused.
    subroutine reset_entry(k)
        integer, intent(in) :: k
        type(compilation_buffer_type) :: empty
        type(status_type) :: st
        integer :: i
        do i = 1, compilation_buffers(k)%nlocs
            if (compilation_buffers(k)%locs(i)%marker%index /= 0) call free_marker(compilation_buffers(k)%locs(i)%marker, st)
        end do
        if (compilation_buffers(k)%parsed%index /= 0) call free_marker(compilation_buffers(k)%parsed, st)
        if (compilation_buffers(k)%current_error_marker%index /= 0) then
            call free_marker(compilation_buffers(k)%current_error_marker, st)
        end if
        if (compilation_buffers(k)%messages_start_marker%index /= 0) then
            call free_marker(compilation_buffers(k)%messages_start_marker, st)
        end if
        compilation_buffers(k) = empty
    end subroutine reset_entry

    !> Forget what the current buffer had as a compilation buffer, when
    !> `kill_all_local_variables' runs.
    subroutine kill_compilation_locals()
        integer :: k
        k = entry_of(current_buffer())
        if (k /= 0) call reset_entry(k)
    end subroutine kill_compilation_locals

    !> Test if inside a compilation buffer.  Port of
    !> compile.el:compilation-buffer-internal-p.
    logical function compilation_buffer_internal_p()
        integer :: k
        compilation_buffer_internal_p = .false.
        k = entry_of(current_buffer())
        if (k /= 0) compilation_buffer_internal_p = compilation_buffers(k)%setup
    end function compilation_buffer_internal_p

    !> Test if BUFFER is a compilation buffer.  Port of
    !> compile.el:compilation-buffer-p.
    logical function compilation_buffer_p(buffer)
        type(handle_type), intent(in) :: buffer
        integer :: k
        compilation_buffer_p = .false.
        k = entry_of(buffer)
        if (k /= 0) compilation_buffer_p = compilation_buffers(k)%setup
    end function compilation_buffer_p

    !> Return a compilation buffer.
    !> If AVOID_CURRENT is false, and the current buffer is a compilation
    !> buffer, return it.  If AVOID_CURRENT is true, return the current
    !> buffer only as a last resort.  Port of
    !> compile.el:compilation-find-buffer.
    recursive subroutine compilation_find_buffer(buffer, status, avoid_current)
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: avoid_current
        logical :: avoid
        avoid = .false.
        if (present(avoid_current)) avoid = avoid_current
        status = status_type()
        if (compilation_buffer_internal_p() .and. .not. avoid) then
            buffer = current_buffer()
            return
        end if
        call next_error_find_buffer(buffer, status, avoid, compilation_buffer_internal_p)
    end subroutine compilation_find_buffer

    !> The counts of errors, warnings and informational messages found so
    !> far in the current buffer (`compilation_num_errors_found' and
    !> friends).
    subroutine compilation_message_counts(errors, warnings, infos)
        integer(int64), intent(out) :: errors, warnings, infos
        integer :: k
        errors = compilation_num_errors_found
        warnings = compilation_num_warnings_found
        infos = compilation_num_infos_found
        k = entry_of(current_buffer())
        if (k == 0) return
        if (.not. compilation_buffers(k)%has_counts) return
        errors = compilation_buffers(k)%num_errors
        warnings = compilation_buffers(k)%num_warnings
        infos = compilation_buffers(k)%num_infos
    end subroutine compilation_message_counts

    ! ------------------------------------------------------------------
    ! Messages, locations and file structures
    ! ------------------------------------------------------------------

    !> A new message in entry K.
    integer function new_message(k, loc, type, end_loc, rule) result(id)
        integer, intent(in) :: k, loc, type, end_loc
        character(len=*), intent(in), optional :: rule
        type(message_type), allocatable :: grown(:)
        integer :: i, n
        if (.not. allocated(compilation_buffers(k)%messages)) allocate (compilation_buffers(k)%messages(16))
        n = compilation_buffers(k)%nmessages
        if (n == size(compilation_buffers(k)%messages)) then
            allocate (grown(2*n))
            do i = 1, n
                grown(i)%loc = compilation_buffers(k)%messages(i)%loc
                grown(i)%type = compilation_buffers(k)%messages(i)%type
                grown(i)%end_loc = compilation_buffers(k)%messages(i)%end_loc
                if (allocated(compilation_buffers(k)%messages(i)%rule)) then
                    call move_alloc(compilation_buffers(k)%messages(i)%rule, grown(i)%rule)
                end if
            end do
            call move_alloc(grown, compilation_buffers(k)%messages)
        end if
        id = n + 1
        compilation_buffers(k)%nmessages = id
        compilation_buffers(k)%messages(id)%loc = loc
        compilation_buffers(k)%messages(id)%type = type
        compilation_buffers(k)%messages(id)%end_loc = end_loc
        if (allocated(compilation_buffers(k)%messages(id)%rule)) deallocate (compilation_buffers(k)%messages(id)%rule)
        if (present(rule)) compilation_buffers(k)%messages(id)%rule = rule
    end function new_message

    !> A new location of column COL (none unless HAS_COL) in entry K.
    integer function new_loc(k, has_col, col) result(id)
        integer, intent(in) :: k
        logical, intent(in) :: has_col
        integer(int64), intent(in) :: col
        type(loc_type), allocatable :: grown(:)
        integer :: n
        if (.not. allocated(compilation_buffers(k)%locs)) allocate (compilation_buffers(k)%locs(16))
        n = compilation_buffers(k)%nlocs
        if (n == size(compilation_buffers(k)%locs)) then
            allocate (grown(2*n))
            grown(1:n) = compilation_buffers(k)%locs(1:n)
            call move_alloc(grown, compilation_buffers(k)%locs)
        end if
        id = n + 1
        compilation_buffers(k)%nlocs = id
        compilation_buffers(k)%locs(id) = loc_type()
        compilation_buffers(k)%locs(id)%has_col = has_col
        compilation_buffers(k)%locs(id)%col = col
    end function new_loc

    !> A new directory stack node in entry K: DIR (if present) on PARENT.
    integer function new_dir(k, parent, dir) result(id)
        integer, intent(in) :: k, parent
        character(len=*), intent(in), optional :: dir
        type(dir_node_type), allocatable :: grown(:)
        integer :: i, n
        if (.not. allocated(compilation_buffers(k)%dirs)) allocate (compilation_buffers(k)%dirs(16))
        n = compilation_buffers(k)%ndirs
        if (n == size(compilation_buffers(k)%dirs)) then
            allocate (grown(2*n))
            do i = 1, n
                grown(i)%parent = compilation_buffers(k)%dirs(i)%parent
                if (allocated(compilation_buffers(k)%dirs(i)%dir)) then
                    call move_alloc(compilation_buffers(k)%dirs(i)%dir, grown(i)%dir)
                end if
            end do
            call move_alloc(grown, compilation_buffers(k)%dirs)
        end if
        id = n + 1
        compilation_buffers(k)%ndirs = id
        compilation_buffers(k)%dirs(id)%parent = parent
        if (allocated(compilation_buffers(k)%dirs(id)%dir)) deallocate (compilation_buffers(k)%dirs(id)%dir)
        if (present(dir)) compilation_buffers(k)%dirs(id)%dir = dir
    end function new_dir

    !> The message whose index the `compilation_message' property at POS
    !> holds (0 for none).
    integer function message_at(pos) result(id)
        integer(int64), intent(in) :: pos
        id = property_id_at(pos, "compilation_message")
        if (id /= 0) then
            if (id > compilation_buffers(entry_of(current_buffer()))%nmessages) id = 0
        end if
    end function message_at

    !> The directory stack the `compilation_directory' property at POS
    !> holds (0 for none).
    integer function dir_at(pos) result(id)
        integer(int64), intent(in) :: pos
        id = property_id_at(pos, "compilation_directory")
        if (id /= 0) then
            if (id > compilation_buffers(entry_of(current_buffer()))%ndirs) id = 0
        end if
    end function dir_at

    !> The index the property PROP at POS holds, 0 for none (or when the
    !> current buffer has no entry).
    integer function property_id_at(pos, prop) result(id)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: prop
        type(value_type) :: v
        type(status_type) :: st
        id = 0
        if (entry_of(current_buffer()) == 0) return
        if (pos < 1) return
        call get_text_property(pos, sym(prop), v, st)
        if (.not. st%ok()) return
        if (.not. fixnump(v)) return
        if (xfixnum(v) < 1 .or. xfixnum(v) > huge(0)) return
        id = int(xfixnum(v))
    end function property_id_at

    !> The directory stack in effect at POS: the `compilation_directory'
    !> property at the previous change of it (0 for none).  Cf.
    !> compile.el:compilation--previous-directory (without its cache).
    integer function directory_before(pos) result(id)
        integer(int64), intent(in) :: pos
        integer(int64) :: prev
        logical :: found
        type(status_type) :: st
        id = 0
        call previous_single_property_change(pos, sym("compilation_directory"), prev, found, st)
        if (.not. found) return
        id = dir_at(prev - 1)
        if (id == 0) id = dir_at(prev)
    end function directory_before

    !> Retrieve FILE's file-structure or create a new one.  FILE is
    !> FILENAME, relative to DIRECTORY if present; FILENAME may be relative
    !> or absolute.  The file structure's directory is the truename of
    !> DIRECTORY.  Port of compile.el:compilation-get-file-structure.
    recursive integer function compilation_get_file_structure(k, filename, directory) result(fs)
        integer, intent(in) :: k
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: directory
        character(len=:), allocatable :: name, spec
        type(status_type) :: st
        logical :: has_spec
        fs = find_key(k, filename, directory)
        if (fs /= 0) return
        ! File was not previously encountered, at least not in the form
        ! passed.  Let's normalize it and look again.
        name = filename
        has_spec = present(directory)
        if (has_spec) then
            call file_truename(directory, spec, st)
            if (.not. st%ok()) spec = directory
        end if
        ! Some compilers produce bogus file names like "./bar//foo.c" for
        ! file "bar/foo.c"; expand_file_name would collapse these into
        ! "/foo.c" and fail to find the appropriate file.  So we look for
        ! doubled slashes in the file name and fix them.
        name = command_line_normalize_file_name(name)
        ! Retrieve or create file-structure for normalized name.
        if (has_spec) then
            fs = find_key(k, name, spec)
        else
            fs = find_key(k, name)
        end if
        if (fs == 0) then
            fs = new_file(k, name, has_spec, spec)
            if (has_spec) then
                call add_key(k, name, fs, spec)
            else
                call add_key(k, name, fs)
            end if
        end if
        ! Store it for the possibly unnormalized name.
        if (present(directory)) then
            call add_key(k, filename, fs, directory)
        else
            call add_key(k, filename, fs)
        end if
    end function compilation_get_file_structure

    !> The file structure of the key FILENAME . DIRECTORY (0 for none).
    integer function find_key(k, filename, directory) result(fs)
        integer, intent(in) :: k
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: directory
        integer :: i
        fs = 0
        do i = 1, compilation_buffers(k)%nkeys
            associate (key => compilation_buffers(k)%keys(i))
                if (.not. same_string(key%filename, filename)) cycle
                if (present(directory) .neqv. allocated(key%directory)) cycle
                if (present(directory)) then
                    if (.not. same_string(key%directory, directory)) cycle
                end if
                fs = key%file
                return
            end associate
        end do
    end function find_key

    logical function same_string(a, b)
        character(len=*), intent(in) :: a, b
        same_string = len(a) == len(b)
        if (same_string) same_string = a == b
    end function same_string

    subroutine add_key(k, filename, fs, directory)
        integer, intent(in) :: k, fs
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: directory
        type(file_key_type), allocatable :: grown(:)
        integer :: i, n
        if (present(directory)) then
            if (find_key(k, filename, directory) /= 0) return
        else
            if (find_key(k, filename) /= 0) return
        end if
        if (.not. allocated(compilation_buffers(k)%keys)) allocate (compilation_buffers(k)%keys(0))
        n = compilation_buffers(k)%nkeys
        if (n == size(compilation_buffers(k)%keys)) then
            allocate (grown(max(16, 2*n)))
            do i = 1, n
                call move_alloc(compilation_buffers(k)%keys(i)%filename, grown(i)%filename)
                if (allocated(compilation_buffers(k)%keys(i)%directory)) then
                    call move_alloc(compilation_buffers(k)%keys(i)%directory, grown(i)%directory)
                end if
                grown(i)%file = compilation_buffers(k)%keys(i)%file
            end do
            call move_alloc(grown, compilation_buffers(k)%keys)
        end if
        n = n + 1
        compilation_buffers(k)%nkeys = n
        compilation_buffers(k)%keys(n)%filename = filename
        if (allocated(compilation_buffers(k)%keys(n)%directory)) deallocate (compilation_buffers(k)%keys(n)%directory)
        if (present(directory)) compilation_buffers(k)%keys(n)%directory = directory
        compilation_buffers(k)%keys(n)%file = fs
    end subroutine add_key

    integer function new_file(k, filename, has_dir, directory) result(fs)
        integer, intent(in) :: k
        character(len=*), intent(in) :: filename
        logical, intent(in) :: has_dir
        character(len=:), allocatable, intent(in) :: directory
        type(file_struct_type), allocatable :: grown(:)
        integer :: i, n
        if (.not. allocated(compilation_buffers(k)%files)) allocate (compilation_buffers(k)%files(0))
        n = compilation_buffers(k)%nfiles
        if (n == size(compilation_buffers(k)%files)) then
            allocate (grown(max(8, 2*n)))
            do i = 1, n
                call move_alloc(compilation_buffers(k)%files(i)%filename, grown(i)%filename)
                if (allocated(compilation_buffers(k)%files(i)%directory)) then
                    call move_alloc(compilation_buffers(k)%files(i)%directory, grown(i)%directory)
                end if
                call move_alloc(compilation_buffers(k)%files(i)%lines, grown(i)%lines)
            end do
            call move_alloc(grown, compilation_buffers(k)%files)
        end if
        fs = n + 1
        compilation_buffers(k)%nfiles = fs
        compilation_buffers(k)%files(fs)%filename = filename
        if (allocated(compilation_buffers(k)%files(fs)%directory)) deallocate (compilation_buffers(k)%files(fs)%directory)
        if (has_dir) compilation_buffers(k)%files(fs)%directory = directory
        if (allocated(compilation_buffers(k)%files(fs)%lines)) deallocate (compilation_buffers(k)%files(fs)%lines)
        allocate (compilation_buffers(k)%files(fs)%lines(0))
    end function new_file

    !> Forget the file structure of FILE (a name), so that later messages
    !> about it get new locations.  Port of
    !> compile.el:compilation--flush-file-structure.
    recursive subroutine compilation_flush_file_structure(file)
        character(len=*), intent(in) :: file
        integer :: k, fs, i, n
        k = current_entry()
        fs = compilation_get_file_structure(k, file)
        n = 0
        do i = 1, compilation_buffers(k)%nkeys
            if (compilation_buffers(k)%keys(i)%file == fs) cycle
            n = n + 1
            if (n /= i) then
                call move_alloc(compilation_buffers(k)%keys(i)%filename, compilation_buffers(k)%keys(n)%filename)
                if (allocated(compilation_buffers(k)%keys(n)%directory)) then
                    deallocate (compilation_buffers(k)%keys(n)%directory)
                end if
                if (allocated(compilation_buffers(k)%keys(i)%directory)) then
                    call move_alloc(compilation_buffers(k)%keys(i)%directory, compilation_buffers(k)%keys(n)%directory)
                end if
                compilation_buffers(k)%keys(n)%file = compilation_buffers(k)%keys(i)%file
            end if
        end do
        compilation_buffers(k)%nkeys = n
    end subroutine compilation_flush_file_structure

    !> The line node of LINE (none unless HAS_LINE) in file structure FS
    !> of entry K, made if needed in its place: compilation-assq on the
    !> loc tree.
    integer function line_assq(k, fs, has_line, line) result(node)
        integer, intent(in) :: k, fs
        logical, intent(in) :: has_line
        integer(int64), intent(in) :: line
        type(line_node_type), allocatable :: grown(:)
        integer :: n, i
        n = size(compilation_buffers(k)%files(fs)%lines)
        node = 1
        if (.not. has_line) then
            if (n > 0) then
                if (.not. compilation_buffers(k)%files(fs)%lines(1)%has_line) return
            end if
        else
            do while (node <= n)
                associate (x => compilation_buffers(k)%files(fs)%lines(node))
                    if (.not. x%has_line) then
                        node = node + 1
                        cycle
                    end if
                    if (line < x%line) then
                        node = node + 1
                        cycle
                    end if
                    if (x%line == line) return
                    exit
                end associate
            end do
        end if
        ! Insert a new node at NODE.
        allocate (grown(n + 1))
        do i = 1, node - 1
            call move_line_node(compilation_buffers(k)%files(fs)%lines(i), grown(i))
        end do
        grown(node)%has_line = has_line
        grown(node)%line = line
        allocate (grown(node)%locs(0))
        do i = node, n
            call move_line_node(compilation_buffers(k)%files(fs)%lines(i), grown(i + 1))
        end do
        call move_alloc(grown, compilation_buffers(k)%files(fs)%lines)
    end function line_assq

    subroutine move_line_node(from, to)
        type(line_node_type), intent(inout) :: from
        type(line_node_type), intent(inout) :: to
        to%has_line = from%has_line
        to%line = from%line
        call move_alloc(from%locs, to%locs)
    end subroutine move_line_node

    !> The loc of column COL (none unless HAS_COL) in line node NODE of
    !> file structure FS of entry K, made if needed in its place:
    !> compilation-assq on a line.
    integer function col_assq(k, fs, node, has_col, col) result(loc)
        integer, intent(in) :: k, fs, node
        logical, intent(in) :: has_col
        integer(int64), intent(in) :: col
        integer, allocatable :: locs(:)
        integer :: n, j, id
        locs = compilation_buffers(k)%files(fs)%lines(node)%locs
        n = size(locs)
        j = 1
        if (.not. has_col) then
            if (n > 0) then
                if (.not. compilation_buffers(k)%locs(locs(1))%has_col) then
                    loc = locs(1)
                    return
                end if
            end if
        else
            do while (j <= n)
                id = locs(j)
                if (.not. compilation_buffers(k)%locs(id)%has_col) then
                    j = j + 1
                    cycle
                end if
                if (col < compilation_buffers(k)%locs(id)%col) then
                    j = j + 1
                    cycle
                end if
                if (compilation_buffers(k)%locs(id)%col == col) then
                    loc = id
                    return
                end if
                exit
            end do
        end if
        loc = new_loc(k, has_col, col)
        compilation_buffers(k)%files(fs)%lines(node)%locs = [locs(1:j - 1), loc, locs(j:n)]
    end function col_assq

    !> Collapse multiple slashes to one, to handle non-Emacs file names.
    !> Port of startup.el:command-line-normalize-file-name.
    function command_line_normalize_file_name(file_in) result(file)
        character(len=*), intent(in) :: file_in
        character(len=:), allocatable :: file
        integer :: k, j
        file = file_in
        ! /// at the beginning is supposed to mean just /, not //.
        if (len(file) >= 3) then
            if (file(1:3) == "///") then
                j = 1
                do while (j < len(file))
                    if (file(j + 1:j + 1) /= "/") exit
                    j = j + 1
                end do
                file = "/"//file(j + 1:)
            end if
        end if
        ! Don't collapse // at the start of the file name.
        k = 2
        do while (k < len(file))
            if (file(k:k) == "/" .and. file(k + 1:k + 1) == "/") then
                j = k + 1
                do while (j <= len(file))
                    if (file(j:j) /= "/") exit
                    j = j + 1
                end do
                file = file(1:k)//file(j:)
            end if
            k = k + 1
        end do
    end function command_line_normalize_file_name

    ! ------------------------------------------------------------------
    ! Parsing
    ! ------------------------------------------------------------------

    !> The type of a message whose type depends on which of the
    !> subexpressions WARNING and INFO of the last match matched.  Port of
    !> compile.el:compilation-type.
    integer function compilation_type(warning, info) result(type)
        integer, intent(in) :: warning, info
        type(status_type) :: st
        type = 2
        if (warning /= 0) then
            if (match_end(warning, st) /= 0) then
                type = 1
                return
            end if
        end if
        if (info /= 0) then
            if (match_end(info, st) /= 0) type = 0
        end if
    end function compilation_type

    !> The face of a message whose type depends on which of the
    !> subexpressions WARNING and INFO of the last match matched:
    !> `compilation_warning_face', `compilation_info_face' or
    !> `compilation_error_face'.  Port of compile.el:compilation-face.
    function compilation_face(warning, info) result(face)
        integer, intent(in) :: warning, info
        character(len=:), allocatable :: face
        select case (compilation_type(warning, info))
        case (1)
            face = setting_string(compilation_warning_face)
        case (0)
            face = setting_string(compilation_info_face)
        case default
            face = setting_string(compilation_error_face)
        end select
    end function compilation_face

    function setting_string(s) result(v)
        type(string_setting), intent(in) :: s
        character(len=:), allocatable :: v
        v = ""
        if (allocated(s%value)) v = s%value
    end function setting_string

    !> The face for messages of TYPE (2 error, 1 warning, 0 info).
    function type_face(type) result(face)
        integer, intent(in) :: type
        character(len=:), allocatable :: face
        select case (type)
        case (0)
            face = setting_string(compilation_info_face)
        case (1)
            face = setting_string(compilation_warning_face)
        case default
            face = setting_string(compilation_error_face)
        end select
    end function type_face

    !> Match position of subexpression G in the match data MD (0 when it
    !> didn't match): its beginning, or its end if END.
    integer(int64) function md_pos(md, g, end) result(pos)
        integer(int64), intent(in) :: md(:)
        integer, intent(in) :: g
        logical, intent(in) :: end
        pos = 0
        if (g < 0) return
        if (2*g + 2 > size(md)) return
        if (end) then
            pos = md(2*g + 2)
        else
            pos = md(2*g + 1)
        end if
    end function md_pos

    !> The number at the start of S (0 if none), as string-to-number reads
    !> it.
    integer(int64) function string_to_number(s) result(n)
        character(len=*), intent(in) :: s
        integer :: k, sign
        n = 0
        k = 1
        do while (k <= len(s))
            if (s(k:k) /= " " .and. s(k:k) /= TB) exit
            k = k + 1
        end do
        sign = 1
        if (k <= len(s)) then
            if (s(k:k) == "-") then
                sign = -1
                k = k + 1
            else if (s(k:k) == "+") then
                k = k + 1
            end if
        end if
        do while (k <= len(s))
            if (s(k:k) < "0" .or. s(k:k) > "9") exit
            if (n < 900000000000000000_int64) n = 10*n + (iachar(s(k:k)) - iachar("0"))
            k = k + 1
        end do
        n = sign*n
    end function string_to_number

    !> The text of subexpression G of the match data MD.
    function md_string(md, g) result(s)
        integer(int64), intent(in) :: md(:)
        integer, intent(in) :: g
        character(len=:), allocatable :: s
        type(status_type) :: st
        call buffer_substring(md_pos(md, g, .false.), md_pos(md, g, .true.), s, st)
        if (.not. st%ok()) s = ""
    end function md_string

    !> Put PROP VAL on the text of subexpression MATCHNUM, if it matched.
    !> Port of compile.el:compilation--put-prop.
    subroutine compilation_put_prop(md, matchnum, prop, val)
        integer(int64), intent(in) :: md(:)
        integer, intent(in) :: matchnum
        character(len=*), intent(in) :: prop, val
        type(status_type) :: st
        if (matchnum == 0) return
        if (md_pos(md, matchnum, .false.) == 0) return
        call put_text_property(md_pos(md, matchnum, .false.), md_pos(md, matchnum, .true.), sym(prop), sym(val), st)
    end subroutine compilation_put_prop

    !> Like `beginning_of_line'.  Port of
    !> compile.el:compilation-beginning-of-line (`selective_display' is
    !> Deferred).
    subroutine compilation_beginning_of_line(n)
        integer(int64), intent(in) :: n
        call beginning_of_line(n)
    end subroutine compilation_beginning_of_line

    !> Go to column COL on the current line.
    !> If SCREEN is true, columns are screen columns, otherwise, they are
    !> just char-counts.  FIRST is the first column's number.  Port of
    !> compile.el:compilation-move-to-column.
    subroutine compilation_move_to_column(col_in, screen, first)
        integer(int64), intent(in) :: col_in, first
        logical, intent(in) :: screen
        integer(int64) :: col, reached, saved
        col = col_in - first
        if (screen) then
            ! Presumably, the compilation tool doesn't know about our
            ! current `tab_width' setting, so it probably assumed 8-wide
            ! TABs.
            saved = tab_width%value
            tab_width%value = 8
            call move_to_column(max(col, 0_int64), reached)
            tab_width%value = saved
        else
            call goto_char(min(line_beginning_position() + col, line_end_position()))
        end if
    end subroutine compilation_move_to_column

    !> Move forward ARG lines and position at first nonblank character.
    !> Port of simple.el:forward-to-indentation.
    subroutine forward_to_indentation(arg)
        integer(int64), intent(in) :: arg
        integer(int64) :: moved
        type(status_type) :: st
        moved = forward_line(arg)
        moved = skip_chars_forward(" "//TB, st)
    end subroutine forward_to_indentation

    !> Parse errors between START and END.
    !> The errors recognized are the ones specified in RULES (anonymous
    !> rules) which default to `compilation_error_regexp_alist' if RULES
    !> is absent.  Port of compile.el:compilation-parse-errors.
    recursive subroutine compilation_parse_errors(start, end, status, rules)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(compilation_rule_type), intent(in), optional :: rules(:)
        type(compilation_rule_type) :: rule
        logical :: saved_fold, omake_included
        integer :: r, n, j
        status = status_type()
        saved_fold = case_fold_search%value
        case_fold_search = compilation_error_case_fold_search%value
        omake_included = .false.
        if (allocated(compilation_error_regexp_alist)) omake_included = any(compilation_error_regexp_alist == "omake")
        if (present(rules)) then
            n = size(rules)
        else
            n = 0
            if (allocated(compilation_error_regexp_alist)) n = size(compilation_error_regexp_alist)
        end if
        do r = 1, n
            if (present(rules)) then
                rule = rules(r)
                if (allocated(rule%name)) deallocate (rule%name)
            else
                j = rule_index(trim(compilation_error_regexp_alist(r)))
                if (j == 0) cycle
                rule = compilation_error_regexp_alist_alist(j)
            end if
            call parse_rule(rule, start, end, omake_included, status)
            if (.not. status%ok()) exit
        end do
        case_fold_search = saved_fold
    end subroutine compilation_parse_errors

    !> The index of the rule NAME in `compilation_error_regexp_alist_alist'
    !> (0 for none).
    integer function rule_index(name) result(j)
        character(len=*), intent(in) :: name
        if (allocated(compilation_error_regexp_alist_alist)) then
            do j = 1, size(compilation_error_regexp_alist_alist)
                if (allocated(compilation_error_regexp_alist_alist(j)%name)) then
                    if (same_string(compilation_error_regexp_alist_alist(j)%name, name)) return
                end if
            end do
        end if
        j = 0
    end function rule_index

    !> Parse the messages RULE matches between START and END (the body of
    !> the dolist of compile.el:compilation-parse-errors).
    recursive subroutine parse_rule(rule, start, end, omake_included, status)
        type(compilation_rule_type), intent(in) :: rule
        integer(int64), intent(in) :: start, end
        logical, intent(in) :: omake_included
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pat, face, help
        integer(int64), allocatable :: md(:)
        integer(int64) :: found, b, e
        type(status_type) :: st
        type(value_type) :: props
        logical :: has_props, changed, file_given
        integer :: msg, this_type, h, mn
        status = status_type()
        if (.not. allocated(rule%regexp)) return
        pat = rule%regexp
        ! omake reports some error indented, so skip the indentation.
        if (omake_included .and. len(pat) > 0) then
            if (string_match("\`\([^^]\|\^\( \*\|\[\)\)", pat, st) == 0) then
                pat = "^\(?:      \)?"//pat(2:)
            end if
        end if
        file_given = rule%file /= 0 .or. associated(rule%file_function)
        call goto_char(start)
        do
            call re_search_forward(pat, found, status, bound=end, noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            if (found == 0) exit
            md = match_data()
            call compilation_error_properties(rule, md, has_props, msg, help, status)
            if (.not. status%ok()) return
            call set_match_data(md)
            if (.not. has_props) cycle
            if (file_given) then
                if (rule%warning /= 0 .or. rule%info /= 0) then
                    this_type = compilation_type(rule%warning, rule%info)
                else
                    this_type = rule%type
                end if
                call compilation_note_type(this_type)
                call compilation_put_prop(md, rule%file, "font_lock_face", type_face(this_type))
            end if
            call compilation_put_prop(md, rule%line, "font_lock_face", setting_string(compilation_line_face))
            call compilation_put_prop(md, rule%end_line, "font_lock_face", setting_string(compilation_line_face))
            call compilation_put_prop(md, rule%column, "font_lock_face", setting_string(compilation_column_face))
            call compilation_put_prop(md, rule%end_column, "font_lock_face", setting_string(compilation_column_face))
            ! Obey HIGHLIGHT.
            if (allocated(rule%highlights)) then
                do h = 1, size(rule%highlights)
                    mn = rule%highlights(h)%submatch
                    if (md_pos(md, mn, .false.) == 0) cycle
                    if (allocated(face)) deallocate (face)
                    if (associated(rule%highlights(h)%face_function)) then
                        call rule%highlights(h)%face_function(face, status)
                        if (.not. status%ok()) return
                    else if (allocated(rule%highlights(h)%face)) then
                        face = rule%highlights(h)%face
                    end if
                    if (.not. allocated(face)) cycle
                    if (len(face) == 0) cycle
                    call put_text_property(md_pos(md, mn, .false.), md_pos(md, mn, .true.), sym("font_lock_face"), &
                                           sym(face), st)
                end do
            end if
            mn = rule%hyperlink
            b = md_pos(md, mn, .false.)
            e = md_pos(md, mn, .true.)
            if (b == 0) cycle
            props = list(sym("compilation_message"), make_fixnum(int(msg, int64)), sym("help_echo"), make_string(help), &
                         sym("mouse_face"), sym("highlight"))
            call add_text_properties(b, e, props, changed, st)
            call font_lock_append_text_property(b, e, sym("font_lock_face"), &
                                                sym(setting_string(compilation_message_face)), st)
        end do
    end subroutine parse_rule

    !> Note that a new message with severity TYPE was seen.
    !> This updates the appropriate variable used by the mode-line.  Port
    !> of compile.el:compilation--note-type.
    subroutine compilation_note_type(type)
        integer, intent(in) :: type
        integer :: k
        k = entry_of(current_buffer())
        if (k /= 0) then
            if (compilation_buffers(k)%has_counts) then
                select case (type)
                case (0)
                    compilation_buffers(k)%num_infos = compilation_buffers(k)%num_infos + 1
                case (1)
                    compilation_buffers(k)%num_warnings = compilation_buffers(k)%num_warnings + 1
                case (2)
                    compilation_buffers(k)%num_errors = compilation_buffers(k)%num_errors + 1
                case default
                    ! (Other types are not counted.)
                end select
                call update_mode_line_process(k)
                return
            end if
        end if
        select case (type)
        case (0)
            compilation_num_infos_found = compilation_num_infos_found + 1
        case (1)
            compilation_num_warnings_found = compilation_num_warnings_found + 1
        case (2)
            compilation_num_errors_found = compilation_num_errors_found + 1
        case default
            ! (Other types are not counted.)
        end select
    end subroutine compilation_note_type

    !> The central driver, called when parsing to gather all information
    !> needed to later jump to corresponding source code: the message of
    !> the match of RULE (whose match data is MD) in MSG and the help echo
    !> HELP, HAS_PROPS false for none.  Port of
    !> compile.el:compilation-error-properties
    !> (`compilation_auto_jump_to_first_error' is Deferred).
    recursive subroutine compilation_error_properties(rule, md, has_props, msg, help, status)
        type(compilation_rule_type), intent(in) :: rule
        integer(int64), intent(in) :: md(:)
        logical, intent(out) :: has_props
        integer, intent(out) :: msg
        character(len=:), allocatable, intent(out) :: help
        type(status_type), intent(out) :: status
        type(error_spec_type) :: spec
        character(len=:), allocatable :: filename, directory, file_name, replaced
        integer(int64), allocatable :: saved(:)
        integer(int64) :: pos, ec
        type(status_type) :: st
        logical :: found, has_file, has_dir, has_ec
        integer :: k, d, prev, loc, fs, j
        status = status_type()
        has_props = .false.
        msg = 0
        call text_property_not_all(md_pos(md, 0, .false.), point(), sym("compilation_message"), nil(), pos, found, st)
        if (found) return
        k = current_entry()
        has_file = .false.
        has_dir = .false.
        if (associated(rule%file_function)) then
            call rule%file_function(filename, directory, has_file)
            has_dir = allocated(directory)
            if (.not. has_file) has_dir = .false.
        else if (rule%file /= 0) then
            if (md_pos(md, rule%file, .false.) /= 0) then
                filename = md_string(md, rule%file)
                has_file = .true.
                if (.not. file_name_absolute_p(filename)) then
                    d = directory_before(md_pos(md, 0, .false.))
                    if (d /= 0) then
                        if (allocated(compilation_buffers(k)%dirs(d)%dir)) then
                            directory = compilation_buffers(k)%dirs(d)%dir
                            has_dir = .true.
                        end if
                    end if
                end if
            end if
        else
            ! This message didn't mention one, get it from previous.
            call previous_single_property_change(point(), sym("compilation_message"), pos, found, st)
            if (found) then
                prev = message_at(pos - 1)
                if (prev == 0) prev = message_at(pos)
                if (prev /= 0) then
                    loc = compilation_buffers(k)%messages(prev)%loc
                    if (loc /= 0) then
                        fs = compilation_buffers(k)%locs(loc)%file
                        if (fs /= 0) then
                            filename = compilation_buffers(k)%files(fs)%filename
                            has_file = .true.
                            if (allocated(compilation_buffers(k)%files(fs)%directory)) then
                                directory = compilation_buffers(k)%files(fs)%directory
                                has_dir = .true.
                            end if
                        end if
                    end if
                end if
            end if
            if (.not. has_file) then
                filename = "*unknown*"
                has_file = .true.
            end if
        end if
        ! All of these fields are optional, get them only if we have an
        ! index, and it matched some part of the message.
        call get_number(rule%line_function, rule%line, spec%has_line, spec%line)
        call get_number(rule%end_line_function, rule%end_line, spec%has_end_line, spec%end_line)
        call get_number(rule%column_function, rule%column, spec%has_col, spec%col)
        call get_number(rule%end_column_function, rule%end_column, has_ec, ec)
        if (has_ec) then
            ! Add one to get an exclusive upper bound.
            spec%end_col = ec + 1
            spec%has_end_col = .true.
        else if (spec%has_end_line) then
            spec%end_col = -1
            spec%has_end_col = .true.
        end if
        if (rule%warning /= 0 .or. rule%info /= 0) then
            spec%type = compilation_type(rule%warning, rule%info)
        else
            spec%type = rule%type
        end if
        ! Remove matches like /bin/sh and do other file name transforms.
        if (has_file .and. allocated(compilation_transform_file_match_alist)) then
            saved = match_data()
            if (has_dir) then
                call expand_file_name(filename, file_name, st, directory)
            else
                file_name = filename
            end if
            do j = 1, size(compilation_transform_file_match_alist)
                if (.not. allocated(compilation_transform_file_match_alist(j)%regexp)) cycle
                if (string_match(compilation_transform_file_match_alist(j)%regexp, file_name, st) == 0) cycle
                if (allocated(compilation_transform_file_match_alist(j)%replacement)) then
                    call replace_match(compilation_transform_file_match_alist(j)%replacement, st, &
                                       fixedcase=.false., literal=.false., string=file_name, result=replaced)
                    filename = replaced
                    has_dir = .false.
                else
                    has_file = .false.
                end if
                exit
            end do
            call set_match_data(saved)
        end if
        ! If we ignored all the files with errors on this line, then return
        ! nothing.
        if (.not. has_file) return
        spec%filename = filename
        if (has_dir) spec%directory = directory
        if (allocated(rule%name)) spec%rule = rule%name
        call compilation_internal_error_properties(spec, msg, help, status)
        has_props = status%ok()
    contains

        subroutine get_number(fn, group, has, value)
            procedure(compilation_number_function), pointer, intent(in) :: fn
            integer, intent(in) :: group
            logical, intent(out) :: has
            integer(int64), intent(out) :: value
            has = .false.
            value = 0
            if (associated(fn)) then
                call fn(value, has)
            else if (group /= 0) then
                if (md_pos(md, group, .false.) /= 0) then
                    value = string_to_number(md_string(md, group))
                    has = .true.
                end if
            end if
        end subroutine get_number
    end subroutine compilation_error_properties

    !> Get the meta-info that will be added as text-properties: the message
    !> MSG and the help echo HELP for the location SPEC.  Port of
    !> compile.el:compilation-internal-error-properties.
    recursive subroutine compilation_internal_error_properties(spec_in, msg, help, status)
        type(error_spec_type), intent(in) :: spec_in
        integer, intent(out) :: msg
        character(len=:), allocatable, intent(out) :: help
        type(status_type), intent(out) :: status
        type(error_spec_type) :: spec
        type(handle_type) :: marker, end_marker, mbuf, comp, new_marker
        type(status_type) :: st
        integer(int64) :: pos, mline, first, opoint, begv, zv, target
        logical :: screen, valid
        integer :: k, fs, marker_node, x, n, loc, end_loc, node, enode
        status = status_type()
        spec = spec_in
        comp = current_buffer()
        k = current_entry()
        if (allocated(spec%directory)) then
            fs = compilation_get_file_structure(k, spec%filename, spec%directory)
        else
            fs = compilation_get_file_structure(k, spec%filename)
        end if
        ! Get first already existing marker (if any has one, all have one).
        marker = null_handle()
        marker_node = 0
        n = size(compilation_buffers(k)%files(fs)%lines)
        if (n > 0) then
            marker_node = 1
            if (size(compilation_buffers(k)%files(fs)%lines(1)%locs) > 0) then
                marker = compilation_buffers(k)%locs(compilation_buffers(k)%files(fs)%lines(1)%locs(1))%marker
            end if
        end if
        valid = .false.
        if (marker%index /= 0) then
            call marker_buffer(marker, mbuf, st)
            valid = st%ok() .and. mbuf%index /= 0
        end if
        new_marker = null_handle()
        end_marker = null_handle()
        if (valid) then
            ! Normalize no linenumber to line 1.
            if (.not. spec%has_line) then
                spec%line = 1
                spec%has_line = .true.
            end if
            ! Find nearest loc, at least one exists.
            do x = 2, n
                associate (xl => compilation_buffers(k)%files(fs)%lines(x)%line)
                    if (xl > spec%line) then
                        marker_node = x
                    else
                        mline = 1
                        if (compilation_buffers(k)%files(fs)%lines(marker_node)%has_line) then
                            mline = compilation_buffers(k)%files(fs)%lines(marker_node)%line
                        end if
                        if (mline - spec%line > spec%line - xl) marker_node = x
                        exit
                    end if
                end associate
            end do
            marker = compilation_buffers(k)%locs(compilation_buffers(k)%files(fs)%lines(marker_node)%locs(1))%marker
            mline = 1
            if (compilation_buffers(k)%files(fs)%lines(marker_node)%has_line) then
                mline = compilation_buffers(k)%files(fs)%lines(marker_node)%line
            end if
            screen = compilation_error_screen_columns%value
            first = compilation_first_column%value
            call marker_buffer(marker, mbuf, st)
            call set_buffer(mbuf, st)
            ! Obey the compilation_error_screen_columns of the target
            ! buffer if its major mode set it buffer-locally.
            if (compilation_error_screen_columns%local_p()) screen = compilation_error_screen_columns%value
            if (compilation_first_column%local_p()) first = compilation_first_column%value
            opoint = point()
            begv = point_min()
            zv = point_max()
            call widen()
            call marker_position(marker, pos, st)
            call goto_char(pos)
            ! Set end-marker if appropriate and go to line.
            if (.not. (spec%has_end_col .or. spec%has_end_line)) then
                call compilation_beginning_of_line(spec%line - mline + 1)
            else
                target = spec%line
                if (spec%has_end_line) target = spec%end_line
                call compilation_beginning_of_line(target - mline + 1)
                if (.not. spec%has_end_col) then
                    call end_of_line()
                else if (spec%end_col < 0) then
                    call end_of_line()
                else
                    call compilation_move_to_column(spec%end_col, screen, first)
                end if
                call point_marker(end_marker)
                if (spec%has_end_line) call compilation_beginning_of_line(spec%line - spec%end_line + 1)
            end if
            if (spec%has_col) then
                call compilation_move_to_column(spec%col, screen, first)
            else
                call forward_to_indentation(0_int64)
            end if
            call point_marker(new_marker)
            call narrow_to_region(begv, zv, st)
            call goto_char(opoint)
            call set_buffer(comp, st)
            k = entry_of(comp)
        end if
        marker = new_marker
        node = line_assq(k, fs, spec%has_line, spec%line)
        end_loc = 0
        if (spec%has_end_line) then
            enode = line_assq(k, fs, .true., spec%end_line)
            end_loc = col_assq(k, fs, enode, spec%has_end_col, spec%end_col)
            node = line_assq(k, fs, spec%has_line, spec%line)
        else if (spec%has_end_col) then
            end_loc = col_assq(k, fs, node, .true., spec%end_col)
        end if
        loc = col_assq(k, fs, node, spec%has_col, spec%col)
        ! If they are new, make the loc(s) reference the file they point
        ! to.
        if (.not. compilation_buffers(k)%locs(loc)%filled) then
            call fill_loc(compilation_buffers(k)%locs(loc), spec%has_line, spec%line, fs, marker)
            marker = null_handle()
        end if
        if (end_loc /= 0) then
            if (.not. compilation_buffers(k)%locs(end_loc)%filled) then
                if (spec%has_end_line) then
                    call fill_loc(compilation_buffers(k)%locs(end_loc), .true., spec%end_line, fs, end_marker)
                else
                    call fill_loc(compilation_buffers(k)%locs(end_loc), spec%has_line, spec%line, fs, end_marker)
                end if
                end_marker = null_handle()
            end if
        end if
        ! The markers not kept are garbage.
        if (marker%index /= 0) call free_marker(marker, st)
        if (end_marker%index /= 0) call free_marker(end_marker, st)
        if (allocated(spec%rule)) then
            msg = new_message(k, loc, spec%type, end_loc, spec%rule)
        else
            msg = new_message(k, loc, spec%type, end_loc)
        end if
        if (spec%has_col) then
            help = "mouse-2: visit this file, line and column"
        else if (spec%has_line) then
            help = "mouse-2: visit this file and line"
        else
            help = "mouse-2: visit this file"
        end if
    end subroutine compilation_internal_error_properties

    subroutine fill_loc(loc, has_line, line, fs, marker)
        type(loc_type), intent(inout) :: loc
        logical, intent(in) :: has_line
        integer(int64), intent(in) :: line
        integer, intent(in) :: fs
        type(handle_type), intent(in) :: marker
        loc%filled = .true.
        loc%has_line = has_line
        loc%line = line
        loc%file = fs
        loc%marker = marker
    end subroutine fill_loc

    !> Remove the properties parsing put between START and END.  Port of
    !> compile.el:compilation--remove-properties.
    subroutine compilation_remove_properties(start_in, end_in)
        integer(int64), intent(in), optional :: start_in, end_in
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        integer(int64) :: start, end, next
        logical :: found, changed
        type(value_type) :: v, props
        start = point_min()
        if (present(start_in)) start = start_in
        end = point_max()
        if (present(end_in)) end = end_in
        call silent_modifications_begin(silent)
        props = list(sym("compilation_debug"), sym("compilation_directory"), sym("compilation_message"), &
                     sym("font_lock_face"), sym("help_echo"), sym("mouse_face"))
        do
            call next_single_property_change(start, sym("compilation_message"), next, found, st, end)
            if (.not. found) next = end
            call get_text_property(start, sym("compilation_message"), v, st)
            if (st%ok() .and. .not. nilp(v)) call remove_list_of_text_properties(start, next, props, changed, st)
            if (next >= end) exit
            start = next
        end do
        call silent_modifications_end(silent)
    end subroutine compilation_remove_properties

    !> change-major-mode-hook: remove the properties of the whole buffer.
    subroutine remove_properties_hook(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call compilation_remove_properties()
    end subroutine remove_properties_hook

    !> Parse the text between START and END: the directory messages first,
    !> then the error messages.  Port of compile.el:compilation--parse-region.
    recursive subroutine compilation_parse_region(start, end_in, status)
        integer(int64), intent(in) :: start, end_in
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: md(:)
        integer(int64) :: end, found
        type(handle_type) :: proc
        character(len=:), allocatable :: state
        type(status_type) :: st
        type(value_type) :: props, more
        logical :: changed
        integer :: i
        status = status_type()
        end = end_in
        call goto_char(end)
        if (.not. bolp()) then
            ! We generally don't like to parse partial lines.
            proc = get_buffer_process(current_buffer())
            if (proc%index /= 0) then
                state = process_status(proc)
                if (state == "run" .or. state == "open") end = line_beginning_position()
            end if
        end if
        call compilation_remove_properties(start, end)
        call goto_char(start)
        if (allocated(compilation_directory_matcher%regexp)) then
            do
                call re_search_forward(compilation_directory_matcher%regexp, found, status, bound=end, &
                                       noerror=SEARCH_NOERROR)
                if (.not. status%ok()) return
                if (found == 0) exit
                md = match_data()
                if (.not. allocated(compilation_directory_matcher%dir)) cycle
                do i = 1, size(compilation_directory_matcher%dir)
                    if (md_pos(md, compilation_directory_matcher%dir(i), .false.) == 0) then
                        call set_error(status, FM_GENERIC_ERROR, "Wrong type argument: integer_or_marker_p, .false.")
                        return
                    end if
                    call compilation_directory_properties(md, compilation_directory_matcher%dir(i), &
                                                          compilation_directory_matcher%leave(i), props, more)
                    call add_text_properties(md_pos(md, compilation_directory_matcher%dir(i), .false.), &
                                             md_pos(md, compilation_directory_matcher%dir(i), .true.), props, changed, st)
                    call add_text_properties(md_pos(md, compilation_directory_matcher%dir(i), .false.), &
                                             md_pos(md, compilation_directory_matcher%dir(i), .true.), more, changed, st)
                end do
            end do
        end if
        call compilation_parse_errors(start, end, status)
    end subroutine compilation_parse_region

    !> The text properties of a directory change message.  The
    !> `compilation_directory' property is important, because it is the
    !> stack of nested enter-messages.  Relative filenames on the following
    !> lines are relative to the top of the stack.  Port of
    !> compile.el:compilation-directory-properties.
    subroutine compilation_directory_properties(md, idx, leave, props, more)
        integer(int64), intent(in) :: md(:)
        integer, intent(in) :: idx, leave
        type(value_type), intent(out) :: props, more
        integer :: k, dir, value, msg
        logical :: leaving
        character(len=:), allocatable :: face
        k = current_entry()
        leaving = .false.
        if (leave /= 0) leaving = md_pos(md, leave, .true.) /= 0
        ! Find previous stack, and push onto it, or if `leave' pop it.
        dir = directory_before(md_pos(md, 0, .false.))
        if (leaving) then
            face = setting_string(compilation_leave_directory_face)
            value = 0
            if (dir /= 0) value = compilation_buffers(k)%dirs(dir)%parent
            if (value == 0) then
                ! '(nil): nil only isn't a property change.
                if (compilation_buffers(k)%nil_dir == 0) compilation_buffers(k)%nil_dir = new_dir(k, 0)
                value = compilation_buffers(k)%nil_dir
            end if
        else
            face = setting_string(compilation_enter_directory_face)
            value = new_dir(k, dir, md_string(md, idx))
        end if
        ! Place a `compilation_message' everywhere we change
        ! text-properties so compilation--remove-properties can know what
        ! to remove.
        msg = new_message(k, 0, 0, 0)
        props = list(sym("font_lock_face"), sym(face), sym("compilation_directory"), make_fixnum(int(value, int64)), &
                     sym("compilation_message"), make_fixnum(int(msg, int64)), sym("mouse_face"), sym("highlight"))
        more = list(sym("help_echo"), make_string("mouse-2: visit destination directory"))
    end subroutine compilation_directory_properties

    !> Make sure the text has been parsed up to LIMIT.  Port of
    !> compile.el:compilation--ensure-parse.
    recursive subroutine compilation_ensure_parse(limit_in)
        integer(int64), intent(in) :: limit_in
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        type(value_type) :: v
        integer(int64) :: opoint, limit, parsed, start, moved
        integer :: k
        k = current_entry()
        opoint = point()
        call goto_char(limit_in)
        limit = line_beginning_position(2_int64)
        if (compilation_buffers(k)%parsed%index == 0) then
            ! We use a marker for compilation--parsed so that users (such as
            ! grep.el) don't need to flush-parse when they modify the buffer
            ! in a way that impacts buffer positions but does not require
            ! re-parsing.
            call copy_marker_at(annotation_end(), compilation_buffers(k)%parsed, .false., st)
        end if
        call marker_position(compilation_buffers(k)%parsed, parsed, st)
        if (parsed < limit) then
            start = max(parsed, point_min())
            call set_marker(compilation_buffers(k)%parsed, limit, st)
            call goto_char(start)
            ! Not line-beginning-position: ignore (comint) fields.
            moved = forward_line(0_int64)
            do while (.not. bobp())
                call get_text_property(point() - 1, sym("compilation_multiline"), v, st)
                if (nilp(v)) exit
                moved = forward_line(-1_int64)
            end do
            call marker_position(compilation_buffers(k)%parsed, parsed, st)
            call silent_modifications_begin(silent)
            call compilation_parse_region(point(), parsed, st)
            call silent_modifications_end(silent)
        end if
        call goto_char(opoint)
    end subroutine compilation_ensure_parse

    !> Where (text-property-search-forward 'compilation-annotation) from
    !> the start of the buffer leaves point: the end of the first region
    !> with a `compilation_annotation' property, or point-min.
    integer(int64) function annotation_end() result(pos)
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: next
        logical :: found
        pos = point_min()
        if (pos >= point_max()) return
        do
            call get_text_property(pos, sym("compilation_annotation"), v, st)
            if (.not. nilp(v)) then
                call next_single_property_change(pos, sym("compilation_annotation"), next, found, st)
                if (.not. found) next = point_max()
                pos = next
                return
            end if
            call next_single_property_change(pos, sym("compilation_annotation"), next, found, st)
            if (.not. found) then
                pos = point_min()
                return
            end if
            pos = next
        end do
    end function annotation_end

    !> Mark the region between START and END for re-parsing.  Port of
    !> compile.el:compilation--flush-parse.
    subroutine compilation_flush_parse(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: parsed
        integer :: k
        status = status_type()
        if (end < 0) return
        k = entry_of(current_buffer())
        if (k == 0) return
        if (compilation_buffers(k)%parsed%index == 0) return
        call marker_position(compilation_buffers(k)%parsed, parsed, status)
        if (.not. status%ok()) return
        call set_marker(compilation_buffers(k)%parsed, min(start, parsed), status)
    end subroutine compilation_flush_parse

    !> The font-lock MATCHER (compilation--ensure-parse): parse up to
    !> LIMIT; it never matches.
    subroutine ensure_parse_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        status = status_type()
        found = .false.
        if (.not. compilation_buffer_internal_p()) return
        call compilation_ensure_parse(limit)
    end subroutine ensure_parse_matcher

    !> Forget the locations found so far: messages later in the buffer get
    !> new locations, and the next error is searched after what the buffer
    !> has now.  Port of compile.el:compilation-forget-errors.
    subroutine compilation_forget_errors()
        type(handle_type) :: proc
        type(status_type) :: st
        integer(int64) :: pos
        integer :: k
        k = current_entry()
        ! In case we hit the same file/line specs, we want to recompute a
        ! new marker for them, so flush our cache.
        compilation_buffers(k)%nkeys = 0
        if (compilation_buffers(k)%current_error_marker%index /= 0) then
            call free_marker(compilation_buffers(k)%current_error_marker, st)
        end if
        compilation_buffers(k)%current_error_marker = null_handle()
        compilation_buffers(k)%has_current_error = .false.
        proc = get_buffer_process(current_buffer())
        if (proc%index /= 0) then
            call marker_position(process_mark(proc), pos, st)
        else
            pos = point_max()
        end if
        if (compilation_buffers(k)%messages_start_marker%index /= 0) then
            call free_marker(compilation_buffers(k)%messages_start_marker, st)
        end if
        compilation_buffers(k)%messages_start_marker = null_handle()
        compilation_buffers(k)%has_messages_start = .true.
        ! In the future, ignore the text already present in the buffer.
        if (pos > point_min()) then
            call copy_marker_at(pos - 1, compilation_buffers(k)%messages_start_marker, .false., st)
        else
            compilation_buffers(k)%messages_start_pos = pos
        end if
        compilation_buffers(k)%auto_jump_to_next = compilation_scroll_output%value == COMPILATION_SCROLL_FIRST_ERROR
    end subroutine compilation_forget_errors

    !> What the `compilation_message' property at POS of the current
    !> buffer says (FOUND false for none).
    subroutine compilation_message_info(pos, info, found)
        integer(int64), intent(in) :: pos
        type(compilation_message_info_type), intent(out) :: info
        logical, intent(out) :: found
        integer :: k, msg, loc, fs
        found = .false.
        msg = message_at(pos)
        if (msg == 0) return
        found = .true.
        k = entry_of(current_buffer())
        info%type = compilation_buffers(k)%messages(msg)%type
        if (allocated(compilation_buffers(k)%messages(msg)%rule)) info%rule = compilation_buffers(k)%messages(msg)%rule
        loc = compilation_buffers(k)%messages(msg)%loc
        if (loc /= 0) then
            info%has_loc = .true.
            info%has_line = compilation_buffers(k)%locs(loc)%has_line
            info%line = compilation_buffers(k)%locs(loc)%line
            info%has_column = compilation_buffers(k)%locs(loc)%has_col
            info%column = compilation_buffers(k)%locs(loc)%col
            fs = compilation_buffers(k)%locs(loc)%file
            if (fs /= 0) then
                info%file = compilation_buffers(k)%files(fs)%filename
                if (allocated(compilation_buffers(k)%files(fs)%directory)) then
                    info%directory = compilation_buffers(k)%files(fs)%directory
                end if
            end if
        end if
        loc = compilation_buffers(k)%messages(msg)%end_loc
        if (loc /= 0) then
            info%has_end = .true.
            info%has_end_line = compilation_buffers(k)%locs(loc)%has_line
            info%end_line = compilation_buffers(k)%locs(loc)%line
            info%has_end_column = compilation_buffers(k)%locs(loc)%has_col
            info%end_column = compilation_buffers(k)%locs(loc)%col
        end if
    end subroutine compilation_message_info

    ! ------------------------------------------------------------------
    ! Moving between messages
    ! ------------------------------------------------------------------

    !> Like next_single_property_change, but parse the text first.  Port of
    !> compile.el:compilation-next-single-property-change.
    recursive subroutine compilation_next_single_property_change(position_in, prop, res, found, limit)
        integer(int64), intent(in) :: position_in
        character(len=*), intent(in) :: prop
        integer(int64), intent(out) :: res
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: limit
        integer(int64) :: position, parsed, lim
        type(status_type) :: st
        integer :: k
        logical :: again
        position = position_in
        do
            k = current_entry()
            parsed = -1
            if (compilation_buffers(k)%parsed%index /= 0) call marker_position(compilation_buffers(k)%parsed, parsed, st)
            lim = point_max()
            if (present(limit)) lim = limit
            parsed = max(parsed, lim)
            call compilation_ensure_parse(parsed)
            if (present(limit)) then
                call next_single_property_change(position, sym(prop), res, found, st, limit)
            else
                call next_single_property_change(position, sym(prop), res, found, st)
            end if
            again = .not. found
            if (found .and. present(limit)) again = res == limit
            if (.not. (again .and. position < lim)) exit
            position = parsed
        end do
    end subroutine compilation_next_single_property_change

    !> Move point to the next error in the compilation buffer.
    !> This function does NOT find the source line like \[next_error],
    !> but you can use \[compilation_display_error] to find and display
    !> the corresponding source code.  Prefix arg N says how many error
    !> messages to move forwards (or backwards, if negative).  Where the
    !> current error ends and the next one begins is determined by the
    !> rules from `compilation_error_regexp_alist' that matched the
    !> compilation messages; this function moves point to where the
    !> `compilation_message' text property changes its value.  In general,
    !> all the messages that have the same line and column numbers are
    !> considered parts of a single compilation message.
    !>
    !> Optional arg DIFFERENT_FILE, if true, means find next error for a
    !> file that is different from the current one.  Optional arg PT, if
    !> present, specifies the value of point to start looking for the next
    !> message.  In interactive invocations, DIFFERENT_FILE and PT are
    !> always absent.  Port of compile.el:compilation-next-error.
    recursive subroutine compilation_next_error(n, status, different_file, pt)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: different_file
        integer(int64), intent(in), optional :: pt
        integer :: msg
        call next_error_1(n, status, msg, different_file, pt)
    end subroutine compilation_next_error

    !> compilation-next-error, giving the message MSG found.
    recursive subroutine next_error_1(n_in, status, msg, different_file, pt_in)
        integer(int64), intent(in) :: n_in
        type(status_type), intent(out) :: status
        integer, intent(out) :: msg
        logical, intent(in), optional :: different_file
        integer(int64), intent(in), optional :: pt_in
        integer(int64) :: pt, n, opt, limit, res
        type(status_type) :: st
        logical :: different, found, forward
        integer :: loc, last, k
        status = status_type()
        msg = 0
        different = .false.
        if (present(different_file)) different = different_file
        if (.not. compilation_buffer_p(current_buffer())) then
            call set_error(status, FM_GENERIC_ERROR, "Not in a compilation buffer")
            return
        end if
        pt = point()
        if (present(pt_in)) pt = pt_in
        n = n_in
        call compilation_ensure_parse(pt)
        k = entry_of(current_buffer())
        msg = message_at(pt)
        loc = 0
        if (msg /= 0) loc = compilation_buffers(k)%messages(msg)%loc
        last = 0
        if (n == 0) then
            ! Find message near here.
            if (msg == 0) then
                msg = message_at(max(pt - 1, point_min()))
                if (msg == 0) then
                    call previous_single_property_change(pt, sym("compilation_message"), res, found, st, &
                                                         line_beginning_position())
                    pt = res
                    msg = message_at(max(pt - 1, point_min()))
                    if (msg == 0) then
                        call compilation_next_single_property_change(pt, "compilation_message", res, found, &
                                                                     line_end_position())
                        pt = res
                        msg = message_at(pt)
                        if (msg == 0) pt = point()
                    end if
                end if
            end if
        else
            if (loc /= 0) last = compilation_buffers(k)%locs(loc)%file
            forward = n >= 0
            if (forward) then
                limit = point_max()
            else
                ! Don't move "back" to message at or before point.  Pass an
                ! explicit (point-min) to make sure pt is non-nil.
                call previous_single_property_change(pt, sym("compilation_message"), res, found, st, point_min())
                pt = res
                limit = point_min()
            end if
            ! compilation-loop.
            do while ((forward .and. n > 0) .or. (.not. forward .and. n < 0))
                opt = pt
                call property_change(pt, res, found)
                if (found) then
                    pt = res
                else
                    ! Handle the case where the first error message is at
                    ! the start of the buffer, and n < 0.
                    if (message_at(limit) == message_at(opt)) then
                        call loop_error()
                        return
                    end if
                    pt = limit
                end if
                ! prop 'compilation-message usually has 2 changes, on and
                ! off, so re-search if off.
                msg = message_at(pt)
                if (msg == 0) then
                    call property_change(pt, res, found, limit)
                    if (found) then
                        pt = res
                        msg = message_at(pt)
                    end if
                    if (msg == 0) then
                        call loop_error()
                        return
                    end if
                end if
                k = entry_of(current_buffer())
                if (compilation_buffers(k)%messages(msg)%type < compilation_skip_threshold%value) cycle
                if (different) then
                    if (loc_file(k, msg) == last) cycle
                    last = loc_file(k, msg)
                end if
                if (compilation_skip_visited%value) then
                    if (compilation_buffers(k)%messages(msg)%loc /= 0) then
                        if (compilation_buffers(k)%locs(compilation_buffers(k)%messages(msg)%loc)%visited) cycle
                    end if
                end if
                if (compilation_skip_to_next_location%value) then
                    if (compilation_buffers(k)%messages(msg)%loc == loc) cycle
                end if
                ! Count this message only if none of the above are true.
                if (forward) then
                    n = n - 1
                else
                    n = n + 1
                end if
            end do
        end if
        call goto_char(pt)
        if (msg == 0) call set_error(status, FM_USER_ERROR, "No "//COMPILATION_ERROR_STEM//" here")
    contains

        subroutine property_change(from, res, found, lim)
            integer(int64), intent(in) :: from
            integer(int64), intent(out) :: res
            logical, intent(out) :: found
            integer(int64), intent(in), optional :: lim
            type(status_type) :: st
            if (forward) then
                call compilation_next_single_property_change(from, "compilation_message", res, found, lim)
            else if (present(lim)) then
                call previous_single_property_change(from, sym("compilation_message"), res, found, st, lim)
            else
                call previous_single_property_change(from, sym("compilation_message"), res, found, st)
            end if
        end subroutine property_change

        subroutine loop_error()
            type(handle_type) :: proc
            proc = get_buffer_process(current_buffer())
            if (forward) then
                if (proc%index /= 0) then
                    call set_error(status, FM_USER_ERROR, "No more "//COMPILATION_ERROR_STEM//"s yet")
                else
                    call set_error(status, FM_USER_ERROR, "Past last "//COMPILATION_ERROR_STEM)
                end if
            else
                call set_error(status, FM_USER_ERROR, "Moved back before first "//COMPILATION_ERROR_STEM)
            end if
        end subroutine loop_error
    end subroutine next_error_1

    !> The file structure of the location of message MSG of entry K (0
    !> for none).
    integer function loc_file(k, msg) result(fs)
        integer, intent(in) :: k, msg
        fs = 0
        if (compilation_buffers(k)%messages(msg)%loc /= 0) then
            fs = compilation_buffers(k)%locs(compilation_buffers(k)%messages(msg)%loc)%file
        end if
    end function loc_file

    !> The command: move by the prefix argument.
    !>
    !> Move point to the next error in the compilation buffer.
    recursive subroutine cmd_compilation_next_error(status)
        type(status_type), intent(out) :: status
        call compilation_next_error(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_compilation_next_error

    !> Move point to the previous error in the compilation buffer.
    !> Prefix arg N says how many error messages to move backwards (or
    !> forwards, if negative).  Does NOT find the source line like
    !> \[previous_error].  This is like `compilation_next_error', but moves
    !> in the other direction.  Port of compile.el:compilation-previous-error.
    recursive subroutine compilation_previous_error(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        call compilation_next_error(-n, status)
    end subroutine compilation_previous_error

    !> The command: move back by the prefix argument.
    !>
    !> Move point to the previous error in the compilation buffer.
    recursive subroutine cmd_compilation_previous_error(status)
        type(status_type), intent(out) :: status
        call compilation_previous_error(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_compilation_previous_error

    !> Move point to the next error for a different file than the current
    !> one.  Prefix arg N says how many files to move forwards (or
    !> backwards, if negative).  Port of compile.el:compilation-next-file.
    recursive subroutine compilation_next_file(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        call compilation_next_error(n, status, .true.)
    end subroutine compilation_next_file

    !> The command: move by the prefix argument.
    !>
    !> Move point to the next error for a different file than the current
    !> one.
    recursive subroutine cmd_compilation_next_file(status)
        type(status_type), intent(out) :: status
        call compilation_next_file(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_compilation_next_file

    !> Move point to the previous error for a different file than the
    !> current one.  Prefix arg N says how many files to move backwards (or
    !> forwards, if negative).  Port of compile.el:compilation-previous-file.
    recursive subroutine compilation_previous_file(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        call compilation_next_file(-n, status)
    end subroutine compilation_previous_file

    !> The command: move back by the prefix argument.
    !>
    !> Move point to the previous error for a different file than the
    !> current one.
    recursive subroutine cmd_compilation_previous_file(status)
        type(status_type), intent(out) :: status
        call compilation_previous_file(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_compilation_previous_file

    !> Display the source for current error in another window.  Port of
    !> compile.el:compilation-display-error.
    recursive subroutine compilation_display_error(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: selected
        type(status_type) :: st
        call set_current_error_pos(point())
        ! (next-error-no-select 0)
        selected = selected_frame%selected_window
        call next_error(status, 0_int64)
        call select_window(selected, st, norecord=.true.)
    end subroutine compilation_display_error

    !> Make compilation-current-error the position POS of the current
    !> buffer.
    subroutine set_current_error_pos(pos)
        integer(int64), intent(in) :: pos
        type(status_type) :: st
        integer :: k
        k = current_entry()
        if (compilation_buffers(k)%current_error_marker%index /= 0) then
            call free_marker(compilation_buffers(k)%current_error_marker, st)
        end if
        compilation_buffers(k)%current_error_marker = null_handle()
        compilation_buffers(k)%has_current_error = .true.
        compilation_buffers(k)%current_error_pos = pos
    end subroutine set_current_error_pos

    !> Switch the `compilation_skip_threshold' level.  Port of
    !> compile.el:compilation-set-skip-threshold.
    subroutine compilation_set_skip_threshold(level)
        integer, intent(in) :: level
        compilation_skip_threshold = level
        select case (compilation_skip_threshold%value)
        case (0)
            call message("Skipping Nothing")
        case (1)
            call message("Skipping Info messages")
        case (2)
            call message("Skipping Warnings and info")
        case default
            call message("Skipping .false.")
        end select
    end subroutine compilation_set_skip_threshold

    !> The command: the level is the prefix argument, or the next one,
    !> modulo 3.
    !>
    !> Switch the `compilation_skip_threshold' level.
    subroutine cmd_compilation_set_skip_threshold(status)
        type(status_type), intent(out) :: status
        integer(int64) :: level
        status = status_type()
        if (.not. nilp(current_prefix_arg)) then
            level = prefix_numeric_value(current_prefix_arg)
        else
            level = compilation_skip_threshold%value + 1
        end if
        call compilation_set_skip_threshold(int(modulo(level, 3_int64)))
    end subroutine cmd_compilation_set_skip_threshold

    !> Visit the source for the error message at point.
    !> Use this command in a compilation log buffer.  Port of
    !> compile.el:compile-goto-error (visiting a directory message is
    !> Deferred: needs dired).
    recursive subroutine compile_goto_error(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. compilation_buffer_p(current_buffer())) then
            call set_error(status, FM_GENERIC_ERROR, "Not in a compilation buffer")
            return
        end if
        call compilation_ensure_parse(point())
        if (dir_at(point()) /= 0) then
            call set_error(status, FM_GENERIC_ERROR, "Visiting directories is not supported yet")
            return
        end if
        call set_current_error_pos(point())
        call next_error_internal(status)
    end subroutine compile_goto_error

    !> Update markers in LOC, and set MARKER to location pointed by LOC.
    !> SCREEN_COLUMNS and FIRST_COLUMN are the value of
    !> `compilation_error_screen_columns' and `compilation_first_column' to
    !> use if they are not set buffer-locally in the target buffer.  Port
    !> of compile.el:compilation--update-markers.
    recursive subroutine compilation_update_markers(loc, marker, screen_columns, first_column, status)
        integer, intent(in) :: loc
        type(handle_type), intent(in) :: marker
        logical, intent(in) :: screen_columns
        integer(int64), intent(in) :: first_column
        type(status_type), intent(out) :: status
        type(handle_type) :: comp, target, m
        type(status_type) :: st
        character(len=:), allocatable :: filename, directory
        logical :: screen
        integer(int64) :: first, last, opoint, begv, zv, moved
        integer :: k, fs, node, i, id
        comp = current_buffer()
        k = entry_of(comp)
        fs = compilation_buffers(k)%locs(loc)%file
        filename = compilation_buffers(k)%files(fs)%filename
        if (allocated(compilation_buffers(k)%files(fs)%directory)) then
            directory = compilation_buffers(k)%files(fs)%directory
            call compilation_find_file(marker, filename, target, status, directory)
        else
            call compilation_find_file(marker, filename, target, status)
        end if
        if (.not. status%ok()) return
        k = entry_of(comp)
        call set_buffer(target, status)
        if (.not. status%ok()) return
        screen = screen_columns
        first = first_column
        ! Obey the compilation-error-screen-columns of the target buffer if
        ! its major mode set it buffer-locally.
        if (compilation_error_screen_columns%local_p()) screen = compilation_error_screen_columns%value
        if (compilation_first_column%local_p()) first = compilation_first_column%value
        last = 1
        opoint = point()
        begv = point_min()
        zv = point_max()
        call widen()
        call goto_char(point_min())
        ! Treat file's found lines in forward order, 1 by 1.
        do node = size(compilation_buffers(k)%files(fs)%lines), 1, -1
            associate (line => compilation_buffers(k)%files(fs)%lines(node))
                ! Else this is a filename without a line#.
                if (line%has_line) then
                    call compilation_beginning_of_line(line%line - last + 1)
                    last = line%line
                end if
                ! Treat line's found columns and store/update a marker for
                ! each.
                do i = 1, size(line%locs)
                    id = line%locs(i)
                    if (compilation_buffers(k)%locs(id)%has_col) then
                        if (compilation_buffers(k)%locs(id)%col == -1) then
                            ! Special case for range end.
                            call end_of_line()
                        else
                            call compilation_move_to_column(compilation_buffers(k)%locs(id)%col, screen, first)
                        end if
                    else
                        call beginning_of_line()
                        moved = skip_chars_forward(" "//TB, st)
                    end if
                    if (compilation_buffers(k)%locs(id)%marker%index /= 0) then
                        call set_marker(compilation_buffers(k)%locs(id)%marker, point(), st)
                        if (.not. st%ok()) then
                            call point_marker(m)
                            compilation_buffers(k)%locs(id)%marker = m
                        end if
                    else
                        call point_marker(m)
                        compilation_buffers(k)%locs(id)%marker = m
                    end if
                end do
            end associate
        end do
        call narrow_to_region(begv, zv, st)
        call goto_char(opoint)
        call set_buffer(comp, st)
    end subroutine compilation_update_markers

    !> Advance to the next error message and visit the file where the
    !> error was.  This is the value of `next_error_function' in
    !> Compilation buffers.  Port of
    !> compile.el:compilation-next-error-function.
    recursive subroutine compilation_next_error_function(n, reset, status)
        integer(int64), intent(in) :: n
        logical, intent(in) :: reset
        type(status_type), intent(out) :: status
        type(handle_type) :: marker, comp, loc_marker, end_marker, mbuf
        type(status_type) :: st
        logical :: screen, valid
        integer(int64) :: first, pt
        integer :: k, msg, loc, end_loc
        comp = current_buffer()
        k = current_entry()
        if (reset) then
            if (compilation_buffers(k)%current_error_marker%index /= 0) then
                call free_marker(compilation_buffers(k)%current_error_marker, st)
            end if
            compilation_buffers(k)%current_error_marker = null_handle()
            compilation_buffers(k)%has_current_error = .false.
        end if
        screen = compilation_error_screen_columns%value
        first = compilation_first_column%value
        if (compilation_buffers(k)%has_current_error) then
            if (compilation_buffers(k)%current_error_marker%index /= 0) then
                call marker_position(compilation_buffers(k)%current_error_marker, pt, st)
            else
                pt = compilation_buffers(k)%current_error_pos
            end if
        else if (compilation_buffers(k)%has_messages_start) then
            if (compilation_buffers(k)%messages_start_marker%index /= 0) then
                call marker_position(compilation_buffers(k)%messages_start_marker, pt, st)
            else
                pt = compilation_buffers(k)%messages_start_pos
            end if
        else
            pt = point_min()
        end if
        call next_error_1(n, status, msg, pt_in=pt)
        if (.not. status%ok()) return
        k = entry_of(comp)
        loc = compilation_buffers(k)%messages(msg)%loc
        end_loc = compilation_buffers(k)%messages(msg)%end_loc
        if (loc == 0) then
            call set_error(status, FM_USER_ERROR, "No next error")
            return
        end if
        call point_marker(marker)
        if (compilation_buffers(k)%current_error_marker%index /= 0) then
            call free_marker(compilation_buffers(k)%current_error_marker, st)
        end if
        call point_marker(compilation_buffers(k)%current_error_marker)
        compilation_buffers(k)%has_current_error = .true.
        ! (overlay-arrow-position is Deferred.)
        ! If loc contains no marker, no error in that file has been
        ! visited.  If the marker is invalid the buffer has been killed.
        ! So, recalculate all markers for that file.
        valid = .false.
        if (compilation_buffers(k)%locs(loc)%marker%index /= 0) then
            call marker_buffer(compilation_buffers(k)%locs(loc)%marker, mbuf, st)
            valid = st%ok() .and. mbuf%index /= 0
        end if
        if (.not. valid) then
            call compilation_update_markers(loc, marker, screen, first, status)
            if (.not. status%ok()) then
                call free_marker(marker, st)
                return
            end if
        end if
        k = entry_of(comp)
        loc_marker = compilation_buffers(k)%locs(loc)%marker
        end_marker = null_handle()
        if (end_loc /= 0) end_marker = compilation_buffers(k)%locs(end_loc)%marker
        call compilation_goto_locus(marker, loc_marker, end_marker, status)
        call free_marker(marker, st)
        if (.not. status%ok()) return
        k = entry_of(comp)
        if (k /= 0) compilation_buffers(k)%locs(loc)%visited = .true.
    end subroutine compilation_next_error_function

    !> The command: visit the error the prefix argument says.
    !>
    !> Advance to the next error message and visit the file where the
    !> error was.
    recursive subroutine cmd_compilation_next_error_function(status)
        type(status_type), intent(out) :: status
        call compilation_next_error_function(prefix_numeric_value(current_prefix_arg), .false., status)
    end subroutine cmd_compilation_next_error_function

    !> Maybe align the compilation output window W with marker MK near
    !> top.  Port of compile.el:compilation-set-window (the terminal has no
    !> fringe).
    subroutine compilation_set_window(w, mk)
        type(handle_type), intent(in) :: w, mk
        type(status_type) :: st
        integer(int64) :: pos, opoint, moved
        call marker_position(mk, pos, st)
        opoint = point()
        if (compilation_context_lines%value >= 0) then
            call goto_char(pos)
            call compilation_beginning_of_line(1 - compilation_context_lines%value)
            call set_window_start(windows(w%index), point(), st)
            call goto_char(opoint)
        else if (compilation_context_lines%value == COMPILATION_CONTEXT_NIL) then
            ! There is no left fringe.
            call goto_char(pos)
            moved = forward_line(0_int64)
            call set_window_start(windows(w%index), line_beginning_position(), st)
            call goto_char(opoint)
        end if
        call set_window_point(windows(w%index), pos, st)
    end subroutine compilation_set_window

    !> Jump to an error corresponding to MSG at MK.
    !> All arguments are markers.  If END_MK is not the null handle, mark
    !> is set there.  Port of compile.el:compilation-goto-locus
    !> (highlighting the locus is Deferred).
    recursive subroutine compilation_goto_locus(msg, mk, end_mk, status)
        type(handle_type), intent(in) :: msg, mk, end_mk
        type(handle_type) :: msg_buffer, mk_buffer, pre_existing, w, selected
        type(display_alist_type) :: alist
        type(status_type) :: st
        type(status_type), intent(out) :: status
        logical :: from_compilation_buffer, saved_pop_up
        integer(int64) :: pos
        status = status_type()
        call marker_buffer(msg, msg_buffer, st)
        call marker_buffer(mk, mk_buffer, st)
        if (mk_buffer%index == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Marker does not point anywhere")
            return
        end if
        ! Show compilation buffer in other window, scrolled to this error.
        selected = selected_frame%selected_window
        from_compilation_buffer = same(windows(selected%index)%buffer, msg_buffer)
        ! Use an existing window if it is in a visible frame.
        pre_existing = get_buffer_window(msg_buffer)
        if (from_compilation_buffer .and. pre_existing%index /= 0) then
            w = pre_existing
        else
            call display_buffer(msg_buffer, w, st)
        end if
        call set_buffer(msg_buffer, st)
        call marker_position(msg, pos, st)
        call goto_char(pos)
        if (w%index /= 0) call compilation_set_window(w, msg)
        if (from_compilation_buffer .or. same(w, selected)) then
            ! If the compilation buffer window is selected, keep the
            ! compilation buffer in this window; display the source in
            ! another window.
            saved_pop_up = pop_up_windows
            pop_up_windows = .true.
            alist%inhibit_same_window = .true.
            call pop_to_buffer(mk_buffer, status, alist=alist)
            pop_up_windows = saved_pop_up
        else
            call switch_to_buffer(buffers(mk_buffer%index)%name, status)
        end if
        if (.not. status%ok()) return
        call set_buffer(mk_buffer, st)
        call marker_position(mk, pos, st)
        ! If narrowing gets in the way of going to the right place, widen.
        call goto_char(pos)
        if (point() /= pos) then
            call widen()
            call goto_char(pos)
        end if
        if (end_mk%index /= 0) then
            call marker_position(end_mk, pos, st)
            if (st%ok()) call push_mark(pos, nomsg=.true.)
        else
            if (buffers(cur)%mark_active) buffers(cur)%mark_active = .false.
        end if
    end subroutine compilation_goto_locus

    !> Expand FILENAME in DIRECTORY, or resolve its true name.  Port of
    !> compile.el:compilation--expand-fn.
    function compilation_expand_fn(directory, filename) result(name)
        character(len=*), intent(in) :: directory, filename
        character(len=:), allocatable :: name
        type(status_type) :: st
        call expand_file_name(filename, name, st, directory)
        if (file_exists_p(name)) return
        call file_truename(file_name_as_directory(directory)//filename, name, st)
        if (.not. st%ok()) call expand_file_name(filename, name, st, directory)
    end function compilation_expand_fn

    !> Search for FILENAME: in DIRECTORY (or `default_directory'), then
    !> when `compilation_search_all_directories', in the directories
    !> entered before MARKER.  The buffer found (null if none) and the
    !> directory searched.  Port of compile.el:compilation-find-file-1
    !> (`compilation_search_path' is Deferred: only its default, the
    !> directory, is searched).
    recursive subroutine compilation_find_file_1(marker, filename_in, buffer, spec_dir, status, directory)
        type(handle_type), intent(in) :: marker
        character(len=*), intent(in) :: filename_in
        type(handle_type), intent(out) :: buffer
        character(len=:), allocatable, intent(out) :: spec_dir
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: directory
        character(len=:), allocatable :: filename, name, absolute, thisdir
        type(handle_type) :: mbuf, old
        type(status_type) :: st
        integer(int64) :: pos, opoint
        integer :: d, k
        logical :: found
        status = status_type()
        buffer = null_handle()
        if (present(directory)) then
            call expand_file_name(directory, spec_dir, st, default_dir())
        else
            spec_dir = default_dir()
        end if
        filename = filename_in
        thisdir = spec_dir
        if (file_name_absolute_p(filename)) then
            ! The file name is absolute.  Use its explicit directory as the
            ! first in the search path, and strip it from FILENAME.
            call expand_file_name(filename, absolute, st, default_dir())
            filename = abbreviate_file_name(absolute)
            thisdir = file_name_directory(filename)
            filename = file_name_nondirectory(filename)
            name = compilation_expand_fn(thisdir, filename)
            if (file_exists_p(name)) then
                call find_file_noselect(name, buffer, status)
                return
            end if
            ! Then the nil of compilation-search-path.
            thisdir = spec_dir
        end if
        name = compilation_expand_fn(thisdir, filename)
        if (file_exists_p(name)) then
            call find_file_noselect(name, buffer, status)
            return
        end if
        ! If we haven't found it, this might be a parallel build.  Search
        ! the directories further up the buffer.
        if (.not. compilation_search_all_directories%value) return
        call marker_buffer(marker, mbuf, st)
        if (mbuf%index == 0) return
        old = current_buffer()
        call set_buffer(mbuf, st)
        k = entry_of(mbuf)
        opoint = point()
        call marker_position(marker, pos, st)
        call goto_char(pos)
        call previous_single_property_change(point(), sym("compilation_directory"), pos, found, st)
        if (found) call goto_char(pos)
        d = dir_at(point() - 1)
        if (d == 0) d = dir_at(point())
        call goto_char(opoint)
        ! (cdr dirs): the directories before the innermost.
        if (d /= 0) d = compilation_buffers(k)%dirs(d)%parent
        do while (d /= 0)
            if (allocated(compilation_buffers(k)%dirs(d)%dir)) then
                thisdir = compilation_buffers(k)%dirs(d)%dir
                name = compilation_expand_fn(thisdir, filename)
                if (file_exists_p(name)) then
                    call set_buffer(old, st)
                    call find_file_noselect(name, buffer, status)
                    return
                end if
            end if
            k = entry_of(mbuf)
            d = compilation_buffers(k)%dirs(d)%parent
        end do
        call set_buffer(old, st)
    end subroutine compilation_find_file_1

    !> The current buffer's default directory.
    function default_dir() result(dir)
        character(len=:), allocatable :: dir
        dir = ""
        if (cur == 0) return
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
    end function default_dir

    !> Find a buffer for file FILENAME.
    !> If FILENAME is not found at all, ask the user where to find it.
    !> Pop up the buffer containing MARKER and scroll to MARKER if we ask
    !> the user where to find the file.  The directory searched is
    !> DIRECTORY; if it is relative, it is combined with
    !> `default_directory'; if absent, `default_directory' is used.  Port
    !> of compile.el:compilation-find-file (FORMATS and
    !> `compilation_search_path' are Deferred).
    recursive subroutine compilation_find_file(marker, filename, buffer, status, directory)
        use minibuffer_fm, only: read_file_name
        use dispnew_fm, only: ding
        type(handle_type), intent(in) :: marker
        character(len=*), intent(in) :: filename
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: directory
        character(len=:), allocatable :: spec_dir, name, origname
        type(handle_type) :: mbuf, w, old
        type(status_type) :: st
        integer(int64) :: pos, opoint
        logical :: saved_pop_up
        call compilation_find_file_1(marker, filename, buffer, spec_dir, status, directory)
        if (.not. status%ok()) return
        do while (buffer%index == 0)
            ! The file doesn't exist.  Ask the user where to find it.
            call marker_buffer(marker, mbuf, st)
            old = current_buffer()
            opoint = point()
            if (mbuf%index /= 0) then
                saved_pop_up = pop_up_windows
                pop_up_windows = .true.
                call display_buffer(mbuf, w, st)
                pop_up_windows = saved_pop_up
                call set_buffer(mbuf, st)
                call marker_position(marker, pos, st)
                call goto_char(pos)
                if (w%index /= 0) call compilation_set_window(w, marker)
                call set_buffer(old, st)
                call goto_char(opoint)
            end if
            call read_file_name("Find this "//COMPILATION_ERROR_STEM//" in (default "//filename//"): ", name, status, &
                                dir=spec_dir, default_filename=filename, mustmatch=1)
            if (.not. status%ok()) return
            origname = name
            if (.not. file_exists_p(name)) then
                call message(format_message("Cannot find file `")//name//format_message("'"))
                call ding(status=st)
                call sit_for(2000_int32)
            else if (file_directory_p(name)) then
                name = compilation_expand_fn(name, filename)
                if (.not. file_exists_p(name)) then
                    call message(format_message("No `")//filename//format_message("' in directory ")//origname)
                    call ding(status=st)
                    call sit_for(2000_int32)
                else
                    call find_file_noselect(name, buffer, status)
                    if (.not. status%ok()) return
                end if
            else
                call find_file_noselect(name, buffer, status)
                if (.not. status%ok()) return
            end if
        end do
    end subroutine compilation_find_file

    ! ------------------------------------------------------------------
    ! Compilation mode and the minor modes
    ! ------------------------------------------------------------------

    !> Major mode for compilation log buffers.
    !> \<compilation_mode_map>To visit the source for a line-numbered
    !> error, move point to the error message line and type
    !> \[compile_goto_error].  To kill the compilation, type
    !> \[kill_compilation].
    !>
    !> Runs `compilation_mode_hook' with `run_mode_hooks' (which see).
    !> Port of compile.el:compilation-mode (NAME-OF-MODE "Compilation").
    recursive subroutine compilation_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        if (.not. associated(major_modes%compilation_mode%run)) call syms_of_compile()
        ! kill-all-local-variables, use-local-map, major-mode, mode-name.
        call major_modes%compilation_mode%begin(saved, status)
        if (.not. status%ok()) return
        call compilation_setup(status)
        if (.not. status%ok()) return
        buffers(cur)%read_only = .true.
        call major_modes%compilation_mode%finish(saved, status)
    end subroutine compilation_mode

    !> Prepare the buffer for the compilation parsing commands to work.
    !> Optional argument MINOR indicates this is called from
    !> `compilation_minor_mode'.  Port of compile.el:compilation-setup.
    recursive subroutine compilation_setup(status, minor)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: minor
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: is_minor
        integer :: k
        status = status_type()
        is_minor = .false.
        if (present(minor)) is_minor = minor
        k = current_entry()
        compilation_buffers(k)%has_counts = .true.
        compilation_buffers(k)%num_errors = 0
        compilation_buffers(k)%num_warnings = 0
        compilation_buffers(k)%num_infos = 0
        ! Note that compilation-next-error-function is for interfacing
        ! with the next-error function in simple.el, and it's only
        ! coincidentally named similarly to compilation-next-error.
        call set_next_error_function(compilation_next_error_function)
        ! compilation-locs.
        compilation_buffers(k)%setup = .true.
        compilation_buffers(k)%nkeys = 0
        call before_change_functions%add(compilation_flush_parse, local=.true.)
        ! Also for minor mode, since it's not permanent-local.
        call change_major_mode_hook%add(remove_properties_hook, local=.true.)
        call compilation_mode_font_lock_keywords(keywords)
        if (is_minor) then
            call font_lock_add_keywords("", keywords, status)
            if (.not. status%ok()) return
            call font_lock_flush(status)
        else
            call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        end if
    end subroutine compilation_setup

    !> Undo `compilation_setup' for the minor modes.  Port of
    !> compile.el:compilation--unsetup (removing the font-lock keywords is
    !> Deferred).
    recursive subroutine compilation_unsetup(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer :: k
        status = status_type()
        call before_change_functions%remove(compilation_flush_parse, local=.true.)
        k = entry_of(current_buffer())
        if (k /= 0) then
            if (compilation_buffers(k)%parsed%index /= 0) call free_marker(compilation_buffers(k)%parsed, st)
            compilation_buffers(k)%parsed = null_handle()
        end if
        call compilation_remove_properties()
        call font_lock_flush(status)
    end subroutine compilation_unsetup

    !> Return expressions to highlight in Compilation mode: parsing, then
    !> what compile.el compilation-mode-font-lock-keywords highlights (the
    !> face variables are read when the keywords are made).  Port of
    !> compile.el:compilation-mode-font-lock-keywords.
    subroutine compilation_mode_font_lock_keywords(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)
        allocate (keywords(6))
        keywords(1)%matcher => ensure_parse_matcher
        allocate (keywords(1)%highlights(0))
        ! configure output lines.  (compilation-face '(4 . 3)) on 2: the
        ! error face, unless 3 (info) or 4 (warning), which are all of 2,
        ! matched.
        keywords(2)%regexp = "^[Cc]hecking \(?:[Ff]or \|[Ii]f \|[Ww]hether \(?:to \)?\)?\(.+\)\.\.\. *" &
                             //"\(?:(cached) *\)?\(\(yes\(?: .+\)?\)\|no\|\(.*\)\)$"
        allocate (keywords(2)%highlights(4))
        call set_highlight(keywords(2)%highlights(1), 1, "font_lock_variable_name_face")
        call set_highlight(keywords(2)%highlights(2), 2, setting_string(compilation_error_face))
        call set_highlight(keywords(2)%highlights(3), 3, setting_string(compilation_info_face), .true.)
        call set_highlight(keywords(2)%highlights(4), 4, setting_string(compilation_warning_face), .true.)
        ! Command output lines.  Recognize `make[n]:' lines too.
        keywords(3)%regexp = "^\([[:alnum:]_/.+-]+\)\(\[\([0-9]+\)\]\)?["//TB//" ]*:"
        allocate (keywords(3)%highlights(2))
        call set_highlight(keywords(3)%highlights(1), 1, "font_lock_function_name_face")
        call set_highlight(keywords(3)%highlights(2), 3, setting_string(compilation_line_face), laxmatch=.true.)
        keywords(4)%regexp = " --?o\(?:utfile\|utput\)?[= ]\(\S +\)"
        allocate (keywords(4)%highlights(1))
        call set_highlight(keywords(4)%highlights(1), 1, "font_lock_keyword_face")
        keywords(5)%regexp = "^Compilation \(finished\).*"
        allocate (keywords(5)%highlights(1))
        call set_highlight(keywords(5)%highlights(1), 1, setting_string(compilation_info_face))
        keywords(6)%regexp = "^Compilation \(exited abnormally\|interrupt\|killed\|terminated\|" &
                             //"segmentation fault\)\(?:.*with code \([0-9]+\)\)?.*"
        allocate (keywords(6)%highlights(2))
        call set_highlight(keywords(6)%highlights(1), 1, setting_string(compilation_error_face))
        call set_highlight(keywords(6)%highlights(2), 2, setting_string(compilation_error_face), laxmatch=.true.)
    end subroutine compilation_mode_font_lock_keywords

    !> Make H the MATCH-HIGHLIGHT of SUBEXP in FACE; OVERRIDE true for an
    !> optional (lax) highlight replacing the faces there, LAXMATCH true
    !> for one that need not match.
    subroutine set_highlight(h, subexp, face, override, laxmatch)
        type(font_lock_highlight_type), intent(inout) :: h
        integer, intent(in) :: subexp
        character(len=*), intent(in) :: face
        logical, intent(in), optional :: override, laxmatch
        h%match%subexp = subexp
        h%match%face = face
        h%match%laxmatch = .false.
        if (present(laxmatch)) h%match%laxmatch = laxmatch
        if (present(override)) then
            if (override) then
                h%match%override = FONT_LOCK_OVERRIDE_T
                h%match%laxmatch = .true.
            end if
        end if
    end subroutine set_highlight

    !> Toggle Compilation minor mode.
    !>
    !> When Compilation minor mode is enabled, all the error-parsing
    !> commands of Compilation major mode are available.  See
    !> `compilation_mode'.  Port of compile.el:compilation-minor-mode.
    recursive subroutine compilation_minor_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%compilation_minor_mode%command(status)
    end subroutine compilation_minor_mode

    !> The body of Compilation minor mode.
    recursive subroutine compilation_minor_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        if (enable) then
            call compilation_setup(status, .true.)
        else
            call compilation_unsetup(status)
        end if
    end subroutine compilation_minor_mode_body

    !> Toggle Compilation Shell minor mode.
    !>
    !> When Compilation Shell minor mode is enabled, all the error-parsing
    !> commands of the Compilation major mode are available but bound to
    !> keys that don't collide with Shell mode.  See `compilation_mode'.
    !> Port of compile.el:compilation-shell-minor-mode.
    recursive subroutine compilation_shell_minor_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%compilation_shell_minor_mode%command(status)
    end subroutine compilation_shell_minor_mode

    ! ------------------------------------------------------------------
    ! Starting
    ! ------------------------------------------------------------------

    !> The default value of `compile_command': "make -k -jN ", N being
    !> the number of processors divided by 1.5, rounded up.
    function default_compile_command() result(command)
        character(len=:), allocatable :: command
        character(len=16) :: n
        integer :: p
        p = num_processors()
        write (n, "(i0)") (2*p + 2)/3
        command = "make -k -j"//trim(n)//" "
    end function default_compile_command

    !> Read the compilation command, COMMAND as initial input.  Port of
    !> compile.el:compilation-read-command.
    subroutine compilation_read_command_from_user(command, result, status)
        use lisp_fm, only: stringp, xstring
        use data_fm, only: car
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        type(value_type) :: history, first
        logical :: same_first
        same_first = .false.
        call symbol_value(sym("compile_history"), history, status)
        if (status%ok()) call car(history, first, status)
        if (status%ok()) then
            if (stringp(first)) same_first = xstring(first) == command .and. len(xstring(first)) == len(command)
        end if
        if (same_first) then
            call read_shell_command("Compile command: ", result, status, initial=command, history="compile_history", &
                                    history_position=1)
        else
            call read_shell_command("Compile command: ", result, status, initial=command, history="compile_history")
        end if
    end subroutine compilation_read_command_from_user

    !> Compile the program including the current buffer.  Default: run
    !> `make'.
    !> Runs COMMAND, a shell command, in a separate process asynchronously
    !> with output going to the buffer `*compilation*'.
    !>
    !> You can then use the command \[next_error] to find the next error
    !> message and move to the source code that caused it.
    !>
    !> If optional second arg COMINT is true the buffer will be in Comint
    !> mode with `compilation_shell_minor_mode'.
    !>
    !> Interactively, prompts for the command if the variable
    !> `compilation_read_command' is true; otherwise uses
    !> `compile_command'.  With prefix arg, always prompts.
    !> Additionally, with universal prefix arg, compilation buffer will be
    !> in comint mode, i.e. interactive.
    !>
    !> To run more than one compilation at once, start one then rename
    !> the `*compilation*' buffer to some other name with
    !> \[rename_buffer].  Then _switch buffers_ and start the new
    !> compilation.  It will create a new `*compilation*' buffer.
    !>
    !> On most systems, termination of the main compilation process kills
    !> its subprocesses.  Port of compile.el:compile.
    recursive subroutine compile(command, status, comint)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: comint
        character(len=:), allocatable :: dir
        if (command /= compile_command_value() .or. len(command) /= len(compile_command_value())) then
            compile_command = command
        end if
        call save_some_buffers(.not. compilation_ask_about_save%value, .false., status)
        if (.not. status%ok()) return
        dir = ""
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        ! setq-default: the value of buffers without their own.
        compilation_directory = dir
        call compilation_start(command, status, comint)
    end subroutine compile

    !> `compile_command', set to its default value first if needed.
    function compile_command_value() result(command)
        character(len=:), allocatable :: command
        if (.not. allocated(compile_command%value)) compile_command = default_compile_command()
        command = compile_command%value
    end function compile_command_value

    !> The command: interactively, read the command (if
    !> `compilation_read_command' or with a prefix argument), then compile,
    !> in Comint mode with C-u.  The interactive form of compile.el:compile.
    !>
    !> Compile the program including the current buffer.  Default: run
    !> `make'.
    recursive subroutine cmd_compile(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: command, initial
        command = compile_command_value()
        if (compilation_read_command%value .or. .not. nilp(current_prefix_arg)) then
            initial = command
            call compilation_read_command_from_user(initial, command, status)
            if (.not. status%ok()) return
        end if
        call compile(command, status, consp(current_prefix_arg))
    end subroutine cmd_compile

    !> Re-compile the program including the current buffer.
    !> If this is run in a Compilation mode buffer, reuse the arguments from
    !> the original use.  Otherwise, recompile using `compile_command'.
    !> If the optional argument `edit_command' is true, the command can be
    !> edited.  Port of compile.el:recompile.
    recursive subroutine recompile(status, edit_command)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: edit_command
        character(len=:), allocatable :: command, saved_dir, dir, saved_command
        logical :: edit, had_dir, comint
        type(handle_type) :: old
        integer :: k
        edit = .false.
        if (present(edit_command)) edit = edit_command
        call save_some_buffers(.not. compilation_ask_about_save%value, .false., status)
        if (.not. status%ok()) return
        old = current_buffer()
        ! (let ((default-directory (or compilation-directory default-directory)))
        had_dir = allocated(buffers(cur)%directory)
        if (had_dir) saved_dir = buffers(cur)%directory
        dir = compilation_directory_value()
        if (len(dir) > 0) buffers(cur)%directory = dir
        command = compile_command_value()
        k = entry_of(current_buffer())
        if (k /= 0) then
            if (.not. compilation_buffers(k)%has_arguments) k = 0
        end if
        if (edit) then
            if (k /= 0) then
                call compilation_read_command_from_user(compilation_buffers(k)%command, command, status)
            else
                saved_command = command
                call compilation_read_command_from_user(saved_command, command, status)
            end if
            if (.not. status%ok()) then
                call restore_dir()
                return
            end if
            k = entry_of(current_buffer())
            if (k /= 0) then
                if (compilation_buffers(k)%has_arguments) compilation_buffers(k)%command = command
            end if
        end if
        comint = .false.
        if (k /= 0) then
            command = compilation_buffers(k)%command
            comint = compilation_buffers(k)%comint
        end if
        call compilation_start(command, status, comint)
        call restore_dir()
    contains

        subroutine restore_dir()
            type(status_type) :: st
            ! The let binding ends in the buffer that was current.
            if (.not. buffer_live_p(old)) return
            call set_buffer(old, st)
            if (had_dir) then
                buffers(cur)%directory = saved_dir
            else if (allocated(buffers(cur)%directory)) then
                deallocate (buffers(cur)%directory)
            end if
        end subroutine restore_dir
    end subroutine recompile

    !> The current buffer's `compilation_directory' ("" for nil).
    function compilation_directory_value() result(dir)
        character(len=:), allocatable :: dir
        integer :: k
        dir = ""
        k = entry_of(current_buffer())
        if (k /= 0) then
            if (allocated(compilation_buffers(k)%directory)) then
                dir = compilation_buffers(k)%directory
                return
            end if
        end if
        if (allocated(compilation_directory%value)) dir = compilation_directory%value
    end function compilation_directory_value

    !> The command: recompile, editing the command with a prefix argument.
    !>
    !> Re-compile the program including the current buffer.
    recursive subroutine cmd_recompile(status)
        type(status_type), intent(out) :: status
        call recompile(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_recompile

    !> Insert TEXT at point, adding the `compilation_annotation' text
    !> property.  This property is used to distinguish output of the
    !> compilation process from additional information inserted by Emacs.
    !> Port of compile.el:compilation-insert-annotation.
    subroutine compilation_insert_annotation(text)
        character(len=*), intent(in) :: text
        type(status_type) :: st
        integer(int64) :: start
        start = point()
        call insert(text, st)
        call put_text_property(start, point(), sym("compilation_annotation"), t_value(), st)
    end subroutine compilation_insert_annotation

    !> The first 19 characters of `current_time_string': "Sun Sep 16
    !> 01:03:52".  Cf. editfns.c:Fcurrent_time_string.
    function current_time_19() result(s)
        character(len=:), allocatable :: s
        integer :: t(7)
        character(len=19) :: buf
        t = emacs_localtime_wday(emacs_time())
        write (buf, "(a3, 1x, a3, 1x, i2, 1x, i2.2, ':', i2.2, ':', i2.2)") DAY_NAMES(t(7)), MONTH_NAMES(t(2)), t(3), &
            t(4), t(5), t(6)
        s = buf
    end function current_time_19

    !> The name of MODE (its mode-name) in lower case.  Cf.
    !> compile.el:compilation--downcase-mode-name.
    function downcase_name(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        integer :: i
        s = name
        do i = 1, len(s)
            if (s(i:i) >= "A" .and. s(i:i) <= "Z") s(i:i) = achar(iachar(s(i:i)) + 32)
        end do
    end function downcase_name

    !> Run compilation command COMMAND (low level interface).
    !> If COMMAND starts with a cd command, that becomes the
    !> `default_directory'.
    !>
    !> COMINT true means use `compilation_shell_minor_mode' under
    !> `comint_mode' (MODE t), else the buffer is in Compilation mode.
    !>
    !> The buffer is the current buffer if it is in Compilation mode, else
    !> *compilation*.  Port of compile.el:compilation-start (MODE
    !> `compilation-mode' or t; NAME-FUNCTION, HIGHLIGHT-REGEXP and
    !> CONTINUE are Deferred).
    recursive subroutine compilation_start(command, status, comint)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: comint
        character(len=:), allocatable :: thisdir, cd_dir, origdir
        type(handle_type) :: outbuf, comp_proc, outwin, old, proc
        type(process_arg_type) :: args(3), env(2), switches(2)
        type(value_type) :: inhibit
        type(status_type) :: st
        logical :: answer, scroll, use_comint
        integer(int64) :: desired
        integer :: k
        use_comint = .false.
        if (present(comint)) use_comint = comint
        old = current_buffer()
        thisdir = ""
        if (allocated(buffers(cur)%directory)) thisdir = buffers(cur)%directory
        ! compilation--default-buffer-name.
        if (buffers(cur)%major_mode == "compilation_mode") then
            outbuf = current_buffer()
        else
            call get_buffer_create("*compilation*", outbuf, status)
            if (.not. status%ok()) return
        end if
        call set_buffer(outbuf, status)
        if (.not. status%ok()) return
        comp_proc = get_buffer_process(outbuf)
        if (comp_proc%index /= 0) then
            answer = process_status(comp_proc) /= "run" .or. .not. process_query_on_exit_flag(comp_proc)
            if (.not. answer) then
                call yes_or_no_p("A compilation process is running; kill it? ", answer, status)
                if (.not. status%ok()) return
            end if
            if (answer) then
                call interrupt_process(st, comp_proc)
                if (st%ok()) then
                    call sit_for(1000_int32)
                    call delete_process(comp_proc, st)
                end if
            else
                call set_error(status, FM_GENERIC_ERROR, format_message("Cannot have two processes in `")// &
                               buffers(cur)%name//format_message("' at once"))
                call set_buffer(old, st)
                return
            end if
        end if
        ! First transfer directory from where M-x compile was called.
        buffers(cur)%directory = thisdir
        ! Make compilation buffer read-only.  The filter can still write it.
        ! Clear out the compilation buffer.
        call symbol_value(sym("inhibit_read_only"), inhibit, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        ! Then evaluate a cd command if any, but don't perform it yet, else
        ! start-command would do it again through the shell: (cd "..") AND
        ! sh -c "cd ..; make"
        call command_cd_directory(command, thisdir, cd_dir)
        call cd(cd_dir, status)
        if (.not. status%ok()) then
            call set(sym("inhibit_read_only"), inhibit, st)
            buffers(cur)%directory = thisdir
            return
        end if
        call erase_buffer(st)
        ! Select the desired mode.
        origdir = buffers(cur)%directory
        if (.not. use_comint) then
            buffers(cur)%undo_enabled = .false.
            if (allocated(buffers(cur)%undo_list)) deallocate (buffers(cur)%undo_list)
            call compilation_mode(status)
        else
            buffers(cur)%read_only = .false.
            call comint_mode(status)
            if (status%ok()) call minor_modes%compilation_shell_minor_mode%enable(status)
        end if
        if (.not. status%ok()) then
            call set(sym("inhibit_read_only"), inhibit, st)
            return
        end if
        ! Remember the original dir, so we can use it when we recompile.
        k = current_entry()
        compilation_buffers(k)%directory = thisdir
        if (buffer_size() == 0) then
            ! Output a mode setter, for saving and later reloading this
            ! buffer.
            call compilation_insert_annotation("-*- mode: compilation; default-directory: "// &
                                               prin1_to_string(make_string(abbreviate_file_name(origdir)))// &
                                               " -*-"//NL)
        end if
        call compilation_insert_annotation(buffers(cur)%mode_name//" started at "//current_time_19()//NL//NL// &
                                           command//NL)
        k = current_entry()
        call system_clock(compilation_buffers(k)%start_count)
        call set(sym("inhibit_read_only"), inhibit, st)
        ! The let binding of default-directory ends here.
        buffers(cur)%directory = thisdir
        call restore_buffer_modified_p(.false.)
        ! Pop up the compilation buffer.
        call display_buffer(outbuf, outwin, st)
        scroll = compilation_scroll_output%value /= COMPILATION_SCROLL_NIL
        k = current_entry()
        compilation_buffers(k)%has_arguments = .true.
        compilation_buffers(k)%command = command
        compilation_buffers(k)%comint = use_comint
        if (outwin%index /= 0 .and. .not. scroll) call set_window_start(windows(outwin%index), point_min(), st)
        ! Position point as the user will see it.
        if (scroll) then
            desired = point_max()
        else
            desired = point_min()
        end if
        call goto_char(desired)
        if (outwin%index /= 0 .and. .not. same(outwin, selected_frame%selected_window)) then
            call set_window_point(windows(outwin%index), desired, st)
        end if
        ! Start the compilation.
        if (.not. allocated(shell_file_name%value)) call init_callproc()
        env(1)%s = "INSIDE_EMACS="//upstream_version//",compile"
        ! Some external programs (like "git grep") use a pager; defeat that.
        env(2)%s = "PAGER="
        if (use_comint) then
            switches(1)%s = shell_command_switch%value
            switches(2)%s = command
            call comint_exec(outbuf, downcase_name(buffers(cur)%mode_name), shell_file_name%value, switches, status, &
                             environment=env)
            if (status%ok()) proc = get_buffer_process(outbuf)
        else
            args(1)%s = shell_file_name%value
            args(2)%s = shell_command_switch%value
            args(3)%s = command
            call make_process(downcase_name(buffers(cur)%mode_name), args, proc, status, buffer=outbuf, &
                              environment=env)
        end if
        if (.not. status%ok()) then
            call set_buffer(old, st)
            return
        end if
        ! Make the buffer's mode line show process state.
        k = current_entry()
        compilation_buffers(k)%mode_line = 1
        call update_mode_line_process(k)
        ! Set the process as killable without query by default.
        if (compilation_always_kill%value) call set_process_query_on_exit_flag(proc, .false., st)
        call set_process_sentinel_and_filter(proc,.not. use_comint)
        ! Use (point-max) here so that output comes in after the initial
        ! text, regardless of where the user sees point.
        call set_marker(process_mark(proc), point_max(), st, outbuf)
        if (compilation_disable_input%value) call process_send_eof(st, proc)
        call push_in_progress(proc)
        ! Now finally cd to where the shell started make/grep/...
        buffers(cur)%directory = origdir
        if (scroll) call goto_char(point_max())
        ! Make it so the next C-x ` will use this buffer.
        next_error_last_buffer = outbuf
        call set_buffer(old, st)
    end subroutine compilation_start

    subroutine set_process_sentinel_and_filter(proc, filter)
        use process_fm, only: set_process_sentinel, set_process_filter
        type(handle_type), intent(in) :: proc
        logical, intent(in) :: filter
        type(status_type) :: st
        call set_process_sentinel(proc, st, compilation_sentinel)
        ! Keep the comint filter, since it's needed for proper handling of
        ! the prompts.
        if (filter) call set_process_filter(proc, st, compilation_filter)
    end subroutine set_process_sentinel_and_filter

    subroutine push_in_progress(proc)
        type(handle_type), intent(in) :: proc
        type(handle_type), allocatable :: grown(:)
        integer :: n
        if (.not. allocated(compilation_in_progress)) allocate (compilation_in_progress(0))
        n = size(compilation_in_progress)
        allocate (grown(n + 1))
        grown(1) = proc
        grown(2:) = compilation_in_progress
        call move_alloc(grown, compilation_in_progress)
    end subroutine push_in_progress

    !> Whether compilations are in progress (`compilation_in_progress' is
    !> true), for the "[Compiling] " of the mode line.
    logical function compilation_in_progress_p()
        compilation_in_progress_p = .false.
        if (allocated(compilation_in_progress)) compilation_in_progress_p = size(compilation_in_progress) > 0
    end function compilation_in_progress_p

    !> Give entry K's buffer (the current buffer) the `mode_line_process'
    !> of its state: ":%s" or the exit status, then the counts
    !> (compile.el compilation-mode-line-errors).
    subroutine update_mode_line_process(k)
        integer, intent(in) :: k
        type(mode_line_part_type) :: parts(8)
        if (compilation_buffers(k)%mode_line == 0) return
        if (compilation_buffers(k)%mode_line == 1) then
            parts(1)%text = ":%s"
            parts(1)%face = "compilation_mode_line_run"
        else
            parts(1)%text = compilation_buffers(k)%exit_text
            parts(1)%face = compilation_buffers(k)%exit_face
        end if
        parts(2)%text = " ["
        parts(2)%face = ""
        parts(3)%text = count_string(compilation_buffers(k)%num_errors)
        parts(3)%face = "compilation_error"
        parts(4)%text = " "
        parts(4)%face = ""
        parts(5)%text = count_string(compilation_buffers(k)%num_warnings)
        parts(5)%face = "compilation_warning"
        parts(6)%text = " "
        parts(6)%face = ""
        parts(7)%text = count_string(compilation_buffers(k)%num_infos)
        parts(7)%face = "compilation_info"
        parts(8)%text = "]"
        parts(8)%face = ""
        call set_mode_line_process(parts)
    end subroutine update_mode_line_process

    function count_string(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function count_string

    !> The directory a leading "cd DIR;" of COMMAND changes to (DEFAULT if
    !> there is none): the cond of compile.el:compilation-start.
    subroutine command_cd_directory(command, default, dir)
        character(len=*), intent(in) :: command, default
        character(len=:), allocatable, intent(out) :: dir
        character(len=:), allocatable :: arg, out
        type(status_type) :: st
        logical :: matched
        integer :: k
        integer(int64) :: idx
        idx = string_match("\`\s *cd\(?:\s +\(\S +?\|'[^']*'\|""\(?:[^""`$\\]\|\\.\)*""\)\)?\s *[;&"//NL// &
                           "]", command, st)
        if (idx == 0) then
            dir = default
            return
        end if
        arg = match_string(1, command, matched)
        if (.not. matched) then
            dir = "~"
        else if (arg(1:1) == "'") then
            dir = arg(2:len(arg) - 1)
        else if (arg(1:1) == """") then
            ! (replace-regexp-in-string "\\\\\\(.\\)" "\\1" ...)
            out = ""
            k = 2
            do while (k <= len(arg) - 1)
                if (arg(k:k) == "\" .and. k < len(arg) - 1) then
                    k = k + 1
                end if
                out = out//arg(k:k)
                k = k + 1
            end do
            dir = out
        else
            ! Globbing (file-expand-wildcards) is Deferred.
            dir = substitute_env_vars(arg, .false.)
        end if
    end subroutine command_cd_directory

    !> Make DIR become the current buffer's default directory.
    !> If your environment includes a `CDPATH' variable, try each one of
    !> that list of directories (separated by occurrences of
    !> `path_separator') when resolving a relative directory name.  Port of
    !> files.el:cd and cd-absolute.
    subroutine cd(dir, status)
        character(len=*), intent(in) :: dir
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cdpath, entry, candidate, absolute, base, cdir
        logical :: found
        integer :: b, e
        base = ""
        if (allocated(buffers(cur)%directory)) base = buffers(cur)%directory
        if (file_name_absolute_p(dir)) then
            call expand_file_name(dir, absolute, status, base)
            if (.not. status%ok()) return
        else
            ! cd-path: CDPATH's directories, or "./".
            call getenv("CDPATH", cdpath, found)
            if (.not. found) cdpath = "./"
            absolute = ""
            b = 1
            do while (b <= len(cdpath) + 1)
                e = index(cdpath(b:), ":")
                if (e == 0) then
                    e = len(cdpath) + 1
                else
                    e = b + e - 1
                end if
                entry = cdpath(b:e - 1)
                if (len(entry) == 0) entry = "./"
                call expand_file_name(entry, cdir, status, base)
                if (.not. status%ok()) cdir = base
                call expand_file_name(dir, candidate, status, file_name_as_directory(cdir))
                if (status%ok()) then
                    if (file_directory_p(candidate)) then
                        absolute = candidate
                        exit
                    end if
                end if
                b = e + 1
            end do
            if (len(absolute) == 0) then
                call set_error(status, FM_GENERIC_ERROR, "No such directory found via CDPATH environment variable")
                return
            end if
        end if
        ! cd-absolute.
        absolute = abbreviate_file_name(file_name_as_directory(absolute))
        if (.not. file_directory_p(absolute)) then
            if (file_exists_p(absolute)) then
                call set_error(status, FM_GENERIC_ERROR, absolute//" is not a directory")
            else
                call set_error(status, FM_GENERIC_ERROR, absolute//": no such directory")
            end if
            return
        end if
        if (.not. file_accessible_directory_p(absolute)) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot cd to "//absolute//":  Permission denied")
            return
        end if
        buffers(cur)%directory = absolute
    end subroutine cd

    ! ------------------------------------------------------------------
    ! Output and exit
    ! ------------------------------------------------------------------

    !> Process filter for compilation buffers.
    !> Just inserts the text, handles carriage motion (see
    !> `comint_inhibit_carriage_motion') and runs
    !> `compilation_filter_hook'.  Port of compile.el:compilation-filter
    !> (`compilation_max_output_line_length' and
    !> `compilation_hidden_output' are Deferred).
    recursive subroutine compilation_filter(proc, string, status)
        type(handle_type), intent(in) :: proc
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, pos, min, max, mark
        type(value_type) :: inhibit
        type(status_type) :: st
        integer(int64) :: p, min_pos, max_pos
        status = status_type()
        buf = process_buffer(proc)
        if (.not. buffer_live_p(buf)) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        call symbol_value(sym("inhibit_read_only"), inhibit, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        ! `save-excursion' doesn't use the right insertion-type for us.
        call copy_marker_at(point(), pos, .true., st)
        call copy_marker_at(point_min(), min, .false., st)
        ! If we are inserting at the end of the accessible part of the
        ! buffer, keep the inserted text visible.
        call copy_marker_at(point_max(), max, .true., st)
        mark = process_mark(proc)
        call marker_position(mark, compilation_filter_start, st)
        call widen()
        call goto_char(compilation_filter_start)
        call insert(string, status)
        if (status%ok()) then
            if (.not. comint_inhibit_carriage_motion%value) then
                call marker_position(mark, p, st)
                call comint_carriage_motion(p, point())
            end if
            call set_marker(mark, point(), st)
            ! Update the number of errors in compilation-mode-line-errors.
            call compilation_ensure_parse(point())
            call compilation_filter_hook%run(status)
        end if
        call marker_position(pos, p, st)
        call goto_char(p)
        call marker_position(min, min_pos, st)
        call marker_position(max, max_pos, st)
        call narrow_to_region(min_pos, max_pos, st)
        call free_marker(pos, st)
        call free_marker(min, st)
        call free_marker(max, st)
        call set(sym("inhibit_read_only"), inhibit, st)
    end subroutine compilation_filter

    !> Sentinel for compilation buffers.  Port of
    !> compile.el:compilation-sentinel.
    recursive subroutine compilation_sentinel(proc, msg, status)
        type(handle_type), intent(in) :: proc
        character(len=*), intent(in) :: msg
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        type(status_type) :: st
        character(len=:), allocatable :: state
        integer :: k
        status = status_type()
        state = process_status(proc)
        if (state /= "exit" .and. state /= "signal") return
        buffer = process_buffer(proc)
        if (.not. buffer_live_p(buffer)) then
            ! Buffer killed.
            call set_process_buffer(proc, null_handle(), st)
        else
            old = current_buffer()
            call set_buffer(buffer, st)
            ! Write something in the compilation buffer and hack its mode
            ! line.
            call compilation_handle_exit(state, process_exit_status(proc), msg)
            if (buffer_live_p(old)) call set_buffer(old, st)
        end if
        if (allocated(compilation_in_progress)) then
            compilation_in_progress = pack(compilation_in_progress, &
                                           [(.not. same(compilation_in_progress(k), proc), &
                                             k=1, size(compilation_in_progress))])
        end if
        ! Since the buffer and mode line will show that the process is dead,
        ! we can delete it now.
        call delete_process(proc, st)
    end subroutine compilation_sentinel

    !> Write MSG in the current buffer and hack its `mode_line_process'.
    !> Port of compile.el:compilation-handle-exit.
    subroutine compilation_handle_exit(process_status_name, exit_status, msg)
        character(len=*), intent(in) :: process_status_name, msg
        integer, intent(in) :: exit_status
        type(value_type) :: inhibit
        type(status_type) :: st
        integer(int64) :: omax, opoint, now, rate
        integer :: k
        real :: elapsed
        character(len=32) :: duration
        character(len=:), allocatable :: line
        call symbol_value(sym("inhibit_read_only"), inhibit, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        omax = point_max()
        opoint = point()
        ! Record where we put the message, so we can ignore it later on.
        call goto_char(omax)
        call compilation_insert_annotation(NL//buffers(cur)%mode_name//" "//msg)
        if (bolp() .and. point() > point_min()) call goto_char(point() - 1)
        k = entry_of(current_buffer())
        call system_clock(now, rate)
        elapsed = 0
        if (k /= 0) elapsed = real(now - compilation_buffers(k)%start_count)/real(max(rate, 1_int64))
        if (elapsed < 10) then
            write (duration, "(f0.2, ' s')") elapsed
        else if (elapsed < 60) then
            write (duration, "(f0.1, ' s')") elapsed
        else
            write (duration, "(i0, ':', i2.2, ':', i2.2)") int(elapsed)/3600, mod(int(elapsed)/60, 60), mod(int(elapsed), 60)
        end if
        if (duration(1:1) == ".") duration = "0"//duration(1:len(duration) - 1)
        call compilation_insert_annotation(" at "//current_time_19()//", duration "//trim(duration))
        call goto_char(point_max())
        ! Prevent that message from being recognized as a compilation error.
        call put_text_property(omax, point(), sym("compilation_handle_exit"), t_value(), st)
        line = msg
        if (len(line) > 0) then
            if (line(len(line):len(line)) == NL) line = line(1:len(line) - 1)
        end if
        k = entry_of(current_buffer())
        if (k /= 0) then
            compilation_buffers(k)%mode_line = 2
            compilation_buffers(k)%exit_text = ":"//process_status_name//" ["//trim(code_string())//"]"
            if (exit_status > 0) then
                compilation_buffers(k)%exit_face = "compilation_mode_line_fail"
            else
                compilation_buffers(k)%exit_face = "compilation_mode_line_exit"
            end if
            call update_mode_line_process(k)
        end if
        call message(buffers(cur)%mode_name//" "//line)
        if (opoint < omax) call goto_char(opoint)
        call set(sym("inhibit_read_only"), inhibit, st)
    contains

        function code_string() result(c)
            character(len=16) :: c
            write (c, "(i0)") exit_status
        end function code_string
    end subroutine compilation_handle_exit

    !> Kill the process made by the \[compile] or \[grep] commands.  Port
    !> of compile.el:kill-compilation.
    recursive subroutine kill_compilation(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, proc, old
        type(status_type) :: st
        call compilation_find_buffer(buffer, status)
        if (.not. status%ok()) return
        proc = get_buffer_process(buffer)
        if (proc%index /= 0) then
            call interrupt_process(status, proc)
        else
            old = current_buffer()
            call set_buffer(buffer, st)
            call set_error(status, FM_GENERIC_ERROR, "The "//downcase_name(buffers(cur)%mode_name)// &
                           " process is not running")
            call set_buffer(old, st)
        end if
    end subroutine kill_compilation

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> The faces of compile.el.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR
        f => compilation_error
        call f%define("compilation_error", "Face used to highlight compiler errors.")
        call f%spec_entry(display_when(), face_attrs(inherit="error"))
        f => compilation_warning
        call f%define("compilation_warning", "Face used to highlight compiler warnings.")
        call f%spec_entry(display_when(), face_attrs(inherit="warning"))
        f => compilation_info
        call f%define("compilation_info", "Face used to highlight compiler information.")
        call f%spec_entry(display_when(), face_attrs(inherit="success"))
        f => compilation_mode_line_fail
        call f%define("compilation_mode_line_fail", "Face for Compilation mode's ""error"" mode line indicator.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="compilation_error"))
        call f%spec_entry(display_when(classes=[C], min_colors=16), face_attrs(foreground="Red1", weight="bold"))
        call f%spec_entry(display_when(classes=[C], min_colors=8), face_attrs(foreground="red"))
        call f%spec_entry(display_when(), face_attrs(inverse_video=.true., weight="bold"))
        f => compilation_mode_line_run
        call f%define("compilation_mode_line_run", "Face for Compilation mode's ""running"" mode line indicator.")
        call f%spec_entry(display_when(), face_attrs(inherit="compilation_warning"))
        f => compilation_mode_line_exit
        call f%define("compilation_mode_line_exit", "Face for Compilation mode's ""exit"" mode line indicator.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="compilation_info"))
        call f%spec_entry(display_when(classes=[C], min_colors=16), face_attrs(foreground="ForestGreen", weight="bold"))
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="green", weight="bold"))
        call f%spec_entry(display_when(), face_attrs(weight="bold"))
        f => compilation_line_number
        call f%define("compilation_line_number", "Face for displaying line numbers in compiler messages.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_keyword_face"))
        f => compilation_column_number
        call f%define("compilation_column_number", "Face for displaying column numbers in compiler messages.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_doc_face"))
    end subroutine define_faces

    !> Define Compilation mode and the minor modes, and register the
    !> commands.
    subroutine syms_of_compile()
        type(status_type) :: st
        call define_faces()
        call major_modes%compilation_mode%define("compilation_mode", "Compilation", compilation_mode, &
                                                 compilation_mode_hook)
        major_modes%compilation_mode%special = .true.
        compilation_mode_map = major_modes%compilation_mode%map
        call set_keymap_parent(compilation_mode_map, special_mode_map, st)
        call keymap_set(compilation_mode_map, "C-c C-c", "compile_goto_error", st)
        call keymap_set(compilation_mode_map, "RET", "compile_goto_error", st)
        call keymap_set(compilation_mode_map, "C-o", "compilation_display_error", st)
        call keymap_set(compilation_mode_map, "C-c C-k", "kill_compilation", st)
        call keymap_set(compilation_mode_map, "M-n", "compilation_next_error", st)
        call keymap_set(compilation_mode_map, "M-p", "compilation_previous_error", st)
        call keymap_set(compilation_mode_map, "M-{", "compilation_previous_file", st)
        call keymap_set(compilation_mode_map, "M-}", "compilation_next_file", st)
        call keymap_set(compilation_mode_map, "n", "next_error_no_select", st)
        call keymap_set(compilation_mode_map, "p", "previous_error_no_select", st)
        call keymap_set(compilation_mode_map, "TAB", "compilation_next_error", st)
        call keymap_set(compilation_mode_map, "<backtab>", "compilation_previous_error", st)
        call keymap_set(compilation_mode_map, "g", "recompile", st)
        call minor_modes%compilation_minor_mode%define("compilation_minor_mode", lighter=" Compilation", &
                                                       body=compilation_minor_mode_body, &
                                                       hook=compilation_minor_mode_hook, keymap=.true.)
        compilation_minor_mode_map = minor_modes%compilation_minor_mode%map
        call set_keymap_parent(compilation_minor_mode_map, special_mode_map, st)
        call keymap_set(compilation_minor_mode_map, "C-c C-c", "compile_goto_error", st)
        call keymap_set(compilation_minor_mode_map, "RET", "compile_goto_error", st)
        call keymap_set(compilation_minor_mode_map, "C-o", "compilation_display_error", st)
        call keymap_set(compilation_minor_mode_map, "C-c C-k", "kill_compilation", st)
        call keymap_set(compilation_minor_mode_map, "M-n", "compilation_next_error", st)
        call keymap_set(compilation_minor_mode_map, "M-p", "compilation_previous_error", st)
        call keymap_set(compilation_minor_mode_map, "M-{", "compilation_previous_file", st)
        call keymap_set(compilation_minor_mode_map, "M-}", "compilation_next_file", st)
        call keymap_set(compilation_minor_mode_map, "n", "next_error_no_select", st)
        call keymap_set(compilation_minor_mode_map, "p", "previous_error_no_select", st)
        call keymap_set(compilation_minor_mode_map, "g", "recompile", st)
        call minor_modes%compilation_shell_minor_mode%define("compilation_shell_minor_mode", lighter=" Shell-Compile", &
                                                             body=compilation_minor_mode_body, &
                                                             hook=compilation_shell_minor_mode_hook, keymap=.true.)
        compilation_shell_minor_mode_map = minor_modes%compilation_shell_minor_mode%map
        call keymap_set(compilation_shell_minor_mode_map, "C-M-m", "compile_goto_error", st)
        call keymap_set(compilation_shell_minor_mode_map, "C-M-n", "compilation_next_error", st)
        call keymap_set(compilation_shell_minor_mode_map, "C-M-p", "compilation_previous_error", st)
        call keymap_set(compilation_shell_minor_mode_map, "M-{", "compilation_previous_file", st)
        call keymap_set(compilation_shell_minor_mode_map, "M-}", "compilation_next_file", st)
        commands%compilation_mode%run => compilation_mode
        commands%compile%run => cmd_compile
        commands%recompile%run => cmd_recompile
        commands%kill_compilation%run => kill_compilation
        commands%compile_goto_error%run => compile_goto_error
        commands%compilation_next_error%run => cmd_compilation_next_error
        commands%compilation_previous_error%run => cmd_compilation_previous_error
        commands%compilation_next_file%run => cmd_compilation_next_file
        commands%compilation_previous_file%run => cmd_compilation_previous_file
        commands%compilation_display_error%run => compilation_display_error
        commands%compilation_set_skip_threshold%run => cmd_compilation_set_skip_threshold
        commands%compilation_next_error_function%run => cmd_compilation_next_error_function
        commands%compilation_minor_mode%run => compilation_minor_mode
        commands%compilation_shell_minor_mode%run => compilation_shell_minor_mode
        call add_local_variables_killer(kill_compilation_locals)
        call add_mode_line_modes_entry("compilation_in_progress", "[Compiling] ", compilation_in_progress_p)
        call forward_setting(compilation_error_screen_columns)
        call forward_setting(compilation_first_column)
        if (.not. allocated(compile_command%value)) compile_command = default_compile_command()
        if (.not. allocated(compilation_error_regexp_alist_alist)) call init_compilation_error_regexp_alist_alist()
        if (.not. allocated(compilation_error_regexp_alist)) call init_compilation_error_regexp_alist()
        if (.not. allocated(compilation_directory_matcher%regexp)) then
            compilation_directory_matcher%regexp = "\(?:Entering\|Leavin\(g\)\) directory [`']\(.+\)'$"
            compilation_directory_matcher%dir = [2]
            compilation_directory_matcher%leave = [1]
        end if
        if (.not. allocated(compilation_transform_file_match_alist)) then
            allocate (compilation_transform_file_match_alist(1))
            compilation_transform_file_match_alist(1)%regexp = "/bin/[a-z]*sh\'"
        end if
        if (.not. allocated(compilation_message_face%value)) compilation_message_face = "underline"
        if (.not. allocated(compilation_error_face%value)) compilation_error_face = "compilation_error"
        if (.not. allocated(compilation_warning_face%value)) compilation_warning_face = "compilation_warning"
        if (.not. allocated(compilation_info_face%value)) compilation_info_face = "compilation_info"
        if (.not. allocated(compilation_line_face%value)) compilation_line_face = "compilation_line_number"
        if (.not. allocated(compilation_column_face%value)) compilation_column_face = "compilation_column_number"
        if (.not. allocated(compilation_enter_directory_face%value)) then
            compilation_enter_directory_face = "font_lock_function_name_face"
        end if
        if (.not. allocated(compilation_leave_directory_face%value)) then
            compilation_leave_directory_face = "font_lock_builtin_face"
        end if
    end subroutine syms_of_compile
end module compile_fm
