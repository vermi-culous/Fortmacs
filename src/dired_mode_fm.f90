! dired_mode_fm.f90 -- Dired, the directory editor.
! Ported from GNU Emacs 31.1 lisp/dired.el.
!
! Copyright (C) 1985-1986, 1992-1997, 2000-2026 Free Software Foundation, Inc.
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

!> Dired: a buffer listing a directory as `ls -al' does, where files are
!> visited, marked, flagged and deleted.  `dired' (C-x d) and
!> `dired_other_window' (C-x 4 d) read the directory in the minibuffer;
!> `dired_noselect' makes the buffer; `dired_mode' is its major mode,
!> with `dired_mode_map', `dired_mode_hook', a read-only buffer and the
!> Dired faces through Font Lock keywords.  The listing comes from
!> `insert_directory' (files.el), which runs `insert_directory_program'
!> with `dired_listing_switches' and "--dired" so the file names are
!> known exactly.  Named `dired_mode_fm' because dired.c is `dired_fm'.
!> dired-aux.el (the operations on marked files) is in the submodule
!> dired_aux_fm; its options are declared here.
!>
!> Deferred: a list of files as the directory argument (a cons
!> `dired_directory'), inserted subdirectories (`i', `dired_insert_subdir',
!> `$' hiding, `dired_switches_alist', `dired_subdir_switches'),
!> `dired_hide_details_mode' and its options (the `invisible' properties
!> are set, but redisplay shows invisible text), `dired_find_alternate_file'
!> (`a') and `dired_kill_when_opening_new_dired_buffer', the dired-aux.el
!> commands listed as deferred in dired_aux_fm (`P', `!', `&', `X', `E',
!> `=', `A', `Q', `.'), `dired_toggle_read_only' (wdired; C-x C-q runs
!> `read_only_mode'), `w' with a prefix of 1 (project.el),
!> `dired_auto_revert_buffer' as a function, `dired_movement_style' t,
!> mouse, drag-and-drop and click-to-select, `dired_make_directory_clickable'
!> (keymaps in text properties), the warning about file names with
!> newlines and `dired_auto_toggle_b_switch', `dired_maybe_use_globstar',
!> `dired_filename_display_length', `mode_line_buffer_identification'
!> (%17b), `page_delimiter' and `list_buffers_directory' as buffer-local
!> values, `buffer_stale_function' (auto-revert), directory-local
!> variables, the desktop, isearch on file names, dired-x
!> (`dired_find_subdir', `dired_omit_mode', killing buffers of deleted
!> files), `combine_change_calls' in `dired_readin' (the change hooks run
!> for each change), trash (`delete_by_moving_to_trash'),
!> `dired_trivial_filenames' t, B and L (byte-compiling and loading Lisp:
!> not applicable).
module dired_mode_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_fm, only: status_type, set_error, format_error, format_message, condition_matches, error_name, &
                        FM_GENERIC_ERROR, FM_USER_ERROR, FM_QUIT
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, nilp, t_value, make_fixnum, make_string, list, list_elements, fixnump, &
                       xfixnum, consp, xstring, command_proc
    use lread_fm, only: sym, read_string_literal
    use data_fm, only: set, add_local_variables_killer, value_eq, kill_all_local_variables
    use commands_fm, only: commands
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, buffer_live_p, resolve_buffer, get_buffer, &
                         get_buffer_create, generate_new_buffer_name, kill_buffer, erase_buffer, buffer_modified_p, &
                         restore_buffer_modified_p, inhibit_read_only, get_file_buffer, buffer_handle, &
                         buf_fetch_char, set_buffer_read_only
    use marker_fm, only: make_marker, free_marker, set_marker, marker_position, marker_buffer, char_to_byte, &
                         set_marker_insertion_type
    use insdel_fm, only: insert, inhibit_modification_hooks
    use editfns_fm, only: point, point_min, point_max, goto_char, char_after, char_before, following_char, &
                          preceding_char, bobp, eobp, bolp, eolp, buffer_substring, delete_region, &
                          line_beginning_position, line_end_position, widen, subst_char_in_region, &
                          region_beginning, region_end, insert_char_n, narrow_to_region
    use cmds_fm, only: forward_line, beginning_of_line, end_of_line, forward_char, delete_char
    use search_fm, only: re_search_forward, re_search_backward, search_forward, looking_at, string_match, &
                         match_beginning, match_end, regexp_quote, replace_match, string_index_to_offset, &
                         case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
    use textprop_fm, only: get_text_property, put_text_property, add_text_properties, text_properties_at, &
                           set_text_properties, next_single_property_change, remove_text_properties
    use syntax_fm, only: skip_chars_forward
    use indent_fm, only: current_column, move_to_column, indent_rigidly, indent_tabs_mode
    use fileio_fm, only: expand_file_name, file_name_directory, file_name_nondirectory, file_name_as_directory, &
                         directory_file_name, file_name_absolute_p, file_exists_p, file_directory_p, &
                         file_readable_p, file_symlink_p, file_attributes, file_stat_type, insert_file_contents
    use files_fm, only: insert_directory, insert_directory_clean, insert_directory_wildcard_in_dir_p, &
                        insert_directory_program, abbreviate_file_name, create_file_buffer, find_file, &
                        find_file_other_window, find_file_noselect, file_relative_name, file_truename, &
                        delete_file, delete_directory, file_name_extension, auto_save_file_name_p, &
                        backup_file_name_p, byte_count_to_string, get_free_disk_space, revert_buffer, &
                        revert_buffer_function, find_directory_functions, find_file_run_dired, &
                        temporary_file_directory, dired_ls_error_buffer, dired_ls_error_file, make_directory, &
                        directory_listing_before_filename_regexp
    use callproc_fm, only: call_process, shell_file_name, init_callproc
    use dired_fm, only: directory_files, completion_ignored_extensions
    use window_fm, only: windows, window_leaves, window_live_p, pop_to_buffer_same_window, &
                         switch_to_buffer_other_window, display_buffer, fit_window_to_buffer, select_window, &
                         window_point, set_window_point, quit_restore_window, display_alist_type, &
                         temp_buffer_window_show, recenter, display_buffer_mark_dedicated, DEDICATED_WEAK, &
                         DISPLAY_ACTION_BELOW_SELECTED, BK_KILL, same_window
    use frame_fm, only: selected_frame, selected_window
    use keymap_fm, only: make_keymap, keymap_live_p, set_keymap_parent, define_key, keymap_set
    use keyboard_fm, only: read_event, last_command
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use xdisp_fm, only: message, cursor_in_echo_area
    use dispnew_fm, only: ding
    use simple_fm, only: line_number_at_pos, line_move, line_move_visual, goal_column, kill_new, kill_append, push_mark, &
                         region_active_p, undo, special_mode_map
    use fns_fm, only: yes_or_no_p
    use subr_fm, only: match_string, make_progress_reporter, progress_reporter_update, progress_reporter_done, &
                       progress_reporter_type
    use map_ynp_fm, only: read_answer, read_answer_type
    use minibuf_fm, only: read_string, minibuffer_string_type
    use minibuffer_fm, only: read_file_name, completion_insert_strings
    use replace_fm, only: read_regexp
    use help_fm, only: substitute_command_keys, register_keymap_name
    use print_fm, only: prin1_to_string
    use eval_fm, only: hook_type
    use derived_fm, only: derived_mode_p
    use modes_fm, only: major_modes
    use faces_fm, only: named_face_type, face_attrs, display_when, FACE_CLASS_COLOR
    use font_core_fm, only: font_lock_keyword_type, font_lock_highlight_type, font_lock_defaults, &
                            FONT_LOCK_OVERRIDE_NIL
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight, font_lock_anchored
    implicit none(type, external)
    private

    public :: syms_of_dired_mode, dired, dired_other_window, dired_noselect, dired_mode
    public :: dired_get_filename, dired_move_to_filename, dired_move_to_end_of_filename, dired_current_directory, &
              dired_get_marked_files, dired_goto_file, dired_revert, dired_build_subdir_alist, dired_get_subdir, &
              dired_buffers_for_dir, dired_file_name_at_point, dired_insert_directory, dired_readin, &
              dired_find_buffer_nocreate, dired_save_positions, dired_restore_positions, dired_positions_type, &
              dired_between_files, dired_next_line, dired_previous_line, dired_next_dirline, dired_prev_dirline, &
              dired_up_directory, dired_find_file, dired_unmark, dired_flag_file_deletion, &
              dired_toggle_marks, dired_mark_files_regexp, dired_mark_files_containing_regexp, &
              dired_do_flagged_delete, dired_do_delete, dired_delete_file, dired_sort_other, dired_sort_toggle, &
              dired_unmark_all_files, dired_change_marks, dired_string_type, dired_subdir_dirs, dired_directory_of, &
              dired_actual_switches_of, dired_check_switches, dired_switches_escape_p, dired_switches_recursive_p, &
              dired_glob_regexp, dired_make_relative, dired_remove_entry, dired_delete_entry, &
              dired_number_of_marked_files, dired_marker_prompt
    public :: dired_create_directory, dired_add_file, dired_add_entry, dired_do_redisplay, dired_update_file_line, &
              dired_goto_subdir, dired_kill_subdir, dired_mark_subdir_files, dired_after_subdir_garbage, &
              dired_prev_subdir
    ! Used by the dired_aux_fm submodule (gfortran drops private parent
    ! procedures that submodules call).
    public :: dired_goto_next_nontrivial_file, dired_map_over_marks, dired_mark_files_in_region, dired_next_subdir, &
              dired_subdir_max, dired_mark_1, dired_log, dired_log_summary, dired_in_this_tree_p, dired_normalize_subdir, &
              dired_get_file_for_visit, dired_advertise, temp_buffer_window_setup, insert_marked_files, fit_window
    public :: dired_do_chmod, dired_do_chgrp, dired_do_chown, dired_do_touch, dired_kill_line, dired_do_kill_lines, &
              dired_compress_file, dired_do_compress, dired_do_compress_to, dired_remove_file, dired_relist_file, &
              dired_query, dired_copy_file, dired_copy_file_recursive, dired_rename_file, dired_hardlink, &
              dired_make_relative_symlink, dired_create_files, dired_do_create_files, dired_do_copy, dired_do_symlink, &
              dired_do_relsymlink, dired_do_hardlink, dired_do_rename, dired_do_rename_regexp, dired_do_copy_regexp, &
              dired_do_hardlink_regexp, dired_do_symlink_regexp, dired_do_relsymlink_regexp, dired_upcase, &
              dired_downcase, dired_create_empty_file, dired_view_file, dired_show_file_type, dired_file_creator_proc, &
              dired_name_constructor_proc

    ! ------------------------------------------------------------------
    ! Options
    ! ------------------------------------------------------------------

    !> Switches passed to `ls' for Dired.  MUST contain the `l' option.
    !> May contain all other options that don't contradict `-l';
    !> may contain even `F', `b', `i' and `s'.  See also the variable
    !> `dired_ls_f_marks_symlinks' concerning the `F' switch.
    !>
    !> If you have files with names with embedded newline characters, adding
    !> `b' to the switches will allow Dired to handle those files better.
    !>
    !> Options that include embedded whitespace must be quoted
    !> like this: "--option=value with spaces".  Port of dired.el
    !> dired-listing-switches.
    character(len=:), allocatable, public :: dired_listing_switches

    !> Values of `dired_use_ls_dired' (upstream nil, t and `unspecified').
    integer, parameter, public :: DIRED_USE_LS_DIRED_NIL = 0, DIRED_USE_LS_DIRED_T = 1, &
                                  DIRED_USE_LS_DIRED_UNSPECIFIED = 2

    !> DIRED_USE_LS_DIRED_T means Dired should pass the "--dired" option to "ls".
    !> If DIRED_USE_LS_DIRED_NIL, don't pass "--dired" to "ls".
    !> The special value DIRED_USE_LS_DIRED_UNSPECIFIED means to check whether
    !> `insert_directory_program' supports the "--dired" option, and save
    !> the result in this variable.
    !> This is performed the first time `dired_insert_directory'
    !> invokes `insert_directory_program'.
    !>
    !> Note that if you set this option to DIRED_USE_LS_DIRED_NIL, either through choice or
    !> because your "ls" program does not support "--dired", Dired
    !> will fail to parse some "unusual" file names, e.g. those with leading
    !> spaces.  You might want to install ls from GNU Coreutils, which does
    !> support this option.  Port of dired.el dired-use-ls-dired.
    integer, public :: dired_use_ls_dired = DIRED_USE_LS_DIRED_UNSPECIFIED

    !> Informs Dired about how `ls -lF' marks symbolic links.
    !> Set this to true if `ls' (or whatever program is specified by
    !> `insert_directory_program') with `-lF' marks the symbolic link
    !> itself with a trailing @ (usually the case under Ultrix and macOS).
    !>
    !> Example: if `ln -s foo bar; ls -F bar' gives `bar -> foo', set it to
    !> false (the default), if it gives `bar@ -> foo', set it to true.
    !>
    !> Dired checks if there is really a @ appended.  Thus, if you have a
    !> marking `ls' program on one host and a non-marking on another host, and
    !> don't care about symbolic links which really end in a @, you can
    !> always set this variable to true.  Port of dired.el
    !> dired-ls-F-marks-symlinks.
    logical, public :: dired_ls_f_marks_symlinks = .false.

    !> Regexp of files to skip when finding first file of a directory.
    !> An unallocated value means move to the subdir line.  Port of
    !> dired.el dired-trivial-filenames.
    character(len=:), allocatable, public :: dired_trivial_filenames

    !> Values of `dired_free_space' (upstream nil, `separate' and `first').
    integer, parameter, public :: DIRED_FREE_SPACE_NIL = 0, DIRED_FREE_SPACE_SEPARATE = 1, &
                                  DIRED_FREE_SPACE_FIRST = 2

    !> Whether and how to display the disk space usage info in Dired buffers.
    !> If DIRED_FREE_SPACE_NIL, don't display.
    !> If DIRED_FREE_SPACE_SEPARATE, display on a separate line, and include both the used
    !> and the free disk space.
    !> If DIRED_FREE_SPACE_FIRST, the default, display only the free disk space on the first
    !> line, following the directory name.  Port of dired.el
    !> dired-free-space (with DIRED_FREE_SPACE_FIRST the space is a
    !> `display' property, which redisplay does not show yet).
    integer, public :: dired_free_space = DIRED_FREE_SPACE_FIRST

    !> Values of `dired_mark_region' (upstream nil, `file' and `line').
    integer, parameter, public :: DIRED_MARK_REGION_NIL = 0, DIRED_MARK_REGION_FILE = 1, DIRED_MARK_REGION_LINE = 2

    !> Defines what commands that mark files do with the active region.
    !>
    !> When DIRED_MARK_REGION_NIL, marking commands don't operate on all files in the
    !> active region.  They process their prefix arguments as usual.
    !>
    !> When the value of this option is another one, then all Dired commands
    !> that mark or unmark files will operate on all files in the region
    !> if the region is active in Transient Mark mode.
    !>
    !> When DIRED_MARK_REGION_FILE, the region marking is based on the file name.
    !> This means don't mark the file if the end of the region is
    !> before the file name displayed on the Dired line, so the file name
    !> is visually outside the region.  This behavior is consistent with
    !> marking files without the region using the key `m' that works on
    !> the file name where the cursor is.
    !>
    !> When DIRED_MARK_REGION_LINE, the region marking is based on Dired lines,
    !> so include the file into marking if the end of the region
    !> is anywhere on its Dired line, except the beginning of the line.
    !> Port of dired.el dired-mark-region.
    integer, public :: dired_mark_region = DIRED_MARK_REGION_FILE

    !> Values of `dired_movement_style' (upstream nil, `cycle', `bounded',
    !> `cycle-files' and `bounded-files').
    integer, parameter, public :: DIRED_MOVEMENT_STYLE_NIL = 0, DIRED_MOVEMENT_STYLE_CYCLE = 1, &
                                  DIRED_MOVEMENT_STYLE_BOUNDED = 2, DIRED_MOVEMENT_STYLE_CYCLE_FILES = 3, &
                                  DIRED_MOVEMENT_STYLE_BOUNDED_FILES = 4

    !> A value other than DIRED_MOVEMENT_STYLE_NIL means point skips empty
    !> lines when moving in Dired buffers.
    !> This affects only `dired_next_line', `dired_previous_line',
    !> `dired_next_dirline', `dired_prev_dirline'.
    !>
    !> Possible values other than DIRED_MOVEMENT_STYLE_NIL:
    !>  * DIRED_MOVEMENT_STYLE_CYCLE:  when moving from the last/first visible line, cycle back
    !>              to the first/last visible line.
    !>  * DIRED_MOVEMENT_STYLE_BOUNDED: don't move up/down if the current line is the
    !>               first/last visible line.
    !>  * DIRED_MOVEMENT_STYLE_CYCLE_FILES: like DIRED_MOVEMENT_STYLE_CYCLE
    !>    but moves only between file lines.
    !>  * DIRED_MOVEMENT_STYLE_BOUNDED_FILES: like DIRED_MOVEMENT_STYLE_BOUNDED
    !>    but moves only between file lines.
    !> Port of dired.el dired-movement-style.
    integer, public :: dired_movement_style = DIRED_MOVEMENT_STYLE_NIL

    !> Automatically revert Dired buffers on revisiting their directory.
    !> If true, revisiting an existing Dired buffer always reverts it.
    !> If false, revisiting a Dired buffer doesn't revert it, but displays
    !> a message telling the user how to revert it if the directory has
    !> changed on disk.  Port of dired.el dired-auto-revert-buffer (a
    !> predicate function as the value is deferred).
    logical, public :: dired_auto_revert_buffer = .false.

    !> Whether symlinks are checked for validity.
    !> Set it to false for remote directories, which suffer from a slow
    !> connection.  Port of dired.el dired-check-symlinks.
    logical, public :: dired_check_symlinks = .true.

    !> Values of `dired_recursive_deletes' (upstream nil, `always', `top'
    !> and t).
    integer, parameter, public :: DIRED_RECURSIVE_NEVER = 0, DIRED_RECURSIVE_ALWAYS = 1, DIRED_RECURSIVE_TOP = 2, &
                                  DIRED_RECURSIVE_ASK = 3

    !> Whether Dired deletes directories recursively.
    !> If DIRED_RECURSIVE_NEVER, Dired will not delete non-empty directories.
    !> DIRED_RECURSIVE_ALWAYS means to delete non-empty directories recursively,
    !> without asking.  This is dangerous!
    !> DIRED_RECURSIVE_TOP means to ask for each top-level directory specified by the
    !> Dired deletion command, and delete its subdirectories without
    !> asking.
    !> DIRED_RECURSIVE_ASK means to ask for each directory.  Port of
    !> dired.el dired-recursive-deletes.
    integer, public :: dired_recursive_deletes = DIRED_RECURSIVE_TOP

    !> Dired commands for which Dired should not popup list of affected
    !> files: `all', or the names of the commands in `commands'.
    type, public :: dired_no_confirm_type
        logical :: all = .false.
        character(len=16), allocatable :: commands(:)
    end type dired_no_confirm_type

    !> Dired commands for which Dired should not popup list of affected files.
    !>
    !> If set, Dired will not pop up the list of files to be affected by
    !> some Dired commands, when asking for confirmation.  (Dired will still
    !> ask for confirmation, just without showing the affected files.)
    !>
    !> If `all' is true, the list of affected files is never popped up.
    !> `commands' can also hold command names: then the list of the
    !> affected files will not be popped up only for the corresponding Dired
    !> commands.  Recognized command names are `byte_compile', `chgrp',
    !> `chmod', `chown', `compress', `copy', `delete', `hardlink', `load',
    !> `move', `print', `shell', `symlink', `touch' and `uncompress'.  Port of
    !> dired.el dired-no-confirm.
    type(dired_no_confirm_type), public :: dired_no_confirm

    !> Values of `dired_switches_in_mode_line' other than a number of
    !> characters (upstream nil and `as-is').
    integer, parameter, public :: DIRED_SWITCHES_SORT_ORDER = -1, DIRED_SWITCHES_AS_IS = -2

    !> How to indicate `dired_actual_switches' in mode-line.
    !> Possible values:
    !>  * DIRED_SWITCHES_SORT_ORDER: Indicate name-or-date sort order, if
    !>    possible.  Else show full switches.
    !>  * DIRED_SWITCHES_AS_IS:  Show full switches.
    !>  * N >= 0:  Show only the first N chars of full switches.
    !> Port of dired.el dired-switches-in-mode-line (a function as the
    !> value is deferred).
    integer, public :: dired_switches_in_mode_line = DIRED_SWITCHES_SORT_ORDER

    !> True means the Dired sort command is disabled.
    !> The idea is to set this buffer-locally in special Dired buffers.
    !> Port of dired.el dired-sort-inhibit (one global value).
    logical, public :: dired_sort_inhibit = .false.

    !> Regular expression to match "garbage" files for
    !> `dired_flag_garbage_files'.  Port of dired.el
    !> dired-garbage-files-regexp.
    character(len=:), allocatable, public :: dired_garbage_files_regexp

    !> String of `ls' switches (single letters) except "t" that influence
    !> sorting.
    !>
    !> This indicates to Dired which option switches to watch out for because they
    !> will change the sorting order behavior of `ls'.
    !>
    !> To change the default sorting order (e.g. add a `-v' option), see the
    !> variable `dired_listing_switches'.  To temporarily override the listing
    !> format, use `\[universal_argument] \[dired]'.  Port of dired.el
    !> dired-ls-sorting-switches.
    character(len=*), parameter, public :: dired_ls_sorting_switches = "SXU"

    !> True means that Dired, when looking for files containing a
    !> regexp, reads them from the file system even if a buffer visits
    !> them.  Port of dired.el dired-always-read-filesystem.
    logical, public :: dired_always_read_filesystem = .false.

    !> In Dired, the current mark character.
    !> This is what the do-commands look for, and what the mark-commands
    !> store.  Port of dired.el dired-marker-char.
    character, public :: dired_marker_char = "*"

    !> Character used to flag files for deletion.  Port of dired.el
    !> dired-del-marker.
    character, public :: dired_del_marker = "D"

    !> The buffer where Dired logs its errors.  Port of dired.el
    !> dired-log-buffer.
    character(len=*), parameter, public :: dired_log_buffer = "*Dired log*"

    !> Name of chown command (usually `chown').  Port of dired.el
    !> dired-chown-program.
    character(len=:), allocatable, public :: dired_chown_program

    !> Name of touch command (usually `touch').  Port of dired.el
    !> dired-touch-program.
    character(len=:), allocatable, public :: dired_touch_program

    !> The value of the `dired_keep_marker_*' options that keeps the
    !> marks (upstream t).
    character, parameter, public :: DIRED_KEEP_MARKER = achar(0)

    !> Controls marking of renamed files.
    !> If DIRED_KEEP_MARKER, files keep their previous marks when they are renamed.
    !> If a character, renamed files (whether previously marked or not)
    !> are afterward marked with that character.
    !> This option affects only files renamed by `dired_do_rename' and
    !> `dired_do_rename_regexp'.  Port of dired.el dired-keep-marker-rename.
    character, public :: dired_keep_marker_rename = DIRED_KEEP_MARKER

    !> Controls marking of copied files.
    !> If DIRED_KEEP_MARKER, copied files are marked if and as the corresponding original files were.
    !> If a character, copied files are unconditionally marked with that character.
    !> Port of dired.el dired-keep-marker-copy.
    character, public :: dired_keep_marker_copy = "C"

    !> Controls marking of newly made hard links.
    !> If DIRED_KEEP_MARKER, they are marked if and as the files linked to were marked.
    !> If a character, new links are unconditionally marked with that character.
    !> Port of dired.el dired-keep-marker-hardlink.
    character, public :: dired_keep_marker_hardlink = "H"

    !> Controls marking of newly made symbolic links.
    !> If DIRED_KEEP_MARKER, they are marked if and as the files linked to were marked.
    !> If a character, new links are unconditionally marked with that character.
    !> Port of dired.el dired-keep-marker-symlink.
    character, public :: dired_keep_marker_symlink = "Y"

    !> Controls marking of newly made relative symbolic links.
    !> If DIRED_KEEP_MARKER, they are marked if and as the files linked to were marked.
    !> If a character, new links are unconditionally marked with that character.
    !> Port of dired.el dired-keep-marker-relsymlink.
    character, public :: dired_keep_marker_relsymlink = "S"

    !> If true, Dired preserves the last-modified time in a file copy.
    !> (This works on only some systems.)  Port of dired.el
    !> dired-copy-preserve-time.
    logical, public :: dired_copy_preserve_time = .true.

    !> If true, Dired dereferences symlinks when copying them.
    !> This is similar to the "-L" option for the "cp" shell command.
    !> Port of dired.el dired-copy-dereference.
    logical, public :: dired_copy_dereference = .false.

    !> Whether Dired copies directories recursively.
    !> If DIRED_RECURSIVE_NEVER, never copy recursively.
    !> DIRED_RECURSIVE_ALWAYS means to copy recursively without asking.
    !> DIRED_RECURSIVE_TOP means to ask for each directory at top level.
    !> DIRED_RECURSIVE_ASK means to ask for each directory.  Port of
    !> dired.el dired-recursive-copies.
    integer, public :: dired_recursive_copies = DIRED_RECURSIVE_TOP

    !> Values of `dired_dwim_target' (upstream nil, t or
    !> dired-dwim-target-next, dired-dwim-target-next-visible and
    !> dired-dwim-target-recent).
    integer, parameter, public :: DIRED_DWIM_TARGET_NIL = 0, DIRED_DWIM_TARGET_NEXT = 1, &
                                  DIRED_DWIM_TARGET_NEXT_VISIBLE = 2, DIRED_DWIM_TARGET_RECENT = 3

    !> If set, Dired tries to guess a default target directory.
    !> This means: if there is a Dired buffer displayed in some window,
    !> use its current directory, instead of this Dired buffer's
    !> current directory.
    !>
    !> You can set it to prefer either the next window with a Dired buffer
    !> (DIRED_DWIM_TARGET_NEXT, or DIRED_DWIM_TARGET_NEXT_VISIBLE on
    !> visible frames), or the most recently used window with a Dired
    !> buffer (DIRED_DWIM_TARGET_RECENT).
    !>
    !> The target is used in the prompt for file copy, rename etc.  Port
    !> of dired.el dired-dwim-target (a function as the value is
    !> deferred).
    integer, public :: dired_dwim_target = DIRED_DWIM_TARGET_NIL

    !> Values of `dired_create_destination_dirs' (upstream nil, `always'
    !> and `ask').
    integer, parameter, public :: DIRED_CREATE_DESTINATION_DIRS_NIL = 0, DIRED_CREATE_DESTINATION_DIRS_ALWAYS = 1, &
                                  DIRED_CREATE_DESTINATION_DIRS_ASK = 2

    !> Whether Dired should create destination dirs when copying/moving files.
    !> If DIRED_CREATE_DESTINATION_DIRS_NIL, don't create non-existent destination directories.
    !> If DIRED_CREATE_DESTINATION_DIRS_ASK, ask the user whether to create them.
    !> If DIRED_CREATE_DESTINATION_DIRS_ALWAYS, create them without asking.
    !>
    !> Also see `dired_create_destination_dirs_on_trailing_dirsep'.  Port
    !> of dired-aux.el dired-create-destination-dirs.
    integer, public :: dired_create_destination_dirs = DIRED_CREATE_DESTINATION_DIRS_NIL

    !> If true, consider a file name ending in a slash as a directory to create.
    !>
    !> If this variable is true and a single destination file name of
    !> a Dired command ends in a directory separator (/), that file name
    !> will be treated as a non-existent directory, and that directory
    !> will be created if the variable `dired_create_destination_dirs'
    !> says so.
    !>
    !> For example, if both `dired_create_destination_dirs' and this
    !> option are set, renaming a directory named `old_name' to
    !> `new_name/' (note the trailing directory separator) will
    !> create `new_name' if it does not already exist, and will
    !> move `old_name' into it.  By contrast, if only `new_name'
    !> (without the trailing /) is given, or this option or
    !> `dired_create_destination_dirs' is not set, `old_name' will be
    !> renamed to `new_name'.  Port of dired-aux.el
    !> dired-create-destination-dirs-on-trailing-dirsep.
    logical, public :: dired_create_destination_dirs_on_trailing_dirsep = .false.

    !> Automatically revert Dired buffers after `dired_do' operations.
    !> This option controls whether to refresh the directory listing in a
    !> Dired buffer that is the destination of one of these operations:
    !> `dired_do_copy', `dired_do_rename', `dired_do_symlink', `dired_do_hardlink'.
    !> If the value is true, always revert the Dired buffer updated in the result
    !> of these operations.  Port of dired-aux.el dired-do-revert-buffer
    !> (a function as the value is deferred).
    logical, public :: dired_do_revert_buffer = .false.

    !> Whether `dired_create_empty_file' acts on the current directory.
    !> If true, `dired_create_empty_file' creates a new empty file and adds
    !> an entry for it (or its topmost new parent directory if created) under
    !> the current subdirectory in the Dired buffer by default (otherwise, it
    !> adds the new file (and new subdirectories if provided) to whichever
    !> directory the user enters at the prompt).  If false,
    !> `dired_create_empty_file' acts on the default directory by default.
    !> Port of dired-aux.el dired-create-empty-file-in-current-directory.
    logical, public :: dired_create_empty_file_in_current_directory = .false.

    !> Default suffix for compressing a single file.
    !> If empty, ".gz" will be used.  Port of dired-aux.el
    !> dired-compress-file-default-suffix.
    character(len=:), allocatable, public :: dired_compress_file_default_suffix

    !> Default suffix for compressing a directory.
    !> If empty, ".tar.gz" will be used.  Port of dired-aux.el
    !> dired-compress-directory-default-suffix.
    character(len=:), allocatable, public :: dired_compress_directory_default_suffix

    !> A rule of `dired_compress_file_suffixes': (REGEXP NEW-SUFFIX
    !> PROGRAM), PROGRAM empty for nil.
    type, public :: dired_compress_suffix_type
        character(len=:), allocatable :: regexp, new_suffix, program
    end type dired_compress_suffix_type

    !> A rule (REGEXP . CMD) of `dired_compress_file_alist' and
    !> `dired_compress_files_alist'.
    type, public :: dired_compress_rule_type
        character(len=:), allocatable :: regexp, command
    end type dired_compress_rule_type

    !> Control changes in file name suffixes for compression and uncompression.
    !> Each element specifies one transformation rule, and has the form:
    !>   (REGEXP NEW-SUFFIX PROGRAM)
    !> The rule applies when the old file name matches REGEXP.
    !> The new file name is computed by deleting the part that matches REGEXP
    !>  (as well as anything after that), then adding NEW-SUFFIX in its place.
    !> If PROGRAM is not empty, the rule is an uncompression rule,
    !> and uncompression is done by running PROGRAM.
    !>
    !> Within PROGRAM, %i denotes the input file, and %o denotes the
    !> output file.
    !>
    !> Otherwise, the rule is a compression rule, and compression is done with gzip.
    !> ARGS are command switches passed to PROGRAM.  Port of dired-aux.el
    !> dired-compress-file-suffixes.
    type(dired_compress_suffix_type), allocatable, public :: dired_compress_file_suffixes(:)

    !> Controls the compression shell command for `dired_do_compress_to'.
    !>
    !> Each element is (REGEXP . CMD), where REGEXP is the name of the
    !> archive to which you want to compress, and CMD is the
    !> corresponding command.
    !>
    !> Within CMD, %i denotes the input file(s), and %o denotes the
    !> output file.  %i path(s) are relative, while %o is absolute.  Port
    !> of dired-aux.el dired-compress-file-alist.
    type(dired_compress_rule_type), allocatable, public :: dired_compress_file_alist(:)

    !> Controls the compression shell command for `dired_do_compress_to'.
    !>
    !> Each element is (REGEXP . CMD), where REGEXP is the name of the
    !> archive to which you want to compress, and CMD is the
    !> corresponding command.
    !>
    !> Within CMD, %i denotes the input file(s), and %o denotes the
    !> output file.  %i path(s) are relative, while %o is absolute.  Port
    !> of dired-aux.el dired-compress-files-alist.
    type(dired_compress_rule_type), allocatable, public :: dired_compress_files_alist(:)

    !> What a file creator of `dired_create_files' is, for its special
    !> cases: `dired_copy_file', `make_symbolic_link' or another.
    integer, parameter, public :: DIRED_CREATOR_OTHER = 0, DIRED_CREATOR_COPY = 1, DIRED_CREATOR_SYMLINK = 2

    abstract interface
        !> A FILE-CREATOR of `dired_create_files': create TO from FROM,
        !> overwriting TO if OK_FLAG.
        recursive subroutine dired_file_creator_proc(from, to, ok_flag, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: from, to
            logical, intent(in) :: ok_flag
            type(status_type), intent(out) :: status
        end subroutine dired_file_creator_proc

        !> A NAME-CONSTRUCTOR of `dired_create_files': the new file name TO
        !> for FROM, or SKIP true to skip it (upstream nil).
        recursive subroutine dired_name_constructor_proc(from, to, skip, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: from
            character(len=:), allocatable, intent(out) :: to
            logical, intent(out) :: skip
            type(status_type), intent(out) :: status
        end subroutine dired_name_constructor_proc
    end interface

    abstract interface
        !> A `dired_deletion_confirmer': ask PROMPT, ANSWER true for yes.
        recursive subroutine dired_confirmer_proc(prompt, answer, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            logical, intent(out) :: answer
            type(status_type), intent(out) :: status
        end subroutine dired_confirmer_proc
    end interface

    !> How Dired asks whether to delete the files: `yes_or_no_p' (or
    !> y_or_n_p).  Port of dired.el dired-deletion-confirmer.
    procedure(dired_confirmer_proc), pointer, public :: dired_deletion_confirmer => null()

    ! ------------------------------------------------------------------
    ! Hooks
    ! ------------------------------------------------------------------

    !> Run at the very end of `dired_mode'.  Port of dired.el
    !> dired-mode-hook.
    type(hook_type), target, public :: dired_mode_hook = hook_type(name="dired_mode_hook")

    !> Event Hook run before reading in the directory.  Port of dired.el
    !> dired-before-readin-hook.
    type(hook_type), target, public :: dired_before_readin_hook = hook_type(name="dired_before_readin_hook")

    !> Hook run after each time a file or directory is read by Dired.
    !> After each listing of a file or directory, this hook is run
    !> with the buffer narrowed to the listing.  Port of dired.el
    !> dired-after-readin-hook.
    type(hook_type), target, public :: dired_after_readin_hook = hook_type(name="dired_after_readin_hook")

    !> This hook is used to position the point.
    !> It is run by the function `dired_initial_position'.  Port of
    !> dired.el dired-initial-position-hook.
    type(hook_type), target, public :: dired_initial_position_hook = hook_type(name="dired_initial_position_hook")

    !> Local keymap for Dired mode buffers.  Port of dired.el
    !> dired-mode-map.
    type(handle_type), public :: dired_mode_map

    ! ------------------------------------------------------------------
    ! Faces
    ! ------------------------------------------------------------------

    !> Face used for directory headers.  Port of dired.el dired-header.
    type(named_face_type), target, public :: dired_header
    !> Face used for Dired marks.  Port of dired.el dired-mark.
    type(named_face_type), target, public :: dired_mark
    !> Face used for marked files.  Port of dired.el dired-marked.
    type(named_face_type), target, public :: dired_marked
    !> Face used for files flagged for deletion.  Port of dired.el
    !> dired-flagged.
    type(named_face_type), target, public :: dired_flagged
    !> Face used to highlight a part of a buffer that needs user attention.
    !> Port of dired.el dired-warning.
    type(named_face_type), target, public :: dired_warning
    !> Face used to highlight permissions of group- and world-writable files.
    !> Port of dired.el dired-perm-write.
    type(named_face_type), target, public :: dired_perm_write
    !> Face used to highlight permissions of suid and guid files.  Port of
    !> dired.el dired-set-id.
    type(named_face_type), target, public :: dired_set_id
    !> Face used for subdirectories.  Port of dired.el dired-directory.
    type(named_face_type), target, public :: dired_directory
    !> Face used for symbolic links.  Port of dired.el dired-symlink.
    type(named_face_type), target, public :: dired_symlink
    !> Face used for broken symbolic links.  Port of dired.el
    !> dired-broken-symlink.
    type(named_face_type), target, public :: dired_broken_symlink
    !> Face used for sockets, pipes, block devices and char devices.  Port
    !> of dired.el dired-special.
    type(named_face_type), target, public :: dired_special
    !> Face used for files suffixed with `completion_ignored_extensions'.
    !> Port of dired.el dired-ignored.
    type(named_face_type), target, public :: dired_ignored

    !> Face name used for directory headers.  Port of dired.el
    !> dired-header-face.
    character(len=:), allocatable, public :: dired_header_face
    !> Face name used for Dired marks.  Port of dired.el dired-mark-face.
    character(len=:), allocatable, public :: dired_mark_face
    !> Face name used for marked files.  Port of dired.el
    !> dired-marked-face.
    character(len=:), allocatable, public :: dired_marked_face
    !> Face name used for files flagged for deletion.  Port of dired.el
    !> dired-flagged-face.
    character(len=:), allocatable, public :: dired_flagged_face
    !> Face name used for a part of a buffer that needs user attention.
    !> Port of dired.el dired-warning-face.
    character(len=:), allocatable, public :: dired_warning_face
    !> Face name used for permissions of group- and world-writable files.
    !> Port of dired.el dired-perm-write-face.
    character(len=:), allocatable, public :: dired_perm_write_face
    !> Face name used for subdirectories.  Port of dired.el
    !> dired-directory-face.
    character(len=:), allocatable, public :: dired_directory_face
    !> Face name used for symbolic links.  Port of dired.el
    !> dired-symlink-face.
    character(len=:), allocatable, public :: dired_symlink_face
    !> Face name used for files suffixed with `completion_ignored_extensions'.
    !> Port of dired.el dired-ignored-face.
    character(len=:), allocatable, public :: dired_ignored_face

    ! ------------------------------------------------------------------
    ! Internal state
    ! ------------------------------------------------------------------

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    !> Regexp for optional initial inode and file size as made by `ls -i -s'.
    character(len=*), parameter :: dired_re_inode_size = "[0-9 "//TAB//"]*[.,0-9]*[BkKMGTPEZYRQ]?[ "//TAB//"]*"
    !> Regexp matching a marked line.  The match ends just after the marker.
    character(len=*), parameter :: dired_re_mark = "^[^ "//LF//"]"
    character(len=*), parameter :: dired_re_maybe_mark = "^. "
    character(len=*), parameter :: dired_re_dir = dired_re_maybe_mark//dired_re_inode_size//"d[^:]"
    character(len=*), parameter :: dired_re_sym = dired_re_maybe_mark//dired_re_inode_size//"l[^:]"
    character(len=*), parameter :: dired_re_special = dired_re_maybe_mark//dired_re_inode_size//"[bcsp][^:]"
    !> Match ls permission string of an executable file.
    character(len=*), parameter :: dired_re_exe = dired_re_maybe_mark//dired_re_inode_size// &
                                   "-[-r][-w][xs][-r][-w].[-r][-w].\|"// &
                                   dired_re_maybe_mark//dired_re_inode_size// &
                                   "-[-r][-w].[-r][-w][xs][-r][-w].\|"// &
                                   dired_re_maybe_mark//dired_re_inode_size// &
                                   "-[-r][-w].[-r][-w].[-r][-w][xst]"
    character(len=*), parameter :: dired_re_perms = "[-bcdlps][-r][-w].[-r][-w].[-r][-w]."
    character(len=*), parameter :: dired_re_dot = "^.* \.\.?/?$"
    !> Regexp matching a maybe hidden subdirectory line in `ls -lR' output.
    character(len=*), parameter :: dired_subdir_regexp = "^. \(.+\)\(:\)"//LF
    !> Regular expression to match the permission flags in `ls -l'.
    character(len=*), parameter :: dired_permission_flags_regexp = &
                                   "\([^ ]\)[-r][-w]\([^ ]\)[-r][-w]\([^ ]\)[-r][-w]\([^ ]\)"
    !> Regexp recognized by Dired to set `by date' mode.
    character(len=*), parameter :: dired_sort_by_date_regexp = &
                                   "\(\`\| \)-[^- ]*t[^ "//dired_ls_sorting_switches//"]*\(\(\`\| +\)\(--[^ ]+\|-[^- t"// &
                                   dired_ls_sorting_switches//"]+\|""[^""]*""\)\)* *$"
    !> Regexp recognized by Dired to set `by name' mode.
    character(len=*), parameter :: dired_sort_by_name_regexp = &
                                   "\`\(\(\`\| +\)\(--[^ ]+\|-[^- t"//dired_ls_sorting_switches// &
                                   "]+\|""[^""]*""\)\)* *$"

    !> Values of the LOCALP argument of `dired_get_filename' (upstream nil,
    !> t, `no-dir' and `verbatim').
    integer, parameter, public :: DIRED_LOCALP_NIL = 0, DIRED_LOCALP_T = 1, DIRED_LOCALP_NO_DIR = 2, &
                                  DIRED_LOCALP_VERBATIM = 3

    !> Kinds of the ARG of `dired_map_over_marks' and the commands on
    !> marked files: DIRED_ARG_NIL (the marked files), DIRED_ARG_INTEGER
    !> with a number N (the next N files), DIRED_ARG_MARKED (only marked
    !> files) or DIRED_ARG_OTHER (the current file).
    integer, parameter, public :: DIRED_ARG_NIL = 0, DIRED_ARG_INTEGER = 1, DIRED_ARG_MARKED = 2, DIRED_ARG_OTHER = 3

    !> A string (a file name).
    type, public :: dired_string_type
        character(len=:), allocatable :: s
    end type dired_string_type

    !> One element of `dired_subdir_alist': a listed directory and a
    !> marker at the start of its header line.
    type :: dired_subdir_type
        character(len=:), allocatable :: dir
        type(handle_type) :: marker
    end type dired_subdir_type

    !> A Dired buffer's own variables (buffer-local upstream): by buffer
    !> serial number.
    type :: dired_local_type
        integer(int64) :: serial = 0
        logical :: set = .false.
        !> dired-directory
        character(len=:), allocatable :: directory
        !> dired-actual-switches
        character(len=:), allocatable :: actual_switches
        !> dired-subdir-alist, its first element first (the last
        !> directory in the buffer)
        type(dired_subdir_type), allocatable :: subdir_alist(:)
        !> dired-subdir-alist-pre-R (directory names; its markers are
        !> never used)
        logical :: has_pre_r = .false.
        type(dired_string_type), allocatable :: subdir_alist_pre_r(:)
    end type dired_local_type

    type(dired_local_type), allocatable :: locals(:)

    !> One element of `dired_buffers'.
    type :: dired_buffer_entry_type
        character(len=:), allocatable :: dir
        type(handle_type) :: buffer
    end type dired_buffer_entry_type

    !> Alist of expanded directories and their associated Dired buffers
    !> (the first is the most recent).  Port of dired.el dired-buffers.
    type(dired_buffer_entry_type), allocatable :: dired_buffers(:)

    !> A position saved by `dired_save_positions': a window (or a marker
    !> for a window's previous buffer), the file name there (if any) and
    !> the line number.
    type :: dired_saved_position_type
        type(handle_type) :: window, marker
        logical :: has_file = .false.
        character(len=:), allocatable :: file
        integer(int64) :: line = 1
    end type dired_saved_position_type

    !> What `dired_save_positions' returns: the buffer's position, those of
    !> the windows showing it, and those it has in windows' previous buffers.
    type :: dired_positions_type
        type(handle_type) :: buffer
        type(dired_saved_position_type) :: here
        type(dired_saved_position_type), allocatable :: windows(:), prevs(:)
    end type dired_positions_type

    !> Results collected by the bodies of `dired_map_over_marks'.
    type(dired_string_type), allocatable :: map_files(:)
    logical, allocatable :: map_found(:)
    type(handle_type), allocatable :: map_markers(:)
    integer :: map_n = 0
    integer :: map_localp = DIRED_LOCALP_NIL
    logical :: map_no_error = .false.

    !> The regexp `dired_mark_files_regexp' and its siblings test.
    character(len=:), allocatable :: mark_regexp

    !> Arguments of the body of `dired_mark' (dired_repeat_over_lines).
    character :: repeat_char

    !> The files `dired_mark_pop_up' shows.
    type(dired_string_type), allocatable :: pop_up_files(:)

    ! dired-aux.el, in the submodule dired_aux_fm.
    interface
        !> Create a directory called DIRECTORY.
        !> Parent directories of DIRECTORY are created as needed.
        !> If DIRECTORY already exists, signal an error.  Port of
        !> dired-aux.el:dired-create-directory.  (In dired_aux_fm.)
        recursive module subroutine dired_create_directory(directory, status)
            implicit none(type, external)
            character(len=*), intent(in) :: directory
            type(status_type), intent(out) :: status
        end subroutine dired_create_directory

        !> Add an entry for FILENAME in every Dired buffer listing its
        !> directory, marked with MARKER_CHAR if present.  Port of
        !> dired-aux.el:dired-add-file.  (In dired_aux_fm.)
        recursive module subroutine dired_add_file(filename, status, marker_char)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            type(status_type), intent(out) :: status
            character, intent(in), optional :: marker_char
        end subroutine dired_add_file

        !> Add a new Dired entry for FILENAME.
        !> Optionally mark it with MARKER_CHAR.  Note that this adds the
        !> entry `out of order' if files are sorted by time, etc.
        !> Skips files that match `dired_trivial_filenames'.
        !> Exposes hidden subdirectories if a file is added there.
        !> ADDED is true on success.  Port of dired-aux.el:dired-add-entry
        !> (no dired-x omitting).  (In dired_aux_fm.)
        recursive module subroutine dired_add_entry(filename, added, status, marker_char, relative)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            logical, intent(out) :: added
            type(status_type), intent(out) :: status
            character, intent(in), optional :: marker_char
            logical, intent(in), optional :: relative
        end subroutine dired_add_entry

        !> Redisplay all marked (or next ARG) files.
        !> If on a subdir line, redisplay that subdirectory (not ported
        !> yet: an error).  Port of dired-aux.el:dired-do-redisplay.  (In
        !> dired_aux_fm.)
        recursive module subroutine dired_do_redisplay(arg_kind, arg_n, test_for_subdir, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            logical, intent(in) :: test_for_subdir
            type(status_type), intent(out) :: status
        end subroutine dired_do_redisplay

        !> Delete the current line, and insert an entry for FILE (none if
        !> FILE is absent), keeping the mark in its first column.  Port
        !> of dired-aux.el:dired-update-file-line.  (In dired_aux_fm.)
        recursive module subroutine dired_update_file_line(status, file)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: file
        end subroutine dired_update_file_line

        !> Go to end of header line of inserted directory DIR in this Dired
        !> buffer.  Return value of point on success, otherwise 0.  The
        !> next char is \n.  Port of dired-aux.el:dired-goto-subdir.  (In
        !> dired_aux_fm.)
        module function dired_goto_subdir(dir) result(pos)
            implicit none(type, external)
            character(len=*), intent(in) :: dir
            integer(int64) :: pos
        end function dired_goto_subdir

        !> Remove all lines of current subdirectory.
        !> Lower levels are unaffected.  Port of
        !> dired-aux.el:dired-kill-subdir (no REMEMBER-MARKS).  (In
        !> dired_aux_fm.)
        recursive module subroutine dired_kill_subdir(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine dired_kill_subdir

        !> Mark all files except `.' and `..' in current subdirectory.
        !> If the Dired buffer shows multiple directories, this command
        !> marks the files listed in the subdirectory that point is in.
        !> Port of dired-aux.el:dired-mark-subdir-files.  (In dired_aux_fm.)
        recursive module subroutine dired_mark_subdir_files(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine dired_mark_subdir_files

        !> Return pos of first file line of DIR, skipping header and total
        !> or wildcard lines.  Port of
        !> dired-aux.el:dired-after-subdir-garbage.  (In dired_aux_fm.)
        module function dired_after_subdir_garbage(dir, status) result(pos)
            implicit none(type, external)
            character(len=*), intent(in) :: dir
            type(status_type), intent(out) :: status
            integer(int64) :: pos
        end function dired_after_subdir_garbage

        !> Go to previous subdirectory, regardless of level.  Return the
        !> position or 0.  Port of dired-aux.el:dired-prev-subdir.  (In
        !> dired_aux_fm.)
        module function dired_prev_subdir(arg, status, no_error_if_not_found, no_skip) result(pos)
            implicit none(type, external)
            integer(int64), intent(in) :: arg
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: no_error_if_not_found, no_skip
            integer(int64) :: pos
        end function dired_prev_subdir

        !> Change the mode of the marked (or next ARG) files.
        !> Both octal numeric modes like `644' and symbolic modes like `g+w'
        !> are supported.  Port of dired-aux.el:dired-do-chmod.  (In
        !> dired_aux_fm.)
        recursive module subroutine dired_do_chmod(arg_kind, arg_n, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_chmod

        !> Change the group of the marked (or next ARG) files.  Port of
        !> dired-aux.el:dired-do-chgrp.  (In dired_aux_fm.)
        recursive module subroutine dired_do_chgrp(arg_kind, arg_n, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_chgrp

        !> Change the owner of the marked (or next ARG) files.  Port of
        !> dired-aux.el:dired-do-chown.  (In dired_aux_fm.)
        recursive module subroutine dired_do_chown(arg_kind, arg_n, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_chown

        !> Change the timestamp of the marked (or next ARG) files.
        !> This calls touch.  Port of dired-aux.el:dired-do-touch.  (In
        !> dired_aux_fm.)
        recursive module subroutine dired_do_touch(arg_kind, arg_n, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_touch

        !> Kill the current line (not the files).
        !> Kill ARG lines starting with the current line.
        !> (A negative argument kills backward.)  Port of
        !> dired-aux.el:dired-kill-line.  (In dired_aux_fm.)
        recursive module subroutine dired_kill_line(arg, status)
            implicit none(type, external)
            integer(int64), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine dired_kill_line

        !> Remove all marked lines, or the next ARG lines.
        !> The files or directories on those lines are _not_ deleted.  Only the
        !> Dired listing is affected.
        !>
        !> INIT_COUNT is added to the number of lines removed by this
        !> invocation, for the reporting message; COUNT is that number.
        !>
        !> A FMT of "" will suppress the messaging.  Port of
        !> dired-aux.el:dired-do-kill-lines.  (In dired_aux_fm.)
        recursive module subroutine dired_do_kill_lines(arg_kind, arg_n, status, fmt, init_count, count)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: fmt
            integer(int64), intent(in), optional :: init_count
            integer(int64), intent(out), optional :: count
        end subroutine dired_do_kill_lines

        !> Compress or uncompress FILE.
        !> NEWNAME is the name of the compressed or uncompressed file;
        !> FOUND is false if there was no change in files.  Port of
        !> dired-aux.el:dired-compress-file.  (In dired_aux_fm.)
        recursive module subroutine dired_compress_file(file, newname, found, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file
            character(len=:), allocatable, intent(out) :: newname
            logical, intent(out) :: found
            type(status_type), intent(out) :: status
        end subroutine dired_compress_file

        !> Compress or uncompress marked (or next ARG) files.
        !> If invoked on a directory, compress all of the files in
        !> the directory and all of its subdirectories, recursively,
        !> into a .tar.gz archive.
        !> If invoked on a .tar.gz or a .tgz or a .zip or a .7z archive,
        !> uncompress and unpack all the files in the archive.
        !>
        !> If ARG_KIND is DIRED_ARG_MARKED, compress only the marked files,
        !> or none if no files are marked.  Port of
        !> dired-aux.el:dired-do-compress.  (In dired_aux_fm.)
        recursive module subroutine dired_do_compress(arg_kind, arg_n, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_compress

        !> Compress selected files and directories to an archive.
        !> Prompt for the archive file name.
        !> Choose the archiving command based on the archive file-name extension
        !> and `dired_compress_files_alist'.  Port of
        !> dired-aux.el:dired-do-compress-to.  (In dired_aux_fm.)
        recursive module subroutine dired_do_compress_to(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine dired_do_compress_to

        !> Remove entry FILE on each Dired buffer.
        !> Note this doesn't delete FILE in the file system.
        !> See `dired_delete_file' in case you wish that.  Port of
        !> dired-aux.el:dired-remove-file.  (In dired_aux_fm.)
        module subroutine dired_remove_file(file)
            implicit none(type, external)
            character(len=*), intent(in) :: file
        end subroutine dired_remove_file

        !> Create or update the line for FILE in all Dired buffers it would
        !> belong in.  Port of dired-aux.el:dired-relist-file.  (In
        !> dired_aux_fm.)
        recursive module subroutine dired_relist_file(file, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
        end subroutine dired_relist_file

        !> Query user with PROMPT, and store the answer in STATE.
        !> ANSWER is false or true.
        !>
        !> The user may type y or SPC to accept once; n or DEL to skip once;
        !> ! to accept this and subsequent queries; or q or ESC to decline
        !> this and subsequent queries.
        !>
        !> If STATE already holds an answer (it starts as 0), this function
        !> may return automatically without querying the user.  If STATE
        !> is !, ANSWER is true; if STATE is q or ESC, false.  HELP_FORM is
        !> what the help character shows.  Port of dired-aux.el:dired-query.
        !> (In dired_aux_fm.)
        recursive module subroutine dired_query(state, prompt, answer, status, help_form)
            implicit none(type, external)
            integer, intent(inout) :: state
            character(len=*), intent(in) :: prompt
            logical, intent(out) :: answer
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: help_form
        end subroutine dired_query

        !> Copy FROM to TO, overwriting it if OK_FLAG, as Dired's file
        !> creator for copying.  Port of dired-aux.el:dired-copy-file.  (In
        !> dired_aux_fm.)
        recursive module subroutine dired_copy_file(from, to, ok_flag, status)
            implicit none(type, external)
            character(len=*), intent(in) :: from, to
            logical, intent(in) :: ok_flag
            type(status_type), intent(out) :: status
        end subroutine dired_copy_file

        !> Copy FROM to TO; a directory with `copy_directory' when RECURSIVE
        !> (a `dired_recursive_copies' value) says so, a symbolic link as a
        !> link unless DEREFERENCE.  PRESERVE_TIME keeps the modification
        !> time; TOP says FROM is a top-level file of the operation.  Port of
        !> dired-aux.el:dired-copy-file-recursive.  (In dired_aux_fm.)
        recursive module subroutine dired_copy_file_recursive(from, to, ok_flag, status, preserve_time, top, recursive, &
                                                              dereference)
            implicit none(type, external)
            character(len=*), intent(in) :: from, to
            logical, intent(in) :: ok_flag
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: preserve_time, top, dereference
            integer, intent(in), optional :: recursive
        end subroutine dired_copy_file_recursive

        !> Rename FILE to NEWNAME.
        !> Signal a `file_already_exists' error if a file NEWNAME already exists
        !> unless OK_IF_ALREADY_EXISTS is true.  Port of
        !> dired-aux.el:dired-rename-file (`dired_vc_rename_file' is
        !> deferred).  (In dired_aux_fm.)
        recursive module subroutine dired_rename_file(file, newname, ok_if_already_exists, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file, newname
            logical, intent(in) :: ok_if_already_exists
            type(status_type), intent(out) :: status
        end subroutine dired_rename_file

        !> Give FILE the additional name NEWNAME and update its line.  Port
        !> of dired-aux.el:dired-hardlink.  (In dired_aux_fm.)
        recursive module subroutine dired_hardlink(file, newname, ok_if_already_exists, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file, newname
            logical, intent(in) :: ok_if_already_exists
            type(status_type), intent(out) :: status
        end subroutine dired_hardlink

        !> Make a symbolic link (pointing to FILE1) in FILE2.
        !> The link is relative (if possible), for example
        !>
        !>     "/vol/tex/bin/foo" "/vol/local/bin/foo"
        !>
        !> results in
        !>
        !>     "../../tex/bin/foo" "/vol/local/bin/foo"
        !>
        !> Port of dired-aux.el:dired-make-relative-symlink.  (In
        !> dired_aux_fm.)
        recursive module subroutine dired_make_relative_symlink(file1, file2, status, ok_if_already_exists)
            implicit none(type, external)
            character(len=*), intent(in) :: file1, file2
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: ok_if_already_exists
        end subroutine dired_make_relative_symlink

        !> Create one or more new files from a list of existing files FN_LIST.
        !> This function also handles querying the user, updating Dired
        !> buffers, and displaying a success or failure message.
        !>
        !> FILE_CREATOR is called once for each file in FN_LIST, and must
        !> create a new file, querying the user and updating Dired buffers
        !> as necessary.  It is given the old file name, the new name, and
        !> an argument OK_FLAG with the same meaning as
        !> OK_IF_ALREADY_EXISTS in `copy_file'.  CREATOR_KIND tells
        !> whether it is `dired_copy_file' (DIRED_CREATOR_COPY).
        !>
        !> OPERATION should be a capitalized string describing the operation
        !> performed (e.g. `Copy').  It is used for error logging.
        !>
        !> FN_LIST is the list of files to copy (full absolute file names).
        !>
        !> NAME_CONSTRUCTOR is given the name of an old file and returns
        !> either the corresponding new file name or nil (SKIP) to skip.
        !>
        !> Mark each newly-created file's Dired entry with the character
        !> MARKER_CHAR, or with the file's own mark if MARKER_CHAR is
        !> DIRED_KEEP_MARKER (" " for none).  Port of
        !> dired-aux.el:dired-create-files.  (In dired_aux_fm.)
        recursive module subroutine dired_create_files(file_creator, creator_kind, operation, fn_list, &
                                                       name_constructor, marker_char, status)
            implicit none(type, external)
            procedure(dired_file_creator_proc) :: file_creator
            integer, intent(in) :: creator_kind
            character(len=*), intent(in) :: operation
            type(dired_string_type), intent(in) :: fn_list(:)
            procedure(dired_name_constructor_proc) :: name_constructor
            character, intent(in) :: marker_char
            type(status_type), intent(out) :: status
        end subroutine dired_create_files

        !> Create a new file for each marked file.
        !> Prompt user for a target directory in which to create the new
        !>   files.  The target may also be a non-directory file, if only
        !>   one file is marked.  The initial suggestion for target is the
        !>   Dired buffer's current directory (or, if `dired_dwim_target' is
        !>   set, the current directory of a neighboring Dired window).
        !>
        !> OP_SYMBOL is the symbol for the operation.  Function
        !>   `dired_mark_pop_up' will determine whether pop-ups are
        !>   appropriate for this OP_SYMBOL.
        !>
        !> FILE_CREATOR, CREATOR_KIND and OPERATION as in `dired_create_files'.
        !>
        !> ARG as in `dired_get_marked_files'.
        !>
        !> MARKER_CHAR as in `dired_create_files'.
        !>
        !> OP1 is an alternate form for OPERATION if there is only one file.
        !>
        !> HOW_TO_PLAIN true means the target is always treated as a plain
        !> file (upstream's HOW-TO t); otherwise `file_directory_p' tells
        !> whether the target is a directory.  Port of
        !> dired-aux.el:dired-do-create-files (HOW-TO functions are
        !> deferred).  (In dired_aux_fm.)
        recursive module subroutine dired_do_create_files(op_symbol, file_creator, creator_kind, operation, arg_kind, &
                                                          arg_n, marker_char, status, op1, how_to_plain)
            implicit none(type, external)
            character(len=*), intent(in) :: op_symbol, operation
            procedure(dired_file_creator_proc) :: file_creator
            integer, intent(in) :: creator_kind, arg_kind
            integer(int64), intent(in) :: arg_n
            character, intent(in) :: marker_char
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: op1
            logical, intent(in), optional :: how_to_plain
        end subroutine dired_do_create_files

        !> Copy all marked (or next ARG) files, or copy the current file,
        !> into the directory (or to the file) read in the minibuffer;
        !> INVERT_DEREFERENCE inverts `dired_copy_dereference' (C-u).  Port
        !> of dired-aux.el:dired-do-copy.  (In dired_aux_fm.)
        recursive module subroutine dired_do_copy(arg_kind, arg_n, status, invert_dereference)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: invert_dereference
        end subroutine dired_do_copy

        !> Make symbolic links to current file or all marked (or next ARG)
        !> files.  Port of dired-aux.el:dired-do-symlink.  (In dired_aux_fm.)
        recursive module subroutine dired_do_symlink(arg_kind, arg_n, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_symlink

        !> Relative symlink all marked (or next ARG) files into a
        !> directory.  Port of dired-aux.el:dired-do-relsymlink.  (In
        !> dired_aux_fm.)
        recursive module subroutine dired_do_relsymlink(arg_kind, arg_n, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_relsymlink

        !> Add names (hard links) current file or all marked (or next ARG)
        !> files.  Port of dired-aux.el:dired-do-hardlink.  (In dired_aux_fm.)
        recursive module subroutine dired_do_hardlink(arg_kind, arg_n, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_hardlink

        !> Rename current file or all marked (or next ARG) files.  Port of
        !> dired-aux.el:dired-do-rename.  (In dired_aux_fm.)
        recursive module subroutine dired_do_rename(arg_kind, arg_n, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_rename

        !> Rename selected files whose names match REGEXP to NEWNAME.
        !> With WHOLE_NAME, renaming by regexp affects the absolute file
        !> name.  Port of dired-aux.el:dired-do-rename-regexp.  (In
        !> dired_aux_fm.)
        recursive module subroutine dired_do_rename_regexp(regexp, newname, arg_kind, arg_n, whole_name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: regexp, newname
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            logical, intent(in) :: whole_name
            type(status_type), intent(out) :: status
        end subroutine dired_do_rename_regexp

        !> Copy selected files whose names match REGEXP to NEWNAME.
        !> See function `dired_do_rename_regexp' for more info.  Port of
        !> dired-aux.el:dired-do-copy-regexp.  (In dired_aux_fm.)
        recursive module subroutine dired_do_copy_regexp(regexp, newname, arg_kind, arg_n, whole_name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: regexp, newname
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            logical, intent(in) :: whole_name
            type(status_type), intent(out) :: status
        end subroutine dired_do_copy_regexp

        !> Hardlink selected files whose names match REGEXP to NEWNAME.
        !> See function `dired_do_rename_regexp' for more info.  Port of
        !> dired-aux.el:dired-do-hardlink-regexp.  (In dired_aux_fm.)
        recursive module subroutine dired_do_hardlink_regexp(regexp, newname, arg_kind, arg_n, whole_name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: regexp, newname
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            logical, intent(in) :: whole_name
            type(status_type), intent(out) :: status
        end subroutine dired_do_hardlink_regexp

        !> Symlink selected files whose names match REGEXP to NEWNAME.
        !> See function `dired_do_rename_regexp' for more info.  Port of
        !> dired-aux.el:dired-do-symlink-regexp.  (In dired_aux_fm.)
        recursive module subroutine dired_do_symlink_regexp(regexp, newname, arg_kind, arg_n, whole_name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: regexp, newname
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            logical, intent(in) :: whole_name
            type(status_type), intent(out) :: status
        end subroutine dired_do_symlink_regexp

        !> RelSymlink all marked files containing REGEXP to NEWNAME.
        !> See functions `dired_do_rename_regexp' and `dired_do_relsymlink'
        !> for more info.  Port of dired-aux.el:dired-do-relsymlink-regexp.
        !> (In dired_aux_fm.)
        recursive module subroutine dired_do_relsymlink_regexp(regexp, newname, arg_kind, arg_n, whole_name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: regexp, newname
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            logical, intent(in) :: whole_name
            type(status_type), intent(out) :: status
        end subroutine dired_do_relsymlink_regexp

        !> Rename all marked (or next ARG) files to upper case.  Port of
        !> dired-aux.el:dired-upcase.  (In dired_aux_fm.)
        recursive module subroutine dired_upcase(arg_kind, arg_n, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
        end subroutine dired_upcase

        !> Rename all marked (or next ARG) files to lower case.  Port of
        !> dired-aux.el:dired-downcase.  (In dired_aux_fm.)
        recursive module subroutine dired_downcase(arg_kind, arg_n, status)
            implicit none(type, external)
            integer, intent(in) :: arg_kind
            integer(int64), intent(in) :: arg_n
            type(status_type), intent(out) :: status
        end subroutine dired_downcase

        !> Create an empty file called FILE.
        !> Parent directories of FILE are created as needed.
        !> Add an entry in the Dired buffer for the topmost new parent
        !> directory of FILE, if created, otherwise for the new file.
        !> If FILE already exists, signal an error.  Port of
        !> dired-aux.el:dired-create-empty-file.  (In dired_aux_fm.)
        recursive module subroutine dired_create_empty_file(file, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
        end subroutine dired_create_empty_file

        !> In Dired, examine a file in view mode, returning to Dired when done.
        !> When file is a directory, show it in this buffer if it is inserted.
        !> Otherwise, display it in another buffer.  Port of
        !> dired.el:dired-view-file.  (In dired_aux_fm.)
        recursive module subroutine dired_view_file(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine dired_view_file

        !> Print the type of FILE, according to the `file' command.
        !> If DEREF_SYMLINKS is true and FILE is a symbolic link, then the
        !> command will print the type of the target of the link instead.
        !> Port of dired-aux.el:dired-show-file-type.  (In dired_aux_fm.)
        recursive module subroutine dired_show_file_type(file, deref_symlinks, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file
            logical, intent(in) :: deref_symlinks
            type(status_type), intent(out) :: status
        end subroutine dired_show_file_type

        !> Bind the dired-aux.el commands in `dired_mode_map'.  (In
        !> dired_aux_fm.)
        module subroutine dired_aux_keys()
            implicit none(type, external)
        end subroutine dired_aux_keys

        !> Register the dired-aux.el commands.  (In dired_aux_fm.)
        module subroutine syms_of_dired_aux()
            implicit none(type, external)
        end subroutine syms_of_dired_aux
    end interface

contains

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    !> Whether a live buffer has serial number SERIAL.
    logical function serial_live(serial)
        integer(int64), intent(in) :: serial
        integer(int64) :: k
        serial_live = .false.
        if (.not. allocated(buffers)) return
        do k = 1, size(buffers, kind=int64)
            if (buffers(k)%serial /= serial) cycle
            serial_live = buffer_live_p(buffer_handle(k))
            return
        end do
    end function serial_live

    !> Free the markers of a subdirectory alist.
    subroutine free_alist(alist)
        type(dired_subdir_type), allocatable, intent(inout) :: alist(:)
        integer :: k
        if (.not. allocated(alist)) return
        do k = 1, size(alist)
            if (alist(k)%marker%index /= 0) call release_marker(alist(k)%marker)
        end do
        deallocate (alist)
    end subroutine free_alist

    !> Forget entry K (of a killed buffer, or for kill-all-local-variables).
    subroutine clear_entry(k)
        integer, intent(in) :: k
        call free_alist(locals(k)%subdir_alist)
        if (allocated(locals(k)%directory)) deallocate (locals(k)%directory)
        if (allocated(locals(k)%actual_switches)) deallocate (locals(k)%actual_switches)
        if (allocated(locals(k)%subdir_alist_pre_r)) deallocate (locals(k)%subdir_alist_pre_r)
        locals(k)%has_pre_r = .false.
        locals(k)%set = .false.
        locals(k)%serial = 0
    end subroutine clear_entry

    !> The index of the current buffer's entry in `locals', made if MAKE
    !> (0 if none).
    integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(dired_local_type), allocatable :: grown(:)
        integer :: i, n
        k = 0
        if (cur == 0) return
        if (.not. allocated(locals)) allocate (locals(0))
        do i = 1, size(locals)
            if (locals(i)%set .and. locals(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do
        if (.not. make) return
        ! Reuse the entry of a dead buffer.
        do i = 1, size(locals)
            if (locals(i)%set) then
                if (serial_live(locals(i)%serial)) cycle
                call clear_entry(i)
            end if
            locals(i)%set = .true.
            locals(i)%serial = buffers(cur)%serial
            k = i
            return
        end do
        n = size(locals)
        allocate (grown(n + 1))
        grown(1:n) = locals
        call move_alloc(grown, locals)
        locals(n + 1)%set = .true.
        locals(n + 1)%serial = buffers(cur)%serial
        k = n + 1
    end function local_index

    !> kill-all-local-variables: Dired's buffer-local variables.
    subroutine kill_dired_locals()
        integer :: k
        k = local_index(.false.)
        if (k > 0) call clear_entry(k)
    end subroutine kill_dired_locals

    !> The current buffer's `dired_directory' ("" if unset).  Port of
    !> dired.el dired-directory (buffer-local).
    function dired_directory_of() result(dir)
        character(len=:), allocatable :: dir
        integer :: k
        dir = ""
        k = local_index(.false.)
        if (k == 0) return
        if (allocated(locals(k)%directory)) dir = locals(k)%directory
    end function dired_directory_of

    !> The current buffer's `dired_actual_switches' ("" if unset): the
    !> value of `dired_listing_switches' used to make this buffer's text.
    !> Port of dired.el dired-actual-switches (buffer-local).
    function dired_actual_switches_of() result(sw)
        character(len=:), allocatable :: sw
        integer :: k
        sw = ""
        k = local_index(.false.)
        if (k == 0) return
        if (allocated(locals(k)%actual_switches)) sw = locals(k)%actual_switches
    end function dired_actual_switches_of

    !> The directories of the current buffer's `dired_subdir_alist', in
    !> its order (the last one in the buffer first).
    function dired_subdir_dirs() result(dirs)
        type(dired_string_type), allocatable :: dirs(:)
        integer :: k, i
        allocate (dirs(0))
        k = local_index(.false.)
        if (k == 0) return
        if (.not. allocated(locals(k)%subdir_alist)) return
        deallocate (dirs)
        allocate (dirs(size(locals(k)%subdir_alist)))
        do i = 1, size(dirs)
            dirs(i)%s = locals(k)%subdir_alist(i)%dir
        end do
    end function dired_subdir_dirs

    !> The number of elements of the current buffer's `dired_subdir_alist'.
    integer function alist_size()
        integer :: k
        alist_size = 0
        k = local_index(.false.)
        if (k == 0) return
        if (allocated(locals(k)%subdir_alist)) alist_size = size(locals(k)%subdir_alist)
    end function alist_size

    !> The current buffer's `default_directory'.
    function default_directory() result(dir)
        character(len=:), allocatable :: dir
        dir = "./"
        if (cur == 0) return
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
    end function default_directory

    !> `expand_file_name' as a function (relative to `default_directory').
    function expand(name, dir) result(full)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: dir
        character(len=:), allocatable :: full
        type(status_type) :: st
        call expand_file_name(name, full, st, dir)
        if (.not. st%ok()) full = name
    end function expand

    ! ------------------------------------------------------------------
    ! Small helpers
    ! ------------------------------------------------------------------

    !> Whether text after point matches REGEXP (looking-at-p).
    logical function looking_at_p(regexp)
        character(len=*), intent(in) :: regexp
        type(status_type) :: st
        looking_at_p = looking_at(regexp, st, inhibit_modify=.true.)
    end function looking_at_p

    !> Whether REGEXP matches STRING (string-match-p).
    logical function string_match_p(regexp, string)
        character(len=*), intent(in) :: regexp, string
        type(status_type) :: st
        string_match_p = string_match(regexp, string, st, inhibit_modify=.true.) > 0
    end function string_match_p

    !> Search forward for REGEXP, bounded by BOUND if positive; true if
    !> found (point after the match), else point unchanged.
    logical function re_search_forward_p(regexp, bound, count) result(found)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in), optional :: bound, count
        integer(int64) :: pos
        type(status_type) :: st
        found = .false.
        if (present(bound)) then
            if (bound > 0) then
                call re_search_forward(regexp, pos, st, bound=bound, noerror=SEARCH_NOERROR, count=count)
            else
                call re_search_forward(regexp, pos, st, noerror=SEARCH_NOERROR, count=count)
            end if
        else
            call re_search_forward(regexp, pos, st, noerror=SEARCH_NOERROR, count=count)
        end if
        found = st%ok() .and. pos > 0
    end function re_search_forward_p

    !> Search backward for REGEXP; true if found.
    logical function re_search_backward_p(regexp, count) result(found)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in), optional :: count
        integer(int64) :: pos
        type(status_type) :: st
        call re_search_backward(regexp, pos, st, noerror=SEARCH_NOERROR, count=count)
        found = st%ok() .and. pos > 0
    end function re_search_backward_p

    !> The position of marker M (0 if it points nowhere).
    integer(int64) function mpos(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st
        call marker_position(m, mpos, st)
        if (.not. st%ok()) mpos = 0
    end function mpos

    !> Free marker M, ignoring a stale handle.
    subroutine release_marker(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st
        call free_marker(m, st)
    end subroutine release_marker

    !> Match data position (0 if the group did not match).
    integer(int64) function mbeg(n)
        integer, intent(in) :: n
        type(status_type) :: st
        mbeg = match_beginning(n, st)
    end function mbeg

    integer(int64) function mend(n)
        integer, intent(in) :: n
        type(status_type) :: st
        mend = match_end(n, st)
    end function mend

    !> The position N lines after this one's start (line-beginning-position N).
    integer(int64) function bol(n)
        integer(int64), intent(in), optional :: n
        bol = line_beginning_position(n)
    end function bol

    integer(int64) function eol()
        eol = line_end_position()
    end function eol

    !> Move N lines (forward-line), ignoring the shortage.
    subroutine fwd_line(n)
        integer(int64), intent(in) :: n
        integer(int64) :: shortage
        shortage = forward_line(n)
    end subroutine fwd_line

    !> The character after POS (default point) as an ASCII character, or
    !> achar(0) when there is none or it is not ASCII.
    character function char_at(pos)
        integer(int64), intent(in), optional :: pos
        integer(int32) :: c
        logical :: found
        if (present(pos)) then
            call char_after(c, found, pos)
        else
            call char_after(c, found)
        end if
        char_at = achar(0)
        if (found .and. c >= 0 .and. c < 128) char_at = achar(c)
    end function char_at

    !> The character before point (ASCII, else achar(0)).
    character function char_before_point()
        integer(int32) :: c
        logical :: found
        call char_before(c, found)
        char_before_point = achar(0)
        if (found .and. c >= 0 .and. c < 128) char_before_point = achar(c)
    end function char_before_point

    !> Bind `inhibit_read_only' to true (SAVED keeps the old value).
    subroutine inhibit_read_only_begin(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st
        saved = inhibit_read_only()
        call set(sym("inhibit_read_only"), t_value(), st)
    end subroutine inhibit_read_only_begin

    subroutine inhibit_read_only_end(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st
        call set(sym("inhibit_read_only"), saved, st)
    end subroutine inhibit_read_only_end

    !> Replace the character after point with C (delete-char 1, insert).
    subroutine replace_char_at_point(c, status)
        character, intent(in) :: c
        type(status_type), intent(out) :: status
        call delete_char(1_int64, status)
        if (.not. status%ok()) return
        call insert(c, status)
    end subroutine replace_char_at_point

    !> TEXT with every occurrence of FROM replaced by TO (string-replace).
    function string_replace(from, to, text) result(out)
        character(len=*), intent(in) :: from, to, text
        character(len=:), allocatable :: out
        integer :: i, k
        out = ""
        i = 1
        do
            k = index(text(i:), from)
            if (k == 0) exit
            out = out//text(i:i + k - 2)//to
            i = i + k - 1 + len(from)
        end do
        out = out//text(i:)
    end function string_replace

    !> The decimal representation of N.
    function itoa(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa

    !> "" if COUNT is 1, else "s".  Port of dired.el:dired-plural-s.
    function dired_plural_s(count) result(s)
        integer(int64), intent(in) :: count
        character(len=:), allocatable :: s
        s = "s"
        if (count == 1) s = ""
    end function dired_plural_s

    !> Run hook HOOK, ignoring its status.
    subroutine run_hook_quietly(hook)
        type(hook_type), intent(inout) :: hook
        type(status_type) :: st
        call hook%run(st)
    end subroutine run_hook_quietly

    ! ------------------------------------------------------------------
    ! Switches
    ! ------------------------------------------------------------------

    !> Return true if the string SWITCHES matches LONG or SHORT format.
    !> Port of dired.el:dired-check-switches.
    logical function dired_check_switches(switches, short, long) result(p)
        character(len=*), intent(in) :: switches, short
        character(len=*), intent(in), optional :: long
        character(len=:), allocatable :: re
        logical :: saved
        re = "\(\`\| \)-[[:alnum:]]*"//short
        if (present(long)) re = re//"\|--"//long//"\>"
        saved = case_fold_search%value
        case_fold_search%value = .false.
        p = string_match_p(re, switches)
        case_fold_search%value = saved
    end function dired_check_switches

    !> Return true if the string SWITCHES contains -b or --escape.  Port
    !> of dired.el:dired-switches-escape-p.
    logical function dired_switches_escape_p(switches)
        character(len=*), intent(in) :: switches
        dired_switches_escape_p = dired_check_switches(switches, "b", "\(quoting-style=\)?escape")
    end function dired_switches_escape_p

    !> Return true if the string SWITCHES contains -R or --recursive.  Port
    !> of dired.el:dired-switches-recursive-p.
    logical function dired_switches_recursive_p(switches)
        character(len=*), intent(in) :: switches
        dired_switches_recursive_p = dired_check_switches(switches, "R", "recursive")
    end function dired_switches_recursive_p

    !> Return true if `ls' supports the "--dired" switch.  Port of
    !> dired.el:dired--check-use-ls-dired.
    logical function dired_check_use_ls_dired()
        type(value_type) :: result
        type(status_type) :: st
        call call_process(insert_directory_program, result, st, args=list(make_string("--dired"), make_string("-N")))
        dired_check_use_ls_dired = .false.
        if (st%ok() .and. fixnump(result)) dired_check_use_ls_dired = xfixnum(result) == 0
    end function dired_check_use_ls_dired

    ! ------------------------------------------------------------------
    ! Reading in a directory
    ! ------------------------------------------------------------------

    !> Insert a directory listing of DIR, Dired style.
    !> Use SWITCHES to make the listings.
    !> If FILE_LIST is present, list only those files.
    !> Otherwise, if WILDCARD is true, expand wildcards;
    !>  in that case, DIR should be a file name that uses wildcards.
    !> In other cases, DIR should be a directory name or a directory filename.
    !> If HDR is true, insert a header line with the directory name.  Port
    !> of dired.el:dired-insert-directory (no remote directories or
    !> globstar).
    recursive subroutine dired_insert_directory(dir_in, switches_in, status, file_list, wildcard, hdr)
        character(len=*), intent(in) :: dir_in, switches_in
        type(status_type), intent(out) :: status
        type(dired_string_type), intent(in), optional :: file_list(:)
        logical, intent(in), optional :: wildcard, hdr
        character(len=:), allocatable :: dir, switches, dw_dir, dw_wild, script, saved_dir, dir_name
        type(value_type) :: result, props, saved_ro
        type(handle_type) :: end_marker
        type(status_type) :: st
        integer(int64) :: opoint, beg, found, mb, content_point, here
        logical :: wild, header, use_ls_dired, dir_wildcard, have_list, changed, saved_tabs, had_dir
        integer :: k
        dir = dir_in
        switches = switches_in
        wild = .false.
        if (present(wildcard)) wild = wildcard
        header = .false.
        if (present(hdr)) header = hdr
        have_list = present(file_list)
        status = status_type()
        opoint = point()
        ! Use -N with --dired, to countermand possible non-default quoting
        ! style, in particular via the environment variable QUOTING_STYLE.
        if (dired_use_ls_dired == DIRED_USE_LS_DIRED_UNSPECIFIED) then
            if (dired_check_use_ls_dired()) then
                dired_use_ls_dired = DIRED_USE_LS_DIRED_T
            else
                dired_use_ls_dired = DIRED_USE_LS_DIRED_NIL
                call message(format_message("ls does not support --dired -N; see `dired_use_ls_dired' "// &
                                            "for more details."))
            end if
            use_ls_dired = dired_use_ls_dired == DIRED_USE_LS_DIRED_T
        else
            use_ls_dired = dired_use_ls_dired /= DIRED_USE_LS_DIRED_NIL
        end if
        if (use_ls_dired) switches = "--dired -N "//switches
        ! Expand directory wildcards and fill file-list.
        dir_wildcard = .false.
        if (.not. have_list .and. wild) dir_wildcard = insert_directory_wildcard_in_dir_p(dir, dw_dir, dw_wild)
        if (dir_wildcard) then
            switches = "-d "//switches
            script = insert_directory_program//" "//switches//" "//dw_wild
            had_dir = allocated(buffers(cur)%directory)
            if (had_dir) saved_dir = buffers(cur)%directory
            buffers(cur)%directory = dw_dir
            call call_process(shell_name(), result, status, destination=t_value(), &
                              args=list(make_string("-c"), make_string(script)))
            if (had_dir) then
                buffers(cur)%directory = saved_dir
            else
                deallocate (buffers(cur)%directory)
            end if
            if (.not. status%ok()) return
            if (.not. (fixnump(result))) then
                call set_error(status, FM_USER_ERROR, dw_wild//": No files matching wildcard")
                return
            else if (xfixnum(result) /= 0) then
                call set_error(status, FM_USER_ERROR, dw_wild//": No files matching wildcard")
                return
            end if
            call insert_directory_clean(point(), switches, status)
            if (.not. status%ok()) return
        else if (have_list) then
            do k = 1, size(file_list)
                beg = point()
                call insert_directory(file_list(k)%s, switches, status)
                if (.not. status%ok()) return
                ! `dired_align_file' doesn't fare well with dired
                ! implementations that don't indent entries by one column.
                here = point()
                call goto_char(beg)
                if (.not. looking_at_p(" ")) then
                    call insert(" ", status)
                    here = here + 1
                end if
                call goto_char(here)
                ! Re-align fields, if necessary.
                call dired_align_file(beg, point())
            end do
        else
            call insert_directory(dir, switches, status, wild,.not. wild)
            if (.not. status%ok()) return
        end if
        ! Quote certain characters, unless ls quoted them for us.
        if (.not. dired_switches_escape_p(dired_actual_switches_of())) then
            here = point()
            call make_marker(end_marker)
            call set_marker(end_marker, here, st)
            call goto_char(opoint)
            do
                call search_forward("\", found, st, bound=mpos(end_marker), noerror=SEARCH_NOERROR)
                if (found <= 0) exit
                mb = mbeg(0)
                call text_properties_at(mb, props, st)
                call replace_match("\\", st, literal=.true.)
                call set_text_properties(mb, mb + 2, props, changed, st)
            end do
            call goto_char(opoint)
            do
                call search_forward(achar(13), found, st, bound=mpos(end_marker), noerror=SEARCH_NOERROR)
                if (found <= 0) exit
                mb = mbeg(0)
                call text_properties_at(mb, props, st)
                call replace_match("\015", st, literal=.true.)
                call set_text_properties(mb, mb + 4, props, changed, st)
            end do
            call goto_char(mpos(end_marker))
            call release_marker(end_marker)
        else
            ! Replace any newlines in DIR with literal "\n"s, for the sake
            ! of the header line.  To disambiguate a literal "\n" in the
            ! actual dirname, we also replace "\" with "\\".
            dir = string_replace("\", "\\", dir)
            dir = string_replace(LF, "\n", dir)
        end if
        ! If we used --dired and it worked, the lines are already indented.
        ! Otherwise, indent them.
        here = point()
        call goto_char(opoint)
        if (.not. looking_at_p("  ")) then
            call goto_char(here)
            saved_tabs = indent_tabs_mode%value
            indent_tabs_mode%value = .false.
            call inhibit_read_only_begin(saved_ro)
            call indent_rigidly(opoint, point(), 2_int64, st)
            call inhibit_read_only_end(saved_ro)
            indent_tabs_mode%value = saved_tabs
        else
            call goto_char(here)
        end if
        ! Insert text at the beginning to standardize things.
        content_point = opoint
        here = point()
        call make_marker(end_marker)
        call set_marker(end_marker, here, st)
        call goto_char(opoint)
        if (header .or. wild) then
            if (.not. header_present()) then
                ! Inserted directory name must be absolute, but keep in
                ! mind it may be replaced in some instances like in
                ! `dired_build_subdir_alist'.
                if (dir_wildcard) then
                    dir_name = dw_dir
                else
                    dir_name = directory_file_name(file_name_directory(dir))
                end if
                call insert("  "//dir_name//":"//LF, status)
                if (.not. status%ok()) return
            end if
            content_point = point()
        end if
        if (wild) then
            ! Insert "wildcard" line where "total" line would be for a full dir.
            if (insert_directory_wildcard_in_dir_p(dir, dw_dir, dw_wild)) then
                call insert("  wildcard "//dw_wild//LF, status)
            else
                call insert("  wildcard "//file_name_nondirectory(dir)//LF, status)
            end if
            if (.not. status%ok()) return
        end if
        content_point = dired_insert_disk_space(opoint, dir, status)
        if (.not. status%ok()) return
        call goto_char(mpos(end_marker))
        call release_marker(end_marker)
        call dired_insert_set_properties(content_point, point())
    contains

        !> (and (looking-at "^  \\(.*\\):$")
        !>      (file-name-absolute-p (match-string 1)))
        logical function header_present()
            character(len=:), allocatable :: name
            type(status_type) :: s2
            header_present = .false.
            if (.not. looking_at("^  \(.*\):$", s2)) return
            call buffer_substring(mbeg(1), mend(1), name, s2)
            header_present = file_name_absolute_p(name)
        end function header_present
    end subroutine dired_insert_directory

    !> The shell for wildcard listings: `shell_file_name' (callproc.c).
    function shell_name() result(sh)
        character(len=:), allocatable :: sh
        if (.not. allocated(shell_file_name%value)) call init_callproc()
        sh = shell_file_name%value
    end function shell_name

    !> Insert the amount of free space after the listing that starts at
    !> BEG (for FILE), as `dired_free_space' says; return where the file
    !> lines start.  Port of dired.el:dired--insert-disk-space.
    integer(int64) function dired_insert_disk_space(beg, file, status) result(pos)
        integer(int64), intent(in) :: beg
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: available
        integer(int64) :: opoint
        logical :: found
        status = status_type()
        opoint = point()
        pos = beg
        call goto_char(beg)
        ! First find the line to put it on.
        if (.not. re_search_forward_p("^ *\(total\)")) then
            call goto_char(opoint)
            return
        end if
        if (dired_free_space == DIRED_FREE_SPACE_NIL .or. dired_free_space == DIRED_FREE_SPACE_FIRST) then
            call delete_region(mbeg(0), bol(2_int64), status)
        else
            ! Replace "total" with "total used in directory" to avoid
            ! confusion.
            call replace_match("total used in directory", status, subexp=1)
        end if
        if (.not. status%ok()) return
        call get_free_disk_space(file, available, found, status)
        if (.not. status%ok()) return
        if (found) then
            select case (dired_free_space)
            case (DIRED_FREE_SPACE_SEPARATE)
                call end_of_line()
                call insert(" available "//available, status)
                ! The separate free-space line is considered part of the
                ! directory content, for the purposes of
                ! `dired_hide_details_mode'.
                call beginning_of_line()
                pos = point()
            case (DIRED_FREE_SPACE_FIRST)
                call goto_char(beg)
                if (looking_at_p(" */")) then
                    call end_of_line()
                    if (char_before_point() == ":") then
                        call put_text_property(point() - 1, point(), sym("display"), &
                                               make_string(": ("//available//" available)"), status)
                    end if
                end if
                call fwd_line(1_int64)
                pos = point()
            case default
                pos = beg
            end select
        end if
        call goto_char(opoint)
    end function dired_insert_disk_space

    !> Add various text properties to the lines in the region, from BEG to
    !> END.  Port of dired.el:dired-insert-set-properties (no
    !> `dired_hide_details_preserved_columns', click-to-select, mouse
    !> dragging or `dired_filename_display_length').
    subroutine dired_insert_set_properties(beg, end)
        integer(int64), intent(in) :: beg, end
        type(handle_type) :: end_marker
        type(status_type) :: st
        type(value_type) :: props
        integer(int64) :: opoint, fbeg, fend, pos, b
        logical :: changed
        opoint = point()
        call make_marker(end_marker)
        call set_marker(end_marker, end, st)
        call goto_char(beg)
        do while (point() < mpos(end_marker))
            ! (ignore-errors ...)
            pos = dired_move_to_filename(st)
            if (pos == 0) then
                if (.not. (looking_at_p("^$") .or. looking_at_p(dired_subdir_regexp))) then
                    call put_text_property(bol(), eol() + 1, sym("invisible"), sym("dired_hide_details_information"), st)
                end if
            else
                fend = point() - 1
                b = bol() + 1
                call put_text_property(b, fend, sym("invisible"), sym("dired_hide_details_detail"), st)
                fbeg = point()
                pos = dired_move_to_end_of_filename(st)
                if (st%ok()) then
                    fend = pos - 1
                    call goto_char(fbeg)
                    props = list(sym("mouse_face"), sym("highlight"), sym("dired_filename"), t_value(), &
                                 sym("help_echo"), make_string("mouse-2: visit this file in other window"))
                    call add_text_properties(fbeg, fend + 1, props, changed, st)
                    if (fend + 5 < eol()) then
                        call put_text_property(fend + 5, eol(), sym("invisible"), sym("dired_hide_details_link"), st)
                    end if
                end if
            end if
            call fwd_line(1_int64)
        end do
        call release_marker(end_marker)
        call goto_char(opoint)
    end subroutine dired_insert_set_properties

    !> Align the fields of a file to the ones of surrounding lines.
    !> BEG..END is the line where the file info is located.  Port of
    !> dired.el:dired-align-file (the spaces do not inherit text
    !> properties).
    subroutine dired_align_file(beg, end)
        integer(int64), intent(in) :: beg, end
        integer(int64) :: opoint, file, file_col, other, other_col, alt_col, curcol, align_pt_offset, spaces, &
                          shortage, reached, moved, filepos
        type(handle_type) :: file_marker, other_marker
        type(status_type) :: st
        character(len=:), allocatable :: re
        logical :: num_align, have_offset
        opoint = point()
        ! Check that there is indeed a file, and that there is another
        ! adjacent file with which to align, and that additional spaces are
        ! needed to align the filenames.
        call goto_char(beg)
        file = dired_move_to_filename(st, eol_in=end)
        if (file == 0) then
            call goto_char(opoint)
            return
        end if
        file_col = current_column()
        other = 0
        call goto_char(beg)
        shortage = forward_line(-1_int64)
        if (shortage == 0) other = dired_move_to_filename(st)
        if (other == 0) then
            call goto_char(beg)
            shortage = forward_line(1_int64)
            if (shortage == 0) other = dired_move_to_filename(st)
        end if
        if (other == 0) then
            call goto_char(opoint)
            return
        end if
        other_col = current_column()
        if (other == file .or. other_col <= file_col) then
            call goto_char(opoint)
            return
        end if
        ! If we've only looked at the line above, check to see if the line
        ! below exists as well and if so, align with the shorter one.
        if (other < file) then
            call goto_char(beg)
            shortage = forward_line(1_int64)
            if (shortage == 0) then
                if (dired_move_to_filename(st) /= 0) then
                    alt_col = current_column()
                    if (alt_col < other_col) then
                        other_col = alt_col
                        other = point()
                    end if
                end if
            end if
        end if
        ! Keep positions up-to-date when we insert stuff.
        call make_marker(other_marker)
        call set_marker(other_marker, other, st)
        call make_marker(file_marker)
        call set_marker(file_marker, file, st)
        ! Main loop.
        call goto_char(beg)
        ! Skip to the first field.
        moved = skip_chars_forward(" ", st)
        do while (other_col > file_col .and. mpos(file_marker) > point())
            ! We're now just in front of a field, with a space behind us.
            curcol = current_column()
            ! Nums are right-aligned.
            num_align = looking_at_p("[0-9]")
            ! Let's find the distance to the alignment-point (either the
            ! beginning or the end of the next field, depending on whether
            ! this field is left or right aligned).  It is never TRT to
            ! realign the first column of file's data.
            have_offset = .true.
            align_pt_offset = 0
            if (curcol /= 0) then
                filepos = point()
                call goto_char(mpos(other_marker))
                call move_to_column(curcol, reached)
                if (char_before_point() == " ") then
                    re = " *"
                else
                    re = "[^ ]* *"
                end if
                if (num_align) re = re//"[0-9][^ ]*"
                have_offset = looking_at(re, st)
                if (have_offset) align_pt_offset = mend(0) - mbeg(0)
                call goto_char(filepos)
            end if
            ! Now, the number of spaces to insert is align-pt-offset minus
            ! the distance to the equivalent point on the current line.
            spaces = 0
            if (have_offset) then
                if (.not. num_align) then
                    spaces = align_pt_offset
                else
                    filepos = point()
                    moved = skip_chars_forward("^ ", st)
                    spaces = align_pt_offset - (current_column() - curcol)
                    call goto_char(filepos)
                end if
            end if
            if (have_offset .and. spaces > 0) then
                file_col = spaces + file_col
                if (file_col > other_col) spaces = spaces - (file_col - other_col)
                call insert_char_n(32, spaces, st)
                ! Let's just make really sure we did not mess up.
                filepos = point()
                if (dired_move_to_filename(st) /= mpos(file_marker)) then
                    ! Damn!  We messed up: let's revert the change.
                    call goto_char(filepos)
                    call delete_char(-spaces, st)
                else
                    call goto_char(filepos)
                end if
            end if
            ! Now skip to next field.
            moved = skip_chars_forward("^ ", st)
            moved = skip_chars_forward(" ", st)
        end do
        call release_marker(file_marker)
        call release_marker(other_marker)
        call goto_char(opoint)
    end subroutine dired_align_file

    !> Read in a new Dired buffer.
    !> Differs from `dired_insert_subdir' in that it accepts
    !> wildcards, erases the buffer, and builds the subdir-alist anew
    !> (including making it buffer-local and clearing it first).  Port of
    !> dired.el:dired-readin (no `combine_change_calls').
    recursive subroutine dired_readin(status, run_after_hook)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: run_after_hook
        character(len=:), allocatable :: dirname
        type(value_type) :: saved_ro
        type(file_stat_type) :: attributes
        type(status_type) :: st
        integer(int64) :: opoint
        logical :: found, saved_undo, after
        after = .true.
        if (present(run_after_hook)) after = run_after_hook
        status = status_type()
        ! default-directory and dired-actual-switches must be buffer-local
        ! and initialized by now.
        dirname = expand(dired_directory_of())
        opoint = point()
        ! This hook which may want to modify dired-actual-switches based on
        ! dired-directory.
        call dired_before_readin_hook%run(status)
        if (.not. status%ok()) return
        if (buffers(cur)%undo_enabled) buffers(cur)%undo_count = 0
        call widen()
        call inhibit_read_only_begin(saved_ro)
        ! Don't make undo entries for readin.
        saved_undo = buffers(cur)%undo_enabled
        buffers(cur)%undo_enabled = .false.
        call erase_buffer(status)
        if (status%ok()) call dired_readin_insert(status)
        buffers(cur)%undo_enabled = saved_undo
        call inhibit_read_only_end(saved_ro)
        if (.not. status%ok()) return
        call goto_char(point_min())
        call dired_build_subdir_alist(st)
        call file_attributes(dirname, attributes, found)
        if (found) then
            if (iand(attributes%mode, int(o'170000', int32)) == int(o'040000', int32)) then
                ! (set-visited-file-modtime MTIME)
                buffers(cur)%modtime_known = .true.
                buffers(cur)%modtime_sec = attributes%mtime_sec
                buffers(cur)%modtime_nsec = attributes%mtime_nsec
                buffers(cur)%modtime_size = -1
            end if
        end if
        call restore_buffer_modified_p(.false.)
        ! (save-excursion: the text was replaced, so point is at the start.)
        call goto_char(point_min())
        ! The hook can successfully use dired functions (e.g.
        ! dired-get-filename) as the subdir-alist has been built.
        if (after) call dired_after_readin_hook%run(status)
    end subroutine dired_readin

    !> Insert listing for the specified dir already in dired-directory,
    !> assuming a clean buffer.  Port of dired.el:dired-readin-insert (no
    !> file list).
    recursive subroutine dired_readin_insert(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir, base
        type(handle_type) :: tmpbuf, old
        type(dired_subdir_type), allocatable :: saved_alist(:)
        logical :: had_alist
        type(status_type) :: st
        type(handle_type) :: begv_m, zv_m
        integer(int64) :: found, opoint
        integer :: k
        status = status_type()
        dir = expand(dired_directory_of())
        if (len(file_name_nondirectory(dir)) == 0) then
            ! If we are reading a whole single directory...
            call dired_insert_directory(dir, dired_actual_switches_of(), status, &
                                        wildcard=.not. file_directory_p(dir), hdr=.true.)
        else if (.not. (insert_directory_wildcard_in_dir_p(dir) .or. &
                        file_readable_p(directory_file_name(file_name_directory(dir))))) then
            call set_error(status, FM_GENERIC_ERROR, "Directory "//dir//" inaccessible or nonexistent")
        else
            ! Else treat it as a wildcard spec.
            call dired_insert_directory(dir, dired_actual_switches_of(), status, wildcard=.true., hdr=.true.)
        end if
        if (.not. status%ok()) return
        ! Every time `temporary_file_directory' is (re)displayed in Dired a
        ! new `ls' error file is created and the Dired buffer has an entry
        ! for it.  The file itself is deleted in `insert_directory' but its
        ! Dired entry remains, so we remove it here.
        tmpbuf = dired_find_buffer_nocreate(temporary_file_directory())
        if (tmpbuf%index == 0 .or. .not. allocated(dired_ls_error_file)) return
        old = current_buffer()
        call set_buffer(tmpbuf, st)
        opoint = point()
        call make_marker(begv_m)
        call set_marker(begv_m, buffers(cur)%begv, st)
        call make_marker(zv_m)
        call set_marker(zv_m, buffers(cur)%zv, st)
        call set_marker_insertion_type(zv_m, .true., st)
        call widen()
        call goto_char(point_min())
        base = file_name_base(dired_ls_error_file)
        call search_forward(base, found, st, noerror=SEARCH_NOERROR)
        if (found > 0) then
            ! The call chain of `dired_remove_entry' requires a non-nil
            ! `dired_subdir_alist', so we set it.
            k = local_index(.true.)
            had_alist = allocated(locals(k)%subdir_alist)
            if (had_alist) then
                saved_alist = locals(k)%subdir_alist
                deallocate (locals(k)%subdir_alist)
            end if
            allocate (locals(k)%subdir_alist(1))
            locals(k)%subdir_alist(1)%dir = temporary_file_directory()
            call make_marker(locals(k)%subdir_alist(1)%marker)
            call set_marker(locals(k)%subdir_alist(1)%marker, point_min(), st)
            call dired_remove_entry(dired_ls_error_file)
            call free_alist(locals(k)%subdir_alist)
            if (had_alist) locals(k)%subdir_alist = saved_alist
        end if
        call narrow_to_region(mpos(begv_m), mpos(zv_m), st)
        call release_marker(begv_m)
        call release_marker(zv_m)
        call goto_char(opoint)
        call set_buffer(old, st)
    contains

        !> FILENAME without directory and extension (file-name-base).
        function file_name_base(filename) result(b)
            character(len=*), intent(in) :: filename
            character(len=:), allocatable :: b
            integer :: k2
            b = file_name_nondirectory(filename)
            k2 = index(b, ".", back=.true.)
            if (k2 > 1) b = b(1:k2 - 1)
        end function file_name_base
    end subroutine dired_readin_insert

    ! ------------------------------------------------------------------
    ! The subdirectory alist
    ! ------------------------------------------------------------------

    !> Build `dired_subdir_alist' by parsing the buffer.
    !> If optional arg SWITCHES is present, use its value instead of
    !> `dired_actual_switches'.  Port of dired.el:dired-build-subdir-alist.
    recursive subroutine dired_build_subdir_alist(status, switches, called_interactively)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        logical, intent(in), optional :: called_interactively
        character(len=:), allocatable :: sw, new_dir_name, temp, res
        type(value_type) :: saved_ro
        type(handle_type) :: m
        type(dired_subdir_type), allocatable :: grown(:)
        integer(int64) :: opoint, count, b1, e1, header_bol, after
        logical :: saved_undo, file_entry, esc
        integer :: k, i
        status = status_type()
        k = local_index(.true.)
        call free_alist(locals(k)%subdir_alist)
        allocate (locals(k)%subdir_alist(0))
        opoint = point()
        call inhibit_read_only_begin(saved_ro)
        saved_undo = buffers(cur)%undo_enabled
        buffers(cur)%undo_enabled = .false.
        if (present(switches)) then
            sw = switches
        else
            sw = dired_actual_switches_of()
        end if
        count = 0
        call goto_char(point_min())
        do while (re_search_forward_p(dired_subdir_regexp))
            after = point()
            b1 = mbeg(1)
            e1 = mend(1)
            ! Avoid taking a file name ending in a colon as a subdir name.
            call goto_char(mbeg(0))
            call beginning_of_line()
            header_bol = point()
            file_entry = looking_at_p(dired_re_maybe_mark//dired_re_inode_size//dired_re_perms)
            if (.not. file_entry) then
                call buffer_substring(b1, e1, new_dir_name, status)
                new_dir_name = expand(new_dir_name)
                call goto_char(b1)
                call delete_region(b1, e1, status)
                call insert(new_dir_name, status)
                after = after + (len_chars(new_dir_name) - (e1 - b1))
                count = count + 1
                ! Undo any escaping of newlines and \ by
                ! dired-insert-directory.  Convert "n" preceded by odd number
                ! of \ to newline, and \\ to \.
                if (dired_switches_escape_p(sw) .and. index(new_dir_name, "\") > 0) then
                    res = ""
                    esc = .false.
                    do i = 1, len(new_dir_name)
                        if (new_dir_name(i:i) == "\") then
                            if (esc) then
                                res = res//"\"
                                esc = .false.
                            else
                                esc = .true.
                            end if
                        else if (esc .and. new_dir_name(i:i) == "n") then
                            res = res//LF
                            esc = .false.
                        else
                            temp = ""
                            if (esc) temp = "\"
                            res = res//temp//new_dir_name(i:i)
                            esc = .false.
                        end if
                    end do
                    new_dir_name = res
                end if
                ! dired-alist-add-1: place a sub directory boundary between
                ! lines.
                call make_marker(m)
                call set_marker(m, header_bol, status)
                allocate (grown(size(locals(k)%subdir_alist) + 1))
                grown(1)%dir = dired_normalize_subdir(new_dir_name)
                grown(1)%marker = m
                grown(2:) = locals(k)%subdir_alist
                call move_alloc(grown, locals(k)%subdir_alist)
            end if
            call goto_char(after)
        end do
        buffers(cur)%undo_enabled = saved_undo
        call inhibit_read_only_end(saved_ro)
        call goto_char(opoint)
        if (count > 1 .and. present(called_interactively)) then
            if (called_interactively) call message("Buffer includes "//itoa(count)//" directories")
        end if
    contains

        integer(int64) function len_chars(s)
            character(len=*), intent(in) :: s
            integer :: j
            len_chars = 0
            do j = 1, len(s)
                if (iand(ichar(s(j:j)), 192) /= 128) len_chars = len_chars + 1
            end do
        end function len_chars
    end subroutine dired_build_subdir_alist

    !> DIR as an absolute directory name (relative names are relative to
    !> `default_directory').  Port of dired.el:dired-normalize-subdir.
    function dired_normalize_subdir(dir) result(norm)
        character(len=*), intent(in) :: dir
        character(len=:), allocatable :: norm
        if (file_name_absolute_p(dir)) then
            norm = file_name_as_directory(dir)
        else
            norm = file_name_as_directory(expand(dir, default_directory()))
        end if
    end function dired_normalize_subdir

    !> Return the name of the subdirectory to which this line belongs.
    !> This returns a string with trailing slash, like `default_directory'.
    !> With LOCALP, return a file name relative to `default_directory',
    !> in which case the value could be an empty string if
    !> `default_directory' is the directory where the file on this line
    !> resides.  Port of dired.el:dired-current-directory.
    function dired_current_directory(status, localp) result(dir)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: localp
        character(len=:), allocatable :: dir
        integer :: k, i
        integer(int64) :: here
        status = status_type()
        dir = ""
        k = local_index(.false.)
        if (k == 0 .or. alist_size() == 0) then
            ! probably because called in a non-dired buffer
            call set_error(status, FM_GENERIC_ERROR, "No subdir_alist in #<buffer "//buffers(cur)%name//">")
            return
        end if
        here = point()
        do i = 1, size(locals(k)%subdir_alist)
            dir = locals(k)%subdir_alist(i)%dir
            ! use `<=' (not `<') as subdir line is part of subdir
            if (mpos(locals(k)%subdir_alist(i)%marker) <= here) exit
        end do
        if (present(localp)) then
            if (localp) dir = dired_make_relative(dir, default_directory())
        end if
    end function dired_current_directory

    !> Convert FILE (an absolute file name) to a name relative to DIR.
    !> If DIR is omitted, it defaults to `default_directory'.
    !> If FILE is not in the directory tree of DIR, return FILE
    !> unchanged.  Port of dired.el:dired-make-relative.
    function dired_make_relative(file, dir_in) result(rel)
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: dir_in
        character(len=:), allocatable :: rel, dir
        type(status_type) :: st
        integer(int64) :: e
        if (present(dir_in)) then
            dir = dir_in
        else
            dir = default_directory()
        end if
        ! This case comes into play if default-directory is set to use ~.
        if (string_match_p("\(\`\|:\)~", dir)) dir = expand(dir)
        rel = file
        if (string_match("^"//regexp_quote(dir), file, st) > 0) then
            e = mend(0)
            rel = file(string_index_to_offset(file, e) + 1:)
        end if
    end function dired_make_relative

    !> Return the subdir name on this line, or "" if not on a headerline.
    !> Port of dired.el:dired-get-subdir.
    function dired_get_subdir() result(dir)
        character(len=:), allocatable :: dir
        character(len=:), allocatable :: cur_dir
        type(status_type) :: st
        integer :: k, i
        dir = ""
        cur_dir = dired_current_directory(st)
        if (.not. st%ok()) return
        k = local_index(.false.)
        do i = 1, size(locals(k)%subdir_alist)
            if (locals(k)%subdir_alist(i)%dir == cur_dir) then
                ! alist stores b-o-l positions
                if (bol() == mpos(locals(k)%subdir_alist(i)%marker)) dir = cur_dir
                return
            end if
        end do
    end function dired_get_subdir

    !> The index of DIR in `dired_subdir_alist' (0-based, as nth wants
    !> it), or -1.  Port of dired.el:dired-subdir-index.
    integer function dired_subdir_index(dir) result(index)
        character(len=*), intent(in) :: dir
        integer :: k, i
        index = -1
        k = local_index(.false.)
        if (k == 0) return
        do i = 1, size(locals(k)%subdir_alist)
            if (locals(k)%subdir_alist(i)%dir == dir) then
                index = i - 1
                return
            end if
        end do
    end function dired_subdir_index

    !> Go to next subdirectory, regardless of level.  Return the position
    !> (0 if not found and NO_ERROR_IF_NOT_FOUND).  Port of
    !> dired.el:dired-next-subdir.
    function dired_next_subdir(arg, status, no_error_if_not_found, no_skip) result(pos)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_error_if_not_found, no_skip
        integer(int64) :: pos
        character(len=:), allocatable :: this_dir
        integer :: k, index
        logical :: noerr, skip
        pos = 0
        noerr = .false.
        if (present(no_error_if_not_found)) noerr = no_error_if_not_found
        skip = .true.
        if (present(no_skip)) skip = .not. no_skip
        this_dir = dired_current_directory(status)
        if (.not. status%ok()) return
        k = local_index(.false.)
        index = dired_subdir_index(this_dir) - int(arg)
        if (index >= 0 .and. index < size(locals(k)%subdir_alist)) then
            call goto_char(mpos(locals(k)%subdir_alist(index + 1)%marker))
            if (skip) call end_of_line()
            pos = point()
        else if (.not. noerr) then
            if (arg > 0) then
                call set_error(status, FM_GENERIC_ERROR, "Last directory")
            else
                call set_error(status, FM_GENERIC_ERROR, "First directory")
            end if
        end if
    end function dired_next_subdir

    !> Where the current subdirectory ends.  Port of
    !> dired.el:dired-subdir-max.
    integer(int64) function dired_subdir_max() result(pos)
        integer(int64) :: opoint
        type(status_type) :: st
        opoint = point()
        pos = point_max()
        if (alist_size() > 1) then
            if (dired_next_subdir(1_int64, st, .true., .true.) /= 0) pos = point()
        end if
        call goto_char(opoint)
    end function dired_subdir_max

    ! ------------------------------------------------------------------
    ! File names in the buffer
    ! ------------------------------------------------------------------

    !> Move to the beginning of the filename on the current line.
    !> Return the position of the beginning of the filename, or 0 if none
    !> found.
    !>
    !> If RAISE_ERROR, signal an error if we can't find the filename on
    !> the current line.
    !>
    !> If EOL_IN is present, it is a position to use instead of
    !> `line_end_position' as the end of the line.  Port of
    !> dired.el:dired-move-to-filename.
    integer(int64) function dired_move_to_filename(status, raise_error, eol_in) result(pos)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: raise_error
        integer(int64), intent(in), optional :: eol_in
        integer(int64) :: e, change
        logical :: raise, found
        status = status_type()
        raise = .false.
        if (present(raise_error)) raise = raise_error
        if (present(eol_in)) then
            e = eol_in
        else
            e = eol()
        end if
        pos = 0
        call beginning_of_line()
        ! First try assuming `ls --dired' was used.
        call next_single_property_change(point(), sym("dired_filename"), change, found, status, limit=e)
        if (.not. status%ok()) return
        if (found .and. change < e) then
            call goto_char(change)
            pos = change
        else if (re_search_forward_p(directory_listing_regexp(), e)) then
            call goto_char(mend(0))
            pos = point()
        else if (re_search_forward_p(dired_permission_flags_regexp, e)) then
            ! Ha!  There *is* a file.  Our regexp-from-hell just failed to
            ! find it.
            if (raise) then
                call set_error(status, FM_GENERIC_ERROR, &
                               "Unrecognized line!  Check directory_listing_before_filename_regexp")
                return
            end if
            call beginning_of_line()
        else if (raise) then
            call set_error(status, FM_GENERIC_ERROR, "No file on this line")
        end if
    end function dired_move_to_filename

    !> `directory_listing_before_filename_regexp' (files.el).
    function directory_listing_regexp() result(re)
        character(len=:), allocatable :: re
        re = directory_listing_before_filename_regexp
    end function directory_listing_regexp

    !> Move to the end of the file name, with point at its beginning
    !> (after `dired_move_to_filename').  Return the position, or 0 if
    !> point did not move; an error then unless NO_ERROR.  Port of
    !> dired.el:dired-move-to-end-of-filename.
    integer(int64) function dired_move_to_end_of_filename(status, no_error) result(pos)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_error
        type(value_type) :: v
        character(len=:), allocatable :: bits
        character :: file_type
        integer(int64) :: opoint, e, change, found_pos, saved
        logical :: noerr, used_f, hidden, symlink, executable, found
        status = status_type()
        noerr = .false.
        if (present(no_error)) noerr = no_error
        pos = 0
        call get_text_property(point(), sym("dired_filename"), v, status)
        if (.not. status%ok()) return
        if (.not. nilp(v)) then
            call next_single_property_change(point(), sym("dired_filename"), change, found, status)
            ! No property change can happen on or before the last file name
            ! in the Dired listing when there is at least one prior file
            ! name containing a newline (bug#79528).
            if (.not. found) change = point_max() - 1
            call goto_char(change)
            pos = change
            return
        end if
        opoint = point()
        used_f = dired_check_switches(dired_actual_switches_of(), "F", "classify")
        e = eol()
        hidden = dired_hidden_p()
        file_type = " "
        symlink = .false.
        executable = .false.
        if (.not. hidden) then
            saved = point()
            ! Find out what kind of file this is: restrict perm bits to be
            ! non-blank, otherwise this matches one char to early (looking
            ! backward).
            if (re_search_backward_p(dired_permission_flags_regexp)) then
                file_type = char_at(mbeg(1))
                symlink = file_type == "l"
                ! Only with -F we need to know whether it's an executable.
                if (used_f) then
                    call buffer_substring(mbeg(2), mend(2), bits, status)
                    executable = scan(bits, "xst") > 0
                    call buffer_substring(mbeg(3), mend(3), bits, status)
                    executable = executable .or. scan(bits, "xst") > 0
                    call buffer_substring(mbeg(4), mend(4), bits, status)
                    executable = executable .or. scan(bits, "xst") > 0
                end if
            else if (.not. noerr) then
                call goto_char(saved)
                call set_error(status, FM_GENERIC_ERROR, "No file on this line")
                return
            end if
            call goto_char(saved)
            ! Move point to end of name:
            if (symlink) then
                call search_forward(" -> ", found_pos, status, bound=e, noerror=SEARCH_NOERROR)
                if (found_pos > 0) then
                    call goto_char(point() - 4)
                    ! did ls really mark the link?
                    if (used_f .and. dired_ls_f_marks_symlinks .and. char_before_point() == "@") then
                        call goto_char(point() - 1)
                    end if
                end if
            else
                ! else not a symbolic link
                call goto_char(e)
                ! ls -lF marks dirs, sockets, fifos and executables with
                ! exactly one trailing character.
                if (used_f .and. (scan(file_type, "dsp") > 0 .or. executable)) call goto_char(point() - 1)
            end if
        end if
        if (.not. noerr .and. opoint == point()) then
            if (hidden) then
                call set_error(status, FM_GENERIC_ERROR, &
                               substitute_command_keys("File line is hidden, type \[dired_hide_subdir] to unhide"))
            else
                call set_error(status, FM_GENERIC_ERROR, "No file on this line")
            end if
            return
        end if
        if (opoint /= point()) pos = point()
    end function dired_move_to_end_of_filename

    !> Whether the character at point is hidden (`invisible' is `dired').
    !> Port of dired.el:dired--hidden-p (text properties only).
    logical function dired_hidden_p(pos)
        integer(int64), intent(in), optional :: pos
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: p
        p = point()
        if (present(pos)) p = pos
        dired_hidden_p = .false.
        if (p >= point_max()) return
        call get_text_property(p, sym("invisible"), v, st)
        if (st%ok()) dired_hidden_p = value_eq(v, sym("dired"))
    end function dired_hidden_p

    !> In Dired, return name of file mentioned on this line.
    !> Value returned normally includes the directory name.
    !> LOCALP with value DIRED_LOCALP_NO_DIR means don't include directory
    !> name in result.  A value of DIRED_LOCALP_VERBATIM means to return
    !> the name exactly as it occurs in the buffer, and DIRED_LOCALP_T
    !> means construct name relative to `default_directory', which still
    !> may contain slashes if in a subdirectory.
    !> NO_ERROR_IF_NOT_FILEP means treat `.' and `..' as regular filenames
    !> and return no name (FOUND false) if no filename on this line.
    !> Otherwise, an error occurs in these cases.  Port of
    !> dired.el:dired-get-filename.
    recursive subroutine dired_get_filename(file, found, status, localp, no_error_if_not_filep)
        character(len=:), allocatable, intent(out) :: file
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: localp
        logical, intent(in), optional :: no_error_if_not_filep
        character(len=:), allocatable :: raw, quoted, rep, dir
        integer(int64) :: opoint, p1, p2, next, start, b
        integer :: lp, i, shift
        logical :: noerr, saved_fold, already_absolute, verbatim
        type(status_type) :: st
        lp = DIRED_LOCALP_NIL
        if (present(localp)) lp = localp
        noerr = .false.
        if (present(no_error_if_not_filep)) noerr = no_error_if_not_filep
        status = status_type()
        found = .false.
        file = ""
        ! (Hidden subdirectories are deferred: no subdirectory is hidden.)
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        opoint = point()
        p2 = 0
        p1 = dired_move_to_filename(status,.not. noerr)
        if (status%ok() .and. p1 /= 0) p2 = dired_move_to_end_of_filename(status, noerr)
        call goto_char(opoint)
        if (.not. status%ok()) then
            case_fold_search%value = saved_fold
            return
        end if
        ! nil if no file on this line, but no-error-if-not-filep is t:
        if (p1 /= 0 .and. p2 /= 0) then
            call buffer_substring(p1, p2, raw, status)
            ! Unquote names quoted by ls or by dired-insert-directory.
            quoted = ""
            do i = 1, len(raw)
                if (raw(i:i) == '"') then
                    if (i == 1) then
                        quoted = quoted//"\"
                    else if (raw(i - 1:i - 1) /= "\") then
                        quoted = quoted//"\"
                    end if
                end if
                quoted = quoted//raw(i:i)
            end do
            ! Unescape any spaces escaped by ls -b (bug#10469).
            if (dired_switches_escape_p(dired_actual_switches_of())) then
                verbatim = lp == DIRED_LOCALP_VERBATIM
                rep = ""
                shift = -1
                if (verbatim) then
                    rep = "\\"
                    shift = 1
                end if
                start = 1
                do
                    b = string_match("\(\\\) ", quoted, st, start)
                    if (b == 0) exit
                    quoted = quoted(1:string_index_to_offset(quoted, mbeg(1)))//rep// &
                             quoted(string_index_to_offset(quoted, mend(1)) + 1:)
                    start = shift + mend(0)
                end do
            end if
            ! Hence we don't need to worry about converting `\\' back to `\'.
            call read_string_literal(quoted//'"', 1_int64, file, next, status)
            if (.not. status%ok()) then
                case_fold_search%value = saved_fold
                return
            end if
            found = .true.
        end if
        case_fold_search%value = saved_fold
        if (.not. found) return
        already_absolute = index(file, "/") == 1
        if (lp == DIRED_LOCALP_VERBATIM) then
            return
        else if (.not. noerr .and. (file == "." .or. file == "..")) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Cannot operate on `.' or `..'"))
            found = .false.
        else if (lp == DIRED_LOCALP_NO_DIR .and. already_absolute) then
            file = file_name_nondirectory(file)
        else if (already_absolute) then
            return
        else if (lp == DIRED_LOCALP_NO_DIR) then
            return
        else
            dir = dired_current_directory(status, lp == DIRED_LOCALP_T)
            if (.not. status%ok()) then
                found = .false.
                return
            end if
            file = dir//file
        end if
    end subroutine dired_get_filename

    !> Try to get a file name at point in the current Dired buffer.
    !> It returns an abbreviated name that can't be used as an argument
    !> to `dired_goto_file'.  FOUND is false for none.  Port of
    !> dired.el:dired-file-name-at-point.
    subroutine dired_file_name_at_point(name, found)
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: found
        character(len=:), allocatable :: filename
        type(status_type) :: st
        name = ""
        call dired_get_filename(filename, found, st, no_error_if_not_filep=.true.)
        if (.not. found) return
        if (file_directory_p(filename)) then
            name = file_name_as_directory(abbreviate_file_name(filename))
        else
            name = abbreviate_file_name(filename)
        end if
    end subroutine dired_file_name_at_point

    !> True if there is no file name on this line.  Port of
    !> dired.el:dired-between-files.
    logical function dired_between_files()
        integer(int64) :: opoint
        type(status_type) :: st
        opoint = point()
        dired_between_files = dired_move_to_filename(st) == 0
        call goto_char(opoint)
    end function dired_between_files

    ! ------------------------------------------------------------------
    ! Dired buffers
    ! ------------------------------------------------------------------

    !> Is FILE part of the directory tree starting at DIR?  Port of
    !> dired.el:dired-in-this-tree-p.
    logical function dired_in_this_tree_p(file, dir)
        character(len=*), intent(in) :: file, dir
        logical :: saved
        saved = case_fold_search%value
        case_fold_search%value = .false.
        dired_in_this_tree_p = string_match_p("^"//regexp_quote(dir), file)
        case_fold_search%value = saved
    end function dired_in_this_tree_p

    !> Convert glob-pattern PATTERN to a regular expression.  Port of
    !> dired.el:dired-glob-regexp.
    function dired_glob_regexp(pattern) result(regexp)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: regexp
        integer :: matched, k, set_start, set_cont, set_end
        regexp = ""
        matched = 0
        do
            k = scan(pattern(matched + 1:), "[?*")
            if (k == 0) exit
            k = matched + k
            regexp = regexp//regexp_quote(pattern(matched + 1:k - 1))
            select case (pattern(k:k))
            case ("?")
                regexp = regexp//"."
                matched = k
            case ("[")
                ! Fails to handle ^ yet ????
                set_start = k
                set_cont = set_start + 2
                if (set_start + 1 <= len(pattern)) then
                    if (pattern(set_start + 1:set_start + 1) == "^") set_cont = set_start + 3
                end if
                set_end = index(pattern(set_cont:), "]")
                if (set_end == 0) then
                    matched = len(pattern)
                    exit
                end if
                set_end = set_cont + set_end - 1
                regexp = regexp//pattern(set_start:set_end)
                matched = set_end
            case default
                regexp = regexp//".*"
                matched = k
            end select
        end do
        regexp = "\`"//regexp//regexp_quote(pattern(matched + 1:))//"\'"
    end function dired_glob_regexp

    !> Return a list of buffers for DIR (top level or in-situ subdir).
    !> If FILE is present, include only those whose wildcard pattern (if
    !> any) matches FILE.  The list is in reverse order of buffer creation,
    !> most recent last.  As a side effect, killed Dired buffers for DIR
    !> are removed from `dired_buffers'.  Port of
    !> dired.el:dired-buffers-for-dir.
    function dired_buffers_for_dir(dir_in, file) result(result)
        character(len=*), intent(in) :: dir_in
        character(len=*), intent(in), optional :: file
        type(handle_type), allocatable :: result(:)
        character(len=:), allocatable :: dir, wildcards
        type(handle_type) :: old, buf
        type(handle_type), allocatable :: grown(:)
        type(status_type) :: st
        logical :: ok
        integer :: i, k
        allocate (result(0))
        call prune_dired_buffers()
        dir = file_name_as_directory(expand(dir_in))
        old = current_buffer()
        do i = 1, size(dired_buffers)
            buf = dired_buffers(i)%buffer
            if (.not. dired_in_this_tree_p(dir, dired_buffers(i)%dir)) cycle
            call set_buffer(buf, st)
            ok = .false.
            k = local_index(.false.)
            if (k > 0) then
                if (dired_subdir_index(dir) >= 0) then
                    if (.not. present(file)) then
                        ok = .true.
                    else
                        wildcards = file_name_nondirectory(dired_directory_of())
                        ok = len(wildcards) == 0
                        if (.not. ok) ok = string_match_p(dired_glob_regexp(wildcards), file)
                    end if
                end if
            end if
            if (ok) then
                allocate (grown(size(result) + 1))
                grown(1) = buf
                grown(2:) = result
                call move_alloc(grown, result)
            end if
        end do
        call set_buffer(old, st)
    end function dired_buffers_for_dir

    !> Remove the killed buffers from `dired_buffers'.
    subroutine prune_dired_buffers()
        type(dired_buffer_entry_type), allocatable :: kept(:)
        integer :: i, n
        if (.not. allocated(dired_buffers)) allocate (dired_buffers(0))
        n = 0
        do i = 1, size(dired_buffers)
            if (buffer_live_p(dired_buffers(i)%buffer)) n = n + 1
        end do
        if (n == size(dired_buffers)) return
        allocate (kept(n))
        n = 0
        do i = 1, size(dired_buffers)
            if (.not. buffer_live_p(dired_buffers(i)%buffer)) cycle
            n = n + 1
            kept(n) = dired_buffers(i)
        end do
        call move_alloc(kept, dired_buffers)
    end subroutine prune_dired_buffers

    !> Advertise in variable `dired_buffers' that we dired
    !> `default_directory'.  Port of dired.el:dired-advertise.
    subroutine dired_advertise()
        character(len=:), allocatable :: expanded_default
        type(handle_type), allocatable :: bufs(:)
        type(dired_buffer_entry_type), allocatable :: grown(:)
        type(handle_type) :: me
        integer :: i
        expanded_default = expand(default_directory())
        me = current_buffer()
        bufs = dired_buffers_for_dir(expanded_default)
        do i = 1, size(bufs)
            ! we have already advertised ourselves
            if (same_window(bufs(i), me)) return
        end do
        allocate (grown(size(dired_buffers) + 1))
        grown(1)%dir = expanded_default
        grown(1)%buffer = me
        grown(2:) = dired_buffers
        call move_alloc(grown, dired_buffers)
    end subroutine dired_advertise

    !> Find the Dired buffer for DIRNAME (the null handle for none).
    !> Port of dired.el:dired-find-buffer-nocreate (no dired-x; MODE
    !> `dired_mode').
    function dired_find_buffer_nocreate(dirname_in) result(found)
        character(len=*), intent(in) :: dirname_in
        type(handle_type) :: found
        character(len=:), allocatable :: dirname, dd
        type(handle_type) :: old
        type(status_type) :: st
        integer :: i
        found = null_handle()
        call prune_dired_buffers()
        dirname = expand(dirname_in)
        old = current_buffer()
        do i = 1, size(dired_buffers)
            call set_buffer(dired_buffers(i)%buffer, st)
            if (.not. st%ok()) cycle
            if (.not. allocated(buffers(cur)%major_mode)) cycle
            if (buffers(cur)%major_mode /= "dired_mode") cycle
            dd = dired_directory_of()
            ! dired-directory is nil during find-alternate-file
            if (len(dd) == 0) cycle
            if (dirname == expand(dd)) then
                found = dired_buffers(i)%buffer
                exit
            end if
        end do
        call set_buffer(old, st)
    end function dired_find_buffer_nocreate

    ! ------------------------------------------------------------------
    ! The dired command
    ! ------------------------------------------------------------------

    !> Read the directory (and with a prefix argument the switches) for
    !> `dired': "Dired STR(directory): ".  Port of
    !> dired.el:dired-read-dir-and-switches.
    recursive subroutine dired_read_dir_and_switches(str, dirname, switches, have_switches, status)
        character(len=*), intent(in) :: str
        character(len=:), allocatable, intent(out) :: dirname, switches
        logical, intent(out) :: have_switches
        type(status_type), intent(out) :: status
        have_switches = .not. nilp(current_prefix_arg)
        switches = ""
        if (have_switches) then
            call read_string("Dired listing switches: ", switches, status, initial=dired_listing_switches)
            if (.not. status%ok()) return
        end if
        call read_file_name("Dired "//str//"(directory): ", dirname, status, default_filename=default_directory())
    end subroutine dired_read_dir_and_switches

    !> "Edit" directory DIRNAME--delete, rename, print, etc. some files in it.
    !> SWITCHES specifies the options to be used when invoking
    !> `insert_directory_program', usually `ls', which produces the
    !> listing of the directory files and their attributes.
    !> Interactively, a prefix argument will cause the command to prompt
    !> for SWITCHES.
    !>
    !> Dired displays a list of files in DIRNAME (which may also have shell
    !> wildcards appended to select certain files).
    !>
    !> \<dired_mode_map>You can flag files for deletion with \[dired_flag_file_deletion] and then
    !> delete them by typing \[dired_do_flagged_delete].
    !> Type \[describe_mode] after entering Dired for more info.
    !>
    !> If DIRNAME is already in a Dired buffer, that buffer is used without
    !> refresh.  Port of dired.el:dired (DIRNAME a string).
    recursive subroutine dired(dirname, status, switches)
        character(len=*), intent(in) :: dirname
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        type(handle_type) :: buf
        call dired_noselect(dirname, buf, status, switches)
        if (.not. status%ok()) return
        if (buf%index /= 0) call pop_to_buffer_same_window(buf, status)
        if (.not. status%ok()) return
        call dired_display_ls_error()
    end subroutine dired

    !> "Edit" directory DIRNAME.  Like `dired' but select in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of dired.el:dired-other-window.
    recursive subroutine dired_other_window(dirname, status, switches)
        character(len=*), intent(in) :: dirname
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        type(handle_type) :: buf
        call dired_noselect(dirname, buf, status, switches)
        if (.not. status%ok()) return
        if (buf%index /= 0) call switch_to_buffer_other_window(buf, status)
        if (.not. status%ok()) return
        call dired_display_ls_error()
    end subroutine dired_other_window

    !> "Edit" directory DIRNAME--delete, rename, print, etc. some files in it.
    !> Optional second argument SWITCHES specifies the options to be used
    !> when invoking `insert_directory_program', usually `ls', which
    !> produces the listing of the directory files and their attributes.
    !> Interactively, a prefix argument will cause the command to prompt
    !> for SWITCHES.
    !>
    !> \<dired_mode_map>You can flag files for deletion with \[dired_flag_file_deletion] and then
    !> delete them by typing \[dired_do_flagged_delete].
    !> Type \[describe_mode] after entering Dired for more info.
    !>
    !> If DIRNAME is already in a Dired buffer, that buffer is used without
    !> refresh.  Port of dired.el:dired (interactive).
    recursive subroutine cmd_dired(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dirname, switches
        logical :: have_switches
        call dired_read_dir_and_switches("", dirname, switches, have_switches, status)
        if (.not. status%ok()) return
        if (have_switches) then
            call dired(dirname, status, switches)
        else
            call dired(dirname, status)
        end if
    end subroutine cmd_dired

    !> "Edit" directory DIRNAME.  Like `dired' but select in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of dired.el:dired-other-window (interactive).
    recursive subroutine cmd_dired_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dirname, switches
        logical :: have_switches
        call dired_read_dir_and_switches("in other window ", dirname, switches, have_switches, status)
        if (.not. status%ok()) return
        if (have_switches) then
            call dired_other_window(dirname, status, switches)
        else
            call dired_other_window(dirname, status)
        end if
    end subroutine cmd_dired_other_window

    !> Like `dired' but return the Dired buffer as value, do not select it.
    !> Port of dired.el:dired-noselect (DIR-OR-LIST a string;
    !> `find_file_visit_truename' nil).
    recursive subroutine dired_noselect(dir_or_list, buffer, status, switches)
        character(len=*), intent(in) :: dir_or_list
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        character(len=:), allocatable :: dirname
        logical :: initially_was_dirname
        if (len(dir_or_list) == 0) then
            dirname = default_directory()
        else
            dirname = dir_or_list
        end if
        ! This loses the distinction between "/foo/*/" and "/foo/*" that
        ! some shells make:
        initially_was_dirname = file_name_as_directory(dirname) == dirname
        call expand_file_name(directory_file_name(dirname), dirname, status)
        if (.not. status%ok()) return
        dirname = abbreviate_file_name(dirname)
        ! If the argument was syntactically a directory name not a file
        ! name, or if it happens to name a file that is a directory, convert
        ! it syntactically to a directory name.
        if (initially_was_dirname .or. file_directory_p(dirname)) dirname = file_name_as_directory(dirname)
        call dired_internal_noselect(dirname, buffer, status, switches)
    end subroutine dired_noselect

    !> `dired_noselect' as a `find_directory_functions' function.
    recursive subroutine dired_noselect_dir(dirname, buf, status)
        character(len=*), intent(in) :: dirname
        type(handle_type), intent(out) :: buf
        type(status_type), intent(out) :: status
        call dired_noselect(dirname, buf, status)
    end subroutine dired_noselect_dir

    !> Whether the directory visited by the current dired buffer has
    !> changed on disk.  Port of dired.el:dired-directory-changed-p.
    logical function dired_directory_changed_p(dirname) result(changed)
        character(len=*), intent(in) :: dirname
        type(file_stat_type) :: attributes
        logical :: found
        changed = .false.
        if (.not. buffers(cur)%modtime_known) return
        call file_attributes(dirname, attributes, found)
        if (.not. found) return
        if (iand(attributes%mode, int(o'170000', int32)) /= int(o'040000', int32)) return
        changed = .not. (attributes%mtime_sec == buffers(cur)%modtime_sec .and. &
                         attributes%mtime_nsec == buffers(cur)%modtime_nsec)
    end function dired_directory_changed_p

    !> Make or find the Dired buffer for DIRNAME (the null handle when
    !> `ls' failed).  Port of dired.el:dired-internal-noselect (DIRNAME a
    !> string; no MODE).
    recursive subroutine dired_internal_noselect(dirname, buffer, status, switches)
        character(len=*), intent(in) :: dirname
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        character(len=:), allocatable :: sw, dir_part
        type(handle_type) :: old_buf
        type(status_type) :: st
        integer :: k
        logical :: new_buffer_p, failed
        status = status_type()
        old_buf = current_buffer()
        ! Look for an existing buffer.
        buffer = dired_find_buffer_nocreate(dirname)
        ! Note that buffer already is in dired-mode, if found.
        new_buffer_p = buffer%index == 0
        if (new_buffer_p) then
            call create_file_buffer(dirname, buffer, status)
            if (.not. status%ok()) return
        end if
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        if (.not. new_buffer_p) then
            if (present(switches)) then
                ! ... but new switches: file list may have changed
                k = local_index(.true.)
                locals(k)%directory = dirname
                ! this calls dired-revert
                call dired_sort_other(switches, status)
            else if (dired_auto_revert_buffer) then
                ! Always revert regardless of whether it has changed or not.
                call revert_buffer(status)
            else if (dired_directory_changed_p(dirname)) then
                ! If directory has changed on disk, offer to revert.
                call message(substitute_command_keys( &
                             "Directory has changed on disk; type \[revert_buffer] to update Dired"))
            end if
        else
            ! Else a new buffer
            if (insert_directory_wildcard_in_dir_p(dirname, dir_part)) then
                buffers(cur)%directory = dir_part
            else
                ! We can do this unconditionally because dired-noselect
                ! ensures that the name is passed in directory name syntax
                ! if it was the name of a directory at all.
                buffers(cur)%directory = file_name_directory(dirname)
            end if
            if (present(switches)) then
                sw = switches
            else
                sw = dired_listing_switches
            end if
            call dired_mode(status, dirname, sw)
            if (status%ok()) then
                ! default-directory and dired-actual-switches are set now
                ! (buffer-local), so we can call dired-readin.
                failed = .true.
                dired_ls_error_buffer = null_handle()
                call dired_readin(status)
                if (status%ok()) then
                    failed = .false.
                    if (buffer_live_p(dired_ls_error_buffer)) then
                        if (buffer_live_p(get_buffer("*ls error*"))) failed = .true.
                    end if
                end if
            else
                failed = .true.
            end if
            ! If either `dired_readin' failed (e.g. if parent directories
            ! are inaccessible) or `ls' errored, don't leave the Dired buffer
            ! around.
            if (failed) then
                call kill_buffer(buffer, st)
                buffer = null_handle()
            else
                call goto_char(point_min())
                call dired_initial_position(dirname)
            end if
        end if
        if (buffer_live_p(old_buf)) call set_buffer(old_buf, st)
    end subroutine dired_internal_noselect

    !> Pop up the buffer displaying the current `ls' error, if any.  Port
    !> of dired.el:dired--display-ls-error.
    subroutine dired_display_ls_error()
        type(handle_type) :: errwin
        type(status_type) :: st
        if (buffer_live_p(dired_ls_error_buffer)) then
            call display_buffer(dired_ls_error_buffer, errwin, st)
            if (window_live_p(errwin)) call fit_window_to_buffer(errwin)
            dired_ls_error_buffer = null_handle()
        end if
    end subroutine dired_display_ls_error

    !> Position point in a new listing of DIRNAME.
    !> Point is assumed to be at the beginning of a new subdir line.
    !> Runs the hook `dired_initial_position_hook'.  Port of
    !> dired.el:dired-initial-position (no dired-x).
    subroutine dired_initial_position(dirname)
        character(len=*), intent(in) :: dirname
        call end_of_line()
        if (allocated(dired_trivial_filenames)) call dired_goto_next_nontrivial_file()
        call run_hook_quietly(dired_initial_position_hook)
        if (len(dirname) < 0) return
    end subroutine dired_initial_position

    !> Position point on first nontrivial file after point.  Port of
    !> dired.el:dired-goto-next-nontrivial-file.
    subroutine dired_goto_next_nontrivial_file()
        character(len=:), allocatable :: fn
        type(status_type) :: st
        integer(int64) :: pos
        logical :: found
        ! so there is a file to compare with
        call dired_goto_next_file()
        if (.not. allocated(dired_trivial_filenames)) return
        do while (.not. eobp())
            call dired_get_filename(fn, found, st, no_error_if_not_filep=.true.)
            if (.not. found) fn = ""
            if (.not. string_match_p(dired_trivial_filenames, file_name_nondirectory(fn))) exit
            call fwd_line(1_int64)
            pos = dired_move_to_filename(st)
        end do
    end subroutine dired_goto_next_nontrivial_file

    !> Move to the next line with a file, within the subdirectory.  Port
    !> of dired.el:dired-goto-next-file.
    subroutine dired_goto_next_file()
        integer(int64) :: max
        type(status_type) :: st
        max = dired_subdir_max() - 1
        do while (dired_move_to_filename(st) == 0 .and. point() < max)
            call fwd_line(1_int64)
        end do
    end subroutine dired_goto_next_file

    ! ------------------------------------------------------------------
    ! Dired mode
    ! ------------------------------------------------------------------

    !> Mode for "editing" directory listings.
    !> In Dired, you are "editing" a list of the files in a directory and
    !>   (optionally) its subdirectories, in the format of `ls -lR'.
    !> "Editing" means that you can run shell commands on files, visit,
    !>   compress, load or byte-compile them, change their file attributes
    !>   and insert subdirectories into the same buffer.  You can "mark"
    !>   files for later commands or "flag" them for deletion, either file
    !>   by file or all files matching certain criteria.
    !> You can move using the usual cursor motion commands.\<dired_mode_map>
    !> The buffer is read-only.  Digits are prefix arguments.
    !> Type \[dired_flag_file_deletion] to flag a file `D' for deletion.
    !> Type \[dired_mark] to Mark a file or subdirectory for later commands.
    !>   Most commands operate on the marked files and use the current file
    !>   if no files are marked.  Use a numeric prefix argument to operate on
    !>   the next ARG (or previous -ARG if ARG<0) files, or just `1'
    !>   to operate on the current file only.  Prefix arguments override marks.
    !>   Mark-using commands display a list of failures afterwards.  Type \[dired_summary]
    !>   to see why something went wrong.
    !> Type \[dired_unmark] to Unmark a file or all files of an inserted subdirectory.
    !> Type \[dired_unmark_backward] to back up one line and unmark or unflag.
    !> Type \[dired_do_flagged_delete] to delete (eXpunge) the files flagged `D'.
    !> Type \[dired_find_file] to Find the current line's file
    !>   (or dired it in another buffer, if it is a directory).
    !> Type \[dired_find_file_other_window] to find file or Dired directory in Other window.
    !> Type \[dired_sort_toggle_or_edit] to toggle Sorting by name/date or change the `ls' switches.
    !> Type \[revert_buffer] to read all currently expanded directories aGain.
    !>   This retains all marks and hides subdirs again that were hidden before.
    !> Use \`SPC' and \`DEL' to move down and up by lines.
    !>
    !> If Dired ever gets confused, you can either type \[revert_buffer]
    !> to read the directories again, type \[dired_do_redisplay]
    !> to relist the file at point or the marked files or a
    !> subdirectory, or type \[dired_build_subdir_alist] to parse the buffer
    !> again for the directory tree.
    !>
    !> This mode runs the following hooks:
    !>
    !>   `dired_before_readin_hook'
    !>   `dired_after_readin_hook'
    !>   `dired_mode_hook'
    !>
    !> DIRNAME is the directory (`dired_directory'; by default
    !> `default_directory') and SWITCHES the `ls' switches (by default
    !> `dired_listing_switches').  Port of dired.el:dired-mode.
    recursive subroutine dired_mode(status, dirname, switches)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: dirname, switches
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: saved
        integer :: k
        if (.not. keymap_live_p(dired_mode_map)) call syms_of_dired_mode()
        ! (kill-all-local-variables) (use-local-map dired-mode-map)
        call major_modes%dired_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! default-directory is already set
        call dired_advertise()
        buffers(cur)%read_only = .true.
        call revert_buffer_function%set_local(dired_revert)
        k = local_index(.true.)
        if (present(dirname)) then
            locals(k)%directory = dirname
        else
            locals(k)%directory = default_directory()
        end if
        if (present(switches)) then
            locals(k)%actual_switches = switches
        else
            locals(k)%actual_switches = dired_listing_switches
        end if
        call dired_font_lock_keywords(keywords)
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        ! before sorting
        call dired_sort_other(locals(k)%actual_switches, status, no_revert=.true.)
        if (.not. status%ok()) return
        call major_modes%dired_mode%finish(saved, status)
    end subroutine dired_mode

    !> `dired_mode' as a command procedure (not interactive).
    recursive subroutine dired_mode_cmd(status)
        type(status_type), intent(out) :: status
        call dired_mode(status)
    end subroutine dired_mode_cmd

    ! ------------------------------------------------------------------
    ! Reverting
    ! ------------------------------------------------------------------

    !> Reread the Dired buffer.
    !> Must also be called after `dired_actual_switches' have changed.
    !> Should not fail even on completely garbaged buffers.
    !> Preserves old cursor, marks/flags, hidden-p.
    !>
    !> Dired sets `revert_buffer_function' to this function.  The args
    !> IGNORE_AUTO and NOCONFIRM, passed from `revert_buffer', are
    !> ignored.  Port of dired.el:dired-revert (no inserted subdirectories
    !> to restore: they are deferred; no directory-local variables).
    recursive subroutine dired_revert(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status
        type(dired_positions_type) :: positions
        type(dired_string_type), allocatable :: mark_files(:)
        character, allocatable :: mark_chars(:)
        type(value_type) :: saved_ro
        logical :: modflag, saved_fold
        ! just in case user narrowed
        call widen()
        call buffer_modified_p(current_buffer(), modflag, status)
        if (.not. status%ok()) return
        call dired_save_positions(positions)
        saved_fold = case_fold_search%value
        ! we check for upper case ls flags
        case_fold_search%value = .false.
        call inhibit_read_only_begin(saved_ro)
        call goto_char(point_min())
        ! only after dired-remember-hidden since this unhides:
        call dired_remember_marks(point_min(), point_max(), mark_files, mark_chars)
        ! Run dired-after-readin-hook just once, below.
        call dired_readin(status, run_after_hook=.false.)
        if (status%ok()) then
            ! mark files that were marked
            call dired_mark_remembered(mark_files, mark_chars)
            ! ... run the hook for the whole buffer, and only after markers
            ! have been reinserted.
            call dired_after_readin_hook%run(status)
            call dired_restore_positions(positions)
            if (.not. modflag) call restore_buffer_modified_p(.false.)
        end if
        call inhibit_read_only_end(saved_ro)
        case_fold_search%value = saved_fold
        call free_positions(positions)
        if (ignore_auto .and. noconfirm) continue
    end subroutine dired_revert

    !> Return current positions in the buffer and all windows with this
    !> directory: the file name and line number at point, in the windows
    !> showing the buffer, and at the point a window keeps for it among its
    !> previous buffers.  Port of dired.el:dired-save-positions.
    subroutine dired_save_positions(positions)
        type(dired_positions_type), intent(out) :: positions
        type(handle_type), allocatable :: list(:)
        type(dired_saved_position_type), allocatable :: grown(:)
        type(handle_type) :: me
        integer(int64) :: opoint, pos
        integer :: k, j
        me = current_buffer()
        positions%buffer = me
        opoint = point()
        call save_at(opoint, positions%here)
        allocate (positions%windows(0), positions%prevs(0))
        call window_leaves(selected_frame%root_window, list)
        do k = 1, size(list)
            if (.not. same_window(windows(list(k)%index)%buffer, me)) cycle
            if (same_window(list(k), selected_window())) then
                pos = opoint
            else
                pos = window_point(windows(list(k)%index))
            end if
            allocate (grown(size(positions%windows) + 1))
            grown(1:size(positions%windows)) = positions%windows
            call save_at(pos, grown(size(grown)))
            grown(size(grown))%window = list(k)
            call move_alloc(grown, positions%windows)
        end do
        ! For each window that showed the current buffer before, its
        ! window-point marker in the window's list of previous buffers.
        do k = 1, size(list)
            if (.not. allocated(windows(list(k)%index)%prev_buffers)) cycle
            do j = 1, size(windows(list(k)%index)%prev_buffers)
                if (.not. same_window(windows(list(k)%index)%prev_buffers(j)%buffer, me)) cycle
                if (windows(list(k)%index)%prev_buffers(j)%point%index == 0) exit
                pos = mpos(windows(list(k)%index)%prev_buffers(j)%point)
                allocate (grown(size(positions%prevs) + 1))
                grown(2:) = positions%prevs
                call save_at(pos, grown(1))
                grown(1)%marker = windows(list(k)%index)%prev_buffers(j)%point
                call move_alloc(grown, positions%prevs)
                exit
            end do
        end do
        call goto_char(opoint)
    contains

        subroutine save_at(p, saved)
            integer(int64), intent(in) :: p
            type(dired_saved_position_type), intent(inout) :: saved
            character(len=:), allocatable :: fn
            type(status_type) :: st
            call goto_char(p)
            call dired_get_filename(fn, saved%has_file, st, no_error_if_not_filep=.true.)
            if (saved%has_file) saved%file = fn
            saved%line = line_number_at_pos(p)
        end subroutine save_at
    end subroutine dired_save_positions

    !> Release what `dired_save_positions' made (nothing to free: the
    !> markers belong to the windows).
    subroutine free_positions(positions)
        type(dired_positions_type), intent(inout) :: positions
        if (allocated(positions%windows)) deallocate (positions%windows)
        if (allocated(positions%prevs)) deallocate (positions%prevs)
    end subroutine free_positions

    !> Restore POSITIONS saved with `dired_save_positions'.  Port of
    !> dired.el:dired-restore-positions.
    subroutine dired_restore_positions(positions)
        type(dired_positions_type), intent(in) :: positions
        type(status_type) :: st
        type(handle_type) :: mbuf
        integer(int64) :: opoint, pos
        integer :: k
        call goto_char(position_of(positions%here))
        do k = 1, size(positions%windows)
            ! Ensure that window still displays the original buffer.
            if (.not. window_live_p(positions%windows(k)%window)) cycle
            if (.not. same_window(windows(positions%windows(k)%window%index)%buffer, positions%buffer)) cycle
            if (same_window(positions%windows(k)%window, selected_window())) then
                call goto_char(position_of(positions%windows(k)))
            else
                opoint = point()
                pos = position_of(positions%windows(k))
                call goto_char(opoint)
                call set_window_point(windows(positions%windows(k)%window%index), pos, st)
            end if
        end do
        opoint = point()
        do k = 1, size(positions%prevs)
            ! Sanity check of the point marker.
            call marker_buffer(positions%prevs(k)%marker, mbuf, st)
            if (.not. st%ok()) cycle
            if (mbuf%index == 0) cycle
            if (.not. same_window(mbuf, positions%buffer)) cycle
            pos = position_of(positions%prevs(k))
            call set_marker(positions%prevs(k)%marker, pos, st)
        end do
        call goto_char(opoint)
    contains

        !> Go to the saved file, else to the saved line's file; return
        !> point.
        integer(int64) function position_of(saved) result(p)
            type(dired_saved_position_type), intent(in) :: saved
            integer(int64) :: q
            type(status_type) :: s2
            q = 0
            if (saved%has_file) q = dired_goto_file(saved%file, s2)
            if (q == 0) then
                call goto_char(point_min())
                call fwd_line(saved%line - 1)
                q = dired_move_to_filename(s2)
            end if
            p = point()
        end function position_of
    end subroutine dired_restore_positions

    !> Return the files and their marks, from BEG to END (the last first).
    !> Port of dired.el:dired-remember-marks.
    subroutine dired_remember_marks(beg, end, files, chars)
        integer(int64), intent(in) :: beg, end
        type(dired_string_type), allocatable, intent(out) :: files(:)
        character, allocatable, intent(out) :: chars(:)
        type(dired_string_type), allocatable :: gf(:)
        character, allocatable :: gc(:)
        character(len=:), allocatable :: fil
        type(status_type) :: st
        integer(int64) :: opoint
        logical :: found
        allocate (files(0), chars(0))
        opoint = point()
        call goto_char(beg)
        do while (re_search_forward_p(dired_re_mark, end))
            call dired_get_filename(fil, found, st, no_error_if_not_filep=.true.)
            if (.not. found) cycle
            allocate (gf(size(files) + 1), gc(size(chars) + 1))
            gf(1)%s = fil
            gf(2:) = files
            gc(1) = char_before_point()
            gc(2:) = chars
            call move_alloc(gf, files)
            call move_alloc(gc, chars)
        end do
        call goto_char(opoint)
    end subroutine dired_remember_marks

    !> Mark all files remembered in FILES with their CHARS.  Port of
    !> dired.el:dired-mark-remembered.
    subroutine dired_mark_remembered(files, chars)
        type(dired_string_type), intent(in) :: files(:)
        character, intent(in) :: chars(:)
        type(status_type) :: st
        integer(int64) :: opoint
        integer :: k
        opoint = point()
        do k = 1, size(files)
            if (dired_goto_file(files(k)%s, st) /= 0) then
                call beginning_of_line()
                call replace_char_at_point(chars(k), st)
            end if
        end do
        call goto_char(opoint)
    end subroutine dired_mark_remembered

    ! ------------------------------------------------------------------
    ! Going to a file
    ! ------------------------------------------------------------------

    !> Go to line describing file FILE in this Dired buffer.  Return the
    !> position, or 0.  FILE must be an absolute file name.  Port of
    !> dired.el:dired-goto-file.
    function dired_goto_file(file_in, status) result(found)
        character(len=*), intent(in) :: file_in
        type(status_type), intent(out) :: status
        integer(int64) :: found
        character(len=:), allocatable :: file, dir
        integer(int64) :: opoint
        logical :: saved_fold, ok
        status = status_type()
        found = 0
        if (.not. file_name_absolute_p(file_in)) then
            call set_error(status, FM_GENERIC_ERROR, format_message("File name `")//file_in// &
                           format_message("' is not absolute"))
            return
        end if
        ! does no harm if not a directory
        file = directory_file_name(file_in)
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        dir = file_name_directory(file)
        opoint = point()
        ! First, look for a listing under the absolute name.
        call goto_char(point_min())
        found = dired_goto_file_1(file, file, point_max())
        if (found == 0) then
            ! Next, look for it as a relative name with leading
            ! subdirectories.
            call goto_char(point_min())
            found = dired_goto_file_1(file_relative_name(file, default_directory()), file, point_max())
        end if
        if (found == 0) then
            ! Otherwise, look for it as a relative name, a base name only.
            if (dir == expand(default_directory())) then
                call goto_char(point_min())
                ok = .true.
            else
                ok = .false.
                if (alist_size() > 1) ok = dired_goto_subdir(dir) /= 0
            end if
            if (ok) found = dired_goto_file_1(file_name_nondirectory(file), file, dired_subdir_max())
        end if
        case_fold_search%value = saved_fold
        ! Return buffer position, if found.
        if (found /= 0) then
            call goto_char(found)
        else
            call goto_char(opoint)
        end if
    end function dired_goto_file

    !> Advance to the Dired listing labeled by FILE; return its position.
    !> Return 0 if the listing is not found.  If FILE contains characters
    !> that would not appear in a Dired buffer, search using the quoted
    !> forms of those characters.
    !>
    !> FULL_NAME specifies the actual file name the listing must have,
    !> as returned by `dired_get_filename'.  LIMIT is the search limit.
    !> Port of dired.el:dired-goto-file-1.
    function dired_goto_file_1(file, full_name, limit) result(found)
        character(len=*), intent(in) :: file, full_name
        integer(int64), intent(in) :: limit
        integer(int64) :: found
        character(len=:), allocatable :: str, fn
        integer(int64) :: pos
        type(status_type) :: st
        logical :: have
        str = string_replace(achar(13), "\^m", file)
        str = string_replace("\", "\\", str)
        if (dired_switches_escape_p(dired_actual_switches_of()) .and. scan(str, " "//TAB//LF) > 0) then
            str = string_replace(" ", "\ ", str)
            str = string_replace(TAB, "\t", str)
            str = string_replace(LF, "\n", str)
        end if
        found = 0
        do while (found == 0)
            ! filenames are preceded by SPC, this makes the search faster
            ! (e.g. for the filename "-").
            call search_forward(" "//str, pos, st, bound=limit, noerror=SEARCH_MOVE)
            if (pos <= 0) exit
            ! Check that we are in the right place.  Match could have BASE
            ! just as initial substring or in permission bits etc.
            call dired_get_filename(fn, have, st, no_error_if_not_filep=.true.)
            if (have .and. fn == full_name .and. len(fn) == len(full_name)) then
                found = dired_move_to_filename(st)
            else
                call fwd_line(1_int64)
            end if
        end do
    end function dired_goto_file_1

    !> Go to line describing file FILE in this Dired buffer.  Port of
    !> dired.el:dired-goto-file (interactive).
    recursive subroutine cmd_dired_goto_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, dir
        integer(int64) :: pos
        dir = dired_current_directory(status)
        if (.not. status%ok()) return
        call read_file_name("Goto file: ", file, status, dir=dir)
        if (.not. status%ok()) return
        ! let push-mark display its message
        call push_mark()
        pos = dired_goto_file(expand(file), status)
    end subroutine cmd_dired_goto_file

    ! ------------------------------------------------------------------
    ! Moving
    ! ------------------------------------------------------------------

    !> The prefix argument of a command as a number (1 if none).
    integer(int64) function prefix_n()
        prefix_n = prefix_numeric_value(current_prefix_arg)
    end function prefix_n

    !> Move down ARG lines, then position at filename.  Port of
    !> dired.el:dired--trivial-next-line (no invisible text).
    subroutine dired_trivial_next_line(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical :: saved_visual
        integer(int64) :: saved_goal, pos
        saved_visual = line_move_visual
        saved_goal = goal_column
        line_move_visual = .false.
        goal_column = -1
        call line_move(arg, status, noerror=.true.)
        line_move_visual = saved_visual
        goal_column = saved_goal
        pos = dired_move_to_filename(status)
    end subroutine dired_trivial_next_line

    !> Goto ARGth next directory file line.  Port of
    !> dired.el:dired--trivial-next-dirline.
    subroutine dired_trivial_next_dirline(arg, status, opoint_in)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: opoint_in
        integer(int64) :: opoint, pos
        logical :: found
        status = status_type()
        opoint = point()
        if (present(opoint_in)) opoint = opoint_in
        if (arg > 0) then
            found = re_search_forward_p(dired_re_dir, count=arg)
        else
            call beginning_of_line()
            found = re_search_backward_p(dired_re_dir, count=-arg)
        end if
        if (found) then
            ! user may type `i' or `f'
            pos = dired_move_to_filename(status)
        else
            call goto_char(opoint)
            if (dired_movement_style == DIRED_MOVEMENT_STYLE_NIL) then
                call set_error(status, FM_GENERIC_ERROR, "No more subdirectories")
            end if
        end if
    end subroutine dired_trivial_next_dirline

    !> Move ARG lines (JUMP 1) or directory lines (JUMP 2), as
    !> `dired_movement_style' says.  Port of
    !> dired.el:dired--move-to-next-line.
    subroutine dired_move_to_next_line(arg_in, jump, status)
        integer(int64), intent(in) :: arg_in
        integer, intent(in) :: jump
        type(status_type), intent(out) :: status
        integer(int64) :: arg, old_arg, old_position, moving_down, pos
        logical :: wrapped, cycle_style, files_style
        status = status_type()
        arg = arg_in
        wrapped = .false.
        old_arg = arg
        ! It's always true that we should move to the filename when
        ! possible.
        pos = dired_move_to_filename(status)
        old_position = point()
        ! Up/Down indicates the direction.
        moving_down = -1
        if (arg > 0) moving_down = 1
        cycle_style = dired_movement_style == DIRED_MOVEMENT_STYLE_CYCLE .or. &
                      dired_movement_style == DIRED_MOVEMENT_STYLE_CYCLE_FILES
        files_style = dired_movement_style == DIRED_MOVEMENT_STYLE_CYCLE_FILES .or. &
                      dired_movement_style == DIRED_MOVEMENT_STYLE_BOUNDED_FILES
        ! Line by line in case we forget to skip empty lines.
        do while (arg /= 0)
            call jumpfun(moving_down)
            if (old_position == point()) then
                ! Now point is at beginning/end of movable area, but it
                ! still wants to move farther.
                if (cycle_style) then
                    ! `cycle': go to the other end.  Argument not changing
                    ! on the second wrap means infinite loop with no files
                    ! found.
                    if (wrapped .and. old_arg == arg) then
                        arg = 0
                    else if (moving_down > 0) then
                        call goto_char(point_min())
                    else
                        call goto_char(point_max())
                    end if
                    wrapped = .true.
                else
                    ! `bounded': go back to the last non-empty line.
                    do while (empty_line_p() .and. arg /= 0)
                        call jumpfun(-moving_down)
                        ! Point not moving means infinite loop.
                        if (old_position == point()) then
                            arg = 0
                        else
                            old_position = point()
                        end if
                    end do
                    ! Encountered a boundary, so let's stop movement.
                    if (empty_line_p()) then
                        arg = 0
                    else
                        arg = moving_down
                    end if
                end if
            end if
            if (.not. empty_line_p()) then
                ! Has moved to a non-empty line.  This movement does make
                ! sense.
                arg = arg - moving_down
            end if
            old_position = point()
        end do
    contains

        subroutine jumpfun(n)
            integer(int64), intent(in) :: n
            type(status_type) :: s2
            if (jump == 1) then
                call dired_trivial_next_line(n, s2)
            else
                call dired_trivial_next_dirline(n, s2)
            end if
        end subroutine jumpfun

        !> (and (dired-between-files) (or FILES-STYLE (not (dired-get-subdir))))
        logical function empty_line_p()
            empty_line_p = .false.
            if (.not. dired_between_files()) return
            if (files_style) then
                empty_line_p = .true.
            else
                empty_line_p = len(dired_get_subdir()) == 0
            end if
        end function empty_line_p
    end subroutine dired_move_to_next_line

    !> Move down ARG lines, then position at filename.
    !> The argument ARG (interactively, prefix argument) says how many lines
    !> to move; the default is one line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-next-line.
    subroutine dired_next_line(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        if (dired_movement_style /= DIRED_MOVEMENT_STYLE_NIL) then
            call dired_move_to_next_line(arg, 1, status)
        else
            call dired_trivial_next_line(arg, status)
        end if
    end subroutine dired_next_line

    !> Move up ARG lines, then position at filename.
    !> The argument ARG (interactively, prefix argument) says how many lines
    !> to move; the default is one line.
    !>
    !> Whether to skip empty lines and how to move from first line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-previous-line.
    subroutine dired_previous_line(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        call dired_next_line(-arg, status)
    end subroutine dired_previous_line

    !> Goto ARGth next directory file line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-next-dirline.
    subroutine dired_next_dirline(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        if (dired_movement_style /= DIRED_MOVEMENT_STYLE_NIL) then
            call dired_move_to_next_line(arg, 2, status)
        else
            call dired_trivial_next_dirline(arg, status)
        end if
    end subroutine dired_next_dirline

    !> Goto ARGth previous directory file line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-prev-dirline.
    subroutine dired_prev_dirline(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        call dired_next_dirline(-arg, status)
    end subroutine dired_prev_dirline

    !> Move down ARG lines, then position at filename.
    !> The argument ARG (interactively, prefix argument) says how many lines
    !> to move; the default is one line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-next-line (interactive).
    subroutine cmd_dired_next_line(status)
        type(status_type), intent(out) :: status
        call dired_next_line(prefix_n(), status)
    end subroutine cmd_dired_next_line

    !> Move up ARG lines, then position at filename.
    !> The argument ARG (interactively, prefix argument) says how many lines
    !> to move; the default is one line.
    !>
    !> Whether to skip empty lines and how to move from first line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-previous-line (interactive).
    subroutine cmd_dired_previous_line(status)
        type(status_type), intent(out) :: status
        call dired_previous_line(prefix_n(), status)
    end subroutine cmd_dired_previous_line

    !> Goto ARGth next directory file line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-next-dirline (interactive).
    subroutine cmd_dired_next_dirline(status)
        type(status_type), intent(out) :: status
        call dired_next_dirline(prefix_n(), status)
    end subroutine cmd_dired_next_dirline

    !> Goto ARGth previous directory file line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-prev-dirline (interactive).
    subroutine cmd_dired_prev_dirline(status)
        type(status_type), intent(out) :: status
        call dired_prev_dirline(prefix_n(), status)
    end subroutine cmd_dired_prev_dirline

    !> Go to next subdirectory, regardless of level.  Port of
    !> dired.el:dired-next-subdir (interactive).
    subroutine cmd_dired_next_subdir(status)
        type(status_type), intent(out) :: status
        integer(int64) :: pos
        pos = dired_next_subdir(prefix_n(), status)
    end subroutine cmd_dired_next_subdir

    !> Go to previous subdirectory, regardless of level.
    !> When called interactively and not on a subdir line, go to this
    !> subdir's line.  Port of dired-aux.el:dired-prev-subdir
    !> (interactive).
    subroutine cmd_dired_prev_subdir(status)
        type(status_type), intent(out) :: status
        integer(int64) :: pos, arg
        if (.not. nilp(current_prefix_arg)) then
            arg = prefix_n()
        else if (len(dired_get_subdir()) > 0) then
            ! if on subdir start already, don't stay there!
            arg = 1
        else
            arg = 0
        end if
        pos = dired_prev_subdir(arg, status)
    end subroutine cmd_dired_prev_subdir

    !> Run Dired on parent directory of current directory.
    !> Find the parent directory either in this buffer or another buffer.
    !> Creates a buffer if necessary.
    !> If OTHER_WINDOW (the optional prefix arg), display the parent
    !> directory in another window.  Port of dired.el:dired-up-directory.
    recursive subroutine dired_up_directory(status, other_window)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: other_window
        character(len=:), allocatable :: dir, up
        integer(int64) :: pos
        logical :: other
        other = .false.
        if (present(other_window)) other = other_window
        dir = dired_current_directory(status)
        if (.not. status%ok()) return
        up = file_name_directory(directory_file_name(dir))
        if (dired_goto_file(directory_file_name(dir), status) /= 0) return
        if (.not. status%ok()) return
        ! Only try dired-goto-subdir if buffer has more than one dir.
        if (alist_size() > 1) then
            if (dired_goto_subdir(up) /= 0) return
        end if
        if (other) then
            call dired_other_window(up, status)
        else
            call dired_find_possibly_alternative_file(up, status)
        end if
        if (.not. status%ok()) return
        pos = dired_goto_file(dir, status)
    end subroutine dired_up_directory

    !> Run Dired on parent directory of current directory.
    !> Find the parent directory either in this buffer or another buffer.
    !> Creates a buffer if necessary.
    !> If OTHER-WINDOW (the optional prefix arg), display the parent
    !> directory in another window.  Port of dired.el:dired-up-directory
    !> (interactive).
    recursive subroutine cmd_dired_up_directory(status)
        type(status_type), intent(out) :: status
        call dired_up_directory(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_dired_up_directory

    ! ------------------------------------------------------------------
    ! Visiting
    ! ------------------------------------------------------------------

    !> Get the current line's file name, with an error if file does not
    !> exist.  Port of dired.el:dired-get-file-for-visit.
    recursive subroutine dired_get_file_for_visit(file_name, status)
        character(len=:), allocatable, intent(out) :: file_name
        type(status_type), intent(out) :: status
        logical :: found
        ! We pass t for second arg so that we don't get error for `.' and
        ! `..'.
        call dired_get_filename(file_name, found, status, no_error_if_not_filep=.true.)
        if (.not. status%ok()) return
        if (.not. found) then
            call set_error(status, FM_USER_ERROR, "No file on this line")
            return
        end if
        ! (file-name-sans-versions raw t) keeps backup versions: the name
        ! is unchanged.
        if (file_exists_p(file_name)) return
        if (file_symlink_p(file_name)) then
            call set_error(status, FM_GENERIC_ERROR, "File is a symlink to a nonexistent target")
        else
            call set_error(status, FM_GENERIC_ERROR, substitute_command_keys( &
                           "File no longer exists; type \<dired_mode_map>\[revert_buffer] to update Dired buffer"))
        end if
    end subroutine dired_get_file_for_visit

    !> Find FILE (with `find_file_run_dired' bound to true).  Port of
    !> dired.el:dired--find-possibly-alternative-file with dired--find-file
    !> (`dired_kill_when_opening_new_dired_buffer' nil).
    recursive subroutine dired_find_possibly_alternative_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        logical :: saved
        saved = find_file_run_dired
        find_file_run_dired = .true.
        call find_file(file, status)
        find_file_run_dired = saved
    end subroutine dired_find_possibly_alternative_file

    !> In Dired, visit the file or directory named on this line.  Port of
    !> dired.el:dired-find-file.
    recursive subroutine dired_find_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        call dired_get_file_for_visit(file, status)
        if (.not. status%ok()) return
        call dired_find_possibly_alternative_file(file, status)
    end subroutine dired_find_file

    !> In Dired, visit this file or directory in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of dired.el:dired-find-file-other-window.
    recursive subroutine dired_find_file_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        logical :: saved
        call dired_get_file_for_visit(file, status)
        if (.not. status%ok()) return
        saved = find_file_run_dired
        find_file_run_dired = .true.
        call find_file_other_window(file, status)
        find_file_run_dired = saved
    end subroutine dired_find_file_other_window

    !> In Dired, display this file or directory in another window.  Port of
    !> dired.el:dired-display-file.
    recursive subroutine dired_display_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(handle_type) :: buf, window
        type(display_alist_type) :: alist
        call dired_get_file_for_visit(file, status)
        if (.not. status%ok()) return
        call find_file_noselect(file, buf, status)
        if (.not. status%ok()) return
        ! (display-buffer BUFFER t)
        alist%inhibit_same_window = .true.
        call display_buffer(buf, window, status, alist=alist)
    end subroutine dired_display_file

    ! ------------------------------------------------------------------
    ! Marked files
    ! ------------------------------------------------------------------

    !> Return a string suitable for use in a Dired prompt.
    !> ARG_KIND and ARG_N are normally the prefix argument for the calling
    !> command.  FILES should be a list of file names.
    !>
    !> The return value has a form like "foo.txt", "[next 3 files]",
    !> or "* [3 files]".  Port of dired.el:dired-mark-prompt.
    function dired_marker_prompt(arg_kind, arg_n, files) result(prompt)
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        type(dired_string_type), intent(in) :: files(:)
        character(len=:), allocatable :: prompt
        if (size(files) == 1) then
            prompt = files(1)%s
        else if (arg_kind == DIRED_ARG_INTEGER) then
            ! abs(arg) = count
            prompt = "[next "//itoa(arg_n)//" files]"
        else
            prompt = dired_marker_char//" ["//itoa(int(size(files), int64))//" files]"
        end if
    end function dired_marker_prompt

    !> The kind and number of the prefix argument, as the commands on
    !> marked files take it ("P"): none, a number (or `-'), or another
    !> value (C-u).
    subroutine prefix_kind(arg_kind, arg_n)
        integer, intent(out) :: arg_kind
        integer(int64), intent(out) :: arg_n
        arg_n = 0
        if (nilp(current_prefix_arg)) then
            arg_kind = DIRED_ARG_NIL
        else if (consp(current_prefix_arg)) then
            arg_kind = DIRED_ARG_OTHER
        else
            arg_kind = DIRED_ARG_INTEGER
            arg_n = prefix_n()
        end if
    end subroutine prefix_kind

    !> The regexp matching a line marked with `dired_marker_char'.  Port
    !> of dired.el:dired-marker-regexp.
    function dired_marker_regexp() result(re)
        character(len=:), allocatable :: re
        re = "^"//regexp_quote(dired_marker_char)
    end function dired_marker_regexp

    !> This version skips non-file lines.  Call FUNCTION at the start of
    !> the next ARG file lines (previous -ARG if negative).  Port of
    !> dired.el:dired-repeat-over-lines.
    recursive subroutine dired_repeat_over_lines(arg_in, function, status)
        integer(int64), intent(in) :: arg_in
        interface
            recursive subroutine function(status)
                import :: status_type
                implicit none(type, external)
                type(status_type), intent(out) :: status
            end subroutine function
        end interface
        type(status_type), intent(out) :: status
        type(handle_type) :: pos
        type(status_type) :: st
        integer(int64) :: arg, opoint, p
        status = status_type()
        arg = arg_in
        call make_marker(pos)
        call beginning_of_line()
        do while (arg > 0 .and. .not. eobp())
            arg = arg - 1
            call beginning_of_line()
            do while (.not. eobp() .and. dired_between_files())
                call fwd_line(1_int64)
            end do
            opoint = point()
            call fwd_line(1_int64)
            call set_marker(pos, point() + 1, st)
            call goto_char(opoint)
            call function(status)
            call goto_char(opoint)
            if (.not. status%ok()) exit
            ! Advance to the next line--actually, to the line that *was*
            ! next.  (If FUNCTION inserted some new lines in between, skip
            ! them.)
            call goto_char(mpos(pos))
        end do
        do while (status%ok() .and. arg < 0 .and. .not. bobp())
            arg = arg + 1
            call fwd_line(-1_int64)
            do while (.not. bobp() .and. dired_between_files())
                call fwd_line(-1_int64)
            end do
            call beginning_of_line()
            opoint = point()
            call function(status)
            call goto_char(opoint)
        end do
        call release_marker(pos)
        p = dired_move_to_filename(st)
    end subroutine dired_repeat_over_lines

    !> Run BODY with point on each marked line (or the next ARG_N lines,
    !> or the current line), as dired.el's `dired_map_over_marks' macro
    !> does.  FOUND tells whether marked lines were found (or, for an
    !> integer or other ARG, true); BODY_RUNS counts the calls.  UPWARD
    !> means the calls went upwards (a negative integer ARG).  Port of
    !> dired.el:dired-map-over-marks (SHOW-PROGRESS nil).
    recursive subroutine dired_map_over_marks(body, arg_kind, arg_n, status, found, upward)
        interface
            recursive subroutine body(status)
                import :: status_type
                implicit none(type, external)
                type(status_type), intent(out) :: status
            end subroutine body
        end interface
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        type(status_type), intent(out) :: status
        logical, intent(out) :: found, upward
        type(value_type) :: saved_ro
        type(handle_type) :: next_position
        type(status_type) :: st
        integer(int64) :: opoint, p
        logical :: saved_fold
        status = status_type()
        found = .false.
        upward = .false.
        call inhibit_read_only_begin(saved_ro)
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        if (arg_kind == DIRED_ARG_INTEGER) then
            ! no save-excursion, want to move point.
            call dired_repeat_over_lines(arg_n, body, status)
            upward = arg_n < 0
            found = .true.
        else if (arg_kind == DIRED_ARG_OTHER) then
            ! non-nil, non-integer, non-marked ARG means use current file:
            call body(status)
            found = .true.
        else
            opoint = point()
            call goto_char(point_min())
            ! remember position of next marked file before BODY can insert
            ! lines before the just found file.
            if (re_search_forward_p(dired_marker_regexp())) then
                found = .true.
                call make_marker(next_position)
                call set_marker(next_position, point(), st)
                do while (next_position%index /= 0)
                    call goto_char(mpos(next_position))
                    call body(status)
                    if (.not. status%ok()) exit
                    ! move after last match
                    call goto_char(mpos(next_position))
                    call fwd_line(1_int64)
                    if (re_search_forward_p(dired_marker_regexp())) then
                        call set_marker(next_position, point(), st)
                    else
                        call release_marker(next_position)
                        next_position = null_handle()
                    end if
                end do
                if (next_position%index /= 0) call release_marker(next_position)
            end if
            call goto_char(opoint)
            if (.not. found .and. arg_kind /= DIRED_ARG_MARKED .and. status%ok()) call body(status)
        end if
        case_fold_search%value = saved_fold
        call inhibit_read_only_end(saved_ro)
        ! save-excursion loses, again
        p = dired_move_to_filename(st)
    end subroutine dired_map_over_marks

    !> Collect the file name of this line into the `map_files' results
    !> (`map_localp', `map_no_error').
    recursive subroutine collect_filename(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        logical :: found
        call dired_get_filename(file, found, status, map_localp, map_no_error)
        if (.not. status%ok()) return
        call push_result(file, found)
    end subroutine collect_filename

    !> Collect the file name and a marker at point (dired-do-delete's
    !> body).
    recursive subroutine collect_filename_and_marker(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(handle_type), allocatable :: grown(:)
        type(status_type) :: st
        logical :: found
        integer :: n
        call dired_get_filename(file, found, status)
        if (.not. status%ok()) return
        call push_result(file, found)
        n = map_n
        allocate (grown(n))
        if (n > 1) grown(1:n - 1) = map_markers(1:n - 1)
        call make_marker(grown(n))
        call set_marker(grown(n), point(), st)
        call move_alloc(grown, map_markers)
    end subroutine collect_filename_and_marker

    !> Append FILE (FOUND false for nil) to the `map_files' results.
    subroutine push_result(file, found)
        character(len=*), intent(in) :: file
        logical, intent(in) :: found
        type(dired_string_type), allocatable :: gf(:)
        logical, allocatable :: gl(:)
        allocate (gf(map_n + 1), gl(map_n + 1))
        if (map_n > 0) then
            gf(1:map_n) = map_files(1:map_n)
            gl(1:map_n) = map_found(1:map_n)
        end if
        gf(map_n + 1)%s = file
        gl(map_n + 1) = found
        call move_alloc(gf, map_files)
        call move_alloc(gl, map_found)
        map_n = map_n + 1
    end subroutine push_result

    !> Start collecting results.
    subroutine reset_results()
        if (allocated(map_files)) deallocate (map_files)
        if (allocated(map_found)) deallocate (map_found)
        if (allocated(map_markers)) deallocate (map_markers)
        allocate (map_files(0), map_found(0), map_markers(0))
        map_n = 0
    end subroutine reset_results

    !> Return the marked files' names as list of strings.
    !> The list is in the same order as the Dired buffer text, that is, the car
    !>   is the first marked file.
    !> Values returned are normally absolute file names.
    !> LOCALP is as in `dired_get_filename'.
    !> ARG_KIND and ARG_N, if not DIRED_ARG_NIL, specify files near point
    !>  to return instead of marked files.  They usually come from the
    !>  prefix argument of the caller.
    !>   If ARG is an integer, return the next ARG files (previous if ARG is
    !>    negative).
    !>   If ARG is DIRED_ARG_MARKED, return only marked files; return none
    !>    if none are marked.
    !>   If ARG is DIRED_ARG_OTHER, return the current file name.
    !>   If no files are marked, and ARG is DIRED_ARG_NIL, also return the
    !>   current file name.
    !>
    !> If DISTINGUISH_ONE_MARKED is true, then if we find just one marked
    !> file, ONE_MARKED is true.
    !>
    !> If ERROR is present, signal it (a message) when the list of found
    !> files is empty.  Port of dired.el:dired-get-marked-files (no
    !> FILTER).
    recursive subroutine dired_get_marked_files(files, status, localp, arg_kind, arg_n, distinguish_one_marked, &
                                                one_marked, error)
        type(dired_string_type), allocatable, intent(out) :: files(:)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: localp, arg_kind
        integer(int64), intent(in), optional :: arg_n
        logical, intent(in), optional :: distinguish_one_marked
        logical, intent(out), optional :: one_marked
        character(len=*), intent(in), optional :: error
        integer :: kind, k, n
        integer(int64) :: num
        integer(int64) :: opoint
        logical :: found, upward, distinguish
        kind = DIRED_ARG_NIL
        if (present(arg_kind)) kind = arg_kind
        num = 0
        if (present(arg_n)) num = arg_n
        distinguish = .false.
        if (present(distinguish_one_marked)) distinguish = distinguish_one_marked
        if (present(one_marked)) one_marked = .false.
        call reset_results()
        map_localp = DIRED_LOCALP_NIL
        if (present(localp)) map_localp = localp
        map_no_error = .true.
        opoint = point()
        call dired_map_over_marks(collect_filename, kind, num, status, found, upward)
        if (kind /= DIRED_ARG_INTEGER) call goto_char(opoint)
        allocate (files(0))
        if (.not. status%ok()) return
        ! (delq nil ...) in buffer order
        n = count(map_found(1:map_n))
        deallocate (files)
        allocate (files(n))
        n = 0
        if (upward) then
            do k = map_n, 1, -1
                if (.not. map_found(k)) cycle
                n = n + 1
                files(n)%s = map_files(k)%s
            end do
        else
            do k = 1, map_n
                if (.not. map_found(k)) cycle
                n = n + 1
                files(n)%s = map_files(k)%s
            end do
        end if
        if (distinguish .and. found .and. kind == DIRED_ARG_NIL .and. map_n == 1 .and. present(one_marked)) then
            one_marked = n == 1
        end if
        if (size(files) == 0 .and. present(error)) then
            if (len(error) > 0) then
                call set_error(status, FM_USER_ERROR, error)
            else
                call set_error(status, FM_USER_ERROR, "No files specified")
            end if
        end if
    end subroutine dired_get_marked_files

    ! ------------------------------------------------------------------
    ! Marking
    ! ------------------------------------------------------------------

    !> Whether Dired marking commands should act on region.  Port of
    !> dired.el:dired-mark--region-use-p.
    logical function dired_mark_region_use_p() result(use_region)
        integer(int64) :: b, e
        type(status_type) :: st
        use_region = .false.
        if (dired_mark_region == DIRED_MARK_REGION_NIL) return
        if (.not. region_active_p()) return
        call region_beginning(b, st)
        call region_end(e, st)
        use_region = e > b
    end function dired_mark_region_use_p

    !> The value of the region beginning aligned to Dired file lines.  Port
    !> of dired.el:dired-mark--region-beginning.
    integer(int64) function dired_mark_region_beginning() result(pos)
        integer(int64) :: opoint, b
        type(status_type) :: st
        pos = point_min()
        if (.not. dired_mark_region_use_p()) return
        opoint = point()
        call region_beginning(b, st)
        call goto_char(b)
        pos = bol()
        call goto_char(opoint)
    end function dired_mark_region_beginning

    !> The value of the region end aligned to Dired file lines.  Port of
    !> dired.el:dired-mark--region-end.
    integer(int64) function dired_mark_region_end() result(pos)
        integer(int64) :: opoint, e
        type(status_type) :: st
        pos = point_max()
        if (.not. dired_mark_region_use_p()) return
        opoint = point()
        call region_end(e, st)
        call goto_char(e)
        pos = region_end_line_position()
        call goto_char(opoint)
    end function dired_mark_region_end

    !> At the region's end: the end of its line when the file there is in
    !> the region, else the start of its line (dired-mark-region).
    integer(int64) function region_end_line_position() result(pos)
        type(value_type) :: v
        type(status_type) :: st
        logical :: whole
        if (dired_mark_region == DIRED_MARK_REGION_LINE) then
            whole = .not. bolp()
        else
            whole = .false.
            if (point() > point_min()) then
                call get_text_property(point() - 1, sym("dired_filename"), v, st)
                whole = .not. nilp(v)
            end if
        end if
        if (whole) then
            pos = eol()
        else
            pos = bol()
        end if
    end function region_end_line_position

    !> Mark files for PREDICATE, according to `dired_marker_char'.
    !> PREDICATE is evaluated on each line, with point at beginning of line.
    !> MSG is a noun phrase for the type of files being marked.
    !> It should end with a noun that can be pluralized by adding `s'.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region if `dired_mark_region' is true.  Otherwise, operate
    !> on the whole buffer.
    !>
    !> Return value is the number of files marked (0 if none were marked).
    !> Port of dired.el:dired-mark-if.
    recursive function dired_mark_if(predicate, msg, status) result(count)
        interface
            recursive logical function predicate()
                implicit none(type, external)
            end function predicate
        end interface
        character(len=*), intent(in) :: msg
        type(status_type), intent(out) :: status
        integer(int64) :: count
        type(value_type) :: saved_ro
        type(handle_type) :: end_marker
        type(status_type) :: st
        character(len=:), allocatable :: action, region
        integer(int64) :: opoint, beg
        logical :: use_region_p
        status = status_type()
        call inhibit_read_only_begin(saved_ro)
        use_region_p = dired_mark_region_use_p()
        beg = dired_mark_region_beginning()
        call make_marker(end_marker)
        call set_marker(end_marker, dired_mark_region_end(), st)
        opoint = point()
        count = 0
        region = ""
        if (use_region_p) region = " in region"
        if (len(msg) > 0) then
            if (dired_marker_char == " ") then
                action = "Unmarking"
            else if (dired_del_marker == dired_marker_char) then
                action = "Flagging"
            else
                action = "Marking"
            end if
            if (dired_del_marker == dired_marker_char) then
                call message(action//" "//msg//"s for deletion"//region//"...")
            else
                call message(action//" "//msg//"s"//region//"...")
            end if
        end if
        call goto_char(beg)
        do while (point() < mpos(end_marker))
            if (predicate()) then
                if (char_at() /= dired_marker_char) then
                    call replace_char_at_point(dired_marker_char, status)
                    if (.not. status%ok()) exit
                    count = count + 1
                end if
            end if
            call fwd_line(1_int64)
        end do
        if (len(msg) > 0 .and. status%ok()) then
            if (dired_marker_char == " ") then
                action = "un"
            else
                action = ""
            end if
            if (dired_marker_char == dired_del_marker) then
                action = action//"flagged"
            else
                action = action//"marked"
            end if
            call message(itoa(count)//" "//msg//dired_plural_s(count)//" "//action//region)
        end if
        call release_marker(end_marker)
        call goto_char(opoint)
        call inhibit_read_only_end(saved_ro)
    end function dired_mark_if

    !> Mark (with `dired_marker_char') the files of the lines from START
    !> to END, except `.' and `..'.  Port of
    !> dired.el:dired-mark-files-in-region.
    recursive subroutine dired_mark_files_in_region(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        type(handle_type) :: end_marker
        character(len=:), allocatable :: fn
        type(status_type) :: st
        logical :: found
        status = status_type()
        if (start > end) then
            call set_error(status, FM_GENERIC_ERROR, "Start > End")
            return
        end if
        call inhibit_read_only_begin(saved_ro)
        call make_marker(end_marker)
        call set_marker(end_marker, end, st)
        ! assumed at beginning of line
        call goto_char(start)
        do while (point() < mpos(end_marker))
            ! Skip subdir line and following garbage like the `total' line:
            do while (point() < mpos(end_marker) .and. dired_between_files())
                call fwd_line(1_int64)
            end do
            if (.not. looking_at_p(dired_re_dot)) then
                call dired_get_filename(fn, found, st, no_error_if_not_filep=.true.)
                if (found) then
                    call replace_char_at_point(dired_marker_char, status)
                    if (.not. status%ok()) exit
                end if
            end if
            call fwd_line(1_int64)
        end do
        call release_marker(end_marker)
        call inhibit_read_only_end(saved_ro)
    end subroutine dired_mark_files_in_region

    !> The body of `dired_mark' for one line: mark it with the current
    !> `dired_marker_char' unless it is `.' or `..' (unless a symlink).
    recursive subroutine mark_this_line(status)
        type(status_type), intent(out) :: status
        integer(int64) :: opoint
        logical :: dot_ok, found
        status = status_type()
        dot_ok = .not. looking_at_p(dired_re_dot)
        if (.not. dot_ok) then
            ! Don't skip symlinks to ".", "..", etc.
            opoint = point()
            found = re_search_forward_p(dired_permission_flags_regexp)
            dot_ok = char_at(mbeg(1)) == "l"
            call goto_char(opoint)
        end if
        if (dot_ok .or. dired_marker_char /= dired_del_marker) then
            call replace_char_at_point(repeat_char, status)
        end if
    end subroutine mark_this_line

    !> Mark the file at point in the Dired buffer.
    !> If the region is active in Transient Mark mode, mark all files
    !> in the region if `dired_mark_region' is true.
    !> Otherwise, with a prefix arg, mark files on the next ARG lines.
    !>
    !> If on a subdir headerline, mark all its files except `.' and `..'.
    !>
    !> Use \[dired_unmark_all_files] to remove all marks
    !> and \[dired_unmark] on a subdir to remove the marks in
    !> this subdir.  Port of dired.el:dired-mark (ARG a number;
    !> INTERACTIVE for the region).
    recursive subroutine dired_mark_1(arg, interactive, status)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: interactive
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        integer(int64) :: opoint, b, e, end
        status = status_type()
        if (interactive .and. dired_mark_region_use_p()) then
            ! Mark files in the active region.
            opoint = point()
            call region_beginning(b, status)
            call region_end(e, status)
            call goto_char(e)
            end = region_end_line_position()
            call goto_char(b)
            call dired_mark_files_in_region(bol(), end, status)
            call goto_char(opoint)
        else if (len(dired_get_subdir()) > 0) then
            ! Mark subdir files from the subdir headerline.
            opoint = point()
            call dired_mark_subdir_files(status)
            call goto_char(opoint)
        else
            ! Mark the current (or next ARG) files.
            call inhibit_read_only_begin(saved_ro)
            repeat_char = dired_marker_char
            call dired_repeat_over_lines(arg, mark_this_line, status)
            call inhibit_read_only_end(saved_ro)
        end if
    end subroutine dired_mark_1

    !> Mark the file at point in the Dired buffer.
    !> If the region is active in Transient Mark mode, mark all files
    !> in the region if `dired_mark_region' is true.
    !> Otherwise, with a prefix arg, mark files on the next ARG lines.
    !>
    !> If on a subdir headerline, mark all its files except `.' and `..'.
    !>
    !> Use \[dired_unmark_all_files] to remove all marks
    !> and \[dired_unmark] on a subdir to remove the marks in
    !> this subdir.  Port of dired.el:dired-mark (interactive).
    recursive subroutine cmd_dired_mark(status)
        type(status_type), intent(out) :: status
        call dired_mark_1(prefix_n(), .true., status)
    end subroutine cmd_dired_mark

    !> Unmark the file at point in the Dired buffer.
    !> If the region is active, unmark all files in the region.
    !> Otherwise, with a prefix arg, unmark files on the next ARG lines.
    !>
    !> If looking at a subdir, unmark all its files except `.' and `..'.
    !> If the region is active in Transient Mark mode, unmark all files
    !> in the active region.  Port of dired.el:dired-unmark.
    recursive subroutine dired_unmark(arg, interactive, status)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: interactive
        type(status_type), intent(out) :: status
        character :: saved
        saved = dired_marker_char
        dired_marker_char = " "
        call dired_mark_1(arg, interactive, status)
        dired_marker_char = saved
    end subroutine dired_unmark

    !> Unmark the file at point in the Dired buffer.
    !> If the region is active, unmark all files in the region.
    !> Otherwise, with a prefix arg, unmark files on the next ARG lines.
    !>
    !> If looking at a subdir, unmark all its files except `.' and `..'.
    !> If the region is active in Transient Mark mode, unmark all files
    !> in the active region.  Port of dired.el:dired-unmark (interactive).
    recursive subroutine cmd_dired_unmark(status)
        type(status_type), intent(out) :: status
        call dired_unmark(prefix_n(), .true., status)
    end subroutine cmd_dired_unmark

    !> In Dired, flag the current line's file for deletion.
    !> If the region is active, flag all files in the region.
    !> Otherwise, with a prefix arg, flag files on the next ARG lines.
    !>
    !> If on a subdir headerline, flag all its files except `.' and `..'.
    !> If the region is active in Transient Mark mode, flag all files
    !> in the active region.  Port of dired.el:dired-flag-file-deletion.
    recursive subroutine dired_flag_file_deletion(arg, interactive, status)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: interactive
        type(status_type), intent(out) :: status
        character :: saved
        saved = dired_marker_char
        dired_marker_char = dired_del_marker
        call dired_mark_1(arg, interactive, status)
        dired_marker_char = saved
    end subroutine dired_flag_file_deletion

    !> In Dired, flag the current line's file for deletion.
    !> If the region is active, flag all files in the region.
    !> Otherwise, with a prefix arg, flag files on the next ARG lines.
    !>
    !> If on a subdir headerline, flag all its files except `.' and `..'.
    !> If the region is active in Transient Mark mode, flag all files
    !> in the active region.  Port of dired.el:dired-flag-file-deletion
    !> (interactive).
    recursive subroutine cmd_dired_flag_file_deletion(status)
        type(status_type), intent(out) :: status
        call dired_flag_file_deletion(prefix_n(), .true., status)
    end subroutine cmd_dired_flag_file_deletion

    !> In Dired, move up lines and remove marks or deletion flags there.
    !> Optional prefix ARG says how many lines to unmark/unflag; default
    !> is one line.
    !> If the region is active in Transient Mark mode, unmark all files
    !> in the active region.  Port of dired.el:dired-unmark-backward.
    recursive subroutine cmd_dired_unmark_backward(status)
        type(status_type), intent(out) :: status
        call dired_unmark(-prefix_n(), .true., status)
    end subroutine cmd_dired_unmark_backward

    !> Toggle marks: marked files become unmarked, and vice versa.
    !> Flagged files (indicated with flags such as `C' and `D', not
    !> with `*') are not affected, and `.' and `..' are never toggled.
    !> As always, hidden subdirs are not affected.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region if `dired_mark_region' is true.  Otherwise, operate
    !> on the whole buffer.  Port of dired.el:dired-toggle-marks.
    recursive subroutine dired_toggle_marks(status)
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        type(handle_type) :: end_marker
        type(status_type) :: st
        integer(int64) :: opoint
        status = status_type()
        opoint = point()
        call inhibit_read_only_begin(saved_ro)
        call make_marker(end_marker)
        call set_marker(end_marker, dired_mark_region_end(), st)
        call goto_char(dired_mark_region_beginning())
        do while (point() < mpos(end_marker))
            if (.not. (dired_between_files() .or. looking_at_p(dired_re_dot))) then
                ! use subst instead of insdel because it does not move the
                ! gap and thus should be faster and because other characters
                ! are left alone automatically
                if (char_at() == " ") then
                    call subst_char_in_region(point(), point() + 1, iachar(" "), iachar(dired_marker_char), status)
                else
                    call subst_char_in_region(point(), point() + 1, iachar(dired_marker_char), iachar(" "), status)
                end if
                if (.not. status%ok()) exit
            end if
            call fwd_line(1_int64)
        end do
        call release_marker(end_marker)
        call inhibit_read_only_end(saved_ro)
        call goto_char(opoint)
    end subroutine dired_toggle_marks

    !> The predicate of `dired_mark_files_regexp'.
    recursive logical function regexp_file_p() result(p)
        character(len=:), allocatable :: fn
        type(status_type) :: st
        logical :: found
        p = .false.
        if (looking_at_p(dired_re_dot)) return
        ! empty line
        if (eolp()) return
        if (dired_hidden_p()) return
        call dired_get_filename(fn, found, st, DIRED_LOCALP_T, .true.)
        if (.not. found) return
        p = string_match_p(mark_regexp, fn)
    end function regexp_file_p

    !> Mark all files matching REGEXP for use in later commands.
    !> A prefix argument means to unmark them instead.
    !> `.' and `..' are never marked.
    !>
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.
    !>
    !> REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\.o$' for
    !> object files--just `.o' will mark more than you might think.  Port
    !> of dired.el:dired-mark-files-regexp.
    recursive subroutine dired_mark_files_regexp(regexp, status, marker_char)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        character, intent(in), optional :: marker_char
        character :: saved
        integer(int64) :: count
        saved = dired_marker_char
        if (present(marker_char)) dired_marker_char = marker_char
        mark_regexp = regexp
        count = dired_mark_if(regexp_file_p, "matching file", status)
        dired_marker_char = saved
    end subroutine dired_mark_files_regexp

    !> Read a regexp in the minibuffer for the marking commands, with
    !> `dired_regexp_history'.
    recursive subroutine read_dired_regexp(prompt, regexp, status, file_defaults)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: regexp
        type(status_type), intent(out) :: status
        logical, intent(in) :: file_defaults
        type(minibuffer_string_type), allocatable :: defaults(:)
        character(len=:), allocatable :: fn, ext
        type(status_type) :: st
        logical :: found
        if (file_defaults) then
            ! Add more suggestions into the default list
            allocate (defaults(1))
            defaults(1)%s = ""
            call dired_get_filename(fn, found, st, DIRED_LOCALP_T, .true.)
            if (found) call add_default(fn)
            call dired_get_filename(fn, found, st, DIRED_LOCALP_NIL, .true.)
            if (found) then
                call file_name_extension(fn, ext, found, period=.true.)
                call add_default(regexp_quote(ext)//"\'")
            end if
            call read_regexp(prompt, regexp, status, defaults=defaults, history="dired_regexp_history")
        else
            call read_regexp(prompt, regexp, status, history="dired_regexp_history")
        end if
    contains

        subroutine add_default(s)
            character(len=*), intent(in) :: s
            type(minibuffer_string_type), allocatable :: grown(:)
            allocate (grown(size(defaults) + 1))
            grown(1:size(defaults)) = defaults
            grown(size(grown))%s = s
            call move_alloc(grown, defaults)
        end subroutine add_default
    end subroutine read_dired_regexp

    !> Mark all files matching REGEXP for use in later commands.
    !> A prefix argument means to unmark them instead.
    !> `.' and `..' are never marked.
    !>
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.
    !>
    !> REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\.o$' for
    !> object files--just `.o' will mark more than you might think.  Port
    !> of dired.el:dired-mark-files-regexp (interactive).
    recursive subroutine cmd_dired_mark_files_regexp(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp, verb
        logical :: unmark
        unmark = .not. nilp(current_prefix_arg)
        verb = "Mark"
        if (unmark) verb = "Unmark"
        call read_dired_regexp(verb//" files (regexp): ", regexp, status, .true.)
        if (.not. status%ok()) return
        if (unmark) then
            call dired_mark_files_regexp(regexp, status, " ")
        else
            call dired_mark_files_regexp(regexp, status)
        end if
    end subroutine cmd_dired_mark_files_regexp

    !> The predicate of `dired_mark_files_containing_regexp'.
    recursive logical function containing_regexp_p() result(p)
        character(len=:), allocatable :: fn
        type(handle_type) :: prebuf, old, tmp
        type(status_type) :: st
        integer(int64) :: opoint, pos, inserted
        logical :: found
        p = .false.
        if (looking_at_p(dired_re_dot)) return
        ! empty line
        if (eolp()) return
        if (dired_hidden_p()) return
        call dired_get_filename(fn, found, st, DIRED_LOCALP_NIL, .true.)
        if (.not. found) return
        if (.not. file_readable_p(fn) .or. file_directory_p(fn)) return
        prebuf = get_file_buffer(expand(fn))
        call message("Checking "//fn)
        old = current_buffer()
        ! For now we do it inside emacs.  Grep might be better if there
        ! are a lot of files.
        if (prebuf%index /= 0 .and. .not. dired_always_read_filesystem) then
            call set_buffer(prebuf, st)
            opoint = point()
            call goto_char(point_min())
            call re_search_forward(mark_regexp, pos, st, noerror=SEARCH_NOERROR)
            p = st%ok() .and. pos > 0
            call goto_char(opoint)
        else
            call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, st)
            call set_buffer(tmp, st)
            call insert_file_contents(fn, .false., inserted, st)
            call goto_char(point_min())
            call re_search_forward(mark_regexp, pos, st, noerror=SEARCH_NOERROR)
            p = st%ok() .and. pos > 0
            call kill_buffer(tmp, st)
        end if
        call set_buffer(old, st)
    end function containing_regexp_p

    !> Mark all files with contents containing REGEXP for use in later commands.
    !> A prefix argument means to unmark them instead.
    !> `.' and `..' are never marked.
    !>
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.
    !>
    !> Note that if a file is visited in an Emacs buffer, and
    !> `dired_always_read_filesystem' is false, this command will
    !> look in the buffer without revisiting the file, so the results might
    !> be inconsistent with the file on disk if its contents has changed
    !> since it was last visited.  Port of
    !> dired.el:dired-mark-files-containing-regexp.
    recursive subroutine dired_mark_files_containing_regexp(regexp, status, marker_char)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        character, intent(in), optional :: marker_char
        character :: saved
        integer(int64) :: count
        saved = dired_marker_char
        if (present(marker_char)) dired_marker_char = marker_char
        mark_regexp = regexp
        count = dired_mark_if(containing_regexp_p, "matching file", status)
        dired_marker_char = saved
    end subroutine dired_mark_files_containing_regexp

    !> Mark all files with contents containing REGEXP for use in later commands.
    !> A prefix argument means to unmark them instead.
    !> `.' and `..' are never marked.  Port of
    !> dired.el:dired-mark-files-containing-regexp (interactive).
    recursive subroutine cmd_dired_mark_files_containing_regexp(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp, verb
        logical :: unmark
        unmark = .not. nilp(current_prefix_arg)
        verb = "Mark"
        if (unmark) verb = "Unmark"
        call read_dired_regexp(verb//" files containing (regexp): ", regexp, status, .false.)
        if (.not. status%ok()) return
        if (unmark) then
            call dired_mark_files_containing_regexp(regexp, status, " ")
        else
            call dired_mark_files_containing_regexp(regexp, status)
        end if
    end subroutine cmd_dired_mark_files_containing_regexp

    !> In Dired, flag all files containing the specified REGEXP for deletion.
    !> The match is against the non-directory part of the filename.  Use `^'
    !>   and `$' to anchor matches.  Exclude subdirs by hiding them.
    !> `.' and `..' are never flagged.  Port of
    !> dired.el:dired-flag-files-regexp (interactive).
    recursive subroutine cmd_dired_flag_files_regexp(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        call read_dired_regexp("Flag for deletion (regexp): ", regexp, status, .false.)
        if (.not. status%ok()) return
        call dired_mark_files_regexp(regexp, status, dired_del_marker)
    end subroutine cmd_dired_flag_files_regexp

    !> Flag for deletion all files that match `dired_garbage_files_regexp'.
    !> Port of dired.el:dired-flag-garbage-files.
    recursive subroutine cmd_dired_flag_garbage_files(status)
        type(status_type), intent(out) :: status
        call dired_mark_files_regexp(dired_garbage_files_regexp, status, dired_del_marker)
    end subroutine cmd_dired_flag_garbage_files

    recursive logical function symlink_line_p()
        symlink_line_p = looking_at_p(dired_re_sym)
    end function symlink_line_p

    recursive logical function directory_line_p()
        directory_line_p = looking_at_p(dired_re_dir) .and. .not. looking_at_p(dired_re_dot)
    end function directory_line_p

    recursive logical function executable_line_p()
        executable_line_p = looking_at_p(dired_re_exe)
    end function executable_line_p

    !> Run `dired_mark_if' with PREDICATE and MSG, unmarking with a prefix
    !> argument, else with marker MARKER.
    recursive subroutine mark_if_command(predicate, msg, marker, status)
        interface
            recursive logical function predicate()
                implicit none(type, external)
            end function predicate
        end interface
        character(len=*), intent(in) :: msg
        character, intent(in) :: marker
        type(status_type), intent(out) :: status
        character :: saved
        integer(int64) :: count
        saved = dired_marker_char
        if (nilp(current_prefix_arg)) then
            dired_marker_char = marker
        else
            dired_marker_char = " "
        end if
        count = dired_mark_if(predicate, msg, status)
        dired_marker_char = saved
    end subroutine mark_if_command

    !> Mark all symbolic links.
    !> With prefix argument, unmark or unflag all those files.
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.  Port of
    !> dired.el:dired-mark-symlinks.
    recursive subroutine cmd_dired_mark_symlinks(status)
        type(status_type), intent(out) :: status
        call mark_if_command(symlink_line_p, "symbolic link", dired_marker_char, status)
    end subroutine cmd_dired_mark_symlinks

    !> Mark all directory file lines except `.' and `..'.
    !> With prefix argument, unmark or unflag all those files.
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.  Port of
    !> dired.el:dired-mark-directories.
    recursive subroutine cmd_dired_mark_directories(status)
        type(status_type), intent(out) :: status
        call mark_if_command(directory_line_p, "directory file", dired_marker_char, status)
    end subroutine cmd_dired_mark_directories

    !> Mark all executable files.
    !> With prefix argument, unmark or unflag all those files.
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.  Port of
    !> dired.el:dired-mark-executables.
    recursive subroutine cmd_dired_mark_executables(status)
        type(status_type), intent(out) :: status
        call mark_if_command(executable_line_p, "executable file", dired_marker_char, status)
    end subroutine cmd_dired_mark_executables

    !> The predicate of `dired_flag_auto_save_files'.
    recursive logical function auto_save_line_p() result(p)
        character(len=:), allocatable :: fn
        type(status_type) :: st
        integer(int64) :: opoint
        logical :: found
        p = .false.
        ! It is less than general to check for # here, but it's the only
        ! way this runs fast enough.
        opoint = point()
        call end_of_line()
        if (char_before_point() == "#") then
            p = .true.
        else if (char_before_point() == "*") then
            ! Handle executables in case of -F option.
            call goto_char(point() - 1)
            p = char_before_point() == "#"
        end if
        call goto_char(opoint)
        if (.not. p) return
        p = .not. looking_at_p(dired_re_dir)
        if (.not. p) return
        call dired_get_filename(fn, found, st, DIRED_LOCALP_T, .true.)
        p = .false.
        if (found) p = auto_save_file_name_p(file_name_nondirectory(fn))
    end function auto_save_line_p

    !> Flag for deletion files whose names suggest they are auto save files.
    !> A prefix argument says to unmark or unflag those files instead.
    !> If the region is active in Transient Mark mode, flag files
    !> only in the active region if `dired_mark_region' is true.  Port of
    !> dired.el:dired-flag-auto-save-files.
    recursive subroutine cmd_dired_flag_auto_save_files(status)
        type(status_type), intent(out) :: status
        call mark_if_command(auto_save_line_p, "auto save file", dired_del_marker, status)
    end subroutine cmd_dired_flag_auto_save_files

    !> The predicate of `dired_flag_backup_files'.
    recursive logical function backup_line_p() result(p)
        character(len=:), allocatable :: fn
        type(status_type) :: st
        integer(int64) :: opoint
        logical :: found
        ! Don't call backup-file-name-p unless the last character looks
        ! like it might be the end of a backup file name.
        opoint = point()
        call end_of_line()
        ! Handle executables in case of -F option.
        if (char_before_point() == "*") call goto_char(point() - 1)
        p = char_before_point() == "~"
        call goto_char(opoint)
        if (.not. p) return
        p = .not. looking_at_p(dired_re_dir)
        if (.not. p) return
        call dired_get_filename(fn, found, st, DIRED_LOCALP_T, .true.)
        p = .false.
        if (found) p = backup_file_name_p(fn)
    end function backup_line_p

    !> Flag all backup files (names ending with `~') for deletion.
    !> With prefix argument, unmark or unflag these files.
    !> If the region is active in Transient Mark mode, flag files
    !> only in the active region if `dired_mark_region' is true.  Port of
    !> dired.el:dired-flag-backup-files.
    recursive subroutine cmd_dired_flag_backup_files(status)
        type(status_type), intent(out) :: status
        call mark_if_command(backup_line_p, "backup file", dired_del_marker, status)
    end subroutine cmd_dired_flag_backup_files

    !> Read a character in the echo area after PROMPT (read-char): C-g
    !> quits, other non-characters are an error.
    subroutine read_char(prompt, c, status)
        character(len=*), intent(in) :: prompt
        integer(int64), intent(out) :: c
        type(status_type), intent(out) :: status
        type(value_type) :: ev
        logical :: have
        c = 0
        if (len(prompt) > 0) call message(prompt)
        cursor_in_echo_area = .true.
        call read_event(ev, have, status)
        cursor_in_echo_area = .false.
        if (.not. status%ok()) return
        if (.not. have) then
            call set_error(status, FM_QUIT)
        else if (.not. fixnump(ev)) then
            call set_error(status, FM_GENERIC_ERROR, "Non-character input-event")
        else if (xfixnum(ev) == 7) then
            call set_error(status, FM_QUIT)
        else if (xfixnum(ev) < 0 .or. xfixnum(ev) > int(z'3FFFFF', int64)) then
            call set_error(status, FM_GENERIC_ERROR, "Non-character input-event")
        else
            c = xfixnum(ev)
        end if
    end subroutine read_char

    !> Change all OLD marks to NEW marks.
    !> OLD and NEW are both characters used to mark files.  Port of
    !> dired.el:dired-change-marks (ASCII marks).
    recursive subroutine dired_change_marks(old, new, status)
        character, intent(in) :: old, new
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        character(len=:), allocatable :: fn
        type(status_type) :: st
        integer(int64) :: opoint, found_pos
        logical :: ok
        status = status_type()
        if (new == achar(13) .or. old == achar(13)) then
            if (new == achar(13)) then
                call set_error(status, FM_USER_ERROR, format_message("Invalid mark character: `")//new// &
                               format_message("'"))
            else
                call set_error(status, FM_USER_ERROR, format_message("Invalid mark character: `")//old// &
                               format_message("'"))
            end if
            return
        end if
        opoint = point()
        call inhibit_read_only_begin(saved_ro)
        call goto_char(point_min())
        do
            call search_forward(LF//old, found_pos, st, noerror=SEARCH_NOERROR)
            if (found_pos <= 0) exit
            ok = .true.
            if (old == " ") call dired_get_filename(fn, ok, st, DIRED_LOCALP_NO_DIR, .true.)
            if (ok) call subst_char_in_region(mbeg(0), mend(0), iachar(old), iachar(new), status)
        end do
        call inhibit_read_only_end(saved_ro)
        call goto_char(opoint)
    end subroutine dired_change_marks

    !> Change all OLD marks to NEW marks.
    !> OLD and NEW are both characters used to mark files.  Port of
    !> dired.el:dired-change-marks (interactive).
    recursive subroutine cmd_dired_change_marks(status)
        type(status_type), intent(out) :: status
        integer(int64) :: old, new
        call read_char("Change (old mark): ", old, status)
        if (.not. status%ok()) return
        call read_char("Change "//achar(min(old, 127_int64))//" marks to (new mark): ", new, status)
        if (.not. status%ok()) return
        if (old > 127 .or. new > 127) then
            call set_error(status, FM_USER_ERROR, "Invalid mark character")
            return
        end if
        call dired_change_marks(achar(old), achar(new), status)
    end subroutine cmd_dired_change_marks

    !> Remove a specific mark (or any mark) from every file.
    !> After this command, type the mark character to remove,
    !> or type RET to remove all marks.  Port of
    !> With ARG true, query for each marked file.  Port of
    !> dired.el:dired-unmark-all-files.
    recursive subroutine dired_unmark_all_files(mark, status, arg)
        character, intent(in) :: mark
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: arg
        character(len=:), allocatable :: file, help
        integer :: query
        logical :: ask, answer, file_found
        type(value_type) :: saved_ro
        type(status_type) :: st
        integer(int64) :: opoint, count, found_pos
        logical :: saved_fold, found
        status = status_type()
        opoint = point()
        count = 0
        ask = .false.
        if (present(arg)) ask = arg
        query = 0
        help = substitute_command_keys("Type \`SPC' or \`y' to unmark one file, \`DEL' or \`n' to skip to next,"//LF// &
                                       "\`!' to unmark all remaining files with no more questions.")
        call inhibit_read_only_begin(saved_ro)
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        call goto_char(point_min())
        do
            if (mark == achar(13)) then
                found = re_search_forward_p(dired_re_mark)
            else
                call search_forward(LF//mark, found_pos, st, noerror=SEARCH_NOERROR)
                found = found_pos > 0
            end if
            if (.not. found) exit
            if (ask) then
                found_pos = point()
                call dired_get_filename(file, file_found, st, DIRED_LOCALP_T, .true.)
                call goto_char(found_pos)
                if (.not. (st%ok() .and. file_found)) cycle
                call dired_query(query, format_message("Unmark file `")//file//format_message("'? "), answer, status, &
                                 help_form=help)
                call goto_char(found_pos)
                if (.not. status%ok()) exit
                if (.not. answer) cycle
            end if
            call subst_char_in_region(point() - 1, point(), iachar(char_before_point()), iachar(" "), status)
            count = count + 1
        end do
        case_fold_search%value = saved_fold
        call inhibit_read_only_end(saved_ro)
        if (count == 1) then
            call message("1 mark removed")
        else
            call message(itoa(count)//" marks removed")
        end if
        call goto_char(opoint)
    end subroutine dired_unmark_all_files

    !> Remove a specific mark (or any mark) from every file.
    !> After this command, type the mark character to remove,
    !> or type RET to remove all marks.
    !> With prefix arg, query for each marked file.
    !> Type \[help_command] at that time for help.  Port of
    !> dired.el:dired-unmark-all-files (interactive).
    recursive subroutine cmd_dired_unmark_all_files(status)
        type(status_type), intent(out) :: status
        integer(int64) :: mark
        call read_char("Remove marks (RET means all): ", mark, status)
        if (.not. status%ok()) return
        if (mark > 127) then
            call set_error(status, FM_GENERIC_ERROR, "Non-ASCII mark character")
            return
        end if
        call dired_unmark_all_files(achar(mark), status,.not. nilp(current_prefix_arg))
    end subroutine cmd_dired_unmark_all_files

    !> Remove all marks from all files in the Dired buffer.  Port of
    !> dired.el:dired-unmark-all-marks.
    recursive subroutine cmd_dired_unmark_all_marks(status)
        type(status_type), intent(out) :: status
        call dired_unmark_all_files(achar(13), status)
    end subroutine cmd_dired_unmark_all_marks

    !> Move to the ARGth next marked file.
    !> ARG is the numeric prefix argument and defaults to 1.
    !> If WRAP is true, which happens interactively, wrap around
    !> to the beginning of the buffer and search from there, if no
    !> marked file is found after this line.
    !> Optional argument OPOINT specifies the buffer position to
    !> return to if no ARGth marked file is found; it defaults to
    !> the position where this command was invoked.  Port of
    !> dired.el:dired-next-marked-file.
    recursive subroutine dired_next_marked_file(arg, wrap, status, opoint_in)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: wrap
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: opoint_in
        integer(int64) :: opoint, pos
        logical :: found
        status = status_type()
        ! return to where interactively started
        opoint = point()
        if (present(opoint_in)) opoint = opoint_in
        if (arg > 0) then
            found = re_search_forward_p(dired_re_mark, count=arg)
        else
            call beginning_of_line()
            found = re_search_backward_p(dired_re_mark, count=-arg)
        end if
        if (found) then
            pos = dired_move_to_filename(status)
        else if (.not. wrap) then
            call goto_char(opoint)
            call set_error(status, FM_GENERIC_ERROR, "No next marked file")
        else
            call message("(Wraparound for next marked file)")
            if (arg > 0) then
                call goto_char(point_min())
            else
                call goto_char(point_max())
            end if
            call dired_next_marked_file(arg, .false., status, opoint)
        end if
    end subroutine dired_next_marked_file

    !> Move to the ARGth next marked file.
    !> ARG is the numeric prefix argument and defaults to 1.
    !> Interactively, wrap around to the beginning of the buffer and
    !> search from there, if no marked file is found after this line.
    !> Port of dired.el:dired-next-marked-file (interactive).
    recursive subroutine cmd_dired_next_marked_file(status)
        type(status_type), intent(out) :: status
        call dired_next_marked_file(prefix_n(), .true., status)
    end subroutine cmd_dired_next_marked_file

    !> Move to the ARGth previous marked file.
    !> ARG is the numeric prefix argument and defaults to 1.
    !> Interactively, wrap around to the end of the buffer and search
    !> backwards from there, if no ARGth marked file is found before this
    !> line.  Port of dired.el:dired-prev-marked-file.
    recursive subroutine cmd_dired_prev_marked_file(status)
        type(status_type), intent(out) :: status
        call dired_next_marked_file(-prefix_n(), .true., status)
    end subroutine cmd_dired_prev_marked_file

    !> Display the number and total size of the marked files.  Port of
    !> dired.el:dired-number-of-marked-files.
    recursive subroutine dired_number_of_marked_files(status)
        type(status_type), intent(out) :: status
        type(dired_string_type), allocatable :: files(:)
        type(file_stat_type) :: st
        integer(int64) :: nmarked
        real(real64) :: size
        logical :: one_marked, found
        integer :: k
        call dired_get_marked_files(files, status, distinguish_one_marked=.true., one_marked=one_marked)
        if (.not. status%ok()) return
        if (one_marked) then
            nmarked = 1
        else if (size_of(files) <= 1) then
            nmarked = 0
        else
            nmarked = size_of(files)
        end if
        size = 0
        do k = 1, size_of(files)
            call file_attributes(files(k)%s, st, found, nofollow=.true.)
            if (found) size = size + real(st%size, real64)
        end do
        if (nmarked == 0) then
            call message("No marked files")
        else if (nmarked == 1) then
            call message(itoa(nmarked)//" marked file ("//byte_count_to_string(size)//" total size)")
        else
            call message(itoa(nmarked)//" marked files ("//byte_count_to_string(size)//" total size)")
        end if
    contains

        integer function size_of(a)
            type(dired_string_type), intent(in) :: a(:)
            size_of = ubound(a, 1)
        end function size_of
    end subroutine dired_number_of_marked_files

    ! ------------------------------------------------------------------
    ! Deleting files
    ! ------------------------------------------------------------------

    !> Delete FILE or directory (possibly recursively if RECURSIVE is
    !> not DIRED_RECURSIVE_NEVER).  RECURSIVE determines what to do with a
    !> non-empty directory.  The effect of its possible values is:
    !>
    !>   DIRED_RECURSIVE_NEVER  -- do not delete.
    !>   DIRED_RECURSIVE_ALWAYS -- delete recursively without asking.
    !>   DIRED_RECURSIVE_TOP    -- ask for each directory at top level.
    !>   Anything else          -- ask for each sub-directory.
    !>
    !> The answer may change RECURSIVE (and `dired_recursive_deletes').
    !> Port of dired.el:dired-delete-file (TRASH deferred).
    recursive subroutine dired_delete_file(file, recursive, status)
        character(len=*), intent(in) :: file
        integer, intent(inout) :: recursive
        type(status_type), intent(out) :: status
        type(read_answer_type) :: answers(4)
        type(value_type) :: files
        type(value_type), allocatable :: elems(:)
        character(len=:), allocatable :: answer, name
        type(status_type) :: st
        logical :: empty_dir_p
        integer :: k
        status = status_type()
        ! This test is equivalent to (and (file-directory-p fn)
        ! (not (file-symlink-p fn))) but more efficient.
        if (.not. (file_directory_p(file) .and. .not. file_symlink_p(file))) then
            call delete_file(file, status)
            return
        end if
        empty_dir_p = .true.
        call directory_files(file, files, st, full=.true.)
        if (st%ok()) then
            call list_elements(files, elems)
            do k = 1, size(elems)
                name = file_name_nondirectory(xstring(elems(k)))
                if (name /= "." .and. name /= "..") empty_dir_p = .false.
            end do
        end if
        if (recursive /= DIRED_RECURSIVE_NEVER .and. .not. empty_dir_p) then
            if (recursive /= DIRED_RECURSIVE_ALWAYS) then
                answers(1) = read_answer_type("yes", iachar("y"), "delete recursively the current directory")
                answers(2) = read_answer_type("no", iachar("n"), "skip to next")
                answers(3) = read_answer_type("all", iachar("!"), "delete all remaining directories with no more questions")
                answers(4) = read_answer_type("quit", iachar("q"), "exit")
                call read_answer("Recursively delete "//dired_make_relative(file)//"? ", answers, answer, status)
                if (.not. status%ok()) return
                select case (answer)
                case ("all")
                    recursive = DIRED_RECURSIVE_ALWAYS
                    dired_recursive_deletes = recursive
                case ("yes")
                    if (recursive == DIRED_RECURSIVE_TOP) recursive = DIRED_RECURSIVE_ALWAYS
                case ("no")
                    recursive = DIRED_RECURSIVE_NEVER
                case default
                    ! quit, and catch all unknown answers
                    call set_error(status, FM_QUIT)
                    return
                end select
            end if
        else
            ! Empty dir or recursive is nil.
            recursive = DIRED_RECURSIVE_NEVER
        end if
        call delete_directory(file, status, recursive /= DIRED_RECURSIVE_NEVER)
    end subroutine dired_delete_file

    !> In Dired, delete the files flagged for deletion.
    !> If NOMESSAGE is true, we don't display any message
    !> if there are no flagged files.
    !> `dired_recursive_deletes' controls whether deletion of
    !> non-empty directories is allowed.  Port of
    !> dired.el:dired-do-flagged-delete.
    recursive subroutine dired_do_flagged_delete(status, nomessage)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: nomessage
        character :: saved
        integer(int64) :: opoint
        logical :: found, upward, saved_fold, quiet
        status = status_type()
        quiet = .false.
        if (present(nomessage)) quiet = nomessage
        saved = dired_marker_char
        dired_marker_char = dired_del_marker
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        opoint = point()
        call goto_char(point_min())
        found = re_search_forward_p(dired_marker_regexp())
        call goto_char(opoint)
        if (found) then
            call reset_results()
            call dired_map_over_marks(collect_filename_and_marker, DIRED_ARG_NIL, 0_int64, status, found, upward)
            if (status%ok()) call dired_internal_do_deletions(DIRED_ARG_NIL, 0_int64, upward, status)
            call free_map_markers()
        else if (.not. quiet) then
            call message("(No deletions requested)")
        end if
        case_fold_search%value = saved_fold
        dired_marker_char = saved
    end subroutine dired_do_flagged_delete

    !> In Dired, delete the files flagged for deletion.
    !> `dired_recursive_deletes' controls whether deletion of
    !> non-empty directories is allowed.  Port of
    !> dired.el:dired-do-flagged-delete (interactive).
    recursive subroutine cmd_dired_do_flagged_delete(status)
        type(status_type), intent(out) :: status
        call dired_do_flagged_delete(status)
    end subroutine cmd_dired_do_flagged_delete

    !> Delete all marked (or next ARG) files.
    !> `dired_recursive_deletes' controls whether deletion of
    !> non-empty directories is allowed.
    !>
    !> If ARG_KIND is DIRED_ARG_MARKED, delete only the marked files, or
    !> none if no files are marked.  Port of dired.el:dired-do-delete.
    recursive subroutine dired_do_delete(arg_kind, arg_n, status)
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        type(status_type), intent(out) :: status
        logical :: found, upward
        call reset_results()
        ! this may move point if ARG is an integer
        call dired_map_over_marks(collect_filename_and_marker, arg_kind, arg_n, status, found, upward)
        if (status%ok()) call dired_internal_do_deletions(arg_kind, arg_n, upward, status)
        call free_map_markers()
    end subroutine dired_do_delete

    !> Delete all marked (or next ARG) files.
    !> `dired_recursive_deletes' controls whether deletion of
    !> non-empty directories is allowed.  Port of dired.el:dired-do-delete
    !> (interactive).
    recursive subroutine cmd_dired_do_delete(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call prefix_kind(kind, n)
        call dired_do_delete(kind, n, status)
    end subroutine cmd_dired_do_delete

    !> Free the markers the deletion bodies made.
    subroutine free_map_markers()
        integer :: k
        if (.not. allocated(map_markers)) return
        do k = 1, size(map_markers)
            if (map_markers(k)%index /= 0) call release_marker(map_markers(k))
        end do
        deallocate (map_markers)
        allocate (map_markers(0))
    end subroutine free_map_markers

    !> Delete the files collected in `map_files' (with `map_markers' at
    !> their lines), after confirmation.  ARG is the prefix argument;
    !> UPWARD says the files were collected upwards.  Port of
    !> dired.el:dired-internal-do-deletions (TRASH deferred).
    recursive subroutine dired_internal_do_deletions(arg_kind, arg_n, upward, status)
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        logical, intent(in) :: upward
        type(status_type), intent(out) :: status
        type(dired_string_type), allocatable :: files(:), rel(:), failures(:), grown(:)
        type(handle_type), allocatable :: markers(:)
        type(handle_type), allocatable :: bufs(:)
        type(handle_type) :: old
        type(value_type) :: saved_ro
        type(progress_reporter_type) :: reporter
        type(status_type) :: st
        character(len=:), allocatable :: fn, prompt
        integer(int64) :: opoint, count, succ, p
        integer :: k, i, j, recursive, saved_recursive
        logical :: answer, canceled
        status = status_type()
        ! L is an alist of files to delete, with their buffer positions;
        ! filenames are absolute.  (In buffer order.)
        allocate (files(map_n), markers(map_n))
        do k = 1, map_n
            if (upward) then
                j = map_n + 1 - k
            else
                j = k
            end if
            files(k)%s = map_files(j)%s
            markers(k) = map_markers(j)
        end do
        count = map_n
        succ = 0
        ! Bind `dired_recursive_deletes' so that we can change it locally
        ! according with the user answer within `dired_delete_file'.
        saved_recursive = dired_recursive_deletes
        ! canonicalize file list for pop up
        allocate (rel(size(files)))
        do k = 1, size(files)
            rel(k)%s = dired_make_relative(files(k)%s)
        end do
        prompt = "Delete "//dired_marker_prompt(arg_kind, arg_n, rel)//" "
        call dired_mark_pop_up(" *Deletions*", "delete", rel, prompt, answer, status)
        if (.not. status%ok()) then
            dired_recursive_deletes = saved_recursive
            return
        end if
        if (answer) then
            opoint = point()
            allocate (failures(0))
            canceled = .false.
            call make_progress_reporter(reporter, "Deleting...", 0_int64, count)
            do k = 1, size(files)
                call goto_char(mpos(markers(k)))
                p = dired_move_to_filename(st)
                call inhibit_read_only_begin(saved_ro)
                fn = files(k)%s
                recursive = dired_recursive_deletes
                call dired_delete_file(fn, recursive, st)
                if (st%ok()) then
                    ! if we get here, removing worked
                    succ = succ + 1
                    call progress_reporter_update(reporter, succ)
                    ! dired-fun-in-all-buffers
                    bufs = dired_buffers_for_dir(file_name_directory(fn), file_name_nondirectory(fn))
                    old = current_buffer()
                    do i = 1, size(bufs)
                        call set_buffer(bufs(i), st)
                        call dired_delete_entry(fn, st)
                    end do
                    call set_buffer(old, st)
                    ! For when FN's directory name is different from the
                    ! current buffer's dired-directory.
                    call dired_delete_entry(fn, st)
                    call inhibit_read_only_end(saved_ro)
                else if (condition_matches(st%code, FM_QUIT)) then
                    call inhibit_read_only_end(saved_ro)
                    call message("OK, canceled")
                    canceled = .true.
                    exit
                else
                    call inhibit_read_only_end(saved_ro)
                    ! catch errors from failed deletions
                    call dired_log(error_name(st%code)//": "//format_error(st)//LF)
                    allocate (grown(size(failures) + 1))
                    grown(1)%s = fn
                    grown(2:) = failures
                    call move_alloc(grown, failures)
                end if
            end do
            if (.not. canceled) then
                if (size(failures) == 0) then
                    call progress_reporter_done(reporter)
                else if (count == 1) then
                    call dired_log_summary(itoa(int(size(failures), int64))//" of "//itoa(count)//" deletion failed", &
                                           failures)
                else
                    call dired_log_summary(itoa(int(size(failures), int64))//" of "//itoa(count)//" deletions failed", &
                                           failures)
                end if
            end if
            call goto_char(opoint)
        else
            call message("(No deletions performed)")
        end if
        dired_recursive_deletes = saved_recursive
        p = dired_move_to_filename(st)
    end subroutine dired_internal_do_deletions

    !> Remove entry FILE in the current Dired buffer.
    !> Note this doesn't delete FILE in the file system.
    !> See `dired_delete_file' in case you wish that.  Port of
    !> dired.el:dired-remove-entry.
    subroutine dired_remove_entry(file)
        character(len=*), intent(in) :: file
        type(value_type) :: saved_ro
        type(status_type) :: st
        integer(int64) :: opoint, b
        opoint = point()
        if (dired_goto_file(file, st) /= 0) then
            call inhibit_read_only_begin(saved_ro)
            call beginning_of_line()
            b = point()
            call delete_region(b, bol(2_int64), st)
            call inhibit_read_only_end(saved_ro)
        end if
        call goto_char(min(opoint, point_max()))
    end subroutine dired_remove_entry

    !> Remove entry FILE in the current Dired buffer.
    !> Like `dired_remove_entry' followed by `dired_clean_up_after_deletion'.
    !> Note this doesn't delete FILE in the file system.
    !> See `dired_delete_file' in case you wish that.  Port of
    !> dired.el:dired-delete-entry.
    recursive subroutine dired_delete_entry(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        call dired_remove_entry(file)
        call dired_clean_up_after_deletion(file, status)
    end subroutine dired_delete_entry

    !> Clean up after a deleted file or directory FN.
    !> Removes any expanded subdirectory of deleted directory.  Port of
    !> dired.el:dired-clean-up-after-deletion (no dired-x: no buffers are
    !> killed).
    recursive subroutine dired_clean_up_after_deletion(fn, status)
        character(len=*), intent(in) :: fn
        type(status_type), intent(out) :: status
        integer(int64) :: opoint
        status = status_type()
        opoint = point()
        if (alist_size() > 1) then
            if (dired_goto_subdir(fn) /= 0) call dired_kill_subdir(status)
        end if
        call goto_char(min(opoint, point_max()))
    end subroutine dired_clean_up_after_deletion

    !> Return FUNCTION's result after showing which files are marked:
    !> ask PROMPT with `dired_deletion_confirmer' (ANSWER).  Displays the
    !> file names in a window showing a buffer named BUFFER_NAME.  The
    !> window is not shown if there is just one file, `dired_no_confirm'
    !> has `all' set, or OP_SYMBOL is among its `commands'.
    !>
    !> By default, Dired shrinks the display buffer to fit the marked
    !> files.  Port of dired.el:dired-mark-pop-up (the function is the
    !> confirmer).
    recursive subroutine dired_mark_pop_up(buffer_name, op_symbol, files, prompt, answer, status)
        character(len=*), intent(in) :: buffer_name, op_symbol, prompt
        type(dired_string_type), intent(in) :: files(:)
        logical, intent(out) :: answer
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, window, owindow, old
        type(display_alist_type) :: alist
        type(status_type) :: st
        integer :: saved_dedicated, k
        logical :: quiet
        answer = .false.
        if (.not. associated(dired_deletion_confirmer)) dired_deletion_confirmer => yes_or_no_p
        quiet = dired_no_confirm%all .or. size(files) == 1
        if (allocated(dired_no_confirm%commands)) then
            do k = 1, size(dired_no_confirm%commands)
                if (trim(dired_no_confirm%commands(k)) == op_symbol) quiet = .true.
            end do
        end if
        if (quiet) then
            call dired_deletion_confirmer(prompt, answer, status)
            return
        end if
        old = current_buffer()
        call temp_buffer_window_setup(buffer_name, buffer, status)
        if (.not. status%ok()) return
        ! Mark *Marked Files* window as softly-dedicated, to prevent other
        ! buffers e.g. *Completions* from reusing it (bug#17554).
        saved_dedicated = display_buffer_mark_dedicated
        display_buffer_mark_dedicated = DEDICATED_WEAK
        ! (display-buffer-below-selected (window-height . fit-window-to-buffer)
        !  (preserve-size . (nil . t)) (body-function ...))
        pop_up_files = files
        alist%window_height => fit_window
        alist%body_function => insert_marked_files
        call temp_buffer_window_show(buffer, window, status, [DISPLAY_ACTION_BELOW_SELECTED], alist)
        display_buffer_mark_dedicated = saved_dedicated
        call set_buffer(old, st)
        if (.not. status%ok()) return
        if (.not. window_live_p(window)) then
            call dired_deletion_confirmer(prompt, answer, status)
            return
        end if
        ! (with-selected-window window (unwind-protect (apply function args)
        !    (quit-restore-window window 'kill)))
        owindow = selected_window()
        call select_window(window, st, norecord=.true.)
        call dired_deletion_confirmer(prompt, answer, status)
        if (window_live_p(window)) call quit_restore_window(window, BK_KILL, st)
        if (window_live_p(owindow)) call select_window(owindow, st, norecord=.true.)
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine dired_mark_pop_up

    !> The `window_height' of `dired_mark_pop_up': fit-window-to-buffer.
    subroutine fit_window(window)
        type(handle_type), intent(in) :: window
        call fit_window_to_buffer(window)
    end subroutine fit_window

    !> The `body_function' of `dired_mark_pop_up': insert the files in
    !> columns (dired-format-columns-of-files).
    subroutine insert_marked_files(window)
        type(handle_type), intent(in) :: window
        type(minibuffer_string_type), allocatable :: strings(:)
        type(status_type) :: st
        integer(int64) :: beg
        logical :: changed
        integer :: k
        if (.not. window_live_p(window)) return
        allocate (strings(size(pop_up_files)))
        do k = 1, size(pop_up_files)
            strings(k)%s = pop_up_files(k)%s
        end do
        beg = point()
        call completion_insert_strings(strings)
        call remove_text_properties(beg, point(), list(sym("mouse_face"), nil(), sym("help_echo"), nil()), changed, st)
    end subroutine insert_marked_files

    !> Set up temporary buffer BUFFER_NAME: no local variables, writable,
    !> no file or undo, empty.  Port of window.el:temp-buffer-window-setup
    !> (no `temp_buffer_window_setup_hook').
    subroutine temp_buffer_window_setup(buffer_name, buffer, status)
        character(len=*), intent(in) :: buffer_name
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: old_dir
        type(value_type) :: saved_ro
        logical :: saved_hooks
        old_dir = default_directory()
        call get_buffer_create(buffer_name, buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call kill_all_local_variables()
        buffers(cur)%directory = old_dir
        buffers(cur)%read_only = .false.
        if (allocated(buffers(cur)%filename)) deallocate (buffers(cur)%filename)
        buffers(cur)%undo_enabled = .false.
        buffers(cur)%undo_count = 0
        call inhibit_read_only_begin(saved_ro)
        saved_hooks = inhibit_modification_hooks%value
        inhibit_modification_hooks = .true.
        call erase_buffer(status)
        inhibit_modification_hooks = saved_hooks
        call inhibit_read_only_end(saved_ro)
    end subroutine temp_buffer_window_setup

    ! ------------------------------------------------------------------
    ! Logging failures
    ! ------------------------------------------------------------------

    !> Log a message (TEXT, usually ending with a newline) in the Dired log
    !> buffer.  Port of dired.el:dired-log (a string).
    subroutine dired_log(text)
        character(len=*), intent(in) :: text
        type(handle_type) :: old, buf
        type(value_type) :: saved_ro
        type(status_type) :: st
        old = current_buffer()
        call get_buffer_create(dired_log_buffer, buf, st)
        call set_buffer(buf, st)
        call goto_char(point_max())
        call inhibit_read_only_begin(saved_ro)
        call insert(text, st)
        call inhibit_read_only_end(saved_ro)
        call set_buffer(old, st)
    end subroutine dired_log

    !> End a bunch of errors: insert the current time and buffer at the
    !> start of the page, and a formfeed at the end.  Port of
    !> dired.el:dired-log (LOG t).
    subroutine dired_log_end()
        type(handle_type) :: old, buf
        type(value_type) :: saved_ro
        type(status_type) :: st
        character(len=:), allocatable :: obuf_name
        old = current_buffer()
        obuf_name = buffers(cur)%name
        call get_buffer_create(dired_log_buffer, buf, st)
        call set_buffer(buf, st)
        call goto_char(point_max())
        call inhibit_read_only_begin(saved_ro)
        call backward_page()
        if (.not. bolp()) call insert(LF, st)
        call insert(current_time_string()//format_message(TAB//"Buffer `")//obuf_name//format_message("'")//LF, st)
        call goto_char(point_max())
        call insert(achar(12)//LF, st)
        call inhibit_read_only_end(saved_ro)
        call set_buffer(old, st)
    end subroutine dired_log_end

    !> Move backward to page boundary (page-delimiter "^\f").  Port of
    !> textmodes/page.el:backward-page (COUNT 1).
    subroutine backward_page()
        logical :: found
        integer(int64) :: opoint
        if (bobp()) return
        opoint = point()
        if (re_search_backward_p("^"//achar(12))) then
            if (mend(0) == opoint) then
                call goto_char(mbeg(0))
            else
                call goto_char(opoint)
            end if
        end if
        call goto_char(point() - 1)
        found = re_search_backward_p("^"//achar(12))
        if (found) then
            call goto_char(mend(0))
        else
            call goto_char(point_min())
        end if
    end subroutine backward_page

    !> The current local time as `current_time_string' gives it:
    !> "Sun Sep 16 01:03:52 1973".  Port of editfns.c:Fcurrent_time_string.
    function current_time_string() result(s)
        character(len=:), allocatable :: s
        character(len=3), parameter :: days(0:6) = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
        character(len=3), parameter :: months(12) = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", &
                                                     "Sep", "Oct", "Nov", "Dec"]
        integer :: v(8), y, m, d, w
        character(len=32) :: buf
        call date_and_time(values=v)
        y = v(1)
        m = v(2)
        d = v(3)
        ! Zeller's congruence (Sunday = 0)
        if (m < 3) then
            w = mod(d + (13*(m + 13))/5 + (y - 1) + (y - 1)/4 - (y - 1)/100 + (y - 1)/400 + 6, 7)
        else
            w = mod(d + (13*(m + 1))/5 + y + y/4 - y/100 + y/400 + 6, 7)
        end if
        write (buf, "(a3,1x,a3,1x,i2,1x,i2.2,':',i2.2,':',i2.2,1x,i4)") days(w), months(m), d, v(5), v(6), v(7), y
        s = trim(buf)
    end function current_time_string

    !> State a summary of a command's failures, in echo area and log buffer.
    !> STRING is an overall summary of the failures.
    !> FAILURES is a list of file names that we failed to operate on.
    !> Port of dired.el:dired-log-summary.
    subroutine dired_log_summary(string, failures)
        character(len=*), intent(in) :: string
        type(dired_string_type), intent(in) :: failures(:)
        type(handle_type) :: old, buf
        character(len=:), allocatable :: text, list_text
        type(status_type) :: st
        integer :: k
        if (size(failures) == 1) then
            old = current_buffer()
            buf = get_buffer(dired_log_buffer)
            call set_buffer(buf, st)
            call goto_char(point_max())
            call backward_page()
            if (eolp()) call fwd_line(1_int64)
            call buffer_substring(point(), point_max(), text, st)
            call set_buffer(old, st)
            call message(text)
        else if (size(failures) == 0) then
            call message(string//"--type ? for details")
        else
            ! (format "%s" failures): a list of strings.
            list_text = "("
            do k = 1, size(failures)
                if (k > 1) list_text = list_text//" "
                list_text = list_text//failures(k)%s
            end do
            list_text = list_text//")"
            call message(string//"--type ? for details "//list_text)
        end if
        ! Log a summary describing a bunch of errors.
        call dired_log(LF//string//LF)
        call dired_log_end()
    end subroutine dired_log_summary

    !> Pop up a buffer with error log output from Dired.
    !> A group of errors from a single command ends with a formfeed.
    !> Thus, use \[backward_page] to find the beginning of a group of
    !> errors.  Port of dired.el:dired-why.
    recursive subroutine dired_why(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: owindow, window, buf
        type(status_type) :: st
        status = status_type()
        buf = get_buffer(dired_log_buffer)
        if (buf%index == 0) return
        owindow = selected_window()
        call display_buffer(buf, window, status)
        if (.not. status%ok() .or. .not. window_live_p(window)) return
        call select_window(window, st)
        call goto_char(point_max())
        call fwd_line(-1_int64)
        call backward_page()
        call recenter(make_fixnum(0_int64), .false., st)
        call select_window(owindow, st)
    end subroutine dired_why

    !> Summarize basic Dired commands and show recent Dired errors.  Port
    !> of dired.el:dired-summary.
    recursive subroutine dired_summary(status)
        type(status_type), intent(out) :: status
        call dired_why(status)
        call message(substitute_command_keys("\`d'-elete, \`u'-ndelete, \`x'-punge, \`f'-ind, "// &
                                             "\`o'-ther window, \`R'-ename, \`C'-opy, \`h'-elp"))
    end subroutine dired_summary

    !> Undo in a Dired buffer.
    !> A numeric ARG serves as a repeat count.
    !> This doesn't recover lost files, it just undoes changes in the buffer itself.
    !> You can use it to recover marks, killed lines or subdirs.  Port of
    !> dired.el:dired-undo.
    recursive subroutine cmd_dired_undo(status)
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        type(status_type) :: st
        ! undo-ignore-read-only
        call inhibit_read_only_begin(saved_ro)
        call undo(current_prefix_arg, status)
        call inhibit_read_only_end(saved_ro)
        if (.not. status%ok()) return
        call dired_build_subdir_alist(st)
        call message("Change in Dired buffer undone."//LF//"Actual changes in files cannot be undone by Emacs.")
    end subroutine cmd_dired_undo

    !> Copy names of marked (or next ARG) files into the kill ring.
    !> If there are several names, they will be separated by a space,
    !> and file names that have spaces or quote characters in them will
    !> be quoted (with double quotes).  (When there's a single file, no
    !> quoting is done.)
    !>
    !> With a zero prefix arg, use the absolute file name of each marked file.
    !> With \[universal_argument], use the file name relative to the Dired buffer's
    !> `default_directory'.  (This still may contain slashes if in a subdirectory.)
    !>
    !> If on a subdir headerline, use absolute subdirname instead;
    !> prefix arg and marked files are ignored in this case.
    !>
    !> You can then feed the file name(s) to other commands with \[yank].
    !> Port of dired.el:dired-copy-filename-as-kill (a prefix value of 1,
    !> names relative to the project root, needs project.el: deferred).
    recursive subroutine cmd_dired_copy_filename_as_kill(status)
        type(status_type), intent(out) :: status
        type(dired_string_type), allocatable :: files(:)
        character(len=:), allocatable :: subdir, string
        integer :: k
        status = status_type()
        subdir = dired_get_subdir()
        if (len(subdir) > 0) then
            allocate (files(1))
            files(1)%s = subdir
        else if (nilp(current_prefix_arg)) then
            call dired_get_marked_files(files, status, DIRED_LOCALP_NO_DIR)
        else if (consp(current_prefix_arg)) then
            call dired_get_marked_files(files, status, DIRED_LOCALP_T)
        else if (prefix_n() == 0) then
            call dired_get_marked_files(files, status)
        else if (prefix_n() == 1 .and. fixnump(current_prefix_arg)) then
            call set_error(status, FM_GENERIC_ERROR, "Names relative to the project root (project.el) are "// &
                           "not supported yet")
            return
        else
            call dired_get_marked_files(files, status, DIRED_LOCALP_NO_DIR, DIRED_ARG_INTEGER, prefix_n())
        end if
        if (.not. status%ok()) return
        if (size(files) == 1) then
            string = files(1)%s
        else
            string = ""
            do k = 1, size(files)
                if (k > 1) string = string//" "
                if (scan(files(k)%s, " ""'") > 0) then
                    string = string//prin1_to_string(make_string(files(k)%s))
                else
                    string = string//files(k)%s
                end if
            end do
        end if
        if (len(string) > 0) then
            if (symbol_is(last_command, "kill_region")) then
                call kill_append(string, .false.)
            else
                call kill_new(string)
            end if
            call message(string)
        end if
    contains

        logical function symbol_is(v, name)
            type(value_type), intent(in) :: v
            character(len=*), intent(in) :: name
            symbol_is = value_eq(v, sym(name))
        end function symbol_is
    end subroutine cmd_dired_copy_filename_as_kill

    ! ------------------------------------------------------------------
    ! Sorting
    ! ------------------------------------------------------------------

    !> Set mode-line according to option `dired_switches_in_mode_line'.
    !> Port of dired.el:dired-sort-set-mode-line.
    subroutine dired_sort_set_mode_line()
        character(len=:), allocatable :: sw, xs
        logical :: saved
        if (.not. allocated(buffers(cur)%major_mode)) return
        if (buffers(cur)%major_mode /= "dired_mode") return
        sw = dired_actual_switches_of()
        saved = case_fold_search%value
        case_fold_search%value = .false.
        if (dired_switches_in_mode_line == DIRED_SWITCHES_SORT_ORDER) then
            if (string_match_p(dired_sort_by_name_regexp, sw)) then
                buffers(cur)%mode_name = "Dired by name"
            else if (string_match_p(dired_sort_by_date_regexp, sw)) then
                buffers(cur)%mode_name = "Dired by date"
            else
                buffers(cur)%mode_name = "Dired "//sw
            end if
        else if (dired_switches_in_mode_line >= 0) then
            xs = sw(1:min(len(sw), dired_switches_in_mode_line))
            if (len(xs) == 0) then
                buffers(cur)%mode_name = "Dired"
            else if (len(xs) < len(sw)) then
                buffers(cur)%mode_name = "Dired "//xs//char(226)//char(128)//char(166)
            else
                buffers(cur)%mode_name = "Dired "//xs
            end if
        else
            buffers(cur)%mode_name = "Dired "//sw
        end if
        case_fold_search%value = saved
    end subroutine dired_sort_set_mode_line

    !> Toggle sorting by date, and refresh the Dired buffer.
    !> With a prefix argument, edit the current listing switches instead.
    !> Port of dired.el:dired-sort-toggle-or-edit.
    recursive subroutine cmd_dired_sort_toggle_or_edit(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: switches
        status = status_type()
        if (dired_sort_inhibit) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot sort this Dired buffer")
            return
        end if
        if (.not. nilp(current_prefix_arg)) then
            call read_string("ls switches (must contain -l): ", switches, status, initial=dired_actual_switches_of())
            if (.not. status%ok()) return
            call dired_sort_other(switches, status)
        else
            call dired_sort_toggle(status)
        end if
    end subroutine cmd_dired_sort_toggle_or_edit

    !> Toggle between sort by date/name.  Reverts the buffer.  Port of
    !> dired.el:dired-sort-toggle.
    recursive subroutine dired_sort_toggle(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: sw, m2, m4, newsw
        character(len=*), parameter :: switch_regexp = "\(\`\| \)-\([a-su-zA-Z]*\)\(t\)\([^ ]*\)"
        type(status_type) :: st
        logical :: sorting_by_date, saved
        integer :: k
        status = status_type()
        sw = dired_actual_switches_of()
        saved = case_fold_search%value
        case_fold_search%value = .false.
        sorting_by_date = string_match_p(dired_sort_by_date_regexp, sw)
        ! Remove the -t switch.
        do while (string_match(switch_regexp, sw, st) > 0)
            m2 = match_string(2, sw)
            m4 = match_string(4, sw)
            if (len(m2) == 0 .and. len(m4) == 0) then
                ! Remove a stand-alone -t switch.
                call replace_match("", st, fixedcase=.true., literal=.true., string=sw, result=newsw)
            else
                ! Remove a switch of the form -XtY for some X and Y.
                call replace_match("", st, fixedcase=.true., literal=.true., string=sw, subexp=3, result=newsw)
            end if
            sw = newsw
        end do
        ! Now, if we weren't sorting by date before, add the -t switch.
        ! Some simple-minded ls implementations (eg ftp servers) only allow
        ! a single option string, so try not to add " -t" if possible.
        if (.not. sorting_by_date) then
            if (string_match_p("\`-[[:alnum:]]+\'", sw)) then
                sw = sw//"t"
            else
                sw = sw//" -t"
            end if
        end if
        case_fold_search%value = saved
        k = local_index(.true.)
        locals(k)%actual_switches = sw
        call dired_sort_set_mode_line()
        call revert_buffer(status)
    end subroutine dired_sort_toggle

    !> Specify new `ls' SWITCHES for current Dired buffer.
    !> Values matching `dired_sort_by_date_regexp' or `dired_sort_by_name_regexp'
    !> set the minor mode accordingly, others appear literally in the mode line.
    !> With NO_REVERT, don't refresh the listing afterwards.  Port of
    !> dired.el:dired-sort-other.
    recursive subroutine dired_sort_other(switches, status, no_revert)
        character(len=*), intent(in) :: switches
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_revert
        integer :: k
        logical :: nr
        status = status_type()
        nr = .false.
        if (present(no_revert)) nr = no_revert
        call dired_sort_r_check(switches)
        k = local_index(.true.)
        locals(k)%actual_switches = switches
        call dired_sort_set_mode_line()
        if (.not. nr) call revert_buffer(status)
    end subroutine dired_sort_other

    !> Additional processing of -R in ls option string SWITCHES.
    !> Saves `dired_subdir_alist' when R is set and restores saved value
    !> minus any directories explicitly deleted when R is cleared.
    !> To be called first in body of `dired_sort_other', etc.  Port of
    !> dired.el:dired-sort-R-check.
    subroutine dired_sort_r_check(switches)
        character(len=*), intent(in) :: switches
        type(dired_subdir_type), allocatable :: kept(:)
        integer :: k, i, j, n
        k = local_index(.true.)
        if (dired_switches_recursive_p(switches) .and. .not. dired_switches_recursive_p(dired_actual_switches_of())) then
            ! Adding -R to ls switches -- save `dired_subdir_alist':
            locals(k)%has_pre_r = .true.
            if (allocated(locals(k)%subdir_alist_pre_r)) deallocate (locals(k)%subdir_alist_pre_r)
            allocate (locals(k)%subdir_alist_pre_r(alist_size()))
            do i = 1, alist_size()
                locals(k)%subdir_alist_pre_r(i)%s = locals(k)%subdir_alist(i)%dir
            end do
        else if (dired_switches_recursive_p(dired_actual_switches_of()) .and. &
                 .not. dired_switches_recursive_p(switches)) then
            ! Deleting -R from ls switches -- revert to pre-R subdirs that
            ! are still present:
            if (.not. allocated(locals(k)%subdir_alist)) return
            allocate (kept(0))
            if (locals(k)%has_pre_r .and. size(locals(k)%subdir_alist_pre_r) > 0) then
                do i = 1, size(locals(k)%subdir_alist_pre_r)
                    do j = 1, size(locals(k)%subdir_alist)
                        if (locals(k)%subdir_alist(j)%dir == locals(k)%subdir_alist_pre_r(i)%s) then
                            ! subdir still present...
                            kept = [kept, locals(k)%subdir_alist(j)]
                            locals(k)%subdir_alist(j)%marker = null_handle()
                            exit
                        end if
                    end do
                end do
                deallocate (locals(k)%subdir_alist_pre_r)
                allocate (locals(k)%subdir_alist_pre_r(0))
            else if (size(locals(k)%subdir_alist) > 0) then
                ! No pre-R subdir alist, so revert to main directory listing:
                n = size(locals(k)%subdir_alist)
                kept = [kept, locals(k)%subdir_alist(n)]
                locals(k)%subdir_alist(n)%marker = null_handle()
            end if
            call free_alist(locals(k)%subdir_alist)
            call move_alloc(kept, locals(k)%subdir_alist)
        end if
    end subroutine dired_sort_r_check

    ! ------------------------------------------------------------------
    ! Font Lock
    ! ------------------------------------------------------------------

    subroutine face_mark(face)
        character(len=:), allocatable, intent(out) :: face
        face = dired_mark_face
    end subroutine face_mark

    subroutine face_marked(face)
        character(len=:), allocatable, intent(out) :: face
        face = dired_marked_face
    end subroutine face_marked

    subroutine face_flagged(face)
        character(len=:), allocatable, intent(out) :: face
        face = dired_flagged_face
    end subroutine face_flagged

    subroutine face_perm_write(face)
        character(len=:), allocatable, intent(out) :: face
        face = dired_perm_write_face
    end subroutine face_perm_write

    subroutine face_directory(face)
        character(len=:), allocatable, intent(out) :: face
        face = dired_directory_face
    end subroutine face_directory

    subroutine face_symlink(face)
        character(len=:), allocatable, intent(out) :: face
        face = dired_symlink_face
    end subroutine face_symlink

    subroutine face_ignored(face)
        character(len=:), allocatable, intent(out) :: face
        face = dired_ignored_face
    end subroutine face_ignored

    subroutine face_header(face)
        character(len=:), allocatable, intent(out) :: face
        face = dired_header_face
    end subroutine face_header

    !> PRE-MATCH-FORM (dired-move-to-filename).
    subroutine pre_move_to_filename(limit)
        integer(int64), intent(out) :: limit
        type(status_type) :: st
        limit = dired_move_to_filename(st)
    end subroutine pre_move_to_filename

    !> PRE-MATCH-FORM of the -F ignored-extension keyword: if the last
    !> character is not part of the filename, move back to the start of the
    !> filename so it can be fontified; otherwise, leave point at the end of
    !> the line; that way, nothing is fontified.
    subroutine pre_ignored_f(limit)
        integer(int64), intent(out) :: limit
        type(value_type) :: v
        type(status_type) :: st
        call end_of_line()
        limit = 0
        call get_text_property(point() - 1, sym("mouse_face"), v, st)
        if (nilp(v)) limit = dired_move_to_filename(st)
    end subroutine pre_ignored_f

    !> The truename of the file at point, if it can be found (TRUENAME
    !> "" and FOUND false otherwise); NAME_FOUND tells whether there is
    !> a file name.
    subroutine truename_at_point(truename, found, name_found)
        character(len=:), allocatable, intent(out) :: truename
        logical, intent(out) :: found, name_found
        character(len=:), allocatable :: file
        type(status_type) :: st
        found = .false.
        truename = ""
        call dired_file_name_at_point(file, name_found)
        if (.not. name_found) return
        ! (ignore-errors (file-truename file))
        call file_truename(file, truename, st)
        found = st%ok()
    end subroutine truename_at_point

    !> Search forward for REGEXP up to LIMIT, setting FOUND.
    subroutine search_to(regexp, limit, found, status)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: pos
        call re_search_forward(regexp, pos, status, bound=limit, noerror=SEARCH_NOERROR)
        found = status%ok() .and. pos > 0
    end subroutine search_to

    !> Matcher of a broken symbolic link: either not existent target or
    !> circular link.
    subroutine broken_symlink_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: truename
        logical :: have, name_found
        status = status_type()
        found = .false.
        if (.not. dired_check_symlinks) return
        call truename_at_point(truename, have, name_found)
        if (have) then
            if (file_exists_p(truename)) return
        end if
        call search_to("\(.+\) \(->\) ?\(.+\)", limit, found, status)
    end subroutine broken_symlink_matcher

    !> Matcher of a symbolic link to a directory.
    subroutine dir_symlink_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: truename
        logical :: have, name_found
        status = status_type()
        found = .false.
        if (.not. dired_check_symlinks) return
        call truename_at_point(truename, have, name_found)
        if (.not. have) return
        if (.not. file_directory_p(truename)) return
        call search_to("\(.+-> ?\)\(.+\)", limit, found, status)
    end subroutine dir_symlink_matcher

    !> Matcher of a symbolic link to a non-directory, or no check at all.
    subroutine other_symlink_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: truename
        logical :: have, name_found
        status = status_type()
        found = .false.
        if (.not. dired_check_symlinks) then
            call search_to("\(.+-> ?\)\(.+\)", limit, found, status)
            return
        end if
        call truename_at_point(truename, have, name_found)
        if (.not. name_found) return
        if (have) then
            if (file_directory_p(truename)) return
        end if
        call search_to("\(.+-> ?\)\(.+\)", limit, found, status)
    end subroutine other_symlink_matcher

    !> The keywords Dired gives Font Lock: marks, marked and flagged
    !> files, group- and world-writable and set-id permissions,
    !> directories, files with extensions in
    !> `completion_ignored_extensions' (the alternation `regexp_opt' makes,
    !> written out), symbolic links (broken, to directories, to other
    !> files), special files, file names ending in a colon, and directory
    !> headers.  Port of dired.el dired-font-lock-keywords (made for each
    !> buffer by `dired_mode', so the `eval' parts see the current value).
    subroutine dired_font_lock_keywords(kw)
        type(font_lock_keyword_type), allocatable, intent(out) :: kw(:)
        type(font_lock_highlight_type) :: h1(1), h2(2), h3(3), a(1)
        character(len=:), allocatable :: exts, prefix
        character(len=64) :: props(1)
        integer :: k
        props(1) = "dired_symlink_filename"
        prefix = dired_re_maybe_mark//dired_re_inode_size
        allocate (kw(16))
        ! Dired marks.
        h1(1) = font_lock_highlight(0, face_function=face_mark)
        kw(1) = font_lock_keyword(dired_re_mark, highlights=h1)
        ! Marked files.
        h1(1) = font_lock_highlight(0, face_function=face_marked)
        a(1) = font_lock_anchored(h1, regexp=".+", pre_match_form=pre_move_to_filename)
        kw(2) = font_lock_keyword("^[*]", highlights=a)
        ! Flagged files.
        h1(1) = font_lock_highlight(0, face_function=face_flagged)
        a(1) = font_lock_anchored(h1, regexp=".+", pre_match_form=pre_move_to_filename)
        kw(3) = font_lock_keyword("^[D]", highlights=a)
        ! Files that are group or world writable: we don't need to
        ! highlight the file name, only the permissions, to win generally.
        h1(1) = font_lock_highlight(1, face_function=face_perm_write)
        kw(4) = font_lock_keyword(prefix//"[-d]....\(w\)....", highlights=h1)
        kw(5) = font_lock_keyword(prefix//"[-d].......\(w\).", highlights=h1)
        ! suid and guid
        h1(1) = font_lock_highlight(1, face="dired_set_id")
        kw(6) = font_lock_keyword(prefix//"[-d]..\(s\)......", highlights=h1)
        kw(7) = font_lock_keyword(prefix//"[-d].....\([sS]\)...", highlights=h1)
        ! Subdirectories.
        h1(1) = font_lock_highlight(0, face_function=face_directory)
        a(1) = font_lock_anchored(h1, regexp=".+", pre_match_form=pre_move_to_filename)
        kw(8) = font_lock_keyword(dired_re_dir, highlights=a)
        ! Files suffixed with `completion_ignored_extensions'.  It is
        ! quicker to first find just an extension, then go back to the
        ! start of that file name.
        exts = "\(?:"
        do k = 1, size(completion_ignored_extensions)
            if (k > 1) exts = exts//"\|"
            exts = exts//regexp_quote(trim(completion_ignored_extensions(k)))
        end do
        exts = exts//"\)"
        h1(1) = font_lock_highlight(0, face_function=face_ignored)
        a(1) = font_lock_anchored(h1, regexp=".+", pre_match_form=pre_move_to_filename)
        kw(9) = font_lock_keyword("\("//exts//"\|#\|\.#.+\)$", highlights=a)
        ! Files suffixed with `completion_ignored_extensions' plus a
        ! character put in by -F.
        a(1) = font_lock_anchored(h1, regexp=".+", pre_match_form=pre_ignored_f)
        kw(10) = font_lock_keyword("\("//exts//"\|#\|\.#.+\)[*=|]$", highlights=a)
        ! Broken Symbolic link.
        h3(1) = font_lock_highlight(1, face="dired_broken_symlink")
        h3(2) = font_lock_highlight(2, face_function=face_symlink)
        h3(3) = font_lock_highlight(3, face="dired_broken_symlink", true_properties=props)
        a(1) = font_lock_anchored(h3, matcher=broken_symlink_matcher, pre_match_form=pre_move_to_filename)
        kw(11) = font_lock_keyword(dired_re_sym, highlights=a)
        ! Symbolic link to a directory.
        h2(1) = font_lock_highlight(1, face_function=face_symlink)
        h2(2) = font_lock_highlight(2, face_function=face_directory, true_properties=props)
        a(1) = font_lock_anchored(h2, matcher=dir_symlink_matcher, pre_match_form=pre_move_to_filename)
        kw(12) = font_lock_keyword(dired_re_sym, highlights=a)
        ! Symbolic link to a non-directory.  Or no check at all.
        h2(1) = font_lock_highlight(1, face_function=face_symlink)
        h2(2) = font_lock_highlight(2, face="default", true_properties=props)
        a(1) = font_lock_anchored(h2, matcher=other_symlink_matcher, pre_match_form=pre_move_to_filename)
        kw(13) = font_lock_keyword(dired_re_sym, highlights=a)
        ! Sockets, pipes, block devices, char devices.
        h1(1) = font_lock_highlight(0, face="dired_special")
        a(1) = font_lock_anchored(h1, regexp=".+", pre_match_form=pre_move_to_filename)
        kw(14) = font_lock_keyword(dired_re_special, highlights=a)
        ! Explicitly put the default face on file names ending in a colon
        ! to avoid fontifying them as directory header.
        h1(1) = font_lock_highlight(0, face="default")
        a(1) = font_lock_anchored(h1, regexp=".+", pre_match_form=pre_move_to_filename)
        kw(15) = font_lock_keyword(prefix//dired_re_perms//".*:$", highlights=a)
        ! Directory headers.
        h1(1) = font_lock_highlight(1, face_function=face_header)
        kw(16) = font_lock_keyword(dired_subdir_regexp, highlights=h1)
    end subroutine dired_font_lock_keywords

    !> Define the Dired faces.
    subroutine init_dired_faces()
        integer, parameter :: C = FACE_CLASS_COLOR
        type(named_face_type), pointer :: f
        f => dired_header
        call f%define("dired_header", "Face used for directory headers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_type_face"))
        f => dired_mark
        call f%define("dired_mark", "Face used for Dired marks.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_constant_face"))
        f => dired_marked
        call f%define("dired_marked", "Face used for marked files.")
        call f%spec_entry(display_when(), face_attrs(inherit="warning"))
        f => dired_flagged
        call f%define("dired_flagged", "Face used for files flagged for deletion.")
        call f%spec_entry(display_when(), face_attrs(inherit="error"))
        f => dired_warning
        call f%define("dired_warning", "Face used to highlight a part of a buffer that needs user attention.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_warning_face"))
        f => dired_perm_write
        call f%define("dired_perm_write", "Face used to highlight permissions of group- and world-writable files.")
        call f%spec_entry(display_when(types=[character(len=3) :: "w32", "pc"]), face_attrs(inherit="default"))
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_comment_delimiter_face"))
        f => dired_set_id
        call f%define("dired_set_id", "Face used to highlight permissions of suid and guid files.")
        call f%spec_entry(display_when(types=[character(len=3) :: "w32", "pc"]), face_attrs(inherit="default"))
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_warning_face"))
        f => dired_directory
        call f%define("dired_directory", "Face used for subdirectories.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_function_name_face"))
        f => dired_symlink
        call f%define("dired_symlink", "Face used for symbolic links.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_keyword_face"))
        f => dired_broken_symlink
        call f%define("dired_broken_symlink", "Face used for broken symbolic links.")
        call f%spec_entry(display_when(classes=[C]), face_attrs(foreground="yellow1", background="red1", weight="bold"))
        call f%spec_entry(display_when(), face_attrs(weight="bold", slant="italic", underline=.true.))
        f => dired_special
        call f%define("dired_special", "Face used for sockets, pipes, block devices and char devices.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_variable_name_face"))
        f => dired_ignored
        call f%define("dired_ignored", "Face used for files suffixed with `completion_ignored_extensions'.")
        call f%spec_entry(display_when(), face_attrs(inherit="shadow"))
    end subroutine init_dired_faces

    ! ------------------------------------------------------------------
    ! Commands that wrap the functions
    ! ------------------------------------------------------------------

    !> Build `dired_subdir_alist' by parsing the buffer.
    !> Returns the new value of `dired_subdir_alist'.
    !> Port of dired.el:dired-build-subdir-alist (interactive).
    recursive subroutine cmd_dired_build_subdir_alist(status)
        type(status_type), intent(out) :: status
        call dired_build_subdir_alist(status, called_interactively=.true.)
    end subroutine cmd_dired_build_subdir_alist

    !> In Dired, visit the file or directory named on this line.  Port of
    !> dired.el:dired-find-file (interactive).
    recursive subroutine cmd_dired_find_file(status)
        type(status_type), intent(out) :: status
        call dired_find_file(status)
    end subroutine cmd_dired_find_file

    !> In Dired, visit this file or directory in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of dired.el:dired-find-file-other-window (interactive).
    recursive subroutine cmd_dired_find_file_other_window(status)
        type(status_type), intent(out) :: status
        call dired_find_file_other_window(status)
    end subroutine cmd_dired_find_file_other_window

    !> In Dired, display this file or directory in another window.  Port of
    !> dired.el:dired-display-file (interactive).
    recursive subroutine cmd_dired_display_file(status)
        type(status_type), intent(out) :: status
        call dired_display_file(status)
    end subroutine cmd_dired_display_file

    !> Toggle marks: marked files become unmarked, and vice versa.
    !> Flagged files (indicated with flags such as `C' and `D', not
    !> with `*') are not affected, and `.' and `..' are never toggled.
    !> As always, hidden subdirs are not affected.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region if `dired_mark_region' is true.  Otherwise, operate
    !> on the whole buffer.  Port of dired.el:dired-toggle-marks
    !> (interactive).
    recursive subroutine cmd_dired_toggle_marks(status)
        type(status_type), intent(out) :: status
        call dired_toggle_marks(status)
    end subroutine cmd_dired_toggle_marks

    !> Display the number and total size of the marked files.  Port of
    !> dired.el:dired-number-of-marked-files (interactive).
    recursive subroutine cmd_dired_number_of_marked_files(status)
        type(status_type), intent(out) :: status
        call dired_number_of_marked_files(status)
    end subroutine cmd_dired_number_of_marked_files

    !> Pop up a buffer with error log output from Dired.
    !> A group of errors from a single command ends with a formfeed.
    !> Thus, use \[backward_page] to find the beginning of a group of
    !> errors.  Port of dired.el:dired-why (interactive).
    recursive subroutine cmd_dired_why(status)
        type(status_type), intent(out) :: status
        call dired_why(status)
    end subroutine cmd_dired_why

    !> Summarize basic Dired commands and show recent Dired errors.  Port
    !> of dired.el:dired-summary (interactive).
    recursive subroutine cmd_dired_summary(status)
        type(status_type), intent(out) :: status
        call dired_summary(status)
    end subroutine cmd_dired_summary

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Bind KEYS (key-parse syntax) to COMMAND in `dired_mode_map'.
    subroutine bind(keys, command)
        character(len=*), intent(in) :: keys, command
        type(status_type) :: st
        call keymap_set(dired_mode_map, keys, command, st)
    end subroutine bind

    !> Bind [remap FROM] to COMMAND in `dired_mode_map'.
    subroutine bind_remap(from, command)
        character(len=*), intent(in) :: from, command
        type(value_type), allocatable :: events(:)
        type(status_type) :: st
        call list_elements(list(sym("remap"), sym(from)), events)
        call define_key(dired_mode_map, events, sym(command), st)
    end subroutine bind_remap

    !> Define Dired: its options, faces, commands, Dired mode and
    !> `dired_mode_map', and the keys `C-x d' in CTL_X_MAP and `C-x 4 d'
    !> in CTL_X_4_MAP.
    subroutine syms_of_dired_mode(ctl_x_map, ctl_x_4_map)
        type(handle_type), intent(in), optional :: ctl_x_map, ctl_x_4_map
        type(status_type) :: st
        if (.not. allocated(dired_listing_switches)) dired_listing_switches = "-al"
        if (.not. allocated(dired_trivial_filenames)) dired_trivial_filenames = "\`\.\.?\'\|\`\.?#"
        if (.not. allocated(dired_garbage_files_regexp)) then
            dired_garbage_files_regexp = "\(?:\.\(?:aux\|bak\|dvi\|log\|orig\|rej\|toc\)\)\'"
        end if
        if (.not. allocated(dired_header_face)) dired_header_face = "dired_header"
        if (.not. allocated(dired_mark_face)) dired_mark_face = "dired_mark"
        if (.not. allocated(dired_marked_face)) dired_marked_face = "dired_marked"
        if (.not. allocated(dired_flagged_face)) dired_flagged_face = "dired_flagged"
        if (.not. allocated(dired_warning_face)) dired_warning_face = "dired_warning"
        if (.not. allocated(dired_perm_write_face)) dired_perm_write_face = "dired_perm_write"
        if (.not. allocated(dired_directory_face)) dired_directory_face = "dired_directory"
        if (.not. allocated(dired_symlink_face)) dired_symlink_face = "dired_symlink"
        if (.not. allocated(dired_ignored_face)) dired_ignored_face = "dired_ignored"
        if (.not. associated(dired_deletion_confirmer)) dired_deletion_confirmer => yes_or_no_p
        if (.not. allocated(dired_buffers)) allocate (dired_buffers(0))
        call init_dired_faces()
        call add_local_variables_killer(kill_dired_locals)
        find_directory_functions => dired_noselect_dir
        ! Commands.
        commands%dired%run => cmd_dired
        commands%dired_other_window%run => cmd_dired_other_window
        commands%dired_mode%run => dired_mode_cmd
        commands%dired_mode%interactive = .false.
        commands%dired_next_line%run => cmd_dired_next_line
        commands%dired_previous_line%run => cmd_dired_previous_line
        commands%dired_next_dirline%run => cmd_dired_next_dirline
        commands%dired_prev_dirline%run => cmd_dired_prev_dirline
        commands%dired_next_subdir%run => cmd_dired_next_subdir
        commands%dired_prev_subdir%run => cmd_dired_prev_subdir
        commands%dired_up_directory%run => cmd_dired_up_directory
        commands%dired_find_file%run => cmd_dired_find_file
        commands%dired_find_file%advertised_binding = "RET"
        commands%dired_find_file_other_window%run => cmd_dired_find_file_other_window
        commands%dired_display_file%run => cmd_dired_display_file
        commands%dired_goto_file%run => cmd_dired_goto_file
        commands%dired_mark%run => cmd_dired_mark
        commands%dired_unmark%run => cmd_dired_unmark
        commands%dired_flag_file_deletion%run => cmd_dired_flag_file_deletion
        commands%dired_unmark_backward%run => cmd_dired_unmark_backward
        commands%dired_toggle_marks%run => cmd_dired_toggle_marks
        commands%dired_mark_files_regexp%run => cmd_dired_mark_files_regexp
        commands%dired_mark_files_containing_regexp%run => cmd_dired_mark_files_containing_regexp
        commands%dired_flag_files_regexp%run => cmd_dired_flag_files_regexp
        commands%dired_flag_garbage_files%run => cmd_dired_flag_garbage_files
        commands%dired_mark_symlinks%run => cmd_dired_mark_symlinks
        commands%dired_mark_directories%run => cmd_dired_mark_directories
        commands%dired_mark_executables%run => cmd_dired_mark_executables
        commands%dired_flag_auto_save_files%run => cmd_dired_flag_auto_save_files
        commands%dired_flag_backup_files%run => cmd_dired_flag_backup_files
        commands%dired_change_marks%run => cmd_dired_change_marks
        commands%dired_unmark_all_files%run => cmd_dired_unmark_all_files
        commands%dired_unmark_all_marks%run => cmd_dired_unmark_all_marks
        commands%dired_next_marked_file%run => cmd_dired_next_marked_file
        commands%dired_prev_marked_file%run => cmd_dired_prev_marked_file
        commands%dired_number_of_marked_files%run => cmd_dired_number_of_marked_files
        commands%dired_do_flagged_delete%run => cmd_dired_do_flagged_delete
        commands%dired_do_delete%run => cmd_dired_do_delete
        commands%dired_why%run => cmd_dired_why
        commands%dired_summary%run => cmd_dired_summary
        commands%dired_undo%run => cmd_dired_undo
        commands%dired_copy_filename_as_kill%run => cmd_dired_copy_filename_as_kill
        commands%dired_sort_toggle_or_edit%run => cmd_dired_sort_toggle_or_edit
        commands%dired_build_subdir_alist%run => cmd_dired_build_subdir_alist
        call syms_of_dired_aux()
        ! Dired mode, with a full keymap whose parent is special-mode-map.
        if (.not. keymap_live_p(dired_mode_map)) call make_keymap(dired_mode_map)
        major_modes%dired_mode%map = dired_mode_map
        call major_modes%dired_mode%define("dired_mode", "Dired", dired_mode_cmd, dired_mode_hook)
        major_modes%dired_mode%special = .true.
        call set_keymap_parent(dired_mode_map, special_mode_map, st)
        ! Commands to mark or flag certain categories of files
        call bind("#", "dired_flag_auto_save_files")
        call bind("~", "dired_flag_backup_files")
        ! Upper case keys (except !) for operating on the marked files
        call bind("D", "dired_do_delete")
        ! Tree Dired commands
        call bind("M-DEL", "dired_unmark_all_files")
        call bind("C-M-n", "dired_next_subdir")
        call bind("C-M-p", "dired_prev_subdir")
        ! move to marked files
        call bind("M-{", "dired_prev_marked_file")
        call bind("M-}", "dired_next_marked_file")
        ! Make all regexp commands share a `%' prefix:
        call bind("% d", "dired_flag_files_regexp")
        call bind("% g", "dired_mark_files_containing_regexp")
        call bind("% m", "dired_mark_files_regexp")
        call bind("% &", "dired_flag_garbage_files")
        ! Commands for marking and unmarking.
        call bind("* *", "dired_mark_executables")
        call bind("* /", "dired_mark_directories")
        call bind("* @", "dired_mark_symlinks")
        call bind("* %", "dired_mark_files_regexp")
        call bind("* N", "dired_number_of_marked_files")
        call bind("* c", "dired_change_marks")
        call bind("* s", "dired_mark_subdir_files")
        call bind("* m", "dired_mark")
        call bind("* u", "dired_unmark")
        call bind("* ?", "dired_unmark_all_files")
        call bind("* !", "dired_unmark_all_marks")
        call bind("U", "dired_unmark_all_marks")
        call bind("* DEL", "dired_unmark_backward")
        call bind("* C-n", "dired_next_marked_file")
        call bind("* C-p", "dired_prev_marked_file")
        call bind("* t", "dired_toggle_marks")
        ! Lower keys for commands not operating on all the marked files
        call bind("d", "dired_flag_file_deletion")
        call bind("e", "dired_find_file")
        call bind("f", "dired_find_file")
        call bind("C-m", "dired_find_file")
        call bind("g", "revert_buffer")
        call bind("j", "dired_goto_file")
        call bind("l", "dired_do_redisplay")
        call bind("m", "dired_mark")
        call bind("n", "dired_next_line")
        call bind("o", "dired_find_file_other_window")
        call bind("C-o", "dired_display_file")
        call bind("p", "dired_previous_line")
        call bind("s", "dired_sort_toggle_or_edit")
        call bind("t", "dired_toggle_marks")
        call bind("u", "dired_unmark")
        call bind("w", "dired_copy_filename_as_kill")
        call bind("x", "dired_do_flagged_delete")
        call bind("+", "dired_create_directory")
        ! moving
        call bind("<", "dired_prev_dirline")
        call bind(">", "dired_next_dirline")
        call bind("^", "dired_up_directory")
        call bind("SPC", "dired_next_line")
        call bind("S-SPC", "dired_previous_line")
        call bind_remap("next_line", "dired_next_line")
        call bind_remap("previous_line", "dired_previous_line")
        ! misc
        call bind("?", "dired_summary")
        call bind("DEL", "dired_unmark_backward")
        call bind_remap("undo", "dired_undo")
        call dired_aux_keys()
        call register_keymap_name("dired_mode_map", dired_mode_map)
        ! (keymap-set ctl-x-map "d" #'dired)
        if (present(ctl_x_map)) call keymap_set(ctl_x_map, "d", "dired", st)
        ! (keymap-set ctl-x-4-map "d" #'dired-other-window)
        if (present(ctl_x_4_map)) call keymap_set(ctl_x_4_map, "d", "dired_other_window", st)
    end subroutine syms_of_dired_mode
end module dired_mode_fm
