! minibuffer_fm.f90 -- minibuffer commands and reading file names.
! Ported from GNU Emacs 31.1 lisp/minibuffer.el.
!
! Copyright (C) 2008-2026 Free Software Foundation, Inc.
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

!> Leaving the minibuffer, its basic keys, `read_file_name', and
!> completion: `completing_read', the completion styles basic and emacs22,
!> TAB, SPC, ? and RET in the completion maps, `minibuffer_message', and
!> the *Completions* list (shown in the root window while the minibuffer
!> is active, as there is no window splitting yet).
!>
!> Not ported yet: the partial-completion, substring and flex styles,
!> cycling, completion metadata (categories, annotations, groups),
!> moving through and choosing from *Completions* (M-<up>, M-<down>,
!> M-RET, mouse), `completion_at_point' in buffers, `completion_show_help',
!> and completion of environment variables and ~USER in file names.
module minibuffer_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use commands_fm, only: commands
    use error_fm, only: format_message, status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use buffer_fm, only: buffers, cur, get_buffer, get_buffer_create, set_buffer, current_buffer, erase_buffer, &
                         set_buffer_read_only, buffer_list, bury_buffer_internal, vdeactivate_mark
    use insdel_fm, only: del_range, insert
    use keymap_fm, only: define_key, key_parse, make_sparse_keymap, set_keymap_parent, keymap_live_p, &
                         free_keymap
    use lisp_fm, only: value_type, nil, t_value, make_string, make_fixnum, list, list_elements, list_length, &
                       list_elt, fixnump, xfixnum, stringp, xstring, nilp, make_list
    use data_fm, only: value_eq, symbol_value, set
    use character_fm, only: string_char_and_length
    use editfns_fm, only: point, point_max, goto_char, buffer_substring
    use frame_fm, only: selected_frame, frame_selected_window_ref
    use window_fm, only: window_type, windows, set_window_start, window_live_p, same_window, get_buffer_window, &
                         temp_buffer_window_show, fit_window_to_buffer, display_alist_type, delete_window, &
                         switch_to_prev_buffer, scroll_window, pos_visible_in_window_p, minibuffer_scroll_window, &
                         display_buffer_mark_dedicated, DEDICATED_NONE, DEDICATED_WEAK, BK_BURY, &
                         DISPLAY_ACTION_MAYBE_SAME_WINDOW, DISPLAY_ACTION_REUSE_WINDOW, DISPLAY_ACTION_AT_BOTTOM, &
                         recenter_top_bottom, scroll_up_command, &
                         scroll_down_command, set_window_buffer
    use callint_fm, only: current_prefix_arg
    use xdisp_fm, only: message
    use dispnew_fm, only: ding
    use fns_fm, only: compare_strings, COMPARE_EQUAL, schars, substring, string_lessp, yes_or_no_p
    use dired_fm, only: file_name_completion, file_name_all_completions
    use lread_fm, only: sym
    use keyboard_fm, only: throw_exit, command_loop_level, this_command, last_command, minibuffer_message_text, &
                           minibuffer_message_timeout
    use timer_fm, only: run_at_time, free_timer, timerp
    use minibuf_fm, only: minibufferp, minibuf_level, minibuffer_prompt_end, abort_minibuffers, &
                          minibuffer_innermost_command_loop_p, innermost_minibuffer_p, &
                          minibuffer_local_map, read_from_minibuffer, completion_table_type, &
                          completion_predicate, completion_function_table, try_completion, all_completions, &
                          test_completion, completion_ignore_case, TABLE_FUNCTION, COMPLETE_TRY, COMPLETE_ALL, &
                          COMPLETE_TEST, COMPLETE_BOUNDARIES, minibuffer_calling_buffer, completion_list_table, &
                          minibuffer_setup_function, minibuffer_setup_proc, history_added, minibuffer_default, &
                          minibuffer_string_type
    use fileio_fm, only: file_name_absolute_p, expand_file_name, substitute_in_file_name, file_name_directory, &
                         file_name_nondirectory, file_exists_p, file_name_as_directory
    use subr_fm, only: add_to_history, history_delete_duplicates
    use files_fm, only: abbreviate_file_name
    implicit none(type, external)
    private

    public :: exit_minibuffer, delete_minibuffer_contents, read_file_name, syms_of_minibuffer, read_buffer
    public :: completion_insert_strings
    public :: internal_complete_buffer, minibuffer_maybe_quote_filename
    public :: completing_read, completion_try_completion, completion_all_completions, completion_boundaries, &
              minibuffer_complete, minibuffer_complete_word, minibuffer_complete_and_exit, &
              minibuffer_completion_help, minibuffer_hide_completions, minibuffer_message, &
              read_file_name_internal

    !> Values of REQUIRE_MATCH for `completing_read', also those of
    !> `minibuffer_completion_confirm': nil, t, `confirm',
    !> `confirm_after_completion' and any other non-nil value.
    integer, parameter, public :: REQUIRE_MATCH_NIL = 0, REQUIRE_MATCH_T = 1, REQUIRE_MATCH_CONFIRM = 2, &
                                  REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION = 3, REQUIRE_MATCH_OTHER = 4

    !> Kinds of `completion_try_completion' result: nil, t, and (STRING .
    !> POINT).
    integer, parameter, public :: TRY_NONE = 0, TRY_EXACT = 1, TRY_STRING = 2

    !> A `completion_try_completion' result.
    type, public :: try_result_type
        integer :: kind = TRY_NONE
        character(len=:), allocatable :: string
        integer(int64) :: point = 0
    end type try_result_type

    !> A string in an array.
    type :: string_box
        character(len=:), allocatable :: s
    end type string_box

    !> The completion table of the active minibuffer (minibuffer.el
    !> minibuffer-completion-table), its predicate (null for none) and
    !> minibuffer-completion-confirm (a REQUIRE_MATCH_ value).
    type(completion_table_type), public :: minibuffer_completion_table
    procedure(completion_predicate), pointer, public :: minibuffer_completion_predicate => null()
    integer, public :: minibuffer_completion_confirm = REQUIRE_MATCH_NIL

    !> Whether the minibuffer is completing a file name.  Port of
    !> minibuffer.el minibuffer-completing-file-name.
    logical, public :: minibuffer_completing_file_name = .false.

    !> True means automatically provide help for invalid completion
    !> input.  (Only true and false: `lazy', `always' and `visible' are not
    !> ported.)  Port of minibuffer.el completion-auto-help.
    logical, public :: completion_auto_help = .true.

    !> If true, print helpful inline messages during completion.  Port
    !> of minibuffer.el completion-show-inline-help.
    logical, public :: completion_show_inline_help = .true.

    !> If true, stay quiet when there is no match.  Port of
    !> minibuffer.el completion-fail-discreetly.
    logical, public :: completion_fail_discreetly = .false.

    !> Local keymap for minibuffer input with completion; the one for an
    !> exact match; and both with SPC unbound for file names.  Ports of
    !> minibuffer.el minibuffer-local-completion-map,
    !> minibuffer-local-must-match-map and (composed with each)
    !> minibuffer-local-filename-completion-map.
    type(handle_type), public :: minibuffer_local_completion_map, minibuffer_local_must_match_map, &
                                 minibuffer_local_filename_completion_map, &
                                 minibuffer_local_filename_must_match_map

    !> The last text `minibuffer_message' showed (not cleared by the next
    !> command, unlike the display), for programs and tests.
    character(len=:), allocatable, public :: last_minibuffer_message

    !> The timer that removes the text `minibuffer_message' shows.  Port
    !> of minibuffer.el minibuffer--message-timer.
    type(handle_type) :: minibuffer_message_timer

    !> Maximum height for *Completions* buffer window (0 for no maximum).
    !> Port of minibuffer.el completions-max-height.
    integer, public :: completions_max_height = 0

    !> True means when reading a filename start with default dir in minibuffer.
    !>
    !> When the initial minibuffer contents show a name of a file or a directory,
    !> typing RETURN without editing the initial contents is equivalent to typing
    !> the default file name.
    !>
    !> If this variable is true, the minibuffer contents are always
    !> initially non-empty, and typing RETURN without editing will fetch the
    !> default name, if one is provided.
    !>
    !> If this variable is false, the minibuffer often starts out empty.
    !> Port of minibuffer.el insert-default-directory.
    logical, public :: insert_default_directory = .true.

    !> True means when reading a file name completion ignores case.
    !> Port of minibuffer.el read-file-name-completion-ignore-case (nil on
    !> GNU/Linux).
    logical, public :: read_file_name_completion_ignore_case = .false.

    !> What `read_file_name_setup' sets up: the minibuffer's directory and
    !> the initial input (none unless `rfn_have_insdef').
    character(len=:), allocatable :: rfn_dir, rfn_insdef
    logical :: rfn_have_insdef = .false.

    !> True means completion ignores case when reading a buffer name.
    !> Port of minibuf.c read-buffer-completion-ignore-case.
    logical, public :: read_buffer_completion_ignore_case = .false.

    !> The buffer name internal-complete-buffer-except leaves out
    !> (unallocated for none) and the predicate it combines with its own.
    character(len=:), allocatable :: rbts_except
    procedure(completion_predicate), pointer :: rbts_pred2 => null()

    !> completing-read-default's minibuffer setup: the ambient
    !> `completion_ignore_case', and the setup function of an enclosing
    !> minibuffer-with-setup-hook, run after it.
    logical :: crd_ignore_case = .false.
    procedure(minibuffer_setup_proc), pointer :: crd_outer_setup => null()

contains

    !> Terminate this minibuffer argument.  Port of
    !> minibuffer.el:exit-minibuffer.
    subroutine exit_minibuffer(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (minibufferp()) then
            if (.not. minibuffer_innermost_command_loop_p()) then
                call set_error(status, FM_GENERIC_ERROR, "Not in most nested command loop")
                return
            end if
            if (.not. innermost_minibuffer_p()) then
                call set_error(status, FM_GENERIC_ERROR, "Not in most nested minibuffer")
                return
            end if
        end if
        ! If the command that uses this has made modifications in the
        ! minibuffer, we don't want them to cause deactivation of the mark
        ! in the original buffer.
        vdeactivate_mark = .false.
        call throw_exit(.false.)
    end subroutine exit_minibuffer

    !> Delete the text after the prompt.  Port of
    !> minibuffer.el:delete-minibuffer-contents.
    subroutine delete_minibuffer_contents(status)
        type(status_type), intent(out) :: status
        call del_range(minibuffer_prompt_end(), buffers(cur)%zv, status)
    end subroutine delete_minibuffer_contents

    !> Read a file name, prompting with PROMPT and completing in directory DIR.
    !> Return the file name as a string.
    !> The return value is not expanded---you must call `expand_file_name'
    !> yourself.
    !>
    !> DIR is the directory to use for completing relative file names.
    !> It should be an absolute directory name, or absent (which means the
    !> current buffer's value of `default_directory').
    !>
    !> DEFAULT_FILENAME specifies the default file name to return if the
    !> user exits the minibuffer with the same non-empty string inserted
    !> by this function.  If DEFAULT_FILENAME is absent, then if INITIAL
    !> is present, the default is DIR combined with INITIAL; otherwise, if
    !> the current buffer is visiting a file, that file serves as the
    !> default; otherwise, the default is simply the string inserted into
    !> the minibuffer.
    !>
    !> If the user exits with an empty minibuffer, return an empty
    !> string.  (This happens only if the user erases the pre-inserted
    !> contents, or if `insert_default_directory' is false.)
    !>
    !> Fourth arg MUSTMATCH can take the following values:
    !> - REQUIRE_MATCH_NIL means that the user can exit with any input.
    !> - REQUIRE_MATCH_T means that the user is not allowed to exit unless
    !>   the input is (or completes to) an existing file.
    !> - REQUIRE_MATCH_CONFIRM means that the user can exit with any input,
    !>   but she needs to confirm her choice if the input is not an
    !>   existing file.
    !> - REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION means that the user can
    !>   exit with any input, but she needs to confirm her choice if she
    !>   called `minibuffer_complete' right before
    !>   `minibuffer_complete_and_exit' and the input is not an existing
    !>   file.
    !> - REQUIRE_MATCH_OTHER behaves like REQUIRE_MATCH_T except that
    !>   typing RET does not exit if it does non-null completion.
    !>
    !> Fifth arg INITIAL specifies text to start with.  It will be
    !> interpreted as the trailing part of DEFAULT_FILENAME, so using a
    !> full file name for INITIAL will usually lead to surprising
    !> results.
    !>
    !> See also `read_file_name_completion_ignore_case'.  Port of
    !> minibuffer.el:read-file-name-default (no PREDICATE, function
    !> MUSTMATCH, list DEFAULT_FILENAME, file dialogs,
    !> minibuffer-default-add-function, minibuffer-local-filename-syntax or
    !> file-name-quote for names containing "/~": Deferred).
    recursive subroutine read_file_name(prompt, result, status, dir, default_filename, mustmatch, initial)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: dir, default_filename, initial
        integer, intent(in), optional :: mustmatch
        character(len=:), allocatable :: d, default, insdef, val, val1, expanded
        type(value_type) :: history
        type(value_type), allocatable :: elems(:), kept(:)
        type(status_type) :: st
        logical :: have_default, have_insdef, saved_completing, used_default, replace_in_history, add_history
        integer :: match, k, n
        if (present(dir)) then
            d = dir
        else if (allocated(buffers(cur)%directory)) then
            d = buffers(cur)%directory
        else
            d = "~/"
        end if
        if (.not. file_name_absolute_p(d)) then
            call expand_file_name(d, val, status)
            if (.not. status%ok()) return
            d = val
        end if
        have_default = .true.
        if (present(default_filename)) then
            default = default_filename
        else if (.not. present(initial)) then
            have_default = allocated(buffers(cur)%filename)
            if (have_default) default = buffers(cur)%filename
        else if (len(initial) == 0) then
            ! Special-case "" because (expand-file-name "" "/tmp/")
            ! returns "/tmp" rather than "/tmp/" (bug#39057).
            default = d
        else
            call expand_file_name(initial, default, status, d)
            if (.not. status%ok()) return
        end if
        ! If dir starts with user's homedir, change that to ~.
        d = abbreviate_file_name(d)
        ! Likewise for default-filename.
        if (have_default) default = abbreviate_file_name(default)
        if (insert_default_directory) then
            if (present(initial)) then
                insdef = minibuffer_maybe_quote_filename(d//initial)
            else
                insdef = minibuffer_maybe_quote_filename(d)
            end if
        else if (present(initial)) then
            insdef = minibuffer_maybe_quote_filename(initial)
        else
            insdef = ""
        end if
        have_insdef = insert_default_directory .or. present(initial)
        match = REQUIRE_MATCH_NIL
        if (present(mustmatch)) match = mustmatch
        ! The minibuffer's default-directory: DIR as a directory name.
        call expand_file_name(d, expanded, status)
        if (.not. status%ok()) return
        rfn_dir = file_name_as_directory(expanded)
        rfn_insdef = insdef
        rfn_have_insdef = have_insdef
        minibuffer_setup_function => read_file_name_setup
        saved_completing = minibuffer_completing_file_name
        minibuffer_completing_file_name = .true.
        if (have_insdef .and. have_default) then
            call completing_read(prompt, completion_function_table(read_file_name_internal), val, status, &
                                 require_match=match, initial=insdef, history="file_name_history", &
                                 default=default, used_default=used_default)
        else if (have_insdef) then
            call completing_read(prompt, completion_function_table(read_file_name_internal), val, status, &
                                 require_match=match, initial=insdef, history="file_name_history", &
                                 used_default=used_default)
        else if (have_default) then
            call completing_read(prompt, completion_function_table(read_file_name_internal), val, status, &
                                 require_match=match, history="file_name_history", default=default, &
                                 used_default=used_default)
        else
            call completing_read(prompt, completion_function_table(read_file_name_internal), val, status, &
                                 require_match=match, history="file_name_history", used_default=used_default)
        end if
        minibuffer_completing_file_name = saved_completing
        minibuffer_setup_function => null()
        if (.not. status%ok()) return
        ! What completing-read returned is the history's first element
        ! (`eq') if read_from_minibuffer added it.
        replace_in_history = history_added
        add_history = .false.
        ! If completing-read returned the inserted default string itself
        ! (rather than a new string with the same contents), it has to
        ! mean that the user typed RET with the minibuffer empty.  In that
        ! case, we really want to return "" so that commands such as
        ! set-visited-file-name can distinguish.
        if (used_default) then
            ! In this case, completing-read has not added an element to
            ! the history.  Maybe we should.
            if (.not. replace_in_history) add_history = .true.
            val = ""
        end if
        if (have_default .and. have_insdef) then
            if (val == insdef .and. len(val) == len(insdef)) val = default
        end if
        val = substitute_in_file_name(val)
        if (replace_in_history) then
            ! Replace what completing_read added to the history with what
            ! we will actually return.  As an exception, if that's the
            ! same as the second item in file-name-history, it's really a
            ! repeat (Bug#4657).
            val1 = minibuffer_maybe_quote_filename(val)
            call symbol_value(sym("file_name_history"), history, st)
            if (st%ok()) then
                call list_elements(history, elems)
                if (size(elems) > 0) then
                    allocate (kept(size(elems)))
                    kept(1) = elems(1)
                    n = 1
                    do k = 2, size(elems)
                        if (history_delete_duplicates .and. string_equal(elems(k), val1)) cycle
                        n = n + 1
                        kept(n) = elems(k)
                    end do
                    if (n >= 2) then
                        if (string_equal(kept(2), val1)) then
                            call set(sym("file_name_history"), make_list(kept(2:n)), st)
                        else
                            kept(1) = make_string(val1)
                            call set(sym("file_name_history"), make_list(kept(1:n)), st)
                        end if
                    else
                        kept(1) = make_string(val1)
                        call set(sym("file_name_history"), make_list(kept(1:n)), st)
                    end if
                end if
            end if
        else if (add_history) then
            call add_to_history("file_name_history", minibuffer_maybe_quote_filename(val))
        end if
        result = val
    contains

        logical function string_equal(v, s)
            type(value_type), intent(in) :: v
            character(len=*), intent(in) :: s
            string_equal = stringp(v)
            if (string_equal) string_equal = xstring(v) == s .and. len(xstring(v)) == len(s)
        end function string_equal
    end subroutine read_file_name

    !> The minibuffer-with-setup-hook function of `read_file_name': the
    !> minibuffer's default-directory is the directory read in, a first
    !> default equal to the initial input is dropped, and
    !> `completion_ignore_case' is `read_file_name_completion_ignore_case'.
    !> Part of minibuffer.el:read-file-name-default.
    subroutine read_file_name_setup()
        type(minibuffer_string_type), allocatable :: rest(:)
        integer :: k
        buffers(cur)%directory = rfn_dir
        ! When the first default in `minibuffer-default' duplicates
        ! initial input `insdef', reset `minibuffer-default' to nil.
        if (allocated(minibuffer_default) .and. rfn_have_insdef) then
            if (size(minibuffer_default) > 0) then
                if (minibuffer_default(1)%s == rfn_insdef .and. len(minibuffer_default(1)%s) == len(rfn_insdef)) then
                    ! (cdr-safe minibuffer-default)
                    allocate (rest(size(minibuffer_default) - 1))
                    do k = 2, size(minibuffer_default)
                        rest(k - 1)%s = minibuffer_default(k)%s
                    end do
                    deallocate (minibuffer_default)
                    if (size(rest) > 0) call move_alloc(rest, minibuffer_default)
                end if
            end if
        end if
        call completion_ignore_case%set_local(read_file_name_completion_ignore_case)
    end subroutine read_file_name_setup

    !> Protect FILENAME from `substitute_in_file_name', as needed.
    !> Useful to give the user default values that won't be substituted.
    !> Port of minibuffer.el:minibuffer-maybe-quote-filename (file-name-quote
    !> for names with "/~" is not ported: they are only $-doubled).
    function minibuffer_maybe_quote_filename(filename) result(quoted)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: quoted
        quoted = double_dollars(filename)
    end function minibuffer_maybe_quote_filename

    !> Return STR with dollar signs doubled.  Port of
    !> minibuffer.el:minibuffer--double-dollars.
    pure function double_dollars(str) result(doubled)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: doubled
        integer :: i
        doubled = ""
        do i = 1, len(str)
            if (str(i:i) == "$") then
                doubled = doubled//"$$"
            else
                doubled = doubled//str(i:i)
            end if
        end do
    end function double_dollars

    !> Terminate this minibuffer argument.
    !> Port of minibuffer.el:exit-minibuffer.
    subroutine cmd_exit_minibuffer(status)
        type(status_type), intent(out) :: status
        call exit_minibuffer(status)
    end subroutine cmd_exit_minibuffer

    !> Abort the current minibuffer.
    !> If we are not currently in the innermost minibuffer, prompt the user to
    !> confirm the aborting of the current minibuffer and all contained ones.
    !> Port of minibuf.c:abort-minibuffers.
    subroutine cmd_abort_minibuffers(status)
        type(status_type), intent(out) :: status
        call abort_minibuffers(status, yes_or_no_p)
    end subroutine cmd_abort_minibuffers

    subroutine cmd_minibuffer_complete(status)
        type(status_type), intent(out) :: status
        call minibuffer_complete(status)
    end subroutine cmd_minibuffer_complete

    subroutine cmd_minibuffer_complete_word(status)
        type(status_type), intent(out) :: status
        call minibuffer_complete_word(status)
    end subroutine cmd_minibuffer_complete_word

    subroutine cmd_minibuffer_completion_help(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call minibuffer_completion_help()
    end subroutine cmd_minibuffer_completion_help

    subroutine cmd_minibuffer_complete_and_exit(status)
        type(status_type), intent(out) :: status
        call minibuffer_complete_and_exit(status)
    end subroutine cmd_minibuffer_complete_and_exit

    !> Call `exit_minibuffer' (there is no selected completion to insert
    !> first yet).  Port of minibuffer.el:minibuffer-completion-exit.
    subroutine cmd_minibuffer_completion_exit(status)
        type(status_type), intent(out) :: status
        call exit_minibuffer(status)
    end subroutine cmd_minibuffer_completion_exit

    !> Run `recenter_top_bottom' from the minibuffer in its original window.
    !> Port of minibuffer.el:minibuffer-recenter-top-bottom.
    subroutine cmd_minibuffer_recenter_top_bottom(status)
        type(status_type), intent(out) :: status
        call with_minibuffer_selected_window(1, status)
    end subroutine cmd_minibuffer_recenter_top_bottom

    !> Run `scroll_up_command' from the minibuffer in its original window.
    !> Port of minibuffer.el:minibuffer-scroll-up-command (no shift
    !> selection).
    subroutine cmd_minibuffer_scroll_up_command(status)
        type(status_type), intent(out) :: status
        call with_minibuffer_selected_window(2, status)
    end subroutine cmd_minibuffer_scroll_up_command

    !> Run `scroll_down_command' from the minibuffer in its original window.
    !> Port of minibuffer.el:minibuffer-scroll-down-command (no shift
    !> selection).
    subroutine cmd_minibuffer_scroll_down_command(status)
        type(status_type), intent(out) :: status
        call with_minibuffer_selected_window(3, status)
    end subroutine cmd_minibuffer_scroll_down_command

    !> Run command `which' (1 recenter-top-bottom, 2 scroll-up-command,
    !> 3 scroll-down-command) with the prefix argument in the window that
    !> was selected when the minibuffer was entered.  Port of
    !> minibuffer.el:with-minibuffer-selected-window.
    recursive subroutine with_minibuffer_selected_window(which, status)
        integer, intent(in) :: which
        type(status_type), intent(out) :: status
        type(handle_type) :: mini
        type(window_type), pointer :: sw
        type(status_type) :: st
        logical :: saved
        status = status_type()
        if (.not. selected_frame%minibuffer_selected .or. minibuf_level == 0) return
        mini = current_buffer()
        saved = selected_frame%minibuffer_selected
        selected_frame%minibuffer_selected = .false.
        sw => frame_selected_window_ref()
        call set_buffer(sw%buffer, st)
        select case (which)
        case (1)
            call recenter_top_bottom(current_prefix_arg, status)
        case (2)
            call scroll_up_command(current_prefix_arg, status)
        case default
            call scroll_down_command(current_prefix_arg, status)
        end select
        selected_frame%minibuffer_selected = saved
        call set_buffer(mini, st)
    end subroutine with_minibuffer_selected_window

    !> Register the commands, minibuffer.el's minibuffer-local-map keys, and
    !> the completion keymaps.
    subroutine syms_of_minibuffer()
        type(status_type) :: status
        commands%exit_minibuffer%run => cmd_exit_minibuffer
        commands%abort_minibuffers%run => cmd_abort_minibuffers
        commands%minibuffer_complete%run => cmd_minibuffer_complete
        commands%minibuffer_complete_word%run => cmd_minibuffer_complete_word
        commands%minibuffer_completion_help%run => cmd_minibuffer_completion_help
        commands%minibuffer_complete_and_exit%run => cmd_minibuffer_complete_and_exit
        commands%minibuffer_completion_exit%run => cmd_minibuffer_completion_exit
        commands%minibuffer_recenter_top_bottom%run => cmd_minibuffer_recenter_top_bottom
        commands%minibuffer_scroll_up_command%run => cmd_minibuffer_scroll_up_command
        commands%minibuffer_scroll_down_command%run => cmd_minibuffer_scroll_down_command
        call bind(minibuffer_local_map, "C-g", "abort_minibuffers")
        call bind(minibuffer_local_map, "C-j", "exit_minibuffer")
        call bind(minibuffer_local_map, "RET", "exit_minibuffer")
        ! Rebuilding: drop the old maps.
        if (keymap_live_p(minibuffer_local_completion_map)) call free_keymap(minibuffer_local_completion_map, status)
        if (keymap_live_p(minibuffer_local_must_match_map)) call free_keymap(minibuffer_local_must_match_map, status)
        if (keymap_live_p(minibuffer_local_filename_completion_map)) then
            call free_keymap(minibuffer_local_filename_completion_map, status)
        end if
        if (keymap_live_p(minibuffer_local_filename_must_match_map)) then
            call free_keymap(minibuffer_local_filename_must_match_map, status)
        end if
        call make_sparse_keymap(minibuffer_local_completion_map)
        call set_keymap_parent(minibuffer_local_completion_map, minibuffer_local_map, status)
        call bind(minibuffer_local_completion_map, "TAB", "minibuffer_complete")
        call bind(minibuffer_local_completion_map, "<backtab>", "minibuffer_complete")
        call bind(minibuffer_local_completion_map, "SPC", "minibuffer_complete_word")
        call bind(minibuffer_local_completion_map, "RET", "minibuffer_completion_exit")
        call bind(minibuffer_local_completion_map, "?", "minibuffer_completion_help")
        call make_sparse_keymap(minibuffer_local_must_match_map)
        call set_keymap_parent(minibuffer_local_must_match_map, minibuffer_local_completion_map, status)
        call bind(minibuffer_local_must_match_map, "RET", "minibuffer_complete_and_exit")
        call bind(minibuffer_local_must_match_map, "C-j", "minibuffer_complete_and_exit")
        ! minibuffer-local-filename-completion-map ("SPC" nil) composed with
        ! each: an explicit nil hides the parent's binding.
        call make_sparse_keymap(minibuffer_local_filename_completion_map)
        call set_keymap_parent(minibuffer_local_filename_completion_map, minibuffer_local_completion_map, status)
        call bind_nil(minibuffer_local_filename_completion_map, "SPC")
        call make_sparse_keymap(minibuffer_local_filename_must_match_map)
        call set_keymap_parent(minibuffer_local_filename_must_match_map, minibuffer_local_must_match_map, status)
        call bind_nil(minibuffer_local_filename_must_match_map, "SPC")
    contains

        subroutine bind(map, keys, command)
            type(handle_type), intent(in) :: map
            character(len=*), intent(in) :: keys, command
            type(value_type), allocatable :: seq(:)
            type(status_type) :: st
            call key_parse(keys, seq, st)
            if (st%ok()) call define_key(map, seq, sym(command), st)
        end subroutine bind

        subroutine bind_nil(map, keys)
            type(handle_type), intent(in) :: map
            character(len=*), intent(in) :: keys
            type(value_type), allocatable :: seq(:)
            type(status_type) :: st
            call key_parse(keys, seq, st)
            if (st%ok()) call define_key(map, seq, nil(), st)
        end subroutine bind_nil
    end subroutine syms_of_minibuffer

    ! ------------------------------------------------------------------
    ! Completion styles
    ! ------------------------------------------------------------------

    !> Return the boundaries of text on which COLLECTION will operate.
    !> STRING is the string on which completion will be performed.
    !> SUFFIX is the string after point.  If COLLECTION is a function, it
    !> is called with STRING, PRED and COMPLETE_BOUNDARIES with SUFFIX.
    !>
    !> The result is (START . END) where START is the position in STRING
    !> of the beginning of the completion field and END is the position in
    !> SUFFIX of the end of the completion field.  E.g. for simple
    !> completion tables, the result is always (0 . (length SUFFIX)) and
    !> for file names the result is the positions delimited by the
    !> closest directory separators.  Port of
    !> minibuffer.el:completion-boundaries.
    subroutine completion_boundaries(string, collection, suffix, bstart, bend, pred)
        character(len=*), intent(in) :: string, suffix
        type(completion_table_type), intent(in) :: collection
        integer(int64), intent(out) :: bstart, bend
        procedure(completion_predicate), optional :: pred
        type(value_type) :: boundaries
        type(status_type) :: status
        bstart = 0
        bend = schars(suffix)
        if (collection%kind /= TABLE_FUNCTION) return
        call collection%fn(string, COMPLETE_BOUNDARIES, boundaries, status, pred, suffix)
        if (.not. status%ok() .or. list_length(boundaries) /= 2) return
        if (fixnump(list_elt(boundaries, 1_int64))) bstart = xfixnum(list_elt(boundaries, 1_int64))
        if (fixnump(list_elt(boundaries, 2_int64))) bend = xfixnum(list_elt(boundaries, 2_int64))
    end subroutine completion_boundaries

    !> Merge end of COMPLETION with beginning of SUFFIX.
    !> Simple generalization of the "merge trailing /" done in Emacs-22.
    !> Return the new suffix.  Port of minibuffer.el:completion--merge-suffix.
    function merge_suffix(completion, point, suffix) result(r)
        character(len=*), intent(in) :: completion, suffix
        integer(int64), intent(in) :: point
        character(len=:), allocatable :: r, tail
        integer(int64) :: i, n
        r = suffix
        if (len(suffix) == 0) return
        n = schars(completion)
        ! The leftmost end of COMPLETION (from POINT on) that SUFFIX starts
        ! with; it must not be empty.
        do i = point, n - 1
            tail = substring(completion, i)
            if (len(tail) <= len(suffix)) then
                if (suffix(1:len(tail)) == tail) then
                    r = suffix(len(tail) + 1:)
                    return
                end if
            end if
        end do
    end function merge_suffix

    !> Port of minibuffer.el:completion-basic-try-completion (text after
    !> point that is part of the completion field needs
    !> partial-completion's matching: it gives nil here, so the next style
    !> runs).
    subroutine basic_try_completion(string, table, point, res, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: res
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: beforepoint, afterpoint
        type(value_type) :: completion
        type(status_type) :: status
        integer(int64) :: bstart, bend
        beforepoint = substring(string, 0_int64, point)
        afterpoint = substring(string, point)
        call completion_boundaries(beforepoint, table, afterpoint, bstart, bend, pred)
        if (bend /= 0) return
        ! `try_completion' may return a subtly different result than
        ! `all+merge', so try to use it whenever possible.
        call try_completion(beforepoint, table, completion, status, pred)
        call set_try_result(completion, res)
        if (res%kind == TRY_STRING) then
            res%point = schars(res%string)
            res%string = res%string//merge_suffix(res%string, point, afterpoint)
        end if
    end subroutine basic_try_completion

    !> Port of minibuffer.el:completion-emacs22-try-completion.
    subroutine emacs22_try_completion(string, table, point, res, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: res
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: suffix
        type(value_type) :: completion
        type(status_type) :: status
        suffix = substring(string, point)
        call try_completion(substring(string, 0_int64, point), table, completion, status, pred)
        call set_try_result(completion, res)
        select case (res%kind)
        case (TRY_EXACT)
            if (len(suffix) > 0) then
                res%kind = TRY_STRING
                res%string = string
                res%point = point
            end if
        case (TRY_STRING)
            ! Merge a trailing / in completion with a / after point.
            if (len(res%string) > 0 .and. len(suffix) > 0) then
                if (res%string(len(res%string):) == "/" .and. suffix(1:1) == "/") suffix = suffix(2:)
            end if
            res%point = schars(res%string)
            res%string = res%string//suffix
        case default
            ! nil: no completion.
        end select
    end subroutine emacs22_try_completion

    !> The `try_completion' value `v' as a try result.
    subroutine set_try_result(v, res)
        type(value_type), intent(in) :: v
        type(try_result_type), intent(out) :: res
        if (stringp(v)) then
            res%kind = TRY_STRING
            res%string = xstring(v)
        else if (nilp(v)) then
            res%kind = TRY_NONE
        else
            res%kind = TRY_EXACT
        end if
    end subroutine set_try_result

    !> Try to complete STRING using completion table TABLE.
    !> Only the elements of table that satisfy predicate PRED are
    !> considered.  POINT is the position of point within STRING.  The
    !> result is nil to indicate that there is no completion, t to
    !> indicate that STRING is the only possible completion, or a pair
    !> (NEWSTRING . NEWPOINT) of the completed result string together
    !> with a new position for point.  Port of
    !> minibuffer.el:completion-try-completion (completion-styles basic
    !> and emacs22; partial-completion is not ported).
    subroutine completion_try_completion(string, table, point, res, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: res
        procedure(completion_predicate), optional :: pred
        call basic_try_completion(string, table, point, res, pred)
        if (res%kind == TRY_NONE) call emacs22_try_completion(string, table, point, res, pred)
    end subroutine completion_try_completion

    !> List the possible completions of STRING in completion table TABLE.
    !> Only the elements of table that satisfy predicate PRED are
    !> considered.  POINT is the position of point within STRING.  The
    !> completions go to `all' and the base-size to `base_size'.  Port of
    !> minibuffer.el:completion-all-completions (styles basic, then
    !> emacs22, as in completion-try-completion).
    subroutine completion_all_completions(string, table, point, all, base_size, pred)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: table
        integer(int64), intent(in) :: point
        type(value_type), intent(out) :: all
        integer(int64), intent(out) :: base_size
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: beforepoint, afterpoint
        type(status_type) :: status
        integer(int64) :: bstart, bend
        beforepoint = substring(string, 0_int64, point)
        afterpoint = substring(string, point)
        call completion_boundaries(beforepoint, table, afterpoint, bstart, bend, pred)
        base_size = bstart
        all = nil()
        ! completion-basic-all-completions: with nothing of the field
        ! after point, the pattern is trivial: all-completions of the
        ! text before point.
        if (bend == 0) call all_completions(beforepoint, table, all, status, pred)
        if (list_length(all) > 0) return
        ! completion-emacs22-all-completions.
        call all_completions(beforepoint, table, all, status, pred)
    end subroutine completion_all_completions

    ! The minibuffer's table and predicate (a null predicate pointer is
    ! passed as an absent argument).

    subroutine mb_boundaries(string, suffix, bstart, bend)
        character(len=*), intent(in) :: string, suffix
        integer(int64), intent(out) :: bstart, bend
        if (associated(minibuffer_completion_predicate)) then
            call completion_boundaries(string, minibuffer_completion_table, suffix, bstart, bend, &
                                       minibuffer_completion_predicate)
        else
            call completion_boundaries(string, minibuffer_completion_table, suffix, bstart, bend)
        end if
    end subroutine mb_boundaries

    subroutine mb_try_completion(string, point, res)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: res
        if (associated(minibuffer_completion_predicate)) then
            call completion_try_completion(string, minibuffer_completion_table, point, res, &
                                           minibuffer_completion_predicate)
        else
            call completion_try_completion(string, minibuffer_completion_table, point, res)
        end if
    end subroutine mb_try_completion

    subroutine mb_all_completions(string, point, all, base_size)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: point
        type(value_type), intent(out) :: all
        integer(int64), intent(out) :: base_size
        if (associated(minibuffer_completion_predicate)) then
            call completion_all_completions(string, minibuffer_completion_table, point, all, base_size, &
                                            minibuffer_completion_predicate)
        else
            call completion_all_completions(string, minibuffer_completion_table, point, all, base_size)
        end if
    end subroutine mb_all_completions

    logical function mb_test(string)
        character(len=*), intent(in) :: string
        if (associated(minibuffer_completion_predicate)) then
            mb_test = test_completion(string, minibuffer_completion_table, minibuffer_completion_predicate)
        else
            mb_test = test_completion(string, minibuffer_completion_table)
        end if
    end function mb_test

    subroutine mb_try(string, result, status)
        character(len=*), intent(in) :: string
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        if (associated(minibuffer_completion_predicate)) then
            call try_completion(string, minibuffer_completion_table, result, status, minibuffer_completion_predicate)
        else
            call try_completion(string, minibuffer_completion_table, result, status)
        end if
    end subroutine mb_try

    ! ------------------------------------------------------------------
    ! Completion commands
    ! ------------------------------------------------------------------

    !> Temporarily display MESSAGE at the end of minibuffer text.
    !> This function is designed to be called from the minibuffer, i.e.,
    !> when Emacs prompts the user for some input, and the user types
    !> into the minibuffer.  If called when the current buffer is not
    !> the minibuffer, this function just calls `message', and thus
    !> displays MESSAGE in the echo-area.
    !> When called from the minibuffer, this function displays MESSAGE
    !> at the end of minibuffer text for `minibuffer_message_timeout'
    !> seconds, or until the next input event arrives, whichever comes first.
    !> It encloses MESSAGE in [...] if it is not yet enclosed.
    !> The intent is to show the message without hiding what the user typed.
    !> Port of minibuffer.el:minibuffer-message (no ARGS, text properties
    !> or `minibuffer_message' property; the overlay is
    !> minibuffer_message_text).
    subroutine minibuffer_message(msg)
        character(len=*), intent(in) :: msg
        integer :: k
        type(status_type) :: st
        if (.not. minibufferp()) then
            call message(msg)
            return
        end if
        ! Clear out any old echo-area message to make way for our new
        ! thing.
        call delete_message_overlay()
        call message("")
        minibuffer_message_text = " ["//msg//"]"
        k = verify(msg, " ")
        if (k > 0 .and. len(msg) >= 2) then
            if (msg(k:k) == "[" .and. msg(len(msg):) == "]" .and. len(msg) - k >= 2) minibuffer_message_text = msg
        end if
        last_minibuffer_message = minibuffer_message_text
        call run_at_time(minibuffer_message_timer, real(minibuffer_message_timeout, real64), &
                         delete_message_overlay_timer, st)
    end subroutine minibuffer_message

    !> Remove the text `minibuffer_message' shows.  Port of
    !> minibuffer.el:minibuffer--delete-message-overlay.
    subroutine delete_message_overlay()
        if (timerp(minibuffer_message_timer)) call free_timer(minibuffer_message_timer)
        if (allocated(minibuffer_message_text)) deallocate (minibuffer_message_text)
    end subroutine delete_message_overlay

    !> `delete_message_overlay' as a timer function.
    subroutine delete_message_overlay_timer(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call delete_message_overlay()
    end subroutine delete_message_overlay_timer

    !> Port of minibuffer.el:completion--message.
    subroutine completion_message(msg)
        character(len=*), intent(in) :: msg
        if (completion_show_inline_help) call minibuffer_message(msg)
    end subroutine completion_message

    !> Port of minibuffer.el:completion--done (no :exit-function).
    subroutine completion_done(msg)
        character(len=*), intent(in), optional :: msg
        if (present(msg)) call completion_message(msg)
    end subroutine completion_done

    !> Replace the buffer text between BEG and END with NEWTEXT.
    !> Moves point to the end of the new text.  Port of
    !> minibuffer.el:completion--replace.
    subroutine completion_replace(beg, end, newtext)
        integer(int64), intent(in) :: beg, end
        character(len=*), intent(in) :: newtext
        type(status_type) :: status
        call goto_char(beg)
        call del_range(beg, end, status)
        call insert(newtext, status)
        call goto_char(beg + schars(newtext))
    end subroutine completion_replace

    !> Return true if POS is in the current completion boundaries.
    !> Port of minibuffer.el:completion--in-boundaries-p.
    logical function in_boundaries_p(string, suffix, pos)
        character(len=*), intent(in) :: string, suffix
        integer(int64), intent(in) :: pos
        integer(int64) :: bstart, bend
        call mb_boundaries(string, suffix, bstart, bend)
        in_boundaries_p = bstart >= pos .and. pos >= schars(string) + bend
    end function in_boundaries_p

    !> Do the completion and return a summary of what happened.
    !> M = completion was performed, the text was Modified.
    !> C = there were available Completions.
    !> E = after completion we now have an Exact match.
    !>
    !>  MCE
    !>  000  0 no possible completion
    !>  001  1 was already an exact and unique completion
    !>  010  2 no completion happened
    !>  011  3 was already an exact completion
    !>  100  4 ??? impossible
    !>  101  5 ??? impossible
    !>  110  6 some completion happened
    !>  111  7 completed to an exact completion
    !>
    !> WORD uses `completion__try_word_completion' in place of
    !> `completion_try_completion'.  EXPECT-EXACT, if non-nil, means that
    !> there is no need to tell the user when the buffer's text is already
    !> an exact match.  Port of minibuffer.el:completion--do-completion
    !> (no cycling).
    recursive integer function completion_do_completion(beg, end, word, expect_exact) result(bits)
        integer(int64), intent(in) :: beg, end
        logical, intent(in) :: word, expect_exact
        character(len=:), allocatable :: string
        type(try_result_type) :: comp
        type(status_type) :: status
        integer(int64) :: pos, e, comp_pos
        logical :: completed, unchanged, only_changed_boundaries, exact
        call buffer_substring(beg, end, string, status)
        pos = point() - beg
        if (word) then
            call try_word_completion(string, pos, comp)
        else
            call mb_try_completion(string, pos, comp)
        end if
        select case (comp%kind)
        case (TRY_NONE)
            call minibuffer_hide_completions()
            if (.not. completion_fail_discreetly) then
                call ding()
                call completion_message("No match")
            end if
            bits = 0
        case (TRY_EXACT)
            call minibuffer_hide_completions()
            call goto_char(end)
            if (expect_exact) then
                call completion_done()
            else
                call completion_done("Sole completion")
            end if
            ! Exact and unique match.
            bits = 1
        case default
            ! `completed' should be t if some completion was done, which
            ! doesn't include simply changing the case of the entered
            ! string.  However, for appearance, the string is rewritten if
            ! the case changes.
            comp_pos = comp%point
            completed = compare_strings(comp%string, string, .true.) /= COMPARE_EQUAL
            unchanged = comp%string == string .and. len(comp%string) == len(string)
            only_changed_boundaries = .false.
            if (.not. completed .and. comp_pos /= pos) then
                only_changed_boundaries = .not. in_boundaries_p(substring(string, 0_int64, pos), &
                                                                substring(string, pos), comp_pos)
            end if
            e = end
            if (unchanged) then
                call goto_char(e)
            else
                ! Insert in minibuffer the chars we got.
                call completion_replace(beg, e, comp%string)
                e = beg + schars(comp%string)
            end if
            ! Move point to its completion-mandated destination.
            call goto_char(point() + comp_pos - schars(comp%string))
            if (.not. (unchanged .or. completed)) then
                ! The case of the string changed, but that's all.  We're
                ! not sure whether this is a unique completion or not, so
                ! try again using the real case (this shouldn't recurse
                ! again, because the next time try-completion will return
                ! either t or the exact string).
                bits = completion_do_completion(beg, e, word, expect_exact)
                return
            end if
            ! It did find a match.  Do we match some possibility exactly
            ! now?
            exact = mb_test(comp%string)
            if (completed .or. only_changed_boundaries) then
                call minibuffer_hide_completions()
                ! If completion did not put point at end of field, it's a
                ! sign that completion is not finished.
                if (exact) call completion_done()
            else if (.not. exact) then
                ! Show the completion table, if requested.
                if (completion_auto_help) then
                    call minibuffer_completion_help(beg, e)
                else
                    call completion_message("Next char not unique")
                end if
            else
                ! If the last exact completion and this one were the same,
                ! it means we've already given a "Complete, but not
                ! unique" message and the user's hit TAB again, so now we
                ! give him help.
                if (value_eq(this_command, last_command) .and. completion_auto_help) then
                    call minibuffer_completion_help(beg, e)
                end if
                if (expect_exact) then
                    call completion_done()
                else
                    call completion_done("Complete, but not unique")
                end if
            end if
            bits = 2
            if (completed) bits = bits + 4
            if (exact) bits = bits + 1
        end select
    end function completion_do_completion

    !> Whether `c' has word syntax (ASCII letters and digits, and non-ASCII
    !> characters: the standard syntax table's main word characters).
    elemental logical function word_char_p(c)
        integer(int32), intent(in) :: c
        word_char_p = (c >= iachar("a") .and. c <= iachar("z")) .or. (c >= iachar("A") .and. c <= iachar("Z")) &
                      .or. (c >= iachar("0") .and. c <= iachar("9")) .or. c >= 128
    end function word_char_p

    !> Port of minibuffer.el:completion--try-word-completion (the cut
    !> after the first word only when there is no text after point).
    subroutine try_word_completion(string, point, comp)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: point
        type(try_result_type), intent(out) :: comp
        type(try_result_type) :: tem
        character(len=:), allocatable :: before, after, completion
        character(len=1), parameter :: exts(2) = [" ", "-"]
        integer(int64) :: comppoint, nbefore, ncomp, j, mb1, k, cutpos
        integer(int32) :: c, clen
        integer(int64) :: byte
        integer :: e
        call mb_try_completion(string, point, comp)
        if (comp%kind /= TRY_STRING) return
        before = substring(string, 0_int64, point)
        after = substring(string, point)
        ! If completion finds next char not unique, consider adding a space
        ! or a hyphen.  If both " " and "-" lead to completions, prefer " "
        ! so SPC behaves a bit more like a self-inserting key (bug#17375).
        if (schars(string) == schars(comp%string)) then
            do e = 1, size(exts)
                call mb_try_completion(before//exts(e)//after, point + 1, tem)
                if (tem%kind == TRY_STRING) then
                    comp = tem
                    exit
                end if
            end do
        end if
        ! Completing a single word is actually more difficult than
        ! completing as much as possible, because we first have to find the
        ! "current position" in `completion' in order to find the end of
        ! the word we're completing.  Be as conservative as possible: only
        ! cut after the first word, if we're reasonably sure that our guess
        ! is correct.
        completion = comp%string
        comppoint = comp%point
        nbefore = schars(before)
        ncomp = schars(completion)
        if (len(after) > 0 .or. nbefore == 0) return
        if (comppoint /= ncomp) return
        ! The last occurrence of the text before point in the completion.
        j = -1
        do k = ncomp - nbefore, 0, -1
            if (substring(completion, k, k + nbefore) == before) then
                j = k
                exit
            end if
        end do
        if (j < 0) return
        mb1 = j + nbefore
        ! The first non-word character from there: cut after it.
        cutpos = -1
        byte = len(substring(completion, 0_int64, mb1)) + 1
        k = mb1
        do while (byte <= len(completion))
            call string_char_and_length(completion, byte, c, clen)
            if (.not. word_char_p(c)) then
                cutpos = k + 1
                exit
            end if
            byte = byte + clen
            k = k + 1
        end do
        ! Is there really something to cut?
        if (cutpos < 0 .or. comppoint <= cutpos) return
        comp%string = substring(completion, 0_int64, cutpos)//substring(completion, comppoint)
        comp%point = cutpos
    end subroutine try_word_completion

    !> Where completion starts in the minibuffer; an error in the prompt.
    !> Port of minibuffer.el:minibuffer--completion-prompt-end.
    integer(int64) function completion_prompt_end(status) result(pos)
        type(status_type), intent(out) :: status
        status = status_type()
        pos = minibuffer_prompt_end()
        if (point() < pos) call set_error(status, FM_USER_ERROR, format_message("Can't complete in prompt"))
    end function completion_prompt_end

    !> Complete the minibuffer contents as far as possible.
    !> Return false if there is no valid completion, else true.  If no
    !> characters can be completed, display a list of possible
    !> completions.  If you repeat this command after it displayed such a
    !> list, scroll the window of possible completions.  Port of
    !> minibuffer.el:minibuffer-complete (completion-in-region and
    !> completion--in-region-1).
    subroutine minibuffer_complete(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg
        integer :: bits
        beg = completion_prompt_end(status)
        if (.not. status%ok()) return
        ! If the previous command was not this, mark the completion buffer
        ! obsolete.
        this_command = sym("completion_at_point")
        if (.not. value_eq(last_command, sym("completion_at_point"))) minibuffer_scroll_window = null_handle()
        ! If there's a fresh completion window with a live buffer, and this
        ! command is repeated, scroll that window.
        if (window_live_p(minibuffer_scroll_window)) then
            call scroll_completions(minibuffer_scroll_window)
            return
        end if
        bits = completion_do_completion(beg, point_max(), .false., .false.)
    end subroutine minibuffer_complete

    !> Complete the minibuffer contents at most a single word.
    !> After one word is completed as much as possible, a space or hyphen
    !> is added, provided that matches some possible completion.  Return
    !> false if there is no valid completion, else true.  Port of
    !> minibuffer.el:minibuffer-complete-word.
    subroutine minibuffer_complete_word(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beg
        integer :: bits
        beg = completion_prompt_end(status)
        if (.not. status%ok()) return
        bits = completion_do_completion(beg, point_max(), .true., .false.)
    end subroutine minibuffer_complete_word

    !> Exit if the minibuffer contains a valid completion.
    !> Otherwise, try to complete the minibuffer contents.  If completion
    !> leads to a valid completion, a repetition of this command will
    !> exit.
    !>
    !> If `minibuffer_completion_confirm' is `confirm', do not try to
    !> complete; instead, ask for confirmation and accept any input if
    !> confirmed.  If `minibuffer_completion_confirm' is
    !> `confirm_after_completion', do not try to complete; instead, ask
    !> for confirmation if the preceding minibuffer command was a member
    !> of `minibuffer_confirm_exit_commands', and accept the input
    !> otherwise.  Port of minibuffer.el:minibuffer-complete-and-exit
    !> (with completion-complete-and-exit and
    !> completion--complete-and-exit).
    subroutine minibuffer_complete_and_exit(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: string
        type(value_type) :: compl
        integer(int64) :: beg, end
        integer :: bits
        beg = completion_prompt_end(status)
        if (.not. status%ok()) return
        end = point_max()
        ! Allow user to specify null string.
        if (beg == end) then
            call exit_minibuffer(status)
            return
        end if
        call buffer_substring(beg, end, string, status)
        ! See if we have a completion from the table.
        if (mb_test(string)) then
            if (completion_ignore_case%value) then
                ! Fixup case of the field, if necessary.
                call mb_try(string, compl, status)
                if (stringp(compl)) then
                    if (xstring(compl) /= string .and. schars(xstring(compl)) == schars(string)) then
                        call completion_replace(beg, end, xstring(compl))
                    end if
                end if
            end if
            call exit_minibuffer(status)
            return
        end if
        ! The user is permitted to exit with an input that's rejected by
        ! test-completion, after confirming her choice.
        if (minibuffer_completion_confirm == REQUIRE_MATCH_CONFIRM .or. &
            minibuffer_completion_confirm == REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION) then
            if (value_eq(last_command, this_command) .or. &
                (minibuffer_completion_confirm == REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION .and. &
                 .not. confirm_exit_command(last_command))) then
                call exit_minibuffer(status)
            else
                call minibuffer_message("Confirm")
            end if
            return
        end if
        ! Call do-completion, but ignore errors.
        bits = completion_do_completion(beg, end, .false., .true.)
        select case (bits)
        case (1, 3)
            call exit_minibuffer(status)
        case (7)
            if (minibuffer_completion_confirm == REQUIRE_MATCH_NIL) then
                call exit_minibuffer(status)
            else
                call minibuffer_message("Confirm")
            end if
        case default
            ! Not a match: stay in the minibuffer.
        end select
    end subroutine minibuffer_complete_and_exit

    !> Whether `cmd' is in minibuffer-confirm-exit-commands.
    logical function confirm_exit_command(cmd)
        type(value_type), intent(in) :: cmd
        confirm_exit_command = value_eq(cmd, sym("completion_at_point")) .or. &
                               value_eq(cmd, sym("minibuffer_complete")) .or. &
                               value_eq(cmd, sym("minibuffer_complete_word"))
    end function confirm_exit_command

    ! ------------------------------------------------------------------
    ! The *Completions* buffer
    ! ------------------------------------------------------------------

    !> The window showing *Completions*, or the null handle.  Port of
    !> minibuffer.el:minibuffer--completions-visible.
    function completions_visible() result(window)
        type(handle_type) :: window
        type(handle_type) :: buf
        window = null_handle()
        buf = get_buffer("*Completions*")
        if (buf%index /= 0) window = get_buffer_window(buf)
    end function completions_visible

    !> Scroll the window of possible completions: to the top when its end
    !> is visible, else down a screen.  The scrolling part of
    !> completion--in-region-1.
    subroutine scroll_completions(window)
        type(handle_type), intent(in) :: window
        type(status_type) :: status
        type(handle_type) :: old
        integer(int64) :: zv, begv
        old = current_buffer()
        call set_buffer(windows(window%index)%buffer, status)
        if (.not. status%ok()) return
        zv = buffers(cur)%zv
        begv = buffers(cur)%begv
        call set_buffer(old, status)
        if (pos_visible_in_window_p(zv, window)) then
            ! If end is in view, scroll up to the end.
            call set_window_start(windows(window%index), begv, status)
        else
            ! Else scroll down one screen.
            call scroll_window(window, 1, status)
        end if
    end subroutine scroll_completions

    !> Display a list of possible completions of the current minibuffer
    !> contents.  The list goes in buffer *Completions*, shown in the
    !> frame's root window until the minibuffer is done (there is no
    !> window splitting yet).  Port of
    !> minibuffer.el:minibuffer-completion-help (completions-sort
    !> alphabetical, no annotations or groups).
    recursive subroutine minibuffer_completion_help(start, end)
        integer(int64), intent(in), optional :: start, end
        type(value_type) :: completions
        type(value_type), allocatable :: elts(:)
        type(string_box), allocatable :: strings(:)
        type(handle_type) :: mini, buf, window
        type(status_type) :: status
        type(display_alist_type) :: alist
        character(len=:), allocatable :: string
        integer(int64) :: s, e, base_size
        integer :: k, saved_mark
        call message("Making completion list...")
        s = minibuffer_prompt_end()
        if (present(start)) s = start
        e = point_max()
        if (present(end)) e = end
        call buffer_substring(s, e, string, status)
        call mb_all_completions(string, point() - s, completions, base_size)
        call message("")
        call list_elements(completions, elts)
        if (size(elts) == 0) then
            ! If there are no completions, or if the current input is
            ! already the sole completion, then hide (previous&stale)
            ! completions.
            call minibuffer_hide_completions()
            if (.not. completion_fail_discreetly) then
                call ding()
                call completion_message("No match")
            end if
            return
        end if
        if (size(elts) == 1) then
            if (xstring(elts(1)) == string .and. len(xstring(elts(1))) == len(string)) then
                call minibuffer_hide_completions()
                call completion_message("Sole completion")
                return
            end if
        end if
        allocate (strings(size(elts)))
        do k = 1, size(elts)
            strings(k)%s = xstring(elts(k))
        end do
        ! minibuffer-sort-alphabetically.
        call sort_strings(strings)
        mini = current_buffer()
        call get_buffer_create("*Completions*", buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        call set_buffer_read_only(buf, .false., status)
        call erase_buffer(status)
        call display_completion_list(strings)
        call set_buffer(mini, status)
        ! If the *Completions* buffer is shown in a new window, mark it
        ! as softly-dedicated, so bury-buffer in
        ! minibuffer-hide-completions will know whether to delete the
        ! window or not.
        saved_mark = display_buffer_mark_dedicated
        display_buffer_mark_dedicated = DEDICATED_WEAK
        ! A copy of `display_buffer_fallback_action' where
        ! `display_buffer_use_some_window' is replaced with
        ! `display_buffer_at_bottom' (with-current-buffer-window).
        alist%window_height => completions_fit_window_to_buffer
        if (selected_frame%minibuffer_selected) then
            call temp_buffer_window_show(buf, window, status, [DISPLAY_ACTION_MAYBE_SAME_WINDOW, &
                                                               DISPLAY_ACTION_REUSE_WINDOW, DISPLAY_ACTION_AT_BOTTOM], alist)
        else
            call temp_buffer_window_show(buf, window, status, [DISPLAY_ACTION_MAYBE_SAME_WINDOW, &
                                                               DISPLAY_ACTION_REUSE_WINDOW], alist)
        end if
        display_buffer_mark_dedicated = saved_mark
    end subroutine minibuffer_completion_help

    !> Resize *Completions* buffer window.  Port of
    !> minibuffer.el:completions--fit-window-to-buffer
    !> (`temp_buffer_resize_mode' off).
    subroutine completions_fit_window_to_buffer(window)
        type(handle_type), intent(in) :: window
        if (completions_max_height > 0) then
            call fit_window_to_buffer(window, completions_max_height)
        else
            call fit_window_to_buffer(window)
        end if
    end subroutine completions_fit_window_to_buffer

    !> Sort `strings' as `string_lessp' does.
    subroutine sort_strings(strings)
        type(string_box), intent(inout) :: strings(:)
        character(len=:), allocatable :: tmp
        integer :: i, j
        do i = 2, size(strings)
            tmp = strings(i)%s
            j = i - 1
            do while (j >= 1)
                if (.not. string_lessp(make_string(tmp), make_string(strings(j)%s))) exit
                strings(j + 1)%s = strings(j)%s
                j = j - 1
            end do
            strings(j + 1)%s = tmp
        end do
    end subroutine sort_strings

    !> Display the list of completions, STRINGS, in the current buffer:
    !> the heading line, then the candidates in columns.  Port of
    !> minibuffer.el:display-completion-list with completion--insert-strings
    !> and completion--insert-horizontal (completions-format horizontal;
    !> spaces in place of the `display' alignment, no truncation).
    subroutine display_completion_list(strings)
        type(string_box), intent(in) :: strings(:)
        type(status_type) :: status
        character(len=20) :: count
        integer(int64) :: wwidth
        type(window_type), pointer :: sw
        call goto_char(point_max())
        ! completions-header-format
        write (count, "(i0)") size(strings)
        call insert(trim(count)//" possible completions:"//achar(10), status)
        if (size(strings) == 0) return
        wwidth = 79
        sw => frame_selected_window_ref()
        if (sw%total_cols > 0) wwidth = sw%total_cols - 1
        call insert_strings_horizontal(strings, wwidth)
    end subroutine display_completion_list

    !> Insert STRINGS into the current buffer in columns: the window
    !> showing the buffer gives the width (79 columns if none).  Port of
    !> minibuffer.el:completion--insert-strings with
    !> completion--insert-horizontal (no GROUP-FUN; spaces in place of the
    !> `display' alignment, no truncation).
    subroutine completion_insert_strings(strings)
        type(minibuffer_string_type), intent(in) :: strings(:)
        type(string_box), allocatable :: boxes(:)
        type(handle_type) :: window
        integer(int64) :: wwidth
        integer :: k
        if (size(strings) == 0) return
        allocate (boxes(size(strings)))
        do k = 1, size(strings)
            boxes(k)%s = strings(k)%s
        end do
        wwidth = 79
        window = get_buffer_window(current_buffer())
        if (window_live_p(window)) wwidth = windows(window%index)%total_cols - 1
        call insert_strings_horizontal(boxes, wwidth)
    end subroutine completion_insert_strings

    !> The candidates STRINGS in columns of a window WWIDTH columns wide.
    !> Port of minibuffer.el:completion--insert-strings and
    !> completion--insert-horizontal (see above).
    subroutine insert_strings_horizontal(strings, wwidth)
        type(string_box), intent(in) :: strings(:)
        integer(int64), intent(in) :: wwidth
        type(status_type) :: status
        character(len=:), allocatable :: last_string
        integer(int64) :: length, columns, colwidth, column, cur_col, width
        integer :: k
        logical :: first
        length = 0
        do k = 1, size(strings)
            length = max(length, schars(strings(k)%s))
        end do
        ! At least 2 spaces between columns; don't allocate more columns
        ! than we can fill.
        columns = min(max(1_int64, wwidth/(2 + length)), max(1_int64, int(size(strings), int64)/2))
        colwidth = wwidth/columns
        column = 0
        cur_col = 0
        first = .true.
        do k = 1, size(strings)
            ! Remove (consecutive) duplicates.
            if (.not. first) then
                if (strings(k)%s == last_string .and. len(strings(k)%s) == len(last_string)) cycle
            end if
            last_string = strings(k)%s
            width = schars(strings(k)%s)
            if (.not. first) then
                if (wwidth < column + max(colwidth, width)) then
                    ! No space for `str' at point, move to next line.
                    call insert(achar(10), status)
                    column = 0
                    cur_col = 0
                else
                    ! A space, then alignment to the column.
                    call insert(" "//repeat(" ", int(max(0_int64, column - cur_col - 1))), status)
                    cur_col = max(cur_col + 1, column)
                end if
            end if
            first = .false.
            call insert(strings(k)%s, status)
            cur_col = cur_col + width
            ! Next column to align to.
            column = column + colwidth*((length + colwidth - 1)/colwidth)
        end do
    end subroutine insert_strings_horizontal

    !> Get rid of an out-of-date *Completions* buffer: bury it in its
    !> window, which is deleted if it was made for it (dedicated), or
    !> else shows the buffer it showed before.  Port of
    !> minibuffer.el:minibuffer-hide-completions (with-selected-window and
    !> bury-buffer).
    subroutine minibuffer_hide_completions()
        type(status_type) :: status
        type(handle_type) :: window, buffer, b
        window = completions_visible()
        if (.not. window_live_p(window)) return
        buffer = windows(window%index)%buffer
        call bury_buffer_internal(buffer)
        ! window--delete with DEDICATED-ONLY.
        if (windows(window%index)%dedicated /= DEDICATED_NONE .and. &
            .not. same_window(window, selected_frame%root_window)) then
            call delete_window(window, status)
            if (status%ok()) return
        end if
        ! Switch to another buffer in window.
        windows(window%index)%dedicated = DEDICATED_NONE
        b = switch_to_prev_buffer(window, BK_BURY, status)
    end subroutine minibuffer_hide_completions

    ! ------------------------------------------------------------------
    ! Reading with completion
    ! ------------------------------------------------------------------

    !> Read the name of a buffer and return it as a string.
    !> Prompt with PROMPT, which should be a string ending with a colon and a space.
    !> Provides completion on buffer names the user types.
    !> Optional second arg DEF is value to return if user enters an empty line,
    !>  instead of that empty string; the prompt shows it.
    !> Optional third arg REQUIRE_MATCH has the same meaning as the
    !>  REQUIRE_MATCH argument of `completing_read'.
    !> If `read_buffer_completion_ignore_case' is true, completion ignores
    !> case while reading the buffer name.
    !> The buffer `except' (if given) is left out of the completions.  Port
    !> of minibuf.c:Fread_buffer (`except' as internal-complete-buffer-except;
    !> read-buffer-to-switch puts that table in the minibuffer; no
    !> PREDICATE or read-buffer-function).
    recursive subroutine read_buffer(prompt, result, status, def, require_match, except)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: def
        integer, intent(in), optional :: require_match
        type(handle_type), intent(in), optional :: except
        type(completion_table_type) :: table
        character(len=:), allocatable :: p, saved_except
        logical :: saved_ignore_case, had_except
        integer :: match
        match = REQUIRE_MATCH_NIL
        if (present(require_match)) match = require_match
        had_except = allocated(rbts_except)
        if (had_except) call move_alloc(rbts_except, saved_except)
        table = completion_function_table(internal_complete_buffer)
        if (present(except)) then
            rbts_except = buffers(except%index)%name
            table = completion_function_table(internal_complete_buffer_except)
        end if
        saved_ignore_case = completion_ignore_case%value
        completion_ignore_case = read_buffer_completion_ignore_case
        p = prompt
        if (present(def)) then
            ! A default value was provided: we must change PROMPT, editing
            ! the default value in before the colon (format-prompt with
            ! minibuffer-default-prompt-format " (default %s)").
            if (len(p) >= 2) then
                if (p(len(p) - 1:) == ": ") then
                    p = p(1:len(p) - 2)
                else if (p(len(p):) == ":" .or. p(len(p):) == " ") then
                    p = p(1:len(p) - 1)
                end if
            else if (len(p) == 1) then
                if (p == ":" .or. p == " ") p = ""
            end if
            if (len(def) > 0) then
                p = p//" (default "//def//"): "
            else
                p = p//": "
            end if
            call completing_read(p, table, result, status, require_match=match, history="buffer_name_history", &
                                 default=def)
        else
            call completing_read(p, table, result, status, require_match=match, history="buffer_name_history")
        end if
        completion_ignore_case = saved_ignore_case
        if (allocated(rbts_except)) deallocate (rbts_except)
        if (had_except) call move_alloc(saved_except, rbts_except)
    end subroutine read_buffer

    !> The names of all buffers, in the order of `buffer_list'
    !> (Vbuffer_alist).
    function buffer_names() result(names)
        type(value_type) :: names
        type(handle_type), allocatable :: all(:)
        type(value_type), allocatable :: elts(:)
        integer :: k
        all = buffer_list()
        allocate (elts(size(all)))
        ! Most recently selected first.
        do k = 1, size(all)
            elts(k) = make_string(buffers(all(size(all) + 1 - k)%index)%name)
        end do
        names = make_list(elts)
    end function buffer_names

    !> Perform completion on buffer names.
    !> STRING and PREDICATE have the same meanings as in `try_completion',
    !> `all_completions', and `test_completion'.
    !>
    !> If FLAG is COMPLETE_TRY, invoke `try_completion'; if it is
    !> COMPLETE_ALL, invoke `all_completions'; otherwise invoke
    !> `test_completion'.  Port of minibuf.c:Finternal_complete_buffer.
    recursive subroutine internal_complete_buffer(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        type(completion_table_type) :: alist
        type(value_type), allocatable :: res(:), bufs(:)
        integer :: k, n, total
        status = status_type()
        result = nil()
        alist = completion_list_table(buffer_names())
        select case (action)
        case (COMPLETE_TRY)
            call try_completion(string, alist, result, status, pred)
        case (COMPLETE_ALL)
            call all_completions(string, alist, result, status, pred)
            if (len(string) > 0) return
            ! Strip out internal buffers.
            call list_elements(result, res)
            allocate (bufs(size(res)))
            n = 0
            do k = 1, size(res)
                if (internal_name(res(k))) cycle
                n = n + 1
                bufs(n) = res(k)
            end do
            if (n == 0) then
                ! If all bufs are internal don't strip them out.
                total = int(list_length(buffer_names()))
                if (size(res) /= total) result = nil()
                return
            end if
            result = make_list(bufs(1:n))
        case (COMPLETE_TEST)
            if (test_completion(string, alist, pred)) result = t_value()
        case default
            continue
        end select
        if (present(suffix)) continue
    contains

        logical function internal_name(v)
            type(value_type), intent(in) :: v
            character(len=:), allocatable :: s
            s = xstring(v)
            internal_name = .false.
            if (len(s) > 0) internal_name = s(1:1) == " "
        end function internal_name
    end subroutine internal_complete_buffer

    !> Perform completion on all buffers excluding BUFFER (`rbts_except'):
    !> internal-complete-buffer through completion-table-with-predicate
    !> (not strict).  Port of
    !> minibuffer.el:internal-complete-buffer-except.
    recursive subroutine internal_complete_buffer_except(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        procedure(completion_predicate), pointer :: saved_pred2
        status = status_type()
        result = nil()
        select case (action)
        case (COMPLETE_TEST)
            ! Ignore pred1 since it doesn't really have to apply anyway.
            call internal_complete_buffer(string, action, result, status, pred)
        case (COMPLETE_TRY, COMPLETE_ALL)
            saved_pred2 => rbts_pred2
            rbts_pred2 => null()
            if (present(pred)) rbts_pred2 => pred
            call internal_complete_buffer(string, action, result, status, not_except)
            rbts_pred2 => saved_pred2
            ! If completion failed and we're not applying pred1 strictly,
            ! try again without pred1.
            if (nilp(result)) call internal_complete_buffer(string, action, result, status, pred)
        case default
            if (present(suffix)) continue
        end select
    end subroutine internal_complete_buffer_except

    !> internal-complete-buffer-except's predicate: the buffer is not
    !> `rbts_except' (and PRED2 accepts it).
    recursive logical function not_except(elt)
        type(value_type), intent(in) :: elt
        not_except = .true.
        if (stringp(elt) .and. allocated(rbts_except)) then
            not_except = .not. (xstring(elt) == rbts_except .and. len(xstring(elt)) == len(rbts_except))
        end if
        if (not_except .and. associated(rbts_pred2)) not_except = rbts_pred2(elt)
    end function not_except

    !> Read a string in the minibuffer, with completion.
    !> PROMPT is a string to prompt with; normally it ends in a colon and
    !> a space.  COLLECTION can be a list of strings, an alist, the
    !> obarray or a function (see `try_completion').  PREDICATE limits
    !> completion to a subset of COLLECTION.
    !>
    !> REQUIRE_MATCH can take the following values:
    !> - REQUIRE_MATCH_T means that the user is not allowed to exit unless
    !>   the input is (or completes to) an element of COLLECTION or is
    !>   null.
    !> - REQUIRE_MATCH_NIL means that the user can exit with any input.
    !> - REQUIRE_MATCH_CONFIRM means that the user can exit with any
    !>   input, but she needs to confirm her choice if the input is not an
    !>   element of COLLECTION.
    !> - REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION means that the user can
    !>   exit with any input, but she needs to confirm her choice if she
    !>   called `minibuffer_complete' right before
    !>   `minibuffer_complete_and_exit' and the input is not an element of
    !>   COLLECTION.
    !> - REQUIRE_MATCH_OTHER means that the user can exit only if the
    !>   input is (or completes to) an element of COLLECTION or is null,
    !>   but typing RET does not exit if it does non-null completion.
    !>
    !> If INITIAL-INPUT is present, insert it in the minibuffer initially.
    !> HISTORY names the history list (default `minibuffer_history').
    !> DEFAULT is the value for empty input, and for M-n.  Port of
    !> minibuf.c:Fcompleting_read with
    !> minibuffer.el:completing-read-default.
    recursive subroutine completing_read(prompt, collection, result, status, predicate, require_match, &
                                         initial, history, default, used_default)
        character(len=*), intent(in) :: prompt
        type(completion_table_type), intent(in) :: collection
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        integer, intent(in), optional :: require_match
        character(len=*), intent(in), optional :: initial, history, default
        !> Whether the input was empty and the value is DEFAULT itself (as
        !> `eq' tells upstream).
        logical, intent(out), optional :: used_default
        type(completion_table_type) :: saved_table
        procedure(completion_predicate), pointer :: saved_pred
        type(handle_type) :: keymap
        integer :: saved_confirm, match
        if (present(used_default)) used_default = .false.
        match = REQUIRE_MATCH_NIL
        if (present(require_match)) match = require_match
        if (match /= REQUIRE_MATCH_NIL) then
            keymap = minibuffer_local_must_match_map
            if (minibuffer_completing_file_name) keymap = minibuffer_local_filename_must_match_map
        else
            keymap = minibuffer_local_completion_map
            if (minibuffer_completing_file_name) keymap = minibuffer_local_filename_completion_map
        end if
        ! The minibuffer's local values (minibuffer-with-setup-hook).
        saved_table = minibuffer_completion_table
        saved_pred => minibuffer_completion_predicate
        saved_confirm = minibuffer_completion_confirm
        minibuffer_completion_table = collection
        minibuffer_completion_predicate => null()
        if (present(predicate)) minibuffer_completion_predicate => predicate
        minibuffer_completion_confirm = match
        if (match == REQUIRE_MATCH_T) minibuffer_completion_confirm = REQUIRE_MATCH_NIL
        ! (setq-local completion-ignore-case c-i-c) runs first, then the
        ! functions of enclosing minibuffer-with-setup-hook forms.
        crd_ignore_case = completion_ignore_case%value
        crd_outer_setup => minibuffer_setup_function
        minibuffer_setup_function => completing_read_setup
        if (present(history)) then
            call read_from_minibuffer(prompt, result, status, initial, keymap, history, default)
        else
            call read_from_minibuffer(prompt, result, status, initial, keymap, default=default)
        end if
        minibuffer_setup_function => null()
        minibuffer_completion_table = saved_table
        minibuffer_completion_predicate => saved_pred
        minibuffer_completion_confirm = saved_confirm
        if (.not. status%ok()) return
        if (len(result) == 0 .and. present(default)) then
            result = default
            if (present(used_default)) used_default = .true.
        end if
    end subroutine completing_read

    !> completing-read-default's minibuffer-with-setup-hook function.
    subroutine completing_read_setup()
        procedure(minibuffer_setup_proc), pointer :: outer
        call completion_ignore_case%set_local(crd_ignore_case)
        outer => crd_outer_setup
        crd_outer_setup => null()
        if (associated(outer)) call outer()
    end subroutine completing_read_setup

    !> Completion table for file names.  Port of
    !> minibuffer.el:completion-file-name-table (no ~USER completion or
    !> file-name handlers).
    recursive subroutine completion_file_name_table(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        character(len=:), allocatable :: name, specdir, realdir, saved_dir
        type(value_type), allocatable :: all(:), comp(:)
        type(status_type) :: st
        integer :: slash, k, n
        logical :: had_dir
        status = status_type()
        result = nil()
        select case (action)
        case (COMPLETE_BOUNDARIES)
            slash = 0
            if (present(suffix)) slash = index(suffix, "/")
            if (slash > 0) then
                result = list(make_fixnum(min(schars(file_name_directory(string)), schars(string))), &
                              make_fixnum(schars(suffix(1:slash - 1))))
            else
                result = list(make_fixnum(min(schars(file_name_directory(string)), schars(string))), nil())
            end if
        case (COMPLETE_TEST)
            if (len(string) == 0) return
            if (present(pred)) then
                if (pred(make_string(string))) result = t_value()
            else
                if (file_exists_p(string)) result = t_value()
            end if
        case default
            name = file_name_nondirectory(string)
            specdir = file_name_directory(string)
            realdir = specdir
            if (len(specdir) == 0) then
                realdir = "./"
                if (allocated(buffers(cur)%directory)) realdir = buffers(cur)%directory
            end if
            if (action == COMPLETE_TRY) then
                call file_name_completion(name, realdir, result, st, pred)
                if (.not. st%ok()) result = nil()
                if (stringp(result)) result = make_string(specdir//xstring(result))
            else
                call file_name_all_completions(name, realdir, result, st)
                if (.not. st%ok()) result = nil()
                ! Check the predicate, if necessary.
                if (present(pred) .and. st%ok()) then
                    call list_elements(result, all)
                    allocate (comp(size(all)))
                    n = 0
                    had_dir = allocated(buffers(cur)%directory)
                    if (had_dir) saved_dir = buffers(cur)%directory
                    call expand_file_name(realdir, buffers(cur)%directory, st)
                    do k = 1, size(all)
                        if (.not. pred(all(k))) cycle
                        n = n + 1
                        comp(n) = all(k)
                    end do
                    if (had_dir) then
                        buffers(cur)%directory = saved_dir
                    else
                        deallocate (buffers(cur)%directory)
                    end if
                    result = make_list(comp(1:n))
                end if
            end if
        end select
    end subroutine completion_file_name_table

    !> `substitute_in_file_name' on `filename', with any part before a
    !> "/~" followed by a word character or `*' or `-' cut off first.
    !> Port of minibuffer.el:completion--sifn-regardless-of-system-users.
    function sifn_regardless_of_system_users(filename) result(name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: name
        integer :: k
        character :: c
        name = filename
        k = 1
        do while (k + 2 <= len(name))
            if (name(k:k + 1) == "/~") then
                c = name(k + 2:k + 2)
                if ((c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z") .or. (c >= "0" .and. c <= "9") &
                    .or. c == "_" .or. c == "*" .or. c == "-" .or. iachar(c) >= 128) then
                    name = name(k + 1:)
                    k = 1
                    cycle
                end if
            end if
            k = k + 1
        end do
        name = substitute_in_file_name(name)
    end function sifn_regardless_of_system_users

    !> Return completion boundaries on file name STRING: the start in
    !> STRING (`string_start'), and the end in SUFFIX (`bend', -1 for
    !> nil).  Runs `substitute_in_file_name' on STRING first, but returns
    !> completion boundaries for the original string.  Port of
    !> minibuffer.el:completion--sifn-boundaries.
    subroutine sifn_boundaries(string, suffix, string_start, bend, pred)
        character(len=*), intent(in) :: string, suffix
        integer(int64), intent(out) :: string_start, bend
        procedure(completion_predicate), optional :: pred
        character(len=:), allocatable :: sifned
        type(value_type) :: bounds
        type(status_type) :: st
        ! We want to compute the start boundary on the result of
        ! `substitute_in_file_name' (since that's what we use for actual
        ! completion), and then transform that into an offset in STRING
        ! instead.  We can't do this if we expand environment variables,
        ! so double the $s to prevent that.
        sifned = substitute_in_file_name(double_dollars(string))
        call completion_file_name_table(sifned, COMPLETE_BOUNDARIES, bounds, st, pred, suffix)
        ! Adjust SIFNED-START to be an offset in STRING instead of in
        ! SIFNED.
        string_start = xfixnum(list_elt(bounds, 1_int64)) - schars(sifned) + schars(string)
        bend = -1
        if (fixnump(list_elt(bounds, 2_int64))) bend = xfixnum(list_elt(bounds, 2_int64))
    end subroutine sifn_boundaries

    !> Internal subroutine for `read_file_name'.  Do not call this.
    !> This is a completion table for file names, like
    !> `completion_file_name_table' except that it passes the file name
    !> through `substitute_in_file_name'.  Port of
    !> minibuffer.el:completion--file-name-table (read-file-name-internal;
    !> completion--embedded-envvar-table, completing environment variable
    !> names, is not ported).
    recursive subroutine read_file_name_internal(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        character(len=:), allocatable :: sifned, orig_in_bounds, sifned_in_bounds, compl
        type(value_type), allocatable :: elts(:)
        type(value_type) :: bounds
        integer(int64) :: orig_start, sifned_start, bend
        logical :: only_need_double_dollars
        integer :: k
        status = status_type()
        result = nil()
        if (action == COMPLETE_BOUNDARIES) then
            if (present(suffix)) then
                call sifn_boundaries(string, suffix, orig_start, bend, pred)
            else
                call sifn_boundaries(string, "", orig_start, bend, pred)
            end if
            if (bend >= 0) then
                result = list(make_fixnum(orig_start), make_fixnum(bend))
            else
                result = list(make_fixnum(orig_start), nil())
            end if
            return
        end if
        if (action == COMPLETE_TEST) then
            sifned = substitute_in_file_name(string)
        else
            sifned = sifn_regardless_of_system_users(string)
        end if
        call sifn_boundaries(string, "", orig_start, bend, pred)
        call completion_file_name_table(sifned, COMPLETE_BOUNDARIES, bounds, status, pred, "")
        sifned_start = xfixnum(list_elt(bounds, 1_int64))
        orig_in_bounds = substring(string, orig_start)
        sifned_in_bounds = substring(sifned, sifned_start)
        ! If true, sifn only un-doubled $s in ORIG, so we can fix a
        ! completion to match ORIG by just doubling $s again.  This
        ! preserves more text from the completion, behaving better with
        ! non-nil `completion_ignore_case'.
        only_need_double_dollars = orig_in_bounds == double_dollars(sifned_in_bounds) .and. &
                                   len(orig_in_bounds) == len(double_dollars(sifned_in_bounds))
        call completion_file_name_table(sifned, action, result, status, pred)
        select case (action)
        case (COMPLETE_TRY)
            if (stringp(result)) then
                if (only_need_double_dollars) then
                    ! Extract the newly added text, quote any dollar signs,
                    ! and append it to ORIG.
                    result = make_string(substring(string, 0_int64, orig_start)// &
                                         double_dollars(substring(xstring(result), sifned_start)))
                else
                    result = make_string(string//double_dollars(substring(xstring(result), schars(sifned))))
                end if
            end if
        case (COMPLETE_ALL)
            call list_elements(result, elts)
            do k = 1, size(elts)
                if (only_need_double_dollars) then
                    elts(k) = make_string(double_dollars(xstring(elts(k))))
                else
                    ! Extract the newly added text, quote any dollar signs,
                    ! and append it to the part of ORIG inside the
                    ! completion boundaries.
                    compl = xstring(elts(k))
                    elts(k) = make_string(orig_in_bounds//double_dollars(substring(compl, schars(sifned_in_bounds))))
                end if
            end do
            result = make_list(elts)
        case default
            continue
        end select
    end subroutine read_file_name_internal
end module minibuffer_fm
