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

!> Leaving the minibuffer, its basic keys, `read-file-name', and
!> completion: `completing-read', the completion styles basic and emacs22,
!> TAB, SPC, ? and RET in the completion maps, `minibuffer-message', and
!> the *Completions* list (shown in the root window while the minibuffer
!> is active, as there is no window splitting yet).
!>
!> Not ported yet: the partial-completion, substring and flex styles,
!> cycling, completion metadata (categories, annotations, groups),
!> moving through and choosing from *Completions* (M-<up>, M-<down>,
!> M-RET, mouse), `completion-at-point' in buffers, `completion-show-help',
!> and completion of environment variables and ~USER in file names.
module minibuffer_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: format_message, status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, get_buffer, get_buffer_create, set_buffer, current_buffer, erase_buffer, &
                         set_buffer_read_only, buffer_list
    use insdel_fm, only: del_range, insert
    use keymap_fm, only: define_key, key_parse, make_sparse_keymap, set_keymap_parent, keymap_live_p, &
                         free_keymap
    use lisp_fm, only: value_type, nil, t_value, make_string, make_fixnum, list, list_elements, list_length, &
                       list_elt, fixnump, xfixnum, stringp, xstring, nilp, make_list
    use data_fm, only: value_eq
    use character_fm, only: string_char_and_length
    use editfns_fm, only: point, point_max, goto_char, buffer_substring
    use frame_fm, only: selected_frame, frame_selected_window_ref
    use window_fm, only: window_type, set_window_buffer_internal, set_window_start
    use xdisp_fm, only: message
    use dispnew_fm, only: ding
    use fns_fm, only: compare_strings, COMPARE_EQUAL, schars, substring, string_lessp
    use dired_fm, only: file_name_completion, file_name_all_completions
    use lread_fm, only: sym
    use keyboard_fm, only: throw_exit, command_loop_level, this_command, last_command, minibuffer_message_text
    use minibuf_fm, only: minibufferp, minibuf_level, minibuffer_prompt_end, abort_minibuffers, &
                          minibuffer_local_map, read_from_minibuffer, completion_table_type, &
                          completion_predicate, completion_function_table, try_completion, all_completions, &
                          test_completion, completion_ignore_case, TABLE_FUNCTION, COMPLETE_TRY, COMPLETE_ALL, &
                          COMPLETE_TEST, COMPLETE_BOUNDARIES, minibuffer_calling_buffer, completion_list_table
    use fileio_fm, only: file_name_absolute_p, expand_file_name, substitute_in_file_name, file_name_directory, &
                         file_name_nondirectory, file_exists_p
    use files_fm, only: abbreviate_file_name
    implicit none(type, external)
    private

    public :: exit_minibuffer, delete_minibuffer_contents, read_file_name, syms_of_minibuffer, read_buffer
    public :: completing_read, completion_try_completion, completion_all_completions, completion_boundaries, &
              minibuffer_complete, minibuffer_complete_word, minibuffer_complete_and_exit, &
              minibuffer_completion_help, minibuffer_hide_completions, minibuffer_message, &
              read_file_name_internal

    !> Values of REQUIRE-MATCH for `completing_read', also those of
    !> `minibuffer-completion-confirm': nil, t, `confirm',
    !> `confirm-after-completion' and any other non-nil value.
    integer, parameter, public :: REQUIRE_MATCH_NIL = 0, REQUIRE_MATCH_T = 1, REQUIRE_MATCH_CONFIRM = 2, &
                                  REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION = 3, REQUIRE_MATCH_OTHER = 4

    !> Kinds of `completion-try-completion' result: nil, t, and (STRING .
    !> POINT).
    integer, parameter, public :: TRY_NONE = 0, TRY_EXACT = 1, TRY_STRING = 2

    !> A `completion-try-completion' result.
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

    !> Non-nil means automatically provide help for invalid completion
    !> input.  (Only t and nil: `lazy', `always' and `visible' are not
    !> ported.)  Port of minibuffer.el completion-auto-help.
    logical, public :: completion_auto_help = .true.

    !> If non-nil, print helpful inline messages during completion.  Port
    !> of minibuffer.el completion-show-inline-help.
    logical, public :: completion_show_inline_help = .true.

    !> If non-nil, stay quiet when there is no match.  Port of
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

    !> minibuffer-scroll-window is the *Completions* window: a repeated
    !> TAB scrolls it.
    logical :: completions_scroll_window = .false.
    !> What the root window showed before *Completions*.
    type(handle_type) :: completions_previous_buffer

    !> Non-nil means when reading a filename start with default dir in minibuffer.
    !>
    !> When the initial minibuffer contents show a name of a file or a directory,
    !> typing RETURN without editing the initial contents is equivalent to typing
    !> the default file name.
    !>
    !> If this variable is non-nil, the minibuffer contents are always
    !> initially non-empty, and typing RETURN without editing will fetch the
    !> default name, if one is provided.
    !>
    !> If this variable is nil, the minibuffer often starts out empty.
    !> Port of minibuffer.el insert-default-directory.
    logical, public :: insert_default_directory = .true.

contains

    !> Finish the minibuffer input (RET).  Port of
    !> minibuffer.el:exit-minibuffer.
    subroutine exit_minibuffer(status)
        type(status_type), intent(out) :: status
        if (minibufferp()) then
            if (command_loop_level /= minibuf_level) then
                call set_error(status, FM_GENERIC_ERROR, "Not in most nested command loop")
                return
            end if
        end if
        call throw_exit(.false.)
    end subroutine exit_minibuffer

    !> Delete the text after the prompt.  Port of
    !> minibuffer.el:delete-minibuffer-contents.
    subroutine delete_minibuffer_contents(status)
        type(status_type), intent(out) :: status
        call del_range(minibuffer_prompt_end(), buffers(cur)%zv, status)
    end subroutine delete_minibuffer_contents

    !> Read a file name in the minibuffer, starting with directory `dir'
    !> (default: default-directory) abbreviated as the input to edit.
    !> Unchanged input gives `default_filename' (default: the buffer's
    !> file); the result goes through substitute-in-file-name, so typing
    !> "~/x" or "/x" after the directory means just that.  Port of
    !> minibuffer.el:read-file-name-default (completion through
    !> `read_file_name_internal'; MUSTMATCH is a REQUIRE_MATCH_ value; no
    !> PREDICATE).
    recursive subroutine read_file_name(prompt, result, status, dir, default_filename, mustmatch, initial)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: dir, default_filename, initial
        integer, intent(in), optional :: mustmatch
        character(len=:), allocatable :: d, default, insdef, val
        logical :: have_default, saved_completing
        integer :: match
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
            default = d
        else
            call expand_file_name(initial, default, status, d)
            if (.not. status%ok()) return
        end if
        d = abbreviate_file_name(d)
        if (have_default) default = abbreviate_file_name(default)
        insdef = ""
        if (insert_default_directory) insdef = d
        if (present(initial)) insdef = insdef//initial
        match = REQUIRE_MATCH_NIL
        if (present(mustmatch)) match = mustmatch
        saved_completing = minibuffer_completing_file_name
        minibuffer_completing_file_name = .true.
        if (have_default) then
            call completing_read(prompt, completion_function_table(read_file_name_internal), val, status, &
                                 require_match=match, initial=insdef, history="file-name-history", default=default)
        else
            call completing_read(prompt, completion_function_table(read_file_name_internal), val, status, &
                                 require_match=match, initial=insdef, history="file-name-history")
        end if
        minibuffer_completing_file_name = saved_completing
        if (.not. status%ok()) return
        ! completing-read: empty input gives the default.
        if (len(val) == 0 .and. have_default) val = default
        ! Unchanged input means the default file.
        if (have_default .and. val == insdef .and. len(val) == len(insdef)) val = default
        result = substitute_in_file_name(val)
    end subroutine read_file_name

    !> Terminate this minibuffer argument.
    !> Port of minibuffer.el:exit-minibuffer.
    subroutine cmd_exit_minibuffer(status)
        type(status_type), intent(out) :: status
        call exit_minibuffer(status)
    end subroutine cmd_exit_minibuffer

    !> Abort the current minibuffer.
    !> Port of minibuffer.el:abort-minibuffers (one minibuffer level).
    subroutine cmd_abort_minibuffers(status)
        type(status_type), intent(out) :: status
        call abort_minibuffers(status)
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

    !> Call `exit-minibuffer' (there is no selected completion to insert
    !> first yet).  Port of minibuffer.el:minibuffer-completion-exit.
    subroutine cmd_minibuffer_completion_exit(status)
        type(status_type), intent(out) :: status
        call exit_minibuffer(status)
    end subroutine cmd_minibuffer_completion_exit

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
        call bind(minibuffer_local_map, "C-g", "abort-minibuffers")
        call bind(minibuffer_local_map, "C-j", "exit-minibuffer")
        call bind(minibuffer_local_map, "RET", "exit-minibuffer")
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
        call bind(minibuffer_local_completion_map, "TAB", "minibuffer-complete")
        call bind(minibuffer_local_completion_map, "<backtab>", "minibuffer-complete")
        call bind(minibuffer_local_completion_map, "SPC", "minibuffer-complete-word")
        call bind(minibuffer_local_completion_map, "RET", "minibuffer-completion-exit")
        call bind(minibuffer_local_completion_map, "?", "minibuffer-completion-help")
        call make_sparse_keymap(minibuffer_local_must_match_map)
        call set_keymap_parent(minibuffer_local_must_match_map, minibuffer_local_completion_map, status)
        call bind(minibuffer_local_must_match_map, "RET", "minibuffer-complete-and-exit")
        call bind(minibuffer_local_must_match_map, "C-j", "minibuffer-complete-and-exit")
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
        ! `try-completion' may return a subtly different result than
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

    !> The `try-completion' value `v' as a try result.
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
    !> when Emacs prompts the user for some input, and the user types into
    !> the minibuffer.  If called when the current buffer is not the
    !> minibuffer, this function just calls `message', and thus displays
    !> MESSAGE in the echo-area.  When called from the minibuffer, this
    !> function displays MESSAGE at the end of minibuffer text until the
    !> next input event arrives.  It encloses MESSAGE in [...] if it is
    !> not yet enclosed.  Port of minibuffer.el:minibuffer-message (no
    !> timeout; the overlay is minibuffer_message_text).
    subroutine minibuffer_message(msg)
        character(len=*), intent(in) :: msg
        integer :: k
        if (.not. minibufferp()) then
            call message(msg)
            return
        end if
        ! Clear out any old echo-area message to make way for our new
        ! thing.
        call message("")
        k = verify(msg, " ")
        if (k > 0 .and. len(msg) >= 2) then
            if (msg(k:k) == "[" .and. msg(len(msg):) == "]" .and. len(msg) - k >= 2) then
                minibuffer_message_text = msg
                last_minibuffer_message = msg
                return
            end if
        end if
        minibuffer_message_text = " ["//msg//"]"
        last_minibuffer_message = minibuffer_message_text
    end subroutine minibuffer_message

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

    !> Return non-nil if POS is in the current completion boundaries.
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
    !> WORD uses `completion--try-word-completion' in place of
    !> `completion-try-completion'.  EXPECT-EXACT, if non-nil, means that
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
    !> Return nil if there is no valid completion, else t.  If no
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
        this_command = sym("completion-at-point")
        if (.not. value_eq(last_command, sym("completion-at-point"))) completions_scroll_window = .false.
        if (completions_scroll_window .and. completions_visible()) then
            ! If there's a fresh completion window with a live buffer, and
            ! this command is repeated, scroll that window.
            call scroll_completions()
            return
        end if
        bits = completion_do_completion(beg, point_max(), .false., .false.)
    end subroutine minibuffer_complete

    !> Complete the minibuffer contents at most a single word.
    !> After one word is completed as much as possible, a space or hyphen
    !> is added, provided that matches some possible completion.  Return
    !> nil if there is no valid completion, else t.  Port of
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
    !> If `minibuffer-completion-confirm' is `confirm', do not try to
    !> complete; instead, ask for confirmation and accept any input if
    !> confirmed.  If `minibuffer-completion-confirm' is
    !> `confirm-after-completion', do not try to complete; instead, ask
    !> for confirmation if the preceding minibuffer command was a member
    !> of `minibuffer-confirm-exit-commands', and accept the input
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
            if (completion_ignore_case) then
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
        confirm_exit_command = value_eq(cmd, sym("completion-at-point")) .or. &
                               value_eq(cmd, sym("minibuffer-complete")) .or. &
                               value_eq(cmd, sym("minibuffer-complete-word"))
    end function confirm_exit_command

    ! ------------------------------------------------------------------
    ! The *Completions* buffer
    ! ------------------------------------------------------------------

    !> Whether the root window shows *Completions* (minibuffer--completions-visible).
    logical function completions_visible()
        type(handle_type) :: buf
        type(window_type), pointer :: sw
        sw => frame_selected_window_ref()
        buf = get_buffer("*Completions*")
        completions_visible = buf%index /= 0 .and. &
                              sw%buffer%index == buf%index .and. &
                              sw%buffer%generation == buf%generation
    end function completions_visible

    !> Scroll the window of possible completions: to the top when its end
    !> is visible, else down a screen.  The scrolling part of
    !> completion--in-region-1.
    subroutine scroll_completions()
        type(status_type) :: status
        type(window_type), pointer :: w
        w => frame_selected_window_ref()
        if (w%window_end_valid) then
            call set_window_start(w, 1_int64, status, noforce=.true.)
        else
            call set_window_start(w, w%window_end_pos, status)
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
        type(handle_type) :: mini, buf
        type(status_type) :: status
        character(len=:), allocatable :: string
        integer(int64) :: s, e, base_size
        integer :: k
        type(window_type), pointer :: sw
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
        call set_buffer_read_only(buf, .true., status)
        call goto_char(1_int64)
        call set_buffer(mini, status)
        sw => frame_selected_window_ref()
        if (.not. completions_visible()) then
            completions_previous_buffer = sw%buffer
            call set_window_buffer_internal(selected_frame%selected_window, buf, .false.)
        else
            call set_window_start(sw, 1_int64, status, noforce=.true.)
        end if
        completions_scroll_window = .true.
    end subroutine minibuffer_completion_help

    !> Sort `strings' as `string-lessp' does.
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
        character(len=:), allocatable :: last_string
        integer(int64) :: length, wwidth, columns, colwidth, column, cur_col, width
        integer :: k
        logical :: first
        type(window_type), pointer :: sw
        call goto_char(point_max())
        ! completions-header-format
        write (count, "(i0)") size(strings)
        call insert(trim(count)//" possible completions:"//achar(10), status)
        if (size(strings) == 0) return
        length = 0
        do k = 1, size(strings)
            length = max(length, schars(strings(k)%s))
        end do
        wwidth = 79
        sw => frame_selected_window_ref()
        if (sw%total_cols > 0) wwidth = sw%total_cols - 1
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
    end subroutine display_completion_list

    !> Get rid of an out-of-date *Completions* buffer: the root window goes
    !> back to the buffer it showed.  Port of
    !> minibuffer.el:minibuffer-hide-completions (bury-buffer).
    subroutine minibuffer_hide_completions()
        type(status_type) :: status
        type(handle_type) :: previous
        type(window_type), pointer :: sw
        if (.not. completions_visible()) return
        previous = completions_previous_buffer
        if (previous%index == 0) previous = minibuffer_calling_buffer
        if (previous%index == 0) return
        sw => frame_selected_window_ref()
        call set_window_buffer_internal(selected_frame%selected_window, previous, .false.)
        status = status_type()
    end subroutine minibuffer_hide_completions

    ! ------------------------------------------------------------------
    ! Reading with completion
    ! ------------------------------------------------------------------

    !> Read the name of a buffer and return it as a string.
    !> Prompt with PROMPT, which should be a string ending with a colon and a space.
    !> Provides completion on buffer names the user types.
    !> Optional second arg DEF is value to return if user enters an empty line,
    !>  instead of that empty string; the prompt shows it.
    !> Optional third arg REQUIRE-MATCH has the same meaning as the
    !>  REQUIRE-MATCH argument of `completing-read'.
    !> The buffer `except' (if given) is left out of the completions, as
    !> internal-complete-buffer-except does.  Port of minibuf.c:Fread_buffer
    !> (with format-prompt; internal buffers are not offered).
    recursive subroutine read_buffer(prompt, result, status, def, require_match, except)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: def
        integer, intent(in), optional :: require_match
        type(handle_type), intent(in), optional :: except
        type(handle_type), allocatable :: all(:)
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: p
        integer :: k, n, match
        all = buffer_list()
        allocate (names(size(all)))
        n = 0
        ! Most recently selected first.
        do k = 1, size(all)
            if (present(except)) then
                if (all(k)%index == except%index .and. all(k)%generation == except%generation) cycle
            end if
            if (len(buffers(all(k)%index)%name) > 0) then
                if (buffers(all(k)%index)%name(1:1) == " ") cycle
            end if
            n = n + 1
            names(n) = make_string(buffers(all(k)%index)%name)
        end do
        match = REQUIRE_MATCH_NIL
        if (present(require_match)) match = require_match
        p = prompt
        if (present(def)) then
            if (len(def) > 0) then
                ! Edit the default in before the colon (format-prompt with
                ! minibuffer-default-prompt-format " (default %s)").
                if (len(p) >= 2) then
                    if (p(len(p) - 1:) == ": ") p = p(1:len(p) - 2)
                end if
                if (len(p) >= 1) then
                    if (p(len(p):) == ":" .or. p(len(p):) == " ") p = p(1:len(p) - 1)
                end if
                p = p//" (default "//def//"): "
            end if
            call completing_read(p, completion_list_table(make_list(names(1:n))), result, status, &
                                 require_match=match, history="buffer-name-history", default=def)
        else
            call completing_read(p, completion_list_table(make_list(names(1:n))), result, status, &
                                 require_match=match, history="buffer-name-history")
        end if
    end subroutine read_buffer

    !> Read a string in the minibuffer, with completion.
    !> PROMPT is a string to prompt with; normally it ends in a colon and
    !> a space.  COLLECTION can be a list of strings, an alist, the
    !> obarray or a function (see `try-completion').  PREDICATE limits
    !> completion to a subset of COLLECTION.
    !>
    !> REQUIRE-MATCH can take the following values:
    !> - REQUIRE_MATCH_T means that the user is not allowed to exit unless
    !>   the input is (or completes to) an element of COLLECTION or is
    !>   null.
    !> - REQUIRE_MATCH_NIL means that the user can exit with any input.
    !> - REQUIRE_MATCH_CONFIRM means that the user can exit with any
    !>   input, but she needs to confirm her choice if the input is not an
    !>   element of COLLECTION.
    !> - REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION means that the user can
    !>   exit with any input, but she needs to confirm her choice if she
    !>   called `minibuffer-complete' right before
    !>   `minibuffer-complete-and-exit' and the input is not an element of
    !>   COLLECTION.
    !> - REQUIRE_MATCH_OTHER means that the user can exit only if the
    !>   input is (or completes to) an element of COLLECTION or is null,
    !>   but typing RET does not exit if it does non-null completion.
    !>
    !> If INITIAL-INPUT is present, insert it in the minibuffer initially.
    !> HISTORY names the history list (default `minibuffer-history').
    !> DEFAULT is the value for empty input, and for M-n.  Port of
    !> minibuf.c:Fcompleting_read with
    !> minibuffer.el:completing-read-default.
    recursive subroutine completing_read(prompt, collection, result, status, predicate, require_match, &
                                         initial, history, default)
        character(len=*), intent(in) :: prompt
        type(completion_table_type), intent(in) :: collection
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        integer, intent(in), optional :: require_match
        character(len=*), intent(in), optional :: initial, history, default
        type(completion_table_type) :: saved_table
        procedure(completion_predicate), pointer :: saved_pred
        type(handle_type) :: keymap
        integer :: saved_confirm, match
        logical :: saved_scroll
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
        saved_scroll = completions_scroll_window
        minibuffer_completion_table = collection
        minibuffer_completion_predicate => null()
        if (present(predicate)) minibuffer_completion_predicate => predicate
        minibuffer_completion_confirm = match
        if (match == REQUIRE_MATCH_T) minibuffer_completion_confirm = REQUIRE_MATCH_NIL
        completions_scroll_window = .false.
        if (present(history)) then
            call read_from_minibuffer(prompt, result, status, initial, keymap, history, default)
        else
            call read_from_minibuffer(prompt, result, status, initial, keymap, default=default)
        end if
        minibuffer_completion_table = saved_table
        minibuffer_completion_predicate => saved_pred
        minibuffer_completion_confirm = saved_confirm
        completions_scroll_window = saved_scroll
        if (.not. status%ok()) return
        if (len(result) == 0 .and. present(default)) result = default
    end subroutine completing_read

    !> Completion table for file names (`read-file-name-internal'):
    !> completion-file-name-table on the input (no environment variables,
    !> `substitute-in-file-name' quoting or ~USER completion).  Port of
    !> minibuffer.el:completion-file-name-table.
    recursive subroutine read_file_name_internal(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        character(len=:), allocatable :: name, specdir, realdir, expanded
        type(status_type) :: st
        integer :: slash
        status = status_type()
        result = nil()
        select case (action)
        case (COMPLETE_BOUNDARIES)
            slash = 0
            if (present(suffix)) slash = index(suffix, "/")
            if (slash > 0) then
                result = list(make_fixnum(schars(file_name_directory(string))), &
                              make_fixnum(schars(suffix(1:slash - 1))))
            else
                result = list(make_fixnum(schars(file_name_directory(string))), nil())
            end if
        case (COMPLETE_TEST)
            if (len(string) == 0) return
            if (present(pred)) then
                if (pred(make_string(string))) result = t_value()
            else
                call expand_file_name(string, expanded, st)
                if (st%ok()) then
                    if (file_exists_p(expanded)) result = t_value()
                end if
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
            end if
        end select
    end subroutine read_file_name_internal
end module minibuffer_fm
