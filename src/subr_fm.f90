! subr_fm.f90 -- basic functions from subr.el.
! Ported from GNU Emacs 31.1 lisp/subr.el.
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

!> Questions (`read_key', `y_or_n_p', `read_number'),
!> `add_to_history', and the match data helpers (`match_string',
!> `string_match_p', `looking_at_p', `looking_back').
module subr_fm
    use error_fm, only: status_type, set_error, FM_QUIT
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use lisp_fm, only: value_type, fixnump, xfixnum, nil, nilp, listp, make_string, cons, list_length, list_elt, &
                       make_keymap_ref, symbolp, xsymbol, symbols, &
                       list_elements, make_list, stringp, xstring, t_value
    use lread_fm, only: sym
    use data_fm, only: symbol_value, set
    use fns_fm, only: get
    use eval_fm, only: hook_type
    use buffer_fm, only: buffers, cur, current_buffer, buffer_modified_p, restore_buffer_modified_p
    use insdel_fm, only: inhibit_modification_hooks
    use keyboard_fm, only: read_event, discard_input, this_command_keys
    use dispnew_fm, only: ding
    use commands_fm, only: commands
    use callint_fm, only: prefix_arg
    use xdisp_fm, only: message, cursor_in_echo_area
    use keymap_fm, only: define_key, current_global_map, key_description, make_sparse_keymap, keymap_live_p, &
                         key_parse, lookup_key
    use handle_fm, only: handle_type
    use search_fm, only: string_match, looking_at, re_search_backward, match_beginning, match_end, &
                         re_search_forward, SEARCH_MOVE, regexp_quote, &
                         match_in_string_p, string_index_to_offset, SEARCH_NOERROR, match_data, &
                         set_match_data, match_data_translate, replace_match
    use syntax_fm, only: skip_syntax_backward
    use editfns_fm, only: buffer_substring, point, goto_char, point_min, point_max, narrow_to_region, widen
    implicit none(type, external)
    private

    public :: read_key, y_or_n_p, query_replace_answer, add_to_history
    public :: global_set_key, read_number, undefined, syms_of_subr, defvar_nil, forward_symbol, find_tag_default, &
              find_tag_default_as_regexp, find_tag_default_as_symbol_regexp
    public :: make_progress_reporter, progress_reporter_update, progress_reporter_done
    public :: read_char_from_minibuffer, read_char_choice, shell_quote_argument

    !> A numerical progress reporter (`make_progress_reporter' with
    !> MIN_VALUE and MAX_VALUE): the value at which to report next (the
    !> car of upstream's cons) and the parameters vector.
    type, public :: progress_reporter_type
        real(real64) :: next_value = 0
        !> The time of the next echo area update, in seconds.
        real(real64) :: update_time = 0
        integer(int64) :: min_value = 0, max_value = 0
        character(len=:), allocatable :: text
        integer :: min_change = 1
        real(real64) :: min_time = 0.2_real64
        !> A pulsing reporter (no MIN-VALUE and MAX-VALUE) and its indicator.
        logical :: pulsing = .false.
        integer :: pulse_index = 0
    end type progress_reporter_type
    public :: after_hook_proc, add_delayed_after_hook, silent_modifications_type, silent_modifications_begin, &
              silent_modifications_end
    public :: run_mode_hooks, delay_mode_hooks_begin, delay_mode_hooks_end, delaying_mode_hooks, &
              kill_delayed_mode_hooks

    !> Normal hook run in major mode functions, before the mode hooks.
    !> Port of subr.el change-major-mode-after-body-hook.
    type(hook_type), public, target :: change_major_mode_after_body_hook = &
                                       hook_type(name="change_major_mode_after_body_hook")

    !> Normal hook run at the very end of major mode functions.  Port of
    !> subr.el after-change-major-mode-hook.
    type(hook_type), public, target :: after_change_major_mode_hook = hook_type(name="after_change_major_mode_hook")
    public :: match_string, match_string_no_properties, string_match_p, looking_at_p, looking_back, &
              match_substitute_replacement

    !> Give KEY a global binding as COMMAND (a command name or any key
    !> definition).
    interface global_set_key
        module procedure global_set_key_value, global_set_key_command
    end interface global_set_key

    !> Maximum length of history lists before truncation takes place.
    !> A number means truncate to that length; truncation deletes old
    !> elements, and is done just after inserting a new element.
    !>
    !> This variable only affects history lists that don't specify their own
    !> maximum lengths.  Setting the `history_length' property of a history
    !> variable overrides this default.  Port of minibuf.c history-length
    !> (the value t, no truncation, is not ported: Deferred).
    integer, public :: history_length = 100
    !> True means to delete duplicates in history.
    !> If set to true when adding a new history element, all previous identical
    !> elements are deleted from the history list.  Port of minibuf.c
    !> history-delete-duplicates.
    logical, public :: history_delete_duplicates = .false.
    public :: ANSWER_ACT, ANSWER_SKIP, ANSWER_EXIT, ANSWER_ACT_AND_EXIT, &
              ANSWER_AUTOMATIC, ANSWER_RECENTER, ANSWER_HELP, ANSWER_QUIT, ANSWER_NONE, &
              ANSWER_EDIT_REPLACEMENT, ANSWER_EDIT_REPLACEMENT_EXACT_CASE, ANSWER_ACT_AND_SHOW, &
              ANSWER_EDIT, ANSWER_DELETE_AND_EDIT, ANSWER_BACKUP, ANSWER_UNDO, ANSWER_UNDO_ALL, &
              ANSWER_DIFF, ANSWER_SCROLL_UP, ANSWER_EXIT_PREFIX, ANSWER_SCROLL_DOWN, ANSWER_SCROLL_OTHER_WINDOW, &
              ANSWER_SCROLL_OTHER_WINDOW_DOWN

    !> query-replace-map (replace.el), made by `init_query_replace_map'.
    type(handle_type), public :: query_replace_map
    public :: init_query_replace_map

    !> The keys of `query_replace_map' and their answers.  (Binding ESC
    !> would prohibit the M-v binding.  Instead, callers should check for
    !> ESC specially.)
    character(len=*), parameter, public :: QRM_KEYS(38) = [character(len=11) :: &
                                                       "SPC", "DEL", "<delete>", "<backspace>", "y", "n", "Y", "N", "e", "E", ",", &
                                                           "q", "RET", "<return>", ".", "C-r", "C-w", "C-l", "!", "^", "u", "U", &
                                                          "d", "C-h", "<f1>", "<help>", "?", "C-g", "C-]", "C-v", "M-v", "<next>", &
                                                           "<prior>", "C-M-v", "M-<next>", "C-M-S-v", "M-<prior>", "<escape>"]
    character(len=*), parameter, public :: QRM_ANSWERS(38) = [character(len=27) :: &
                                                              "act", "skip", "skip", "skip", "act", "skip", "act", "skip", &
                                                              "edit_replacement", "edit_replacement_exact_case", "act_and_show", &
                                                              "exit", "exit", "exit", "act_and_exit", "edit", "delete_and_edit", &
                                                              "recenter", "automatic", "backup", "undo", "undo_all", "diff", &
                                                              "help", "help", "help", "help", "quit", "quit", "scroll_up", &
                                                              "scroll_down", "scroll_up", "scroll_down", "scroll_other_window", &
                                                              "scroll_other_window", "scroll_other_window_down", &
                                                              "scroll_other_window_down", "exit_prefix"]

    ! The answers of `query_replace_map'.
    integer, parameter :: ANSWER_NONE = 0, ANSWER_ACT = 1, ANSWER_SKIP = 2, ANSWER_EXIT = 3, &
                          ANSWER_ACT_AND_EXIT = 4, ANSWER_AUTOMATIC = 5, ANSWER_RECENTER = 6, &
                          ANSWER_HELP = 7, ANSWER_QUIT = 8, ANSWER_EDIT_REPLACEMENT = 9, &
                          ANSWER_EDIT_REPLACEMENT_EXACT_CASE = 10, ANSWER_ACT_AND_SHOW = 11, &
                          ANSWER_EDIT = 12, ANSWER_DELETE_AND_EDIT = 13, ANSWER_BACKUP = 14, &
                          ANSWER_UNDO = 15, ANSWER_UNDO_ALL = 16, ANSWER_DIFF = 17, &
                          ANSWER_SCROLL_UP = 18, ANSWER_EXIT_PREFIX = 19, ANSWER_SCROLL_DOWN = 20, &
                          ANSWER_SCROLL_OTHER_WINDOW = 21, ANSWER_SCROLL_OTHER_WINDOW_DOWN = 22

    !> Use `read_key' when reading answers to "y or n" questions by `y_or_n_p'.
    !> Otherwise, use the `read_from_minibuffer' to read the answers.
    !>
    !> When reading via the minibuffer, you can use the normal commands
    !> available in the minibuffer, and can, for instance, temporarily
    !> switch to another buffer, do things there, and then switch back
    !> to the minibuffer before entering the character.  This is not
    !> possible when using `read_key', but using `read_key' may be less
    !> confusing to some users.  Port of subr.el y-or-n-p-use-read-key.
    logical, public :: y_or_n_p_use_read_key = .false.

    !> Name of the history list to add `y_or_n_p' answers to (unallocated for
    !> no history).  Port of subr.el y-or-n-p-history-variable.
    character(len=:), allocatable, public :: y_or_n_p_history_variable

    !> What `silent_modifications_begin' changed, for
    !> `silent_modifications_end' to restore (the bindings of
    !> with-silent-modifications).
    type :: silent_modifications_type
        logical :: modified = .false.
        logical :: undo_enabled = .true.
        logical :: inhibit_modification_hooks = .false.
        type(value_type) :: inhibit_read_only
    end type silent_modifications_type

    abstract interface
        !> A function run after a mode's hooks (a minor mode's :after-hook,
        !> an element of `delayed_after_hook_functions').
        subroutine after_hook_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine after_hook_proc
    end interface

    interface
        !> Ask user a "y or n" question.
        !> Return true (in ANSWER) if answer is "y" and false if it is "n".
        !>
        !> PROMPT is the string to display to ask the question; `y_or_n_p'
        !> adds "(y or n) " to it.  If PROMPT is a non-empty string, and
        !> it ends with a non-space character, a space character will be
        !> appended to it.
        !>
        !> No confirmation of the answer is requested; a single character is
        !> enough.  SPC also means yes, and DEL means no.
        !>
        !> To be precise, this function translates user input into responses
        !> by consulting the bindings in `query_replace_map'; see the
        !> documentation of that variable for more information.  In this
        !> case, the useful bindings are `act', `skip', `recenter',
        !> `scroll_up', `scroll_down', and `quit'.
        !> An `act' response means yes, and a `skip' response means no.
        !> A `quit' response means to invoke `abort_recursive_edit'.
        !> If the user enters `recenter', `scroll_up', or `scroll_down'
        !> responses, perform the requested window recentering or scrolling
        !> and ask again.
        !>
        !> By default, this function uses the minibuffer to read the key.
        !> If `y_or_n_p_use_read_key' is true, `read_key' is used
        !> instead (which means that the user can't change buffers (and the
        !> like) while `y_or_n_p' is running).  Port of subr.el:y-or-n-p
        !> (no help-form or dialog boxes).  (In subr_read_fm.)
        recursive module subroutine y_or_n_p(prompt, answer, status)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            logical, intent(out) :: answer
            type(status_type), intent(out) :: status
        end subroutine y_or_n_p

        !> Register subr.el's commands (and keyboard.c's
        !> abort-recursive-edit).  (In subr_read_fm.)
        !> Read a character from the minibuffer, prompting for it with PROMPT.
        !> Like `read_char', but uses the minibuffer to read and return a character.
        !> CHARS, if present, are the characters accepted; the function will
        !> ignore any input that is not one of CHARS.
        !> HISTORY, if present, names the history list variable to use for
        !> navigating in input history using \`M-p' and \`M-n', with \`RET' to
        !> select a character from history.
        !> If HELP_FORM is present (upstream's `help_form' bound around the
        !> call), then pressing `help_char' displays it.  There is no need to
        !> explicitly add `help_char' to CHARS; `help_char' is bound
        !> automatically to `help_form_show'.  Port of
        !> subr.el:read-char-from-minibuffer (exiting after other
        !> insertions, its `post_command_hook', is deferred).  (In
        !> subr_read_fm.)
        recursive module subroutine read_char_from_minibuffer(prompt, c, status, chars, history, help_form)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            integer, intent(out) :: c
            type(status_type), intent(out) :: status
            integer, intent(in), optional :: chars(:)
            character(len=*), intent(in), optional :: history, help_form
        end subroutine read_char_from_minibuffer

        !> Read and return one of the characters in CHARS, prompting with PROMPT.
        !> CHARS should be a list of single characters.
        !> The function discards any input character that is not one of CHARS,
        !> and by default shows a message to the effect that it is not one of
        !> the expected characters.
        !>
        !> By default, this function uses the minibuffer to read the key
        !> non-modally (see `read_char_from_minibuffer'), and the optional
        !> argument INHIBIT_KEYBOARD_QUIT is ignored.  However, if
        !> `read_char_choice_use_read_key' is true, the modal `read_key'
        !> function is used instead (see read_char_choice_with_read_key),
        !> and INHIBIT_KEYBOARD_QUIT is passed to it.  HELP_FORM is the text
        !> `help_char' shows (upstream's `help_form').  Port of
        !> subr.el:read-char-choice.  (In subr_read_fm.)
        recursive module subroutine read_char_choice(prompt, chars, c, status, inhibit_keyboard_quit, help_form)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            integer, intent(in) :: chars(:)
            integer, intent(out) :: c
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: inhibit_keyboard_quit
            character(len=*), intent(in), optional :: help_form
        end subroutine read_char_choice

        module subroutine syms_of_subr()
            implicit none(type, external)
        end subroutine syms_of_subr

        !> Read from the minibuffer and return a numeric value, prompting with PROMPT.
        !> DEFAULT specifies a default value to return if the user just types RET.
        !> For historical reasons, the value of DEFAULT is always inserted into
        !> PROMPT, so it's recommended to use `format' instead of `format_prompt'
        !> to generate PROMPT.  HIST specifies a history list variable.  See
        !> `read_from_minibuffer' for details of the HIST argument.
        !>
        !> DEFAULTS, in place of DEFAULT, is an array of defaults: the first
        !> is the default, all are offered by M-n.  Port of
        !> subr.el:read-number (the answer is read as upstream's reader reads
        !> an integer; floats are not ported).  (In subr_read_fm.)
        module subroutine read_number(prompt, n, status, default, hist, defaults)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            integer(int64), intent(out) :: n
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: default
            character(len=*), intent(in), optional :: hist
            integer(int64), intent(in), optional :: defaults(:)
        end subroutine read_number

        !> Run mode hooks `delayed_mode_hooks' and HOOK, or delay HOOK.
        !>
        !> If the variable `delay_mode_hooks' is true, does not do anything,
        !> just adds HOOK to `delayed_mode_hooks'.
        !> Otherwise, runs hooks in the sequence: `change_major_mode_after_body_hook',
        !> `delayed_mode_hooks' (in reverse order), HOOK, then
        !> runs the hook `after_change_major_mode_hook'.
        !>
        !> Major mode functions should use this instead of `run_hooks' when
        !> running their FOO_mode_hook.  Port of subr.el:run-mode-hooks
        !> (no file-local variables yet).  (In subr_hooks_fm.)
        recursive module subroutine run_mode_hooks(status, hook)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            type(hook_type), intent(in), target, optional :: hook
        end subroutine run_mode_hooks

        !> Start `delay_mode_hooks': until the matching
        !> delay_mode_hooks_end, `run_mode_hooks' only records its hooks
        !> (in the current buffer).  SAVED keeps the outer value.  Port of
        !> subr.el:delay-mode-hooks.  (In subr_hooks_fm.)
        module subroutine delay_mode_hooks_begin(saved)
            implicit none(type, external)
            logical, intent(out) :: saved
        end subroutine delay_mode_hooks_begin

        !> End `delay_mode_hooks', restoring the value SAVED.  (In
        !> subr_hooks_fm.)
        module subroutine delay_mode_hooks_end(saved)
            implicit none(type, external)
            logical, intent(in) :: saved
        end subroutine delay_mode_hooks_end

        !> Forget the current buffer's `delayed_mode_hooks' (a local
        !> variable that `kill_all_local_variables' kills).  (In
        !> subr_hooks_fm.)
        module subroutine kill_delayed_mode_hooks()
            implicit none(type, external)
        end subroutine kill_delayed_mode_hooks

        !> Whether mode hooks are being delayed in the current buffer (the
        !> variable `delay_mode_hooks').  (In subr_hooks_fm.)
        module logical function delaying_mode_hooks()
            implicit none(type, external)
        end function delaying_mode_hooks

        !> Arrange for FN to run after the delayed mode hooks: add it to
        !> the current buffer's `delayed_after_hook_functions', which
        !> `run_mode_hooks' runs after `after_change_major_mode_hook'.
        !> (Upstream's minor modes push their :after-hook there while mode
        !> hooks are delayed.)  (In subr_hooks_fm.)
        module subroutine add_delayed_after_hook(fn)
            implicit none(type, external)
            procedure(after_hook_proc) :: fn
        end subroutine add_delayed_after_hook
    end interface

contains

    !> Quote ARGUMENT for passing as argument to an inferior shell.
    !>
    !> This function is designed to work with the syntax of your system's
    !> standard shell, and might produce incorrect results with unusual shells.
    !> See Info node `(elisp)Security Considerations'.  Port of
    !> subr.el:shell-quote-argument (POSIX shells: the systems are Linux).
    pure function shell_quote_argument(argument) result(quoted)
        character(len=*), intent(in) :: argument
        character(len=:), allocatable :: quoted
        integer :: i, b
        if (len(argument) == 0) then
            quoted = "''"
            return
        end if
        ! Quote everything except POSIX filename characters.
        ! This should be safe enough even for really weird shells.
        quoted = ""
        do i = 1, len(argument)
            b = ichar(argument(i:i), kind=int32)
            if (index("-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_./", argument(i:i)) > 0 .or. &
                b == 10) then
                if (b == 10) then
                    quoted = quoted//"'"//achar(10)//"'"
                else
                    quoted = quoted//argument(i:i)
                end if
            else if (b >= 128 .and. b < 192) then
                ! A continuation byte of the character quoted before.
                quoted = quoted//argument(i:i)
            else
                quoted = quoted//"\"//argument(i:i)
            end if
        end do
    end function shell_quote_argument

    !> Start modifying the current buffer without marking it modified,
    !> recording undo, running the change hooks or asking about read-only
    !> text; S keeps what to restore.  The start of subr.el's
    !> with-silent-modifications.
    subroutine silent_modifications_begin(s)
        type(silent_modifications_type), intent(out) :: s
        type(status_type) :: st
        if (cur == 0) return
        call buffer_modified_p(current_buffer(), s%modified, st)
        s%undo_enabled = buffers(cur)%undo_enabled
        buffers(cur)%undo_enabled = .false.
        call symbol_value(sym("inhibit_read_only"), s%inhibit_read_only, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        s%inhibit_modification_hooks = inhibit_modification_hooks%value
        inhibit_modification_hooks = .true.
    end subroutine silent_modifications_begin

    !> Restore what `silent_modifications_begin' changed: the end of
    !> with-silent-modifications (the buffer's modified flag as it was).
    subroutine silent_modifications_end(s)
        type(silent_modifications_type), intent(in) :: s
        type(status_type) :: st
        if (cur == 0) return
        inhibit_modification_hooks = s%inhibit_modification_hooks
        call set(sym("inhibit_read_only"), s%inhibit_read_only, st)
        buffers(cur)%undo_enabled = s%undo_enabled
        call restore_buffer_modified_p(s%modified)
    end subroutine silent_modifications_end

    !> Read one key with `prompt' in the echo area and the cursor after it.
    !> End of input reads as a quit.  Port of subr.el:read-key (single
    !> events; no keymap lookup of multi-event keys yet).
    subroutine read_key(prompt, key, status)
        character(len=*), intent(in) :: prompt
        type(value_type), intent(out) :: key
        type(status_type), intent(out) :: status
        logical :: have
        call message(prompt)
        cursor_in_echo_area = .true.
        call read_event(key, have, status)
        cursor_in_echo_area = .false.
        if (.not. status%ok()) return
        if (.not. have) call set_error(status, FM_QUIT)
    end subroutine read_key

    !> Keymap of responses to questions posed by commands like `query_replace'.
    !> The "bindings" in this map are not commands; they are answers.
    !> The valid answers include `act', `skip', `act_and_show',
    !> `act_and_exit', `exit', `exit_prefix', `recenter', `scroll_up',
    !> `scroll_down', `scroll_other_window', `scroll_other_window_down',
    !> `edit', `edit_replacement', `edit_replacement_exact_case',
    !> `delete_and_edit', `automatic', `backup', `undo', `undo_all',
    !> `quit', and `help'.  Made on first use.  Port of replace.el
    !> query-replace-map.
    subroutine init_query_replace_map()
        type(value_type), allocatable :: seq(:)
        type(status_type) :: st
        integer :: k
        if (keymap_live_p(query_replace_map)) return
        call make_sparse_keymap(query_replace_map)
        do k = 1, size(QRM_KEYS)
            call key_parse(trim(QRM_KEYS(k)), seq, st)
            if (st%ok()) call define_key(query_replace_map, seq, sym(trim(QRM_ANSWERS(k))), st)
        end do
    end subroutine init_query_replace_map

    !> The answer `key' is in `query_replace_map' (the bindings
    !> y-or-n-p, map-y-or-n-p and perform-replace use): an ANSWER_ code,
    !> ANSWER_NONE if it has none.
    integer function query_replace_answer(key) result(answer)
        type(value_type), intent(in) :: key
        type(value_type) :: def
        type(status_type) :: st
        character(len=:), allocatable :: name
        answer = ANSWER_NONE
        call init_query_replace_map()
        call lookup_key(make_keymap_ref(query_replace_map), [key], def, st, .true.)
        if (.not. st%ok() .or. .not. symbolp(def)) return
        name = symbols(xsymbol(def))%name
        select case (name)
        case ("act")
            answer = ANSWER_ACT
        case ("skip")
            answer = ANSWER_SKIP
        case ("edit_replacement")
            answer = ANSWER_EDIT_REPLACEMENT
        case ("edit_replacement_exact_case")
            answer = ANSWER_EDIT_REPLACEMENT_EXACT_CASE
        case ("act_and_show")
            answer = ANSWER_ACT_AND_SHOW
        case ("exit")
            answer = ANSWER_EXIT
        case ("act_and_exit")
            answer = ANSWER_ACT_AND_EXIT
        case ("edit")
            answer = ANSWER_EDIT
        case ("delete_and_edit")
            answer = ANSWER_DELETE_AND_EDIT
        case ("recenter")
            answer = ANSWER_RECENTER
        case ("automatic")
            answer = ANSWER_AUTOMATIC
        case ("backup")
            answer = ANSWER_BACKUP
        case ("undo")
            answer = ANSWER_UNDO
        case ("undo_all")
            answer = ANSWER_UNDO_ALL
        case ("diff")
            answer = ANSWER_DIFF
        case ("help")
            answer = ANSWER_HELP
        case ("quit")
            answer = ANSWER_QUIT
        case ("scroll_up")
            answer = ANSWER_SCROLL_UP
        case ("scroll_down")
            answer = ANSWER_SCROLL_DOWN
        case ("scroll_other_window")
            answer = ANSWER_SCROLL_OTHER_WINDOW
        case ("scroll_other_window_down")
            answer = ANSWER_SCROLL_OTHER_WINDOW_DOWN
        case ("exit_prefix")
            answer = ANSWER_EXIT_PREFIX
        case default
            answer = ANSWER_NONE
        end select
    end function query_replace_answer

    !> Add NEWELT to the history list stored in the variable HISTORY-VAR.
    !> If MAXELT is present, it specifies the maximum length of the history.
    !> Otherwise, the maximum history length is the value of the `history_length'
    !> property on symbol HISTORY-VAR, if set, or the value of the `history_length'
    !> variable.  The possible values of maximum length have the same meaning as
    !> the values of `history_length'.
    !> Remove duplicates of NEWELT if `history_delete_duplicates' is non-nil.
    !> If optional fourth arg KEEP-ALL is non-nil, add NEWELT to history even
    !> if it is empty or duplicates the most recent entry in the history.
    !> Port of subr.el:add-to-history (NEWELT a string; `added' says
    !> whether the list changed; a void HISTORY-VAR is a void-variable
    !> error in `status').
    subroutine add_to_history(history_var, newelt, maxelt, keep_all, added, status)
        character(len=*), intent(in) :: history_var, newelt
        integer, intent(in), optional :: maxelt
        logical, intent(in), optional :: keep_all
        logical, intent(out), optional :: added
        type(status_type), intent(out), optional :: status
        type(value_type) :: history, prop, first
        type(value_type), allocatable :: elems(:), kept(:)
        type(status_type) :: st
        logical :: all, limited
        integer :: limit, k, n
        if (present(added)) added = .false.
        if (present(status)) status = status_type()
        limited = .true.
        limit = history_length
        if (present(maxelt)) then
            limit = maxelt
        else
            call get(sym(history_var), sym("history_length"), prop, st)
            if (fixnump(prop)) then
                limit = int(xfixnum(prop))
            else if (.not. nilp(prop)) then
                ! A non-integer MAXELT: no truncation.
                limited = .false.
            end if
        end if
        all = .false.
        if (present(keep_all)) all = keep_all
        call symbol_value(sym(history_var), history, st)
        if (.not. st%ok()) then
            if (present(status)) status = st
            return
        end if
        if (.not. listp(history)) return
        if (.not. all .and. len(newelt) == 0) return
        if (.not. all .and. list_length(history) > 0) then
            first = list_elt(history, 1_int64)
            if (stringp(first)) then
                if (xstring(first) == newelt .and. len(xstring(first)) == len(newelt)) return
            end if
        end if
        call list_elements(history, elems)
        allocate (kept(size(elems) + 1))
        kept(1) = make_string(newelt)
        n = 1
        do k = 1, size(elems)
            if (history_delete_duplicates) then
                ! (delete newelt history)
                if (stringp(elems(k))) then
                    if (xstring(elems(k)) == newelt .and. len(xstring(elems(k))) == len(newelt)) cycle
                end if
            end if
            n = n + 1
            kept(n) = elems(k)
        end do
        if (limited) then
            if (limit <= 0) then
                n = 0
            else
                n = min(n, limit)
            end if
        end if
        call set(sym(history_var), make_list(kept(1:n)), st)
        if (present(added)) added = .true.
    end subroutine add_to_history

    !> Beep to tell the user this binding is undefined.  Port of
    !> subr.el:undefined (no keyboard macro definition or mouse events).
    subroutine undefined(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call ding(status=status)
        if (.not. status%ok()) return
        call message(key_description(this_command_keys)//" is undefined")
        ! If this is a down-mouse event, don't reset prefix-arg; pass it to
        ! the command run by the up event.
        prefix_arg = nil()
    end subroutine undefined

    !> (defvar NAME nil): give the variable NAME the value nil unless it
    !> has a value already.  Port of eval.c:Fdefvar (INITVALUE nil).
    subroutine defvar_nil(name)
        character(len=*), intent(in) :: name
        type(value_type) :: v
        type(status_type) :: st
        call symbol_value(sym(name), v, st)
        if (.not. st%ok()) call set(sym(name), nil(), st)
    end subroutine defvar_nil

    !> Give KEY a global binding as COMMAND.
    !> This is a legacy function; see `keymap_global_set' for the
    !> recommended function to use instead.
    !>
    !> COMMAND is the command definition to use; usually it is
    !> a symbol naming an interactively-callable function.
    !> KEY is a key sequence: a vector of events, as `key_parse' makes.
    !>
    !> Note that if KEY has a local binding in the current buffer,
    !> that local binding will continue to shadow any global binding
    !> that you make with this function.  Port of subr.el:global-set-key.
    subroutine global_set_key_value(key, command, status)
        type(value_type), intent(in) :: key(:), command
        type(status_type), intent(out) :: status
        call define_key(current_global_map(), key, command, status)
    end subroutine global_set_key_value

    !> `global_set_key' with the command named COMMAND.
    subroutine global_set_key_command(key, command, status)
        type(value_type), intent(in) :: key(:)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        call define_key(current_global_map(), key, sym(command), status)
    end subroutine global_set_key_command

    !> Return the string of text matched by the previous search or regexp
    !> operation.  NUM specifies the number of the parenthesized
    !> sub-expression in the last regexp whose match to return.  Zero
    !> means the entire text matched by the whole regexp or whole string.
    !>
    !> The return value is an empty string (and `matched' false) if NUMth
    !> pair didn't match, or there were less than NUM pairs.
    !>
    !> STRING should be given if the last search was by `string_match' on
    !> STRING.  If STRING is absent, the current buffer should be the same
    !> buffer as the one in which the search/match was performed.  Port of
    !> subr.el:match-string.
    function match_string(num, string, matched) result(text)
        integer, intent(in) :: num
        character(len=*), intent(in), optional :: string
        logical, intent(out), optional :: matched
        character(len=:), allocatable :: text
        integer(int64) :: b, e
        type(status_type) :: status
        text = ""
        if (present(matched)) matched = .false.
        b = match_beginning(num, status)
        if (.not. status%ok() .or. b == 0) return
        e = match_end(num, status)
        if (present(matched)) matched = .true.
        if (present(string)) then
            text = string(string_index_to_offset(string, b) + 1:string_index_to_offset(string, e))
        else
            call buffer_substring(b, e, text, status)
        end if
    end function match_string

    !> Return string of text matched by last search, without text
    !> properties.  NUM specifies which parenthesized expression in the
    !> last regexp.  Value is an empty string if NUMth pair didn't match, or there
    !> were less than NUM pairs.  Zero means the entire text matched by
    !> the whole regexp or whole string.  STRING should be given if the
    !> last search was by `string_match' on STRING.  If STRING is absent, the
    !> current buffer should be the same buffer as the one in which the
    !> search/match was performed.  Port of
    !> subr.el:match-string-no-properties.
    function match_string_no_properties(num, string, matched) result(text)
        integer, intent(in) :: num
        character(len=*), intent(in), optional :: string
        logical, intent(out), optional :: matched
        character(len=:), allocatable :: text
        text = match_string(num, string, matched)
    end function match_string_no_properties

    !> Return REPLACEMENT as it will be inserted by `replace_match'.
    !> In other words, all back-references in the form `\&' and `\N'
    !> are substituted with actual strings matched by the last search.
    !> Optional FIXEDCASE, LITERAL, STRING and SUBEXP have the same
    !> meaning as for `replace_match'.  Port of
    !> subr.el:match-substitute-replacement.
    function match_substitute_replacement(replacement, status, fixedcase, literal, string, subexp) result(text)
        character(len=*), intent(in) :: replacement
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: fixedcase, literal
        character(len=*), intent(in), optional :: string
        integer, intent(in), optional :: subexp
        character(len=:), allocatable :: text, match
        integer(int64), allocatable :: saved(:)
        integer(int64) :: start
        text = ""
        match = match_string(0, string)
        ! save-match-data
        saved = match_data()
        start = match_beginning(0, status)
        if (.not. status%ok()) return
        call match_data_translate(1 - start)
        call replace_match(replacement, status, fixedcase, literal, match, subexp, text)
        call set_match_data(saved)
    end function match_substitute_replacement

    !> Same as `string_match' except this function does not change the
    !> match data.  Port of subr.el:string-match-p.
    integer(int64) function string_match_p(regexp, string, status, start) result(idx)
        character(len=*), intent(in) :: regexp, string
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start
        idx = string_match(regexp, string, status, start, inhibit_modify=.true.)
    end function string_match_p

    !> Same as `looking_at' except this function does not change the match
    !> data.  Port of subr.el:looking-at-p.
    logical function looking_at_p(regexp, status)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        looking_at_p = looking_at(regexp, status, inhibit_modify=.true.)
    end function looking_at_p

    !> Return true if text before point matches regular expression
    !> REGEXP.  Like `looking_at' except matches before point, and is
    !> slower.  LIMIT if true speeds up the search by specifying a
    !> minimum starting position, to avoid checking matches that would
    !> start before LIMIT.
    !>
    !> If GREEDY is true, extend the match backwards as far as possible,
    !> stopping when a single additional previous character cannot be part
    !> of a match for REGEXP.  When the match is extended, its starting
    !> position is allowed to occur before LIMIT.
    !>
    !> As a general recommendation, try to avoid using `looking_back'
    !> wherever possible, since it is slow.  Port of subr.el:looking-back.
    logical function looking_back(regexp, status, limit, greedy) result(found)
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: limit
        logical, intent(in), optional :: greedy
        integer(int64) :: start, pos, saved_begv, saved_zv
        logical :: ignored
        start = point()
        call re_search_backward("\(?:"//regexp//"\)\=", pos, status, bound=limit, noerror=SEARCH_NOERROR)
        found = status%ok() .and. pos > 0
        if (found) pos = point()
        call goto_char(start)
        if (.not. found) return
        if (.not. present(greedy)) return
        if (.not. greedy) return
        saved_begv = point_min()
        saved_zv = point_max()
        call narrow_to_region(point_min(), start, status)
        do while (pos > point_min())
            call goto_char(pos - 1)
            if (.not. looking_at("\(?:"//regexp//"\)\'", status)) exit
            pos = pos - 1
        end do
        call goto_char(pos)
        ignored = looking_at("\(?:"//regexp//"\)\'", status)
        call widen()
        call narrow_to_region(saved_begv, saved_zv, status)
        call goto_char(start)
    end function looking_back

    !> Seconds since some fixed time (float-time, for time differences).
    real(real64) function float_time()
        integer(int64) :: count, rate
        call system_clock(count, rate)
        float_time = real(count, real64)/real(rate, real64)
    end function float_time

    !> Return progress reporter object for use with `progress_reporter_update'.
    !>
    !> MESSAGE is shown in the echo area, with a status indicator
    !> appended to the end.  When you call `progress_reporter_done', the
    !> word "done" is printed after the MESSAGE.
    !>
    !> MIN_VALUE and MAX_VALUE are starting (0% complete) and final (100%
    !> complete) states of operation; the latter should be larger.  The
    !> status message shows the percentage progress.  The minimal change
    !> to report is 1%, the minimum interval between echo area updates
    !> 0.2 seconds.  Without MIN_VALUE and MAX_VALUE, the reporter is a
    !> pulsing one (its indicator turns).  Port of
    !> subr.el:make-progress-reporter (default CURRENT-VALUE, MIN-CHANGE
    !> and MIN-TIME).
    subroutine make_progress_reporter(reporter, message_text, min_value, max_value)
        type(progress_reporter_type), intent(out) :: reporter
        character(len=*), intent(in) :: message_text
        integer(int64), intent(in), optional :: min_value, max_value
        type(status_type) :: st
        reporter%text = message_text
        if (string_match("[[:alnum:]]\'", message_text, st) > 0) reporter%text = message_text//"..."
        reporter%update_time = float_time()
        if (.not. (present(min_value) .and. present(max_value))) then
            ! A pulsing indicator.
            reporter%pulsing = .true.
            call progress_reporter_pulse(reporter)
            return
        end if
        reporter%next_value = real(min_value, real64)
        reporter%update_time = float_time()
        reporter%min_value = min_value
        reporter%max_value = max_value
        ! Force a call to `message' now.
        call progress_reporter_update(reporter, min_value)
    end subroutine make_progress_reporter

    !> Report progress of an operation in the echo area.
    !> VALUE should be a number between MIN_VALUE and MAX_VALUE.
    !>
    !> This function is relatively inexpensive.  If the change since
    !> last update is too small or insufficient time has passed, it does
    !> nothing.  Port of subr.el:progress-reporter-update (with
    !> progress-reporter-do-update and progress-reporter-echo-area).
    subroutine progress_reporter_update(reporter, value)
        type(progress_reporter_type), intent(inout) :: reporter
        integer(int64), intent(in) :: value
        real(real64) :: one_percent, now
        integer(int64) :: percentage
        logical :: enough_time_passed
        character(len=24) :: buf
        if (real(value, real64) < reporter%next_value) return
        ! See if enough time has passed since the last update.
        now = float_time()
        enough_time_passed = reporter%update_time < now
        ! Calculate time for the next update.
        if (enough_time_passed) reporter%update_time = reporter%update_time + reporter%min_time
        one_percent = real(reporter%max_value - reporter%min_value, real64)/100.0_real64
        if (reporter%max_value == reporter%min_value) then
            percentage = 0
        else
            percentage = int((value - reporter%min_value)/one_percent, int64)
        end if
        ! If we are not printing message because not enough time has
        ! passed, use 1 instead of MIN-CHANGE.
        if (enough_time_passed) then
            reporter%next_value = reporter%min_value + (percentage + reporter%min_change)*one_percent
        else
            reporter%next_value = reporter%min_value + (percentage + 1)*one_percent
        end if
        reporter%next_value = min(reporter%next_value, real(reporter%max_value, real64))
        reporter%next_value = real(ceiling(reporter%next_value, int64), real64)
        ! Print message only if enough time has passed.
        if (.not. enough_time_passed) return
        if (percentage > 0) then
            write (buf, "(i0)") percentage
            call message(reporter%text//trim(buf)//"%")
        else
            call message(reporter%text)
        end if
    end subroutine progress_reporter_update

    !> Print reporter's message followed by word "done" in echo area.
    !> Port of subr.el:progress-reporter-done.
    subroutine progress_reporter_done(reporter)
        type(progress_reporter_type), intent(in) :: reporter
        call message(reporter%text//"done")
    end subroutine progress_reporter_done

    !> The update of a pulsing reporter: turn its indicator when enough
    !> time has passed.  Port of the pulsing branch of
    !> subr.el:progress-reporter-do-update (with
    !> progress-reporter-echo-area).
    subroutine progress_reporter_pulse(reporter)
        type(progress_reporter_type), intent(inout) :: reporter
        character(len=4), parameter :: pulse_characters = "-\|/"
        integer :: k
        if (.not. (reporter%update_time < float_time())) return
        reporter%update_time = reporter%update_time + reporter%min_time
        reporter%pulse_index = reporter%pulse_index + 1
        k = mod(reporter%pulse_index, 4) + 1
        call message(reporter%text//" "//pulse_characters(k:k))
    end subroutine progress_reporter_pulse

    !> Move point to the next position that is the end of a symbol.
    !> A symbol is any sequence of characters that are in either the
    !> word constituent or symbol constituent syntax class.
    !> With prefix argument ARG, do it ARG times if positive, or move
    !> backwards ARG times if negative.  Port of subr.el:forward-symbol.
    subroutine forward_symbol(arg)
        integer(int64), intent(in) :: arg
        integer(int64) :: found, moved, n
        type(status_type) :: st
        if (arg >= 0) then
            call re_search_forward("\(\sw\|\s_\)+", found, st, noerror=SEARCH_MOVE, count=arg)
        else
            n = arg
            do while (n < 0)
                call re_search_backward("\(\sw\|\s_\)+", found, st, noerror=SEARCH_MOVE)
                if (found > 0) moved = skip_syntax_backward("w_")
                n = n + 1
            end do
        end if
    end subroutine forward_symbol

    !> The bounds of the symbol at point (`found' false if none).  Port
    !> of thingatpt.el:bounds-of-thing-at-point for `symbol' (end-op
    !> forward-symbol, beginning-op thing-at-point--beginning-of-symbol,
    !> whose search failing is an error ignored).
    subroutine bounds_of_symbol_at_point(beg, end, found)
        integer(int64), intent(out) :: beg, end
        logical, intent(out) :: found
        integer(int64) :: orig, at, real_end, real_beg
        type(status_type) :: st
        logical :: ok
        found = .false.
        beg = 0
        end = 0
        orig = point()
        ! If the buffer is totally empty, give up.
        call goto_char(point_min())
        call re_search_forward("[^"//achar(9)//achar(10)//" ]", at, st, noerror=SEARCH_NOERROR)
        if (at == 0) then
            call goto_char(orig)
            return
        end if
        call goto_char(orig)
        ! Try moving forward, then back.
        call forward_symbol(1_int64)
        call beginning_of_symbol(ok)
        if (.not. ok) then
            call goto_char(orig)
            return
        end if
        beg = point()
        if (beg <= orig) then
            ! If that brings us all the way back to ORIG, it worked.  But
            ! END may not be the real end.  So find the real end that
            ! corresponds to BEG.
            call forward_symbol(1_int64)
            real_end = point()
            if (orig <= real_end .and. beg < real_end) then
                end = real_end
                found = .true.
            end if
        else
            call goto_char(orig)
            ! Try a second time, moving backward first and then forward,
            ! so that we can find a thing that ends at ORIG.
            call beginning_of_symbol(ok)
            if (ok) then
                call forward_symbol(1_int64)
                end = point()
                call beginning_of_symbol(ok)
                if (ok) then
                    real_beg = point()
                    if (real_beg <= orig .and. orig <= end .and. real_beg < end) then
                        beg = real_beg
                        found = .true.
                    end if
                end if
            end if
        end if
        call goto_char(orig)
    contains

        ! thing-at-point--beginning-of-symbol: `ok' false when the search
        ! signals.
        subroutine beginning_of_symbol(ok)
            logical, intent(out) :: ok
            integer(int64) :: found, moved
            type(status_type) :: st
            call re_search_backward("\(\sw\|\s_\)+", found, st)
            ok = st%ok() .and. found > 0
            if (ok) moved = skip_syntax_backward("w_")
        end subroutine beginning_of_symbol
    end subroutine bounds_of_symbol_at_point

    !> Determine default tag to search for, based on text at point.
    !> If there is no plausible default, `found' is false.  Port of
    !> subr.el:find-tag-default (with find-tag-default-bounds).
    subroutine find_tag_default(tag, found)
        character(len=:), allocatable, intent(out) :: tag
        logical, intent(out) :: found
        integer(int64) :: beg, end
        type(status_type) :: st
        tag = ""
        call bounds_of_symbol_at_point(beg, end, found)
        if (found) call buffer_substring(beg, end, tag, st)
    end subroutine find_tag_default

    !> Return regexp that matches the default tag at point.
    !> If there is no tag at point, `found' is false.  Port of
    !> subr.el:find-tag-default-as-regexp (find-tag-default-function is
    !> not ported: the default is `find_tag_default').
    subroutine find_tag_default_as_regexp(regexp, found)
        character(len=:), allocatable, intent(out) :: regexp
        logical, intent(out) :: found
        character(len=:), allocatable :: tag
        call find_tag_default(tag, found)
        regexp = ""
        if (found) regexp = regexp_quote(tag)
    end subroutine find_tag_default_as_regexp

    !> Return regexp that matches the default tag at point as symbol.
    !> If there is no tag at point, `found' is false.  Port of
    !> subr.el:find-tag-default-as-symbol-regexp.
    subroutine find_tag_default_as_symbol_regexp(regexp, found)
        character(len=:), allocatable, intent(out) :: regexp
        logical, intent(out) :: found
        call find_tag_default_as_regexp(regexp, found)
        if (found) regexp = "\_<"//regexp//"\_>"
    end subroutine find_tag_default_as_symbol_regexp
end module subr_fm
