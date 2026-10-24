! apropos_fm.f90 -- apropos commands for users and programmers.
! Ported from GNU Emacs 31.1 lisp/apropos.el.
!
! Copyright (C) 1989-2026 Free Software Foundation, Inc.
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

!> The apropos commands: `apropos_command' (C-h a), `apropos_function',
!> `apropos_user_option' (C-h u), `apropos_variable' and
!> `apropos_documentation' (C-h d), which list in *Apropos* the
!> functions and variables whose names (or documentation) match a
!> pattern: a word, a list of words (any two of them, with their
!> synonyms) or a regexp, as upstream.  The functions are the commands
!> and the documented procedures, the variables the documented ones
!> (docstrings_fm); their values are shown as Fortran.  Apropos mode's
!> RET (`apropos_follow'), n and p work on the entries, whose labels
!> stand for upstream's buttons.
!>
!> Deferred: `apropos' itself and `apropos_value', `apropos_library',
!> `apropos_local_variable', `apropos_local_value' (they need symbol
!> property lists, faces, libraries or values of every symbol), the
!> scores of `apropos_sort_by_scores' (the default, false, sorts by
!> name; `apropos_documentation' sorts by score as upstream), faces of
!> entries and matches, mouse buttons, `apropos_compact_layout',
!> `apropos_do_all' (C-u still shows all functions or variables), and
!> revert (g).
module apropos_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, format_message, FM_USER_ERROR, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, symbols, nsymbols, list_elements, make_string, xstring, stringp
    use lread_fm, only: sym
    use buffer_fm, only: buffers, cur, current_buffer, get_buffer, set_buffer, buffer_live_p, get_buffer_create, &
                         kill_buffer, generate_new_buffer_name
    use editfns_fm, only: point_min, point_max, buffer_string, goto_char, line_beginning_position
    use insdel_fm, only: insert
    use textprop_fm, only: put_text_property, get_text_property, next_single_property_change
    use search_fm, only: string_match, regexp_quote, match_end
    use keymap_fm, only: key_description, where_is_internal, keymap_set, make_sparse_keymap
    use callint_fm, only: commandp, current_prefix_arg, fboundp
    use minibuf_fm, only: read_string
    use xdisp_fm, only: message
    use fill_fm, only: fill_region
    use simple_fm, only: fill_prefix, special_mode
    use buffer_fm, only: fill_column
    use cmds_fm, only: forward_line
    use frame_fm, only: selected_frame
    use window_fm, only: window_leaves
    use derived_fm, only: major_mode_type
    use modes_fm, only: major_modes
    use eval_fm, only: hook_type
    use help_fm, only: with_output_to_temp_buffer, substitute_command_keys, register_keymap_name
    implicit none(type, external)
    private

    public :: apropos_command, apropos_documentation, apropos_read_pattern, apropos_parse_pattern, apropos_mode, &
              apropos_words_to_regexp, apropos_true_hit, apropos_calc_scores, apropos_pattern, apropos_regexp, &
              apropos_all_words_regexp, syms_of_apropos, apropos_score_doc, apropos_score_symbol, button_at
    public :: VAR_NONE, VAR_USER_OPTION, VAR_ANY

    !> What `apropos_command' lists: functions (VAR_NONE), user options or
    !> any documented variables (upstream's VAR-PREDICATE).
    integer, parameter :: VAR_NONE = 0, VAR_USER_OPTION = 1, VAR_ANY = 2

    !> Keymap used in Apropos mode.  Port of apropos.el apropos-mode-map.
    type(handle_type), public :: apropos_mode_map

    !> Hook run when mode is turned on.  Port of apropos.el apropos-mode-hook.
    type(hook_type), target, public :: apropos_mode_hook = hook_type(name="apropos_mode_hook")

    !> Apropos pattern as entered by user.  Port of apropos.el
    !> apropos-pattern.
    character(len=:), allocatable :: apropos_pattern

    !> Apropos pattern passed through `regexp_quote'.  Port of apropos.el
    !> apropos-pattern-quoted.
    character(len=:), allocatable :: apropos_pattern_quoted

    !> Regexp used in current apropos run.  Port of apropos.el
    !> apropos-regexp.
    character(len=:), allocatable :: apropos_regexp

    !> Regexp matching `apropos_all_words'.  Port of apropos.el
    !> apropos-all-words-regexp.
    character(len=:), allocatable :: apropos_all_words_regexp

    !> One word of a word list (a string of an array of strings).
    type, public :: apropos_word_type
        character(len=:), allocatable :: s
    end type apropos_word_type

    !> Current list of apropos words extracted from `apropos_pattern', and
    !> the list of words and synonyms.  Port of apropos.el apropos-words
    !> and apropos-all-words.
    type(apropos_word_type), allocatable :: apropos_words(:), apropos_all_words(:)

    !> List of synonyms known by apropos.  Each element is a list of words
    !> where the first word is the standard Emacs term, and the rest of
    !> the words are alternative terms.  Port of apropos.el
    !> apropos-synonyms.
    character(len=*), parameter :: SYNONYMS(4) = [character(len=16) :: "find open edit", "kill cut", "yank paste", &
                                                                                                                 "region selection"]

    !> An entry of the apropos accumulator: SYMBOL SCORE FN-DOC VAR-DOC
    !> (the docs unallocated for nil).
    type :: apropos_item_type
        character(len=:), allocatable :: name
        integer(int64) :: score = 0
        character(len=:), allocatable :: fn_doc, var_doc
    end type apropos_item_type

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

    interface
        !> Show commands (interactively callable functions) that match PATTERN.
        !> PATTERN can be a word, a list of words (separated by spaces),
        !> or a regexp (using some regexp special characters).  If it is a word,
        !> search for matches for that word as a substring.  If it is a list of words,
        !> search for matches for any two (or more) of those words.
        !>
        !> With DO_ALL, also show noninteractive functions.
        !>
        !> If VAR_KIND is not VAR_NONE, show only variables: user options
        !> (VAR_USER_OPTION) or all documented variables (VAR_ANY).
        !>
        !> When called from a program, PATTERN is used as a regexp unless
        !> WORDS, a word list, is allocated.  Port of
        !> apropos.el:apropos-command.  (In apropos_print_fm.)
        module subroutine apropos_command(pattern, words, do_all, var_kind, status)
            implicit none(type, external)
            character(len=*), intent(in) :: pattern
            type(apropos_word_type), allocatable, intent(in) :: words(:)
            logical, intent(in) :: do_all
            integer, intent(in) :: var_kind
            type(status_type), intent(out) :: status
        end subroutine apropos_command

        !> Show symbols whose documentation contains matches for PATTERN.
        !> PATTERN can be a word, a list of words (separated by spaces),
        !> or a regexp (using some regexp special characters).  If it is a word,
        !> search for matches for that word as a substring.  If it is a list of words,
        !> search for matches for any two (or more) of those words.
        !>
        !> The documentation searched is that of the functions and variables
        !> help knows (all of them are predefined).  Port of
        !> apropos.el:apropos-documentation (sorted by scores, as
        !> `apropos_documentation_sort_by_scores' is true).  (In
        !> apropos_print_fm.)
        module subroutine apropos_documentation(pattern, words, status)
            implicit none(type, external)
            character(len=*), intent(in) :: pattern
            type(apropos_word_type), allocatable, intent(in) :: words(:)
            type(status_type), intent(out) :: status
        end subroutine apropos_documentation

        !> Invokes any button at point, otherwise invokes the nearest label
        !> button.  Port of apropos.el:apropos-follow (the label buttons'
        !> actions: describe-function or describe-variable).  (In
        !> apropos_print_fm.)
        recursive module subroutine apropos_follow(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine apropos_follow
    end interface

    !> Text properties marking the entries: `apropos_button' is
    !> `apropos_symbol' on a name and the label's type on a label, whose
    !> `apropos_symbol_name' is the entry's name (upstream's buttons).
    character(len=*), parameter :: P_BUTTON = "apropos_button", P_NAME = "apropos_symbol_name"

contains

    ! ------------------------------------------------------------------
    ! Patterns
    ! ------------------------------------------------------------------

    !> Return a regexp matching any two of the words in WORDS.
    !> WILD should be a subexpression matching wildcards between matches.
    !> Port of apropos.el:apropos-words-to-regexp.
    function apropos_words_to_regexp(words, wild) result(re)
        type(apropos_word_type), intent(in) :: words(:)
        character(len=*), intent(in) :: wild
        character(len=:), allocatable :: re, others
        type(apropos_word_type), allocatable :: w(:)
        integer :: i, j, n
        ! (delete-dups (copy-sequence words))
        allocate (w(size(words)))
        n = 0
        do i = 1, size(words)
            do j = 1, n
                if (w(j)%s == words(i)%s) exit
            end do
            if (j <= n) cycle
            n = n + 1
            w(n)%s = words(i)%s
        end do
        if (n == 1) then
            re = w(1)%s
            return
        end if
        re = ""
        do i = 1, n
            others = ""
            do j = 1, n
                if (j == i) cycle
                if (len(others) > 0) others = others//"\|"
                others = others//w(j)%s
            end do
            if (i > 1) re = re//"\|"
            re = re//"\(?:"//w(i)%s//"\)"//wild//"\(?:"//others//"\)"
        end do
    end function apropos_words_to_regexp

    !> Read an apropos pattern, either a word list or a regexp.
    !> Returns the user pattern, either a list of words which are matched
    !> literally (`words' allocated), or a string which is used as a regexp
    !> to search for.  SUBJECT is a string that is included in the prompt
    !> to identify what kind of objects to search.  Port of
    !> apropos.el:apropos-read-pattern.
    recursive subroutine apropos_read_pattern(subject, pattern, words, status)
        character(len=*), intent(in) :: subject
        character(len=:), allocatable, intent(out) :: pattern
        type(apropos_word_type), allocatable, intent(out) :: words(:)
        type(status_type), intent(out) :: status
        call read_string("Search for "//subject//" (word list or regexp): ", pattern, status)
        if (.not. status%ok()) return
        if (regexp_quote(pattern) == pattern) then
            ! Split into words.
            call split_words(pattern, words)
            if (size(words) == 0) call set_error(status, FM_USER_ERROR, "No word list given")
        end if
    end subroutine apropos_read_pattern

    !> The words of `s', separated by spaces and tabs.
    subroutine split_words(s, words)
        character(len=*), intent(in) :: s
        type(apropos_word_type), allocatable, intent(out) :: words(:)
        type(apropos_word_type), allocatable :: grown(:)
        integer :: i, j, n
        allocate (words(0))
        i = 1
        do while (i <= len(s))
            if (s(i:i) == " " .or. s(i:i) == TAB) then
                i = i + 1
                cycle
            end if
            j = i
            do while (j < len(s))
                if (s(j + 1:j + 1) == " " .or. s(j + 1:j + 1) == TAB) exit
                j = j + 1
            end do
            n = size(words)
            allocate (grown(n + 1))
            grown(1:n) = words
            grown(n + 1)%s = s(i:j)
            call move_alloc(grown, words)
            i = j + 1
        end do
    end subroutine split_words

    !> Rewrite a list of words to a regexp matching all permutations.
    !> If WORDS is not allocated, PATTERN is already a regexp.
    !> MULTILINE_P, if true, means produce a regexp that will match the
    !> words even if separated by newlines.  This updates
    !> `apropos_pattern', `apropos_pattern_quoted', `apropos_regexp',
    !> `apropos_words' and `apropos_all_words_regexp'.  Port of
    !> apropos.el:apropos-parse-pattern.
    subroutine apropos_parse_pattern(pattern, words, multiline_p)
        character(len=*), intent(in) :: pattern
        type(apropos_word_type), allocatable, intent(in) :: words(:)
        logical, intent(in) :: multiline_p
        type(apropos_word_type), allocatable :: syn(:)
        character(len=:), allocatable :: s, a
        integer :: i, k, n
        if (allocated(apropos_words)) deallocate (apropos_words)
        if (allocated(apropos_all_words)) deallocate (apropos_all_words)
        if (allocated(words)) then
            n = size(words)
            apropos_pattern = ""
            do i = 1, n
                if (i > 1) apropos_pattern = apropos_pattern//" "
                apropos_pattern = apropos_pattern//words(i)%s
            end do
            apropos_pattern_quoted = regexp_quote(apropos_pattern)
            ! The words are consed: the last one comes first.
            allocate (apropos_words(n), apropos_all_words(n))
            do i = 1, n
                s = words(i)%s
                a = words(i)%s
                do k = 1, size(SYNONYMS)
                    call split_words(trim(SYNONYMS(k)), syn)
                    if (.not. member(words(i)%s, syn, 1)) cycle
                    a = joined(syn)
                    if (member(words(i)%s, syn, 2)) s = a
                    exit
                end do
                apropos_words(n + 1 - i)%s = s
                apropos_all_words(n + 1 - i)%s = a
            end do
            ! The [^b-a] trick matches any character including a newline.
            if (multiline_p) then
                apropos_all_words_regexp = apropos_words_to_regexp(apropos_all_words, "[^b-a]+?")
                apropos_regexp = apropos_words_to_regexp(apropos_words, "[^b-a]*?")
            else
                apropos_all_words_regexp = apropos_words_to_regexp(apropos_all_words, ".+")
                apropos_regexp = apropos_words_to_regexp(apropos_words, ".*?")
            end if
        else
            apropos_pattern_quoted = regexp_quote(pattern)
            apropos_all_words_regexp = pattern
            apropos_pattern = pattern
            apropos_regexp = pattern
        end if
    contains

        logical function member(w, list, from)
            character(len=*), intent(in) :: w
            type(apropos_word_type), intent(in) :: list(:)
            integer, intent(in) :: from
            integer :: m
            member = .false.
            do m = from, size(list)
                if (list(m)%s == w) member = .true.
            end do
        end function member

        function joined(list) result(r)
            type(apropos_word_type), intent(in) :: list(:)
            character(len=:), allocatable :: r
            integer :: m
            r = ""
            do m = 1, size(list)
                if (m > 1) r = r//"\|"
                r = r//list(m)%s
            end do
        end function joined
    end subroutine apropos_parse_pattern

    !> Return apropos scores for string STR matching WORDS: the offsets
    !> (from 0) of the words found in the string; without words, the start
    !> and end of the match of `apropos_pattern'.  Port of
    !> apropos.el:apropos-calc-scores.
    subroutine apropos_calc_scores(str, words, have_words, scores)
        character(len=*), intent(in) :: str
        type(apropos_word_type), intent(in) :: words(:)
        logical, intent(in) :: have_words
        integer(int64), allocatable, intent(out) :: scores(:)
        type(status_type) :: st
        integer(int64) :: i
        integer :: k
        allocate (scores(0))
        if (have_words .and. size(words) > 0) then
            do k = 1, size(words)
                i = string_match(words(k)%s, str, st, inhibit_modify=.true.)
                if (st%ok() .and. i > 0) scores = [i - 1, scores]
            end do
        else
            i = string_match(apropos_pattern, str, st)
            if (st%ok() .and. i > 0) scores = [i - 1, match_end(0, st) - 1]
        end if
    end subroutine apropos_calc_scores

    !> Return true if STR is a genuine hit: it matches at least two of
    !> WORDS (unless only one was given).  Port of
    !> apropos.el:apropos-true-hit.
    logical function apropos_true_hit(str, words) result(hit)
        character(len=*), intent(in) :: str
        type(apropos_word_type), allocatable, intent(in) :: words(:)
        integer(int64), allocatable :: scores(:)
        hit = .true.
        if (.not. allocated(words)) return
        if (size(words) <= 1) return
        call apropos_calc_scores(str, words, .true., scores)
        hit = size(scores) > 1
    end function apropos_true_hit

    !> Return apropos score for string DOC.  Port of
    !> apropos.el:apropos-score-doc.
    integer(int64) function apropos_score_doc(doc) result(score)
        character(len=*), intent(in) :: doc
        integer(int64), allocatable :: scores(:)
        type(status_type) :: st
        integer(int64) :: l
        integer :: k
        score = 0
        l = len(doc)
        if (l <= 0) return
        if (string_match(apropos_pattern_quoted, doc, st, inhibit_modify=.true.) > 0) score = 10000
        call apropos_calc_scores(doc, apropos_all_words, allocated(apropos_all_words), scores)
        do k = 1, size(scores)
            score = score + 50 + ((l - scores(k))*50)/l
        end do
    end function apropos_score_doc

    !> Return apropos score for SYMBOL (a name), times WEIGHT.  Port of
    !> apropos.el:apropos-score-symbol.
    integer(int64) function apropos_score_symbol(name, weight) result(score)
        character(len=*), intent(in) :: name
        integer, intent(in) :: weight
        integer(int64), allocatable :: scores(:)
        integer(int64) :: l
        integer :: k
        score = 0
        l = len(name)
        call apropos_calc_scores(name, apropos_words, allocated(apropos_words), scores)
        do k = 1, size(scores)
            score = score + (60 - l) + ((l - scores(k))*60)/l
        end do
        score = score*weight
    end function apropos_score_symbol

    ! ------------------------------------------------------------------
    ! Apropos mode
    ! ------------------------------------------------------------------

    !> Major mode for following hyperlinks in output of apropos commands.
    !>
    !> \{apropos_mode_map}  Port of apropos.el:apropos-mode.
    recursive subroutine apropos_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        call major_modes%apropos_mode%begin(saved, status)
        if (.not. status%ok()) return
        call major_modes%apropos_mode%finish(saved, status)
    end subroutine apropos_mode

    !> The apropos button at `pos': its type (`apropos_symbol' or a
    !> label's type, "" for none) and the entry's name.
    subroutine button_at(pos, type, name)
        integer(int64), intent(in) :: pos
        character(len=:), allocatable, intent(out) :: type, name
        type(value_type) :: v
        type(status_type) :: st
        type = ""
        name = ""
        call get_text_property(pos, sym(P_BUTTON), v, st)
        if (nilp(v)) return
        type = symbols_name(v)
        call get_text_property(pos, sym(P_NAME), v, st)
        if (stringp(v)) name = xstring(v)
    end subroutine button_at

    function symbols_name(v) result(s)
        use data_fm, only: symbol_name
        type(value_type), intent(in) :: v
        character(len=:), allocatable :: s
        type(status_type) :: st
        call symbol_name(v, s, st)
        if (.not. st%ok()) s = ""
    end function symbols_name

    !> Whether the line at point starts with an entry's name.
    logical function at_symbol_p()
        character(len=:), allocatable :: type, name
        call button_at(buffers(cur)%pt, type, name)
        at_symbol_p = type == "apropos_symbol"
    end function at_symbol_p

    !> Move cursor down to the next symbol in an `apropos_mode' buffer.
    !> Port of apropos.el:apropos-next-symbol.
    subroutine apropos_next_symbol(status)
        type(status_type), intent(out) :: status
        integer(int64) :: shortage
        status = status_type()
        shortage = forward_line(1_int64)
        do while (.not. at_symbol_p() .and. buffers(cur)%pt < point_max())
            shortage = forward_line(1_int64)
        end do
    end subroutine apropos_next_symbol

    !> Move cursor back to the last symbol in an `apropos_mode' buffer.
    !> Port of apropos.el:apropos-previous-symbol.
    subroutine apropos_previous_symbol(status)
        type(status_type), intent(out) :: status
        integer(int64) :: shortage
        status = status_type()
        shortage = forward_line(-1_int64)
        do while (.not. at_symbol_p() .and. buffers(cur)%pt > point_min())
            shortage = forward_line(-1_int64)
        end do
    end subroutine apropos_previous_symbol

    ! ------------------------------------------------------------------
    ! Interactive commands
    ! ------------------------------------------------------------------

    !> Show commands (interactively callable functions) that match PATTERN.
    !> PATTERN can be a word, a list of words (separated by spaces),
    !> or a regexp (using some regexp special characters).  If it is a word,
    !> search for matches for that word as a substring.  If it is a list of words,
    !> search for matches for any two (or more) of those words.
    !>
    !> With \[universal_argument] prefix, also show noninteractive
    !> functions.  Port of apropos.el:apropos-command (the command).
    recursive subroutine cmd_apropos_command(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pattern
        type(apropos_word_type), allocatable :: words(:)
        logical :: do_all
        do_all = .not. nilp(current_prefix_arg)
        if (do_all) then
            call apropos_read_pattern("command or function", pattern, words, status)
        else
            call apropos_read_pattern("command", pattern, words, status)
        end if
        if (.not. status%ok()) return
        call apropos_command(pattern, words, do_all, VAR_NONE, status)
    end subroutine cmd_apropos_command

    !> Show functions that match PATTERN.
    !>
    !> PATTERN can be a word, a list of words (separated by spaces),
    !> or a regexp (using some regexp special characters).  If it is a word,
    !> search for matches for that word as a substring.  If it is a list of words,
    !> search for matches for any two (or more) of those words.
    !>
    !> This is the same as running `apropos_command' with a
    !> \[universal_argument] prefix.  Port of apropos.el:apropos-function.
    recursive subroutine cmd_apropos_function(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pattern
        type(apropos_word_type), allocatable :: words(:)
        call apropos_read_pattern("function", pattern, words, status)
        if (.not. status%ok()) return
        call apropos_command(pattern, words, .true., VAR_NONE, status)
    end subroutine cmd_apropos_function

    !> Show user options that match PATTERN.
    !> PATTERN can be a word, a list of words (separated by spaces),
    !> or a regexp (using some regexp special characters).  If it is a word,
    !> search for matches for that word as a substring.  If it is a list of words,
    !> search for matches for any two (or more) of those words.
    !>
    !> With \[universal_argument] prefix, also show variables, not just
    !> user options.  Port of apropos.el:apropos-user-option.
    recursive subroutine cmd_apropos_user_option(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pattern
        type(apropos_word_type), allocatable :: words(:)
        if (.not. nilp(current_prefix_arg)) then
            call apropos_read_pattern("variable", pattern, words, status)
            if (.not. status%ok()) return
            call apropos_command(pattern, words, .true., VAR_ANY, status)
        else
            call apropos_read_pattern("user option", pattern, words, status)
            if (.not. status%ok()) return
            call apropos_command(pattern, words, .false., VAR_USER_OPTION, status)
        end if
    end subroutine cmd_apropos_user_option

    !> Show variables that match PATTERN.
    !> With the prefix \[universal_argument], show user options only,
    !> i.e. behave like `apropos_user_option'.  Port of
    !> apropos.el:apropos-variable.
    recursive subroutine cmd_apropos_variable(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pattern
        type(apropos_word_type), allocatable :: words(:)
        if (.not. nilp(current_prefix_arg)) then
            call apropos_read_pattern("user option", pattern, words, status)
            if (.not. status%ok()) return
            call apropos_command(pattern, words, .false., VAR_USER_OPTION, status)
        else
            call apropos_read_pattern("variable", pattern, words, status)
            if (.not. status%ok()) return
            call apropos_command(pattern, words, .true., VAR_ANY, status)
        end if
    end subroutine cmd_apropos_variable

    !> Show symbols whose documentation contains matches for PATTERN.
    !> PATTERN can be a word, a list of words (separated by spaces),
    !> or a regexp (using some regexp special characters).  If it is a word,
    !> search for matches for that word as a substring.  If it is a list of words,
    !> search for matches for any two (or more) of those words.
    !>
    !> Returns list of symbols and documentation found.  Port of
    !> apropos.el:apropos-documentation (the command).
    recursive subroutine cmd_apropos_documentation(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pattern
        type(apropos_word_type), allocatable :: words(:)
        call apropos_read_pattern("documentation", pattern, words, status)
        if (.not. status%ok()) return
        call apropos_documentation(pattern, words, status)
    end subroutine cmd_apropos_documentation

    !> Define Apropos mode and its keymap; register the commands.
    subroutine syms_of_apropos()
        type(status_type) :: st
        call major_modes%apropos_mode%define("apropos_mode", "Apropos", apropos_mode, apropos_mode_hook, &
                                             parent=major_modes%special_mode)
        apropos_mode_map = major_modes%apropos_mode%map
        ! Use `apropos_follow' instead of just using the button definition
        ! of RET, so that users can use it anywhere in an apropos item.
        call keymap_set(apropos_mode_map, "C-m", "apropos_follow", st)
        ! Movement keys.
        call keymap_set(apropos_mode_map, "n", "apropos_next_symbol", st)
        call keymap_set(apropos_mode_map, "p", "apropos_previous_symbol", st)
        call register_keymap_name("apropos_mode_map", apropos_mode_map)
        commands%apropos_mode%run => apropos_mode
        commands%apropos_follow%run => apropos_follow
        commands%apropos_next_symbol%run => apropos_next_symbol
        commands%apropos_previous_symbol%run => apropos_previous_symbol
        commands%apropos_command%run => cmd_apropos_command
        commands%apropos_function%run => cmd_apropos_function
        commands%apropos_user_option%run => cmd_apropos_user_option
        commands%apropos_variable%run => cmd_apropos_variable
        commands%apropos_documentation%run => cmd_apropos_documentation
    end subroutine syms_of_apropos
end module apropos_fm
