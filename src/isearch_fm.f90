! isearch_fm.f90 -- incremental search minor mode.
! Ported from GNU Emacs 31.1 lisp/isearch.el.
!
! Copyright (C) 1992-1997, 1999-2026 Free Software Foundation, Inc.
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

!> Incremental search: C-s and C-r.  As upstream, `isearch_mode' makes
!> `isearch_mode_map' the overriding terminal-local map and installs a
!> pre-command hook; keys bound there extend or move the search, and any
!> other command ends the search first and then runs as usual.
!>
!> Literal and regexp search (C-M-s, C-M-r, M-r), with lax whitespace
!> matching (M-s SPC).  Not ported yet: word, symbol and char-fold search
!> (`isearch_regexp_function', `search_default_mode'),
!> highlighting the match and lazy highlighting (faces from overlays),
!> C-M-w (symbol syntax: forward-symbol), reading a kill with the
!> minibuffer (`isearch_yank_from_kill_ring', C-u M-y), search through invisible text, scrolling during search
!> (`isearch_allow_scroll'), `isearch_allow_motion', query-replace and
!> occur from isearch, the help map, input methods, completion, the
!> minibuffer's own isearch keys while editing the string.
module isearch_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, FM_QUIT, FM_USER_ERROR, FM_INVALID_REGEXP, FM_SEARCH_FAILED, &
                        error_message
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, make_fixnum, fixnump, xfixnum, stringp, xstring, make_keymap_ref, &
                       list_length, list_elt, list_elements, make_list, nilp
    use lread_fm, only: sym
    use data_fm, only: value_eq, symbol_value, set
    use character_fm, only: string_char_and_length, char_string, MAX_CHAR, MAX_MULTIBYTE_LENGTH, &
                            CHAR_SHIFT
    use casetab_fm, only: downcase
    use buffer_fm, only: buffers, cur, transient_mark_mode, buf_fetch_char
    use marker_fm, only: char_to_byte
    use syntax_fm, only: char_syntax, forward_word
    use editfns_fm, only: point, goto_char, point_min, point_max, bobp, eobp, eolp, pos_eol, &
                          buffer_substring
    use cmds_fm, only: forward_char
    use search_fm, only: re_search_forward, re_search_backward, match_beginning, match_end, looking_at, &
                         string_match, regexp_quote, case_fold_search, search_spaces_regexp, SEARCH_NOERROR
    use keymap_fm, only: make_keymap, make_sparse_keymap, define_key, define_key_range, key_parse, &
                         lookup_key, keymap_live_p, free_keymap, overriding_terminal_local_map
    use keyboard_fm, only: this_command, last_command, last_command_event, this_command_keys, &
                           pre_command_hook, redisplay_proc, discard_input, input_pending_p, &
                           read_event, sit_for
    use callint_fm, only: commandp, current_prefix_arg, prefix_numeric_value
    use subr_fm, only: add_to_history, history_delete_duplicates, defvar_nil
    use minibuf_fm, only: read_from_minibuffer, history_add_new_input, minibuf_level
    use xdisp_fm, only: message, message_nolog
    use dispnew_fm, only: ding
    use simple_fm, only: push_mark, current_kill
    implicit none(type, external)
    private

    public :: isearch_forward_command, isearch_backward_command, isearch_mode, isearch_done, &
              isearch_update, isearch_exit, isearch_cancel, isearch_abort, &
              isearch_repeat_forward, isearch_repeat_backward, isearch_delete_char, &
              isearch_del_char, isearch_printing_char, isearch_process_search_string, &
              isearch_yank_string, isearch_yank_char, isearch_yank_word_or_char, isearch_yank_line, &
              isearch_yank_kill, isearch_yank_pop_only, &
              isearch_beginning_of_buffer, isearch_end_of_buffer, isearch_ring_advance, &
              isearch_ring_retreat, isearch_toggle_case_fold, isearch_edit_string, &
              isearch_message_prefix, syms_of_isearch, isearch_forward_regexp_command, &
              isearch_backward_regexp_command, isearch_toggle_regexp, isearch_toggle_lax_whitespace, &
              isearch_no_upper_case_p, word_search_regexp

    !> Values of `isearch_case_fold_search': nil, t, and `yes' (set by
    !> M-c: fold even when the string has upper-case letters).
    integer, parameter, public :: CASE_FOLD_NIL = 0, CASE_FOLD_T = 1, CASE_FOLD_YES = 2

    !> Values of `search_upper_case': nil, t and `not_yanks'.
    integer, parameter, public :: UPPER_CASE_NIL = 0, UPPER_CASE_T = 1, UPPER_CASE_NOT_YANKS = 2

    !> If not UPPER_CASE_NIL, upper case chars disable case fold searching.
    !> That is, upper and lower case chars must match exactly.
    !> This applies no matter where the chars come from, but does not
    !> apply to chars in regexps that are prefixed with `\'.
    !> If this value is UPPER_CASE_NOT_YANKS, text yanked into the search string
    !> in Isearch mode is always downcased.  Port of isearch.el
    !> search-upper-case.
    integer, public :: search_upper_case = UPPER_CASE_NOT_YANKS

    !> True means random control characters terminate incremental
    !> search.  (Only true and false: `edit', `append', `move' and
    !> `shift_move' are not ported.)  Port of isearch.el search-exit-option.
    logical, public :: search_exit_option = .true.

    !> If true, do a nonincremental search instead of an incremental
    !> one.  This applies to RET typed immediately after entering
    !> incremental search.  Port of isearch.el
    !> search-nonincremental-instead.
    logical, public :: search_nonincremental_instead = .true.

    !> Maximum length of search ring before oldest elements are thrown
    !> away.  Port of isearch.el search-ring-max.
    integer, public :: search_ring_max = 16

    !> Maximum length of regexp search ring before oldest elements are
    !> thrown away.  Port of isearch.el regexp-search-ring-max.
    integer, public :: regexp_search_ring_max = 16

    !> If allocated, regular expression to match a sequence of whitespace
    !> chars.  When you enter a space or spaces in the incremental search,
    !> it will match any sequence matched by this regexp.  As an
    !> exception, spaces are treated normally in regexp incremental search
    !> if they occur in a regexp construct like [...] or *, + or ?.
    !>
    !> If the value is a string, it applies to both ordinary and regexp
    !> incremental search.  If the value is unallocated, or
    !> `isearch_lax_whitespace' is false for ordinary incremental search, or
    !> `isearch_regexp_lax_whitespace' is false for regexp incremental
    !> search, then each space you type matches literally, against one
    !> space.  Port of isearch.el search-whitespace-regexp (made "[ \t]+"
    !> by syms_of_isearch).
    character(len=:), allocatable, public :: search_whitespace_regexp

    !> If true, a space will match a sequence of whitespace chars.
    !> When you enter a space or spaces in ordinary incremental search, it
    !> will match any sequence matched by the regexp defined by the
    !> variable `search_whitespace_regexp'.  If the value is false, each
    !> space you type matches literally, against one space.  You can
    !> toggle the value of this variable by the command
    !> `isearch_toggle_lax_whitespace', usually bound to `M-s SPC' during
    !> isearch.  Port of isearch.el isearch-lax-whitespace.
    logical, public :: isearch_lax_whitespace = .true.

    !> If true, a space will match a sequence of whitespace chars.
    !> When you enter a space or spaces in regexp incremental search, it
    !> will match any sequence matched by the regexp defined by the
    !> variable `search_whitespace_regexp'.  If the value is false, each
    !> space you type matches literally, against one space.  You can
    !> toggle the value of this variable by the command
    !> `isearch_toggle_lax_whitespace', usually bound to `M-s SPC' during
    !> isearch.  Port of isearch.el isearch-regexp-lax-whitespace.
    logical, public :: isearch_regexp_lax_whitespace = .false.

    !> isearch.el isearch-mode: whether Isearch mode is on.
    logical, public, protected :: isearch_active = .false.

    !> Keymap for `isearch_mode'.  Port of isearch.el isearch-mode-map.
    type(handle_type), public, protected :: isearch_mode_map

    !> The search state, as isearch.el's global variables.  Strings are
    !> UTF-8; an unallocated `isearch_error' is nil; position 0 is nil.
    character(len=:), allocatable, public, protected :: isearch_string, isearch_message
    character(len=:), allocatable, public, protected :: isearch_error
    logical, public, protected :: isearch_forward = .true., isearch_success = .true.
    logical, public, protected :: isearch_wrapped = .false.
    !> Searching for a regexp (isearch-regexp).
    logical, public, protected :: isearch_regexp = .false.
    integer(int64), public, protected :: isearch_other_end = 0, isearch_opoint = 0, isearch_barrier = 0
    integer, public, protected :: isearch_case_fold_search = CASE_FOLD_T
    integer :: isearch_last_case_fold_search = CASE_FOLD_T
    !> isearch-adjusted: nil, t (shows "pending"), or a toggle.
    integer, parameter :: ADJUSTED_NIL = 0, ADJUSTED_T = 1, ADJUSTED_TOGGLE = 2
    integer :: isearch_adjusted = ADJUSTED_NIL
    logical :: isearch_yank_flag = .false.
    logical :: isearch_just_started = .false., isearch_nonincremental = .false.
    integer(int64) :: isearch_yank_prev_point = 0

    !> isearch.el search-ring-yank-pointer and
    !> regexp-search-ring-yank-pointer: 0-based, -1 for nil.
    integer :: search_ring_yank_pointer = -1, regexp_search_ring_yank_pointer = -1

    !> One element of isearch-cmds (isearch.el isearch--state).
    type :: isearch_state
        character(len=:), allocatable :: string, message, error
        integer(int64) :: point = 0, other_end = 0, barrier = 0
        logical :: success = .true., forward = .true., wrapped = .false.
        integer :: case_fold = CASE_FOLD_T
    end type isearch_state

    !> isearch-cmds: the states, newest last.
    type(isearch_state), allocatable :: cmds(:)
    integer :: ncmds = 0

    !> isearch--saved-local-map and isearch--saved-overriding-local-map.
    type(handle_type) :: saved_local_map, saved_overriding_local_map
    !> The pre-command hook isearch-mode replaced.
    procedure(redisplay_proc), pointer :: saved_pre_command_hook => null()

contains

    ! ------------------------------------------------------------------
    ! Strings
    ! ------------------------------------------------------------------

    !> The characters of UTF-8 `s'.
    subroutine decode(s, chars, n)
        character(len=*), intent(in) :: s
        integer(int32), allocatable, intent(out) :: chars(:)
        integer, intent(out) :: n
        integer(int64) :: i
        integer(int32) :: c, clen
        allocate (chars(len(s)))
        n = 0
        i = 1
        do while (i <= len(s))
            call string_char_and_length(s, i, c, clen)
            n = n + 1
            chars(n) = c
            i = i + clen
        end do
    end subroutine decode

    !> The UTF-8 encoding of character `c'.
    function encode(c) result(s)
        integer(int32), intent(in) :: c
        character(len=:), allocatable :: s
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        integer(int32) :: nbytes
        type(status_type) :: status
        call char_string(c, buf, nbytes, status)
        s = buf(1:nbytes)
    end function encode

    !> Number of characters in UTF-8 `s'.
    integer function nchars(s)
        character(len=*), intent(in) :: s
        integer(int32), allocatable :: chars(:)
        call decode(s, chars, nchars)
    end function nchars

    !> `s' without its last `n' characters.
    function drop_chars(s, n) result(r)
        character(len=*), intent(in) :: s
        integer, intent(in) :: n
        character(len=:), allocatable :: r
        integer(int32), allocatable :: chars(:)
        integer :: k, m
        call decode(s, chars, m)
        r = ""
        do k = 1, m - n
            r = r//encode(chars(k))
        end do
    end function drop_chars

    !> Port of casefiddle.c:Fdowncase for a string (character by
    !> character through the case table).
    function downcase_string(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer(int32), allocatable :: chars(:)
        integer :: k, m
        call decode(s, chars, m)
        r = ""
        do k = 1, m
            r = r//encode(downcase(chars(k)))
        end do
    end function downcase_string

    !> Return true if there are no upper case chars in STRING.
    !> If REGEXP-FLAG is true, disregard letters preceded by `\' (but
    !> not `\\') since they have special meaning in a regexp.  Port of
    !> isearch.el:isearch-no-upper-case-p.
    logical function isearch_no_upper_case_p(string, regexp_flag) result(none)
        character(len=*), intent(in) :: string
        logical, intent(in) :: regexp_flag
        integer(int32), allocatable :: chars(:)
        integer :: k, m, at
        logical :: quote_flag
        type(status_type) :: status
        integer(int64) :: i
        call decode(string, chars, m)
        none = .true.
        quote_flag = .false.
        do k = 1, m
            if (regexp_flag .and. chars(k) == iachar("\")) then
                quote_flag = .not. quote_flag
            else
                if (.not. quote_flag .and. downcase(chars(k)) /= chars(k)) then
                    none = .false.
                    return
                end if
                quote_flag = .false.
            end if
        end do
        ! Even if there's no uppercase char, we want to detect the use of
        ! [:upper:] or [:lower:] char-class, which indicates clearly that
        ! the user cares about case distinction.
        if (regexp_flag) then
            at = index(string, "[:upper:]")
            if (at == 0) at = index(string, "[:lower:]")
            if (at > 0) then
                i = string_match(string(1:at - 1), "", status, inhibit_modify=.true.)
                if (status%code == FM_INVALID_REGEXP) then
                    if (status%detail == """Unmatched [ or [^""") none = .false.
                end if
            end if
        end if
    end function isearch_no_upper_case_p

    !> Return a regexp which matches words, ignoring punctuation.
    !> Given STRING, a string of words separated by word delimiters,
    !> compute a regexp that matches those exact words separated by
    !> arbitrary punctuation.  If the string begins or ends in whitespace,
    !> the beginning or the end of the string matches arbitrary whitespace.
    !> Otherwise if LAX is non-nil, the beginning or the end of the string
    !> need not match a word boundary.
    !>
    !> Used in `word_search_forward', `word_search_backward',
    !> `word_search_forward_lax', `word_search_backward_lax'.  Port of
    !> isearch.el:word-search-regexp.
    function word_search_regexp(string, lax) result(regexp)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: lax
        character(len=:), allocatable :: regexp, word
        integer(int32), allocatable :: chars(:)
        integer :: k, m, nwords
        logical :: is_lax
        is_lax = .false.
        if (present(lax)) is_lax = lax
        regexp = ""
        if (len(string) == 0) return
        call decode(string, chars, m)
        if (.not. any([(char_syntax(int(chars(k))) == "w", k=1, m)])) then
            regexp = "\W+"
            return
        end if
        if (char_syntax(int(chars(1))) /= "w") then
            regexp = "\W+"
        else
            regexp = "\<"
        end if
        ! (mapconcat 'regexp-quote (split-string string "\\W+" t) "\\W+")
        nwords = 0
        word = ""
        do k = 1, m + 1
            if (k <= m) then
                if (char_syntax(int(chars(k))) == "w") then
                    word = word//encode(chars(k))
                    cycle
                end if
            end if
            if (len(word) > 0) then
                if (nwords > 0) regexp = regexp//"\W+"
                regexp = regexp//regexp_quote(word)
                nwords = nwords + 1
                word = ""
            end if
        end do
        if (char_syntax(int(chars(m))) /= "w") then
            regexp = regexp//"\W+"
        else if (.not. is_lax) then
            regexp = regexp//"\>"
        end if
    end function word_search_regexp

    ! ------------------------------------------------------------------
    ! The search ring
    ! ------------------------------------------------------------------

    !> The name of the search ring for regexp search if `regexp', else of
    !> the ordinary search ring.
    function ring_name(regexp) result(name)
        logical, intent(in) :: regexp
        character(len=:), allocatable :: name
        if (regexp) then
            name = "regexp_search_ring"
        else
            name = "search_ring"
        end if
    end function ring_name

    !> The search ring (isearch.el search-ring or regexp-search-ring, as
    !> for the current search), most recent first.
    subroutine search_ring(elems)
        type(value_type), allocatable, intent(out) :: elems(:)
        type(value_type) :: ring
        type(status_type) :: status
        call symbol_value(sym(ring_name(isearch_regexp)), ring, status)
        if (.not. status%ok()) ring = nil()
        call list_elements(ring, elems)
    end subroutine search_ring

    !> Add STRING to the beginning of the search ring.
    !> REGEXP if non-nil says use the regexp search ring.  Port of
    !> isearch.el:isearch-update-ring (no text properties on STRING).
    subroutine isearch_update_ring(string, regexp)
        character(len=*), intent(in) :: string
        logical, intent(in) :: regexp
        logical :: saved
        saved = history_delete_duplicates
        history_delete_duplicates = .true.
        call add_to_history(ring_name(regexp), string, merge(regexp_search_ring_max, search_ring_max, regexp), &
                            keep_all=.true.)
        history_delete_duplicates = saved
    end subroutine isearch_update_ring

    !> Port of isearch.el:isearch-ring-adjust1.
    subroutine isearch_ring_adjust1(advance)
        logical, intent(in) :: advance
        type(value_type), allocatable :: ring(:)
        integer :: length, yank_pointer
        call search_ring(ring)
        length = size(ring)
        if (length == 0) return
        if (isearch_regexp) then
            yank_pointer = regexp_search_ring_yank_pointer
        else
            yank_pointer = search_ring_yank_pointer
        end if
        if (yank_pointer < 0) yank_pointer = merge(0, -1, advance)
        yank_pointer = modulo(yank_pointer + merge(-1, 1, advance), length)
        if (isearch_regexp) then
            regexp_search_ring_yank_pointer = yank_pointer
        else
            search_ring_yank_pointer = yank_pointer
        end if
        if (stringp(ring(yank_pointer + 1))) then
            isearch_string = xstring(ring(yank_pointer + 1))
            isearch_message = isearch_string
        end if
    end subroutine isearch_ring_adjust1

    !> Port of isearch.el:isearch-ring-adjust (search-ring-update nil:
    !> edit the string in the minibuffer).
    subroutine isearch_ring_adjust(advance, status)
        logical, intent(in) :: advance
        type(status_type), intent(out) :: status
        call isearch_ring_adjust1(advance)
        ! Otherwise, edit the search string instead.
        call isearch_edit_string(status)
    end subroutine isearch_ring_adjust

    !> Advance to the next search string in the ring.  Port of
    !> isearch.el:isearch-ring-advance.
    subroutine isearch_ring_advance(status)
        type(status_type), intent(out) :: status
        call isearch_ring_adjust(.true., status)
    end subroutine isearch_ring_advance

    !> Retreat to the previous search string in the ring.  Port of
    !> isearch.el:isearch-ring-retreat.
    subroutine isearch_ring_retreat(status)
        type(status_type), intent(out) :: status
        call isearch_ring_adjust(.false., status)
    end subroutine isearch_ring_retreat

    ! ------------------------------------------------------------------
    ! States
    ! ------------------------------------------------------------------

    !> Port of isearch.el:isearch--get-state.
    function get_state() result(s)
        type(isearch_state) :: s
        s%string = isearch_string
        s%message = isearch_message
        s%point = point()
        s%success = isearch_success
        s%forward = isearch_forward
        s%other_end = isearch_other_end
        if (allocated(isearch_error)) s%error = isearch_error
        s%wrapped = isearch_wrapped
        s%barrier = isearch_barrier
        s%case_fold = isearch_case_fold_search
    end function get_state

    !> Port of isearch.el:isearch--set-state.
    subroutine set_state(s)
        type(isearch_state), intent(in) :: s
        isearch_string = s%string
        isearch_message = s%message
        isearch_success = s%success
        isearch_forward = s%forward
        isearch_other_end = s%other_end
        if (allocated(isearch_error)) deallocate (isearch_error)
        if (allocated(s%error)) isearch_error = s%error
        isearch_wrapped = s%wrapped
        isearch_barrier = s%barrier
        isearch_case_fold_search = s%case_fold
        call goto_char(s%point)
    end subroutine set_state

    logical function same_state(a, b)
        type(isearch_state), intent(in) :: a, b
        same_state = a%string == b%string .and. len(a%string) == len(b%string) .and. &
                     a%message == b%message .and. len(a%message) == len(b%message) .and. &
                     a%point == b%point .and. (a%success .eqv. b%success) .and. &
                     (a%forward .eqv. b%forward) .and. a%other_end == b%other_end .and. &
                     (allocated(a%error) .eqv. allocated(b%error)) .and. &
                     (a%wrapped .eqv. b%wrapped) .and. a%barrier == b%barrier .and. &
                     a%case_fold == b%case_fold
    end function same_state

    !> Port of isearch.el:isearch-push-state.
    subroutine isearch_push_state()
        type(isearch_state), allocatable :: grown(:)
        integer :: k
        if (.not. allocated(cmds)) allocate (cmds(16))
        if (ncmds == size(cmds)) then
            allocate (grown(2*size(cmds)))
            do k = 1, ncmds
                grown(k) = cmds(k)
            end do
            call move_alloc(grown, cmds)
        end if
        ncmds = ncmds + 1
        cmds(ncmds) = get_state()
    end subroutine isearch_push_state

    !> Port of isearch.el:isearch-pop-state.
    subroutine isearch_pop_state()
        ncmds = ncmds - 1
        if (ncmds > 0) call set_state(cmds(ncmds))
    end subroutine isearch_pop_state

    ! ------------------------------------------------------------------
    ! Starting and ending
    ! ------------------------------------------------------------------

    !> Do incremental search forward.
    !> With a prefix argument, do an incremental regular expression
    !> search instead.
    !> As you type characters, they add to the search string and are
    !> found.  Type DEL to cancel last input item from end of search
    !> string.  Type RET to exit, leaving point at location found.  Type
    !> LFD (C-j) to match end of line.  Type C-s to search again forward,
    !> C-r to search again backward.  Type C-q to quote control character
    !> to search for it.  Type M-c to toggle search case-sensitivity.
    !> Type M-e to edit the search string in the minibuffer.  Other
    !> control and meta characters terminate the search and are then
    !> executed normally.  C-g while searching or when search has failed
    !> cancels input back to what has been found successfully.  C-g when
    !> search is successful aborts and moves point to starting point.
    !> Port of isearch.el:isearch-forward.
    subroutine isearch_forward_command(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_mode(.true.,.not. nilp(current_prefix_arg))
    end subroutine isearch_forward_command

    !> Do incremental search backward.
    !> With a prefix argument, do a regular expression search instead.
    !> See the command `isearch_forward' for more information.  Port of
    !> isearch.el:isearch-backward.
    subroutine isearch_backward_command(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_mode(.false.,.not. nilp(current_prefix_arg))
    end subroutine isearch_backward_command

    !> Do incremental search forward for regular expression.
    !> With a prefix argument, do a regular string search instead.
    !> Like ordinary incremental search except that your input is treated
    !> as a regexp.  See the command `isearch_forward' for more
    !> information.
    !>
    !> In incremental searches, a space or spaces normally matches any
    !> whitespace defined by the variable `search_whitespace_regexp'.  To
    !> search for a literal space and nothing else, enter C-q SPC.  To
    !> toggle whitespace matching, use `isearch_toggle_lax_whitespace',
    !> usually bound to `M-s SPC' during isearch.  This command does not
    !> support character folding.  Port of
    !> isearch.el:isearch-forward-regexp.
    subroutine isearch_forward_regexp_command(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_mode(.true., nilp(current_prefix_arg))
    end subroutine isearch_forward_regexp_command

    !> Do incremental search backward for regular expression.
    !> With a prefix argument, do a regular string search instead.
    !> Like ordinary incremental search except that your input is treated
    !> as a regexp.  See the command `isearch_forward_regexp' for more
    !> information.  Port of isearch.el:isearch-backward-regexp.
    subroutine isearch_backward_regexp_command(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_mode(.false., nilp(current_prefix_arg))
    end subroutine isearch_backward_regexp_command

    !> Start Isearch minor mode.
    !> It is called by the function `isearch_forward' and other related
    !> functions.  The arg FORWARD means searching in the forward
    !> direction.  The non-nil arg REGEXP does an incremental regular
    !> expression search.  Port of isearch.el:isearch-mode (no OP-FUN,
    !> RECURSIVE-EDIT or REGEXP-FUNCTION).
    subroutine isearch_mode(forward, regexp)
        logical, intent(in) :: forward
        logical, intent(in), optional :: regexp
        ! Initialize global vars.
        isearch_forward = forward
        isearch_regexp = flag(regexp)
        isearch_last_case_fold_search = isearch_case_fold_search
        isearch_case_fold_search = merge(CASE_FOLD_T, CASE_FOLD_NIL, case_fold_search%value)
        isearch_string = ""
        isearch_message = ""
        ncmds = 0
        isearch_success = .true.
        isearch_wrapped = .false.
        isearch_barrier = point()
        isearch_adjusted = ADJUSTED_NIL
        isearch_yank_flag = .false.
        if (allocated(isearch_error)) deallocate (isearch_error)
        isearch_other_end = 0
        isearch_just_started = .true.
        isearch_opoint = point()
        search_ring_yank_pointer = -1
        regexp_search_ring_yank_pointer = -1
        isearch_active = .true.
        ! Prevent successive calls overriding original map in
        ! `isearch_done'.  (Bug#79368)
        if (.not. same_handle(overriding_terminal_local_map, isearch_mode_map)) then
            saved_local_map = overriding_terminal_local_map
        end if
        overriding_terminal_local_map = isearch_mode_map
        saved_overriding_local_map = overriding_terminal_local_map
        call isearch_push_state()
        call isearch_update()
        if (.not. associated(pre_command_hook, isearch_pre_command_hook)) then
            saved_pre_command_hook => pre_command_hook
            pre_command_hook => isearch_pre_command_hook
        end if
    end subroutine isearch_mode

    logical function same_handle(a, b)
        type(handle_type), intent(in) :: a, b
        same_handle = a%index == b%index .and. a%generation == b%generation
    end function same_handle

    !> This is called after every isearch command to update the display.
    !> Port of isearch.el:isearch-update (no slow-terminal window, hscroll
    !> or highlighting).
    subroutine isearch_update()
        if (.not. input_pending_p()) call isearch_message_show()
        isearch_adjusted = ADJUSTED_NIL
        isearch_yank_flag = .false.
    end subroutine isearch_update

    !> Exit Isearch mode.
    !> Called by all commands that terminate isearch-mode.
    !> For successful search, pass no args.
    !> For a failing search, NOPUSH is t.
    !> For going to the minibuffer to edit the search string,
    !> NOPUSH is t and EDIT is t.
    !> If NOPUSH is non-nil, we don't push the string on the search ring.
    !> Port of isearch.el:isearch-done (no intangible text or hooks).
    subroutine isearch_done(nopush, edit)
        logical, intent(in), optional :: nopush, edit
        if (associated(pre_command_hook, isearch_pre_command_hook)) then
            pre_command_hook => saved_pre_command_hook
            saved_pre_command_hook => null()
        end if
        overriding_terminal_local_map = saved_local_map
        saved_local_map = null_handle()
        isearch_active = .false.
        if (len(isearch_string) > 0 .and. .not. flag(nopush)) then
            ! Update the ring data.
            call isearch_update_ring(isearch_string, isearch_regexp)
        end if
        ! If there was movement, mark the starting position.  Maybe should
        ! test difference between and set mark only if > threshold.
        if (point() /= isearch_opoint) then
            if (.not. (transient_mark_mode%value .and. buffers(cur)%mark_active)) then
                call push_mark(isearch_opoint, nomsg=.true.)
                if (minibuf_level == 0 .and. .not. flag(edit)) call message("Mark saved where search started")
            end if
        end if
    end subroutine isearch_done

    logical function flag(x)
        logical, intent(in), optional :: x
        flag = .false.
        if (present(x)) flag = x
    end function flag

    !> Exit search normally.
    !> However, if this is the first command after starting incremental
    !> search and `search_nonincremental_instead' is true, do a
    !> nonincremental search instead via `isearch_edit_string'.  Port of
    !> isearch.el:isearch-exit.
    subroutine isearch_exit(status)
        type(status_type), intent(out) :: status
        logical :: saved
        status = status_type()
        if (search_nonincremental_instead .and. len(isearch_string) == 0) then
            saved = isearch_nonincremental
            isearch_nonincremental = .true.
            ! This calls isearch-done as well.
            call isearch_edit_string(status)
            isearch_nonincremental = saved
        else
            call isearch_done()
        end if
    end subroutine isearch_exit

    !> Terminate the search and go back to the starting point.  Port of
    !> isearch.el:isearch-cancel.
    subroutine isearch_cancel(status)
        type(status_type), intent(out) :: status
        call goto_char(isearch_opoint)
        ! Exit isearch..
        call isearch_done(nopush=.true.)
        ! ..and pass on quit signal.
        call set_error(status, FM_QUIT)
    end subroutine isearch_cancel

    !> Abort incremental search mode if searching is successful, signaling
    !> quit.  Otherwise, revert to previous successful search and continue
    !> searching.  Use `isearch_exit' to quit without signaling.  Port of
    !> isearch.el:isearch-abort.
    subroutine isearch_abort(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call discard_input()
        if (isearch_success .and. .not. allocated(isearch_error)) then
            ! If search is successful and has no incomplete regexp, move
            ! back to starting point and really do quit.
            isearch_success = .false.
            call isearch_cancel(status)
        else
            ! If search is failing, or has an incomplete regexp, rub out
            ! until it is once more successful.
            do while ((.not. isearch_success .or. allocated(isearch_error)) .and. ncmds > 1)
                call isearch_pop_state()
            end do
            call isearch_update()
        end if
    end subroutine isearch_abort

    !> Decide whether to exit Isearch mode before executing the command.
    !> Don't exit Isearch if the key sequence that invoked this command
    !> is bound in `isearch_mode_map', or if the invoked command is a
    !> prefix argument command.  Otherwise, exit Isearch (when
    !> `search_exit_option' is t) before the command is executed globally
    !> with terminated Isearch.  Port of isearch.el:isearch-pre-command-hook
    !> (isearch-allow-prefix t, no scrolling or motion).
    subroutine isearch_pre_command_hook()
        type(value_type) :: binding
        type(status_type) :: status
        ! Don't exit Isearch if we're in the middle of some
        ! `set_transient_map' thingy like `universal_argument__mode'.
        if (.not. same_handle(overriding_terminal_local_map, saved_overriding_local_map)) return
        ! Don't exit Isearch for isearch key bindings.
        if (allocated(this_command_keys)) then
            if (size(this_command_keys) > 0) then
                call lookup_key(make_keymap_ref(isearch_mode_map), this_command_keys, binding, status)
                if (status%ok() .and. .not. fixnump(binding) .and. commandp(binding)) return
            end if
        end if
        ! Handle a prefix argument.
        if (value_eq(this_command, sym("universal_argument")) .or. &
            value_eq(this_command, sym("universal_argument_more")) .or. &
            value_eq(this_command, sym("digit_argument")) .or. &
            value_eq(this_command, sym("negative_argument"))) return
        ! Other characters terminate the search and are then executed
        ! normally.
        if (search_exit_option) call isearch_done()
    end subroutine isearch_pre_command_hook

    ! ------------------------------------------------------------------
    ! Messages
    ! ------------------------------------------------------------------

    !> Generate and print the message string.  Port of
    !> isearch.el:isearch-message (no face on the failing part).
    subroutine isearch_message_show(c_q_hack, ellipsis)
        logical, intent(in), optional :: c_q_hack, ellipsis
        call message_nolog(isearch_message_string(flag(c_q_hack), flag(ellipsis)))
    end subroutine isearch_message_show

    function isearch_message_string(c_q_hack, ellipsis) result(m)
        logical, intent(in) :: c_q_hack
        logical, intent(in), optional :: ellipsis
        character(len=:), allocatable :: m
        m = isearch_message_prefix(isearch_nonincremental, flag(ellipsis))
        if (allocated(isearch_message)) m = m//isearch_text_description(isearch_message)
        m = m//isearch_message_suffix(c_q_hack)
    end function isearch_message_string

    !> Return position of first mismatch in search string, or -1 (nil) if
    !> none.  Port of isearch.el:isearch-fail-pos (MSG nil).
    integer(int64) function isearch_fail_pos() result(pos)
        integer :: k
        pos = -1
        if (isearch_success .and. .not. allocated(isearch_error)) return
        k = ncmds
        do while (k >= 1)
            if (cmds(k)%success .and. .not. allocated(cmds(k)%error)) exit
            k = k - 1
        end do
        pos = 0
        if (k < 1) return
        if (.not. allocated(cmds(k)%string) .or. .not. allocated(isearch_string)) return
        if (len(cmds(k)%string) < len(isearch_string)) then
            if (isearch_string(1:len(cmds(k)%string)) == cmds(k)%string) pos = schars_of(cmds(k)%string)
        end if
    end function isearch_fail_pos

    !> The number of characters of `s'.
    integer(int64) function schars_of(s)
        character(len=*), intent(in) :: s
        integer :: i
        schars_of = 0
        do i = 1, len(s)
            if (iand(iachar(s(i:i)), 192) /= 128) schars_of = schars_of + 1
        end do
    end function schars_of

    !> How `isearch_message' displays: each character as
    !> isearch-text-char-description shows it, control characters as ^C
    !> and DEL as ^? (upstream puts these on the characters as `display'
    !> properties).  Port of isearch.el:isearch-text-char-description.
    function isearch_text_description(text) result(shown)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: shown
        integer :: i, c
        shown = ""
        do i = 1, len(text)
            c = iachar(text(i:i))
            if (c < 32) then
                shown = shown//"^"//achar(c + 64)
            else if (c == 127) then
                shown = shown//"^?"
            else
                shown = shown//text(i:i)
            end if
        end do
    end function isearch_text_description

    !> The prompt before the search string ("I-search: ", "Failing
    !> wrapped I-search backward: ", ...).  Port of
    !> isearch.el:isearch-message-prefix (no lazy count, multi-buffer or
    !> input method).
    function isearch_message_prefix(nonincremental, ellipsis) result(m)
        logical, intent(in) :: nonincremental
        logical, intent(in), optional :: ellipsis
        character(len=:), allocatable :: m
        integer(int32) :: c
        integer(int64) :: pos
        type(status_type) :: status
        ! If about to search, and previous search regexp was invalid, check
        ! that it still is.  If it is valid now, let the message we display
        ! while searching say that it is valid.
        if (allocated(isearch_error) .and. flag(ellipsis)) then
            call re_search_forward(isearch_string, pos, status, bound=point(), noerror=SEARCH_NOERROR)
            if (status%ok()) deallocate (isearch_error)
        end if
        m = ""
        if (.not. isearch_success) m = "failing "
        if (isearch_adjusted == ADJUSTED_T) m = m//"pending "
        if (isearch_wrapped) then
            if (isearch_forward) then
                if (point() > isearch_opoint) m = m//"over"
            else
                if (point() < isearch_opoint) m = m//"over"
            end if
            m = m//"wrapped "
        end if
        if (.not. isearch_success) then
            ! widen-automatically is t.
            associate (b => buffers(cur))
                if (b%begv /= 1 .or. b%zv /= b%text%z) m = m//"narrowed "
            end associate
            if (isearch_case_fold_search == CASE_FOLD_NIL) m = m//"case-sensitive "
        end if
        ! isearch--describe-regexp-mode.
        if (isearch_regexp) m = m//"regexp "
        if (nonincremental) then
            m = m//"search"
        else
            m = m//"I-search"
        end if
        if (.not. isearch_forward) m = m//" backward"
        m = m//": "
        ! Capitalize the first letter (ASCII here).
        c = iachar(m(1:1))
        if (c >= iachar("a") .and. c <= iachar("z")) m(1:1) = achar(c - 32)
    end function isearch_message_prefix

    !> Port of isearch.el:isearch-message-suffix.
    function isearch_message_suffix(c_q_hack) result(m)
        logical, intent(in) :: c_q_hack
        character(len=:), allocatable :: m
        m = ""
        if (c_q_hack) m = "^Q"
        if (allocated(isearch_error)) m = m//" ["//isearch_error//"]"
    end function isearch_message_suffix

    ! ------------------------------------------------------------------
    ! Searching
    ! ------------------------------------------------------------------

    !> Search for the first occurrence of STRING.  If found, move point
    !> to the end of the occurrence, update the match data, and return
    !> true.  An invalid regexp is an error in `status'.  Port of
    !> isearch.el:isearch-search-string (BOUND nil, NOERROR t; no
    !> translation-table-for-input) with isearch-search-fun-default.
    logical function isearch_search_string(string, status) result(found)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp, saved_spaces
        integer(int64) :: pos
        logical :: had_spaces
        if (isearch_regexp) then
            regexp = string
        else
            regexp = regexp_quote(string)
        end if
        ! Use lax whitespace matching when it is on for this kind of
        ! search.
        had_spaces = allocated(search_spaces_regexp)
        if (had_spaces) saved_spaces = search_spaces_regexp
        if (allocated(search_spaces_regexp)) deallocate (search_spaces_regexp)
        if (merge(isearch_regexp_lax_whitespace, isearch_lax_whitespace, isearch_regexp) .and. &
            allocated(search_whitespace_regexp)) search_spaces_regexp = search_whitespace_regexp
        if (isearch_forward) then
            call re_search_forward(regexp, pos, status, noerror=SEARCH_NOERROR)
        else
            call re_search_backward(regexp, pos, status, noerror=SEARCH_NOERROR)
        end if
        if (allocated(search_spaces_regexp)) deallocate (search_spaces_regexp)
        if (had_spaces) search_spaces_regexp = saved_spaces
        found = status%ok() .and. pos > 0
    end function isearch_search_string

    !> Do the search with the current search string.  Port of
    !> isearch.el:isearch-search.
    subroutine isearch_search()
        type(status_type) :: status
        logical :: saved_fold, found
        if (isearch_case_fold_search == CASE_FOLD_T .and. search_upper_case /= UPPER_CASE_NIL) then
            isearch_case_fold_search = merge(CASE_FOLD_T, CASE_FOLD_NIL, &
                                             isearch_no_upper_case_p(isearch_string, isearch_regexp))
        end if
        saved_fold = case_fold_search%value
        case_fold_search = isearch_case_fold_search /= CASE_FOLD_NIL
        if (allocated(isearch_error)) deallocate (isearch_error)
        found = isearch_search_string(isearch_string, status)
        ! An error leaves isearch-success as it was.
        if (status%ok()) isearch_success = found
        case_fold_search = saved_fold
        if (status%code == FM_INVALID_REGEXP) then
            ! (car (cdr lossage)): the message string, without the quotes
            ! the error data print with.
            isearch_error = status%detail(2:len(status%detail) - 1)
            if (index(isearch_error, "Premature ") == 1 .or. index(isearch_error, "Unmatched ") == 1) then
                isearch_error = "incomplete input"
            else if (.not. isearch_regexp .and. index(isearch_error, "Regular expression too big") == 1 .and. &
                     isearch_lax_whitespace .and. allocated(search_whitespace_regexp)) then
                isearch_error = "Too many spaces for whitespace matching"
            end if
        else if (.not. status%ok()) then
            ! Stack overflow in regexp search.
            isearch_error = error_message(status%code)
            if (allocated(status%detail)) isearch_error = status%detail
        end if
        isearch_just_started = .false.
        if (isearch_success) then
            if (isearch_forward) then
                isearch_other_end = match_beginning(0, status)
            else
                isearch_other_end = match_end(0, status)
            end if
        else
            ! Ding if failed this time after succeeding last time.
            if (cmds(ncmds)%success) call ding()
            call goto_char(cmds(ncmds)%point)
        end if
    end subroutine isearch_search

    !> Whether the text at point matches the search string (as a regexp,
    !> or literally), folding case with `fold'; `mend' is where the match
    !> ends.  An invalid regexp doesn't match.
    logical function looking_at_string(string, fold, mend) result(found)
        character(len=*), intent(in) :: string
        logical, intent(in) :: fold
        integer(int64), intent(out) :: mend
        type(status_type) :: status
        logical :: saved_fold
        mend = 0
        saved_fold = case_fold_search%value
        case_fold_search = fold
        if (isearch_regexp) then
            found = looking_at(string, status)
        else
            found = looking_at(regexp_quote(string), status)
        end if
        case_fold_search = saved_fold
        found = found .and. status%ok()
        if (found) mend = match_end(0, status)
    end function looking_at_string

    !> Do the search and update the display.  Port of
    !> isearch.el:isearch-search-and-update.
    subroutine isearch_search_and_update()
        integer(int64) :: mend
        logical :: fold
        ! Unsuccessful regexp search may become successful by addition of
        ! characters which make isearch-string valid.
        if (isearch_success .or. isearch_regexp) then
            ! In reverse search, adding stuff at the end may cause zero or
            ! many more chars to be matched, in the string following point.
            ! Allow all those possibilities without moving point as long as
            ! the match does not extend past search origin.
            fold = isearch_case_fold_search /= CASE_FOLD_NIL
            if (isearch_case_fold_search == CASE_FOLD_T .and. search_upper_case /= UPPER_CASE_NIL) then
                fold = isearch_no_upper_case_p(isearch_string, isearch_regexp)
            end if
            if (reverse_match_here()) then
                isearch_success = .true.
                if (allocated(isearch_error)) deallocate (isearch_error)
                isearch_other_end = mend
                if (isearch_case_fold_search == CASE_FOLD_T .and. search_upper_case /= UPPER_CASE_NIL) then
                    isearch_case_fold_search = merge(CASE_FOLD_T, CASE_FOLD_NIL, &
                                                     isearch_no_upper_case_p(isearch_string, isearch_regexp))
                end if
            else
                ! Not regexp, not reverse, or no match at point.
                ! Do the following before moving point.
                call isearch_message_show(ellipsis=.true.)
                if (isearch_other_end /= 0 .and. isearch_adjusted == ADJUSTED_NIL) then
                    if (isearch_forward) then
                        call goto_char(isearch_other_end)
                    else
                        call goto_char(min(isearch_opoint, isearch_barrier, isearch_other_end + 1))
                    end if
                end if
                call isearch_search()
            end if
        end if
        call isearch_push_state()
        call isearch_update()
    contains

        logical function reverse_match_here()
            reverse_match_here = .false.
            if (isearch_forward .or. isearch_adjusted /= ADJUSTED_NIL) return
            if (.not. looking_at_string(isearch_string, fold, mend)) return
            reverse_match_here = isearch_yank_flag .or. mend <= min(isearch_opoint, isearch_barrier)
        end function reverse_match_here
    end subroutine isearch_search_and_update

    !> Utility for isearch-repeat-forward and isearch-repeat-backward.
    !> Port of isearch.el:isearch-repeat (isearch-wrap-pause t,
    !> isearch-repeat-on-direction-change nil).
    subroutine isearch_repeat(forward, count)
        logical, intent(in) :: forward
        integer(int64), intent(in), optional :: count
        type(value_type), allocatable :: ring(:)
        type(status_type) :: status
        integer(int64) :: n
        integer :: outer
        if (isearch_forward .eqv. forward) then
            ! C-s in forward or C-r in reverse.
            if (len(isearch_string) == 0) then
                ! If search string is empty, use last one.
                call search_ring(ring)
                if (size(ring) == 0) then
                    isearch_error = "No previous search string"
                else
                    if (stringp(ring(1))) isearch_string = xstring(ring(1))
                    isearch_message = isearch_string
                    isearch_case_fold_search = isearch_last_case_fold_search
                    ! After taking the last element, adjust ring to
                    ! previous one.
                    call isearch_ring_adjust1(.false.)
                end if
            else if (.not. isearch_success) then
                ! If already have what to search for, repeat it.
                isearch_wrapped = .true.
                if (isearch_forward) then
                    call goto_char(point_min())
                else
                    call goto_char(point_max())
                end if
            end if
        else
            ! C-s in reverse or C-r in forward, change direction.
            isearch_forward = .not. isearch_forward
            isearch_success = .true.
        end if
        ! For subsequent \| if regexp.
        isearch_barrier = point()
        if (len(isearch_string) == 0) then
            isearch_success = .true.
        else
            ! For the case when count > 1, don't keep intermediate states
            ! added to isearch-cmds by isearch-push-state in this loop.
            outer = ncmds
            n = 1
            if (present(count)) n = count
            do
                n = n - 1
                if (n < 0) exit
                if (isearch_success .and. point() == isearch_other_end .and. .not. isearch_just_started) then
                    ! If repeating a search that found an empty string,
                    ! ensure we advance.
                    if (merge(eobp(), bobp(), isearch_forward)) then
                        ! If there's nowhere to advance to, fail (and wrap
                        ! next time).
                        isearch_success = .false.
                        call ding()
                    else
                        call forward_char(status, merge(1_int64, -1_int64, isearch_forward))
                        call isearch_search()
                    end if
                else
                    call isearch_search()
                end if
                ! Update isearch-cmds, so if isearch-search fails later, it
                ! can restore old successful state from isearch-cmds.
                if (n > 0) call isearch_push_state()
                ! Stop looping on failure.
                if (.not. isearch_success .or. allocated(isearch_error)) n = 0
            end do
            ncmds = outer
        end if
        call isearch_push_state()
        call isearch_update()
    end subroutine isearch_repeat

    !> Repeat incremental search forwards.
    !> With a numeric argument, repeat the search ARG times.
    !> A negative argument searches backwards.  Port of
    !> isearch.el:isearch-repeat-forward.
    recursive subroutine isearch_repeat_forward(arg)
        type(value_type), intent(in) :: arg
        integer(int64) :: count
        if (nilp(arg)) then
            call isearch_repeat(.true.)
            return
        end if
        count = prefix_numeric_value(arg)
        if (count < 0) then
            call isearch_repeat_backward(make_count(abs(count)))
            ! Reverse the direction back.
            call isearch_repeat(.true.)
        else
            ! Take into account one iteration to reverse direction.
            if (.not. isearch_forward) count = count + 1
            call isearch_repeat(.true., count)
        end if
    end subroutine isearch_repeat_forward

    !> Repeat incremental search backwards.
    !> With a numeric argument, repeat the search ARG times.
    !> A negative argument searches forwards.  Port of
    !> isearch.el:isearch-repeat-backward.
    recursive subroutine isearch_repeat_backward(arg)
        type(value_type), intent(in) :: arg
        integer(int64) :: count
        if (nilp(arg)) then
            call isearch_repeat(.false.)
            return
        end if
        count = prefix_numeric_value(arg)
        if (count < 0) then
            call isearch_repeat_forward(make_count(abs(count)))
            ! Reverse the direction back.
            call isearch_repeat(.false.)
        else
            ! Take into account one iteration to reverse direction.
            if (isearch_forward) count = count + 1
            call isearch_repeat(.false., count)
        end if
    end subroutine isearch_repeat_backward

    function make_count(n) result(v)
        integer(int64), intent(in) :: n
        type(value_type) :: v
        v = make_fixnum(n)
    end function make_count

    !> Go to the first occurrence of the current search string.
    !> Move point to the beginning of the buffer and search forwards from
    !> the top.  With a numeric argument, go to the ARGth absolute
    !> occurrence counting from the beginning of the buffer.  Port of
    !> isearch.el:isearch-beginning-of-buffer.
    recursive subroutine isearch_beginning_of_buffer(arg)
        integer(int64), intent(in) :: arg
        logical :: current_forward
        if (arg < 0) then
            call isearch_end_of_buffer(abs(arg))
            return
        end if
        ! For the case when the match is at bobp, don't forward char in
        ! isearch-repeat.
        isearch_just_started = .true.
        call goto_char(point_min())
        current_forward = isearch_forward
        call isearch_repeat(.true., arg)
        if (.not. (current_forward .eqv. isearch_forward)) call isearch_repeat(current_forward)
    end subroutine isearch_beginning_of_buffer

    !> Go to the last occurrence of the current search string.
    !> Move point to the end of the buffer and search backwards from the
    !> bottom.  With a numeric argument, go to the ARGth absolute
    !> occurrence counting from the end of the buffer.  Port of
    !> isearch.el:isearch-end-of-buffer.
    recursive subroutine isearch_end_of_buffer(arg)
        integer(int64), intent(in) :: arg
        logical :: current_forward
        if (arg < 0) then
            call isearch_beginning_of_buffer(abs(arg))
            return
        end if
        isearch_just_started = .true.
        call goto_char(point_max())
        current_forward = isearch_forward
        call isearch_repeat(.false., arg)
        if (.not. (current_forward .eqv. isearch_forward)) call isearch_repeat(current_forward)
    end subroutine isearch_end_of_buffer

    ! ------------------------------------------------------------------
    ! Editing the search string
    ! ------------------------------------------------------------------

    !> Undo last input item during a search.
    !> An input item is the result of a command that pushes a new state
    !> of isearch to `isearch_cmds'.  If no input items have been entered
    !> yet, just beep.  Port of isearch.el:isearch-delete-char.
    subroutine isearch_delete_char()
        if (ncmds <= 1) then
            call ding()
        else
            call isearch_pop_state()
        end if
        call isearch_update()
    end subroutine isearch_delete_char

    !> Delete character from end of search string and search again.
    !> Unlike `isearch_delete_char', it only deletes the last character,
    !> but doesn't cancel the effect of other isearch command.
    !> If search string is empty, just beep.  Port of
    !> isearch.el:isearch-del-char.
    subroutine isearch_del_char(arg)
        integer(int64), intent(in) :: arg
        if (len(isearch_string) == 0) then
            call ding()
        else
            isearch_string = drop_chars(isearch_string, int(min(arg, int(nchars(isearch_string), int64))))
            isearch_message = isearch_string
        end if
        ! Do the following before moving point.
        call isearch_message_show()
        ! Use the isearch-other-end as new starting point to be able to
        ! find the remaining part of the search string again.
        if (isearch_other_end /= 0) call goto_char(isearch_other_end)
        call isearch_search()
        call isearch_push_state()
        call isearch_update()
    end subroutine isearch_del_char

    !> Add this ordinary printing CHAR to the search string and search.
    !> With argument, add COUNT copies of the character.  Port of
    !> isearch.el:isearch-printing-char.
    subroutine isearch_printing_char(char, count)
        integer(int64), intent(in) :: char, count
        integer(int64) :: c
        c = char
        if (c == ior(int(CHAR_SHIFT, int64), 32_int64)) c = 32
        call isearch_process_search_char(c, count)
    end subroutine isearch_printing_char

    !> Add CHAR to the search string, COUNT times.
    !> Search is updated accordingly.  Port of
    !> isearch.el:isearch-process-search-char.
    subroutine isearch_process_search_char(char, count)
        integer(int64), intent(in) :: char, count
        character(len=:), allocatable :: one, string
        ! * and ? are special in regexps when not preceded by \.  } and |
        ! are special in regexps when preceded by \.  Nothing special for
        ! + because it matches at least once.
        select case (char)
        case (iachar("*"), iachar("?"))
            call isearch_fallback(.false.)
        case (iachar("}"))
            call isearch_fallback(.true., .true.)
        case (iachar("|"))
            call isearch_fallback(.true., .false., .true.)
        case default
            continue
        end select
        one = encode(int(char, int32))
        string = one
        if (count > 1) string = repeat(one, int(count))
        ! Append the char(s) to the search string, update the message and
        ! re-search.
        call isearch_process_search_string(string, string)
    end subroutine isearch_process_search_char

    !> Port of isearch.el:isearch-process-search-string.
    subroutine isearch_process_search_string(string, message)
        character(len=*), intent(in) :: string, message
        isearch_string = isearch_string//string
        isearch_message = isearch_message//message
        call isearch_search_and_update()
    end subroutine isearch_process_search_string

    !> Return t if STR ends in an odd number of backslashes.  Port of
    !> isearch.el:isearch-backslash.
    pure logical function isearch_backslash(str)
        character(len=*), intent(in) :: str
        integer :: n
        n = 0
        do while (n < len(str))
            if (str(len(str) - n:len(str) - n) /= "\") exit
            n = n + 1
        end do
        isearch_backslash = mod(n, 2) == 1
    end function isearch_backslash

    !> Return point to previous successful match to allow regexp
    !> liberalization.  Respects C-s and C-r by stopping at
    !> `isearch_barrier' as needed.
    !>
    !> Do nothing if a backslash is escaping the liberalizing character.
    !> If WANT-BACKSLASH is non-nil, invert this behavior (for \} and \|).
    !>
    !> Do nothing if regexp has recently been invalid unless optional
    !> ALLOW-INVALID non-nil.
    !>
    !> If optional TO-BARRIER non-nil, ignore previous matches and go
    !> exactly to the barrier.  Port of isearch.el:isearch-fallback.
    subroutine isearch_fallback(want_backslash, allow_invalid, to_barrier)
        logical, intent(in) :: want_backslash
        logical, intent(in), optional :: allow_invalid, to_barrier
        integer :: frame, previous
        integer(int64) :: last_other_end
        character(len=:), allocatable :: string
        integer(int32) :: lchar
        logical :: skip
        if (.not. isearch_regexp) return
        if (isearch_backslash(isearch_string) .neqv. want_backslash) return
        ! We have to check 2 stack frames because the last might be
        ! invalid just because of a backslash.
        if (allocated(isearch_error) .and. ncmds >= 2 .and. .not. flag(allow_invalid)) then
            if (allocated(cmds(ncmds - 1)%error)) return
        end if
        if (flag(to_barrier)) then
            call goto_char(isearch_barrier)
            isearch_adjusted = ADJUSTED_T
            return
        end if
        ! Walk down the stack looking for a valid regexp (as of course only
        ! they can be the previous successful match); this conveniently
        ! removes all bracket-sets and groups that might be in the way, as
        ! well as partial \{\} constructs that the code below leaves
        ! behind.  Also skip over postfix operators.
        frame = ncmds
        previous = ncmds - 1
        do while (previous >= 1)
            skip = allocated(cmds(frame)%error)
            if (.not. skip) then
                string = cmds(frame)%string
                lchar = 0
                if (len(string) > 0) lchar = iachar(string(len(string):len(string)))
                ! The operators aren't always operators; check backslashes.
                if (isearch_backslash(cmds(previous)%string)) then
                    skip = lchar == iachar("}")
                else
                    skip = lchar == iachar("*") .or. lchar == iachar("?") .or. lchar == iachar("+")
                end if
            end if
            if (.not. skip) exit
            frame = previous
            previous = previous - 1
        end do
        if (frame < 1) return
        ! `frame' now refers the most recent valid regexp that is not at
        ! all optional in its last term.  Now dig one level deeper and find
        ! what matched before that.
        last_other_end = isearch_barrier
        if (previous >= 1) then
            if (cmds(previous)%other_end /= 0) last_other_end = cmds(previous)%other_end
        end if
        if (isearch_forward) then
            call goto_char(max(last_other_end, isearch_barrier))
        else
            call goto_char(min(last_other_end, isearch_barrier))
        end if
        isearch_adjusted = ADJUSTED_T
    end subroutine isearch_fallback

    !> Pull STRING into search string.  Port of
    !> isearch.el:isearch-yank-string.
    subroutine isearch_yank_string(string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: s
        s = string
        ! Downcase the string if not supposed to case-fold yanked strings.
        if (isearch_case_fold_search /= CASE_FOLD_NIL .and. search_upper_case == UPPER_CASE_NOT_YANKS) then
            s = downcase_string(s)
        end if
        if (isearch_regexp) s = regexp_quote(s)
        ! Don't move cursor in reverse search.
        isearch_yank_flag = .true.
        call isearch_process_search_string(s, s)
    end subroutine isearch_yank_string

    !> Pull the text from point to the point reached by `jump': 1 moves
    !> forward `arg' characters, 2 to the end of the line (`arg' lines
    !> on), 3 over `arg' words or characters (isearch--yank-char-or-syntax
    !> with word syntax).  Port of isearch.el:isearch-yank-internal.
    subroutine isearch_yank_internal(jump, arg)
        integer, intent(in) :: jump
        integer(int64), intent(in) :: arg
        character(len=:), allocatable :: text
        type(status_type) :: status
        integer(int64) :: saved, from, k
        saved = point()
        if (.not. isearch_forward .and. isearch_other_end /= 0) call goto_char(isearch_other_end)
        if (.not. isearch_success .and. isearch_yank_prev_point /= 0) call goto_char(isearch_yank_prev_point)
        from = point()
        if (jump == 1) then
            call forward_char(status, arg)
        else if (jump == 3) then
            do k = 1, arg
                if (char_syntax(char_after(point())) == "w" .or. char_syntax(char_after(point() + 1)) == "w") then
                    call forward_word(1_int64)
                else
                    call forward_char(status, 1_int64)
                end if
            end do
        else
            call goto_char(pos_eol(merge(arg + 1, arg, eolp())))
        end if
        isearch_yank_prev_point = point()
        if (isearch_success) isearch_yank_prev_point = 0
        call buffer_substring(from, point(), text, status)
        call goto_char(saved)
        call isearch_yank_string(text)
    end subroutine isearch_yank_internal

    !> Pull next character from buffer into search string.
    !> If optional ARG is given, pull in the next ARG characters.  Port
    !> of isearch.el:isearch-yank-char.
    subroutine isearch_yank_char(arg)
        integer(int64), intent(in) :: arg
        call isearch_yank_internal(1, arg)
    end subroutine isearch_yank_char

    !> Pull next character or word from buffer into search string.
    !> If optional ARG is given, pull in the next ARG characters/words.
    !> Port of isearch.el:isearch-yank-word-or-char.
    subroutine isearch_yank_word_or_char(arg)
        integer(int64), intent(in) :: arg
        call isearch_yank_internal(3, arg)
    end subroutine isearch_yank_word_or_char

    !> The character at `pos', or 0 at the end of the accessible region
    !> ((or (char-after POS) 0)).
    integer function char_after(pos)
        integer(int64), intent(in) :: pos
        char_after = 0
        if (pos >= buffers(cur)%begv .and. pos < buffers(cur)%zv) then
            char_after = buf_fetch_char(buffers(cur), char_to_byte(pos))
        end if
    end function char_after

    !> Pull rest of line from buffer into search string.
    !> If optional ARG is given, yank the next ARG lines.  Port of
    !> isearch.el:isearch-yank-line.
    subroutine isearch_yank_line(arg)
        integer(int64), intent(in) :: arg
        call isearch_yank_internal(2, arg)
    end subroutine isearch_yank_line

    !> Pull string from kill ring into search string.  Port of
    !> isearch.el:isearch-yank-kill.
    subroutine isearch_yank_kill(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        if (.not. isearch_active) call isearch_mode(.true.)
        call current_kill(0_int64, text, status)
        if (status%ok()) call isearch_yank_string(text)
    end subroutine isearch_yank_kill

    !> Replace just-yanked search string with previously killed string.
    !> When this command is called not immediately after a
    !> `isearch_yank_kill' or a `isearch_yank_pop_only', it only pops the
    !> last killed string.  Port of isearch.el:isearch-yank-pop-only (no
    !> C-u: reading from the kill ring needs the minibuffer completion).
    subroutine isearch_yank_pop_only(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        if (.not. (value_eq(last_command, sym("isearch_yank_kill")) .or. &
                   value_eq(last_command, sym("isearch_yank_pop")) .or. &
                   value_eq(last_command, sym("isearch_yank_pop_only")))) then
            call isearch_yank_kill(status)
        else
            call isearch_pop_state()
            call current_kill(1_int64, text, status)
            if (status%ok()) call isearch_yank_string(text)
        end if
    end subroutine isearch_yank_pop_only

    !> Print STRING at the end of the isearch prompt for 1 second.
    !> Port of isearch.el:isearch--momentary-message.
    subroutine isearch_momentary_message(string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: m
        m = isearch_message_prefix(isearch_nonincremental)
        if (allocated(isearch_message)) m = m//isearch_text_description(isearch_message)
        call message_nolog(m//" ["//string//"]")
        call sit_for(1000)
    end subroutine isearch_momentary_message

    !> Toggle case folding in searching on or off.
    !> Toggles the value of the variable `isearch_case_fold_search'.
    !> Port of isearch.el:isearch-toggle-case-fold.
    subroutine isearch_toggle_case_fold()
        if (.not. isearch_active) call isearch_mode(.true.)
        if (isearch_case_fold_search /= CASE_FOLD_NIL) then
            isearch_case_fold_search = CASE_FOLD_NIL
            call isearch_momentary_message("case sensitive")
        else
            isearch_case_fold_search = CASE_FOLD_YES
            call isearch_momentary_message("case insensitive")
        end if
        isearch_success = .true.
        isearch_adjusted = ADJUSTED_TOGGLE
        call isearch_update()
    end subroutine isearch_toggle_case_fold

    !> Toggle regexp searching on or off.  Port of
    !> isearch.el:isearch-toggle-regexp.
    subroutine isearch_toggle_regexp()
        if (.not. isearch_active) call isearch_mode(.true.)
        isearch_regexp = .not. isearch_regexp
        isearch_success = .true.
        isearch_adjusted = ADJUSTED_TOGGLE
        call isearch_update()
    end subroutine isearch_toggle_regexp

    !> Toggle lax-whitespace searching on or off.
    !> In ordinary search, toggles the value of the variable
    !> `isearch_lax_whitespace'.  In regexp search, toggles the value of
    !> the variable `isearch_regexp_lax_whitespace'.  Port of
    !> isearch.el:isearch-toggle-lax-whitespace.
    subroutine isearch_toggle_lax_whitespace()
        logical :: on
        if (.not. isearch_active) call isearch_mode(.true.)
        if (isearch_regexp) then
            isearch_regexp_lax_whitespace = .not. isearch_regexp_lax_whitespace
            on = isearch_regexp_lax_whitespace
        else
            isearch_lax_whitespace = .not. isearch_lax_whitespace
            on = isearch_lax_whitespace
        end if
        if (on) then
            call isearch_momentary_message("match spaces loosely")
        else
            call isearch_momentary_message("match spaces literally")
        end if
        isearch_success = .true.
        isearch_adjusted = ADJUSTED_TOGGLE
        call isearch_update()
    end subroutine isearch_toggle_lax_whitespace

    !> Edit the search string in the minibuffer.
    !> RET resumes incremental searching with the edited string.  Port of
    !> isearch.el:isearch-edit-string with with-isearch-suspended (no
    !> minibuffer-local-isearch-map).
    recursive subroutine isearch_edit_string(status)
        type(status_type), intent(out) :: status
        type(isearch_state), allocatable :: saved_cmds(:)
        type(value_type), allocatable :: ring(:)
        character(len=:), allocatable :: new_string, new_message, saved_error, input
        logical :: new_forward, new_nonincremental, saved_success, saved_wrapped, &
                   saved_yank_flag, had_error, saved_history_add
        integer :: new_case_fold, saved_ncmds, saved_adjusted
        integer(int64) :: saved_barrier, saved_opoint, old_point, old_other_end, fail_pos
        integer :: histpos
        type(isearch_state) :: now
        status = status_type()
        new_nonincremental = isearch_nonincremental
        ! Locally bind all isearch global variables to protect them from
        ! recursive isearching.
        new_string = isearch_string
        new_message = isearch_message
        new_forward = isearch_forward
        new_case_fold = isearch_case_fold_search
        saved_ncmds = ncmds
        if (ncmds > 0) saved_cmds = cmds(1:ncmds)
        saved_success = isearch_success
        saved_wrapped = isearch_wrapped
        saved_barrier = isearch_barrier
        saved_adjusted = isearch_adjusted
        saved_yank_flag = isearch_yank_flag
        had_error = allocated(isearch_error)
        if (had_error) saved_error = isearch_error
        saved_opoint = isearch_opoint
        fail_pos = isearch_fail_pos()
        if (isearch_regexp) then
            histpos = regexp_search_ring_yank_pointer + 1
        else
            histpos = search_ring_yank_pointer + 1
        end if
        ! Actually terminate isearching until editing is done.
        call isearch_done(nopush=.true., edit=.true.)
        ! Save old point and isearch-other-end before reading from
        ! minibuffer that can change their values.
        old_point = point()
        old_other_end = isearch_other_end
        ! Don't add a new search string to the search ring here; it should
        ! be added only by `isearch_update_ring' called from `isearch_done'.
        saved_history_add = history_add_new_input
        history_add_new_input = .false.
        if (fail_pos < 0) fail_pos = schars_of(isearch_string)
        call read_from_minibuffer(isearch_message_prefix(isearch_nonincremental), input, status, &
                                  initial=isearch_string, position=fail_pos + 1, history=ring_name(isearch_regexp), &
                                  history_position=histpos)
        history_add_new_input = saved_history_add
        if (status%ok()) then
            new_string = input
            new_message = input
        end if
        ! Always resume isearching by restarting it.
        if (.not. isearch_active) call isearch_mode(isearch_forward, isearch_regexp)
        ! Copy new local values to isearch globals.
        isearch_string = new_string
        isearch_message = new_message
        isearch_forward = new_forward
        isearch_nonincremental = new_nonincremental
        isearch_case_fold_search = new_case_fold
        ! Restore the minibuffer message before moving point.
        call isearch_message_show()
        ! Set point at the start (end) of old match if forward (backward),
        ! so after exiting minibuffer isearch resumes at the start (end) of
        ! this match and can find it again.
        if (old_other_end /= 0 .and. old_point == point()) call goto_char(old_other_end)
        if (status%ok()) then
            ! Empty isearch-string means use default.
            if (len(isearch_string) == 0) then
                call search_ring(ring)
                isearch_string = ""
                if (size(ring) > 0) isearch_string = xstring(ring(1))
                isearch_message = isearch_string
                ! After taking the last element, adjust ring to previous
                ! one.
                call isearch_ring_adjust1(.false.)
            end if
            ! Reinvoke the pending search.
            call isearch_search()
            ! If no code has changed the search parameters, then pushing a
            ! new state of Isearch should not be necessary.
            now = get_state()
            if (ncmds == 0) then
                call isearch_push_state()
            else if (.not. same_state(cmds(ncmds), now)) then
                call isearch_push_state()
            end if
            call isearch_update()
            if (isearch_nonincremental) then
                call isearch_done()
                ! The search done message is confusing when the string is
                ! empty, so erase it.
                if (len(isearch_string) == 0) call message("")
            end if
        end if
        ! The let's bindings end.
        ncmds = saved_ncmds
        if (saved_ncmds > 0) cmds(1:saved_ncmds) = saved_cmds
        isearch_success = saved_success
        isearch_wrapped = saved_wrapped
        isearch_barrier = saved_barrier
        isearch_adjusted = saved_adjusted
        isearch_yank_flag = saved_yank_flag
        if (allocated(isearch_error)) deallocate (isearch_error)
        if (had_error) isearch_error = saved_error
        isearch_opoint = saved_opoint
        ! Handle abort-recursive-edit.
        if (status%code == FM_QUIT) call isearch_abort(status)
    end subroutine isearch_edit_string

    !> Quote special characters for incremental search.
    !> With argument, add COUNT copies of the character.  Port of
    !> isearch.el:isearch-quote-char.
    subroutine isearch_quote_char(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        type(value_type) :: ev
        logical :: have
        integer(int64) :: c
        call message_nolog(isearch_message_string(.true.))
        call read_event(ev, have, status)
        if (.not. status%ok() .or. .not. have) return
        c = -1
        if (fixnump(ev)) c = xfixnum(ev)
        if (c < 0 .or. c > MAX_CHAR) then
            call set_error(status, FM_USER_ERROR, "Not a valid character")
            return
        end if
        call isearch_process_search_char(c, count)
    end subroutine isearch_quote_char

    ! ------------------------------------------------------------------
    ! Commands and the keymap
    ! ------------------------------------------------------------------

    integer(int64) function arg_p()
        arg_p = prefix_numeric_value(current_prefix_arg)
    end function arg_p

    subroutine cmd_isearch_exit(status)
        type(status_type), intent(out) :: status
        call isearch_exit(status)
    end subroutine cmd_isearch_exit

    subroutine cmd_isearch_cancel(status)
        type(status_type), intent(out) :: status
        call isearch_cancel(status)
    end subroutine cmd_isearch_cancel

    subroutine cmd_isearch_abort(status)
        type(status_type), intent(out) :: status
        call isearch_abort(status)
    end subroutine cmd_isearch_abort

    subroutine cmd_isearch_repeat_forward(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_repeat_forward(current_prefix_arg)
    end subroutine cmd_isearch_repeat_forward

    subroutine cmd_isearch_repeat_backward(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_repeat_backward(current_prefix_arg)
    end subroutine cmd_isearch_repeat_backward

    subroutine cmd_isearch_delete_char(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_delete_char()
    end subroutine cmd_isearch_delete_char

    subroutine cmd_isearch_del_char(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_del_char(arg_p())
    end subroutine cmd_isearch_del_char

    subroutine cmd_isearch_printing_char(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. fixnump(last_command_event)) return
        call isearch_printing_char(xfixnum(last_command_event), arg_p())
    end subroutine cmd_isearch_printing_char

    subroutine cmd_isearch_quote_char(status)
        type(status_type), intent(out) :: status
        call isearch_quote_char(arg_p(), status)
    end subroutine cmd_isearch_quote_char

    subroutine cmd_isearch_yank_char(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_yank_char(arg_p())
    end subroutine cmd_isearch_yank_char

    subroutine cmd_isearch_yank_word_or_char(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_yank_word_or_char(arg_p())
    end subroutine cmd_isearch_yank_word_or_char

    subroutine cmd_isearch_yank_line(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_yank_line(arg_p())
    end subroutine cmd_isearch_yank_line

    subroutine cmd_isearch_beginning_of_buffer(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_beginning_of_buffer(arg_p())
    end subroutine cmd_isearch_beginning_of_buffer

    subroutine cmd_isearch_end_of_buffer(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_end_of_buffer(arg_p())
    end subroutine cmd_isearch_end_of_buffer

    subroutine cmd_isearch_yank_kill(status)
        type(status_type), intent(out) :: status
        call isearch_yank_kill(status)
    end subroutine cmd_isearch_yank_kill

    subroutine cmd_isearch_yank_pop_only(status)
        type(status_type), intent(out) :: status
        call isearch_yank_pop_only(status)
    end subroutine cmd_isearch_yank_pop_only

    subroutine cmd_isearch_toggle_case_fold(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_toggle_case_fold()
    end subroutine cmd_isearch_toggle_case_fold

    subroutine cmd_isearch_toggle_regexp(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_toggle_regexp()
    end subroutine cmd_isearch_toggle_regexp

    subroutine cmd_isearch_toggle_lax_whitespace(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call isearch_toggle_lax_whitespace()
    end subroutine cmd_isearch_toggle_lax_whitespace

    subroutine cmd_isearch_edit_string(status)
        type(status_type), intent(out) :: status
        call isearch_edit_string(status)
    end subroutine cmd_isearch_edit_string

    subroutine cmd_isearch_ring_advance(status)
        type(status_type), intent(out) :: status
        call isearch_ring_advance(status)
    end subroutine cmd_isearch_ring_advance

    subroutine cmd_isearch_ring_retreat(status)
        type(status_type), intent(out) :: status
        call isearch_ring_retreat(status)
    end subroutine cmd_isearch_ring_retreat

    !> Bind `keys' (kbd syntax) to command `cmd' in `map'.
    subroutine bind(map, keys, cmd)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: status
        call key_parse(keys, key, status)
        call define_key(map, key, sym(cmd), status)
    end subroutine bind

    !> Register the isearch commands, build `isearch_mode_map', and bind
    !> C-s, C-r, C-M-s and C-M-r in `global_map'.  Port of isearch.el's
    !> isearch-mode-map and global bindings.
    subroutine syms_of_isearch(global_map)
        type(handle_type), intent(in) :: global_map
        type(handle_type) :: meta_map
        type(value_type) :: key(1)
        type(status_type) :: status
        call defvar_nil("search_ring")
        call defvar_nil("regexp_search_ring")
        commands%isearch_forward%run => isearch_forward_command
        commands%isearch_backward%run => isearch_backward_command
        commands%isearch_forward_regexp%run => isearch_forward_regexp_command
        commands%isearch_backward_regexp%run => isearch_backward_regexp_command
        commands%isearch_toggle_regexp%run => cmd_isearch_toggle_regexp
        commands%isearch_toggle_lax_whitespace%run => cmd_isearch_toggle_lax_whitespace
        commands%isearch_exit%run => cmd_isearch_exit
        commands%isearch_cancel%run => cmd_isearch_cancel
        commands%isearch_abort%run => cmd_isearch_abort
        commands%isearch_repeat_forward%run => cmd_isearch_repeat_forward
        commands%isearch_repeat_backward%run => cmd_isearch_repeat_backward
        commands%isearch_delete_char%run => cmd_isearch_delete_char
        commands%isearch_del_char%run => cmd_isearch_del_char
        commands%isearch_printing_char%run => cmd_isearch_printing_char
        commands%isearch_quote_char%run => cmd_isearch_quote_char
        commands%isearch_yank_char%run => cmd_isearch_yank_char
        commands%isearch_yank_line%run => cmd_isearch_yank_line
        commands%isearch_yank_word_or_char%run => cmd_isearch_yank_word_or_char
        commands%isearch_beginning_of_buffer%run => cmd_isearch_beginning_of_buffer
        commands%isearch_end_of_buffer%run => cmd_isearch_end_of_buffer
        commands%isearch_toggle_case_fold%run => cmd_isearch_toggle_case_fold
        commands%isearch_edit_string%run => cmd_isearch_edit_string
        commands%isearch_ring_advance%run => cmd_isearch_ring_advance
        commands%isearch_ring_retreat%run => cmd_isearch_ring_retreat
        commands%isearch_yank_kill%run => cmd_isearch_yank_kill
        commands%isearch_yank_pop_only%run => cmd_isearch_yank_pop_only
        if (.not. allocated(search_whitespace_regexp)) search_whitespace_regexp = "[ "//achar(9)//"]+"
        if (keymap_live_p(isearch_mode_map)) call free_keymap(isearch_mode_map, status)
        call make_keymap(isearch_mode_map)
        ! Make all multibyte characters search for themselves.
        call define_key_range(isearch_mode_map, 256_int64, int(MAX_CHAR, int64), &
                              sym("isearch_printing_char"), status)
        ! Single-byte printing chars extend the search string by default.
        call define_key_range(isearch_mode_map, 32_int64, 255_int64, sym("isearch_printing_char"), status)
        ! To handle local bindings with meta char prefix keys, define
        ! another full keymap.
        call make_sparse_keymap(meta_map)
        key(1) = make_count(27_int64)
        call define_key(isearch_mode_map, key, make_keymap_ref(meta_map), status)
        ! Several non-printing chars change the searching behavior.
        call bind(isearch_mode_map, "C-s", "isearch_repeat_forward")
        call bind(isearch_mode_map, "C-r", "isearch_repeat_backward")
        ! Define M-C-s and M-C-r like C-s and C-r so that the same key
        ! combinations can be used to repeat regexp isearches that can be
        ! used to start these searches.
        call bind(isearch_mode_map, "C-M-s", "isearch_repeat_forward")
        call bind(isearch_mode_map, "C-M-r", "isearch_repeat_backward")
        call bind(isearch_mode_map, "DEL", "isearch_delete_char")
        call bind(isearch_mode_map, "<backspace>", "undefined")
        call bind(isearch_mode_map, "C-g", "isearch_abort")
        call bind(isearch_mode_map, "ESC ESC ESC", "isearch_cancel")
        call bind(isearch_mode_map, "C-q", "isearch_quote_char")
        call bind(isearch_mode_map, "RET", "isearch_exit")
        call bind(isearch_mode_map, "<return>", "isearch_exit")
        call bind(isearch_mode_map, "C-j", "isearch_printing_char")
        call bind(isearch_mode_map, "TAB", "isearch_printing_char")
        call bind(isearch_mode_map, "S-SPC", "isearch_printing_char")
        call bind(isearch_mode_map, "C-M-d", "isearch_del_char")
        call bind(isearch_mode_map, "C-w", "isearch_yank_word_or_char")
        call bind(isearch_mode_map, "C-M-y", "isearch_yank_char")
        call bind(isearch_mode_map, "M-s C-e", "isearch_yank_line")
        call bind(isearch_mode_map, "M-s M-<", "isearch_beginning_of_buffer")
        call bind(isearch_mode_map, "M-s M->", "isearch_end_of_buffer")
        call bind(isearch_mode_map, "M-n", "isearch_ring_advance")
        call bind(isearch_mode_map, "M-p", "isearch_ring_retreat")
        call bind(isearch_mode_map, "C-y", "isearch_yank_kill")
        call bind(isearch_mode_map, "M-y", "isearch_yank_pop_only")
        call bind(isearch_mode_map, "M-c", "isearch_toggle_case_fold")
        call bind(isearch_mode_map, "M-s c", "isearch_toggle_case_fold")
        call bind(isearch_mode_map, "M-r", "isearch_toggle_regexp")
        call bind(isearch_mode_map, "M-s r", "isearch_toggle_regexp")
        call bind(isearch_mode_map, "M-s SPC", "isearch_toggle_lax_whitespace")
        call bind(isearch_mode_map, "M-e", "isearch_edit_string")
        call bind(isearch_mode_map, "M-s e", "isearch_edit_string")
        call bind(global_map, "C-s", "isearch_forward")
        call bind(global_map, "C-r", "isearch_backward")
        call bind(global_map, "C-M-s", "isearch_forward_regexp")
        call bind(global_map, "C-M-r", "isearch_backward_regexp")
    end subroutine syms_of_isearch
end module isearch_fm
