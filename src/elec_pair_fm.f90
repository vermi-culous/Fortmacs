! elec_pair_fm.f90 -- automatically insert matching delimiters.
! Ported from GNU Emacs 31.1 lisp/elec-pair.el.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> Electric Pair mode: typing an opening delimiter inserts the closing
!> one, typing a closing delimiter before an identical one skips it,
!> keeping delimiters balanced, from `post_self_insert_hook'.  Off by
!> default, as upstream.
!>
!> A pair is an `electric_pair_type': two characters (upstream's (CHAR
!> . CHAR)) or two strings (STRING STRING SPC) / (STRING . STRING), the
!> first a regular expression.
!>
!> Deferred: the DEL binding of `electric_pair_mode_map' and
!> `electric_pair_delete_adjacent_pairs' (a menu item with a :filter;
!> keymap menu items are not ported), `self_insert_uses_region_functions'
!> (`electric_pair_will_use_region'; delete-selection mode is not
!> ported), `electric_pair_analyze_conversion' (input methods' text
!> conversion), the function forms of
!> `electric_pair_open_newline_between_pairs' and
!> `electric_pair_skip_whitespace', buffer-local values of
!> `electric_pair_skip_whitespace_function' (one global value),
!> `electric_layout_allow_duplicate_newlines' (electric layout mode is
!> not ported), overwrite mode and the cc-mode syntax-ppss workaround.
module elec_pair_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, FM_SCAN_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, fixnump, xfixnum, make_fixnum
    use lread_fm, only: sym
    use data_fm, only: default_value, set_default, kill_local_variable
    use commands_fm, only: commands
    use eval_fm, only: hook_type, hook_current_prefix_arg
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use buffer_fm, only: buffers, cur, current_buffer, buffer_modified_p, restore_buffer_modified_p, &
                         barf_if_buffer_read_only
    use editfns_fm, only: point, point_min, point_max, goto_char, bobp, char_after, char_before, delete_region, &
                          buffer_substring, line_beginning_position
    use insdel_fm, only: insert
    use cmds_fm, only: post_self_insert_hook, self_insert_command, delete_char, forward_char
    use keyboard_fm, only: last_command_event
    use syntax_fm, only: char_syntax, matching_paren, syntax_ppss, parse_partial_sexp, parse_state_type, &
                         scan_sexps, scan_error_data, skip_chars_forward, skip_syntax_forward, skip_syntax_backward, &
                         forward_comment, ST_STRING_STYLE
    use subr_fm, only: looking_back
    use simple_fm, only: blink_matching_paren, BLINK_MATCHING_PAREN_NIL, use_region_p, mark, newline, &
                         backward_delete_char_untabify
    use modes_fm, only: minor_modes
    use electric_fm, only: electric_after_char_pos
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use character_fm, only: char_string, MAX_MULTIBYTE_LENGTH, string_char_and_length
    implicit none(type, external)
    private

    public :: syms_of_elec_pair, electric_pair_mode, electric_pair_local_mode, electric_pair_syntax_info, &
              electric_pair_post_self_insert_function, electric_pair_open_newline_between_pairs_psif, &
              electric_pair_delete_pair, electric_pair_default_skip_self, electric_pair_default_inhibit, &
              electric_pair_conservative_inhibit, electric_pair_inhibit_if_helps_balance, &
              electric_pair_skip_if_helps_balance, electric_pair_skip_whitespace_default, electric_pair_char_proc, &
              electric_pair_position_proc

    !> One pair: the characters OPEN and CLOSE, or, when OPEN_STRING is
    !> allocated, the strings OPEN_STRING (a regular expression) and
    !> CLOSE_STRING, with SPACE true to insert an extra space after
    !> OPEN_STRING.
    type, public :: electric_pair_type
        integer :: open = -1, close = -1
        character(len=:), allocatable :: open_string, close_string
        logical :: space = .false.
    end type electric_pair_type

    !> What `electric_pair_syntax_info' gives: the syntax character SYNTAX
    !> of the event (`SYNTAX_STR' for a string pair), its PAIR (a
    !> character, or PAIR_STRING for a string pair), whether
    !> `electric_pair_pairs' or `electric_pair_text_pairs' were used
    !> (UNCONDITIONAL), and STRING_OR_COMMENT_START (0 for nil) or, for a
    !> string pair, SPACE.  FOUND false is nil.
    type, public :: electric_pair_info_type
        logical :: found = .false.
        integer :: syntax = 0
        integer :: pair = -1
        character(len=:), allocatable :: pair_string
        logical :: unconditional = .false.
        integer(int64) :: string_or_comment = 0
        logical :: space = .false.
    end type electric_pair_info_type

    !> `electric_pair_info_type' syntax of a string pair (upstream's `str').
    integer, parameter, public :: SYNTAX_STR = -2

    !> Values of `electric_pair_skip_whitespace'.
    integer, parameter, public :: ELECTRIC_PAIR_SKIP_WHITESPACE_NIL = 0, ELECTRIC_PAIR_SKIP_WHITESPACE_T = 1, &
                                  ELECTRIC_PAIR_SKIP_WHITESPACE_CHOMP = 2

    abstract interface
        !> A predicate on a character (`electric_pair_skip_self',
        !> `electric_pair_inhibit_predicate').
        logical function electric_pair_char_proc(char)
            implicit none(type, external)
            integer, intent(in) :: char
        end function electric_pair_char_proc

        !> A function giving a buffer position (`electric_pair_string_bound_function').
        integer(int64) function electric_pair_position_proc()
            import :: int64
            implicit none(type, external)
        end function electric_pair_position_proc
    end interface

    !> Alist of pairs that should be used regardless of major mode.
    !>
    !> Pairs of delimiters in this list are a fallback in case they have
    !> no syntax relevant to `electric_pair_mode' in the mode's syntax
    !> table.
    !>
    !> Each list element should be in one of these forms:
    !>  (CHAR . CHAR)
    !> Where CHAR is character to be used as pair.
    !>
    !>  (STRING STRING SPC)
    !> Where STRING is a string to be used as pair and SPC a true value
    !> which specifies to insert an extra space after first STRING.
    !>
    !>  (STRING . STRING)
    !> This is similar to (STRING STRING SPC) form, except that SPC (space) is
    !> ignored and will not be inserted.
    !>
    !> In both string pairs forms, the first string pair must be a regular
    !> expression.
    !>
    !> Unlike character pairs, string pairs do not support
    !> inserting pairs in regions and cannot be deleted with
    !> `electric_pair_delete_pair', thus string pairs should be used only for
    !> multi-character pairs.
    !>
    !> See also the variable `electric_pair_text_pairs'.  Port of
    !> elec-pair.el electric-pair-pairs (an array of `electric_pair_type').
    type(electric_pair_type), allocatable, public :: electric_pair_pairs(:)

    !> Alist of pairs that should always be used in comments and strings.
    !>
    !> Pairs of delimiters in this list are a fallback in case they have
    !> no syntax relevant to `electric_pair_mode' in the syntax table
    !> defined in `electric_pair_text_syntax_table'.
    !>
    !> The elements have the forms of `electric_pair_pairs'.  Port of
    !> elec-pair.el electric-pair-text-pairs.
    type(electric_pair_type), allocatable, public :: electric_pair_text_pairs(:)

    !> If set, skip char instead of inserting a second closing paren.
    !>
    !> When inserting a closing delimiter right before the same character, just
    !> skip that character instead, so that, for example, consecutively
    !> typing `(' and `)' results in "()" rather than "())".
    !>
    !> This can be convenient for people who find it easier to type `)' than
    !> \[forward_char].
    !>
    !> It is a function of one argument (the closing delimiter just
    !> inserted), whose return value is considered; null never skips.
    !> Port of elec-pair.el electric-pair-skip-self (t is a function
    !> returning true).
    procedure(electric_pair_char_proc), pointer, public :: electric_pair_skip_self => null()

    !> Predicate to prevent insertion of a matching pair.
    !>
    !> The function is called with a single char (the opening delimiter just
    !> inserted).  If it returns true, then `electric_pair_mode' will not
    !> insert a matching closing delimiter.  Port of elec-pair.el
    !> electric-pair-inhibit-predicate.
    procedure(electric_pair_char_proc), pointer, public :: electric_pair_inhibit_predicate => null()

    !> Whether to keep matching delimiters balanced.
    !> When true, typing a delimiter inserts only this character if there is
    !> a unpaired matching delimiter later (if the latter is a closing
    !> delimiter) or earlier (if the latter is a opening delimiter) in the
    !> buffer.  When false, inserting a delimiter disregards unpaired matching
    !> delimiters.
    !>
    !> Whether this variable takes effect depends on the variables
    !> `electric_pair_inhibit_predicate' and `electric_pair_skip_self', which
    !> check the value of this variable before delegating to other predicates
    !> responsible for making decisions on whether to pair/skip some characters
    !> based on the actual state of the buffer's delimiters.  In addition, this
    !> variable has no effect if there is an active region.  Port of
    !> elec-pair.el electric-pair-preserve-balance.
    logical, public :: electric_pair_preserve_balance = .true.

    !> Whether to insert an extra newline between matching delimiters.
    !> If true, then when an opening delimiter immediately precedes a
    !> matching closing delimiter and point is between them, typing a newline
    !> automatically inserts an extra newline after point.  If false, just one
    !> newline is inserted before point.  Port of elec-pair.el
    !> electric-pair-open-newline-between-pairs (no function form).
    logical, public :: electric_pair_open_newline_between_pairs = .true.

    !> Whether typing a closing delimiter moves point over whitespace.
    !> If `ELECTRIC_PAIR_SKIP_WHITESPACE_T' and point is separated from a
    !> closing delimiter only by whitespace, then typing a closing delimiter
    !> of the same type does not insert that character but instead moves
    !> point to immediately after the already present closing delimiter.  If
    !> the value of this variable is `ELECTRIC_PAIR_SKIP_WHITESPACE_CHOMP',
    !> then the whitespace moved over is deleted.  If the value is
    !> `ELECTRIC_PAIR_SKIP_WHITESPACE_NIL', typing a closing delimiter simply
    !> inserts it at point.
    !>
    !> The specific kind of whitespace skipped is given by the variable
    !> `electric_pair_skip_whitespace_chars'.  Port of elec-pair.el
    !> electric-pair-skip-whitespace (no function form).
    integer, public :: electric_pair_skip_whitespace = ELECTRIC_PAIR_SKIP_WHITESPACE_T

    !> Whitespace characters considered by `electric_pair_skip_whitespace'.
    !> Port of elec-pair.el electric-pair-skip-whitespace-chars.
    integer, allocatable, public :: electric_pair_skip_whitespace_chars(:)

    !> Function to use to move point forward over whitespace.
    !> Before attempting a skip, if `electric_pair_skip_whitespace' is
    !> not `ELECTRIC_PAIR_SKIP_WHITESPACE_NIL', this function is called.  It
    !> moves point to a new buffer position, presumably skipping only
    !> whitespace in between.  Port of elec-pair.el
    !> electric-pair-skip-whitespace-function (one global value).
    procedure(command_proc_local), pointer, public :: electric_pair_skip_whitespace_function => null()

    !> Syntax table used when pairing inside comments and strings.
    !>
    !> `electric_pair_mode' considers this syntax table only when point is
    !> within text marked as a comment or enclosed within quotes.  If lookup
    !> fails here, `electric_pair_text_pairs' will be considered.  Port of
    !> elec-pair.el electric-pair-text-syntax-table (the null handle is the
    !> standard syntax table, which is what prog_mode_syntax_table, not
    !> ported, inherits everything from).
    type(handle_type), public :: electric_pair_text_syntax_table

    !> Next buffer position where strings are syntactically unexpected.
    !> Value is a function called with no arguments and returning a
    !> buffer position.  Major modes should set this variable
    !> to experience less slowness with `electric_pair_mode' when pairing
    !> quotes.  Port of elec-pair.el electric-pair-string-bound-function.
    procedure(electric_pair_position_proc), pointer, public :: electric_pair_string_bound_function => null()

    !> Hook run after entering or leaving `electric_pair_mode'.  Port of
    !> the electric-pair-mode-hook define-minor-mode defines.
    type(hook_type), target, public :: electric_pair_mode_hook = hook_type(name="electric_pair_mode_hook")

    !> Hook run after entering or leaving `electric_pair_local_mode'.  Port
    !> of the electric-pair-local-mode-hook define-minor-mode defines.
    type(hook_type), target, public :: electric_pair_local_mode_hook = hook_type(name="electric_pair_local_mode_hook")

    abstract interface
        !> A procedure of no arguments that moves point.
        subroutine command_proc_local(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine command_proc_local
    end interface

    !> A list description (MATCHED-P . PAIR) of `electric_pair_balance_info':
    !> SET false is not yet known; PAIR -1 is nil.
    type :: balance_type
        logical :: set = .false.
        logical :: matched = .false.
        integer :: pair = -1
    end type balance_type

contains

    !> Define Electric Pair mode and its local mode, with their variables'
    !> defaults (as loading elec-pair.el does).
    subroutine syms_of_elec_pair()
        call minor_modes%electric_pair_mode%define("electric_pair_mode", global=.true., keymap=.true., &
                                                   body=electric_pair_mode_body, hook=electric_pair_mode_hook)
        commands%electric_pair_mode%run => electric_pair_mode
        call minor_modes%electric_pair_local_mode%define("electric_pair_local_mode", &
                                                         body=electric_pair_local_mode_body, &
                                                         hook=electric_pair_local_mode_hook, &
                                                         getter=electric_pair_local_mode_p)
        commands%electric_pair_local_mode%run => electric_pair_local_mode
        commands%electric_pair_delete_pair%run => cmd_electric_pair_delete_pair
        ! (?\" . ?\") and electric-quote-chars' single and double quotes.
        if (.not. allocated(electric_pair_pairs)) call default_pairs(electric_pair_pairs)
        if (.not. allocated(electric_pair_text_pairs)) call default_pairs(electric_pair_text_pairs)
        if (.not. allocated(electric_pair_skip_whitespace_chars)) electric_pair_skip_whitespace_chars = [9, 32, 10]
        if (.not. associated(electric_pair_skip_self)) electric_pair_skip_self => electric_pair_default_skip_self
        if (.not. associated(electric_pair_inhibit_predicate)) then
            electric_pair_inhibit_predicate => electric_pair_default_inhibit
        end if
        if (.not. associated(electric_pair_skip_whitespace_function)) then
            electric_pair_skip_whitespace_function => electric_pair_skip_whitespace_default
        end if
        if (.not. associated(electric_pair_string_bound_function)) then
            electric_pair_string_bound_function => string_bound_point_max
        end if
    contains
        subroutine default_pairs(pairs)
            type(electric_pair_type), allocatable, intent(out) :: pairs(:)
            allocate (pairs(3))
            pairs(1)%open = iachar('"')
            pairs(1)%close = iachar('"')
            pairs(2)%open = int(z'2018')
            pairs(2)%close = int(z'2019')
            pairs(3)%open = int(z'201C')
            pairs(3)%close = int(z'201D')
        end subroutine default_pairs
    end subroutine syms_of_elec_pair

    !> The default `electric_pair_string_bound_function': `point_max'.
    integer(int64) function string_bound_point_max()
        string_bound_point_max = point_max()
    end function string_bound_point_max

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> The character `last_command_event' is, or -1.
    integer function event_char()
        event_char = -1
        if (fixnump(last_command_event)) event_char = int(xfixnum(last_command_event))
    end function event_char

    !> (prefix-numeric-value current-prefix-arg) as a post-self-insert
    !> function sees it.
    integer(int64) function psi_num()
        psi_num = prefix_numeric_value(hook_current_prefix_arg)
    end function psi_num

    !> The character after POS (default point), or -1 for nil.
    integer function after(pos)
        integer(int64), intent(in), optional :: pos
        integer(int32) :: c
        logical :: found
        call char_after(c, found, pos)
        after = -1
        if (found) after = int(c)
    end function after

    !> The character before POS (default point), or -1 for nil.
    integer function before(pos)
        integer(int64), intent(in), optional :: pos
        integer(int32) :: c
        logical :: found
        call char_before(c, found, pos)
        before = -1
        if (found) before = int(c)
    end function before

    !> The syntax descriptor character of C in the current syntax table.
    character function syntax_char(c)
        integer, intent(in) :: c
        syntax_char = " "
        if (c >= 0) syntax_char = char_syntax(c)
    end function syntax_char

    !> (matching-paren C): -1 for nil.
    integer function matching(c)
        integer, intent(in) :: c
        matching = -1
        if (c >= 0) matching = matching_paren(c)
    end function matching

    !> The UTF-8 encoding of character C.
    function char_text(c) result(s)
        integer, intent(in) :: c
        character(len=:), allocatable :: s
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        integer(int32) :: n
        type(status_type) :: st
        call char_string(int(c, int32), buf, n, st)
        s = buf(1:n)
    end function char_text

    !> Run with the `electric_pair_text_syntax_table' when
    !> STRING_OR_COMMENT (the start position of the string/comment in which
    !> we are) is not 0: set it as the buffer's syntax table, keeping the
    !> old one in SAVED.  Port of the entry of elec-pair.el:
    !> electric-pair--with-syntax-1.
    subroutine with_syntax_begin(string_or_comment, saved)
        integer(int64), intent(in) :: string_or_comment
        type(handle_type), intent(out) :: saved
        saved = buffers(cur)%syntax_table
        if (string_or_comment /= 0) buffers(cur)%syntax_table = electric_pair_text_syntax_table
    end subroutine with_syntax_begin

    subroutine with_syntax_end(saved)
        type(handle_type), intent(in) :: saved
        buffers(cur)%syntax_table = saved
    end subroutine with_syntax_end

    !> Begin a save-excursion: HERE marks point.
    subroutine save_excursion_begin(here)
        type(handle_type), intent(out) :: here
        type(status_type) :: st
        call copy_marker_at(point(), here, .false., st)
    end subroutine save_excursion_begin

    !> End a save-excursion: back to HERE, which is freed.
    subroutine save_excursion_end(here)
        type(handle_type), intent(in) :: here
        type(status_type) :: st
        integer(int64) :: p
        call marker_position(here, p, st)
        call goto_char(p)
        call free_marker(here, st)
    end subroutine save_excursion_end

    !> (syntax-ppss POS): the state at POS; point moves to POS.
    subroutine ppss_at(pos, state)
        integer(int64), intent(in) :: pos
        type(parse_state_type), intent(out) :: state
        call syntax_ppss(state, pos)
    end subroutine ppss_at

    ! ------------------------------------------------------------------
    ! Syntax
    ! ------------------------------------------------------------------

    !> Move point forward over whitespace.
    !> But do not move point if doing so crosses comment or string
    !> boundaries.  Port of elec-pair.el:electric-pair--skip-whitespace.
    subroutine electric_pair_skip_whitespace_default(status)
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state
        integer(int64) :: saved, string_or_comment, moved
        character(len=:), allocatable :: chars
        integer :: k
        status = status_type()
        saved = point()
        call ppss_at(saved, state)
        string_or_comment = state%comstr_start
        chars = ""
        do k = 1, size(electric_pair_skip_whitespace_chars)
            chars = chars//char_text(electric_pair_skip_whitespace_chars(k))
        end do
        moved = skip_chars_forward(chars, status)
        call ppss_at(point(), state)
        if (string_or_comment /= state%comstr_start) call goto_char(saved)
    end subroutine electric_pair_skip_whitespace_default

    !> Calculate SYNTAX PAIR UNCONDITIONAL STRING_OR_COMMENT_START.
    !>
    !> SYNTAX is COMMAND_EVENT's syntax character.  PAIR is COMMAND_EVENT's
    !> pair.  UNCONDITIONAL indicates that the variables `electric_pair_pairs'
    !> or `electric_pair_text_pairs' were used to look up syntax.
    !> STRING_OR_COMMENT_START indicates that point is inside a comment or
    !> string.  Port of elec-pair.el:electric-pair-syntax-info.
    function electric_pair_syntax_info(command_event) result(info)
        integer, intent(in) :: command_event
        type(electric_pair_info_type) :: info
        type(parse_state_type) :: state
        type(electric_pair_type), allocatable :: fallback(:)
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: opoint, pre_string_or_comment, post_string_or_comment, string_or_comment
        integer :: k, table_syntax, table_pair, direct, reverse
        logical :: pre_t
        opoint = point()
        pre_t = bobp()
        pre_string_or_comment = 0
        if (.not. pre_t) then
            call ppss_at(opoint - 1, state)
            pre_string_or_comment = state%comstr_start
            call goto_char(opoint)
        end if
        call ppss_at(opoint, state)
        post_string_or_comment = state%comstr_start
        string_or_comment = 0
        if (post_string_or_comment /= 0 .and. (pre_t .or. pre_string_or_comment /= 0)) then
            string_or_comment = post_string_or_comment
        end if
        call with_syntax_begin(string_or_comment, saved)
        table_syntax = iachar(syntax_char(command_event))
        table_pair = matching(command_event)
        if (table_pair < 0) table_pair = command_event
        call with_syntax_end(saved)
        if (string_or_comment /= 0) then
            fallback = [electric_pair_text_pairs, electric_pair_pairs]
        else
            fallback = electric_pair_pairs
        end if
        direct = 0
        reverse = 0
        do k = 1, size(fallback)
            if (allocated(fallback(k)%open_string)) cycle
            if (direct == 0 .and. fallback(k)%open == command_event) direct = k
            if (reverse == 0 .and. fallback(k)%close == command_event) reverse = k
        end do
        do k = 1, size(fallback)
            if (.not. allocated(fallback(k)%open_string)) cycle
            if (looking_back(fallback(k)%open_string, st, line_beginning_position())) then
                info%found = .true.
                info%syntax = SYNTAX_STR
                ! Get pair ender.
                info%pair_string = ""
                if (allocated(fallback(k)%close_string)) info%pair_string = fallback(k)%close_string
                ! Check if pairs have to insert a space after first pair
                ! was inserted.
                info%space = fallback(k)%space
                return
            end if
        end do
        if (table_syntax == iachar('"') .or. table_syntax == iachar("(") .or. table_syntax == iachar(")") &
            .or. table_syntax == iachar("$")) then
            info = electric_pair_info_type(found=.true., syntax=table_syntax, pair=table_pair, &
                                           string_or_comment=string_or_comment)
        else if (direct > 0) then
            if (fallback(direct)%open == fallback(direct)%close) then
                info = electric_pair_info_type(found=.true., syntax=iachar('"'), pair=command_event, &
                                               unconditional=.true., string_or_comment=string_or_comment)
            else
                info = electric_pair_info_type(found=.true., syntax=iachar("("), pair=fallback(direct)%close, &
                                               unconditional=.true., string_or_comment=string_or_comment)
            end if
        else if (reverse > 0) then
            info = electric_pair_info_type(found=.true., syntax=iachar(")"), pair=fallback(reverse)%open, &
                                           unconditional=.true., string_or_comment=string_or_comment)
        end if
    end function electric_pair_syntax_info

    !> Insert CHAR TIMES times, without electricity.  Port of
    !> elec-pair.el:electric-pair--insert.
    recursive subroutine electric_pair_insert(char, times, status)
        integer, intent(in) :: char
        integer(int64), intent(in) :: times
        type(status_type), intent(out) :: status
        type(value_type) :: saved_event
        integer :: saved_blink
        logical :: saved_mode
        saved_event = last_command_event
        saved_blink = blink_matching_paren
        saved_mode = minor_modes%electric_pair_mode%state%value
        last_command_event = make_fixnum(int(char, int64))
        blink_matching_paren = BLINK_MATCHING_PAREN_NIL
        minor_modes%electric_pair_mode%state%value = .false.
        call self_insert_command(times, int(char, int64), status)
        last_command_event = saved_event
        blink_matching_paren = saved_blink
        minor_modes%electric_pair_mode%state%value = saved_mode
    end subroutine electric_pair_insert

    !> Like `syntax_ppss', but maybe fall back to `parse_partial_sexp'.
    !>
    !> IN_STRINGS and IN_COMMENTS (upstream's WHERE, by default both) say
    !> when to fall back to `parse_partial_sexp'.  Port of
    !> elec-pair.el:electric-pair--syntax-ppss.
    subroutine electric_pair_syntax_ppss(state, pos, comments_only)
        type(parse_state_type), intent(out) :: state
        integer(int64), intent(in), optional :: pos
        logical, intent(in), optional :: comments_only
        type(parse_state_type) :: quick_ppss
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: p, s_or_c_start, moved
        logical :: in_string, in_comment, found
        p = point()
        if (present(pos)) p = pos
        call ppss_at(p, quick_ppss)
        in_string = quick_ppss%instring >= 0
        if (present(comments_only)) then
            if (comments_only) in_string = .false.
        end if
        in_comment = quick_ppss%incomment /= 0
        s_or_c_start = 0
        if (in_string) then
            s_or_c_start = quick_ppss%comstr_start + 1
        else if (in_comment) then
            call goto_char(quick_ppss%comstr_start)
            found = forward_comment(-point_max())
            moved = skip_syntax_forward(" >!")
            s_or_c_start = point()
        end if
        if (s_or_c_start /= 0) then
            call with_syntax_begin(s_or_c_start, saved)
            call parse_partial_sexp(s_or_c_start, p, state, st)
            call with_syntax_end(saved)
        else
            state = quick_ppss
        end if
    end subroutine electric_pair_syntax_ppss

    !> Examine lists forward or backward according to DIRECTION's sign.
    !>
    !> STRING_OR_COMMENT is the position of the start of the comment/string
    !> in which we are, if applicable.
    !>
    !> Return two descriptions (MATCHED-P . PAIR) for the innermost and
    !> outermost lists that enclose point.  The outermost list enclosing
    !> point is either the first top-level or first mismatched list found
    !> by listing up.
    !>
    !> If the outermost list is matched, don't rely on its PAIR.
    !> If point is not enclosed by any lists, return ((t) . (t)).  Port of
    !> elec-pair.el:electric-pair--balance-info.
    subroutine electric_pair_balance_info(direction, string_or_comment, innermost, outermost)
        integer(int64), intent(in) :: direction, string_or_comment
        type(balance_type), intent(out) :: innermost, outermost
        type(handle_type) :: saved
        type(status_type) :: st
        type(parse_state_type) :: state
        character(len=:), allocatable :: msg
        integer(int64) :: opoint, pos, beg, end, count, r
        opoint = point()
        do while (.not. outermost%set)
            if (direction > 0) then
                count = point_max()
            else
                count = -point_max()
            end if
            call with_syntax_begin(string_or_comment, saved)
            r = scan_sexps(point(), count, st)
            call with_syntax_end(saved)
            if (st%ok()) then
                call at_top_level_or_equivalent()
            else if (st%code /= FM_SCAN_ERROR) then
                exit
            else
                call scan_error_data(st, msg, beg, end)
                pos = point()
                call ppss_at(pos, state)
                call goto_char(pos)
                if (index(msg, "ends prematurely") == 0 .or. (string_or_comment /= 0 .and. state%comstr_start == 0)) then
                    ! Some error happened and it is not of the "ended
                    ! prematurely" kind, or we were in a comment and just
                    ! came out of it.
                    call at_top_level_or_equivalent()
                else
                    ! Exit the sexp.
                    call goto_char(end)
                    call ended_prematurely()
                end if
            end if
        end do
        call goto_char(opoint)
    contains
        !> Called when `scan_sexps' ran perfectly, when it found a
        !> parenthesis pointing in the direction of travel.  Also when
        !> travel started inside a comment and exited it.
        subroutine at_top_level_or_equivalent()
            outermost = balance_type(set=.true., matched=.true.)
            if (.not. innermost%set) innermost = balance_type(set=.true., matched=.true.)
        end subroutine at_top_level_or_equivalent

        !> Called when `scan_sexps' crashed against a parenthesis pointing
        !> opposite the direction of travel.  After traversing that
        !> character, the idea is to travel one sexp in the opposite
        !> direction looking for a matching delimiter.
        subroutine ended_prematurely()
            type(parse_state_type) :: ppss
            type(status_type) :: st2
            type(handle_type) :: saved2
            integer(int64) :: p, q, start
            integer :: opener, m, actual_pair
            logical :: matched
            p = point()
            matched = .false.
            if (direction < 0) then
                q = scan_sexps(point(), 1_int64, st2)
                if (q == 0) q = point()
                if (st2%ok()) then
                    call with_syntax_begin(string_or_comment, saved2)
                    m = matching(before(q))
                    call with_syntax_end(saved2)
                    matched = after(p) == m
                end if
            else
                ! In this case, no need to use `scan_sexps', we can use
                ! some `electric_pair_syntax_ppss' in this case (which
                ! uses the quicker `syntax_ppss' in some cases).
                call electric_pair_syntax_ppss(ppss, point() - 1)
                start = 0
                if (allocated(ppss%open_parens)) then
                    if (size(ppss%open_parens) > 0) start = ppss%open_parens(size(ppss%open_parens))
                end if
                if (start /= 0) then
                    opener = after(start)
                    call with_syntax_begin(string_or_comment, saved2)
                    m = matching(opener)
                    call with_syntax_end(saved2)
                    if (m < 0) m = opener
                    matched = before(p) == m
                end if
            end if
            call goto_char(p)
            if (direction > 0) then
                actual_pair = before()
            else
                actual_pair = after()
            end if
            if (.not. innermost%set) innermost = balance_type(set=.true., matched=matched, pair=actual_pair)
            if (.not. matched) outermost = balance_type(set=.true., matched=matched, pair=actual_pair)
        end subroutine ended_prematurely
    end subroutine electric_pair_balance_info

    !> Return true if there are unbalanced strings started by CHAR.  Port
    !> of elec-pair.el:electric-pair--unbalanced-strings-p.
    logical function unbalanced_strings_p(char)
        integer, intent(in) :: char
        type(parse_state_type) :: selector_ppss, relevant_ppss
        integer(int64) :: opoint, moved
        logical :: found
        opoint = point()
        call ppss_at(opoint, selector_ppss)
        if (selector_ppss%incomment /= 0) then
            ! A comment.
            call goto_char(selector_ppss%comstr_start)
            found = forward_comment(point_max())
            moved = skip_syntax_backward(" >!")
            call electric_pair_syntax_ppss(relevant_ppss, point())
        else
            call ppss_at(electric_pair_string_bound_function(), relevant_ppss)
        end if
        call goto_char(opoint)
        unbalanced_strings_p = relevant_ppss%instring == ST_STRING_STYLE .or. relevant_ppss%instring == char
    end function unbalanced_strings_p

    !> Return true if point is inside a string started by CHAR.
    !>
    !> A comments text is parsed with `electric_pair_text_syntax_table'.
    !> Also consider strings within comments, but not strings within
    !> strings.  Port of elec-pair.el:electric-pair--inside-string-p.
    logical function inside_string_p(char)
        integer, intent(in) :: char
        type(parse_state_type) :: ppss
        call electric_pair_syntax_ppss(ppss, point(), comments_only=.true.)
        inside_string_p = ppss%instring == ST_STRING_STYLE .or. ppss%instring == char
    end function inside_string_p

    !> Return true if auto-pairing of CHAR unbalances delimiters.
    !>
    !> Works by first removing the character from the buffer, then doing
    !> some list calculations, finally restoring the situation as if nothing
    !> happened.  Port of elec-pair.el:electric-pair-inhibit-if-helps-balance
    !> (the atomic change group: the deleted text is put back, without
    !> undo records, and the buffer's modified state restored).
    logical function electric_pair_inhibit_if_helps_balance(char) result(inhibit)
        integer, intent(in) :: char
        type(electric_pair_info_type) :: info
        type(balance_type) :: innermost, outermost
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: opoint, beg
        logical :: modified, undo_enabled
        inhibit = .false.
        info = electric_pair_syntax_info(char)
        if (.not. info%found) return
        opoint = point()
        beg = opoint - psi_num()
        call buffer_modified_p(current_buffer(), modified, st)
        undo_enabled = buffers(cur)%undo_enabled
        buffers(cur)%undo_enabled = .false.
        call buffer_substring(beg, opoint, text, st)
        call delete_region(beg, opoint, st)
        if (info%syntax == iachar("(")) then
            call electric_pair_balance_info(1_int64, info%string_or_comment, innermost, outermost)
            if (outermost%matched) then
                inhibit = .false.
            else
                inhibit = outermost%pair == info%pair
            end if
        else if (info%syntax == iachar('"')) then
            inhibit = unbalanced_strings_p(char)
        end if
        call goto_char(beg)
        call insert(text, st)
        buffers(cur)%undo_enabled = undo_enabled
        call restore_buffer_modified_p(modified)
        call goto_char(opoint)
    end function electric_pair_inhibit_if_helps_balance

    !> Return true if skipping CHAR preserves balance of delimiters.
    !> Works by first removing the character from the buffer, then doing
    !> some list calculations, finally restoring the situation as if nothing
    !> happened.  Port of elec-pair.el:electric-pair-skip-if-helps-balance.
    logical function electric_pair_skip_if_helps_balance(char) result(skip)
        integer, intent(in) :: char
        type(electric_pair_info_type) :: info
        type(balance_type) :: innermost, outermost
        type(status_type) :: st
        integer(int64) :: num, k
        skip = .false.
        num = psi_num()
        info = electric_pair_syntax_info(char)
        if (.not. info%found) return
        call delete_char(-num, st)
        if (info%syntax == iachar(")")) then
            call electric_pair_balance_info(-num, info%string_or_comment, innermost, outermost)
            if (outermost%matched) then
                skip = innermost%matched
            else if (innermost%matched) then
                skip = outermost%pair /= info%pair
            end if
        else if (info%syntax == iachar('"')) then
            skip = inside_string_p(char)
        end if
        do k = 1, num
            call insert(char_text(char), st)
        end do
    end function electric_pair_skip_if_helps_balance

    !> The default `electric_pair_skip_self': help balance.  Port of
    !> elec-pair.el:electric-pair-default-skip-self.
    logical function electric_pair_default_skip_self(char) result(skip)
        integer, intent(in) :: char
        if (electric_pair_preserve_balance) then
            skip = electric_pair_skip_if_helps_balance(char)
        else
            skip = .true.
        end if
    end function electric_pair_default_skip_self

    !> The default `electric_pair_inhibit_predicate': help balance.  Port
    !> of elec-pair.el:electric-pair-default-inhibit.
    logical function electric_pair_default_inhibit(char) result(inhibit)
        integer, intent(in) :: char
        if (electric_pair_preserve_balance) then
            inhibit = electric_pair_inhibit_if_helps_balance(char)
        else
            inhibit = electric_pair_conservative_inhibit(char)
        end if
    end function electric_pair_default_inhibit

    !> A conservative `electric_pair_inhibit_predicate'.  Port of
    !> elec-pair.el:electric-pair-conservative-inhibit.
    logical function electric_pair_conservative_inhibit(char) result(inhibit)
        integer, intent(in) :: char
        integer :: fc
        ! I find it more often preferable not to pair when the same char
        ! is next.
        inhibit = char == after()
        ! Don't pair up when we insert the second of "" or of ((.
        if (.not. inhibit .and. char == before()) inhibit = char == before(point() - 1)
        ! I also find it often preferable not to pair next to a word.
        fc = after()
        if (fc < 0) fc = 0
        if (.not. inhibit) inhibit = syntax_char(fc) == "w"
    end function electric_pair_conservative_inhibit

    ! ------------------------------------------------------------------
    ! post-self-insert-hook
    ! ------------------------------------------------------------------

    !> Do main work for `electric_pair_mode'.
    !> This function is added to `post_self_insert_hook' when
    !> `electric_pair_mode' is enabled.
    !>
    !> If the newly inserted character C has delimiter syntax, this
    !> function may decide to insert additional paired delimiters, or
    !> skip the insertion of the new character altogether by jumping
    !> over an existing identical character, or do nothing.
    !>
    !> The decision is taken by order of preference:
    !>
    !> * According to `use_region_p'.  If this returns true this
    !>   function will unconditionally "wrap" the region in the
    !>   corresponding delimiter for C;
    !>
    !> * According to C alone, by looking C up in the tables
    !>   `electric_pair_pairs' or `electric_pair_text_pairs' (which
    !>   see);
    !>
    !> * According to C's syntax and the syntactic state of the buffer
    !>   (both as defined by the major mode's syntax table).  This is
    !>   done by looking up the variables
    !>  `electric_pair_inhibit_predicate', `electric_pair_skip_self'
    !>   and `electric_pair_skip_whitespace' (which see).  Port of
    !> elec-pair.el:electric-pair-post-self-insert-function.
    recursive subroutine electric_pair_post_self_insert_function(status)
        type(status_type), intent(out) :: status
        type(electric_pair_info_type) :: info
        integer(int64) :: pos, num, beg, m, opoint, saved_point, moved
        integer :: ev, skip_whitespace_info, syntax
        logical :: skip, ok, inhibited
        type(value_type) :: saved_arg
        type(handle_type) :: here
        type(status_type) :: st
        status = status_type()
        pos = 0
        if (minor_modes%electric_pair_mode%state%value) pos = electric_after_char_pos()
        num = 0
        if (pos /= 0) num = psi_num()
        beg = pos - num
        ev = event_char()
        info = electric_pair_syntax_info(ev)
        if (.not. info%found) return
        syntax = info%syntax
        if (pos == 0) return
        if (num == 0) return
        if (is_any(syntax, '()"$') .and. use_region_p()) then
            ! Wrap a pair around the active region.
            call mark(m, st)
            if (syntax == iachar('"') .or. (syntax == iachar(")") .and. point() >= m) .or. &
                (syntax /= iachar(")") .and. m >= point())) then
                call save_excursion_begin(here)
                call goto_char(m)
                call electric_pair_insert(info%pair, num, status)
                call save_excursion_end(here)
            else
                call delete_region(beg, pos, status)
                if (.not. status%ok()) return
                call electric_pair_insert(info%pair, num, status)
                if (.not. status%ok()) return
                call mark(m, st)
                call goto_char(m)
                call electric_pair_insert(ev, num, status)
            end if
            return
        end if
        ! Backslash-escaped: no pairing, no skipping.
        opoint = point()
        call goto_char(beg)
        moved = skip_syntax_backward("\")
        call goto_char(opoint)
        if (modulo(moved, 2_int64) /= 0) then
            saved_arg = hook_current_prefix_arg
            hook_current_prefix_arg = make_fixnum(num - 1)
            call electric_pair_post_self_insert_function(status)
            hook_current_prefix_arg = saved_arg
            return
        end if
        ! Skip self.
        skip_whitespace_info = ELECTRIC_PAIR_SKIP_WHITESPACE_NIL
        if (is_any(syntax, ')"$')) then
            ok = info%unconditional
            if (.not. ok .and. associated(electric_pair_skip_self)) then
                saved_point = point()
                call goto_char(pos)
                ok = electric_pair_skip_self(ev)
                call goto_char(saved_point)
            end if
            if (ok) then
                opoint = point()
                if (.not. (info%unconditional .and. syntax == iachar('"'))) then
                    skip_whitespace_info = electric_pair_skip_whitespace
                    if (skip_whitespace_info /= ELECTRIC_PAIR_SKIP_WHITESPACE_NIL .and. &
                        associated(electric_pair_skip_whitespace_function)) call electric_pair_skip_whitespace_function(st)
                end if
                skip = after() == ev
                call goto_char(opoint)
                if (skip) then
                    ! This is too late: rather than insert&delete we'd want
                    ! to only skip (or insert in overwrite mode).
                    if (skip_whitespace_info /= ELECTRIC_PAIR_SKIP_WHITESPACE_NIL .and. &
                        associated(electric_pair_skip_whitespace_function)) call electric_pair_skip_whitespace_function(st)
                    if (skip_whitespace_info == ELECTRIC_PAIR_SKIP_WHITESPACE_CHOMP) then
                        call delete_region(beg, point(), status)
                    else
                        call delete_region(beg, pos, status)
                    end if
                    if (status%ok()) call forward_char(status, num)
                    return
                end if
            end if
        end if
        ! Insert matching pair.
        if (syntax == SYNTAX_STR) then
            ! String pairs.
            if (info%space) call insert(" ", status)
            if (.not. status%ok()) return
            call save_excursion_begin(here)
            call insert(info%pair_string, status)
            call save_excursion_end(here)
        else if (is_any(syntax, '("$')) then
            ! Char pairs.
            inhibited = .false.
            if (.not. info%unconditional .and. associated(electric_pair_inhibit_predicate)) then
                saved_point = point()
                call goto_char(pos)
                inhibited = electric_pair_inhibit_predicate(ev)
                call goto_char(saved_point)
            end if
            if (.not. inhibited) then
                call save_excursion_begin(here)
                call electric_pair_insert(info%pair, num, status)
                call save_excursion_end(here)
            end if
        end if
    contains
        !> Whether character code S is one of the characters of SET.
        logical function is_any(s, set)
            integer, intent(in) :: s
            character(len=*), intent(in) :: set
            integer :: k
            is_any = .false.
            do k = 1, len(set)
                if (s == iachar(set(k:k))) is_any = .true.
            end do
        end function is_any
    end subroutine electric_pair_post_self_insert_function

    !> Honor `electric_pair_open_newline_between_pairs'.
    !> This function is added to `post_self_insert_hook' when
    !> `electric_pair_mode' is enabled.  Port of
    !> elec-pair.el:electric-pair-open-newline-between-pairs-psif.
    recursive subroutine electric_pair_open_newline_between_pairs_psif(status)
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, moved
        integer :: b, m
        type(handle_type) :: here
        status = status_type()
        if (.not. electric_pair_open_newline_between_pairs) return
        if (event_char() /= 10) return
        if (.not. (point_min() + 1 < point() .and. point() < point_max())) return
        opoint = point()
        moved = skip_chars_backward_tabs()
        b = before(point() - psi_num())
        call goto_char(opoint)
        m = matching(after())
        if (b /= m) return
        call save_excursion_begin(here)
        call newline(status, 1_int64, interactive=.true.)
        call save_excursion_end(here)
    contains
        !> (skip-chars-backward "\t\s").
        integer(int64) function skip_chars_backward_tabs() result(n)
            integer(int64) :: p
            p = point()
            do while (p > point_min())
                if (before(p) /= 9 .and. before(p) /= 32) exit
                p = p - 1
            end do
            n = p - point()
            call goto_char(p)
        end function skip_chars_backward_tabs
    end subroutine electric_pair_open_newline_between_pairs_psif

    !> When between adjacent paired delimiters, delete both of them.
    !> ARG and KILLP are passed directly to
    !> `backward_delete_char_untabify', which see.  Port of
    !> elec-pair.el:electric-pair-delete-pair.
    subroutine electric_pair_delete_pair(arg, status, killp)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: killp
        call delete_char(arg, status)
        if (status%ok()) call backward_delete_char_untabify(arg, status, killp)
    end subroutine electric_pair_delete_pair

    !> When between adjacent paired delimiters, delete both of them.
    !> ARG and KILLP are passed directly to
    !> `backward_delete_char_untabify', which see.  Port of
    !> elec-pair.el:electric-pair-delete-pair (the command, "*p\nP").
    subroutine cmd_electric_pair_delete_pair(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call electric_pair_delete_pair(prefix_numeric_value(current_prefix_arg), status, &
                                       .not. nilp(current_prefix_arg))
    end subroutine cmd_electric_pair_delete_pair

    ! ------------------------------------------------------------------
    ! The modes
    ! ------------------------------------------------------------------

    !> Toggle automatic pairing of delimiters (Electric Pair mode).
    !>
    !> Electric Pair mode is a global minor mode.  When enabled, typing an
    !> opening delimiter (parenthesis, bracket, etc.) automatically inserts the
    !> corresponding closing delimiter.  If the region is active, the
    !> delimiters are inserted around the region instead.
    !>
    !> To toggle the mode only in the current buffer, use
    !> `electric_pair_local_mode'.  Port of elec-pair.el:electric-pair-mode.
    subroutine electric_pair_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%electric_pair_mode%command(status)
    end subroutine electric_pair_mode

    !> The body of Electric Pair mode.
    subroutine electric_pair_mode_body(on, status)
        logical, intent(in) :: on
        type(status_type), intent(out) :: status
        status = status_type()
        if (on) continue
        if (minor_modes%electric_pair_mode%state%value) then
            ! Prioritize this to kick in after
            ! `electric_layout_post_self_insert_function': that
            ! considerably simplifies interoperation when
            ! `electric_pair_mode', `electric_layout_mode' and
            ! `electric_indent_mode' are used together.
            call post_self_insert_hook%add(electric_pair_post_self_insert_function, depth=50)
            call post_self_insert_hook%add(electric_pair_open_newline_between_pairs_psif, depth=50)
        else
            call post_self_insert_hook%remove(electric_pair_post_self_insert_function)
            call post_self_insert_hook%remove(electric_pair_open_newline_between_pairs_psif)
        end if
    end subroutine electric_pair_mode_body

    !> Toggle `electric_pair_mode' only in this buffer.  Port of
    !> elec-pair.el:electric-pair-local-mode.
    subroutine electric_pair_local_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%electric_pair_local_mode%command(status)
    end subroutine electric_pair_local_mode

    !> The state of Electric Pair Local mode: `electric_pair_mode' in the
    !> current buffer.
    logical function electric_pair_local_mode_p() result(on)
        on = minor_modes%electric_pair_mode%state%value
    end function electric_pair_local_mode_p

    !> The body of Electric Pair Local mode with its new state VAL: the
    !> setter (setq-local electric-pair-mode VAL), then the body.
    subroutine electric_pair_local_mode_body(val, status)
        logical, intent(in) :: val
        type(status_type), intent(out) :: status
        type(value_type) :: default
        type(status_type) :: st
        status = status_type()
        call minor_modes%electric_pair_mode%state%set_local(val)
        call default_value(sym("electric_pair_mode"), default, st)
        if (minor_modes%electric_pair_mode%state%value .eqv. .not. nilp(default)) then
            call kill_local_variable(sym("electric_pair_mode"), st)
        else if (nilp(default)) then
            ! Locally enabled, but globally disabled.
            ! Setup the hooks.
            call minor_modes%electric_pair_mode%enable(status)
            ! But keep it globally disabled.
            call set_default(sym("electric_pair_mode"), nil(), st)
        end if
    end subroutine electric_pair_local_mode_body
end module elec_pair_fm
