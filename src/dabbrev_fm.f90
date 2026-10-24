! dabbrev_fm.f90 -- dynamic abbreviation package.
! Ported from GNU Emacs 31.1 lisp/dabbrev.el.
!
! Copyright (C) 1985-1986, 1992, 1994, 1996-1997, 2000-2026 Free
! Software Foundation, Inc.
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

!> Dynamic abbreviations: write just a few characters of words written
!> earlier and expand them with M-/ (`dabbrev_expand'), which searches
!> backward, then forward in the current buffer, then in other buffers.
!>
!> Settings whose upstream values are nil, t or a symbol are integers
!> with the named constants DABBREV_OFF (nil), DABBREV_ON (t),
!> DABBREV_LIKE_CASE_FOLD_SEARCH (`case-fold-search'),
!> DABBREV_LIKE_CASE_REPLACE (`case-replace') and DABBREV_ASK (another
!> value of `dabbrev_check_other_buffers').
!>
!> Deferred: `dabbrev_completion' (C-M-/, `dabbrev_capf' and
!> `dabbrev__find_all_expansions': completion-at-point in buffers is not
!> ported yet) and Picture mode's `picture_replace_match'.
module dabbrev_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, format_message, FM_USER_ERROR, FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: nilp
    use data_fm, only: value_eq
    use buffer_fm, only: buffers, cur, BEG, current_buffer, set_buffer, buffer_live_p, buffer_list, resolve_buffer, &
                         get_buffer, get_buffer_create, generate_new_buffer_name, kill_buffer, &
                         barf_if_buffer_read_only, UE_POINT, undo_entry_type
    use marker_fm, only: copy_marker_at, marker_position, marker_buffer, free_marker
    use editfns_fm, only: point, point_min, point_max, goto_char, bobp, preceding_char, buffer_substring, &
                          delete_region, widen, narrow_to_region, field_beginning, point_marker
    use insdel_fm, only: insert
    use search_fm, only: re_search_forward, re_search_backward, search_forward, search_backward, looking_at, &
                         string_match, match_beginning, match_end, regexp_quote, replace_match, case_fold_search, &
                         SEARCH_NOERROR
    use casefiddle_fm, only: upcase_string, downcase_string
    use replace_fm, only: case_replace
    use undo_fm, only: push_undo_entry
    use subr_fm, only: y_or_n_p, progress_reporter_type, make_progress_reporter, progress_reporter_update, &
                       progress_reporter_done
    use minibuf_fm, only: minibuffer_prompt_end, minibuf_level
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: last_command, this_command
    use frame_fm, only: selected_frame, frame_selected_window_ref
    use window_fm, only: windows, window_leaves, get_mru_window, same_handle, window_type
    use derived_fm, only: provided_mode_derived_p
    use xdisp_fm, only: message
    implicit none(type, external)
    private

    public :: dabbrev_expand, dabbrev_reset_global_variables, dabbrev_same_major_mode_p, dabbrev_select_buffers, &
              dabbrev_friend_buffer_proc, dabbrev_select_buffers_proc, syms_of_dabbrev

    !> Values of the dabbrev settings that upstream are nil, t or a
    !> symbol: off (nil), on (t), like `case_fold_search', like
    !> `case_replace', and ask (another value).
    integer, parameter, public :: DABBREV_OFF = 0, DABBREV_ON = 1, DABBREV_LIKE_CASE_FOLD_SEARCH = 2, &
                                  DABBREV_LIKE_CASE_REPLACE = 3, DABBREV_ASK = 4

    abstract interface
        !> A function deciding whether dabbrev should search OTHER_BUFFER.
        logical function dabbrev_friend_buffer_proc(other_buffer)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: other_buffer
        end function dabbrev_friend_buffer_proc

        !> A function giving in LIST the buffers that should be
        !> searched by dabbrev.
        subroutine dabbrev_select_buffers_proc(list)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), allocatable, intent(out) :: list(:)
        end subroutine dabbrev_select_buffers_proc
    end interface

    !> If true, `dabbrev_expand' only looks backwards.  Port of dabbrev.el
    !> dabbrev-backward-only.
    logical, public :: dabbrev_backward_only = .false.

    !> Limits region searched by `dabbrev_expand' to this many chars away
    !> (unallocated: off).  Port of dabbrev.el dabbrev-limit.
    integer(int64), allocatable, public :: dabbrev_limit

    !> Regexp for skipping leading characters of an abbreviation.
    !>
    !> Example: Set this to "\\$" for programming languages
    !> in which variable names may appear with or without a leading `$'.
    !> (For example, in Makefiles.)
    !>
    !> Leave this unallocated if no characters should be skipped.  Port of
    !> dabbrev.el dabbrev-abbrev-skip-leading-regexp.
    character(len=:), allocatable, public :: dabbrev_abbrev_skip_leading_regexp

    !> True means dabbrev should not insert newlines.
    !> Instead it converts them to spaces.  Port of dabbrev.el
    !> dabbrev-eliminate-newlines.
    logical, public :: dabbrev_eliminate_newlines = .true.

    !> Control whether dabbrev searches should ignore case.
    !> A value of DABBREV_OFF means case is significant.
    !> A value of DABBREV_LIKE_CASE_FOLD_SEARCH means case is significant
    !>  if `case_fold_search' is false.
    !> Any other value means case is not significant.  Port of dabbrev.el
    !> dabbrev-case-fold-search.
    integer, public :: dabbrev_case_fold_search = DABBREV_LIKE_CASE_FOLD_SEARCH

    !> The significance of an uppercase character in an abbreviation.
    !> A false value means case fold search when searching for possible expansions;
    !> true means case sensitive search.
    !>
    !> This variable has an effect only when the value of
    !> `dabbrev_case_fold_search' says to ignore case.  Port of dabbrev.el
    !> dabbrev-upcase-means-case-search.
    logical, public :: dabbrev_upcase_means_case_search = .false.

    !> Whether dabbrev treats expansions as the same if they differ in case.
    !>
    !> A value of DABBREV_OFF means treat them as different.
    !> A value of DABBREV_LIKE_CASE_REPLACE means distinguish them if
    !> `case_replace' is false.
    !> Any other value means to treat them as the same.
    !>
    !> This variable has an effect only when the value of
    !> `dabbrev_case_fold_search' specifies to ignore case.  Port of
    !> dabbrev.el dabbrev-case-distinction.
    integer, public :: dabbrev_case_distinction = DABBREV_LIKE_CASE_REPLACE

    !> Whether dabbrev applies the abbreviations's case pattern to the expansion.
    !>
    !> A value of DABBREV_OFF means preserve the expansion's case pattern.
    !> A value of DABBREV_LIKE_CASE_REPLACE means preserve it if `case_replace'
    !> is false.
    !> Any other value means modify the expansion
    !> by applying the abbreviation's case pattern to it.
    !>
    !> This variable has an effect only when the value of
    !> `dabbrev_case_fold_search' specifies to ignore case.  Port of
    !> dabbrev.el dabbrev-case-replace.
    integer, public :: dabbrev_case_replace = DABBREV_LIKE_CASE_REPLACE

    !> Regexp to recognize a character in an abbreviation or expansion.
    !> This regexp will be surrounded with \\( ... \\) when actually used.
    !>
    !> Set this variable to "\\sw" if you want ordinary words or
    !> "\\sw\\|\\s_" if you want symbols (including characters
    !> whose syntax is "symbol" as well as those whose syntax is
    !> "word").  The abbreviation is from point to the start of the
    !> previous sequence of characters matching this variable.
    !>
    !> The default value (unallocated) is equivalent to "\\sw\\|\\s_".
    !>
    !> For instance, suppose the current buffer is in `c_mode'.  If this
    !> variable is unallocated or "\\sw\\|\\s_", then expanding
    !> `debug_print_in_' looks for a symbol starting with
    !> `debug_print_in_'.  If you set this variable to "\\sw", that
    !> expansion looks for a word prefixed with `in_' (e.g., it would
    !> match `in_range', but not `in_close_range').  If expanding
    !> `debug_print_in' it would look for a word starting with
    !> `in' (e.g. `integer').  Port of dabbrev.el dabbrev-abbrev-char-regexp.
    character(len=:), allocatable, public :: dabbrev_abbrev_char_regexp

    !> True means dabbrev package should search *all* buffers.
    !>
    !> Dabbrev always searches the current buffer first.  Then, if
    !> `dabbrev_check_other_buffers' says so, it searches the buffers
    !> designated by `dabbrev_select_buffers_function'.
    !>
    !> Then, if `dabbrev_check_all_buffers' is true, dabbrev searches
    !> all the other buffers, except those named in `dabbrev_ignored_buffer_names',
    !> or matched by `dabbrev_ignored_buffer_regexps'.  Port of dabbrev.el
    !> dabbrev-check-all-buffers.
    logical, public :: dabbrev_check_all_buffers = .true.

    !> List of buffer names that dabbrev should not check.
    !> See also `dabbrev_ignored_buffer_regexps' and
    !> `dabbrev_ignored_buffer_modes'.  Port of dabbrev.el
    !> dabbrev-ignored-buffer-names (trailing blanks are ignored).
    character(len=:), allocatable, public :: dabbrev_ignored_buffer_names(:)

    !> List of regexps matching names of buffers that dabbrev should not check.
    !> See also `dabbrev_ignored_buffer_names' and
    !> `dabbrev_ignored_buffer_modes'.  Port of dabbrev.el
    !> dabbrev-ignored-buffer-regexps (trailing blanks are ignored).
    character(len=:), allocatable, public :: dabbrev_ignored_buffer_regexps(:)

    !> Inhibit looking for abbreviations in buffers derived from these modes.
    !> See also `dabbrev_ignored_buffer_names' and
    !> `dabbrev_ignored_buffer_regexps'.  Port of dabbrev.el
    !> dabbrev-ignored-buffer-modes (trailing blanks are ignored).
    character(len=:), allocatable, public :: dabbrev_ignored_buffer_modes(:)

    !> Should \[dabbrev_expand] look in other buffers?
    !> DABBREV_OFF: Don't look in other buffers.
    !> DABBREV_ON: Also look for expansions in the buffers pointed out by
    !>    `dabbrev_select_buffers_function'.
    !> DABBREV_ASK: When we can't find any more expansions in
    !> the current buffer, then ask the user whether to look in other
    !> buffers too.
    !>
    !> The default value is DABBREV_ON.  Port of dabbrev.el
    !> dabbrev-check-other-buffers.
    integer, public :: dabbrev_check_other_buffers = DABBREV_ON

    !> A function that selects buffers that should be searched by dabbrev.
    !> The function should take no arguments and return a list of buffers to
    !> search for expansions.  See the source of `dabbrev_select_buffers'
    !> for an example.  Port of dabbrev.el dabbrev-select-buffers-function
    !> (null is `dabbrev_select_buffers', its default).
    procedure(dabbrev_select_buffers_proc), pointer, public :: dabbrev_select_buffers_function => null()

    !> A function to decide whether dabbrev should search OTHER_BUFFER.
    !> The function should take one argument, OTHER_BUFFER, and return
    !> true if that buffer should be searched.  Have a look at
    !> `dabbrev_same_major_mode_p' for an example.  Port of dabbrev.el
    !> dabbrev-friend-buffer-function (null is
    !> `dabbrev_same_major_mode_p', its default).
    procedure(dabbrev_friend_buffer_proc), pointer, public :: dabbrev_friend_buffer_function => null()

    !> If allocated, a list of buffers which dabbrev should search (their
    !> names).
    !> If this variable is allocated and not empty, dabbrev will only look in
    !> these buffers.  It will not even look in the current buffer if it is not a
    !> member of this list.  Port of dabbrev.el
    !> dabbrev-search-these-buffers-only.
    character(len=:), allocatable, public :: dabbrev_search_these_buffers_only(:)

    !> A string, for the tables of strings.
    type :: string_type
        character(len=:), allocatable :: s
    end type string_type

    ! Table of expansions seen so far.
    type(string_type), allocatable :: last_table(:)
    integer :: n_last_table = 0

    ! Last string we tried to expand (unallocated for nil).
    character(len=:), allocatable :: last_abbreviation

    ! Location last abbreviation began: a marker, the null handle for nil.
    type(handle_type) :: last_abbrev_location

    ! Direction of last dabbrevs search (nil when `last_direction_set' is
    ! false).
    integer(int64) :: last_direction = 0
    logical :: last_direction_set = .true.

    ! Last expansion of an abbreviation.
    character(len=:), allocatable :: last_expansion

    ! Location the last expansion was found: a position, or a marker
    ! (`location_marker'), or nil (`location_set' false).
    logical :: location_set = .false.
    integer(int64) :: location_pos = 0
    type(handle_type) :: location_marker

    ! The list of remaining buffers with the same mode as current buffer.
    type(handle_type), allocatable :: friend_buffer_list(:)

    ! The buffer we looked in last, not counting the current buffer.
    type(handle_type) :: last_buffer

    ! The buffer we found the expansion last time.
    type(handle_type) :: last_buffer_found

    ! If not CASE_PATTERN_NIL, the function to use when copying
    ! successive words: upcase or downcase.
    integer, parameter :: CASE_PATTERN_NIL = 0, CASE_PATTERN_UPCASE = 1, CASE_PATTERN_DOWNCASE = 2
    integer :: last_case_pattern = CASE_PATTERN_NIL

    ! Same as dabbrev_check_other_buffers, but is set for every expand.
    integer :: check_other_buffers = DABBREV_ON

    ! Same as dabbrev_check_all_buffers, but is set for every expand.
    logical :: check_all_buffers = .true.

    ! The regexp for recognizing a character in an abbreviation.
    character(len=:), allocatable :: abbrev_char_regexp

    ! The progress reporter for buffer-scanning progress.
    type(progress_reporter_type) :: progress_reporter

    ! A saved point and current buffer (save-excursion).
    type :: excursion_type
        type(handle_type) :: buffer, point
    end type excursion_type

    character(len=*), parameter :: DEFAULT_CHAR_REGEXP = "\sw\|\s_"
    character(len=*), parameter :: LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> Save the current buffer and point (save-excursion).
    subroutine save_excursion(e)
        type(excursion_type), intent(out) :: e
        e%buffer = current_buffer()
        call point_marker(e%point)
    end subroutine save_excursion

    !> Go back to the buffer and point E saved.
    subroutine restore_excursion(e)
        type(excursion_type), intent(inout) :: e
        type(status_type) :: st
        integer(int64) :: pos
        type(handle_type) :: b
        if (buffer_live_p(e%buffer)) then
            call set_buffer(e%buffer, st)
            call marker_buffer(e%point, b, st)
            if (same_handle(b, e%buffer)) then
                call marker_position(e%point, pos, st)
                call goto_char(pos)
            end if
        end if
        call free_marker(e%point, st)
    end subroutine restore_excursion

    !> The number of characters of the UTF-8 text S.
    pure integer(int64) function nchars(s) result(n)
        character(len=*), intent(in) :: s
        integer :: k, b
        n = 0
        do k = 1, len(s)
            b = ichar(s(k:k))
            if (b < 128 .or. b >= 192) n = n + 1
        end do
    end function nchars

    !> The byte index in S of the character after the first N characters
    !> (len(s) + 1 if S has no more).
    pure integer function byte_after(s, n) result(k)
        character(len=*), intent(in) :: s
        integer(int64), intent(in) :: n
        integer(int64) :: m
        integer :: b
        m = 0
        do k = 1, len(s)
            b = ichar(s(k:k))
            if (b < 128 .or. b >= 192) then
                if (m == n) return
                m = m + 1
            end if
        end do
        k = len(s) + 1
    end function byte_after

    !> Whether A and B are the same string (Fortran's == ignores
    !> trailing blanks).
    pure logical function same_string(a, b)
        character(len=*), intent(in) :: a, b
        same_string = .false.
        if (len(a) /= len(b)) return
        same_string = a == b
    end function same_string

    !> The position of the last expansion: its marker's, or the number.
    integer(int64) function location()
        type(status_type) :: st
        location = location_pos
        if (location_marker%index /= 0) call marker_position(location_marker, location, st)
    end function location

    !> Set the location of the last expansion to the position POS.
    subroutine set_location(pos)
        integer(int64), intent(in) :: pos
        call clear_location()
        location_set = .true.
        location_pos = pos
    end subroutine set_location

    !> Make the location of the last expansion nil.
    subroutine clear_location()
        type(status_type) :: st
        if (location_marker%index /= 0) call free_marker(location_marker, st)
        location_marker = null_handle()
        location_set = .false.
        location_pos = 0
    end subroutine clear_location

    !> Whether `last_abbrev_location' is a marker with a position.
    logical function abbrev_location_p()
        integer(int64) :: pos
        type(status_type) :: st
        abbrev_location_p = .false.
        if (last_abbrev_location%index == 0) return
        call marker_position(last_abbrev_location, pos, st)
        abbrev_location_p = st%ok() .and. pos > 0
    end function abbrev_location_p

    !> The position of `last_abbrev_location'.
    integer(int64) function abbrev_location()
        type(status_type) :: st
        call marker_position(last_abbrev_location, abbrev_location, st)
    end function abbrev_location

    !> Record point POS in the current buffer's undo list
    !> (`(setq buffer-undo-list (cons POS buffer-undo-list))').
    subroutine push_undo_point(pos)
        integer(int64), intent(in) :: pos
        type(undo_entry_type) :: entry
        if (.not. buffers(cur)%undo_enabled) return
        entry%kind = UE_POINT
        entry%beg = pos
        call push_undo_entry(entry)
    end subroutine push_undo_point

    !> The user error "No possible abbreviation preceding point".
    subroutine no_abbreviation(status)
        type(status_type), intent(out) :: status
        call set_error(status, FM_USER_ERROR, "No possible abbreviation preceding point")
    end subroutine no_abbreviation

    !> Whether the selected window is the active minibuffer window
    !> (`minibuffer_window_active_p' of the selected window).
    logical function minibuffer_window_active()
        minibuffer_window_active = selected_frame%minibuffer_selected .and. minib_active()
    end function minibuffer_window_active

    logical function minib_active()
        minib_active = minibuf_level > 0
    end function minib_active

    !> Get the buffer from which mini-buffer.  Port of
    !> dabbrev.el:dabbrev--minibuffer-origin.
    function minibuffer_origin() result(b)
        type(handle_type) :: b
        type(window_type), pointer :: w
        w => frame_selected_window_ref()
        b = w%buffer
    end function minibuffer_origin

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Return true if case is ignored in searches for ABBREV.  Port of
    !> dabbrev.el:dabbrev--ignore-case-p.
    logical function ignore_case_p(abbrev)
        character(len=*), intent(in) :: abbrev
        if (dabbrev_case_fold_search == DABBREV_LIKE_CASE_FOLD_SEARCH) then
            ignore_case_p = case_fold_search%value
        else
            ignore_case_p = dabbrev_case_fold_search /= DABBREV_OFF
        end if
        if (ignore_case_p .and. dabbrev_upcase_means_case_search) then
            ignore_case_p = same_string(abbrev, downcase_string(abbrev))
        end if
    end function ignore_case_p

    !> Expand previous word "dynamically".
    !>
    !> Expands to the most recent, preceding word for which this is a prefix.
    !> If no suitable preceding word is found, words following point are
    !> considered.  If still no suitable word is found, then look in the
    !> buffers accepted by the function pointed out by variable
    !> `dabbrev_friend_buffer_function', if `dabbrev_check_other_buffers'
    !> says so.  Then, if `dabbrev_check_all_buffers' is true, look in
    !> all the other buffers, subject to constraints specified
    !> by `dabbrev_ignored_buffer_names' and `dabbrev_ignored_buffer_regexps'.
    !>
    !> A positive prefix argument, N, says to take the Nth backward *distinct*
    !> possibility.  A negative argument says search forward.
    !>
    !> If the cursor has not moved from the end of the previous expansion and
    !> no argument is given, replace the previously-made expansion
    !> with the next possible expansion not yet tried.
    !>
    !> The variable `dabbrev_backward_only' may be used to limit the
    !> direction of search to backward if set true.
    !>
    !> See also `dabbrev_abbrev_char_regexp'.  Port of
    !> dabbrev.el:dabbrev-expand (ARG absent is nil, else its numeric
    !> value).
    recursive subroutine dabbrev_expand(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        type(handle_type) :: buf, mru
        type(excursion_type) :: ex, ex2, ex3
        character(len=:), allocatable :: abbrev, old, expansion, name
        integer(int64) :: orig_point, direction, found, savedb, savedz, pos
        logical :: record_case_pattern, repeat, space, have_old, lb_same
        type(status_type) :: st
        type(handle_type) :: m
        status = status_type()
        ! There are three possible sources of the expansion, which we need to
        ! check in a specific order:
        if (selected_frame%minibuffer_selected) then
            ! If we invoked dabbrev-expand in the minibuffer,
            ! this is the buffer from which we entered the
            ! minibuffer.
            mru = get_mru_window(.false., .false.)
            buf = current_buffer()
            if (mru%index /= 0) buf = windows(mru%index)%buffer
        else if (buffer_live_p(last_buffer_found)) then
            ! Otherwise, if we found the expansion in another
            ! buffer and that buffer is still live, use that
            ! buffer for further expansions.
            buf = last_buffer_found
        else
            ! Otherwise, use the buffer where we invoked
            ! dabbrev-expand.
            buf = current_buffer()
        end if
        orig_point = point()
        record_case_pattern = .false.
        have_old = .false.
        direction = 0
        ! abbrev -- the abbrev to expand
        ! expansion -- the expansion found (eventually) or nil until then
        ! old -- the text currently in the buffer
        !    (the abbrev, or the previously-made expansion)
        call save_excursion(ex)
        repeat = .false.
        if (.not. present(arg) .and. abbrev_location_p()) then
            if (value_eq(last_command, this_command)) then
                repeat = .true.
            else if (selected_frame%minibuffer_selected) then
                repeat = abbrev_location() == point()
            end if
        end if
        if (repeat) then
            ! Find a different expansion for the same abbrev as last time.
            last_buffer_found = null_handle()
            abbrev = ""
            if (allocated(last_abbreviation)) abbrev = last_abbreviation
            if (allocated(last_expansion)) then
                old = last_expansion
                have_old = .true.
            end if
            direction = last_direction
        else
            ! If the user inserts a space after expanding
            ! and then asks to expand again, always fetch the next word.
            space = preceding_char() == 32
            if (space) space = abbrev_location_p()
            ! Comparing with point only makes sense in the buffer
            ! where we called dabbrev-expand, but if that differs
            ! from the buffer containing the expansion, we want to
            ! get the next word in the latter buffer, so we skip
            ! the comparison.
            if (space .and. same_handle(buf, current_buffer())) space = point() == 1 + abbrev_location()
            if (space) then
                ! The "abbrev" to expand is just the space.
                abbrev = " "
                call save_excursion(ex2)
                if (buffer_live_p(last_buffer)) call set_buffer(last_buffer, st)
                savedb = buffers(cur)%begv
                savedz = buffers(cur)%text%z - buffers(cur)%zv
                call widen()
                ! Find the end of the last "expansion" word.
                if (last_direction_set) then
                    if (last_direction == 1 .or. (last_direction == 0 .and. location() < point())) then
                        call set_location(location() + nchars(last_expansion))
                    end if
                end if
                call goto_char(location())
                ! Take the following word, with intermediate separators,
                ! as our expansion this time.
                call re_search_forward("\(?:"//abbrev_char_regexp//"\)+", found, status)
                if (status%ok()) then
                    call buffer_substring(location(), point(), expansion, status)
                    ! Record the end of this expansion, in case we repeat this.
                    call set_location(point())
                end if
                call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
                call restore_excursion(ex2)
                if (.not. status%ok()) then
                    call restore_excursion(ex)
                    return
                end if
                ! Indicate that dabbrev--last-expansion-location is
                ! at the end of the expansion.
                last_direction = -1
                last_direction_set = .true.
            else
                ! We have a different abbrev to expand.
                call dabbrev_reset_global_variables()
                if (.not. present(arg)) then
                    direction = merge(1, 0, dabbrev_backward_only)
                else
                    direction = arg
                end if
                call abbrev_at_point(abbrev, status)
                if (.not. status%ok()) then
                    call restore_excursion(ex)
                    return
                end if
                record_case_pattern = .true.
            end if
        end if
        ! Find the expansion.
        if (.not. allocated(expansion)) call find_expansion(abbrev, direction, ignore_case_p(abbrev), expansion, status)
        call restore_excursion(ex)
        if (.not. status%ok()) return
        if (.not. allocated(expansion)) then
            call dabbrev_reset_global_variables()
            if (have_old) then
                call save_excursion(ex3)
                call push_undo_point(orig_point)
                ! Put back the original abbrev with its original case pattern.
                call search_backward(old, found, status)
                if (status%ok()) call insert(abbrev, status)
                if (status%ok()) call delete_region(point(), point() + nchars(old), status)
                call restore_excursion(ex3)
                if (.not. status%ok()) return
            end if
            if (have_old) then
                call set_error(status, FM_USER_ERROR, format_message("No further dynamic expansion for `")//abbrev// &
                               format_message("' found"))
            else
                call set_error(status, FM_USER_ERROR, format_message("No dynamic expansion for `")//abbrev// &
                               format_message("' found"))
            end if
            return
        end if
        lb_same = same_handle(last_buffer, last_buffer_found)
        ! If we are in the minibuffer and an expansion has
        ! been found but dabbrev--last-buffer-found is not
        ! yet set, we need to set it now.
        if (.not. lb_same .and. last_buffer_found%index /= 0) lb_same = minibuffer_window_active()
        if (.not. lb_same) then
            if (last_buffer%index == 0) then
                name = buffers(cur)%name
            else if (buffer_live_p(last_buffer)) then
                call resolve_buffer(last_buffer, pos, st)
                name = buffers(pos)%name
            end if
            if (allocated(name)) call message(format_message("Expansion found in `")//name//format_message("'"))
            last_buffer_found = last_buffer
        else
            call message("")
        end if
        ! To get correct further expansions we have to be sure to use the
        ! buffer containing the already found expansions.
        if (buffer_live_p(last_buffer_found)) buf = last_buffer_found
        ! If the buffer where we called dabbrev-expand differs from the
        ! buffer containing the expansion, make sure copy-marker is
        ! called in the latter buffer.
        call save_excursion(ex3)
        call set_buffer(buf, st)
        if (location_set .and. location_marker%index == 0) then
            if (same_handle(current_buffer(), last_buffer) .or. last_buffer%index == 0 .or. &
                buffer_live_p(last_buffer)) then
                if (location_pos > point()) then
                    call copy_marker_at(location_pos, m, .false., st)
                    location_marker = m
                end if
            end if
        end if
        call restore_excursion(ex3)
        ! Success: stick it in and return.
        call push_undo_point(orig_point)
        if (.not. have_old) old = ""
        call substitute_expansion(old, have_old, abbrev, expansion, record_case_pattern, status)
        if (.not. status%ok()) return
        ! Save state for re-expand (making sure it's the state of the
        ! buffer containing the already found expansions).
        call save_excursion(ex3)
        call set_buffer(buf, st)
        last_expansion = expansion
        last_abbreviation = abbrev
        if (last_abbrev_location%index /= 0) call free_marker(last_abbrev_location, st)
        call point_marker(last_abbrev_location)
        call restore_excursion(ex3)
    end subroutine dabbrev_expand

    !> Check if OTHER_BUFFER has the same major mode as current buffer.
    !> Port of dabbrev.el:dabbrev--same-major-mode-p.
    logical function dabbrev_same_major_mode_p(other_buffer) result(same)
        type(handle_type), intent(in) :: other_buffer
        integer(int64) :: i
        type(status_type) :: st
        same = .false.
        call resolve_buffer(other_buffer, i, st)
        if (.not. st%ok()) return
        if (allocated(buffers(cur)%major_mode) .and. allocated(buffers(i)%major_mode)) then
            same = same_string(buffers(cur)%major_mode, buffers(i)%major_mode)
        else
            same = allocated(buffers(cur)%major_mode) .eqv. allocated(buffers(i)%major_mode)
        end if
    end function dabbrev_same_major_mode_p

    !> Back over all abbrev type characters then move forward over all skip characters.
    !> Port of dabbrev.el:dabbrev--goto-start-of-abbrev.
    recursive subroutine goto_start_of_abbrev()
        type(status_type) :: st
        character(len=:), allocatable :: re
        re = DEFAULT_CHAR_REGEXP
        if (allocated(abbrev_char_regexp)) re = abbrev_char_regexp
        ! Move backwards over abbrev chars
        if (point() > minibuffer_prompt_end()) then
            call goto_char(point() - 1)
            do
                if (.not. looking_at(re, st)) exit
                if (.not. point() > minibuffer_prompt_end()) exit
                if (point() == field_beginning(point(), .false., point() - 1)) exit
                call goto_char(point() - 1)
            end do
            if (.not. looking_at(re, st)) call goto_char(point() + 1)
        end if
        if (allocated(dabbrev_abbrev_skip_leading_regexp)) then
            do while (looking_at(dabbrev_abbrev_skip_leading_regexp, st))
                if (point() >= point_max()) exit
                call goto_char(point() + 1)
            end do
        end if
    end subroutine goto_start_of_abbrev

    !> Extract the symbol at point to serve as abbreviation.  Port of
    !> dabbrev.el:dabbrev--abbrev-at-point.
    recursive subroutine abbrev_at_point(abbrev, status)
        character(len=:), allocatable, intent(out) :: abbrev
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: re
        integer(int64) :: end, found
        logical :: after
        type(status_type) :: st
        ! Check for error
        if (bobp()) then
            call no_abbreviation(status)
            return
        end if
        status = status_type()
        re = DEFAULT_CHAR_REGEXP
        if (allocated(dabbrev_abbrev_char_regexp)) re = dabbrev_abbrev_char_regexp
        ! Record the end of the abbreviation.
        end = point()
        ! If we aren't right after an abbreviation,
        ! move point back to just after one.
        ! This is so the user can get successive words
        ! by typing the punctuation followed by M-/.
        call goto_char(end - 1)
        after = looking_at(re, st)
        call goto_char(end)
        if (.not. after) then
            call re_search_backward(re, found, st, noerror=SEARCH_NOERROR)
            if (found > 0) then
                call goto_char(point() + 1)
            else
                call goto_char(end)
                call no_abbreviation(status)
                return
            end if
        end if
        ! Now find the beginning of that one.
        call goto_start_of_abbrev()
        call buffer_substring(point(), end, abbrev, status)
        call goto_char(end)
    end subroutine abbrev_at_point

    !> Initialize all global variables.  Port of
    !> dabbrev.el:dabbrev--reset-global-variables.
    subroutine dabbrev_reset_global_variables()
        type(status_type) :: st
        n_last_table = 0
        if (allocated(last_table)) deallocate (last_table)
        if (allocated(last_abbreviation)) deallocate (last_abbreviation)
        if (last_abbrev_location%index /= 0) call free_marker(last_abbrev_location, st)
        last_abbrev_location = null_handle()
        last_direction = 0
        last_direction_set = .false.
        if (allocated(last_expansion)) deallocate (last_expansion)
        call clear_location()
        if (allocated(friend_buffer_list)) deallocate (friend_buffer_list)
        last_buffer = null_handle()
        last_buffer_found = null_handle()
        if (allocated(dabbrev_abbrev_char_regexp)) then
            abbrev_char_regexp = dabbrev_abbrev_char_regexp
        else
            abbrev_char_regexp = DEFAULT_CHAR_REGEXP
        end if
        check_other_buffers = dabbrev_check_other_buffers
        check_all_buffers = dabbrev_check_all_buffers
    end subroutine dabbrev_reset_global_variables

    !> Return a list of other buffers to search for a possible abbrev.
    !> The current buffer is not included in the list.
    !>
    !> This function makes a list of all the buffers returned by
    !> `buffer_list', then discards buffers whose names match
    !> `dabbrev_ignored_buffer_names' or
    !> `dabbrev_ignored_buffer_regexps', and major modes that match
    !> `dabbrev_ignored_buffer_modes'.  It also discards buffers for
    !> which `dabbrev_friend_buffer_function' returns
    !> false when called with the buffer as argument.  It returns the list
    !> of the buffers that are not discarded.  Port of
    !> dabbrev.el:dabbrev--select-buffers.
    subroutine dabbrev_select_buffers(list)
        type(handle_type), allocatable, intent(out) :: list(:)
        type(handle_type), allocatable :: all(:)
        integer :: k, n
        logical :: keep
        call filter_buffer_modes(all)
        allocate (list(size(all)))
        n = 0
        do k = 1, size(all)
            if (same_handle(all(k), current_buffer())) cycle
            if (ignore_buffer_p(all(k))) cycle
            if (associated(dabbrev_friend_buffer_function)) then
                keep = dabbrev_friend_buffer_function(all(k))
            else
                keep = dabbrev_same_major_mode_p(all(k))
            end if
            if (.not. keep) cycle
            n = n + 1
            list(n) = all(k)
        end do
        list = list(1:n)
    end subroutine dabbrev_select_buffers

    !> The live buffers whose major mode is not derived from
    !> `dabbrev_ignored_buffer_modes'.  Port of
    !> dabbrev.el:dabbrev--filter-buffer-modes.
    subroutine filter_buffer_modes(list)
        type(handle_type), allocatable, intent(out) :: list(:)
        type(handle_type), allocatable :: all(:)
        integer(int64) :: i
        integer :: k, n
        type(status_type) :: st
        logical :: ignored
        all = buffer_list()
        allocate (list(size(all)))
        n = 0
        do k = 1, size(all)
            call resolve_buffer(all(k), i, st)
            if (.not. st%ok()) cycle
            ignored = .false.
            if (allocated(dabbrev_ignored_buffer_modes) .and. allocated(buffers(i)%major_mode)) then
                if (size(dabbrev_ignored_buffer_modes) > 0) then
                    ignored = provided_mode_derived_p(buffers(i)%major_mode, dabbrev_ignored_buffer_modes)
                end if
            end if
            if (ignored) cycle
            n = n + 1
            list(n) = all(k)
        end do
        list = list(1:n)
    end subroutine filter_buffer_modes

    !> Search for ABBREV, backwards if REVERSE, N times.
    !> If IGNORE_CASE is true, ignore case while searching.
    !> Return the expansion found, and save the location of the start
    !> of the expansion in `dabbrev__last_expansion_location'.  Port of
    !> dabbrev.el:dabbrev--try-find.
    recursive subroutine try_find(abbrev, reverse, n, ignore_case, expansion, status)
        character(len=*), intent(in) :: abbrev
        logical, intent(in) :: reverse, ignore_case
        integer(int64), intent(in) :: n
        character(len=:), allocatable, intent(out) :: expansion
        type(status_type), intent(out) :: status
        type(excursion_type) :: ex
        integer(int64) :: savedb, savedz, count
        logical :: saved_fold, distinction
        type(status_type) :: st
        status = status_type()
        call save_excursion(ex)
        savedb = buffers(cur)%begv
        savedz = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        if (location_set) call goto_char(location())
        saved_fold = case_fold_search%value
        case_fold_search = ignore_case
        distinction = .false.
        if (ignore_case) then
            if (dabbrev_case_distinction == DABBREV_LIKE_CASE_REPLACE) then
                distinction = case_replace
            else
                distinction = dabbrev_case_distinction /= DABBREV_OFF
            end if
        end if
        count = n
        do while (count > 0)
            if (allocated(expansion)) deallocate (expansion)
            call dabbrev_search(abbrev, reverse, distinction, expansion, status)
            if (.not. status%ok() .or. .not. allocated(expansion)) exit
            count = count - 1
        end do
        case_fold_search = saved_fold
        if (allocated(expansion)) call set_location(point())
        call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
        call restore_excursion(ex)
    end subroutine try_find

    !> Return true if BUFFER should be ignored by dabbrev.  Port of
    !> dabbrev.el:dabbrev--ignore-buffer-p.
    logical function ignore_buffer_p(buffer)
        type(handle_type), intent(in) :: buffer
        integer(int64) :: i
        integer :: k
        type(status_type) :: st
        ignore_buffer_p = .false.
        call resolve_buffer(buffer, i, st)
        if (.not. st%ok()) return
        if (allocated(dabbrev_ignored_buffer_names)) then
            do k = 1, size(dabbrev_ignored_buffer_names)
                if (same_string(buffers(i)%name, trim(dabbrev_ignored_buffer_names(k)))) then
                    ignore_buffer_p = .true.
                    return
                end if
            end do
        end if
        if (allocated(dabbrev_ignored_buffer_regexps)) then
            do k = 1, size(dabbrev_ignored_buffer_regexps)
                if (string_match(trim(dabbrev_ignored_buffer_regexps(k)), buffers(i)%name, st) > 0) then
                    ignore_buffer_p = .true.
                    return
                end if
            end do
        end if
    end function ignore_buffer_p

    !> Find one occurrence of ABBREV, and return the expansion.
    !> DIRECTION > 0 means look that many times backwards.
    !> DIRECTION < 0 means look that many times forward.
    !> DIRECTION = 0 means try both backward and forward.
    !> IGNORE_CASE true means ignore case when searching.
    !> This sets `dabbrev__last_direction' to 1 or -1 according
    !> to the direction in which the occurrence was actually found.
    !> It sets `dabbrev__last_expansion_location' to the location
    !> of the start of the occurrence.  Port of
    !> dabbrev.el:dabbrev--find-expansion.
    recursive subroutine find_expansion(abbrev, direction, ignore_case, expansion, status)
        character(len=*), intent(in) :: abbrev
        integer(int64), intent(in) :: direction
        logical, intent(in) :: ignore_case
        character(len=:), allocatable, intent(out) :: expansion
        type(status_type), intent(out) :: status
        type(excursion_type) :: ex
        type(handle_type) :: file_name_buffer, old
        type(handle_type), allocatable :: rest(:)
        character(len=:), allocatable :: file_name
        logical :: these_only
        integer :: k, n, j, start
        type(status_type) :: st
        status = status_type()
        these_only = .false.
        if (allocated(dabbrev_search_these_buffers_only)) these_only = size(dabbrev_search_these_buffers_only) > 0
        call save_excursion(ex)
        ! If we were scanning something other than the current buffer,
        ! continue scanning there.
        if (buffer_live_p(last_buffer)) call set_buffer(last_buffer, st)
        ! Look backward in current buffer.
        if (.not. these_only .and. direction >= 0) then
            last_direction = min(1_int64, direction)
            last_direction_set = .true.
            call try_find(abbrev, .true., max(1_int64, direction), ignore_case, expansion, status)
        end if
        ! Look forward in current buffer
        ! or whatever buffer we were last scanning.
        if (status%ok() .and. .not. allocated(expansion)) then
            if ((.not. these_only .or. buffer_live_p(last_buffer)) .and. direction <= 0) then
                last_direction = -1
                last_direction_set = .true.
                call try_find(abbrev, .false., max(1_int64, -direction), ignore_case, expansion, status)
            end if
        end if
        ! Look in other buffers.
        ! Always start at (point-min) and look forward.
        if (status%ok() .and. .not. allocated(expansion)) then
            last_direction = -1
            last_direction_set = .true.
            if (.not. buffer_live_p(last_buffer)) then
                ! If we have just now begun to search other buffers,
                ! determine which other buffers we should check.
                ! Put that list in dabbrev--friend-buffer-list.
                n = 0
                if (allocated(friend_buffer_list)) n = size(friend_buffer_list)
                if (n == 0) then
                    call make_friend_buffer_list(friend_buffer_list, status)
                    if (status%ok()) then
                        call make_progress_reporter(progress_reporter, "Scanning for dabbrevs...", &
                                                    -size(friend_buffer_list, kind=int64), 0_int64)
                        ! Upstream's CURRENT-VALUE 0, MIN-CHANGE 1 and MIN-TIME 1.5:
                        ! start again from the time the reporter was made.
                        progress_reporter%update_time = progress_reporter%update_time - progress_reporter%min_time
                        progress_reporter%min_time = 1.5d0
                        progress_reporter%next_value = real(-size(friend_buffer_list), kind(progress_reporter%next_value))
                        call progress_reporter_update(progress_reporter, 0_int64)
                    end if
                end if
            end if
            if (status%ok()) then
                file_name_buffer = null_handle()
                if (allocated(buffers(cur)%filename)) file_name = buffers(cur)%filename
                ! Include the file name components into the abbrev
                ! list (because if you have a file name "foobar", it's
                ! somewhat likely that you'll be talking about foobar
                ! stuff in the file itself).
                if (allocated(file_name)) then
                    old = current_buffer()
                    call get_buffer_create(generate_new_buffer_name(" *abbrev-file*"), file_name_buffer, status)
                    if (status%ok()) call set_buffer(file_name_buffer, status)
                    ! (file-name-split FILE-NAME), one part per line.
                    start = 1
                    do j = 1, len(file_name) + 1
                        if (.not. status%ok()) exit
                        if (j <= len(file_name)) then
                            if (file_name(j:j) /= "/") cycle
                        end if
                        call insert(file_name(start:j - 1)//LF, status)
                        start = j + 1
                    end do
                    call set_buffer(old, st)
                    if (status%ok()) call append_buffer(friend_buffer_list, file_name_buffer)
                end if
                ! Walk through the buffers till we find a match.
                do while (status%ok() .and. .not. allocated(expansion))
                    if (.not. allocated(friend_buffer_list)) exit
                    if (size(friend_buffer_list) == 0) exit
                    last_buffer = friend_buffer_list(1)
                    rest = friend_buffer_list(2:)
                    call move_alloc(rest, friend_buffer_list)
                    call set_buffer(last_buffer, status)
                    if (.not. status%ok()) exit
                    if (allocated(progress_reporter%text)) then
                        call progress_reporter_update(progress_reporter, -size(friend_buffer_list, kind=int64))
                    end if
                    call set_location(point_min())
                    call try_find(abbrev, .false., 1_int64, ignore_case, expansion, status)
                end do
                if (status%ok() .and. allocated(progress_reporter%text)) call progress_reporter_done(progress_reporter)
                if (buffer_live_p(file_name_buffer)) call kill_buffer(file_name_buffer, st)
                if (allocated(friend_buffer_list)) then
                    n = 0
                    do k = 1, size(friend_buffer_list)
                        if (.not. buffer_live_p(friend_buffer_list(k))) cycle
                        n = n + 1
                        friend_buffer_list(n) = friend_buffer_list(k)
                    end do
                    friend_buffer_list = friend_buffer_list(1:n)
                end if
            end if
        end if
        call restore_excursion(ex)
    end subroutine find_expansion

    !> Append B to LIST.
    subroutine append_buffer(list, b)
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), intent(in) :: b
        type(handle_type), allocatable :: grown(:)
        integer :: n
        n = 0
        if (allocated(list)) n = size(list)
        allocate (grown(n + 1))
        if (n > 0) grown(1:n) = list
        grown(n + 1) = b
        call move_alloc(grown, list)
    end subroutine append_buffer

    !> Whether LIST contains B.
    logical function member(b, list)
        type(handle_type), intent(in) :: b, list(:)
        integer :: k
        member = .false.
        do k = 1, size(list)
            if (same_handle(list(k), b)) then
                member = .true.
                return
            end if
        end do
    end function member

    !> LIST with B moved to the front (or put there).
    subroutine move_to_front(list, b)
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), intent(in) :: b
        type(handle_type), allocatable :: out(:)
        integer :: k, n
        allocate (out(size(list) + 1))
        out(1) = b
        n = 1
        do k = 1, size(list)
            if (same_handle(list(k), b)) cycle
            n = n + 1
            out(n) = list(k)
        end do
        list = out(1:n)
    end subroutine move_to_front

    !> Compute the list of buffers to scan.
    !> If dabbrev-search-these-buffers-only, then the current buffer
    !> is included in this list if it should be searched.
    !> Otherwise, the current buffer is searched first specially.,
    !> and it is not included in this list.  Port of
    !> dabbrev.el:dabbrev--make-friend-buffer-list.
    recursive subroutine make_friend_buffer_list(list, status)
        type(handle_type), allocatable, intent(out) :: list(:)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: all(:), leaves(:)
        logical :: these_only, scan
        integer :: k, n, sel, j
        type(handle_type) :: wb, origin
        status = status_type()
        these_only = .false.
        if (allocated(dabbrev_search_these_buffers_only)) these_only = size(dabbrev_search_these_buffers_only) > 0
        allocate (list(0))
        if (these_only) then
            do k = 1, size(dabbrev_search_these_buffers_only)
                call append_buffer(list, get_buffer(trim(dabbrev_search_these_buffers_only(k))))
            end do
        end if
        if (.not. these_only .and. check_other_buffers /= DABBREV_OFF) then
            scan = check_other_buffers == DABBREV_ON
            if (.not. scan) then
                call y_or_n_p("Scan other buffers also? ", scan, status)
                if (.not. status%ok()) return
                check_other_buffers = merge(DABBREV_ON, DABBREV_OFF, scan)
            end if
            if (scan) then
                if (associated(dabbrev_select_buffers_function)) then
                    call dabbrev_select_buffers_function(list)
                else
                    call dabbrev_select_buffers(list)
                end if
                ! If dabbrev-check-all-buffers, tack on all the other
                ! buffers at the end of the list, except those which are
                ! specifically to be ignored.
                if (check_all_buffers) then
                    call filter_buffer_modes(all)
                    do k = 1, size(all)
                        if (member(all(k), list)) cycle
                        if (ignore_buffer_p(all(k))) cycle
                        call append_buffer(list, all(k))
                    end do
                end if
                ! Remove the current buffer.
                n = 0
                do k = 1, size(list)
                    if (same_handle(list(k), current_buffer())) cycle
                    n = n + 1
                    list(n) = list(k)
                end do
                list = list(1:n)
            end if
        end if
        ! Move buffers in the list that are visible on the screen
        ! to the front of the list, but don't add anything to the list.
        if (size(list) > 0) then
            call window_leaves(selected_frame%root_window, leaves)
            sel = 0
            do k = 1, size(leaves)
                if (same_handle(leaves(k), selected_frame%selected_window) .and. &
                    .not. selected_frame%minibuffer_selected) sel = k
            end do
            do j = 1, size(leaves)
                k = modulo(sel + j - 1, size(leaves)) + 1
                if (k == sel) cycle
                wb = windows(leaves(k)%index)%buffer
                if (member(wb, list)) call move_to_front(list, wb)
            end do
        end if
        ! In a minibuffer, search the buffer it was activated from,
        ! first after the minibuffer itself.  Unless we aren't supposed
        ! to search the current buffer either.
        if (selected_frame%minibuffer_selected .and. .not. these_only) then
            origin = minibuffer_origin()
            call move_to_front(list, origin)
        end if
    end subroutine make_friend_buffer_list

    !> EXPANSION from its first character on, after the first FROM
    !> characters (substring).
    function char_substring(s, from, to) result(sub)
        character(len=*), intent(in) :: s
        integer(int64), intent(in) :: from
        integer(int64), intent(in), optional :: to
        character(len=:), allocatable :: sub
        integer :: a, b
        a = byte_after(s, from)
        b = len(s)
        if (present(to)) b = byte_after(s, to) - 1
        sub = s(a:b)
    end function char_substring

    !> Replace OLD with EXPANSION in the buffer.
    !> OLD is text currently in the buffer, perhaps the abbreviation
    !> or perhaps another expansion that was tried previously
    !> (HAVE_OLD false for none).
    !> ABBREV is the abbreviation we are expanding.
    !> It is " " if we are copying subsequent words.
    !> EXPANSION is the expansion substring to be used this time; it
    !> becomes the expansion actually used.
    !> RECORD_CASE_PATTERN, if true, means set `dabbrev__last_case_pattern'
    !> to record whether we upcased the expansion, downcased it, or did
    !> neither.  Port of dabbrev.el:dabbrev--substitute-expansion.
    recursive subroutine substitute_expansion(old, have_old, abbrev, expansion, record_case_pattern, status)
        character(len=*), intent(in) :: old, abbrev
        logical, intent(in) :: have_old, record_case_pattern
        character(len=:), allocatable, intent(inout) :: expansion
        type(status_type), intent(out) :: status
        logical :: use_case_replace, plain
        character(len=:), allocatable :: rest, collapsed
        integer(int64) :: first_letter, found, pos, opoint, alen
        integer :: k, b
        logical :: in_space
        type(status_type) :: st
        status = status_type()
        use_case_replace = ignore_case_p(abbrev)
        if (use_case_replace) then
            if (dabbrev_case_replace == DABBREV_LIKE_CASE_REPLACE) then
                use_case_replace = case_replace
            else
                use_case_replace = dabbrev_case_replace /= DABBREV_OFF
            end if
        end if
        ! If we upcased or downcased the original expansion,
        ! do likewise for the subsequent words when we copy them.
        ! Don't do any of the usual case processing, though.
        if (same_string(abbrev, " ")) then
            if (last_case_pattern == CASE_PATTERN_UPCASE) expansion = upcase_string(expansion)
            if (last_case_pattern == CASE_PATTERN_DOWNCASE) expansion = downcase_string(expansion)
            use_case_replace = .false.
        end if
        ! If the expansion has mixed case
        ! and it is not simply a capitalized word,
        ! or if the abbrev has mixed case,
        ! and if the given abbrev's case pattern
        ! matches the start of the expansion,
        ! copy the expansion's case
        ! instead of downcasing all the rest.
        !
        ! Treat a one-capital-letter (possibly with preceding non-letter
        ! characters) abbrev as "not all upper case", so as to force
        ! preservation of the expansion's pattern if the expansion starts
        ! with a capital letter.
        rest = char_substring(expansion, 1_int64)
        first_letter = string_match("[[:alpha:]]", abbrev, st)
        alen = nchars(abbrev)
        if (first_letter == 0) then
            use_case_replace = .false.
        else
            plain = same_string(rest, downcase_string(rest)) .or. same_string(rest, upcase_string(rest))
            if (plain) then
                if (.not. same_string(abbrev, downcase_string(abbrev))) then
                    plain = same_string(abbrev, upcase_string(abbrev))
                    if (plain) plain = alen - (first_letter - 1) > 1
                end if
            end if
            if (.not. plain .and. nchars(expansion) >= alen) then
                if (same_string(abbrev, char_substring(expansion, 0_int64, alen))) use_case_replace = .false.
            end if
        end if
        ! If the abbrev and the expansion are both all-lower-case
        ! then don't do any conversion.  The conversion would be a no-op
        ! for this replacement, but it would carry forward to subsequent words.
        ! The goal of this is to prevent that carrying forward.
        if (same_string(expansion, downcase_string(expansion)) .and. same_string(abbrev, downcase_string(abbrev))) then
            use_case_replace = .false.
        end if
        if (use_case_replace) expansion = downcase_string(expansion)
        ! In case we insert subsequent words,
        ! record if we upcased or downcased the first word,
        ! in order to do likewise for subsequent words.
        if (record_case_pattern) then
            last_case_pattern = CASE_PATTERN_NIL
            if (use_case_replace) then
                if (same_string(abbrev, upcase_string(abbrev))) then
                    last_case_pattern = CASE_PATTERN_UPCASE
                else if (same_string(abbrev, downcase_string(abbrev))) then
                    last_case_pattern = CASE_PATTERN_DOWNCASE
                end if
            end if
        end if
        ! Convert whitespace to single spaces.
        if (dabbrev_eliminate_newlines) then
            ! If ABBREV is real, search after the end of it.
            ! If ABBREV is space and we are copying successive words,
            ! search starting at the front.
            if (same_string(abbrev, " ")) then
                pos = 0
            else
                pos = alen
            end if
            k = byte_after(expansion, pos)
            collapsed = expansion(1:k - 1)
            in_space = .false.
            do while (k <= len(expansion))
                b = ichar(expansion(k:k))
                if (b == 10 .or. b == 32 .or. b == 9) then
                    if (.not. in_space) collapsed = collapsed//" "
                    in_space = .true.
                else
                    collapsed = collapsed//expansion(k:k)
                    in_space = .false.
                end if
                k = k + 1
            end do
            expansion = collapsed
        end if
        if (have_old) then
            opoint = point()
            call search_backward(old, found, status)
            call goto_char(opoint)
        else
            call search_backward(abbrev, found, status)
            if (status%ok()) call search_forward(abbrev, found, status)
        end if
        if (.not. status%ok()) return
        ! Make case of replacement conform to case of abbreviation
        ! provided (1) that kind of thing is enabled in this buffer
        ! and (2) the replacement itself is all lower case.
        call replace_match(expansion, status, fixedcase=.not. use_case_replace, literal=.true.)
    end subroutine substitute_expansion

    !> Search for something that could be used to expand ABBREV, giving
    !> it in RESULT (unallocated if not found).
    !>
    !> Second arg, REVERSE, is true for reverse search, false for forward.
    !> The variable `dabbrev_limit' controls the maximum search region size.
    !> Third argument IGNORE_CASE true means treat case as insignificant while
    !> looking for a match and when comparing with previous matches.  Also if
    !> that's true and the match is found at the beginning of a sentence
    !> and is in lower case except for the initial then it is converted to
    !> all lower case for return.
    !>
    !> Table of expansions already seen is examined in buffer
    !> `dabbrev__last_table' so that only distinct possibilities are found
    !> by dabbrev-re-expand.
    !>
    !> Leaves point at the location of the start of the expansion.  Port
    !> of dabbrev.el:dabbrev--search.
    recursive subroutine dabbrev_search(abbrev, reverse, ignore_case, result, status)
        character(len=*), intent(in) :: abbrev
        logical, intent(in) :: reverse, ignore_case
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pattern1, pattern2, found_string
        integer(int64) :: savedb, savedz, pos, mb, me, found
        logical :: have, seen
        integer :: k
        type(status_type) :: st
        type(string_type), allocatable :: grown(:)
        status = status_type()
        pattern1 = regexp_quote(abbrev)//"\("//abbrev_char_regexp//"\)"
        pattern2 = regexp_quote(abbrev)//"\(\("//abbrev_char_regexp//"\)+\)"
        ! Limited search.
        savedb = buffers(cur)%begv
        savedz = buffers(cur)%text%z - buffers(cur)%zv
        if (allocated(dabbrev_limit)) then
            if (.not. location_set) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "integer_or_marker_p, .false.")
                return
            end if
            if (reverse) then
                call narrow_to_region(location(), point() - dabbrev_limit, status)
            else
                call narrow_to_region(location(), point() + dabbrev_limit, status)
            end if
            if (.not. status%ok()) return
        end if
        ! Look for a distinct expansion, using dabbrev--last-table.
        have = .false.
        do while (.not. have)
            if (reverse) then
                call re_search_backward(pattern1, found, status, noerror=SEARCH_NOERROR)
            else
                call re_search_forward(pattern1, found, status, noerror=SEARCH_NOERROR)
            end if
            if (.not. status%ok() .or. found == 0) exit
            mb = match_beginning(0, st)
            me = match_end(0, st)
            call goto_char(mb)
            ! In case we matched in the middle of a word,
            ! back up to start of word and verify we still match.
            call goto_start_of_abbrev()
            if (looking_at(pattern1, st)) then
                ! We have a truly valid match.  Find the end.
                call re_search_forward(pattern2, found, status)
                if (.not. status%ok()) exit
                mb = match_beginning(0, st)
                me = match_end(0, st)
                call buffer_substring(mb, me, found_string, status)
                if (.not. status%ok()) exit
                result = found_string
                if (ignore_case) found_string = downcase_string(found_string)
                ! Ignore this match if it's already in the table.
                seen = .false.
                do k = 1, n_last_table
                    if (same_string(found_string, last_table(k)%s)) seen = .true.
                end do
                have = .not. seen
                if (seen) deallocate (result)
            end if
            ! Prepare to continue searching.
            if (reverse) then
                call goto_char(mb)
            else
                call goto_char(me)
            end if
        end do
        pos = point()
        call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
        call goto_char(pos)
        if (.not. status%ok()) then
            if (allocated(result)) deallocate (result)
            return
        end if
        ! If we found something, use it.
        if (have) then
            ! Put it into `dabbrev--last-table'
            ! and return it (either downcased, or as is).
            if (.not. allocated(last_table)) allocate (last_table(8))
            if (n_last_table == size(last_table)) then
                allocate (grown(2*n_last_table))
                grown(1:n_last_table) = last_table(1:n_last_table)
                call move_alloc(grown, last_table)
            end if
            n_last_table = n_last_table + 1
            last_table(n_last_table)%s = found_string
        else if (allocated(result)) then
            deallocate (result)
        end if
    end subroutine dabbrev_search

    recursive subroutine cmd_dabbrev_expand(status)
        type(status_type), intent(out) :: status
        ! Interactive "*P".
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        if (nilp(current_prefix_arg)) then
            call dabbrev_expand(status)
        else
            call dabbrev_expand(status, prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_dabbrev_expand

    !> Register `dabbrev_expand' and give the list settings their
    !> defaults.
    subroutine syms_of_dabbrev()
        commands%dabbrev_expand%run => cmd_dabbrev_expand
        if (.not. allocated(dabbrev_ignored_buffer_names)) then
            dabbrev_ignored_buffer_names = [character(len=13) :: "*Messages*", "*Buffer List*"]
        end if
        if (.not. allocated(dabbrev_ignored_buffer_modes)) then
            dabbrev_ignored_buffer_modes = [character(len=12) :: "archive_mode", "image_mode"]
        end if
    end subroutine syms_of_dabbrev
end module dabbrev_fm
