! rmc_fm.f90 -- read-multiple-choice.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/rmc.el.
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

!> `read-multiple-choice', the short-answers path (one key per choice,
!> read in the echo area), for a terminal: the key of each name is shown
!> in [brackets].  The help is shown in a window split off the bottom of
!> the selected one, removed afterwards (save-window-excursion).
!>
!> Not ported: the long-answers and minibuffer paths, dialog boxes,
!> touch-screen events, the `edit' answer (recursive edit).
module rmc_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_QUIT
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, fixnump, xfixnum, make_fixnum
    use buffer_fm, only: current_buffer, get_buffer_create, set_buffer, set_buffer_read_only, erase_buffer, &
                         kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char
    use keymap_fm, only: single_key_description
    use subr_fm, only: read_key, query_replace_answer, ANSWER_RECENTER, ANSWER_SCROLL_UP
    use window_fm, only: window_ref, window_type, set_window_buffer, split_window, delete_window, window_live_p, &
                         recenter_top_bottom, scroll_up_command
    use frame_fm, only: selected_frame, selected_window
    use dispnew_fm, only: ding
    use xdisp_fm, only: message
    implicit none(type, external)
    private

    public :: read_multiple_choice, rmc_add_key_description, generated_help

    !> A choice of `read_multiple_choice': (CHAR NAME [DESCRIPTION]).
    type, public :: rmc_choice_type
        integer :: key = 0
        character(len=:), allocatable :: name
        character(len=:), allocatable :: description
    end type rmc_choice_type

contains

    !> NAME of a choice with its key shown: "KEY NAME" if KEY is not in
    !> NAME or is a special key (ESC, TAB, RET, DEL, SPC), else NAME with
    !> the key upcased in [brackets] (a terminal without underlining).
    !> Port of rmc.el:rmc--add-key-description.
    function rmc_add_key_description(choice) result(altered)
        type(rmc_choice_type), intent(in) :: choice
        character(len=:), allocatable :: altered, desc
        type(value_type) :: key
        integer :: pos
        key = make_fixnum(int(choice%key, int64))
        desc = single_key_description(key)
        pos = 0
        if (choice%key < 128) pos = index(choice%name, achar(choice%key))
        if (pos == 0 .or. desc == "ESC" .or. desc == "TAB" .or. desc == "RET" .or. desc == "DEL" .or. &
            desc == "SPC") then
            altered = desc//" "//choice%name
        else
            altered = choice%name(1:pos - 1)//"["//upcase_ascii(choice%name(pos:pos))//"]"//choice%name(pos + 1:)
        end if
    end function rmc_add_key_description

    pure function upcase_ascii(c) result(u)
        character, intent(in) :: c
        character :: u
        u = c
        if (c >= "a" .and. c <= "z") u = achar(iachar(c) - 32)
    end function upcase_ascii

    !> Ask user to select an entry from CHOICES, prompting with PROMPT.
    !> This function lets a user select an option from a list of options
    !> by typing a single character, and returns the index of the choice
    !> (`answer').
    !>
    !> CHOICES is a list of (KEY NAME [DESCRIPTION]).  KEY is a character
    !> to be entered.  NAME is a short name for the entry to be displayed
    !> while prompting (if there's no room, it might be shortened).
    !> DESCRIPTION is an optional longer explanation for the entry that
    !> will be displayed in a help buffer if the user requests more help.
    !>
    !> HELP-STRING argument is a string that contains overall
    !> information about the choices.  If this argument is given, it is
    !> displayed in the help buffer instead of the generated help.
    !>
    !> SHOW-HELP, if present, means show the help buffer from the start
    !> (named SHOW-HELP when it is a non-empty string).
    !>
    !> If the user types the `?' key, the help is displayed; other keys
    !> not in CHOICES ask again after "Invalid choice."; C-g quits.  Port
    !> of rmc.el:read-multiple-choice (short answers).
    subroutine read_multiple_choice(prompt, choices, answer, status, help_string, show_help)
        character(len=*), intent(in) :: prompt
        type(rmc_choice_type), intent(in) :: choices(:)
        integer, intent(out) :: answer
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: help_string, show_help
        character(len=:), allocatable :: full_prompt, buf_name
        type(value_type) :: key
        type(handle_type) :: buf, help_window
        type(status_type) :: st
        logical :: wrong_char, shown
        integer :: k, c
        answer = 0
        status = status_type()
        full_prompt = prompt//" ("
        do k = 1, size(choices)
            if (k > 1) full_prompt = full_prompt//", "
            full_prompt = full_prompt//rmc_add_key_description(choices(k))
        end do
        if (.not. present(show_help)) full_prompt = full_prompt//", ?"
        full_prompt = full_prompt//"): "
        buf_name = "*Multiple Choice Help*"
        if (present(show_help)) then
            if (len(show_help) > 0) buf_name = show_help
        end if
        shown = .false.
        if (present(show_help)) call show(buf, help_window)
        wrong_char = .false.
        do while (answer == 0)
            if (wrong_char) then
                call read_key("Invalid choice.  "//full_prompt, key, status)
            else
                call read_key(full_prompt, key, status)
            end if
            if (.not. status%ok()) exit
            if (.not. fixnump(key)) then
                c = -1
            else
                c = int(xfixnum(key))
            end if
            if (c == 7) then
                call set_error(status, FM_QUIT)
                exit
            end if
            select case (query_replace_answer(key))
            case (ANSWER_RECENTER)
                call recenter_top_bottom(nil(), st)
                wrong_char = .false.
                cycle
            case (ANSWER_SCROLL_UP)
                call scroll_up_command(nil(), st)
                wrong_char = .false.
                cycle
            case default
                continue
            end select
            do k = 1, size(choices)
                if (choices(k)%key == c) answer = k
            end do
            if (answer == 0) then
                ! The user has entered an invalid choice, so display the
                ! help messages.
                wrong_char = c /= iachar("?") .and. c /= 8
                if (wrong_char) call ding()
                if (.not. shown) call show(buf, help_window)
            end if
        end do
        if (shown) then
            if (window_live_p(help_window)) call delete_window(help_window, st)
            call kill_buffer(buf, st)
        end if
        call message("")
    contains
        ! rmc--show-help: fill the help buffer and show it at the bottom.
        subroutine show(buf, help_window)
            type(handle_type), intent(out) :: buf, help_window
            type(handle_type) :: previous
            type(window_type), pointer :: w
            character(len=:), allocatable :: text
            integer :: lines, i
            previous = current_buffer()
            call get_buffer_create(buf_name, buf, st)
            if (.not. st%ok()) return
            call set_buffer(buf, st)
            call set_buffer_read_only(buf, .false., st)
            call erase_buffer(st)
            if (present(help_string)) then
                text = help_string
            else
                text = prompt//achar(10)//achar(10)//generated_help(choices, help_columns())
            end if
            call insert(text, st)
            call goto_char(1_int64)
            call set_buffer_read_only(buf, .true., st)
            call set_buffer(previous, st)
            shown = .true.
            ! A window as high as the text (and its mode line) below the
            ! selected one; none if it doesn't fit.
            lines = 1
            do i = 1, len(text) - 1
                if (text(i:i) == achar(10)) lines = lines + 1
            end do
            if (selected_frame%minibuffer_selected) return
            call split_window(selected_window(), -(lines + 1), .true., .false., help_window, st)
            if (.not. st%ok()) then
                call split_window(selected_window(), 0, .false., .false., help_window, st)
                if (.not. st%ok()) return
            end if
            call set_window_buffer(help_window, buf, st)
        end subroutine show
    end subroutine read_multiple_choice
    ! (/ (window-width) 25): the columns of the generated help.
    integer function help_columns() result(columns)
        type(window_type), pointer :: w
        columns = 80/25
        if (.not. window_live_p(selected_window())) return
        w => window_ref(selected_window())
        if (w%total_cols > 0) columns = max(1, w%total_cols/25)
    end function help_columns

    ! Number of characters of the UTF-8 string S.
    pure integer function nchars(s)
        character(len=*), intent(in) :: s
        integer :: i
        nchars = 0
        do i = 1, len(s)
            if (iand(iachar(s(i:i)), 192) /= 128) nchars = nchars + 1
        end do
    end function nchars

    ! TEXT (one line) filled at fill-column 21: the lines, each ended by
    ! a newline.
    function fill21(text) result(filled)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: filled, line, word
        integer :: i, j
        filled = ""
        line = ""
        i = 1
        do while (i <= len(text))
            if (text(i:i) == " ") then
                i = i + 1
                cycle
            end if
            j = index(text(i:), " ")
            if (j == 0) then
                word = text(i:)
                i = len(text) + 1
            else
                word = text(i:i + j - 2)
                i = i + j
            end if
            if (len(line) == 0) then
                line = word
            else if (nchars(line) + 1 + nchars(word) <= 21) then
                line = line//" "//word
            else
                filled = filled//line//achar(10)
                line = word
            end if
        end do
        filled = filled//line//achar(10)
    end function fill21

    !> The help text rmc--show-help generates: each choice ("KEY: NAME"
    !> and its description, filled at column 21) in COLUMNS columns 25
    !> characters apart.  Port of the layout in rmc.el:rmc--show-help.
    function generated_help(choices, columns) result(text)
        type(rmc_choice_type), intent(in) :: choices(:)
        integer, intent(in) :: columns
        character(len=:), allocatable :: text, entry
        type :: line_type
            character(len=:), allocatable :: s
        end type line_type
        type(line_type), allocatable :: lines(:), grown(:)
        integer :: times, start, n, k, row, i, j, pad
        allocate (lines(0))
        n = 0
        start = 1
        times = 0
        do k = 1, size(choices)
            if (times /= 0) then
                if (mod(times, columns) == 0) then
                    ! Go to the next "line".
                    start = n + 1
                else
                    ! Add padding.
                    do row = start, n
                        lines(row)%s = lines(row)%s//repeat(" ", max(mod(times, columns)*25 - nchars(lines(row)%s), 0))
                    end do
                end if
            end if
            times = times + 1
            entry = fill21(achar(choices(k)%key)//": "//rmc_add_key_description(choices(k)))
            if (allocated(choices(k)%description)) entry = entry//fill21(choices(k)%description)
            pad = mod(times - 1, columns)*25
            row = start
            i = 1
            ! (split-string text "\n"): the lines, then a last empty one.
            do
                j = index(entry(i:), achar(10))
                if (row <= n) then
                    if (len(lines(row)%s) > 0) then
                        if (j > 0) then
                            lines(row)%s = lines(row)%s//entry(i:i + j - 2)
                        else
                            lines(row)%s = lines(row)%s//entry(i:)
                        end if
                        row = row + 1
                        if (j == 0) exit
                        i = i + j
                        cycle
                    end if
                end if
                ! At the beginning of a line: a new line before it.
                allocate (grown(n + 1))
                grown(1:row - 1) = lines(1:row - 1)
                if (j > 0) then
                    grown(row)%s = repeat(" ", pad)//entry(i:i + j - 2)
                else
                    grown(row)%s = repeat(" ", pad)//entry(i:)
                end if
                if (row <= n) grown(row + 1:) = lines(row:)
                call move_alloc(grown, lines)
                n = n + 1
                row = row + 1
                if (j == 0) exit
                i = i + j
            end do
        end do
        text = ""
        do row = 1, n
            text = text//lines(row)%s//achar(10)
        end do
    end function generated_help
end module rmc_fm
