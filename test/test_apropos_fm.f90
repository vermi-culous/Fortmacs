! test_apropos_fm.f90 -- tests for apropos_fm.
!
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

!> Ports the apropos-tests.el cases on patterns, scores and hits (not
!> apropos-tests-score-str or -format-plist: `apropos' itself is
!> deferred); own tests run the commands through the command loop.
module test_apropos_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type
    use buffer_fm, only: buffers, cur, get_buffer_create, get_buffer, set_buffer, current_buffer, buffer_live_p
    use frame_fm, only: selected_frame, ensure_frame_windows
    use window_fm, only: windows, delete_other_windows, switch_to_buffer, resize_window_tree
    use editfns_fm, only: buffer_string, goto_char, point_max
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, discard_input, last_message, command_loop
    use bindings_fm, only: init_bindings
    use search_fm, only: string_match
    use apropos_fm, only: apropos_word_type, apropos_words_to_regexp, apropos_parse_pattern, apropos_regexp, &
                          apropos_calc_scores, apropos_true_hit
    implicit none(type, external)
    private

    public :: collect_apropos_fm

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

contains

    subroutine collect_apropos_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("apropos-tests-words-to-regexp-1", test_words_to_regexp_1), &
                    new_unittest("apropos-tests-words-to-regexp-2", test_words_to_regexp_2), &
                    new_unittest("apropos-tests-parse-pattern-1", test_parse_pattern_1), &
                    new_unittest("apropos-tests-parse-pattern-2", test_parse_pattern_2), &
                    new_unittest("apropos-tests-parse-pattern-3", test_parse_pattern_3), &
                    new_unittest("apropos-tests-parse-pattern-single-regexp", test_parse_pattern_regexp), &
                    new_unittest("apropos-tests-parse-pattern-synonyms", test_parse_pattern_synonyms), &
                    new_unittest("apropos-tests-calc-scores", test_calc_scores), &
                    new_unittest("apropos-tests-true-hit", test_true_hit), &
                    new_unittest("apropos-command", test_apropos_command), &
                    new_unittest("apropos-command-no-match", test_apropos_no_match), &
                    new_unittest("apropos-user-option", test_apropos_user_option), &
                    new_unittest("apropos-documentation", test_apropos_documentation), &
                    new_unittest("apropos-follow", test_apropos_follow) &
                    ]
    end subroutine collect_apropos_fm

    !> The words of a list.
    function words(a, b, c) result(w)
        character(len=*), intent(in) :: a
        character(len=*), intent(in), optional :: b, c
        type(apropos_word_type), allocatable :: w(:)
        integer :: n
        n = 1
        if (present(b)) n = 2
        if (present(c)) n = 3
        allocate (w(n))
        w(1)%s = a
        if (present(b)) w(2)%s = b
        if (present(c)) w(3)%s = c
    end function words

    logical function matches(re, s)
        character(len=*), intent(in) :: re, s
        type(status_type) :: st
        matches = string_match(re, s, st) > 0
    end function matches

    subroutine fresh()
        type(handle_type) :: buf
        type(status_type) :: st
        call init_bindings()
        call discard_input()
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        call ensure_frame_windows()
        call delete_other_windows(selected_frame%selected_window, st)
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call switch_to_buffer("*scratch*", st)
        last_message = ""
    end subroutine fresh

    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        call key_parse(keys, evs, st)
        call unread_command_events(evs)
    end subroutine type_keys

    !> `keys' with each character of `text' as a key, spaces as SPC.
    function spelled(text) result(keys)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: keys
        integer :: k
        keys = ""
        do k = 1, len(text)
            if (k > 1) keys = keys//" "
            if (text(k:k) == " ") then
                keys = keys//"SPC"
            else
                keys = keys//text(k:k)
            end if
        end do
    end function spelled

    function apropos_text() result(text)
        character(len=:), allocatable :: text
        type(handle_type) :: old
        type(status_type) :: st
        old = current_buffer()
        text = "(none)"
        if (.not. buffer_live_p(get_buffer("*Apropos*"))) return
        call set_buffer(get_buffer("*Apropos*"), st)
        text = buffer_string()
        call set_buffer(old, st)
    end function apropos_text

    subroutine test_words_to_regexp_1(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: re
        re = apropos_words_to_regexp(words("foo", "bar"), "baz")
        call check(error, matches(re, "foobazbar") .and. matches(re, "barbazfoo") .and. &
                   .not. matches(re, "foo-bar") .and. .not. matches(re, "foobazbazbar"), re)
    end subroutine test_words_to_regexp_1

    subroutine test_words_to_regexp_2(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: re
        re = apropos_words_to_regexp(words("foo", "bar", "baz"), "-")
        call check(error,.not. matches(re, "foo") .and. .not. matches(re, "foobar") .and. &
                   matches(re, "foo-bar") .and. matches(re, "foo-baz"), re)
    end subroutine test_words_to_regexp_2

    subroutine test_parse_pattern_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(apropos_word_type), allocatable :: w(:)
        w = words("foo")
        call apropos_parse_pattern("", w, .false.)
        call check(error, matches(apropos_regexp, "foo") .and. matches(apropos_regexp, "foo-bar") .and. &
                   matches(apropos_regexp, "bar-foo") .and. matches(apropos_regexp, "foo-foo") .and. &
                   .not. matches(apropos_regexp, "bar"), apropos_regexp)
    end subroutine test_parse_pattern_1

    subroutine test_parse_pattern_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(apropos_word_type), allocatable :: w(:)
        w = words("foo", "bar")
        call apropos_parse_pattern("", w, .false.)
        call check(error, matches(apropos_regexp, "foo-bar") .and. matches(apropos_regexp, "bar-foo") .and. &
                   .not. matches(apropos_regexp, "foo") .and. .not. matches(apropos_regexp, "bar") .and. &
                   .not. matches(apropos_regexp, "baz") .and. .not. matches(apropos_regexp, "foo-foo") .and. &
                   .not. matches(apropos_regexp, "bar-bar"), apropos_regexp)
    end subroutine test_parse_pattern_2

    subroutine test_parse_pattern_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(apropos_word_type), allocatable :: w(:)
        w = words("foo", "bar", "baz")
        call apropos_parse_pattern("", w, .false.)
        call check(error, matches(apropos_regexp, "foo-bar") .and. matches(apropos_regexp, "foo-baz") .and. &
                   matches(apropos_regexp, "bar-foo") .and. matches(apropos_regexp, "bar-baz") .and. &
                   matches(apropos_regexp, "baz-foo") .and. matches(apropos_regexp, "baz-bar") .and. &
                   .not. matches(apropos_regexp, "foo") .and. .not. matches(apropos_regexp, "bar") .and. &
                   .not. matches(apropos_regexp, "baz") .and. .not. matches(apropos_regexp, "foo-foo") .and. &
                   .not. matches(apropos_regexp, "bar-bar") .and. .not. matches(apropos_regexp, "baz-baz"), &
                   apropos_regexp)
    end subroutine test_parse_pattern_3

    subroutine test_parse_pattern_regexp(error)
        type(error_type), allocatable, intent(out) :: error
        type(apropos_word_type), allocatable :: w(:)
        call apropos_parse_pattern("foo+bar", w, .false.)
        call check(error,.not. matches(apropos_regexp, "fobar") .and. matches(apropos_regexp, "foobar") .and. &
                   matches(apropos_regexp, "fooobar"), apropos_regexp)
    end subroutine test_parse_pattern_regexp

    !> With the standard synonyms, ("find" "open" "edit") among them.
    subroutine test_parse_pattern_synonyms(error)
        type(error_type), allocatable, intent(out) :: error
        type(apropos_word_type), allocatable :: w(:)
        w = words("open")
        call apropos_parse_pattern("", w, .false.)
        call check(error, matches(apropos_regexp, "find-file") .and. matches(apropos_regexp, "open-file") .and. &
                   matches(apropos_regexp, "edit-file"), apropos_regexp)
    end subroutine test_parse_pattern_synonyms

    subroutine test_calc_scores(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: str = "Return apropos score for string STR."
        integer(int64), allocatable :: scores(:)
        call apropos_calc_scores(str, words("apr"), .true., scores)
        call check(error, size(scores) == 1, "one score")
        if (allocated(error)) return
        call check(error, scores(1) == 7_int64, "apr at 7")
        if (allocated(error)) return
        call apropos_calc_scores(str, words("apr", "str"), .true., scores)
        call check(error, size(scores) == 2, "two scores")
        if (allocated(error)) return
        call check(error, scores(1) == 25_int64 .and. scores(2) == 7_int64, "(25 7)")
        if (allocated(error)) return
        call apropos_calc_scores(str, words("appr", "str"), .true., scores)
        call check(error, size(scores) == 1, "(25)")
        if (allocated(error)) return
        call check(error, scores(1) == 25_int64, "(25)")
        if (allocated(error)) return
        call apropos_calc_scores(str, words("appr", "strr"), .true., scores)
        call check(error, size(scores) == 0, "none")
    end subroutine test_calc_scores

    subroutine test_true_hit(error)
        type(error_type), allocatable, intent(out) :: error
        type(apropos_word_type), allocatable :: w(:)
        w = words("foo", "bar")
        call check(error,.not. apropos_true_hit("foo", w) .and. apropos_true_hit("foo bar", w) .and. &
                   apropos_true_hit("foo bar baz", w))
    end subroutine test_true_hit

    !> C-h a: the commands matching the words, their keys from column
    !> 30, the first line of their docstrings.
    subroutine test_apropos_command(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text
        call fresh()
        call type_keys("C-h a "//spelled("line kill")//" RET")
        call command_loop(st)
        text = apropos_text()
        call check(error, index(text, "Type RET on an entry to view its full documentation."//LF//LF// &
                                "kill_line"//TAB//TAB//"      C-k, <deleteline>"//LF// &
                                "  Kill the rest of the current line;") == 1, text)
        if (allocated(error)) return
        call check(error, index(text, LF//"kill_whole_line"//TAB//TAB//"      C-S-<backspace>"//LF// &
                                "  Kill current line."//LF) > 0 .and. index(text, "forward_line") == 0, text)
        if (allocated(error)) return
        call check(error, last_message, "Type C-x 1 to remove help window.  ")
    end subroutine test_apropos_command

    subroutine test_apropos_no_match(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=*), parameter :: LQ = char(226)//char(128)//char(152), RQ = char(226)//char(128)//char(153)
        call fresh()
        call type_keys("C-h a "//spelled("zzqx")//" RET")
        call command_loop(st)
        call check(error, last_message, "No apropos matches for "//LQ//"zzqx"//RQ)
        if (allocated(error)) return
        call type_keys("C-h a RET")
        call command_loop(st)
        call check(error, last_message, "No word list given")
    end subroutine test_apropos_no_match

    !> C-h u: user options, with their values; C-u C-h u all variables.
    subroutine test_apropos_user_option(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text
        call fresh()
        call type_keys("C-h u "//spelled("fill_column")//" RET")
        call command_loop(st)
        text = apropos_text()
        call check(error, index(text, LF//"fill_column"//LF//"  Column beyond which automatic line-wrapping should "// &
                                "happen."//LF//"  Value: 70"//LF) > 0, text)
        if (allocated(error)) return
        ! A variable that is not a user option.
        call type_keys("C-h u "//spelled("inhibit_modification")//" RET")
        call command_loop(st)
        call check(error, index(last_message, "No apropos matches") == 1, last_message)
        if (allocated(error)) return
        call type_keys("C-u M-x "//spelled("apropos_user_option")//" RET "//spelled("inhibit_modification")//" RET")
        call command_loop(st)
        call check(error, index(apropos_text(), LF//"inhibit_modification_hooks"//LF) > 0, last_message//apropos_text())
    end subroutine test_apropos_user_option

    !> C-h d: functions and variables whose documentation matches, with
    !> type labels, separated, the best matches first.
    subroutine test_apropos_documentation(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text
        call fresh()
        call type_keys("C-h d "//spelled("upcase initial")//" RET")
        call command_loop(st)
        text = apropos_text()
        call check(error, index(text, "Type RET on a type label to view its full documentation."//LF//LF// &
                                "upcase_initials_region"//LF//"  Command: Upcase the initial of each word in the "// &
                                "region.") == 1, text)
        if (allocated(error)) return
        call type_keys("C-h d "//spelled("upcase")//" RET")
        call command_loop(st)
        text = apropos_text()
        call check(error, index(text, LF//"----------------"//LF) > 0, text)
    end subroutine test_apropos_documentation

    !> RET on an entry describes it.
    subroutine test_apropos_follow(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text
        type(handle_type) :: old
        call fresh()
        call type_keys("C-h a "//spelled("kill_whole_line")//" RET C-x o")
        call command_loop(st)
        call check(error, buffers(cur)%name == "*Apropos*", buffers(cur)%name)
        if (allocated(error)) return
        call type_keys("n RET")
        call command_loop(st)
        old = current_buffer()
        call set_buffer(get_buffer("*Help*"), st)
        text = buffer_string()
        call set_buffer(old, st)
        call check(error, index(text, "kill_whole_line is an interactive Fortran subroutine") == 1, text)
        if (allocated(error)) return
        call goto_char(point_max())
        call type_keys("RET")
        call command_loop(st)
        call check(error, last_message, "There is nothing to follow here")
        call fresh()
    end subroutine test_apropos_follow
end module test_apropos_fm
