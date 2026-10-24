! test_simple_electric_fm.f90 -- tests for blinking parens,
! `newline_and_indent' and `backward_delete_char_untabify'
! (simple_electric_fm).
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

!> Own tests (simple-tests.el has no blink cases).  Keys go through the
!> command loop; without a terminal `sit_for' doesn't wait.  Every test
!> kills its buffer.
module test_simple_electric_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: nil
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, goto_char, buffer_string
    use keyboard_fm, only: kbd_store_bytes, command_loop, discard_input, last_message
    use callint_fm, only: prefix_arg
    use frame_fm, only: selected_window_ref
    use window_fm, only: set_window_start
    use simple_fm, only: fundamental_mode, blink_matching_check_mismatch, blink_matching_paren, &
                         BLINK_MATCHING_PAREN_NIL, BLINK_MATCHING_PAREN_T, newline_and_indent, &
                         backward_delete_char_untabify, backward_delete_char_untabify_method, &
                         UNTABIFY_METHOD_HUNGRY, UNTABIFY_METHOD_UNTABIFY
    use modes_fm, only: minor_modes
    use bindings_fm, only: init_bindings
    use test_support, only: show_alone
    implicit none(type, external)
    private

    public :: collect_simple_electric_fm

contains

    subroutine collect_simple_electric_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("blink-matching-check-mismatch", test_check_mismatch), &
                    new_unittest("blink-matching-open-messages", test_messages), &
                    new_unittest("blink-matching-open-offscreen", test_offscreen), &
                    new_unittest("newline-and-indent", test_newline_and_indent), &
                    new_unittest("backward-delete-char-untabify", test_untabify) &
                    ]
    end subroutine collect_simple_electric_fm

    subroutine fresh(h, text)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: text
        type(status_type) :: st
        call init_bindings()
        call discard_input()
        prefix_arg = nil()
        call get_buffer_create("*blink-test*", h, st)
        call set_buffer(h, st)
        call fundamental_mode(st)
        call insert(text, st)
        call show_alone()
    end subroutine fresh

    subroutine keys(text)
        character(len=*), intent(in) :: text
        type(status_type) :: st
        call kbd_store_bytes(text)
        call command_loop(st)
    end subroutine keys

    subroutine test_check_mismatch(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h, "(a] (b) x) ""c""")
        call check(error, blink_matching_check_mismatch(1_int64, 4_int64), "( and ]")
        if (allocated(error)) return
        call check(error,.not. blink_matching_check_mismatch(5_int64, 8_int64), "( and )")
        if (allocated(error)) return
        call check(error, blink_matching_check_mismatch(0_int64, 11_int64), "no opener")
        if (allocated(error)) return
        call check(error,.not. blink_matching_check_mismatch(12_int64, 15_int64), "quotes can't mismatch")
        call kill_buffer(h, st)
    end subroutine test_check_mismatch

    !> Typing a close paren blinks: mismatches and missing openers are
    !> reported; a visible match is shown (here without a message).
    subroutine test_messages(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h, "(foo")
        call keys("]")
        call check(error, last_message, "Mismatched parentheses")
        call kill_buffer(h, st)
        if (allocated(error)) return
        call fresh(h, "foo")
        call keys(")")
        call check(error, last_message, "No matching parenthesis found")
        call kill_buffer(h, st)
        if (allocated(error)) return
        call fresh(h, "(foo")
        call keys(")")
        call check(error, last_message, "")
        call kill_buffer(h, st)
        if (allocated(error)) return
        blink_matching_paren = BLINK_MATCHING_PAREN_NIL
        call fresh(h, "(foo")
        call keys("]")
        blink_matching_paren = BLINK_MATCHING_PAREN_T
        call check(error, last_message, "")
        call kill_buffer(h, st)
    end subroutine test_messages

    !> A match off the screen is shown in the echo area: "Matches " and
    !> its line, or the previous nonblank line when it is alone.
    subroutine test_offscreen(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h, "(defun foo ()"//repeat(achar(10)//"x", 60))
        call set_window_start(selected_window_ref(), 20_int64, st)
        call keys(")")
        call check(error, last_message, "Matches (defun foo ()")
        call kill_buffer(h, st)
        if (allocated(error)) return
        call fresh(h, "int foo()"//achar(10)//"{"//repeat(achar(10)//"x", 60))
        call set_window_start(selected_window_ref(), 20_int64, st)
        call keys("}")
        call check(error, last_message, "Matches int foo()...{")
        call kill_buffer(h, st)
    end subroutine test_offscreen

    subroutine test_newline_and_indent(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h, "  foo  ")
        call newline_and_indent(st, 2_int64)
        call check(error, buffer_string(), "  foo"//achar(10)//"  "//achar(10))
        if (allocated(error)) return
        call check(error, minor_modes%electric_indent_mode%enabled(), "Electric Indent mode restored")
        call kill_buffer(h, st)
    end subroutine test_newline_and_indent

    !> The `untabify' method turns a tab into spaces before deleting one;
    !> `hungry' deletes all the whitespace.
    subroutine test_untabify(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h, "a"//achar(9)//"b")
        call goto_char(3_int64)
        call backward_delete_char_untabify(1_int64, st)
        call check(error, buffer_string(), "a      b")
        if (allocated(error)) return
        call check(error, point() == point_min() + 7, "point after the spaces")
        call kill_buffer(h, st)
        if (allocated(error)) return
        backward_delete_char_untabify_method = UNTABIFY_METHOD_HUNGRY
        call fresh(h, "a "//achar(9)//" b")
        call goto_char(5_int64)
        call backward_delete_char_untabify(1_int64, st)
        backward_delete_char_untabify_method = UNTABIFY_METHOD_UNTABIFY
        call check(error, buffer_string(), "ab")
        call kill_buffer(h, st)
    end subroutine test_untabify
end module test_simple_electric_fm
