! test_paren_fm.f90 -- tests for Show Paren mode (paren_fm).  Includes
! cases ported from GNU Emacs 31.1 test/lisp/paren-tests.el.
!
! Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

!> Every test creates its buffer and kills it before returning.
module test_paren_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point, point_min, point_max
    use itree_fm, only: overlays_at, overlay_get
    use paren_fm, only: show_paren_unescaped_p, show_paren_categorize_paren, show_paren_locate_near_paren, &
                        show_paren_default, show_paren_data_type, show_paren_function, &
                        show_paren_when_point_inside_paren, show_paren_when_point_in_periphery, show_paren_style, &
                        SHOW_PAREN_EXPRESSION, SHOW_PAREN_PARENTHESIS
    use simple_fm, only: blink_paren_open_paren_line_string, fundamental_mode, special_mode
    use modes_fm, only: minor_modes
    use bindings_fm, only: init_bindings
    use timer_fm, only: run_with_timer, free_timer
    use keyboard_fm, only: read_event, set_input_fd, discard_input
    use sysdep_fm, only: emacs_tty_type, open_pty, close_fd, init_sys_modes, reset_sys_modes, emacs_write
    use test_support, only: show_alone
    implicit none(type, external)
    private

    public :: collect_paren_fm

    integer(int32) :: master = -1

contains

    subroutine collect_paren_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("paren-tests-unescaped-p", test_unescaped_p), &
                    new_unittest("paren-tests-categorize-paren", test_categorize_paren), &
                    new_unittest("paren-tests-locate-near-paren", test_locate_near_paren), &
                    new_unittest("paren-tests-locate-near-paren-inside", test_locate_near_paren_inside), &
                    new_unittest("paren-tests-locate-near-paren-in-periphery", test_locate_near_paren_in_periphery), &
                    new_unittest("paren-tests-default", test_default), &
                    new_unittest("paren-tests-open-paren-line", test_open_paren_line), &
                    new_unittest("show-paren-function-overlays", test_function), &
                    new_unittest("show-paren-mode-and-local-mode", test_modes), &
                    new_unittest("show-paren-idle-timer", test_idle_timer) &
                    ]
    end subroutine collect_paren_fm

    !> (with-temp-buffer (insert TEXT) ...)
    subroutine temp_buffer(h, text)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: text
        type(status_type) :: st
        call get_buffer_create("*paren-test*", h, st)
        call set_buffer(h, st)
        call fundamental_mode(st)
        call insert(text, st)
    end subroutine temp_buffer

    subroutine test_unescaped_p(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call temp_buffer(h, "(insert)")
        call goto_char(point() - 1)
        call check(error, show_paren_unescaped_p(point()), "unescaped")
        if (allocated(error)) return
        call insert("\", st)
        call check(error,.not. show_paren_unescaped_p(point()), "escaped")
        call kill_buffer(h, st)
    end subroutine test_unescaped_p

    subroutine test_categorize_paren(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer :: dir
        integer(int64) :: outside
        call temp_buffer(h, "(insert)")
        call goto_char(point() - 1)
        call show_paren_categorize_paren(point(), dir, outside)
        call check(error, dir == -1 .and. outside == point() + 1, "close paren")
        if (allocated(error)) return
        call goto_char(point_min())
        call show_paren_categorize_paren(point(), dir, outside)
        call check(error, dir == 1 .and. outside == point(), "open paren")
        call kill_buffer(h, st)
    end subroutine test_categorize_paren

    !> Whether locate-near-paren gives (DIR . OUTSIDE), DIR 0 for nil.
    logical function near(dir, outside)
        integer, intent(in) :: dir
        integer(int64), intent(in) :: outside
        integer :: d
        integer(int64) :: o
        call show_paren_locate_near_paren(d, o)
        near = d == dir
        if (near .and. dir /= 0) near = o == outside
    end function near

    subroutine test_locate_near_paren(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        show_paren_when_point_inside_paren = .false.
        show_paren_when_point_in_periphery = .false.
        call temp_buffer(h, "(insert)")
        call check(error, near(-1, point()), "after the close paren")
        if (allocated(error)) return
        call goto_char(point() - 1)
        call check(error, near(0, 0_int64), "before the close paren")
        if (allocated(error)) return
        call goto_char(point_min())
        call check(error, near(1, point_min()), "before the open paren")
        if (allocated(error)) return
        call goto_char(point() + 1)
        call check(error, near(0, 0_int64), "after the open paren")
        call kill_buffer(h, st)
    end subroutine test_locate_near_paren

    subroutine test_locate_near_paren_inside(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        show_paren_when_point_inside_paren = .true.
        show_paren_when_point_in_periphery = .false.
        call temp_buffer(h, "(insert)")
        call check(error, near(-1, point()), "after the close paren")
        if (allocated(error)) return
        call goto_char(point() - 1)
        call check(error, near(-1, point() + 1), "before the close paren")
        if (allocated(error)) return
        call goto_char(point_min())
        call check(error, near(1, point_min()), "before the open paren")
        if (allocated(error)) return
        call goto_char(point() + 1)
        call check(error, near(1, point_min()), "after the open paren")
        show_paren_when_point_inside_paren = .false.
        call kill_buffer(h, st)
    end subroutine test_locate_near_paren_inside

    subroutine test_locate_near_paren_in_periphery(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        show_paren_when_point_inside_paren = .false.
        show_paren_when_point_in_periphery = .true.
        call temp_buffer(h, " (insert) ")
        call check(error, near(-1, point() - 1), "end of line")
        if (allocated(error)) return
        call goto_char(point() - 2)
        call check(error, near(0, 0_int64), "before the close paren")
        if (allocated(error)) return
        call goto_char(point_min())
        call check(error, near(1, point() + 1), "beginning of line")
        if (allocated(error)) return
        call goto_char(point() + 1)
        call check(error, near(1, point()), "before the open paren")
        if (allocated(error)) return
        call goto_char(point() + 1)
        call check(error, near(0, 0_int64), "after the open paren")
        show_paren_when_point_in_periphery = .false.
        call kill_buffer(h, st)
    end subroutine test_locate_near_paren_in_periphery

    !> Whether show-paren--default gives (HERE-BEG HERE-END THERE-BEG
    !> THERE-END MISMATCH), THERE-BEG 0 for nil.
    logical function default_is(here_beg, here_end, there_beg, there_end, mismatch)
        integer(int64), intent(in) :: here_beg, here_end, there_beg, there_end
        logical, intent(in) :: mismatch
        type(show_paren_data_type) :: data
        logical :: found
        call show_paren_default(found, data)
        default_is = found .and. data%here_beg == here_beg .and. data%here_end == here_end .and. &
                     data%there_beg == there_beg .and. data%there_end == there_end .and. (data%mismatch .eqv. mismatch)
    end function default_is

    subroutine test_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call temp_buffer(h, "(insert")
        call goto_char(point_min())
        call check(error, default_is(point(), point() + 1, 0_int64, 0_int64, .true.), "unmatched")
        if (allocated(error)) return
        call goto_char(point_max())
        call insert(")", st)
        call check(error, default_is(point() - 1, point(), point_min(), point_min() + 1, .false.), "backward")
        if (allocated(error)) return
        call goto_char(point_min())
        call check(error, default_is(point(), point() + 1, point_max() - 1, point_max(), .false.), "forward")
        call kill_buffer(h, st)
    end subroutine test_default

    !> The context line of the open paren matching the close paren at the
    !> end of the buffer.
    function open_paren_line() result(s)
        character(len=:), allocatable :: s
        type(show_paren_data_type) :: data
        logical :: found
        call show_paren_default(found, data)
        s = blink_paren_open_paren_line_string(min(data%here_beg, data%there_beg))
    end function open_paren_line

    subroutine test_open_paren_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        ! Lisp-like
        call temp_buffer(h, "(defun foo ()"//achar(10)//"                  (dummy))")
        call goto_char(point_max())
        call check(error, open_paren_line(), "(defun foo ()")
        call kill_buffer(h, st)
        if (allocated(error)) return
        ! C-like
        call temp_buffer(h, "int foo() {"//achar(10)//"                 int blah;"//achar(10)//"             }")
        call goto_char(point_max())
        call check(error, open_paren_line(), "int foo() {")
        call kill_buffer(h, st)
        if (allocated(error)) return
        ! C-like with hanging {
        call temp_buffer(h, "int foo()"//achar(10)//"               {"//achar(10)//"                 int blah;"// &
                         achar(10)//"               }")
        call goto_char(point_max())
        call check(error, open_paren_line(), "int foo()...{")
        call kill_buffer(h, st)
    end subroutine test_open_paren_line

    !> The face of the overlay at POS, or "" for none.
    function face_at(pos) result(name)
        integer(int64), intent(in) :: pos
        character(len=:), allocatable :: name
        type(handle_type), allocatable :: vec(:)
        type(value_type) :: face
        integer :: k
        name = ""
        call overlays_at(pos, vec, sorted=.true.)
        do k = 1, size(vec)
            face = overlay_get(vec(k), "face")
            if (value_eq(face, sym("show_paren_match"))) name = "show_paren_match"
            if (value_eq(face, sym("show_paren_mismatch"))) name = "show_paren_mismatch"
            if (value_eq(face, sym("show_paren_match_expression"))) name = "show_paren_match_expression"
            if (len(name) > 0) return
        end do
    end function face_at

    !> show-paren-function puts the faces on the paren and its match, the
    !> mismatch face on mismatched parens, the expression face with the
    !> `expression' style, and removes the highlighting away from parens.
    subroutine test_function(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call temp_buffer(h, "(a (b) c)")
        call show_alone()
        call goto_char(point_max())
        call show_paren_function(st)
        call check(error, face_at(1_int64) == "show_paren_match" .and. face_at(9_int64) == "show_paren_match" .and. &
                   face_at(2_int64) == "", "both parens after a close paren")
        if (allocated(error)) return
        call goto_char(4_int64)
        call show_paren_function(st)
        call check(error, face_at(4_int64) == "show_paren_match" .and. face_at(6_int64) == "show_paren_match" .and. &
                   face_at(1_int64) == "", "before an open paren")
        if (allocated(error)) return
        show_paren_style = SHOW_PAREN_EXPRESSION
        call show_paren_function(st)
        call check(error, face_at(5_int64) == "show_paren_match_expression", "the expression")
        show_paren_style = SHOW_PAREN_PARENTHESIS
        if (allocated(error)) return
        call goto_char(2_int64)
        call show_paren_function(st)
        call check(error, face_at(1_int64) == "" .and. face_at(4_int64) == "", "no paren near point")
        if (allocated(error)) return
        call goto_char(point_max())
        call insert(" (]", st)
        call show_paren_function(st)
        call check(error, face_at(point_max() - 1) == "show_paren_mismatch" .and. &
                   face_at(point_max() - 2) == "show_paren_mismatch", "mismatched")
        call kill_buffer(h, st)
    end subroutine test_function

    !> Show Paren mode is on by default; not in special-mode buffers; and
    !> Show Paren Local mode turns it off in one buffer.
    subroutine test_modes(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call check(error, minor_modes%show_paren_mode%enabled(), "on by default")
        if (allocated(error)) return
        call temp_buffer(h, "(a)")
        call show_alone()
        call special_mode(st)
        call show_paren_function(st)
        call check(error, face_at(1_int64) == "", "not in special mode")
        if (allocated(error)) return
        call fundamental_mode(st)
        call show_paren_function(st)
        call check(error, face_at(1_int64) == "show_paren_match", "in fundamental mode")
        if (allocated(error)) return
        call minor_modes%show_paren_local_mode%disable(st)
        call check(error,.not. minor_modes%show_paren_local_mode%enabled() .and. face_at(1_int64) == "", &
                   "locally off")
        if (allocated(error)) return
        call show_paren_function(st)
        call check(error, face_at(1_int64) == "" .and. minor_modes%show_paren_mode%enabled(), &
                   "still globally on")
        if (allocated(error)) return
        call minor_modes%show_paren_local_mode%enable(st)
        call show_paren_function(st)
        call check(error, face_at(1_int64) == "show_paren_match", "locally on again")
        call kill_buffer(h, st)
    end subroutine test_modes

    subroutine write_x(status)
        type(status_type), intent(out) :: status
        call emacs_write(master, "x", status)
    end subroutine write_x

    !> The idle timer highlights the parens while the command loop waits
    !> for input (bounded: a timer types a key after half a second).
    subroutine test_idle_timer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, key_timer
        type(status_type) :: st
        type(emacs_tty_type) :: old
        type(value_type) :: ev
        integer(int32) :: slave
        logical :: have
        call init_bindings()
        call temp_buffer(h, "(abc)")
        call show_alone()
        call discard_input()
        call open_pty(master, slave, st)
        call check(error, st%ok(), "pty")
        if (allocated(error)) return
        call init_sys_modes(slave, old, st)
        call set_input_fd(slave)
        call run_with_timer(key_timer, 0.5_real64, write_x, st)
        call read_event(ev, have, st)
        call check(error, have .and. face_at(1_int64) == "show_paren_match" .and. &
                   face_at(5_int64) == "show_paren_match", "highlighted after the delay")
        call free_timer(key_timer)
        call reset_sys_modes(slave, old, st)
        call set_input_fd(-1_int32)
        call close_fd(slave)
        call close_fd(master)
        call discard_input()
        call kill_buffer(h, st)
    end subroutine test_idle_timer
end module test_paren_fm
