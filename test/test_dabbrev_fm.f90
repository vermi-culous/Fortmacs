! test_dabbrev_fm.f90 -- tests for dabbrev_fm.
! Ports test/lisp/dabbrev-tests.el where the ported functionality allows.
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

!> dabbrev-tests.el's cases in buffers (the resource files are in
!> test/dabbrev-resources).  Not ported: the dabbrev-completion tests
!> (C-M-/ is not ported), dabbrev-expand-test-other-buffer-3 (it needs
!> emacs-lisp-mode) and the minibuffer tests (they insert into the
!> minibuffer window selected without an active minibuffer).  Own tests:
!> cycling, case replacement, case-sensitive search.
module test_dabbrev_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, erase_buffer, restore_buffer_modified_p, &
                         get_buffer, buffer_live_p, buffer_list, buffers, resolve_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, buffer_string, buffer_substring, point, point_min, point_max, pos_bol, pos_eol, &
                          narrow_to_region, region_beginning, region_end, delete_region
    use fileio_fm, only: expand_file_name, emacs_wd, init_fileio, insert_file_contents
    use files_fm, only: find_file_noselect
    use window_fm, only: switch_to_buffer
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, discard_input, command_loop, last_message
    use callint_fm, only: prefix_arg, current_prefix_arg
    use bindings_fm, only: init_bindings
    use simple_fm, only: fundamental_mode
    use dabbrev_fm, only: dabbrev_reset_global_variables, dabbrev_case_fold_search, DABBREV_OFF, &
                          DABBREV_LIKE_CASE_FOLD_SEARCH
    implicit none(type, external)
    private

    public :: collect_dabbrev_fm

    character(len=*), parameter :: LF = achar(10)

contains

    subroutine collect_dabbrev_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("dabbrev-expand-test", test_expand), &
                    new_unittest("dabbrev-expand-test-same-buffer-1", test_same_buffer_1), &
                    new_unittest("dabbrev-expand-test-same-buffer-2", test_same_buffer_2), &
                    new_unittest("dabbrev-expand-test-same-buffer-3", test_same_buffer_3), &
                    new_unittest("dabbrev-expand-test-same-buffer-4", test_same_buffer_4), &
                    new_unittest("dabbrev-expand-test-other-buffer-1", test_other_buffer_1), &
                    new_unittest("dabbrev-expand-test-other-buffer-2", test_other_buffer_2), &
                    new_unittest("dabbrev-expand-test-other-buffer-4", test_other_buffer_4), &
                    new_unittest("dabbrev-expand-after-killing-buffer", test_after_killing_buffer), &
                    new_unittest("cycling", test_cycling), &
                    new_unittest("case-replace", test_case_replace), &
                    new_unittest("case-fold-off", test_case_fold_off) &
                    ]
    end subroutine collect_dabbrev_fm

    ! ------------------------------------------------------------------
    ! Fixtures
    ! ------------------------------------------------------------------

    !> A fresh buffer NAME shown alone, the other buffers (that earlier
    !> suites left) killed but *scratch* and the minibuffers: dabbrev
    !> searches all buffers, and upstream runs each test file in a new
    !> Emacs.
    subroutine fresh(name, h)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        type(handle_type), allocatable :: all(:)
        integer(int64) :: i
        integer :: k
        all = buffer_list()
        do k = 1, size(all)
            call resolve_buffer(all(k), i, st)
            if (.not. st%ok()) cycle
            if (buffers(i)%name == "*scratch*" .or. index(buffers(i)%name, " *Minibuf-") == 1) cycle
            call set_buffer(all(k), st)
            call restore_buffer_modified_p(.false.)
            call kill_buffer(all(k), st)
        end do
        call init_bindings()
        call discard_input()
        prefix_arg = nil()
        current_prefix_arg = nil()
        call dabbrev_reset_global_variables()
        call get_buffer_create(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call fundamental_mode(st)
        call show_alone()
        last_message = ""
    end subroutine fresh

    !> Type KEYS (kbd syntax) to the command loop.
    subroutine run(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        call key_parse(keys, evs, st)
        call unread_command_events(evs)
        call command_loop(st)
    end subroutine run

    !> The file NAME of test/dabbrev-resources.
    function resource(name) result(path)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path
        type(status_type) :: st
        call init_fileio()
        call expand_file_name("test/dabbrev-resources/"//name, path, st, emacs_wd)
    end function resource

    !> Visit the resource file NAME in the selected window (find-file).
    subroutine find_resource(name, h)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        call find_file_noselect(resource(name), h, st)
        call set_buffer(h, st)
        call show_alone()
    end subroutine find_resource

    !> Kill the visiting buffer H unmodified (with-dabbrev-test).
    subroutine done(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st
        if (.not. buffer_live_p(h)) return
        call set_buffer(h, st)
        call restore_buffer_modified_p(.false.)
        call kill_buffer(h, st)
        call dabbrev_reset_global_variables()
    end subroutine done

    !> The text of the current line.
    function line() result(s)
        character(len=:), allocatable :: s
        type(status_type) :: st
        call buffer_substring(pos_bol(), pos_eol(), s, st)
    end function line

    ! ------------------------------------------------------------------
    ! Ported from dabbrev-tests.el
    ! ------------------------------------------------------------------

    !> Test for bug#1948.
    !> When `dabbrev_eliminate_newlines' is true (the default),
    !> repeated calls to `dabbrev_expand' can result in the source of
    !> first expansion being replaced rather than the destination.
    subroutine test_expand(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh("*dabbrev-test*", h)
        call insert("ab  x"//LF//"a"//LF//"ab  y", st)
        call goto_char(8_int64)
        call run("M-/ SPC M-/ M-/")
        call check(error, buffer_string(), "ab  x"//LF//"ab y"//LF//"ab  y")
        call kill_buffer(h, st)
    end subroutine test_expand

    !> Test expanding a string twice within a single buffer.
    subroutine test_same_buffer_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, s
        type(status_type) :: st
        call fresh("*dabbrev-test*", s)
        call find_resource("INSTALL_BEGIN", h)
        call goto_char(point_max())
        call insert(LF, st)
        call run("Ind M-/")
        call check(error, line(), "Indic")
        if (allocated(error)) return
        call run("SPC M-/")
        call check(error, line(), "Indic and")
        call done(h)
        call kill_buffer(s, st)
    end subroutine test_same_buffer_1

    !> Test expanding a string plus space twice within a single buffer.
    subroutine test_same_buffer_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, s
        type(status_type) :: st
        call fresh("*dabbrev-test*", s)
        call find_resource("INSTALL_BEGIN", h)
        call goto_char(point_max())
        call insert(LF, st)
        call run("Indic SPC M-/")
        call check(error, line(), "Indic and")
        if (allocated(error)) return
        call run("SPC M-/")
        call check(error, line(), "Indic and Khmer")
        call done(h)
        call kill_buffer(s, st)
    end subroutine test_same_buffer_2

    !> Test replacing an expansion within a single buffer.
    subroutine test_same_buffer_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, s
        type(status_type) :: st
        integer(int64) :: inserted
        call fresh("*dabbrev-test*", s)
        call find_resource("INSTALL_BEGIN", h)
        call goto_char(point_max())
        call insert(LF, st)
        call insert_file_contents(resource("dabbrev-expand.el"), .false., inserted, st)
        call goto_char(point_max())
        call insert(LF, st)
        call run("Ind M-/")
        call check(error, line(), "Indicate")
        if (allocated(error)) return
        call run("<C-S-backspace>")
        call run("Ind M-/ M-/")
        call check(error, line(), "Indic")
        if (allocated(error)) return
        call run("SPC M-/")
        call check(error, line(), "Indic and")
        call done(h)
        call kill_buffer(s, st)
    end subroutine test_same_buffer_3

    !> Narrow INSTALL_BEGIN to the paragraph with "Ind" (the keys "C-s
    !> Ind M-a C-SPC M-}" and the narrowing of C-x n n).
    subroutine narrow_to_ind()
        integer(int64) :: b, e
        type(status_type) :: st
        call goto_char(point_min())
        call run("C-s Ind M-a C-SPC M-}")
        call region_beginning(b, st)
        call region_end(e, st)
        call narrow_to_region(b, e, st)
    end subroutine narrow_to_ind

    !> Test expanding a string in a narrowed-region.
    subroutine test_same_buffer_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, s
        type(status_type) :: st
        call fresh("*dabbrev-test*", s)
        call find_resource("INSTALL_BEGIN", h)
        call narrow_to_ind()
        call goto_char(point_max())
        call insert(LF, st)
        call run("Ind M-/")
        call check(error, line(), "Indic")
        if (allocated(error)) return
        call run("SPC M-/")
        call check(error, line(), "Indic and")
        call done(h)
        call kill_buffer(s, st)
    end subroutine test_same_buffer_4

    !> Test expanding a prefix string to a string from another buffer.
    subroutine test_other_buffer_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, a
        type(status_type) :: st
        call fresh("*dabbrev-test*", a)
        call kill_buffer(a, st)
        call find_resource("INSTALL_BEGIN", h)
        call get_buffer_create("a", a, st, inhibit_buffer_hooks=.true.)
        call switch_to_buffer("a", st)
        call run("Ind M-/")
        call check(error, buffer_string(), "Indic")
        if (allocated(error)) return
        call run("SPC M-/")
        call check(error, buffer_string(), "Indic and")
        call kill_buffer(a, st)
        call done(h)
    end subroutine test_other_buffer_1

    !> Test expanding a string + space to a string from another buffer.
    subroutine test_other_buffer_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, a
        type(status_type) :: st
        call fresh("*dabbrev-test*", a)
        call kill_buffer(a, st)
        call find_resource("INSTALL_BEGIN", h)
        call get_buffer_create("a", a, st, inhibit_buffer_hooks=.true.)
        call switch_to_buffer("a", st)
        call run("Indic SPC M-/")
        call check(error, buffer_string(), "Indic and")
        if (allocated(error)) return
        call run("SPC M-/")
        call check(error, buffer_string(), "Indic and Khmer")
        call kill_buffer(a, st)
        call done(h)
    end subroutine test_other_buffer_2

    !> Test expanding a string using another narrowed buffer.
    subroutine test_other_buffer_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, a
        type(status_type) :: st
        call fresh("*dabbrev-test*", a)
        call kill_buffer(a, st)
        call find_resource("INSTALL_BEGIN", h)
        call narrow_to_ind()
        call get_buffer_create("a", a, st, inhibit_buffer_hooks=.true.)
        call switch_to_buffer("a", st)
        call run("Ind M-/")
        call check(error, buffer_string(), "Indic")
        if (allocated(error)) return
        call run("SPC M-/")
        call check(error, buffer_string(), "Indic and")
        call kill_buffer(a, st)
        call done(h)
    end subroutine test_other_buffer_4

    !> Test expansion after killing buffer containing first expansion.
    !> Finding successive expansions in another live buffer should succeed, but
    !> after killing the buffer, expansion should fail with a user-error,
    !> leaving the unexpanded string in the buffer.  (See bug#74090.)
    subroutine test_after_killing_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: foo, scratch
        type(status_type) :: st
        call fresh("foo", foo)
        call insert("abc abd", st)
        call get_buffer_create("*scratch*", scratch, st)
        call switch_to_buffer("*scratch*", st)
        call erase_buffer(st)
        call run("ab M-/")
        call check(error, buffer_string(), "abc")
        if (allocated(error)) return
        call run("SPC ab M-/")
        call check(error, buffer_string(), "abc abc")
        if (allocated(error)) return
        call erase_buffer(st)
        call run("abc SPC ab M-/ M-/")
        call check(error, buffer_string(), "abc abd")
        if (allocated(error)) return
        call kill_buffer(foo, st)
        call set_buffer(scratch, st)
        call erase_buffer(st)
        last_message = ""
        call run("abc SPC ab M-/ M-/")
        call check(error, buffer_string(), "abc ab")
        if (allocated(error)) return
        call check(error, last_message, "No further dynamic expansion for "//char(226)//char(128)//char(152)// &
                   "ab"//char(226)//char(128)//char(153)//" found")
        call erase_buffer(st)
        call dabbrev_reset_global_variables()
    end subroutine test_after_killing_buffer

    ! ------------------------------------------------------------------
    ! Own tests
    ! ------------------------------------------------------------------

    !> Repeated M-/ offers the next distinct expansion, then puts the
    !> abbrev back.
    subroutine test_cycling(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh("*dabbrev-test*", h)
        call insert("foo1 foo2 foo1 ", st)
        call run("fo M-/")
        call check(error, buffer_string(), "foo1 foo2 foo1 foo1")
        if (allocated(error)) return
        call run("M-/")
        call check(error, buffer_string(), "foo1 foo2 foo1 foo2")
        if (allocated(error)) return
        call run("M-/")
        call check(error, buffer_string(), "foo1 foo2 foo1 fo")
        if (allocated(error)) return
        call check(error, last_message, "No further dynamic expansion for "//char(226)//char(128)//char(152)// &
                   "fo"//char(226)//char(128)//char(153)//" found")
        if (allocated(error)) return
        ! A prefix argument takes the Nth distinct expansion back.
        call delete_region(point_min(), point_max(), st)
        call insert("foo1 foo2 ", st)
        call run("fo C-u 2 M-/")
        call check(error, buffer_string(), "foo1 foo2 foo1")
        if (allocated(error)) return
        ! At the beginning of the buffer there is no abbreviation.
        call delete_region(point_min(), point_max(), st)
        call run("C-e M-/")
        call check(error, last_message, "No possible abbreviation preceding point")
        call kill_buffer(h, st)
    end subroutine test_cycling

    !> With case folding, the abbreviation's case pattern applies to the
    !> expansion.
    subroutine test_case_replace(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh("*dabbrev-test*", h)
        call insert("abcdef"//LF, st)
        call run("Ab M-/ SPC AB M-/")
        call check(error, buffer_string(), "abcdef"//LF//"Abcdef ABCDEF")
        call kill_buffer(h, st)
    end subroutine test_case_replace

    subroutine test_case_fold_off(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh("*dabbrev-test*", h)
        call insert("Abc"//LF, st)
        dabbrev_case_fold_search = DABBREV_OFF
        call run("ab M-/")
        dabbrev_case_fold_search = DABBREV_LIKE_CASE_FOLD_SEARCH
        call check(error, buffer_string(), "Abc"//LF//"ab")
        if (allocated(error)) return
        call check(error, last_message, "No dynamic expansion for "//char(226)//char(128)//char(152)// &
                   "ab"//char(226)//char(128)//char(153)//" found")
        call kill_buffer(h, st)
    end subroutine test_case_fold_off
end module test_dabbrev_fm
