! test_electric_fm.f90 -- tests for Electric Indent mode (electric_fm)
! and `post_self_insert_hook' (cmds_fm).
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

!> Own tests: electric-tests.el's Electric Indent cases run in c-mode,
!> js-mode and python-mode, which are not ported; these check the same
!> rules in Fundamental mode (whose `indent_relative' can't reindent, so
!> a new line gets the previous line's indentation).  Keys go through
!> the command loop.  Every test kills its buffers.
module test_electric_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: nil
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_max, buffer_string
    use keyboard_fm, only: kbd_store_bytes, command_loop, discard_input
    use callint_fm, only: prefix_arg
    use cmds_fm, only: post_self_insert_hook
    use simple_fm, only: fundamental_mode
    use electric_fm, only: electric_indent_inhibit, electric_indent_chars
    use modes_fm, only: minor_modes
    use bindings_fm, only: init_bindings
    use test_support, only: show_alone
    implicit none(type, external)
    private

    public :: collect_electric_fm

    !> Calls of the test hook function.
    integer :: hook_calls = 0

contains

    subroutine collect_electric_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("post-self-insert-hook", test_post_self_insert_hook), &
                    new_unittest("electric-indent-mode-on-by-default", test_default_on), &
                    new_unittest("electric-indent-newline-reindents", test_newline), &
                    new_unittest("electric-indent-off", test_off), &
                    new_unittest("electric-newline-and-maybe-indent", test_c_j), &
                    new_unittest("electric-indent-local-mode", test_local_mode), &
                    new_unittest("electric-indent-chars", test_chars), &
                    new_unittest("electric-indent-inhibit-buffer-local", test_inhibit_local) &
                    ]
    end subroutine collect_electric_fm

    !> A fresh Fundamental mode buffer holding TEXT, point at its end, shown
    !> in the selected window; the bindings and modes as at startup.
    subroutine fresh(h, text, name)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: text
        character(len=*), intent(in), optional :: name
        type(status_type) :: st
        call init_bindings()
        call discard_input()
        prefix_arg = nil()
        if (present(name)) then
            call get_buffer_create(name, h, st)
        else
            call get_buffer_create("*electric-test*", h, st)
        end if
        call set_buffer(h, st)
        call fundamental_mode(st)
        call insert(text, st)
        call show_alone()
    end subroutine fresh

    !> Type KEYS.
    subroutine keys(text)
        character(len=*), intent(in) :: text
        type(status_type) :: st
        call kbd_store_bytes(text)
        call command_loop(st)
    end subroutine keys

    subroutine count_hook(status)
        type(status_type), intent(out) :: status
        status = status_type()
        hook_calls = hook_calls + 1
    end subroutine count_hook

    !> self-insert-command runs `post_self_insert_hook' after inserting,
    !> once per command (also with a repeat count).
    subroutine test_post_self_insert_hook(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h, "")
        hook_calls = 0
        call post_self_insert_hook%add(count_hook)
        call keys("ab"//achar(21)//"3c")
        call post_self_insert_hook%remove(count_hook)
        call check(error, buffer_string(), "abccc")
        if (allocated(error)) return
        call check(error, hook_calls == 3, "three commands")
        call kill_buffer(h, st)
    end subroutine test_post_self_insert_hook

    subroutine test_default_on(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call check(error, minor_modes%electric_indent_mode%enabled() .and. &
                   .not. minor_modes%electric_pair_mode%enabled(), "Electric Indent on, Electric Pair off")
    end subroutine test_default_on

    !> RET indents the new line like the previous one and deletes the
    !> trailing whitespace of the previous line.
    subroutine test_newline(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h, "  foo   ")
        call keys(achar(13))
        call check(error, buffer_string(), "  foo"//achar(10)//"  ")
        if (allocated(error)) return
        call check(error, point() == point_max(), "at the end")
        call kill_buffer(h, st)
    end subroutine test_newline

    !> Without Electric Indent mode RET just opens a line.
    subroutine test_off(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h, "  foo   ")
        call minor_modes%electric_indent_mode%disable(st)
        call keys(achar(13))
        call check(error, buffer_string(), "  foo   "//achar(10))
        call minor_modes%electric_indent_mode%enable(st)
        call kill_buffer(h, st)
    end subroutine test_off

    !> C-j inserts just a newline when Electric Indent mode is on, and
    !> indents (newline-and-indent) when it is off.
    subroutine test_c_j(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h, "  foo")
        call keys(achar(10))
        call check(error, buffer_string(), "  foo"//achar(10))
        call kill_buffer(h, st)
        if (allocated(error)) return
        call fresh(h, "  foo  ")
        call minor_modes%electric_indent_mode%disable(st)
        call keys(achar(10))
        call check(error, buffer_string(), "  foo"//achar(10)//"  ")
        call minor_modes%electric_indent_mode%enable(st)
        call kill_buffer(h, st)
    end subroutine test_c_j

    !> Electric Indent Local mode turns the mode off in one buffer only.
    subroutine test_local_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        call fresh(b, "  bar", "*electric-b*")
        call fresh(a, "  foo", "*electric-a*")
        call minor_modes%electric_indent_local_mode%disable(st)
        call check(error,.not. minor_modes%electric_indent_local_mode%enabled() .and. &
                   minor_modes%electric_indent_mode%enabled(), "locally off, globally on")
        if (allocated(error)) return
        call keys(achar(13))
        call check(error, buffer_string(), "  foo"//achar(10))
        if (allocated(error)) return
        call set_buffer(b, st)
        call show_alone()
        call keys(achar(13))
        call check(error, buffer_string(), "  bar"//achar(10)//"  ")
        if (allocated(error)) return
        call set_buffer(a, st)
        call minor_modes%electric_indent_local_mode%enable(st)
        call check(error, minor_modes%electric_indent_local_mode%enabled(), "back on")
        call kill_buffer(a, st)
        call kill_buffer(b, st)
    end subroutine test_local_mode

    !> Only `electric_indent_chars' reindent.
    subroutine test_chars(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h, "  foo"//achar(10)//"x")
        call keys("}")
        call check(error, buffer_string(), "  foo"//achar(10)//"x}")
        if (allocated(error)) return
        electric_indent_chars = [10, iachar("}")]
        call keys("}")
        call check(error, buffer_string(), "  foo"//achar(10)//"  x}}")
        electric_indent_chars = [10]
        call kill_buffer(h, st)
    end subroutine test_chars

    !> `electric_indent_inhibit' is buffer-local.
    subroutine test_inhibit_local(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        call fresh(a, "", "*electric-a*")
        call electric_indent_inhibit%set_local(.true.)
        call fresh(b, "", "*electric-b*")
        call check(error,.not. electric_indent_inhibit%value, "not in another buffer")
        if (allocated(error)) return
        call set_buffer(a, st)
        call check(error, electric_indent_inhibit%value, "in its buffer")
        call kill_buffer(a, st)
        call kill_buffer(b, st)
    end subroutine test_inhibit_local
end module test_electric_fm
