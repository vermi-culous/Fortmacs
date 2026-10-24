! test_view_fm.f90 -- tests for view_fm (view.el) and Read-Only mode
! (simple.el's read-only-mode, files.el's find-file-read-only).
! Our own: upstream has no view-tests.el.
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

module test_view_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use bindings_fm, only: init_bindings
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, current_buffer, erase_buffer, &
                         get_file_buffer, buffer_live_p
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, goto_char
    use keyboard_fm, only: discard_input, kbd_store_bytes, command_loop, last_message
    use fileio_fm, only: init_fileio, emacs_wd, file_name_as_directory, write_region, make_directory_internal, &
                         file_directory_p
    use files_fm, only: delete_directory, view_read_only
    use simple_fm, only: line_number_at_pos
    use window_fm, only: windows, window_start
    use frame_fm, only: selected_window
    use view_fm, only: view_buffer, view_file, view_mode_enabled
    implicit none(type, external)
    private

    public :: collect_view_fm

    character(len=*), parameter :: subdir = "build/test-view"

    character, parameter :: CR = achar(13)

contains

    subroutine collect_view_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("view-buffer-enter-and-exit", test_enter_exit), &
                    new_unittest("view-scroll-and-goto", test_scroll_goto), &
                    new_unittest("view-search", test_search), &
                    new_unittest("view-file-quit-kills", test_view_file_quit), &
                    new_unittest("find-file-read-only-view", test_find_file_read_only), &
                    new_unittest("read-only-mode", test_read_only_mode) &
                    ]
    end subroutine collect_view_fm

    function tdir() result(dir)
        character(len=:), allocatable :: dir
        dir = file_name_as_directory(file_name_as_directory(emacs_wd)//subdir)
    end function tdir

    !> A buffer "lines" of 100 numbered lines, current and shown alone,
    !> with *scratch* shown before it.
    subroutine setup(buf)
        type(handle_type), intent(out) :: buf
        type(handle_type) :: scratch
        type(status_type) :: st
        character(len=12) :: line
        integer :: k
        call init_bindings()
        call init_fileio()
        call discard_input()
        view_read_only = .false.
        call get_buffer_create("*scratch*", scratch, st)
        call set_buffer(scratch, st)
        call show_alone()
        call get_buffer_create("lines", buf, st)
        call set_buffer(buf, st)
        call erase_buffer(st)
        do k = 1, 100
            write (line, "('line ',i0)") k
            call insert(trim(line)//achar(10), st)
        end do
        call goto_char(point_min())
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
        call make_directory_internal(tdir(), st)
    end subroutine setup

    subroutine teardown(buf)
        type(handle_type), intent(in) :: buf
        type(handle_type) :: scratch
        type(status_type) :: st
        call discard_input()
        if (buffer_live_p(buf)) call kill_buffer(buf, st)
        call get_buffer_create("*scratch*", scratch, st)
        call set_buffer(scratch, st)
        call show_alone()
        view_read_only = .false.
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
    end subroutine teardown

    subroutine keys(text)
        character(len=*), intent(in) :: text
        type(status_type) :: st
        call kbd_store_bytes(text)
        call command_loop(st)
    end subroutine keys

    !> `view_buffer' makes the buffer read-only; e exits, restoring it.
    subroutine test_enter_exit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        call setup(buf)
        body: block
            call view_buffer(buf, st)
            call check(error, st%ok() .and. view_mode_enabled() .and. buffers(cur)%read_only, format_error(st))
            if (allocated(error)) exit body
            call keys("e")
            call check(error,.not. view_mode_enabled() .and. .not. buffers(cur)%read_only, "e exits")
            if (allocated(error)) exit body
            call view_buffer(buf, st)
            ! q leaves the window to its previous buffer.
            call keys("q")
            call check(error, buffers(cur)%name, "*scratch*")
        end block body
        call teardown(buf)
    end subroutine test_enter_exit

    !> SPC scrolls a page, 5g goes to line 5, = tells the line, > the end.
    subroutine test_scroll_goto(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: w
        type(handle_type) :: buf
        type(status_type) :: st
        call setup(buf)
        body: block
            call view_buffer(buf, st)
            call keys(" ")
            w = selected_window()
            call check(error, window_start(windows(w%index)) > 1, "scrolled")
            if (allocated(error)) exit body
            call keys("5g")
            call check(error, line_number_at_pos() == 5_int64, "line 5")
            if (allocated(error)) exit body
            call keys("=")
            call check(error, last_message, "Line 5")
            if (allocated(error)) exit body
            call keys("<")
            call check(error, point() == 1_int64, "beginning")
        end block body
        call teardown(buf)
    end subroutine test_scroll_goto

    !> / searches a regexp; n repeats it; a failure says so.
    subroutine test_search(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        call setup(buf)
        body: block
            call view_buffer(buf, st)
            call keys("/line 4."//CR)
            call check(error, line_number_at_pos() == 40_int64, "line 40")
            if (allocated(error)) exit body
            call keys("n")
            call check(error, line_number_at_pos() == 41_int64, "line 41")
            if (allocated(error)) exit body
            call keys("/nothing"//CR)
            call check(error, last_message, "Can't find occurrence 1 of nothing")
        end block body
        call teardown(buf)
    end subroutine test_search

    !> A file viewed with `view_file' is killed by q.
    subroutine test_view_file_quit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf, fbuf
        type(status_type) :: st
        call setup(buf)
        body: block
            call set_buffer(buf, st)
            call write_region(tdir()//"f.txt", .false., st)
            call get_buffer_create("*scratch*", fbuf, st)
            call set_buffer(fbuf, st)
            call show_alone()
            call view_file(tdir()//"f.txt", st)
            fbuf = get_file_buffer(tdir()//"f.txt")
            call check(error, st%ok() .and. fbuf%index /= 0 .and. view_mode_enabled(), format_error(st))
            if (allocated(error)) exit body
            call keys("q")
            call check(error,.not. buffer_live_p(fbuf) .and. buffers(cur)%name == "*scratch*", "killed")
        end block body
        call teardown(buf)
    end subroutine test_view_file_quit

    !> C-x C-r visits read-only, in View mode with `view_read_only'.
    subroutine test_find_file_read_only(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf, fbuf
        type(status_type) :: st
        call setup(buf)
        body: block
            call set_buffer(buf, st)
            call write_region(tdir()//"g.txt", .false., st)
            call get_buffer_create("*scratch*", fbuf, st)
            call set_buffer(fbuf, st)
            call show_alone()
            call keys(achar(24)//achar(18)//tdir()//"g.txt"//CR)
            call check(error, buffers(cur)%read_only .and. .not. view_mode_enabled(), "read-only")
            if (allocated(error)) exit body
            fbuf = current_buffer()
            call kill_buffer(fbuf, st)
            view_read_only = .true.
            call keys(achar(24)//achar(18)//tdir()//"g.txt"//CR)
            call check(error, buffers(cur)%read_only .and. view_mode_enabled(), "in View mode")
            if (allocated(error)) exit body
            fbuf = current_buffer()
            call kill_buffer(fbuf, st)
        end block body
        call teardown(buf)
    end subroutine test_find_file_read_only

    !> C-x C-q toggles read-only; leaving it leaves View mode.
    subroutine test_read_only_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        call setup(buf)
        body: block
            call show_alone()
            call keys(achar(24)//achar(17))
            call check(error, buffers(cur)%read_only, "read-only")
            if (allocated(error)) exit body
            call check(error, last_message, "Read-Only mode enabled in current buffer")
            if (allocated(error)) exit body
            view_read_only = .true.
            call keys(achar(24)//achar(17)//achar(24)//achar(17))
            call check(error, buffers(cur)%read_only .and. view_mode_enabled(), "View mode")
            if (allocated(error)) exit body
            call keys(achar(24)//achar(17))
            call check(error,.not. buffers(cur)%read_only .and. .not. view_mode_enabled(), "editable")
        end block body
        call teardown(buf)
    end subroutine test_read_only_mode
end module test_view_fm
