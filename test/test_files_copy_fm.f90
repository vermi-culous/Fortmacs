! test_files_copy_fm.f90 -- tests for copying, renaming and linking files
! (fileio_fm) and files.el's copy-directory and symbolic modes
! (files_fm's submodule files_copy_fm).
! Ported from GNU Emacs 31.1 test/lisp/files-tests.el
! (files-tests-file-modes-symbolic-to-number,
! files-tests-file-modes-number-to-symbolic, files-tests-copy-directory);
! the others are our own.
!
! Copyright (C) 2012-2026 Free Software Foundation, Inc.
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

module test_files_copy_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use bindings_fm, only: init_bindings
    use error_fm, only: status_type, format_error, FM_FILE_ALREADY_EXISTS
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, current_buffer, erase_buffer
    use insdel_fm, only: insert
    use fileio_fm, only: init_fileio, emacs_wd, file_name_as_directory, file_exists_p, file_directory_p, &
                         write_region, make_directory_internal, copy_file, rename_file, add_name_to_file, &
                         make_symbolic_link, set_file_modes, file_modes, set_file_times, file_attributes, &
                         file_stat_type, file_symlink_target, default_file_modes
    use files_fm, only: delete_directory, copy_directory, file_modes_symbolic_to_number, &
                        file_modes_number_to_symbolic, file_in_directory_p, make_empty_file
    implicit none(type, external)
    private

    public :: collect_files_copy_fm

    character(len=*), parameter :: subdir = "build/test-files-copy"

contains

    subroutine collect_files_copy_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("files-tests-file-modes-symbolic-to-number", test_symbolic_to_number), &
                    new_unittest("files-tests-file-modes-number-to-symbolic", test_number_to_symbolic), &
                    new_unittest("files-tests-copy-directory", test_copy_directory), &
                    new_unittest("copy-file-ok-if-already-exists", test_copy_file_exists), &
                    new_unittest("copy-file-keep-time", test_copy_keep_time), &
                    new_unittest("rename-file-ok-if-already-exists", test_rename_exists), &
                    new_unittest("add-name-to-file", test_add_name), &
                    new_unittest("file-in-directory-p", test_in_directory), &
                    new_unittest("make-empty-file", test_make_empty_file) &
                    ]
    end subroutine collect_files_copy_fm

    function tdir() result(dir)
        character(len=:), allocatable :: dir
        dir = file_name_as_directory(file_name_as_directory(emacs_wd)//subdir)
    end function tdir

    subroutine setup()
        type(status_type) :: st
        call init_bindings()
        call init_fileio()
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
        call make_directory_internal(tdir(), st)
    end subroutine setup

    subroutine teardown()
        type(status_type) :: st
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
    end subroutine teardown

    !> Write TEXT to file NAME (relative to the test directory).
    subroutine make_file(name, text)
        character(len=*), intent(in) :: name, text
        type(handle_type) :: buf, old
        type(status_type) :: st
        old = current_buffer()
        call get_buffer_create(" *files-copy-test*", buf, st)
        call set_buffer(buf, st)
        call erase_buffer(st)
        call insert(text, st)
        call write_region(tdir()//name, .false., st)
        call kill_buffer(buf, st)
        call set_buffer(old, st)
    end subroutine make_file

    !> Port of files-tests-file-modes-symbolic-to-number.
    subroutine test_symbolic_to_number(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=16), parameter :: modes(17) = [character(len=16) :: "a=rwx", "o=t", "o=xt", "o=tx", &
                                                                        "u=rwx,g=rx,o=rx", "u=rwx,g=,o=", "u=rwx", "u=rw,g=r,o=r", &
                                                                              "u=rw,g=r,o=t", "u=rw,g=r,o=xt", "u=rwxs,g=rs,o=xt", &
                                                                     "u=rws,g=rs,o=t", "u=rws,g=rs,o=r", "a=r", "u=S", "u=T", "u=Z"]
        integer, parameter :: expected(17) = [int(o'777'), int(o'1000'), int(o'1001'), int(o'1001'), int(o'755'), &
                                              int(o'700'), int(o'700'), int(o'644'), int(o'1640'), int(o'1641'), &
                                              int(o'7741'), int(o'7640'), int(o'6644'), int(o'444'), -1, -1, -1]
        type(status_type) :: st
        integer(int32) :: n
        integer :: k
        call init_fileio()
        do k = 1, size(modes)
            call file_modes_symbolic_to_number(trim(modes(k)), n, st)
            if (expected(k) < 0) then
                call check(error,.not. st%ok(), trim(modes(k))//" is an error")
            else
                call check(error, st%ok() .and. n == expected(k), trim(modes(k)))
            end if
            if (allocated(error)) return
        end do
        ! Relative to FROM.
        call file_modes_symbolic_to_number("og+rX-w", n, st, int(o'722', int32))
        call check(error, st%ok() .and. n == int(o'755', int32), "og+rX-w")
        if (allocated(error)) return
        call file_modes_symbolic_to_number("+x", n, st, 0_int32)
        call check(error, st%ok() .and. n == iand(int(o'111', int32), default_file_modes()), "+x uses the umask")
    end subroutine test_symbolic_to_number

    !> Port of files-tests-file-modes-number-to-symbolic.
    subroutine test_number_to_symbolic(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: modes(9) = [int(o'755'), int(o'700'), int(o'644'), int(o'1640'), int(o'1641'), &
                                          int(o'7741'), int(o'7640'), int(o'6644'), int(o'444')]
        character(len=10), parameter :: expected(9) = [character(len=10) :: "-rwxr-xr-x", "-rwx------", "-rw-r--r--", &
                                                                           "-rw-r----T", "-rw-r----t", "-rwsr-S--t", "-rwSr-S--T", &
                                                                                                         "-rwSr-Sr--", "-r--r--r--"]
        integer :: k
        do k = 1, size(modes)
            call check(error, file_modes_number_to_symbolic(int(modes(k), int32)), expected(k))
            if (allocated(error)) return
        end do
        call check(error, file_modes_number_to_symbolic(int(o'40755', int32)), "drwxr-xr-x")
    end subroutine test_number_to_symbolic

    !> Port of files-tests-copy-directory.
    subroutine test_copy_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: dirname, source, dest, source2, dest2, source3, dest3
        call setup()
        body: block
            dirname = tdir()
            source = dirname//"source"
            dest = dirname//"dest/new/directory/"
            source2 = dirname//"source2"
            dest2 = dirname//"dest/new2"
            source3 = dirname//"source3/d"
            dest3 = dirname//"dest3/d"
            call make_directory_internal(source, st)
            call make_file("source/file", "")
            call copy_directory(source, dest, st, keep_time=.true., parents=.true., copy_contents=.true.)
            call check(error, st%ok() .and. file_exists_p(dest//"file"), format_error(st))
            if (allocated(error)) exit body
            call make_directory_internal(source2, st)
            call make_directory_internal(source2//"/a", st)
            call copy_directory(source2, dest2, st)
            call check(error, st%ok() .and. file_directory_p(dest2//"/a"), format_error(st))
            if (allocated(error)) exit body
            call make_directory_internal(dirname//"source3", st)
            call make_directory_internal(source3, st)
            call make_file("source3/d/file", "x"//achar(10))
            call make_directory_internal(dirname//"dest3", st)
            call make_directory_internal(dest3, st)
            call make_file("dest3/d/file", "y"//achar(10))
            call copy_directory(source3, dirname//"dest3/", st, keep_time=.true.)
            call check(error, st%ok(), format_error(st))
        end block body
        call teardown()
    end subroutine test_copy_directory

    !> copy-file refuses an existing target unless OK-IF-ALREADY-EXISTS,
    !> and copies into a directory name.
    subroutine test_copy_file_exists(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call setup()
        body: block
            call make_file("a", "1")
            call make_file("b", "2")
            call copy_file(tdir()//"a", tdir()//"b", st)
            call check(error, st%code == FM_FILE_ALREADY_EXISTS, format_error(st))
            if (allocated(error)) exit body
            call check(error, format_error(st), "File already exists: "//tdir()//"b")
            if (allocated(error)) exit body
            call copy_file(tdir()//"a", tdir()//"b", st, ok_if_already_exists=.true.)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call make_directory_internal(tdir()//"d", st)
            call copy_file(tdir()//"a", tdir()//"d/", st)
            call check(error, st%ok() .and. file_exists_p(tdir()//"d/a"), format_error(st))
            if (allocated(error)) exit body
            call copy_file(tdir()//"d", tdir()//"e", st)
            call check(error, format_error(st), "Non-regular file: Is a directory, "//tdir()//"d")
        end block body
        call teardown()
    end subroutine test_copy_file_exists

    !> KEEP-TIME gives the copy the original's modification time.
    subroutine test_copy_keep_time(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(file_stat_type) :: a, b
        logical :: found
        call setup()
        body: block
            call make_file("a", "1")
            call set_file_times(tdir()//"a", st, 1000000000_int64, 0_int32)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call copy_file(tdir()//"a", tdir()//"b", st, keep_time=.true.)
            call file_attributes(tdir()//"a", a, found)
            call file_attributes(tdir()//"b", b, found)
            call check(error, st%ok() .and. a%mtime_sec == 1000000000_int64 .and. b%mtime_sec == a%mtime_sec, &
                       format_error(st))
            if (allocated(error)) exit body
            call copy_file(tdir()//"a", tdir()//"c", st)
            call file_attributes(tdir()//"c", b, found)
            call check(error, b%mtime_sec /= a%mtime_sec, "not kept")
        end block body
        call teardown()
    end subroutine test_copy_keep_time

    !> rename-file refuses an existing target unless OK-IF-ALREADY-EXISTS.
    subroutine test_rename_exists(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call setup()
        body: block
            call make_file("a", "1")
            call make_file("b", "2")
            call rename_file(tdir()//"a", tdir()//"b", st)
            call check(error, st%code == FM_FILE_ALREADY_EXISTS .and. file_exists_p(tdir()//"a"), format_error(st))
            if (allocated(error)) exit body
            call rename_file(tdir()//"a", tdir()//"b", st, ok_if_already_exists=.true.)
            call check(error, st%ok() .and. .not. file_exists_p(tdir()//"a"), format_error(st))
            if (allocated(error)) exit body
            call make_directory_internal(tdir()//"d", st)
            call rename_file(tdir()//"b", tdir()//"d/", st)
            call check(error, st%ok() .and. file_exists_p(tdir()//"d/b"), format_error(st))
        end block body
        call teardown()
    end subroutine test_rename_exists

    !> add-name-to-file makes a hard link.
    subroutine test_add_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(file_stat_type) :: a
        logical :: found
        call setup()
        body: block
            call make_file("a", "1")
            call add_name_to_file(tdir()//"a", tdir()//"b", st)
            call file_attributes(tdir()//"a", a, found)
            call check(error, st%ok() .and. a%nlink == 2, format_error(st))
            if (allocated(error)) exit body
            call add_name_to_file(tdir()//"a", tdir()//"b", st)
            call check(error, st%code == FM_FILE_ALREADY_EXISTS, format_error(st))
            if (allocated(error)) exit body
            call make_symbolic_link("a", tdir()//"l", st)
            call check(error, file_symlink_target(tdir()//"l"), "a")
        end block body
        call teardown()
    end subroutine test_add_name

    !> file-in-directory-p: a directory is inside itself.
    subroutine test_in_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call setup()
        body: block
            call make_directory_internal(tdir()//"d", st)
            call check(error, file_in_directory_p(tdir()//"d/x", tdir()//"d"), "inside")
            if (allocated(error)) exit body
            call check(error, file_in_directory_p(tdir()//"d", tdir()//"d"), "itself")
            if (allocated(error)) exit body
            call check(error,.not. file_in_directory_p(tdir()//"dd", tdir()//"d"), "dd is not in d")
            if (allocated(error)) exit body
            call check(error,.not. file_in_directory_p(tdir(), tdir()//"d"), "parent")
        end block body
        call teardown()
    end subroutine test_in_directory

    !> make-empty-file, with and without PARENTS.
    subroutine test_make_empty_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call setup()
        body: block
            call make_empty_file(tdir()//"x/z/y", st)
            call check(error,.not. st%ok(), "no parent")
            if (allocated(error)) exit body
            call make_empty_file(tdir()//"x/y", st, parents=.true.)
            call check(error, st%ok() .and. file_exists_p(tdir()//"x/y"), format_error(st))
            if (allocated(error)) exit body
            call make_empty_file(tdir()//"x/y", st)
            call check(error, format_error(st), "File exists: "//tdir()//"x/y")
        end block body
        call teardown()
    end subroutine test_make_empty_file
end module test_files_copy_fm
