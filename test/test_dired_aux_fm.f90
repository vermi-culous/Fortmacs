! test_dired_aux_fm.f90 -- tests for the Dired file operations
! (dired-aux.el, in dired_mode_fm's submodule dired_aux_fm).
! Ported from GNU Emacs 31.1 test/lisp/dired-aux-tests.el
! (dired-test-bug28834, dired-test-bug30624); the others are our own.
!
! Copyright (C) 2017-2026 Free Software Foundation, Inc.
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

module test_dired_aux_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support, only: show_alone
    use bindings_fm, only: init_bindings
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, current_buffer, erase_buffer, &
                         get_file_buffer, buffer_live_p, resolve_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, buffer_string, line_beginning_position, line_end_position, buffer_substring, goto_char
    use keyboard_fm, only: discard_input, kbd_store_bytes, command_loop, last_message
    use fileio_fm, only: init_fileio, emacs_wd, file_name_as_directory, file_exists_p, file_directory_p, write_region, &
                         make_directory_internal, file_modes, set_file_modes, file_symlink_target, file_attributes, &
                         file_stat_type, file_executable_p
    use files_fm, only: find_file_noselect, delete_directory
    use sysdep_fm, only: emacs_localtime
    use dired_mode_fm, only: dired, dired_goto_file, dired_buffers_for_dir, dired_get_marked_files, dired_string_type, &
                             dired_deletion_confirmer, dired_auto_revert_buffer, dired_mark_files_regexp, &
                             dired_copy_file_recursive, dired_rename_file, dired_do_create_files, dired_copy_file, &
                             dired_create_empty_file, dired_unmark_all_files, dired_create_destination_dirs, &
                             DIRED_CREATE_DESTINATION_DIRS_NIL, DIRED_CREATE_DESTINATION_DIRS_ALWAYS, &
                             DIRED_CREATE_DESTINATION_DIRS_ASK, DIRED_CREATOR_COPY, DIRED_ARG_NIL, DIRED_LOCALP_NO_DIR, &
                             dired_file_creator_proc
    implicit none(type, external)
    private

    public :: collect_dired_aux_fm

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdir = "build/test-dired-aux"

    character, parameter :: CR = achar(13)

contains

    subroutine collect_dired_aux_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("dired-test-bug28834", test_bug28834), &
                    new_unittest("dired-test-bug30624", test_bug30624), &
                    new_unittest("dired-do-copy-one-file", test_copy_one), &
                    new_unittest("dired-do-copy-marked-into-directory", test_copy_marked), &
                    new_unittest("dired-do-copy-overwrite-query", test_copy_overwrite), &
                    new_unittest("dired-do-rename-visited-buffer", test_rename), &
                    new_unittest("dired-do-links", test_links), &
                    new_unittest("dired-do-chmod", test_chmod), &
                    new_unittest("dired-do-touch", test_touch), &
                    new_unittest("dired-do-kill-lines", test_kill_lines), &
                    new_unittest("dired-upcase", test_upcase), &
                    new_unittest("dired-do-rename-regexp", test_rename_regexp), &
                    new_unittest("dired-create-empty-file", test_create_empty_file), &
                    new_unittest("dired-do-compress", test_compress), &
                    new_unittest("dired-view-file", test_view_file), &
                    new_unittest("dired-unmark-all-files-query", test_unmark_query) &
                    ]
    end subroutine collect_dired_aux_fm

    !> The absolute name of the test directory, as a directory.
    function tdir() result(dir)
        character(len=:), allocatable :: dir
        dir = file_name_as_directory(file_name_as_directory(emacs_wd)//subdir)
    end function tdir

    !> A fresh test directory with a.txt, b.txt and sub/; *scratch*
    !> current and shown alone.
    subroutine setup()
        type(handle_type) :: buf
        type(status_type) :: st
        call init_bindings()
        call init_fileio()
        call discard_input()
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        buffers(cur)%directory = file_name_as_directory(emacs_wd)
        call show_alone()
        call kill_dired_buffers()
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
        call make_directory_internal(tdir(), st)
        call make_file("a.txt", "alpha")
        call make_file("b.txt", "beta")
        call make_directory_internal(tdir()//"sub", st)
        dired_auto_revert_buffer = .false.
        dired_create_destination_dirs = DIRED_CREATE_DESTINATION_DIRS_NIL
    end subroutine setup

    !> Remove the test directory and the buffers showing it.
    subroutine teardown()
        type(status_type) :: st
        type(handle_type) :: buf
        call discard_input()
        call kill_dired_buffers()
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        call show_alone()
        if (file_directory_p(tdir())) call delete_directory(tdir(), st, .true.)
        dired_deletion_confirmer => null()
        dired_create_destination_dirs = DIRED_CREATE_DESTINATION_DIRS_NIL
    end subroutine teardown

    !> Kill every Dired buffer of the test directory and its subdirectories.
    subroutine kill_dired_buffers()
        type(handle_type), allocatable :: bufs(:)
        type(status_type) :: st
        integer :: k
        bufs = dired_buffers_for_dir(tdir())
        do k = 1, size(bufs)
            call kill_buffer(bufs(k), st)
        end do
        bufs = dired_buffers_for_dir(tdir()//"sub/")
        do k = 1, size(bufs)
            call kill_buffer(bufs(k), st)
        end do
    end subroutine kill_dired_buffers

    !> Create the file NAME (relative to the test directory) holding TEXT.
    subroutine make_file(name, text)
        character(len=*), intent(in) :: name, text
        type(handle_type) :: buf, old
        type(status_type) :: st
        old = current_buffer()
        call get_buffer_create(" *dired-aux-test-file*", buf, st)
        call set_buffer(buf, st)
        call erase_buffer(st)
        call insert(text, st)
        call write_region(tdir()//name, .false., st)
        call kill_buffer(buf, st)
        call set_buffer(old, st)
    end subroutine make_file

    !> The contents of file NAME of the test directory.
    function contents(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        type(handle_type) :: buf, old
        type(status_type) :: st
        old = current_buffer()
        call find_file_noselect(tdir()//name, buf, st)
        s = "<none>"
        if (st%ok()) then
            call set_buffer(buf, st)
            s = buffer_string()
            call set_buffer(old, st)
            call kill_buffer(buf, st)
        end if
        call set_buffer(old, st)
    end function contents

    !> Show the Dired buffer of the test directory, point on NAME.
    subroutine dired_on(name)
        character(len=*), intent(in) :: name
        type(status_type) :: st
        integer(int64) :: p
        call dired(tdir(), st)
        call show_alone()
        p = dired_goto_file(tdir()//name, st)
    end subroutine dired_on

    !> Run the command loop on KEYS.
    subroutine keys(text)
        character(len=*), intent(in) :: text
        type(status_type) :: st
        call kbd_store_bytes(text)
        call command_loop(st)
    end subroutine keys

    !> The current line's text.
    function this_line() result(s)
        character(len=:), allocatable :: s
        type(status_type) :: st
        call buffer_substring(line_beginning_position(), line_end_position(), s, st)
    end function this_line

    !> The text of the line of FILE in the current Dired buffer ("" if none).
    function line_of(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        type(status_type) :: st
        integer(int64) :: opoint
        opoint = point()
        s = ""
        if (dired_goto_file(tdir()//name, st) /= 0) s = this_line()
        call goto_char(opoint)
    end function line_of

    !> Port of dired-test-bug28834.
    subroutine test_bug28834(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: from, to_cp, to_mv
        call setup()
        body: block
            from = tdir()//"a.txt"
            to_cp = tdir()//"bar/foo-cp"
            to_mv = tdir()//"qux/foo-mv"
            ! `dired_create_destination_dirs' set to always.
            dired_create_destination_dirs = DIRED_CREATE_DESTINATION_DIRS_ALWAYS
            call dired_copy_file_recursive(from, to_cp, .false., st)
            call check(error, st%ok() .and. file_exists_p(to_cp), "always: copy "//format_error(st))
            if (allocated(error)) exit body
            call dired_rename_file(from, to_mv, .false., st)
            call check(error, st%ok() .and. file_exists_p(to_mv), "always: rename "//format_error(st))
            if (allocated(error)) exit body
            ! `dired_create_destination_dirs' set to nil.
            call reset()
            dired_create_destination_dirs = DIRED_CREATE_DESTINATION_DIRS_NIL
            call dired_copy_file_recursive(from, to_cp, .false., st)
            call check(error,.not. st%ok(), "nil: copy fails")
            if (allocated(error)) exit body
            call dired_rename_file(from, to_mv, .false., st)
            call check(error,.not. st%ok(), "nil: rename fails")
            if (allocated(error)) exit body
            ! `dired_create_destination_dirs' set to ask: answer yes.
            call reset()
            dired_create_destination_dirs = DIRED_CREATE_DESTINATION_DIRS_ASK
            call kbd_store_bytes("yes"//CR)
            call dired_copy_file_recursive(from, to_cp, .false., st)
            call check(error, st%ok() .and. file_exists_p(to_cp), "ask yes: copy "//format_error(st))
            if (allocated(error)) exit body
            call kbd_store_bytes("yes"//CR)
            call dired_rename_file(from, to_mv, .false., st)
            call check(error, st%ok() .and. file_exists_p(to_mv), "ask yes: rename "//format_error(st))
            if (allocated(error)) exit body
            ! Answer no.
            call reset()
            call kbd_store_bytes("no"//CR)
            call dired_copy_file_recursive(from, to_cp, .false., st)
            call check(error,.not. st%ok(), "ask no: copy fails")
            if (allocated(error)) exit body
            call kbd_store_bytes("no"//CR)
            call dired_rename_file(from, to_mv, .false., st)
            call check(error,.not. st%ok(), "ask no: rename fails")
        end block body
        call teardown()
    contains

        subroutine reset()
            type(status_type) :: s
            if (file_directory_p(tdir()//"bar")) call delete_directory(tdir()//"bar", s, .true.)
            if (file_directory_p(tdir()//"qux")) call delete_directory(tdir()//"qux", s, .true.)
            if (.not. file_exists_p(from)) call make_file("a.txt", "alpha")
        end subroutine reset
    end subroutine test_bug28834

    !> Port of dired-test-bug30624: the target directory is created by
    !> `dired_do_create_files' when `dired_create_destination_dirs' is
    !> always.
    subroutine test_bug30624(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        procedure(dired_file_creator_proc), pointer :: creator
        call setup()
        body: block
            call make_file("bug30624_file1", "1")
            call make_file("bug30624_file2", "2")
            dired_create_destination_dirs = DIRED_CREATE_DESTINATION_DIRS_ALWAYS
            call dired(tdir(), st)
            call show_alone()
            call dired_mark_files_regexp("bug30624_file", st)
            ! (dired-mark-read-file-name gives the deleted target directory)
            call kbd_store_bytes(achar(1)//achar(11)//tdir()//"target-dir"//CR)
            creator => dired_copy_file
            call dired_do_create_files("copy", creator, DIRED_CREATOR_COPY, "Copy", DIRED_ARG_NIL, 0_int64, "C", st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call check(error, file_exists_p(tdir()//"target-dir/bug30624_file1") .and. &
                       file_exists_p(tdir()//"target-dir/bug30624_file2"), "copied into the new directory")
        end block body
        call teardown()
    end subroutine test_bug30624

    !> C on one file copies it to the name read, marked C in the listing.
    subroutine test_copy_one(error)
        type(error_type), allocatable, intent(out) :: error
        call setup()
        body: block
            call dired_on("a.txt")
            call keys("Ccopy.txt"//CR)
            call check(error, file_exists_p(tdir()//"copy.txt"), last_message)
            if (allocated(error)) exit body
            call check(error, contents("copy.txt"), "alpha")
            if (allocated(error)) exit body
            call check(error, index(line_of("copy.txt"), "C") == 1, line_of("copy.txt"))
            if (allocated(error)) exit body
            call check(error, last_message, "Copy: 1 file done")
        end block body
        call teardown()
    end subroutine test_copy_one

    !> C with marked files copies them into the directory read.
    subroutine test_copy_marked(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call setup()
        body: block
            call dired_on("a.txt")
            call dired_mark_files_regexp("\.txt\'", st)
            call keys("Csub"//CR)
            call check(error, file_exists_p(tdir()//"sub/a.txt") .and. file_exists_p(tdir()//"sub/b.txt"), last_message)
            if (allocated(error)) exit body
            call check(error, last_message, "Copy: 2 files done")
        end block body
        call teardown()
    end subroutine test_copy_marked

    !> Copying onto an existing file asks first: n skips it, y overwrites.
    subroutine test_copy_overwrite(error)
        type(error_type), allocatable, intent(out) :: error
        call setup()
        body: block
            call dired_on("a.txt")
            call keys("Cb.txt"//CR//"n")
            call check(error, index(last_message, "Copy: "//achar(226)) == 1, last_message)
            if (allocated(error)) exit body
            call check(error, contents("b.txt"), "beta")
            if (allocated(error)) exit body
            call dired_on("a.txt")
            call keys("Cb.txt"//CR//"y")
            call check(error, contents("b.txt"), "alpha")
            if (allocated(error)) exit body
            call check(error, last_message, "Copy: 1 file done")
        end block body
        call teardown()
    end subroutine test_copy_overwrite

    !> R renames the file, its listing line and the buffer visiting it.
    subroutine test_rename(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: fbuf
        type(status_type) :: st
        integer(int64) :: b
        call setup()
        body: block
            call find_file_noselect(tdir()//"a.txt", fbuf, st)
            call dired_on("a.txt")
            call keys("Rrenamed.txt"//CR)
            call check(error, file_exists_p(tdir()//"renamed.txt") .and. .not. file_exists_p(tdir()//"a.txt"), &
                       last_message)
            if (allocated(error)) exit body
            call check(error, len(line_of("a.txt")) == 0 .and. len(line_of("renamed.txt")) > 0, "listing updated")
            if (allocated(error)) exit body
            call resolve_buffer(fbuf, b, st)
            call check(error, buffers(b)%filename, tdir()//"renamed.txt")
            if (allocated(error)) exit body
            call check(error, last_message, "Move: 1 file done")
            call kill_buffer(fbuf, st)
        end block body
        call teardown()
    end subroutine test_rename

    !> H, S and Y make hard, symbolic and relative symbolic links.
    subroutine test_links(error)
        type(error_type), allocatable, intent(out) :: error
        type(file_stat_type) :: st_a
        logical :: found
        call setup()
        body: block
            call dired_on("a.txt")
            call keys("Hhard.txt"//CR)
            call file_attributes(tdir()//"a.txt", st_a, found)
            call check(error, file_exists_p(tdir()//"hard.txt") .and. st_a%nlink == 2, last_message)
            if (allocated(error)) exit body
            call dired_on("a.txt")
            call keys("Ssym.txt"//CR)
            call check(error, file_symlink_target(tdir()//"sym.txt"), tdir()//"a.txt")
            if (allocated(error)) exit body
            call dired_on("a.txt")
            call keys("Ysub/rel.txt"//CR)
            call check(error, file_symlink_target(tdir()//"sub/rel.txt"), "../a.txt")
        end block body
        call teardown()
    end subroutine test_links

    !> M takes octal and symbolic modes.
    subroutine test_chmod(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call setup()
        body: block
            call set_file_modes(tdir()//"a.txt", int(o'644', int32), st)
            call dired_on("a.txt")
            call keys("M600"//CR)
            call check(error, file_modes(tdir()//"a.txt") == int(o'600', int32), last_message)
            if (allocated(error)) exit body
            call keys("Mgo+r,u+x"//CR)
            call check(error, file_modes(tdir()//"a.txt") == int(o'744', int32), last_message)
            if (allocated(error)) exit body
            ! RET with the default (the file's modes) is no mode.
            call keys("M"//CR)
            call check(error, last_message, "No file mode specified")
        end block body
        call teardown()
    end subroutine test_chmod

    !> T sets the time with touch -t.
    subroutine test_touch(error)
        type(error_type), allocatable, intent(out) :: error
        type(file_stat_type) :: attrs
        integer :: parts(6)
        logical :: found
        call setup()
        body: block
            call dired_on("a.txt")
            call keys("T202001020304.05"//CR)
            call file_attributes(tdir()//"a.txt", attrs, found)
            parts = emacs_localtime(attrs%mtime_sec)
            call check(error, found .and. all(parts == [2020, 1, 2, 3, 4, 5]), last_message)
        end block body
        call teardown()
    end subroutine test_touch

    !> k removes the marked lines, not the files.
    subroutine test_kill_lines(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call setup()
        body: block
            call dired_on("a.txt")
            call dired_mark_files_regexp("\.txt\'", st)
            call keys("k")
            call check(error, last_message, "Killed 2 lines.")
            if (allocated(error)) exit body
            call check(error, len(line_of("a.txt")) == 0 .and. file_exists_p(tdir()//"a.txt"), "line gone, file kept")
        end block body
        call teardown()
    end subroutine test_kill_lines

    !> % u renames to upper case after a query.
    subroutine test_upcase(error)
        type(error_type), allocatable, intent(out) :: error
        call setup()
        body: block
            call dired_on("a.txt")
            call keys("%uy")
            call check(error, file_exists_p(tdir()//"A.TXT") .and. .not. file_exists_p(tdir()//"a.txt"), last_message)
            if (allocated(error)) exit body
            call check(error, last_message, "Rename upcase: 1 file done")
        end block body
        call teardown()
    end subroutine test_upcase

    !> % R with ! renames every marked file without more questions.
    subroutine test_rename_regexp(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call setup()
        body: block
            call dired_on("a.txt")
            call dired_mark_files_regexp("\.txt\'", st)
            call keys("%R\.txt"//CR//".text"//CR//"!")
            call check(error, file_exists_p(tdir()//"a.text") .and. file_exists_p(tdir()//"b.text"), last_message)
            if (allocated(error)) exit body
            call check(error, last_message, "Rename: 2 files done")
        end block body
        call teardown()
    end subroutine test_rename_regexp

    !> `dired_create_empty_file' makes the parents and lists the topmost.
    subroutine test_create_empty_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call setup()
        body: block
            call dired_on("a.txt")
            call dired_create_empty_file(tdir()//"new/deep/empty", st)
            call check(error, st%ok() .and. file_exists_p(tdir()//"new/deep/empty"), format_error(st))
            if (allocated(error)) exit body
            call check(error, len(line_of("new")) > 0, "new listed")
            if (allocated(error)) exit body
            call dired_create_empty_file(tdir()//"a.txt", st)
            call check(error, format_error(st), "Cannot create file "//tdir()//"a.txt: file exists")
        end block body
        call teardown()
    end subroutine test_create_empty_file

    !> Z compresses with gzip and uncompresses again.
    subroutine test_compress(error)
        type(error_type), allocatable, intent(out) :: error
        if (.not. (file_executable_p("/bin/gzip") .or. file_executable_p("/usr/bin/gzip"))) then
            call skip_test(error, "no gzip")
            return
        end if
        call setup()
        body: block
            call dired_on("a.txt")
            call keys("Zy")
            call check(error, file_exists_p(tdir()//"a.txt.gz") .and. .not. file_exists_p(tdir()//"a.txt"), &
                       last_message)
            if (allocated(error)) exit body
            call check(error, last_message, "Compress or uncompress: 1 file.")
            if (allocated(error)) exit body
            call check(error, len(line_of("a.txt.gz")) > 0, "listed")
            if (allocated(error)) exit body
            call keys("Zy")
            call check(error, file_exists_p(tdir()//"a.txt") .and. .not. file_exists_p(tdir()//"a.txt.gz"), &
                       last_message)
            if (allocated(error)) exit body
            call check(error, contents("a.txt"), "alpha")
        end block body
        call teardown()
    end subroutine test_compress

    !> v views the file; q returns to Dired and kills the file's buffer.
    subroutine test_view_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: dbuf, vbuf
        call setup()
        body: block
            call dired_on("a.txt")
            dbuf = current_buffer()
            call keys("v")
            vbuf = get_file_buffer(tdir()//"a.txt")
            call check(error, vbuf%index /= 0 .and. buffers(cur)%read_only, "viewing a.txt")
            if (allocated(error)) exit body
            call keys("q")
            call check(error, cur == dbuf%index .and. .not. buffer_live_p(vbuf), "back in Dired")
        end block body
        call teardown()
    end subroutine test_view_file

    !> M-DEL with a prefix argument asks for each file.
    subroutine test_unmark_query(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(dired_string_type), allocatable :: files(:)
        call setup()
        body: block
            call dired_on("a.txt")
            call dired_mark_files_regexp("\.txt\'", st)
            ! Keep a.txt marked, unmark b.txt.
            call kbd_store_bytes("ny")
            call dired_unmark_all_files(CR, st, .true.)
            call check(error, st%ok(), "status: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, last_message, "1 mark removed")
            if (allocated(error)) exit body
            call dired_get_marked_files(files, st, DIRED_LOCALP_NO_DIR)
            call check(error, size(files) == 1, "one marked: "//buffer_string())
            if (allocated(error)) exit body
            call check(error, files(1)%s, "a.txt")
        end block body
        call teardown()
    end subroutine test_unmark_query
end module test_dired_aux_fm
