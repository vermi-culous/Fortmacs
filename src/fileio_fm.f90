! fileio_fm.f90 -- file names and file input/output.
! Ported from GNU Emacs 31.1 src/fileio.c (the Unix parts in use).
!
! Copyright (C) 1985-1988, 1993-2026 Free Software Foundation, Inc.
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

!> File name manipulation (`expand_file_name' and friends), file
!> predicates, and reading and writing buffer text.
!>
!> File names are strings of bytes, as they are on Linux; Fortmacs does no
!> file-name encoding (`file_name_coding_system' is effectively utf-8).
!> Where upstream returns nil for "no directory", the functions here
!> return "" (a directory name is never empty).  There are no file name
!> handlers (TRAMP and friends).
!>
!> Text is read as UTF-8 with invalid bytes kept as raw-byte characters
!> and written back the same way, so any file survives a visit and save
!> unchanged.  End-of-line conversion and other coding systems are not
!> ported yet.
module fileio_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_FILE_ERROR, FM_FILE_MISSING, FM_FILE_ALREADY_EXISTS, &
                        FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT, FM_PERMISSION_DENIED, FM_FILE_DATE_ERROR
    use buffer_fm, only: buffers, cur, BEG, BEG_BYTE, buf_bytes, barf_if_buffer_read_only, vdeactivate_mark
    use character_fm, only: str_as_multibyte, str_as_unibyte
    use marker_fm, only: char_to_byte
    use insdel_fm, only: insert, inhibit_modification_hooks
    use editfns_fm, only: point, goto_char, validate_region
    use env_fm, only: substitute_env_vars
    use sysdep_fm, only: file_stat_type, emacs_open_read, emacs_creat, sys_access, sys_rename, &
                         sys_chmod, sys_stat, sys_mkdir, sys_rmdir, sys_unlink, emacs_get_current_dir_name, passwd_home_by_name, &
                         passwd_home_of_user, emacs_strerror, emacs_read, emacs_full_write, &
                         close_fd, emacs_geteuid, emacs_getegid, ENOENT, EISDIR, EEXIST, EACCES, F_OK, R_OK, &
                         W_OK, X_OK, S_IFMT, S_IFDIR, S_IFREG, S_IFLNK, sys_symlink, EEXIST, emacs_statvfs, &
                         ENOTDIR, emacs_open_mode, sys_link, sys_rename_noreplace, sys_set_file_times, sys_futimens, &
                         emacs_umask, sys_fchmod, sys_chmod_at, sys_fchown, emacs_readlink, O_WRONLY, O_CREAT, O_EXCL, &
                         O_TRUNC, UTIME_NOW, EXDEV, EINVAL, ENOSYS, ENOTSUP
    implicit none(type, external)
    private

    public :: add_name_to_file, set_file_times, default_file_modes, file_symlink_target
    public :: copy_directory_hook, delete_directory_hook, delete_file_hook, file_operation_proc, &
              copy_directory_proc

    abstract interface
        !> files.el's `delete_file' (no trash) or `delete_directory' with
        !> RECURSIVE true, on FILE.
        recursive subroutine file_operation_proc(file, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
        end subroutine file_operation_proc

        !> files.el's `copy_directory' with KEEP_TIME true and PARENTS false.
        recursive subroutine copy_directory_proc(directory, newname, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: directory, newname
            type(status_type), intent(out) :: status
        end subroutine copy_directory_proc
    end interface

    !> What `rename_file' calls (through Lisp upstream) to move a
    !> directory, or delete a file or a directory, across file systems;
    !> set by files_fm.
    procedure(copy_directory_proc), pointer :: copy_directory_hook => null()
    procedure(file_operation_proc), pointer :: delete_directory_hook => null()
    procedure(file_operation_proc), pointer :: delete_file_hook => null()

    !> fileio.c's realmask: the file mode creation mask (-1 until read).
    integer(int32) :: realmask = -1

    public :: file_stat_type
    public :: file_name_directory, file_name_nondirectory, file_name_as_directory, &
              directory_file_name, directory_name_p, file_name_absolute_p, expand_file_name, &
              get_homedir, init_fileio, emacs_wd, substitute_in_file_name
    public :: file_exists_p, file_readable_p, file_writable_p, file_directory_p, &
              file_accessible_directory_p, file_attributes, file_modes, set_file_modes, &
              file_executable_p, file_newer_than_file_p, &
              make_symbolic_link, &
              rename_file, copy_file, report_file_errno, file_regular_p, file_symlink_p, &
              user_uid, group_gid, make_directory_internal, delete_directory_internal, &
              delete_file_internal, file_system_info
    public :: insert_file_contents, write_region, verify_visited_file_modtime, &
              set_visited_file_modtime

    !> emacs.c:emacs_wd, the directory Emacs started in ("" if unknown).
    character(len=:), allocatable, protected :: emacs_wd

    !> NONEXISTENT_MODTIME_NSECS: the modtime of a file that doesn't exist.
    integer(int32), parameter :: NONEXISTENT_MODTIME_NSECS = -1

    !> Bytes per read(2) in insert_file_contents (READ_BUF_SIZE).
    integer, parameter :: READ_BUF_SIZE = 65536

contains

    ! ------------------------------------------------------------------
    ! File names
    ! ------------------------------------------------------------------

    !> Record the starting directory (emacs.c:main sets emacs_wd).
    subroutine init_fileio()
        emacs_wd = emacs_get_current_dir_name()
        realmask = emacs_umask()
    end subroutine init_fileio

    !> The directory part of `filename', ending in a slash, or "" if it has
    !> none.  Port of fileio.c:file_name_directory.
    pure function file_name_directory(filename) result(dir)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dir
        dir = filename(1:index(filename, "/", back=.true.))
    end function file_name_directory

    !> `filename' without its directory part.
    !> Port of fileio.c:Ffile_name_nondirectory.
    pure function file_name_nondirectory(filename) result(name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: name
        name = filename(index(filename, "/", back=.true.) + 1:)
    end function file_name_nondirectory

    !> `file' as a directory name: with a trailing slash ("./" for "").
    !> Port of fileio.c:file_name_as_directory.
    pure function file_name_as_directory(file) result(dir)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: dir
        if (len(file) == 0) then
            dir = "./"
        else if (file(len(file):len(file)) == "/") then
            dir = file
        else
            dir = file//"/"
        end if
    end function file_name_as_directory

    !> The file name of directory `directory': without trailing slashes,
    !> except that "/" and "//" stay and "///..." becomes "/".
    !> Port of fileio.c:directory_file_name.
    pure function directory_file_name(directory) result(file)
        character(len=*), intent(in) :: directory
        character(len=:), allocatable :: file
        integer :: n
        n = len(directory)
        if (.not. (n == 2 .and. directory(1:1) == "/")) then
            do while (n > 1)
                if (directory(n:n) /= "/") exit
                n = n - 1
            end do
        end if
        file = directory(1:n)
    end function directory_file_name

    !> Whether `name' ends in a slash.  Port of fileio.c:Fdirectory_name_p.
    pure logical function directory_name_p(name)
        character(len=*), intent(in) :: name
        directory_name_p = .false.
        if (len(name) > 0) directory_name_p = name(len(name):len(name)) == "/"
    end function directory_name_p

    !> Whether `filename' is absolute: it starts with "/", "~", "~/" or
    !> "~USER" for an existing USER.  Port of fileio.c:file_name_absolute_p.
    logical function file_name_absolute_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: home
        file_name_absolute_p = .false.
        if (len(filename) == 0) return
        if (filename(1:1) == "/") then
            file_name_absolute_p = .true.
        else if (filename(1:1) == "~") then
            if (len(filename) == 1) then
                file_name_absolute_p = .true.
            else if (filename(2:2) == "/") then
                file_name_absolute_p = .true.
            else
                file_name_absolute_p = user_homedir(filename(2:), home)
            end if
        end if
    end function file_name_absolute_p

    !> Home directory of the user named at the start of `name' (up to a
    !> slash); false if there is no such user or the home directory is
    !> not absolute.  Port of fileio.c:user_homedir.
    logical function user_homedir(name, home)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: home
        integer :: length
        logical :: found
        user_homedir = .false.
        home = ""
        length = index(name, "/") - 1
        if (length < 0) length = len(name)
        if (length == 0) return
        call passwd_home_by_name(name(1:length), home, found)
        if (.not. found) return
        if (len(home) == 0) return
        user_homedir = home(1:1) == "/"
    end function user_homedir

    !> The user's home directory: $HOME, or the password entry of $LOGNAME,
    !> $USER or the real user; a relative $HOME is taken relative to the
    !> starting directory.  Port of fileio.c:get_homedir.
    function get_homedir() result(home)
        character(len=:), allocatable :: home
        character(len=*), parameter :: userenv(2) = [character(len=7) :: "LOGNAME", "USER"]
        character(len=:), allocatable :: user
        integer :: length, stat, i
        logical :: found
        call get_environment_variable("HOME", length=length, status=stat)
        if (stat == 0) then
            allocate (character(len=length) :: home)
            if (length > 0) call get_environment_variable("HOME", home)
        else
            found = .false.
            do i = 1, size(userenv)
                call get_environment_variable(trim(userenv(i)), length=length, status=stat)
                if (stat /= 0) cycle
                if (allocated(user)) deallocate (user)
                allocate (character(len=length) :: user)
                if (length > 0) call get_environment_variable(trim(userenv(i)), user)
                call passwd_home_by_name(user, home, found)
                if (found) exit
            end do
            if (.not. found) call passwd_home_of_user(home, found)
            if (.not. found) home = ""
            if (len(home) == 0) return
        end if
        if (len(home) > 0) then
            if (home(1:1) == "/") return
        end if
        if (.not. allocated(emacs_wd)) call init_fileio()
        ! splice_dir_file
        home = file_name_as_directory(emacs_wd)//home
    end function get_homedir

    !> Whether `s' holds a NUL byte (CHECK_STRING_NULL_BYTES).
    pure logical function has_null_byte(s)
        character(len=*), intent(in) :: s
        has_null_byte = index(s, achar(0)) > 0
    end function has_null_byte

    !> `name' made absolute and canonical: relative names are taken
    !> relative to `default_directory' (or the current buffer's
    !> `default_directory', or "/"), "~" and "~USER" become home
    !> directories, and "." and "FOO/.." components and repeated slashes
    !> are removed (without looking at the file system).
    !> Port of fileio.c:Fexpand_file_name (Unix; no file name handlers).
    recursive subroutine expand_file_name(name, expanded, status, default_directory)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: expanded
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: default_directory
        character(len=:), allocatable :: defdir, nm, newdir, target, tmp, home
        logical :: have_defdir, have_newdir, lose
        integer :: p, length, ulen
        if (has_null_byte(name)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "filenamep")
            return
        end if
        ! The default directory: the argument, else the buffer's.
        have_defdir = present(default_directory)
        if (have_defdir) then
            defdir = default_directory
        else if (cur /= 0) then
            if (allocated(buffers(cur)%directory)) then
                if (has_null_byte(buffers(cur)%directory)) then
                    call set_error(status, FM_WRONG_TYPE_ARGUMENT, "filenamep")
                    return
                end if
                have_defdir = .true.
                if (index(buffers(cur)%directory, "/") == 1) then
                    defdir = buffers(cur)%directory
                else
                    ! Relative to the invocation directory (unknown here),
                    ! so to the root, as upstream when that isn't absolute.
                    call expand_file_name(buffers(cur)%directory, defdir, status, "/")
                    if (.not. status%ok()) return
                end if
            end if
        end if
        if (.not. have_defdir) defdir = "/"
        if (has_null_byte(defdir)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "filenamep")
            return
        end if
        ! Make sure the default directory is itself absolute.
        if (len(defdir) == 0) then
            call expand_file_name(defdir, tmp, status)
            if (.not. status%ok()) return
            defdir = tmp
        else if (defdir(1:1) /= "/" .and. defdir /= name) then
            call expand_file_name(defdir, tmp, status)
            if (.not. status%ok()) return
            defdir = tmp
        end if
        nm = name
        ! An absolute name with nothing to simplify is returned as is.
        if (len(nm) > 0) then
            if (nm(1:1) == "/") then
                lose = .false.
                do p = 1, len(nm)
                    if (nm(p:p) /= "/") cycle
                    if (at(nm, p + 1) == "." .and. &
                        (at(nm, p + 2) == "/" .or. at(nm, p + 2) == "" .or. &
                         (at(nm, p + 2) == "." .and. (at(nm, p + 3) == "/" .or. at(nm, p + 3) == "")))) then
                        lose = .true.
                    else if (at(nm, p + 1) == "/" .and. (p /= 1 .or. at(nm, p + 2) == "/")) then
                        lose = .true.
                    end if
                    if (lose) exit
                end do
                if (.not. lose) then
                    expanded = nm
                    return
                end if
            end if
        end if
        ! The prefix: a home directory for ~ or ~USER, else the default
        ! directory for a relative name.
        have_newdir = .false.
        if (at(nm, 1) == "~") then
            if (at(nm, 2) == "/" .or. at(nm, 2) == "") then
                newdir = get_homedir()
                have_newdir = .true.
                nm = nm(2:)
            else if (user_homedir(nm(2:), home)) then
                newdir = home
                have_newdir = .true.
                ulen = index(nm(2:), "/") - 1
                if (ulen < 0) ulen = len(nm) - 1
                nm = nm(2 + ulen:)
            end if
            ! An unknown user leaves the name unchanged.
        end if
        if (.not. have_newdir .and. at(nm, 1) /= "/") then
            newdir = defdir
            have_newdir = .true.
        end if
        target = ""
        if (have_newdir) then
            ! Ignore a trailing slash of newdir, unless it is "/" or "//".
            length = len(newdir)
            do while (length > 1)
                if (newdir(length:length) /= "/") exit
                if (length == 2 .and. newdir(1:1) == "/") exit
                length = length - 1
            end do
            if (len(nm) == 0) then
                target = newdir(1:length)
            else if (nm(1:1) == "/") then
                target = newdir(1:length)
            else
                target = file_name_as_directory(newdir(1:length))
            end if
        end if
        target = target//nm
        expanded = canonicalize(target)
    end subroutine expand_file_name

    !> The byte of `s' at `i', or "" past the end (C's NUL terminator).
    pure function at(s, i) result(c)
        character(len=*), intent(in) :: s
        integer, intent(in) :: i
        character(len=:), allocatable :: c
        if (i >= 1 .and. i <= len(s)) then
            c = s(i:i)
        else
            c = ""
        end if
    end function at

    !> Remove "//", "/." and "/FOO/.." from `target': the last step of
    !> Fexpand_file_name.
    pure function canonicalize(target) result(out)
        character(len=*), intent(in) :: target
        character(len=:), allocatable :: out
        character(len=len(target)) :: buf
        integer :: p, o, n
        n = len(target)
        p = 1
        o = 0
        do while (p <= n)
            if (target(p:p) /= "/") then
                o = o + 1
                buf(o:o) = target(p:p)
                p = p + 1
            else if (at(target, p + 1) == "." .and. (at(target, p + 2) == "/" .or. at(target, p + 2) == "")) then
                ! "/." alone stays "/"; otherwise "/." goes.
                if (o == 0 .and. p + 2 > n) then
                    o = 1
                    buf(1:1) = "/"
                end if
                p = p + 2
            else if (at(target, p + 1) == "." .and. at(target, p + 2) == "." .and. o /= 0 .and. &
                     (at(target, p + 3) == "/" .or. at(target, p + 3) == "")) then
                ! Back up over the previous component.
                do while (o > 0)
                    o = o - 1
                    if (buf(o + 1:o + 1) == "/") exit
                end do
                ! Keep the initial slash only if this is the whole name.
                if (o == 0 .and. buf(1:1) == "/" .and. p + 3 > n) o = 1
                p = p + 3
            else if (at(target, p + 1) == "/" .and. (p /= 1 .or. at(target, p + 2) == "/")) then
                ! Collapse repeated slashes, except a leading "//".
                p = p + 1
            else
                o = o + 1
                buf(o:o) = target(p:p)
                p = p + 1
            end if
        end do
        out = buf(1:o)
    end function canonicalize

    !> Where an absolute file name ("//...", "/~...", "/~user...") starts
    !> after a slash inside `nm', or 0.  Port of
    !> fileio.c:search_embedded_absfilename.
    integer function search_embedded_absfilename(nm) result(p)
        character(len=*), intent(in) :: nm
        do p = 2, len(nm)
            if (nm(p - 1:p - 1) == "/" .and. file_name_absolute_p(nm(p:))) return
        end do
        p = 0
    end function search_embedded_absfilename

    !> `filename' with environment variables substituted ($FOO, ${FOO},
    !> $$; unset ones stay), and everything before an embedded "//" or
    !> "/~" dropped, so that typing "~/x" after a default directory in the
    !> minibuffer means "~/x".  Port of fileio.c:Fsubstitute_in_file_name
    !> with env.el:substitute-env-in-file-name.
    recursive function substitute_in_file_name(filename) result(name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: name
        integer :: p
        p = search_embedded_absfilename(filename)
        if (p > 0) then
            name = substitute_in_file_name(filename(p:))
            return
        end if
        name = substitute_env_vars(filename, .true.)
        if (name == filename .and. len(name) == len(filename)) return
        do
            p = search_embedded_absfilename(name)
            if (p == 0) exit
            name = name(p:)
        end do
    end function substitute_in_file_name

    ! ------------------------------------------------------------------
    ! File predicates and attributes
    ! ------------------------------------------------------------------

    !> Expand `filename', quietly giving "" on error (for predicates).
    function expand_quietly(filename) result(absname)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        type(status_type) :: status
        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) absname = ""
    end function expand_quietly

    !> Whether `filename' exists.  Port of fileio.c:Ffile_exists_p.
    logical function file_exists_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        absname = expand_quietly(filename)
        file_exists_p = len(absname) > 0
        if (file_exists_p) file_exists_p = sys_access(absname, F_OK) == 0
    end function file_exists_p

    !> Return true if FILENAME can be executed by you.
    !> For a directory, this means you can access files in that directory.
    !> (It is generally better to use `file_accessible_directory_p' for that
    !> purpose, though.)  Port of fileio.c:Ffile_executable_p.
    logical function file_executable_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        absname = expand_quietly(filename)
        file_executable_p = len(absname) > 0
        if (file_executable_p) file_executable_p = sys_access(absname, X_OK) == 0
    end function file_executable_p

    !> Return true if file FILE1 is newer than file FILE2.
    !> If FILE1 does not exist, the return value is false;
    !> if FILE2 does not exist, the return value is true.
    !> For existing files, this compares their last-modified times.
    !> Port of fileio.c:Ffile_newer_than_file_p.
    logical function file_newer_than_file_p(file1, file2)
        character(len=*), intent(in) :: file1, file2
        type(file_stat_type) :: st1, st2
        file_newer_than_file_p = .false.
        if (sys_stat(expand_quietly(file1), st1) /= 0) return
        file_newer_than_file_p = .true.
        if (sys_stat(expand_quietly(file2), st2) /= 0) return
        file_newer_than_file_p = st1%mtime_sec > st2%mtime_sec .or. &
                                 (st1%mtime_sec == st2%mtime_sec .and. st1%mtime_nsec > st2%mtime_nsec)
    end function file_newer_than_file_p

    !> Whether `filename' exists and can be read.
    !> Port of fileio.c:Ffile_readable_p.
    logical function file_readable_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        absname = expand_quietly(filename)
        file_readable_p = len(absname) > 0
        if (file_readable_p) file_readable_p = sys_access(absname, R_OK) == 0
    end function file_readable_p

    !> Whether `filename' can be written or created.
    !> Port of fileio.c:Ffile_writable_p.
    logical function file_writable_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        integer(int32) :: err
        file_writable_p = .false.
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        err = sys_access(absname, W_OK)
        if (err == 0) then
            file_writable_p = .true.
            return
        end if
        if (err /= ENOENT) return
        file_writable_p = sys_access(file_name_directory(absname), ior(W_OK, X_OK)) == 0
    end function file_writable_p

    !> Whether `filename' names a directory.  Port of
    !> fileio.c:Ffile_directory_p via file_directory_p's portable path
    !> (O_DIRECTORY differs between the two targets).
    logical function file_directory_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        type(file_stat_type) :: st
        file_directory_p = .false.
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        if (file_accessible_directory_p(absname)) then
            file_directory_p = .true.
            return
        end if
        if (sys_stat(absname, st) /= 0) return
        file_directory_p = iand(st%mode, S_IFMT) == S_IFDIR
    end function file_directory_p

    !> Whether `filename' is a directory whose files can be accessed (a
    !> "/." appended to it exists).
    !> Port of fileio.c:file_accessible_directory_p.
    logical function file_accessible_directory_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        file_accessible_directory_p = .false.
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        file_accessible_directory_p = sys_access(file_name_as_directory(absname)//".", F_OK) == 0
    end function file_accessible_directory_p

    !> Status of `filename' (following links); `found' is false if it
    !> can't be read.  The parts of dired.c:Ffile_attributes in use.
    subroutine file_attributes(filename, st, found, nofollow)
        character(len=*), intent(in) :: filename
        type(file_stat_type), intent(out) :: st
        logical, intent(out) :: found
        !> Describe a symbolic link itself, not its target (lstat), as
        !> upstream's file-attributes always does.
        logical, intent(in), optional :: nofollow
        character(len=:), allocatable :: absname
        found = .false.
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        found = sys_stat(absname, st, nofollow=nofollow) == 0
    end subroutine file_attributes

    !> Whether `filename' is a regular file (following links).
    !> Port of fileio.c:Ffile_regular_p.
    logical function file_regular_p(filename)
        character(len=*), intent(in) :: filename
        type(file_stat_type) :: st
        logical :: found
        call file_attributes(filename, st, found)
        file_regular_p = found
        if (found) file_regular_p = iand(st%mode, S_IFMT) == S_IFREG
    end function file_regular_p

    !> Whether `filename' is a symbolic link.  Like
    !> fileio.c:Ffile_symlink_p, but a truth value instead of the target.
    logical function file_symlink_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        type(file_stat_type) :: st
        file_symlink_p = .false.
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        if (sys_stat(absname, st, nofollow=.true.) /= 0) return
        file_symlink_p = iand(st%mode, S_IFMT) == S_IFLNK
    end function file_symlink_p

    !> The effective user id.  Port of editfns.c:Fuser_uid.
    integer(int32) function user_uid()
        user_uid = emacs_geteuid()
    end function user_uid

    !> The effective group id.  Port of editfns.c:Fgroup_gid.
    integer(int32) function group_gid()
        group_gid = emacs_getegid()
    end function group_gid

    !> Return mode bits of file named FILENAME, as an integer.
    !> Return -1 if FILENAME does not exist.  If optional NOFOLLOW is
    !> true, do not follow FILENAME if it is a symbolic link.  Port of
    !> fileio.c:Ffile_modes.
    integer(int32) function file_modes(filename, nofollow)
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: nofollow
        character(len=:), allocatable :: absname
        type(file_stat_type) :: st
        file_modes = -1
        ! expand_and_dir_to_file
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        absname = directory_file_name(absname)
        if (sys_stat(absname, st, nofollow=nofollow) /= 0) return
        file_modes = iand(st%mode, int(o'7777', int32))
    end function file_modes

    !> Set mode bits of file named FILENAME to MODE (an integer).
    !> Only the 12 low bits of MODE are used.  If optional NOFOLLOW is
    !> true, do not follow FILENAME if it is a symbolic link.  Port of
    !> fileio.c:Fset_file_modes (not interactive).
    subroutine set_file_modes(filename, mode, status, nofollow)
        character(len=*), intent(in) :: filename
        integer(int32), intent(in) :: mode
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: nofollow
        character(len=:), allocatable :: absname
        integer(int32) :: err
        logical :: nofollow_flag
        nofollow_flag = .false.
        if (present(nofollow)) nofollow_flag = nofollow
        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        err = sys_chmod_at(absname, iand(mode, int(o'7777', int32)), nofollow_flag)
        if (err /= 0) call report_file_errno("Doing chmod", absname, err, status)
    end subroutine set_file_modes

    !> The file mode creation mask (fileio.c's realmask), read from the
    !> process the first time it is needed.
    integer(int32) function current_realmask()
        if (realmask < 0) realmask = emacs_umask()
        current_realmask = realmask
    end function current_realmask

    !> Return the default file protection for created files.
    !> The value is an integer.  Port of fileio.c:Fdefault_file_modes.
    integer(int32) function default_file_modes()
        default_file_modes = iand(not(current_realmask()), int(o'777', int32))
    end function default_file_modes

    !> Set times of file FILENAME to MTIME_SEC and MTIME_NSEC (seconds
    !> and nanoseconds since the epoch).  If optional NOFOLLOW is true,
    !> do not follow FILENAME if it is a symbolic link.  Set both access
    !> and modification times.  Use the current time if MTIME_SEC is
    !> absent.  Port of fileio.c:Fset_file_times.
    subroutine set_file_times(filename, status, mtime_sec, mtime_nsec, nofollow)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: mtime_sec
        integer(int32), intent(in), optional :: mtime_nsec
        logical, intent(in), optional :: nofollow
        character(len=:), allocatable :: absname
        integer(int64) :: sec
        integer(int32) :: nsec, err
        logical :: nofollow_flag
        nofollow_flag = .false.
        if (present(nofollow)) nofollow_flag = nofollow
        if (present(mtime_sec)) then
            sec = mtime_sec
            nsec = 0
            if (present(mtime_nsec)) nsec = mtime_nsec
        else
            sec = 0
            nsec = int(UTIME_NOW, int32)
        end if
        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        err = sys_set_file_times(absname, sec, nsec, sec, nsec, nofollow_flag)
        if (err /= 0) call report_file_errno("Setting file times", absname, err, status)
    end subroutine set_file_times

    !> Return FILE as `cp FILE NEWNAME' would name the target: a file
    !> under NEWNAME with FILE's own name if NEWNAME is a directory name,
    !> else NEWNAME (expanded).  Port of fileio.c:expand_cp_target.
    subroutine expand_cp_target(file, newname, target, status)
        character(len=*), intent(in) :: file, newname
        character(len=:), allocatable, intent(out) :: target
        type(status_type), intent(out) :: status
        if (directory_name_p(newname)) then
            call expand_file_name(file_name_nondirectory(file), target, status, newname)
        else
            call expand_file_name(newname, target, status)
        end if
    end subroutine expand_cp_target

    !> Signal an error if file ABSNAME exists: `file_already_exists',
    !> or `file_error' if it is a directory.  KNOWN_TO_EXIST says it is
    !> known to exist.  Port of fileio.c:barf_or_query_if_file_exists
    !> (the query is not ported: INTERACTIVE false).
    subroutine barf_if_file_exists(absname, known_to_exist, status)
        character(len=*), intent(in) :: absname
        logical, intent(in) :: known_to_exist
        type(status_type), intent(inout) :: status
        type(file_stat_type) :: st
        logical :: known
        known = known_to_exist
        if (.not. known) then
            if (sys_stat(absname, st, nofollow=.true.) == 0) then
                if (iand(st%mode, S_IFMT) == S_IFDIR) then
                    call set_error(status, FM_FILE_ERROR, "File is a directory: "//absname)
                    return
                end if
                known = .true.
            end if
        end if
        if (known) call set_error(status, FM_FILE_ALREADY_EXISTS, "File already exists: "//absname)
    end subroutine barf_if_file_exists

    !> Rename FILE as NEWNAME.  Both args must be strings.
    !> If file has names other than FILE, it continues to have those names.
    !> If NEWNAME is a directory name, rename FILE to a like-named file under
    !> NEWNAME.  For NEWNAME to be recognized as a directory name, it should
    !> end in a slash.
    !>
    !> Signal a `file_already_exists' error if a file NEWNAME already exists
    !> unless optional third argument OK_IF_ALREADY_EXISTS is true.  Port
    !> of fileio.c:Frename_file (not interactive: no confirmation).
    recursive subroutine rename_file(file, newname, status, ok_if_already_exists)
        character(len=*), intent(in) :: file, newname
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: ok_if_already_exists
        character(len=:), allocatable :: from, to, target
        type(file_stat_type) :: file_st
        integer(int32) :: err, lerr
        logical :: ok, plain_rename, dirp
        ok = .false.
        if (present(ok_if_already_exists)) ok = ok_if_already_exists
        call expand_file_name(file, from, status)
        if (.not. status%ok()) return
        call expand_cp_target(directory_file_name(from), newname, to, status)
        if (.not. status%ok()) return
        plain_rename = ok
        err = 0
        if (.not. plain_rename) then
            err = sys_rename_noreplace(from, to)
            if (err == 0) return
            select case (err)
            case (EEXIST, EINVAL, ENOSYS, ENOTSUP)
                call barf_if_file_exists(to, err == EEXIST, status)
                if (.not. status%ok()) return
                plain_rename = .true.
            case default
                continue
            end select
        end if
        if (plain_rename) then
            err = sys_rename(from, to)
            if (err == 0) return
            ! Don't prompt again.
            ok = .true.
        end if
        if (err /= EXDEV) then
            call report_file_errno("Renaming", from//", "//to, err, status)
            return
        end if
        ! Across file systems: copy, then delete the original.
        dirp = directory_name_p(from)
        if (.not. dirp) then
            lerr = sys_stat(from, file_st, nofollow=.true.)
            if (lerr /= 0) then
                call report_file_errno("Renaming", from//", "//to, lerr, status)
                return
            end if
            dirp = iand(file_st%mode, S_IFMT) == S_IFDIR
        end if
        if (dirp) then
            if (.not. associated(copy_directory_hook)) then
                call report_file_errno("Renaming", from//", "//to, err, status)
                return
            end if
            call copy_directory_hook(from, to, status)
        else if (iand(file_st%mode, S_IFMT) == S_IFREG) then
            call copy_file(from, to, status, ok_if_already_exists=ok, keep_time=.true., preserve_uid_gid=.true., &
                           preserve_permissions=.true.)
        else if (iand(file_st%mode, S_IFMT) == S_IFLNK) then
            lerr = emacs_readlink(from, target)
            if (lerr /= 0) then
                call report_file_errno("Renaming", from//", "//to, lerr, status)
                return
            end if
            call make_symbolic_link(target, to, status, ok)
        else
            call report_file_errno("Renaming", from//", "//to, err, status)
        end if
        if (.not. status%ok()) return
        ! (delete-by-moving-to-trash nil)
        if (dirp) then
            if (associated(delete_directory_hook)) call delete_directory_hook(from, status)
        else
            if (associated(delete_file_hook)) then
                call delete_file_hook(from, status)
            else
                call delete_file_internal(from, status)
            end if
        end if
    end subroutine rename_file

    !> Copy FILE to NEWNAME.  Both args must be strings.
    !> If NEWNAME is a directory name, copy FILE to a like-named file under
    !> NEWNAME.  For NEWNAME to be recognized as a directory name, it should
    !> end in a slash.
    !>
    !> This function always sets the file modes of the output file to match
    !> the input file.
    !>
    !> The optional argument OK_IF_ALREADY_EXISTS specifies what to do
    !> if file NEWNAME already exists.  If OK_IF_ALREADY_EXISTS is false,
    !> signal a `file_already_exists' error without overwriting.
    !> Otherwise overwrite the existing file.
    !>
    !> KEEP_TIME true means give the output file the same
    !> last-modified time as the old one.  (This works on only some systems.)
    !>
    !> If PRESERVE_UID_GID is true, try to transfer the uid and gid of
    !> FILE to NEWNAME.
    !>
    !> If PRESERVE_PERMISSIONS is true, copy permissions of FILE to NEWNAME;
    !> this includes the file modes, along with ACL entries and SELinux
    !> context if present.  Otherwise, if NEWNAME is created its file
    !> permission bits are those of FILE, masked by the default file
    !> permissions.  Port of fileio.c:Fcopy_file (not interactive: no
    !> confirmation; ACL entries and SELinux contexts are not copied, and
    !> FICLONE and copy_file_range are not used: the data are read and
    !> written).
    subroutine copy_file(file, newname, status, ok_if_already_exists, keep_time, preserve_uid_gid, &
                         preserve_permissions)
        character(len=*), intent(in) :: file, newname
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: ok_if_already_exists, keep_time, preserve_uid_gid, preserve_permissions
        character(len=:), allocatable :: from, to
        character(len=READ_BUF_SIZE) :: chunk
        type(file_stat_type) :: st, out_st
        type(status_type) :: st_io
        integer(int32) :: err, ifd, ofd, new_mask, preserved, default_permissions, mask
        integer(int64) :: nread, written
        logical :: ok, keep, preserve_ids, preserve_perms, already_exists
        ok = .false.
        if (present(ok_if_already_exists)) ok = ok_if_already_exists
        keep = .false.
        if (present(keep_time)) keep = keep_time
        preserve_ids = .false.
        if (present(preserve_uid_gid)) preserve_ids = preserve_uid_gid
        preserve_perms = .false.
        if (present(preserve_permissions)) preserve_perms = preserve_permissions
        call expand_file_name(file, from, status)
        if (.not. status%ok()) return
        call expand_cp_target(from, newname, to, status)
        if (.not. status%ok()) return
        err = emacs_open_read(from, ifd)
        if (err /= 0) then
            call report_file_errno("Opening input file", from, err, status)
            return
        end if
        err = sys_stat(from, st)
        if (err /= 0) then
            call close_fd(ifd)
            call report_file_errno("Input file status", from, err, status)
            return
        end if
        ! We can copy only regular files.
        if (iand(st%mode, S_IFMT) /= S_IFREG) then
            call close_fd(ifd)
            if (iand(st%mode, S_IFMT) == S_IFDIR) then
                call report_file_errno("Non-regular file", from, EISDIR, status)
            else
                call report_file_errno("Non-regular file", from, EINVAL, status)
            end if
            return
        end if
        if (preserve_ids) then
            new_mask = iand(st%mode, int(o'700', int32))
        else
            new_mask = iand(st%mode, int(o'777', int32))
        end if
        already_exists = .false.
        err = emacs_open_mode(to, ior(O_WRONLY, ior(O_CREAT, O_EXCL)), new_mask, ofd)
        if (err == EEXIST) then
            if (.not. ok) then
                call close_fd(ifd)
                call barf_if_file_exists(to, .true., status)
                return
            end if
            already_exists = .true.
            err = emacs_open_mode(to, ior(O_WRONLY, O_TRUNC), 0, ofd)
        end if
        if (err /= 0) then
            call close_fd(ifd)
            call report_file_errno("Opening output file", to, err, status)
            return
        end if
        if (already_exists) then
            err = sys_stat(to, out_st)
            if (err /= 0) then
                call close_fd(ifd)
                call close_fd(ofd)
                call report_file_errno("Output file status", to, err, status)
                return
            end if
            if (st%dev == out_st%dev .and. st%ino == out_st%ino) then
                call close_fd(ifd)
                call close_fd(ofd)
                call report_file_errno("Input and output files are the same", from//", "//to, 0, status)
                return
            end if
        end if
        do
            call emacs_read(ifd, chunk, nread, st_io, err)
            if (.not. st_io%ok()) then
                call close_fd(ifd)
                call close_fd(ofd)
                call report_file_errno("Read error", from, err, status)
                return
            end if
            if (nread == 0) exit
            call emacs_full_write(ofd, chunk(1:nread), written, st_io, err)
            if (.not. st_io%ok() .or. written /= nread) then
                call close_fd(ifd)
                call close_fd(ofd)
                call report_file_errno("Write error", to, err, status)
                return
            end if
        end do
        ! Preserve the original file permissions, and if requested, also
        ! its owner and group.
        mask = current_realmask()
        preserved = iand(st%mode, int(o'7777', int32))
        default_permissions = iand(iand(st%mode, int(o'777', int32)), not(mask))
        if (preserve_ids) then
            ! Attempt to change owner and group.  If that doesn't work
            ! attempt to change just the group, as that is sometimes
            ! allowed.  Adjust the mode mask to eliminate setuid or setgid
            ! bits or group permissions bits that are inappropriate if the
            ! owner or group are wrong.
            if (sys_fchown(ofd, st%uid, st%gid) /= 0) then
                if (sys_fchown(ofd, -1, st%gid) == 0) then
                    preserved = iand(preserved, not(int(o'4000', int32)))
                else
                    preserved = iand(preserved, not(int(o'6000', int32)))
                    ! Copy the other bits to the group bits, since the
                    ! group is wrong.
                    preserved = iand(preserved, not(int(o'70', int32)))
                    preserved = ior(preserved, shiftl(iand(preserved, 7_int32), 3))
                    default_permissions = iand(default_permissions, not(int(o'70', int32)))
                    default_permissions = ior(default_permissions, shiftl(iand(default_permissions, 7_int32), 3))
                end if
            end if
        end if
        err = 0
        if (preserve_perms) then
            ! qcopy_acl without ACLs: the permission bits.
            err = sys_fchmod(ofd, preserved)
        else if (.not. (already_exists .or. iand(new_mask, not(mask)) == default_permissions)) then
            err = sys_fchmod(ofd, default_permissions)
        end if
        if (err /= 0) then
            call close_fd(ifd)
            call close_fd(ofd)
            call report_file_errno("Copying permissions to", to, err, status)
            return
        end if
        if (keep) then
            if (sys_futimens(ofd, st%atime_sec, st%atime_nsec, st%mtime_sec, st%mtime_nsec) /= 0) then
                call close_fd(ifd)
                call close_fd(ofd)
                call set_error(status, FM_FILE_DATE_ERROR, "Cannot set file date: "//to)
                return
            end if
        end if
        call close_fd(ofd)
        call close_fd(ifd)
    end subroutine copy_file

    !> Give FILE additional name NEWNAME.  Both args must be strings.
    !> If NEWNAME is a directory name, give FILE a like-named new name under
    !> NEWNAME.
    !>
    !> Signal a `file_already_exists' error if a file NEWNAME already exists
    !> unless optional third argument OK_IF_ALREADY_EXISTS is true.
    !> Port of fileio.c:Fadd_name_to_file (not interactive).
    subroutine add_name_to_file(file, newname, status, ok_if_already_exists)
        character(len=*), intent(in) :: file, newname
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: ok_if_already_exists
        character(len=:), allocatable :: from, to
        integer(int32) :: err
        logical :: ok
        ok = .false.
        if (present(ok_if_already_exists)) ok = ok_if_already_exists
        call expand_file_name(file, from, status)
        if (.not. status%ok()) return
        call expand_cp_target(from, newname, to, status)
        if (.not. status%ok()) return
        err = sys_link(from, to)
        if (err == 0) return
        if (err == EEXIST) then
            if (.not. ok) then
                call barf_if_file_exists(to, .true., status)
                return
            end if
            err = sys_unlink(to)
            err = sys_link(from, to)
            if (err == 0) return
        end if
        call report_file_errno("Adding new name", from//", "//to, err, status)
    end subroutine add_name_to_file

    !> The target of symbolic link FILENAME, or "" if it is not a
    !> symbolic link (the string `file_symlink_p' returns upstream).
    !> Port of fileio.c:Ffile_symlink_p.
    function file_symlink_target(filename) result(target)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: target
        character(len=:), allocatable :: absname
        integer(int32) :: err
        target = ""
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        if (.not. file_symlink_p(absname)) return
        err = emacs_readlink(absname, target)
        if (err /= 0) target = ""
    end function file_symlink_target

    !> Create directory `directory' (its parent must exist).
    !> Port of fileio.c:Fmake_directory_internal.
    subroutine make_directory_internal(directory, status)
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir
        integer(int32) :: err
        call expand_file_name(directory, dir, status)
        if (.not. status%ok()) return
        dir = directory_file_name(dir)
        err = sys_mkdir(dir, int(o'777', int32))
        if (err /= 0) call report_file_errno("Creating directory", dir, err, status)
    end subroutine make_directory_internal

    !> Delete the empty directory `directory'.
    !> Port of fileio.c:Fdelete_directory_internal.
    subroutine delete_directory_internal(directory, status)
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir
        integer(int32) :: err
        call expand_file_name(directory, dir, status)
        if (.not. status%ok()) return
        dir = directory_file_name(dir)
        err = sys_rmdir(dir)
        if (err /= 0) call report_file_errno("Removing directory", dir, err, status)
    end subroutine delete_directory_internal

    !> Delete file `filename'.  Port of fileio.c:Fdelete_file_internal.
    subroutine delete_file_internal(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: absname
        integer(int32) :: err
        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        err = sys_unlink(absname)
        if (err /= 0 .and. err /= ENOENT) call report_file_errno("Removing old name", absname, err, status)
    end subroutine delete_file_internal

    !> Return storage information about the file system FILENAME is on.
    !> The values are TOTAL, FREE and AVAIL, where TOTAL is the total
    !> storage of the file system, FREE is the free storage, and AVAIL is the
    !> storage available to a non-superuser.  All 3 numbers are in bytes.
    !> If the underlying system call fails, FOUND is false.  Port of
    !> fileio.c:Ffile_system_info (an error other than
    !> a missing file is signaled as "Getting attributes").
    subroutine file_system_info(filename, total, free, avail, found, status)
        character(len=*), intent(in) :: filename
        integer(int64), intent(out) :: total, free, avail
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: absname
        integer(int32) :: err
        found = .false.
        total = 0
        free = 0
        avail = 0
        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        err = emacs_statvfs(absname, total, free, avail)
        if (err == 0) then
            found = .true.
        else if (err /= ENOENT .and. err /= ENOTDIR) then
            ! file_metadata_errno
            call report_file_errno("Getting attributes", absname, err, status)
        end if
    end subroutine file_system_info

    !> A file error for `name' from errno `err': "STRING: <strerror>, NAME",
    !> file-missing for ENOENT.  Port of fileio.c:report_file_errno.
    subroutine report_file_errno(string, name, err, status)
        character(len=*), intent(in) :: string, name
        integer(int32), intent(in) :: err
        type(status_type), intent(inout) :: status
        integer(int32) :: code
        ! (file-already-exists ERRSTRING NAME): no STRING.
        if (err == EEXIST) then
            call set_error(status, FM_FILE_ALREADY_EXISTS, emacs_strerror(err)//": "//name)
            return
        end if
        code = FM_FILE_ERROR
        if (err == ENOENT) code = FM_FILE_MISSING
        if (err == EACCES) code = FM_PERMISSION_DENIED
        call set_error(status, code, string//": "//emacs_strerror(err)//", "//name)
    end subroutine report_file_errno

    ! ------------------------------------------------------------------
    ! Reading and writing
    ! ------------------------------------------------------------------

    !> All the bytes of file `absname'.  Errors opening it are reported
    !> with `what' ("Opening input file").
    subroutine read_file(absname, what, data, status, st, opened)
        character(len=*), intent(in) :: absname, what
        character(len=:), allocatable, intent(out) :: data
        type(status_type), intent(out) :: status
        !> The file's status when it was read.
        type(file_stat_type), intent(out), optional :: st
        !> Whether the file could be opened at all.
        logical, intent(out), optional :: opened
        type(file_stat_type) :: fst
        character(len=:), allocatable :: grown
        character(len=READ_BUF_SIZE) :: chunk
        integer(int32) :: fd, err
        integer(int64) :: n, nread
        data = ""
        if (present(opened)) opened = .false.
        err = emacs_open_read(absname, fd)
        if (err /= 0) then
            call report_file_errno(what, absname, err, status)
            return
        end if
        if (present(opened)) opened = .true.
        err = sys_stat(absname, fst)
        if (present(st)) st = fst
        ! POSIX lets read succeed on a directory; refuse it here.
        if (err == 0 .and. iand(fst%mode, S_IFMT) == S_IFDIR) then
            call close_fd(fd)
            call report_file_errno("Read error", absname, EISDIR, status)
            return
        end if
        deallocate (data)
        allocate (character(len=max(int(fst%size, int64), 1_int64)) :: data)
        n = 0
        do
            call emacs_read(fd, chunk, nread, status, err)
            if (.not. status%ok()) then
                call close_fd(fd)
                status = status_type()
                call report_file_errno("Read error", absname, err, status)
                return
            end if
            if (nread == 0) exit
            if (n + nread > len(data, kind=int64)) then
                allocate (character(len=max(2*len(data, kind=int64), n + nread)) :: grown)
                grown(1:n) = data(1:n)
                call move_alloc(grown, data)
            end if
            data(n + 1:n + nread) = chunk(1:nread)
            n = n + nread
        end do
        call close_fd(fd)
        data = data(1:n)
    end subroutine read_file

    !> Insert the contents of file `filename' after point, leaving point
    !> before them.  With `visit', the buffer (which must be empty) visits
    !> the file: its file name and modification time are recorded and it
    !> is marked unmodified, even if the file doesn't exist (which is then
    !> reported as file-missing).  `inserted' is the number of characters
    !> inserted.  The change hooks run for the inserted text, except when
    !> visiting.  Port of fileio.c:Finsert_file_contents (no BEG, END or
    !> REPLACE; decoding as described in the module documentation).
    subroutine insert_file_contents(filename, visit, inserted, status)
        character(len=*), intent(in) :: filename
        logical, intent(in) :: visit
        integer(int64), intent(out) :: inserted
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: absname, data, text
        type(file_stat_type) :: st
        integer(int64) :: opoint, nchars
        logical :: opened, saved_inhibit
        inserted = 0
        if (buffers(cur)%read_only) then
            call barf_if_buffer_read_only(status)
            if (.not. status%ok()) return
        end if
        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        if (visit .and. buffers(cur)%text%z > BEG) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot do file visiting in a non-empty buffer")
            return
        end if
        call read_file(absname, "Opening input file", data, status, st, opened)
        if (.not. status%ok()) then
            ! notfound: a visit still records the file, as nonexistent.
            if (visit .and. .not. opened) then
                buffers(cur)%modtime_known = .true.
                buffers(cur)%modtime_sec = 0
                buffers(cur)%modtime_nsec = NONEXISTENT_MODTIME_NSECS
                buffers(cur)%modtime_size = -1
                buffers(cur)%filename = absname
                buffers(cur)%text%save_modiff = buffers(cur)%text%modiff
            end if
            return
        end if
        if (buffers(cur)%multibyte) then
            call str_as_multibyte(data, text, nchars)
        else
            text = data
            nchars = len(data, kind=int64)
        end if
        opoint = point()
        if (len(text) > 0) then
            ! Normal visiting is done in a new buffer "before" the buffer
            ! is changed: no change hooks.
            saved_inhibit = inhibit_modification_hooks%value
            if (visit) inhibit_modification_hooks%value = .true.
            call insert(text, status)
            inhibit_modification_hooks%value = saved_inhibit
            if (.not. status%ok()) return
            vdeactivate_mark = .true.
            call goto_char(opoint)
        end if
        inserted = nchars
        if (visit) then
            buffers(cur)%modtime_known = .true.
            buffers(cur)%modtime_sec = st%mtime_sec
            buffers(cur)%modtime_nsec = st%mtime_nsec
            buffers(cur)%modtime_size = st%size
            buffers(cur)%filename = absname
            buffers(cur)%text%save_modiff = buffers(cur)%text%modiff
        end if
    end subroutine insert_file_contents

    !> Write the text between `start' and `end' (default: the whole
    !> buffer, ignoring narrowing) to file `filename', replacing it.  With
    !> `visit', the buffer then visits that file and is unmodified.
    !> `message', if present, receives "Wrote FILE" for the echo area.
    !> Port of fileio.c:Fwrite_region (no APPEND, LOCKNAME or MUSTBENEW;
    !> no fsync, as write-region-inhibit-fsync defaults to t).
    subroutine write_region(filename, visit, status, start, end, message)
        character(len=*), intent(in) :: filename
        logical, intent(in) :: visit
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end
        character(len=:), allocatable, intent(out), optional :: message
        character(len=:), allocatable :: absname, text
        type(file_stat_type) :: st
        type(status_type) :: wst
        integer(int64) :: s, e, written
        integer(int32) :: fd, err
        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        if (present(start) .and. present(end)) then
            s = start
            e = end
            call validate_region(s, e, status)
            if (.not. status%ok()) return
            text = buf_bytes(buffers(cur), char_to_byte(s), char_to_byte(e))
        else
            text = buf_bytes(buffers(cur), BEG_BYTE, buffers(cur)%text%z_byte)
        end if
        if (buffers(cur)%multibyte) text = str_as_unibyte(text)
        err = emacs_creat(absname, int(o'666', int32), fd)
        if (err /= 0) then
            call report_file_errno("Opening output file", absname, err, status)
            return
        end if
        call emacs_full_write(fd, text, written, wst, err)
        call close_fd(fd)
        if (.not. wst%ok()) then
            call report_file_errno("Write error", absname, err, status)
            return
        end if
        if (visit) then
            buffers(cur)%filename = absname
            buffers(cur)%text%save_modiff = buffers(cur)%text%modiff
            if (sys_stat(absname, st) == 0) then
                buffers(cur)%modtime_known = .true.
                buffers(cur)%modtime_sec = st%mtime_sec
                buffers(cur)%modtime_nsec = st%mtime_nsec
                buffers(cur)%modtime_size = st%size
            else
                buffers(cur)%modtime_known = .false.
            end if
        end if
        if (present(message)) message = "Wrote "//absname
    end subroutine write_region

    !> Whether the current buffer's visited file is unchanged since it was
    !> last read or written (true if the buffer visits no file or the time
    !> is unknown).  Port of fileio.c:Fverify_visited_file_modtime.
    logical function verify_visited_file_modtime()
        type(file_stat_type) :: st
        integer(int64) :: sec
        integer(int32) :: nsec
        verify_visited_file_modtime = .true.
        if (.not. allocated(buffers(cur)%filename)) return
        if (.not. buffers(cur)%modtime_known) return
        if (sys_stat(buffers(cur)%filename, st) == 0) then
            sec = st%mtime_sec
            nsec = st%mtime_nsec
        else
            sec = 0
            nsec = NONEXISTENT_MODTIME_NSECS
        end if
        verify_visited_file_modtime = sec == buffers(cur)%modtime_sec .and. &
                                      nsec == buffers(cur)%modtime_nsec .and. &
                                      (buffers(cur)%modtime_size < 0 .or. &
                                       nsec == NONEXISTENT_MODTIME_NSECS .or. &
                                       st%size == buffers(cur)%modtime_size)
    end function verify_visited_file_modtime

    !> Record the visited file's current modification time as the
    !> buffer's.  Port of fileio.c:Fset_visited_file_modtime (no argument).
    subroutine set_visited_file_modtime()
        type(file_stat_type) :: st
        if (.not. allocated(buffers(cur)%filename)) return
        buffers(cur)%modtime_known = .true.
        if (sys_stat(buffers(cur)%filename, st) == 0) then
            buffers(cur)%modtime_sec = st%mtime_sec
            buffers(cur)%modtime_nsec = st%mtime_nsec
            buffers(cur)%modtime_size = st%size
        else
            buffers(cur)%modtime_sec = 0
            buffers(cur)%modtime_nsec = NONEXISTENT_MODTIME_NSECS
            buffers(cur)%modtime_size = -1
        end if
    end subroutine set_visited_file_modtime

    !> Make a symbolic link to TARGET, named LINKNAME.
    !> If LINKNAME is a directory name, make a like-named symbolic link
    !> under LINKNAME.
    !>
    !> Signal a `file_already_exists' error if a file LINKNAME already exists
    !> unless OK_IF_ALREADY_EXISTS is true.  Port of
    !> fileio.c:Fmake_symbolic_link (without the interactive query).
    subroutine make_symbolic_link(target, linkname, status, ok_if_already_exists)
        character(len=*), intent(in) :: target, linkname
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: ok_if_already_exists
        character(len=:), allocatable :: name
        integer(int32) :: err
        logical :: ok
        ok = .false.
        if (present(ok_if_already_exists)) ok = ok_if_already_exists
        ! expand_cp_target: a directory name gets TARGET's own name.
        if (directory_name_p(linkname)) then
            call expand_file_name(file_name_nondirectory(target), name, status, linkname)
        else
            call expand_file_name(linkname, name, status)
        end if
        if (.not. status%ok()) return
        err = sys_symlink(target, name)
        if (err == 0) return
        if (err == EEXIST) then
            if (.not. ok) then
                call set_error(status, FM_FILE_ALREADY_EXISTS, "File already exists: "//name)
                return
            end if
            err = sys_unlink(name)
            err = sys_symlink(target, name)
            if (err == 0) return
        end if
        call report_file_errno("Making symbolic link", target//", "//name, err, status)
    end subroutine make_symbolic_link
end module fileio_fm
