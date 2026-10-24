! files_copy_fm.f90 -- copying directories, file modes and creating empty
! files, a submodule of files_fm.
! Ported from GNU Emacs 31.1 lisp/files.el.
!
! Copyright (C) 1985-1987, 1992-2026 Free Software Foundation, Inc.
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

!> `copy_directory', `file_in_directory_p', `file_equal_p',
!> `make_empty_file' and the symbolic file modes
!> (`file_modes_symbolic_to_number', `file_modes_number_to_symbolic'):
!> a submodule because `copy_directory' uses dired_fm
!> (`directory_files'), which uses files_fm.  It also gives fileio_fm the
!> files.el procedures `rename_file' calls across file systems.
submodule(files_fm) files_copy_fm
    use lisp_fm, only: list_elements, xstring
    use error_fm, only: FM_FILE_ALREADY_EXISTS
    use sysdep_fm, only: S_IFMT, S_IFDIR, S_IFLNK
    use dired_fm, only: directory_files
    use fileio_fm, only: copy_file, make_symbolic_link, file_symlink_target, set_file_times, default_file_modes, &
                         copy_directory_hook, delete_directory_hook, delete_file_hook
    implicit none(type, external)

contains

    recursive module subroutine copy_directory(directory, newname, status, keep_time, parents, copy_contents)
        character(len=*), intent(in) :: directory, newname
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: keep_time, parents, copy_contents
        character(len=:), allocatable :: dir, new, target, file, filetype
        type(value_type) :: files
        type(value_type), allocatable :: elems(:)
        type(file_stat_type) :: attrs, dattrs
        type(status_type) :: err
        integer(int32) :: modes
        logical :: keep, mkparents, contents, follow, found
        integer :: k
        status = status_type()
        keep = .false.
        if (present(keep_time)) keep = keep_time
        mkparents = .false.
        if (present(parents)) mkparents = parents
        contents = .false.
        if (present(copy_contents)) contents = copy_contents
        if (file_in_directory_p(newname, directory)) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Cannot copy `")//directory// &
                           format_message("' into its subdirectory `")//newname//format_message("'"))
            return
        end if
        ! Compute target name.
        call expand_file_name(directory, dir, status)
        if (.not. status%ok()) return
        dir = directory_file_name(dir)
        call expand_file_name(newname, new, status)
        if (.not. status%ok()) return
        ! If DIRECTORY is a symlink, create a symlink with the same target.
        if (file_symlink_p(dir) .and. copy_directory_create_symlink) then
            target = file_symlink_target(dir)
            if (directory_name_p(new)) then
                call make_symbolic_link(target, new//file_name_nondirectory(dir), status, .true.)
            else
                call make_symbolic_link(target, new, status, .true.)
            end if
            return
        end if
        ! Else proceed to copy as a regular directory first by creating
        ! the destination directory if needed, preparing to follow any
        ! symlink to a directory we did not create.
        if (.not. directory_name_p(new)) then
            ! If NEWNAME is not a directory name, create it; that is where
            ! we will copy the files of DIRECTORY.
            call make_directory(new, status, mkparents, follow)
            if (.not. status%ok()) return
        else
            ! NEWNAME is a directory name.  If COPY-CONTENTS is true,
            ! create NEWNAME if it is not already a directory; otherwise,
            ! create NEWNAME/[DIRECTORY-BASENAME].
            if (.not. contents) new = new//file_name_nondirectory(dir)
            call make_directory(directory_file_name(new), err, mkparents, follow)
            if (.not. err%ok()) then
                if (.not. file_directory_p(new)) then
                    status = err
                    return
                end if
                follow = .true.
            end if
        end if
        ! Copy recursively.  We do not want to copy "." and "..".
        call directory_files(dir, files, status, full=.true.)
        if (.not. status%ok()) return
        call list_elements(files, elems)
        do k = 1, size(elems)
            file = xstring(elems(k))
            if (file_name_nondirectory(file) == "." .or. file_name_nondirectory(file) == "..") cycle
            target = file_name_as_directory(new)//file_name_nondirectory(file)
            call file_attributes(file, attrs, found, nofollow=.true.)
            filetype = ""
            if (found) then
                if (iand(attrs%mode, S_IFMT) == S_IFLNK) filetype = file_symlink_target(file)
            end if
            if (found .and. iand(attrs%mode, S_IFMT) == S_IFDIR) then
                ! Directory but not a symlink.
                call copy_directory(file, target, status, keep, mkparents, .true.)
            else if (found .and. iand(attrs%mode, S_IFMT) == S_IFLNK) then
                ! Symbolic link
                call make_symbolic_link(filetype, target, status, .true.)
            else
                call copy_file(file, target, status, ok_if_already_exists=.true., keep_time=keep)
            end if
            if (.not. status%ok()) return
        end do
        ! Set directory attributes.
        modes = file_modes(dir)
        call file_attributes(dir, dattrs, found, nofollow=.true.)
        if (modes >= 0) call set_file_modes(new, modes, status, nofollow=.not. follow)
        if (.not. status%ok()) return
        if (keep .and. found) then
            call set_file_times(new, status, dattrs%mtime_sec, dattrs%mtime_nsec, nofollow=.not. follow)
        end if
    end subroutine copy_directory

    module function file_in_directory_p(file, dir) result(inside)
        character(len=*), intent(in) :: file, dir
        logical :: inside
        character(len=:), allocatable :: f, d, root
        type(status_type) :: st
        integer :: i1, i2, j1, j2
        logical :: mismatch, more1, more2
        inside = .false.
        ! DIR must exist.
        if (.not. file_directory_p(dir)) return
        call file_truename(file, f, st)
        if (.not. st%ok()) return
        call file_truename(dir, d, st)
        if (.not. st%ok()) return
        if (index(f, "//") == 1) then
            ! A UNC on Windows systems, or a "super-root" on Apollo.
            root = "//"
        else if (index(f, "/") == 1) then
            root = "/"
        else
            root = ""
        end if
        ! (split-string file "/" t) and (split-string dir "/" t), compared
        ! component by component.
        mismatch = .false.
        i1 = 1
        j1 = 1
        do
            call next_component(f, i1, i2, more1)
            call next_component(d, j1, j2, more2)
            if (.not. (more1 .and. more2)) exit
            if (f(i1:i2) == d(j1:j2) .and. i2 - i1 == j2 - j1) then
                root = root//f(i1:i2)//"/"
            else
                mismatch = .true.
                exit
            end if
            i1 = i2 + 1
            j1 = j2 + 1
        end do
        if (.not. mismatch) inside = file_equal_p(root, dir)
    contains

        !> The next non-empty component of S at or after START: S(START:FINISH).
        subroutine next_component(s, start, finish, found)
            character(len=*), intent(in) :: s
            integer, intent(inout) :: start
            integer, intent(out) :: finish
            logical, intent(out) :: found
            integer :: slash
            found = .false.
            finish = start - 1
            do while (start <= len(s))
                if (s(start:start) /= "/") exit
                start = start + 1
            end do
            if (start > len(s)) return
            found = .true.
            slash = index(s(start:), "/")
            if (slash == 0) then
                finish = len(s)
            else
                finish = start + slash - 2
            end if
        end subroutine next_component
    end function file_in_directory_p

    module function file_equal_p(file1, file2) result(same)
        character(len=*), intent(in) :: file1, file2
        logical :: same
        character(len=:), allocatable :: t1, t2
        type(file_stat_type) :: a1, a2
        type(status_type) :: st
        logical :: found1, found2
        same = .false.
        call file_truename(file1, t1, st)
        if (.not. st%ok()) return
        call file_truename(file2, t2, st)
        if (.not. st%ok()) return
        call file_attributes(t1, a1, found1, nofollow=.true.)
        if (.not. found1) return
        call file_attributes(t2, a2, found2, nofollow=.true.)
        if (.not. found2) return
        ! (equal f1-attr f2-attr)
        same = a1%mode == a2%mode .and. a1%nlink == a2%nlink .and. a1%uid == a2%uid .and. a1%gid == a2%gid .and. &
               a1%size == a2%size .and. a1%mtime_sec == a2%mtime_sec .and. a1%mtime_nsec == a2%mtime_nsec .and. &
               a1%atime_sec == a2%atime_sec .and. a1%atime_nsec == a2%atime_nsec .and. a1%dev == a2%dev .and. &
               a1%ino == a2%ino
    end function file_equal_p

    module subroutine make_empty_file(filename, status, parents)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: parents
        character(len=:), allocatable :: paren_dir
        type(handle_type) :: old, buf
        type(status_type) :: st
        logical :: mkparents
        status = status_type()
        mkparents = .false.
        if (present(parents)) mkparents = parents
        if (file_exists_p(filename) .and. .not. mkparents) then
            call set_error(status, FM_FILE_ALREADY_EXISTS, "File exists: "//filename)
            return
        end if
        paren_dir = file_name_directory(filename)
        if (len(paren_dir) > 0) then
            if (.not. file_exists_p(paren_dir)) then
                call make_directory(paren_dir, status, mkparents)
                if (.not. status%ok()) return
            end if
        end if
        ! (write-region "" nil filename nil 0): an empty temporary buffer
        ! written without a message.
        old = current_buffer()
        call get_buffer_create(" *make-empty-file*", buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (status%ok()) call erase_buffer(status)
        if (status%ok()) call write_region(filename, .false., status)
        call set_buffer(old, st)
        call kill_buffer(buf, st)
    end subroutine make_empty_file

    !> Convert CHAR to a numeric bit-mask for extracting mode bits.
    !> CHAR is in [ugoa] and represents the category of users (Owner, Group,
    !> Others, or All) for whom to produce the mask.
    !> The bit-mask that is returned extracts from mode bits the access rights
    !> for the specified category of users.  Port of
    !> files.el:file-modes-char-to-who.
    subroutine file_modes_char_to_who(c, mask, status)
        character, intent(in) :: c
        integer(int32), intent(out) :: mask
        type(status_type), intent(inout) :: status
        mask = 0
        select case (c)
        case ("u")
            mask = int(o'4700', int32)
        case ("g")
            mask = int(o'2070', int32)
        case ("o")
            mask = int(o'1007', int32)
        case ("a")
            mask = int(o'7777', int32)
        case default
            call set_error(status, FM_GENERIC_ERROR, c//format_message(": Bad `who' character"))
        end select
    end subroutine file_modes_char_to_who

    !> Convert CHAR to a numeric value of mode bits.
    !> CHAR is in [rwxXstugo] and represents symbolic access permissions.
    !> If CHAR is in [Xugo], the value is taken from FROM.  Port of
    !> files.el:file-modes-char-to-right.
    integer(int32) function file_modes_char_to_right(c, from) result(right)
        character, intent(in) :: c
        integer(int32), intent(in) :: from
        integer(int32) :: uright, gright, oright
        select case (c)
        case ("r")
            right = int(o'0444', int32)
        case ("w")
            right = int(o'0222', int32)
        case ("x")
            right = int(o'0111', int32)
        case ("s")
            right = int(o'6000', int32)
        case ("t")
            right = int(o'1000', int32)
        case ("X")
            ! Rights relative to the previous file modes.
            right = 0
            if (iand(from, int(o'111', int32)) /= 0) right = int(o'0111', int32)
        case ("u")
            uright = iand(int(o'4700', int32), from)
            right = uright + uright/int(o'10', int32) + uright/int(o'100', int32)
        case ("g")
            gright = iand(int(o'2070', int32), from)
            right = gright + gright/int(o'10', int32) + gright*int(o'10', int32)
        case ("o")
            oright = iand(int(o'1007', int32), from)
            right = oright + oright*int(o'10', int32) + oright*int(o'100', int32)
        case default
            ! Never reached: the callers only pass [rwxXstugo].
            right = 0
        end select
    end function file_modes_char_to_right

    !> Convert a symbolic mode string specification to an equivalent number.
    !> RIGHTS is the symbolic mode spec, it should match "([+=-][rwxXstugo]*)+".
    !> WHO_MASK is the bit-mask specifying the category of users to which to
    !> apply the access permissions.  See `file_modes_char_to_who'.
    !> FROM gives the mode bits on which to base permissions if
    !> RIGHTS request to add, remove, or set permissions based on existing ones,
    !> as in "og+rX-w".  Port of files.el:file-modes-rights-to-number.
    integer(int32) function file_modes_rights_to_number(rights, who_mask, from) result(num_rights)
        character(len=*), intent(in) :: rights
        integer(int32), intent(in) :: who_mask, from
        integer(int32) :: num_right
        integer :: i
        character :: op
        num_rights = from
        i = 1
        op = achar(0)
        if (len(rights) >= 1) op = rights(1:1)
        do while (index("+-=", op) > 0)
            num_right = 0
            i = i + 1
            do while (i <= len(rights))
                if (index("rwxXstugo", rights(i:i)) == 0) exit
                num_right = ior(num_right, file_modes_char_to_right(rights(i:i), num_rights))
                i = i + 1
            end do
            num_right = iand(who_mask, num_right)
            select case (op)
            case ("+")
                num_rights = ior(num_rights, num_right)
            case ("-")
                num_rights = iand(num_rights, not(num_right))
            case default
                num_rights = ior(iand(num_rights, not(who_mask)), num_right)
            end select
            op = achar(0)
            if (i <= len(rights)) op = rights(i:i)
        end do
    end function file_modes_rights_to_number

    module subroutine file_modes_symbolic_to_number(modes, num_modes, status, from)
        character(len=*), intent(in) :: modes
        integer(int32), intent(out) :: num_modes
        type(status_type), intent(out) :: status
        integer(int32), intent(in), optional :: from
        character(len=:), allocatable :: rest
        integer(int32) :: num_who, who
        integer :: start, i, who_end, k
        logical :: matched
        status = status_type()
        num_modes = 0
        if (present(from)) num_modes = from
        rest = modes
        do
            if (len(rest) == 0) exit
            if (rest(1:1) == achar(0)) exit
            ! (string-match "^\\([ugoa]*\\)\\([+=-][rwxXstugo]*\\)+\\(,\\|\\)" modes),
            ! case-sensitive: at the start or after a newline.
            matched = .false.
            start = 1
            do while (start <= len(rest))
                i = start
                do while (i <= len(rest))
                    if (index("ugoa", rest(i:i)) == 0) exit
                    i = i + 1
                end do
                who_end = i
                do while (i <= len(rest))
                    if (index("+=-", rest(i:i)) == 0) exit
                    matched = .true.
                    i = i + 1
                    do while (i <= len(rest))
                        if (index("rwxXstugo", rest(i:i)) == 0) exit
                        i = i + 1
                    end do
                end do
                if (matched) exit
                ! The next line start.
                k = index(rest(start:), achar(10))
                if (k == 0) exit
                start = start + k
            end do
            if (.not. matched) then
                call set_error(status, FM_GENERIC_ERROR, format_message("Parse error in modes near `")//rest// &
                               format_message("'"))
                return
            end if
            num_who = 0
            do k = start, who_end - 1
                call file_modes_char_to_who(rest(k:k), who, status)
                if (.not. status%ok()) return
                num_who = ior(num_who, who)
            end do
            if (num_who == 0) num_who = ior(int(o'7000', int32), default_file_modes())
            num_modes = file_modes_rights_to_number(rest(who_end:), num_who, num_modes)
            if (i <= len(rest)) then
                if (rest(i:i) == ",") i = i + 1
            end if
            rest = rest(i:)
        end do
    end subroutine file_modes_symbolic_to_number

    module function file_modes_number_to_symbolic(mode, filetype) result(s)
        integer(int32), intent(in) :: mode
        character, intent(in), optional :: filetype
        character(len=10) :: s
        if (present(filetype)) then
            s(1:1) = filetype
        else
            select case (shiftr(mode, 12))
            case (int(o'14', int32))
                s(1:1) = "s"
            case (int(o'12', int32))
                s(1:1) = "l"
            case (int(o'06', int32))
                s(1:1) = "b"
            case (int(o'04', int32))
                s(1:1) = "d"
            case (int(o'02', int32))
                s(1:1) = "c"
            case (int(o'01', int32))
                s(1:1) = "p"
            case default
                s(1:1) = "-"
            end select
        end if
        s(2:2) = bit(256, "r")
        s(3:3) = bit(128, "w")
        s(4:4) = special(64, 2048, "x", "s", "S")
        s(5:5) = bit(32, "r")
        s(6:6) = bit(16, "w")
        s(7:7) = special(8, 1024, "x", "s", "S")
        s(8:8) = bit(4, "r")
        s(9:9) = bit(2, "w")
        s(10:10) = special(1, 512, "x", "t", "T")
    contains

        character function bit(mask, c)
            integer, intent(in) :: mask
            character, intent(in) :: c
            bit = "-"
            if (iand(mode, mask) /= 0) bit = c
        end function bit

        !> The execute letter: X, or with the special bit S_SET (set) or
        !> S_UNSET (no execute bit).
        character function special(mask, special_mask, x, s_set, s_unset)
            integer, intent(in) :: mask, special_mask
            character, intent(in) :: x, s_set, s_unset
            if (iand(mode, mask) == 0) then
                special = "-"
                if (iand(mode, special_mask) /= 0) special = s_unset
            else
                special = x
                if (iand(mode, special_mask) /= 0) special = s_set
            end if
        end function special
    end function file_modes_number_to_symbolic

    !> `rename_file''s directory copy: (copy-directory FILE NEWNAME t nil).
    recursive subroutine copy_directory_for_rename(directory, newname, status)
        character(len=*), intent(in) :: directory, newname
        type(status_type), intent(out) :: status
        call copy_directory(directory, newname, status, keep_time=.true.)
    end subroutine copy_directory_for_rename

    !> `rename_file''s deletion of a directory: (delete-directory FILE t).
    recursive subroutine delete_directory_for_rename(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        call delete_directory(file, status, .true.)
    end subroutine delete_directory_for_rename

    !> `rename_file''s deletion of a file: (delete-file FILE nil).
    recursive subroutine delete_file_for_rename(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        call delete_file(file, status)
    end subroutine delete_file_for_rename

    module subroutine syms_of_files_copy()
        copy_directory_hook => copy_directory_for_rename
        delete_directory_hook => delete_directory_for_rename
        delete_file_hook => delete_file_for_rename
    end subroutine syms_of_files_copy
end submodule files_copy_fm
