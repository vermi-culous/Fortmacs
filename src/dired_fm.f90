! dired_fm.f90 -- directory listings.
! Ported from GNU Emacs 31.1 src/dired.c.
!
! Copyright (C) 1985-1986, 1993-1994, 1999-2026 Free Software
! Foundation, Inc.
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

!> `directory_files': the names in a directory, as a list of strings.
!>
!> File name completion: `file_name_completion', `file_name_all_completions'.
!>
!> Not ported yet: MATCH (it needs regexps), `directory_files_and_attributes',
!> `file_attributes' as a list, `completion_regexp_list'.
module dired_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_WRONG_TYPE_ARGUMENT
    use lisp_fm, only: value_type, make_string, make_list, nil, t_value
    use fileio_fm, only: expand_file_name, directory_file_name, file_name_as_directory, report_file_errno, &
                         file_directory_p, file_attributes, file_stat_type
    use minibuf_fm, only: completion_ignore_case, completion_predicate
    use fns_fm, only: compare_strings, COMPARE_EQUAL, schars, substring
    use sysdep_fm, only: sys_string_type, emacs_directory_entries, system_user_names, system_group_names, &
                         passwd_name_of_user, user_name_of_uid, group_name_of_gid
    implicit none(type, external)
    private

    public :: directory_files, file_name_completion, file_name_all_completions, system_users, system_groups, &
              file_attribute_names

    !> Completion ignores file names ending in any string in this array.
    !> It does not ignore them if all possible completions end in one of
    !> these strings or when displaying a list of completions.  It ignores
    !> directory names if they match any string in this array which ends in
    !> a slash.  Port of dired.c completion-ignored-extensions, with the
    !> value bindings.el gives it.
    character(len=8), public :: completion_ignored_extensions(67) = [character(len=8) :: &
                                                                     ".o", "~", ".bin", ".lbin", ".so", ".a", ".ln", ".blg", &
                                                                     ".bbl", ".elc", ".lof", ".glo", ".idx", ".lot", ".svn/", &
                                                                     ".hg/", ".git/", ".bzr/", "CVS/", "_darcs/", "_MTN/", &
                                                                     ".fmt", ".tfm", ".class", ".fas", ".lib", ".mem", ".x86f", &
                                                                     ".sparcf", ".dfsl", ".pfsl", ".d64fsl", ".p64fsl", &
                                                                     ".lx64fsl", ".lx32fsl", ".dx64fsl", ".dx32fsl", &
                                                                     ".fx64fsl", ".fx32fsl", ".sx64fsl", ".sx32fsl", &
                                                                     ".wx64fsl", ".wx32fsl", ".fasl", ".ufsl", ".fsl", ".dxl", &
                                                                     ".lo", ".la", ".gmo", ".mo", ".toc", ".aux", ".cp", ".fn", &
                                                                     ".ky", ".pg", ".tp", ".vr", ".cps", ".fns", ".kys", ".pgs", &
                                                                     ".tps", ".vrs", ".pyc", ".pyo"]

contains

    !> Return a list of names of files in DIRECTORY.
    !> There are three optional arguments:
    !> If FULL is true, return absolute file names.  Otherwise return names
    !>  that are relative to the specified directory.
    !> If NOSORT is true, the list is not sorted--its order is unpredictable.
    !>  Otherwise, the list returned is sorted with `string_lessp'.
    !>  NOSORT is useful if you plan to sort the result yourself.
    !> If COUNT is present and a natural number, the function will return
    !>  COUNT number of file names (if so many are present).  Port of
    !> dired.c:Fdirectory_files and directory_files_internal (no MATCH).
    subroutine directory_files(directory, files, status, full, nosort, count)
        character(len=*), intent(in) :: directory
        type(value_type), intent(out) :: files
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: full, nosort
        integer(int64), intent(in), optional :: count
        character(len=:), allocatable :: dir, dirfilename
        type(sys_string_type), allocatable :: names(:)
        type(value_type), allocatable :: elems(:)
        integer, allocatable :: order(:)
        integer(int32) :: err
        integer :: n, k, j, key
        logical :: absolute, sorted
        files = nil()
        if (present(count)) then
            if (count < 0) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "wholenump")
                return
            end if
        end if
        absolute = .false.
        if (present(full)) absolute = full
        sorted = .true.
        if (present(nosort)) sorted = .not. nosort
        call expand_file_name(directory, dir, status)
        if (.not. status%ok()) return
        dirfilename = directory_file_name(dir)
        err = emacs_directory_entries(dirfilename, names)
        if (err /= 0) then
            call report_file_errno("Opening directory", dirfilename, err, status)
            return
        end if
        n = size(names)
        if (present(count)) n = int(min(count, int(n, int64)))
        ! The list is consed up as entries are read: the last read first.
        order = [(k, k=n, 1, -1)]
        ! Sorted with string-lessp (by character codes): an insertion sort
        ! of (nreverse list).
        if (sorted) then
            order = [(k, k=1, n)]
            do k = 2, n
                key = order(k)
                j = k - 1
                do while (j >= 1)
                    if (.not. lessp(names(key)%s, names(order(j))%s)) exit
                    order(j + 1) = order(j)
                    j = j - 1
                end do
                order(j + 1) = key
            end do
        end if
        allocate (elems(n))
        do k = 1, n
            if (absolute) then
                elems(k) = make_string(file_name_as_directory(dirfilename)//names(order(k))%s)
            else
                elems(k) = make_string(names(order(k))%s)
            end if
        end do
        files = make_list(elems)
    end subroutine directory_files

    !> Whether `a' sorts before `b', comparing bytes (UTF-8 sorts as its
    !> code points do) with a prefix first.  As fns.c:Fstring_lessp.
    pure logical function lessp(a, b)
        character(len=*), intent(in) :: a, b
        integer :: k
        do k = 1, min(len(a), len(b))
            if (a(k:k) /= b(k:k)) then
                lessp = iachar(a(k:k)) < iachar(b(k:k))
                return
            end if
        end do
        lessp = len(a) < len(b)
    end function lessp

    !> Complete file name FILE in directory DIRECTORY.
    !> Returns the longest string common to all file names in DIRECTORY
    !> that start with FILE.  If there is only one and FILE matches it
    !> exactly, returns t.  Returns nil if DIRECTORY contains no name
    !> starting with FILE.
    !>
    !> If PREDICATE is present, call it with each possible completion (in
    !> absolute form) and ignore it if PREDICATE returns false.
    !>
    !> This function ignores some of the possible completions as
    !> determined by the variable `completion_ignored_extensions'.  Port of
    !> dired.c:Ffile_name_completion.
    subroutine file_name_completion(file, directory, result, status, predicate)
        character(len=*), intent(in) :: file, directory
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        call file_name_completion_1(file, directory, .false., result, status, predicate)
    end subroutine file_name_completion

    !> Return a list of all completions of file name FILE in directory
    !> DIRECTORY.  These are all file names in directory DIRECTORY which
    !> begin with FILE.  Port of dired.c:Ffile_name_all_completions.
    subroutine file_name_all_completions(file, directory, result, status)
        character(len=*), intent(in) :: file, directory
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        call file_name_completion_1(file, directory, .true., result, status)
    end subroutine file_name_all_completions

    !> Whether `name' ends in one of `completion_ignored_extensions' (for
    !> a directory, one that ends in a slash).
    logical function ignored_extension(name, directoryp) result(ignored)
        character(len=*), intent(in) :: name
        logical, intent(in) :: directoryp
        character(len=:), allocatable :: ext
        integer(int64) :: skip, elen
        integer :: k
        ignored = .false.
        do k = 1, size(completion_ignored_extensions)
            ext = trim(completion_ignored_extensions(k))
            if (len(ext) == 0) cycle
            if (directoryp) then
                ! Directories match the extensions that end in a slash.
                if (ext(len(ext):len(ext)) /= "/") cycle
                ext = ext(1:len(ext) - 1)
                if (len(ext) == 0) cycle
            end if
            elen = schars(ext)
            skip = schars(name) - elen
            if (skip < 0) cycle
            if (compare_strings(name, ext, completion_ignore_case%value, start1=skip) == COMPARE_EQUAL) then
                ignored = .true.
                return
            end if
        end do
    end function ignored_extension

    !> Port of dired.c:file_name_completion (no completion-regexp-list or
    !> decomposed file name encodings).
    subroutine file_name_completion_1(file, dirname, all_flag, result, status, predicate)
        character(len=*), intent(in) :: file, dirname
        logical, intent(in) :: all_flag
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        type(sys_string_type), allocatable :: names(:)
        type(value_type), allocatable :: all(:)
        character(len=:), allocatable :: dir, dirfilename, name, bestmatch
        integer(int64) :: bestmatchsize, nfile, compare, cmp, matchsize, nname
        integer(int32) :: err
        integer :: k, matchcount, nall
        logical :: directoryp, canexclude, includeall, have_best
        result = nil()
        call expand_file_name(dirname, dir, status)
        if (.not. status%ok()) return
        dir = file_name_as_directory(dir)
        dirfilename = directory_file_name(dir)
        err = emacs_directory_entries(dirfilename, names)
        if (err /= 0) then
            call report_file_errno("Opening directory", dirfilename, err, status)
            return
        end if
        ! If not INCLUDEALL, exclude files in completion-ignored-extensions
        ! as well as "." and "..".  Until shown otherwise, assume we can't
        ! exclude anything.
        includeall = .true.
        have_best = .false.
        bestmatchsize = 0
        matchcount = 0
        nfile = schars(file)
        allocate (all(size(names)))
        nall = 0
        do k = 1, size(names)
            name = names(k)%s
            if (schars(name) < nfile) cycle
            if (compare_strings(name, file, completion_ignore_case%value, end1=nfile) /= COMPARE_EQUAL) cycle
            directoryp = file_directory_p(dir//name)
            ! If all_flag is set, always include all.  It would not actually
            ! be helpful to the user to ignore any possible completions when
            ! making a list of them.
            if (.not. all_flag) then
                ! "." and ".." are never interesting as completions, and are
                ! actually in the way in a directory with only one file.
                canexclude = .false.
                if (directoryp .and. (name == "." .or. name == "..")) then
                    canexclude = .true.
                else if (len(name) > len(file)) then
                    canexclude = ignored_extension(name, directoryp)
                end if
                ! We're not including all files and this file can be
                ! excluded.
                if (.not. includeall .and. canexclude) cycle
                if (includeall .and. .not. canexclude) then
                    ! If we have one non-excludable file, we want to exclude
                    ! the excludable files.
                    includeall = .false.
                    ! Throw away any previous excludable match found.
                    have_best = .false.
                    bestmatchsize = 0
                    matchcount = 0
                end if
            end if
            ! This completion is a directory; make it end with '/'.
            if (directoryp) name = file_name_as_directory(name)
            ! Test the predicate, if any.
            if (present(predicate)) then
                if (.not. predicate(make_string(name))) cycle
            end if
            ! Suitably record this match.
            if (matchcount <= 1) matchcount = matchcount + 1
            nname = schars(name)
            if (all_flag) then
                nall = nall + 1
                all(nall) = make_string(name)
            else if (.not. have_best) then
                bestmatch = name
                bestmatchsize = nname
                have_best = .true.
            else
                compare = min(bestmatchsize, nname)
                cmp = compare_strings(bestmatch, name, completion_ignore_case%value, end1=compare, end2=compare)
                matchsize = compare
                if (cmp /= COMPARE_EQUAL) matchsize = abs(cmp) - 1
                if (completion_ignore_case%value) then
                    ! If this is an exact match except for case, use it as
                    ! the best match rather than one that is not an exact
                    ! match.  This way, we get the case pattern of the
                    ! actual match.
                    if ((matchsize == nname .and. matchsize + merge(1, 0, directoryp) < schars(bestmatch)) .or. &
                        ((matchsize == nname) .eqv. (matchsize + merge(1, 0, directoryp) == schars(bestmatch))) &
                        .and. compare_strings(name, file, end1=nfile) == COMPARE_EQUAL &
                        .and. compare_strings(bestmatch, file, end1=nfile) /= COMPARE_EQUAL) then
                        bestmatch = name
                    end if
                end if
                bestmatchsize = matchsize
                ! If the best completion so far is reduced to the string
                ! we're trying to complete, then we already know there's no
                ! other completion, so there's no point looking any further.
                if (matchsize <= nfile .and. .not. includeall .and. &
                    (.not. completion_ignore_case%value .or. matchsize == 0) .and. matchcount > 1) exit
            end if
        end do
        if (all_flag) then
            ! Upstream conses the matches, so the list is in reverse order.
            result = make_list(all(nall:1:-1))
            return
        end if
        if (.not. have_best) return
        ! Return t if the supplied string is an exact match (counting
        ! case); it does not require any change to be made.
        if (matchcount == 1 .and. bestmatch == file .and. len(bestmatch) == len(file)) then
            result = t_value()
            return
        end if
        result = make_string(substring(bestmatch, 0_int64, bestmatchsize))
    end subroutine file_name_completion_1

    !> Return a list of user names currently registered in the system.
    !> If we don't know how to determine that on this platform, just
    !> return a list with one element, taken from `user_real_login_name'.
    !> Port of dired.c:Fsystem_users.
    subroutine system_users(users)
        type(value_type), intent(out) :: users
        type(sys_string_type), allocatable :: names(:)
        type(value_type), allocatable :: elems(:)
        character(len=:), allocatable :: me
        logical :: found
        integer :: k, n
        call system_user_names(names)
        n = size(names)
        if (n == 0) then
            ! At least current user is always known.
            call passwd_name_of_user(me, found)
            allocate (elems(1))
            elems(1) = make_string(me)
            users = make_list(elems)
            return
        end if
        allocate (elems(n))
        ! Consed: the last entry first.
        do k = 1, n
            elems(k) = make_string(names(n + 1 - k)%s)
        end do
        users = make_list(elems)
    end subroutine system_users

    !> Return a list of user group names currently registered in the system.
    !> The value may be empty if not supported on this platform.  Port of
    !> dired.c:Fsystem_groups.
    subroutine system_groups(groups)
        type(value_type), intent(out) :: groups
        type(sys_string_type), allocatable :: names(:)
        type(value_type), allocatable :: elems(:)
        integer :: k, n
        call system_group_names(names)
        n = size(names)
        allocate (elems(n))
        do k = 1, n
            elems(k) = make_string(names(n + 1 - k)%s)
        end do
        groups = make_list(elems)
    end subroutine system_groups

    !> The owner and group of FILENAME (not following a symbolic link) as
    !> names, as `file_attributes' gives them with ID-FORMAT `string': a
    !> number in decimal when the id has no name.  FOUND is false if the
    !> file does not exist.  Part of dired.c:Ffile_attributes.
    subroutine file_attribute_names(filename, user, group, found)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: user, group
        logical, intent(out) :: found
        type(file_stat_type) :: st
        character(len=12) :: num
        logical :: named
        user = ""
        group = ""
        call file_attributes(filename, st, found, nofollow=.true.)
        if (.not. found) return
        call user_name_of_uid(st%uid, user, named)
        if (.not. named) then
            write (num, "(i0)") st%uid
            user = trim(num)
        end if
        call group_name_of_gid(st%gid, group, named)
        if (.not. named) then
            write (num, "(i0)") st%gid
            group = trim(num)
        end if
    end subroutine file_attribute_names
end module dired_fm
