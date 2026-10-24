! files_read_fm.f90 -- reading file names for files.el commands.
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

!> Part of files_fm: reading file names and `find_file_read_only'.  A
!> submodule can use minibuffer_fm even though it
!> depends on files_fm, which a module cannot.
submodule(files_fm) files_read_fm
    use modes_fm, only: minor_modes
    use minibuffer_fm, only: read_file_name, REQUIRE_MATCH_NIL, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION, &
                             read_file_name_completion_ignore_case
    use fns_fm, only: compare_strings, COMPARE_EQUAL
    implicit none(type, external)

contains

    !> Convert FILENAME to be relative to DIRECTORY (default:
    !> `default_directory').
    !> This function returns a relative file name that is equivalent to
    !> FILENAME when used with that default directory as the default.
    !> If FILENAME is a relative file name, it will be interpreted as
    !> existing in `default_directory'.  Port of files.el:file-relative-name
    !> (no remote files; case is folded when
    !> `read_file_name_completion_ignore_case' is true).
    module procedure file_relative_name
    character(len=:), allocatable :: dir, file, file_dir, ancestor, base, rest
    type(status_type) :: st
    logical :: fold
    if (present(directory)) then
        base = directory
    else
        base = ""
        if (cur /= 0) then
            if (allocated(buffers(cur)%directory)) base = buffers(cur)%directory
        end if
    end if
    call expand_file_name(base, dir, st)
    dir = file_name_as_directory(dir)
    call expand_file_name(filename, file, st)
    fold = read_file_name_completion_ignore_case
    ancestor = "."
    file_dir = file_name_as_directory(file)
    do while (.not. (prefix_p(dir, file_dir) .or. prefix_p(dir, file)))
        dir = file_name_directory(dir(1:len(dir) - 1))
        if (ancestor == ".") then
            ancestor = ".."
        else
            ancestor = "../"//ancestor
        end if
    end do
    ! Now ancestor is empty, or .., or ../.., etc.
    if (prefix_p(dir, file)) then
        ! We matched within FILENAME's directory part.  Add the rest of
        ! FILENAME onto ANCESTOR.
        rest = file(len(dir) + 1:)
        if (ancestor == "." .and. len(rest) > 0) then
            ! But don't bother with ANCESTOR if it would give us `./'.
            rel = rest
        else
            rel = file_name_as_directory(ancestor)//rest
        end if
    else
        ! We matched FILENAME's directory equivalent.
        rel = ancestor
    end if
contains

    !> subr.el:string-prefix-p.
    logical function prefix_p(prefix, string)
        character(len=*), intent(in) :: prefix, string
        prefix_p = .false.
        if (len(string) < len(prefix)) return
        if (fold) then
            prefix_p = compare_strings(prefix, string(1:len(prefix)), ignore_case=.true.) == COMPARE_EQUAL
        else
            prefix_p = string(1:len(prefix)) == prefix
        end if
    end function prefix_p
    end procedure file_relative_name

    recursive module subroutine read_file_name_in_minibuffer(prompt, result, status, dir, &
                                                             default_filename, initial, confirm_nonexistent)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: dir, default_filename, initial
        logical, intent(in), optional :: confirm_nonexistent
        integer :: mustmatch
        mustmatch = REQUIRE_MATCH_NIL
        if (present(confirm_nonexistent)) then
            if (confirm_nonexistent) mustmatch = REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION
        end if
        call read_file_name(prompt, result, status, dir, default_filename, mustmatch=mustmatch, initial=initial)
    end subroutine read_file_name_in_minibuffer

    !> Visit FILENAME with FUN (`find_file' or `find_file_other_window')
    !> and make its buffer read-only with `read_only_mode'.  Port of
    !> files.el:find-file--read-only (no wildcards).
    recursive subroutine find_file_read_only_1(other_window, filename, status)
        logical, intent(in) :: other_window
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. file_exists_p(filename)) then
            call set_error(status, FM_GENERIC_ERROR, filename//" does not exist")
            return
        end if
        if (other_window) then
            call find_file_other_window(filename, status)
        else
            call find_file(filename, status)
        end if
        if (.not. status%ok()) return
        ! (with-current-buffer b (read-only-mode 1))
        call minor_modes%read_only_mode%enable(status)
    end subroutine find_file_read_only_1

    recursive module subroutine find_file_read_only(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        call find_file_read_only_1(.false., filename, status)
    end subroutine find_file_read_only

    recursive module subroutine find_file_read_only_other_window(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        call find_file_read_only_1(.true., filename, status)
    end subroutine find_file_read_only_other_window

    !> Edit file FILENAME but don't allow changes.
    !> Like \[find_file], but marks buffer as read-only.
    !> Use \[read_only_mode] to permit editing.  Port of
    !> files.el:find-file-read-only (interactive), with find-file-read-args.
    recursive subroutine cmd_find_file_read_only(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, dir
        dir = "~/"
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        call read_file_name_in_minibuffer("Find file read-only: ", filename, status, default_filename=dir, &
                                          confirm_nonexistent=.true.)
        if (.not. status%ok()) return
        call find_file_read_only(filename, status)
    end subroutine cmd_find_file_read_only

    !> Edit file FILENAME in another window but don't allow changes.
    !> Like \[find_file_other_window], but marks buffer as read-only.
    !> Use \[read_only_mode] to permit editing.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of files.el:find-file-read-only-other-window (interactive), with
    !> find-file-read-args.
    recursive subroutine cmd_find_file_read_only_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, dir
        dir = "~/"
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        call read_file_name_in_minibuffer("Find file read-only other window: ", filename, status, &
                                          default_filename=dir, confirm_nonexistent=.true.)
        if (.not. status%ok()) return
        call find_file_read_only_other_window(filename, status)
    end subroutine cmd_find_file_read_only_other_window

    module subroutine syms_of_files_read(ctl_x_map, ctl_x_4_map)
        type(handle_type), intent(in) :: ctl_x_map
        type(handle_type), intent(in), optional :: ctl_x_4_map
        type(value_type) :: key(1)
        type(status_type) :: status
        commands%find_file_read_only%run => cmd_find_file_read_only
        commands%find_file_read_only_other_window%run => cmd_find_file_read_only_other_window
        ! files.el: (define-key ctl-x-map "\C-r" 'find-file-read-only)
        key(1) = make_fixnum(18_int64)
        call define_key(ctl_x_map, key, sym("find_file_read_only"), status)
        ! files.el: (define-key ctl-x-map "\C-q" 'read-only-mode)
        key(1) = make_fixnum(17_int64)
        call define_key(ctl_x_map, key, sym("read_only_mode"), status)
        if (present(ctl_x_4_map)) then
            ! files.el: (define-key ctl-x-4-map "r" 'find-file-read-only-other-window)
            key(1) = make_fixnum(int(iachar("r"), int64))
            call define_key(ctl_x_4_map, key, sym("find_file_read_only_other_window"), status)
        end if
    end subroutine syms_of_files_read
end submodule files_read_fm
