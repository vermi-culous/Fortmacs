! dired_aux_fm.f90 -- less commonly used parts of Dired, a submodule of
! dired_mode_fm.
! Ported from GNU Emacs 31.1 lisp/dired-aux.el.
!
! Copyright (C) 1985-1986, 1992, 1994, 1998, 2000-2026 Free Software
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

!> The less commonly used parts of Dired: creating directories and empty
!> files (`+'), redisplaying files (`l'), adding, updating and removing
!> file lines (`k'), the subdirectory functions (`dired_goto_subdir',
!> `dired_kill_subdir', `dired_mark_subdir_files', `dired_prev_subdir'),
!> and the operations on marked files: copying, renaming and linking (C,
!> R, H, S, Y and their `%' regexp forms, `% u', `% l') through
!> `dired_create_files' with the overwrite query (`dired_query'), changing
!> modes, owners, groups and times (M, O, G, T), compressing (Z, c),
!> viewing (v, dired.el's `dired_view_file') and file types (y).  A
!> submodule of dired_mode_fm, whose procedures they use and which uses
!> them; dired-aux.el's options are declared in dired_mode_fm.
!>
!> Deferred: shell commands (`!', `&', `X', `dired_do_shell_command',
!> `dired_do_async_shell_command', the guessing of commands,
!> `dired_do_open' and `E': the guessing of commands needs mailcap.el and
!> xdg.el, asynchronous shell commands are not ported), printing (`P'), diffs (`=',
!> `dired_backup_diff', `dired_compare_directories': diff.el), searching
!> and replacing in marked files (`A', `Q', `dired_do_search',
!> `dired_do_query_replace_regexp', `dired_do_isearch': xref and
!> fileloop), `dired_clean_directory' (`.': numbered backups),
!> `dired_backup_overwrite' (false), `dired_vc_rename_file' and
!> `dired_vc_next_action' (VC), HOW-TO functions of
!> `dired_do_create_files' and `dired_copy_how_to_fn' (archives),
!> inserting and hiding subdirectories (`i', `$', `M-$'),
!> `dired_isearch_filenames', the further defaults of
!> `dired_dwim_target_defaults' (offered by M-n: `read_file_name' takes
!> one default) and a procedure as `dired_dwim_target', the error data in
!> the log of a failed file operation (its name and message are logged,
!> not the condition's data list), file name handlers.  Loading and
!> byte-compiling Lisp files (`L', `B') are not applicable.
submodule(dired_mode_fm) dired_aux_fm
    use lisp_fm, only: make_list, xfixnum
    use error_fm, only: FM_FILE_ERROR, FM_FILE_DATE_ERROR
    use sysdep_fm, only: emacs_localtime, S_IFMT, S_IFDIR, S_IFLNK
    use buffer_fm, only: buffer_list, rename_buffer
    use fileio_fm, only: rename_file, copy_file, add_name_to_file, make_symbolic_link, file_symlink_target, &
                         file_executable_p, file_modes, set_file_modes, directory_name_p
    use files_fm, only: copy_directory, file_in_directory_p, make_empty_file, file_modes_number_to_symbolic, &
                        file_modes_symbolic_to_number, set_visited_file_name
    use dired_fm, only: system_users, system_groups, file_attribute_names
    use minibuf_fm, only: read_from_minibuffer, completion_list_table
    use minibuffer_fm, only: completing_read
    use subr_fm, only: y_or_n_p, read_char_choice, shell_quote_argument
    use casefiddle_fm, only: capitalize_string, upcase_string, downcase_string
    use window_fm, only: pop_to_buffer, same_handle
    use view_fm, only: view_file
    use keyboard_fm, only: sit_for
    use print_fm, only: fortran_repr
    implicit none(type, external)

    !> File names `dired_create_files' records as failing (and
    !> `dired_copy_file_recursive' too).  Port of dired-aux.el
    !> dired-create-files-failures.
    type(dired_string_type), allocatable :: create_files_failures(:)

    !> The results of the body of `dired_map_over_marks_check'.
    type(dired_string_type), allocatable :: check_results(:)

    !> The request and the answer of the readers that
    !> `dired_mark_pop_up_body' runs.
    character(len=:), allocatable :: pop_prompt, pop_dir, pop_default, pop_result
    logical :: pop_has_default = .false., pop_used_default = .false., pop_answer = .false.
    type(value_type) :: pop_collection

    !> The target of `dired_do_create_files' (upstream's fluid `target').
    character(len=:), allocatable :: create_target

    !> The state of `dired_do_create_files_regexp''s name constructor
    !> (`rename_regexp_query' and the arguments it closes over).
    character(len=:), allocatable :: regexp_operation, regexp_regexp, regexp_newname, regexp_help
    logical :: regexp_whole_name = .false.
    integer :: regexp_query = 0

    !> The state of `dired_rename_non_directory''s name constructor
    !> (`rename_non_directory_query' and what it closes over).
    character(len=:), allocatable :: non_directory_operation, non_directory_help
    logical :: non_directory_upcase = .false.
    integer :: non_directory_query = 0

contains

    !> Return the topmost nonexistent parent dir of FILENAME.
    !> FILENAME is a full file name.  Port of
    !> dired-aux.el:dired--find-topmost-parent-dir.
    function find_topmost_parent_dir(filename) result(new)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: new, try
        try = filename
        new = ""
        do while (len(try) > 0)
            if (file_exists_p(try)) exit
            if (new == try .and. len(new) == len(try)) exit
            new = try
            try = directory_file_name(file_name_directory(try))
        end do
    end function find_topmost_parent_dir

    recursive module subroutine dired_create_directory(directory, status)
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: expanded, new
        integer(int64) :: pos
        call expand_file_name(directory, expanded, status)
        if (.not. status%ok()) return
        expanded = directory_file_name(expanded)
        if (file_exists_p(expanded)) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot create directory "//expanded//": file exists")
            return
        end if
        new = find_topmost_parent_dir(expanded)
        call make_directory(expanded, status, parents=.true.)
        if (.not. status%ok()) return
        if (len(new) > 0) then
            call dired_add_file(new, status)
            if (.not. status%ok()) return
            pos = dired_move_to_filename(status)
        end if
    end subroutine dired_create_directory

    !> Create a directory called DIRECTORY.
    !> Parent directories of DIRECTORY are created as needed.
    !> If DIRECTORY already exists, signal an error.  Port of
    !> dired-aux.el:dired-create-directory (interactive).
    recursive subroutine cmd_dired_create_directory(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory, dir
        dir = dired_current_directory(status)
        if (.not. status%ok()) return
        call read_file_name("Create directory: ", directory, status, dir=dir)
        if (.not. status%ok()) return
        call dired_create_directory(directory, status)
    end subroutine cmd_dired_create_directory

    recursive module subroutine dired_add_file(filename, status, marker_char)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        character, intent(in), optional :: marker_char
        type(handle_type), allocatable :: bufs(:)
        type(handle_type) :: old
        type(status_type) :: st
        logical :: added
        integer :: k
        status = status_type()
        ! dired-fun-in-all-buffers
        bufs = dired_buffers_for_dir(file_name_directory(filename), file_name_nondirectory(filename))
        old = current_buffer()
        do k = 1, size(bufs)
            call set_buffer(bufs(k), st)
            call dired_add_entry(filename, added, status, marker_char)
            if (.not. status%ok()) exit
        end do
        call set_buffer(old, st)
    end subroutine dired_add_file

    recursive module subroutine dired_add_entry(filename, added, status, marker_char, relative)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: added
        type(status_type), intent(out) :: status
        character, intent(in), optional :: marker_char
        logical, intent(in), optional :: relative
        character(len=:), allocatable :: file, cur_dir, directory, saved_dir, inserted_name, reason
        type(dired_string_type) :: list1(1)
        type(value_type) :: saved_ro, props
        type(status_type) :: st
        integer(int64) :: opoint, p, line_start, pt
        logical :: rel, found, changed
        character :: saved_marker
        status = status_type()
        added = .false.
        rel = .false.
        if (present(relative)) rel = relative
        ! Entry is always for files, even if they happen to also be
        ! directories.
        file = directory_file_name(filename)
        opoint = point()
        cur_dir = dired_current_directory(status)
        if (.not. status%ok()) return
        if (rel) then
            directory = cur_dir
            file = file_relative_name(file, directory)
        else
            directory = file_name_directory(file)
            file = file_name_nondirectory(file)
        end if
        reason = ""
        if (directory == cur_dir) then
            call end_of_line()
            ! (Hidden subdirectories are deferred.)
            ! We are already where we should be, except when point is
            ! before the subdir line or its total line.
            p = dired_after_subdir_garbage(cur_dir, status)
            if (.not. status%ok()) return
            if (point() < p) call goto_char(p)
        else if (dired_goto_subdir(directory) /= 0) then
            ! found - skip subdir and `total' line and uninteresting files
            ! like . and ..  This better not move into the next subdir!
            call dired_goto_next_nontrivial_file()
        else
            ! not found
            reason = "Subdir not found"
        end if
        if (len(reason) == 0) then
            call aux_inhibit_read_only_begin(saved_ro)
            call beginning_of_line()
            line_start = point()
            ! Don't expand `.'.  Show just the file name within directory.
            saved_dir = aux_default_directory()
            buffers(cur)%directory = directory
            list1(1)%s = file
            call dired_insert_directory(directory, dired_actual_switches_of()//" -d", status, file_list=list1)
            buffers(cur)%directory = saved_dir
            if (.not. status%ok()) then
                call aux_inhibit_read_only_end(saved_ro)
                return
            end if
            call goto_char(line_start)
            ! Put in desired marker char.
            if (present(marker_char)) then
                saved_marker = dired_marker_char
                dired_marker_char = marker_char
                call dired_mark_1(1_int64, .false., status)
                dired_marker_char = saved_marker
            end if
            ! Compensate for a bug in ange-ftp: it inserts the file's
            ! absolute name, rather than the relative one.
            call goto_char(line_start)
            call dired_get_filename(inserted_name, found, status, DIRED_LOCALP_VERBATIM)
            if (status%ok() .and. found) then
                if (len(file_name_directory(inserted_name)) > 0) then
                    call end_of_line()
                    call goto_char(point() - len_chars(inserted_name))
                    call text_properties_at(point(), props, st)
                    call delete_char(len_chars(inserted_name), st)
                    pt = point()
                    call insert(file, st)
                    call set_text_properties(pt, point(), props, changed, st)
                    call goto_char(point() + 1)
                else
                    call aux_fwd_line(1_int64)
                end if
            end if
            call aux_fwd_line(-1_int64)
            ! The subdir-alist is not affected, so we can run the hook right
            ! now (narrowed to the new line).
            call run_after_readin_on_line()
            p = dired_move_to_filename(st)
            call aux_inhibit_read_only_end(saved_ro)
            added = status%ok()
        end if
        ! don't move away on failure
        if (len(reason) > 0) call goto_char(opoint)
    contains

        integer(int64) function len_chars(s)
            character(len=*), intent(in) :: s
            integer :: j
            len_chars = 0
            do j = 1, len(s)
                if (iand(ichar(s(j:j)), 192) /= 128) len_chars = len_chars + 1
            end do
        end function len_chars
    end subroutine dired_add_entry

    !> Run `dired_after_readin_hook' with the buffer narrowed to the
    !> current line.
    subroutine run_after_readin_on_line()
        integer(int64) :: opoint, begv, zv
        type(status_type) :: st
        opoint = point()
        begv = buffers(cur)%begv
        zv = buffers(cur)%zv
        call beginning_of_line()
        call narrow_to_region(point(), aux_bol(2_int64), st)
        call dired_after_readin_hook%run(st)
        call widen()
        call narrow_to_region(begv, min(zv, point_max()), st)
        call goto_char(opoint)
    end subroutine run_after_readin_on_line

    module function dired_after_subdir_garbage(dir, status) result(pos)
        character(len=*), intent(in) :: dir
        type(status_type), intent(out) :: status
        integer(int64) :: pos
        integer(int64) :: opoint
        type(status_type) :: st
        status = status_type()
        opoint = point()
        pos = 0
        if (dired_goto_subdir(dir) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "This cannot happen")
            call goto_char(opoint)
            return
        end if
        call aux_fwd_line(1_int64)
        ! don't cross subdir boundary
        do while (.not. eolp())
            if (dired_move_to_filename(st) /= 0) exit
            call aux_fwd_line(1_int64)
        end do
        pos = point()
        call goto_char(opoint)
    end function dired_after_subdir_garbage

    recursive module subroutine dired_do_redisplay(arg_kind, arg_n, test_for_subdir, status)
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        logical, intent(in) :: test_for_subdir
        type(status_type), intent(out) :: status
        integer(int64) :: p
        logical :: found, upward
        type(status_type) :: st
        status = status_type()
        if (test_for_subdir .and. len(dired_get_subdir()) > 0) then
            call set_error(status, FM_GENERIC_ERROR, "Redisplaying a subdirectory (dired_insert_subdir) is not "// &
                           "supported yet")
            return
        end if
        ! message much faster than making dired-map-over-marks show
        ! progress
        call message("Redisplaying...")
        call dired_map_over_marks(redisplay_line, arg_kind, arg_n, status, found, upward)
        if (.not. status%ok()) return
        call dired_after_readin_hook%run(status)
        p = dired_move_to_filename(st)
        call message("Redisplaying...done")
    end subroutine dired_do_redisplay

    !> The body of `dired_do_redisplay' (postponing the readin hook till
    !> we map over all marked files).
    recursive subroutine redisplay_line(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: fname
        logical :: found
        call dired_get_filename(fname, found, status, no_error_if_not_filep=.true.)
        if (.not. status%ok()) return
        if (.not. found) then
            call set_error(status, FM_GENERIC_ERROR, "No file on this line")
            return
        end if
        call message("Redisplaying... "//fname)
        call dired_update_file_line(status, fname)
    end subroutine redisplay_line

    !> Redisplay all marked (or next ARG) files.
    !> If on a subdir line, redisplay that subdirectory.  In that case,
    !> a prefix arg lets you edit the `ls' switches used for the new listing.
    !>
    !> Dired remembers switches specified with a prefix arg, so that reverting
    !> the buffer will not reset them.  However, using `dired_undo' to re-insert
    !> or delete subdirectories can bypass this machinery.  Hence, you sometimes
    !> may have to reset some subdirectory switches after a `dired_undo'.
    !> Port of dired-aux.el:dired-do-redisplay (interactive; redisplaying
    !> a subdirectory is deferred).
    recursive subroutine cmd_dired_do_redisplay(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_do_redisplay(kind, n, .true., status)
    end subroutine cmd_dired_do_redisplay

    recursive module subroutine dired_update_file_line(status, file)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file
        type(value_type) :: saved_ro
        integer(int64) :: opoint, p
        character :: c
        logical :: added, had_char
        type(status_type) :: st
        status = status_type()
        opoint = aux_bol()
        had_char = opoint < point_max()
        c = aux_char_at(opoint)
        ! (buffer-read-only nil)
        call aux_inhibit_read_only_begin(saved_ro)
        call goto_char(opoint)
        call aux_fwd_line(1_int64)
        call delete_region(opoint, point(), status)
        if (status%ok() .and. present(file)) then
            call dired_add_entry(file, added, status, relative=.true.)
            ! Replace space by old marker without moving point.
            if (status%ok() .and. had_char .and. c /= achar(0)) then
                call subst_char_in_region(opoint, opoint + 1, iachar(" "), iachar(c), status)
            end if
        end if
        call aux_inhibit_read_only_end(saved_ro)
        p = dired_move_to_filename(st)
    end subroutine dired_update_file_line

    module function dired_goto_subdir(dir) result(pos)
        character(len=*), intent(in) :: dir
        integer(int64) :: pos
        character(len=:), allocatable :: d
        integer :: k, i
        pos = 0
        d = file_name_as_directory(dir)
        k = aux_local_index()
        if (k == 0) return
        if (.not. allocated(locals(k)%subdir_alist)) return
        do i = 1, size(locals(k)%subdir_alist)
            if (locals(k)%subdir_alist(i)%dir == d) then
                call goto_char(aux_mpos(locals(k)%subdir_alist(i)%marker))
                ! dired-subdir-hidden-p and dired-add-entry depend on point
                ! being at \n after this function succeeds.
                call end_of_line()
                pos = point()
                return
            end if
        end do
    end function dired_goto_subdir

    recursive module subroutine dired_kill_subdir(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cur_dir
        type(value_type) :: saved_ro
        type(dired_subdir_type), allocatable :: kept(:)
        integer(int64) :: beg, end
        logical :: modflag
        integer :: k, i, n
        beg = dired_subdir_min(status)
        if (.not. status%ok()) return
        end = dired_subdir_max()
        call buffer_modified_p(current_buffer(), modflag, status)
        if (.not. status%ok()) return
        cur_dir = dired_current_directory(status)
        if (.not. status%ok()) return
        if (cur_dir == aux_expand(aux_default_directory())) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to kill top level directory")
            return
        end if
        call aux_inhibit_read_only_begin(saved_ro)
        call delete_region(beg, end, status)
        ! don't leave final blank line
        if (status%ok() .and. eobp()) call delete_char(-1_int64, status)
        call aux_inhibit_read_only_end(saved_ro)
        ! dired-unsubdir
        k = aux_local_index()
        if (k > 0) then
            n = 0
            allocate (kept(size(locals(k)%subdir_alist)))
            do i = 1, size(locals(k)%subdir_alist)
                if (locals(k)%subdir_alist(i)%dir == cur_dir) then
                    call aux_release_marker(locals(k)%subdir_alist(i)%marker)
                    cur_dir = char(0)
                else
                    n = n + 1
                    kept(n) = locals(k)%subdir_alist(i)
                end if
            end do
            deallocate (locals(k)%subdir_alist)
            allocate (locals(k)%subdir_alist(n))
            locals(k)%subdir_alist(:) = kept(1:n)
        end if
        call restore_buffer_modified_p(modflag)
    end subroutine dired_kill_subdir

    !> Remove all lines of current subdirectory.
    !> Lower levels are unaffected.  Port of dired-aux.el:dired-kill-subdir
    !> (interactive).
    recursive subroutine cmd_dired_kill_subdir(status)
        type(status_type), intent(out) :: status
        call dired_kill_subdir(status)
    end subroutine cmd_dired_kill_subdir

    !> The start of the current subdirectory.  Port of
    !> dired.el:dired-subdir-min.
    integer(int64) function dired_subdir_min(status) result(pos)
        type(status_type), intent(out) :: status
        integer(int64) :: opoint
        opoint = point()
        pos = dired_prev_subdir(0_int64, status, .true., .true.)
        if (status%ok() .and. pos == 0) call set_error(status, FM_GENERIC_ERROR, "Not in a subdir!")
        call goto_char(opoint)
    end function dired_subdir_min

    recursive module subroutine dired_mark_subdir_files(status)
        type(status_type), intent(out) :: status
        integer(int64) :: p_min
        p_min = dired_subdir_min(status)
        if (.not. status%ok()) return
        call dired_mark_files_in_region(p_min, dired_subdir_max(), status)
    end subroutine dired_mark_subdir_files

    !> Mark all files except `.' and `..' in current subdirectory.
    !> If the Dired buffer shows multiple directories, this command
    !> marks the files listed in the subdirectory that point is in.  Port
    !> of dired-aux.el:dired-mark-subdir-files (interactive).
    recursive subroutine cmd_dired_mark_subdir_files(status)
        type(status_type), intent(out) :: status
        call dired_mark_subdir_files(status)
    end subroutine cmd_dired_mark_subdir_files

    module function dired_prev_subdir(arg, status, no_error_if_not_found, no_skip) result(pos)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_error_if_not_found, no_skip
        integer(int64) :: pos
        pos = dired_next_subdir(-arg, status, no_error_if_not_found, no_skip)
    end function dired_prev_subdir

    module subroutine syms_of_dired_aux()
        commands%dired_create_directory%run => cmd_dired_create_directory
        commands%dired_do_redisplay%run => cmd_dired_do_redisplay
        commands%dired_kill_subdir%run => cmd_dired_kill_subdir
        commands%dired_mark_subdir_files%run => cmd_dired_mark_subdir_files
        commands%dired_do_chmod%run => cmd_dired_do_chmod
        commands%dired_do_chgrp%run => cmd_dired_do_chgrp
        commands%dired_do_chown%run => cmd_dired_do_chown
        commands%dired_do_touch%run => cmd_dired_do_touch
        commands%dired_kill_line%run => cmd_dired_kill_line
        commands%dired_do_kill_lines%run => cmd_dired_do_kill_lines
        commands%dired_do_compress%run => cmd_dired_do_compress
        commands%dired_do_compress_to%run => cmd_dired_do_compress_to
        commands%dired_do_copy%run => cmd_dired_do_copy
        commands%dired_do_symlink%run => cmd_dired_do_symlink
        commands%dired_do_relsymlink%run => cmd_dired_do_relsymlink
        commands%dired_do_hardlink%run => cmd_dired_do_hardlink
        commands%dired_do_rename%run => cmd_dired_do_rename
        commands%dired_do_rename_regexp%run => cmd_dired_do_rename_regexp
        commands%dired_do_copy_regexp%run => cmd_dired_do_copy_regexp
        commands%dired_do_hardlink_regexp%run => cmd_dired_do_hardlink_regexp
        commands%dired_do_symlink_regexp%run => cmd_dired_do_symlink_regexp
        commands%dired_do_relsymlink_regexp%run => cmd_dired_do_relsymlink_regexp
        commands%dired_upcase%run => cmd_dired_upcase
        commands%dired_downcase%run => cmd_dired_downcase
        commands%dired_create_empty_file%run => cmd_dired_create_empty_file
        commands%dired_view_file%run => cmd_dired_view_file
        commands%dired_show_file_type%run => cmd_dired_show_file_type
        call init_dired_aux_options()
    end subroutine syms_of_dired_aux
    ! ------------------------------------------------------------------
    ! Marked-files prompts
    ! ------------------------------------------------------------------

    !> Run BODY (which reads input) after showing which files are marked,
    !> as `dired_mark_pop_up' does for its FUNCTION: the files FILES are
    !> listed in a window showing a buffer named BUFFER_NAME (" *Marked
    !> Files*" if empty), unless there is just one file (ONE_MARKED: a
    !> file that was marked, upstream's (t FILENAME), still shows it),
    !> `dired_no_confirm' has `all' set, or OP_SYMBOL is among its
    !> `commands'.  Port of dired.el:dired-mark-pop-up.
    recursive subroutine dired_mark_pop_up_body(buffer_name, op_symbol, files, one_marked, body, status)
        character(len=*), intent(in) :: buffer_name, op_symbol
        type(dired_string_type), intent(in) :: files(:)
        logical, intent(in) :: one_marked
        interface
            recursive subroutine body(status)
                import :: status_type
                implicit none(type, external)
                type(status_type), intent(out) :: status
            end subroutine body
        end interface
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, window, owindow, old
        type(display_alist_type) :: alist
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer :: saved_dedicated, k, nfiles
        logical :: quiet
        nfiles = size(files)
        if (one_marked) nfiles = 2
        quiet = dired_no_confirm%all .or. nfiles == 1
        if (allocated(dired_no_confirm%commands)) then
            do k = 1, size(dired_no_confirm%commands)
                if (trim(dired_no_confirm%commands(k)) == op_symbol) quiet = .true.
            end do
        end if
        if (quiet) then
            call body(status)
            return
        end if
        name = buffer_name
        if (len(name) == 0) name = " *Marked Files*"
        old = current_buffer()
        call temp_buffer_window_setup(name, buffer, status)
        if (.not. status%ok()) return
        ! Mark *Marked Files* window as softly-dedicated, to prevent other
        ! buffers e.g. *Completions* from reusing it (bug#17554).
        saved_dedicated = display_buffer_mark_dedicated
        display_buffer_mark_dedicated = DEDICATED_WEAK
        pop_up_files = files
        alist%window_height => fit_window
        alist%body_function => insert_marked_files
        call temp_buffer_window_show(buffer, window, status, [DISPLAY_ACTION_BELOW_SELECTED], alist)
        display_buffer_mark_dedicated = saved_dedicated
        call set_buffer(old, st)
        if (.not. status%ok()) return
        if (.not. window_live_p(window)) then
            call body(status)
            return
        end if
        owindow = selected_window()
        call select_window(window, st, norecord=.true.)
        call body(status)
        if (window_live_p(window)) call quit_restore_window(window, BK_KILL, st)
        if (window_live_p(owindow)) call select_window(owindow, st, norecord=.true.)
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine dired_mark_pop_up_body

    !> The reading `dired_mark_pop_up_body' runs: `read_file_name' with
    !> `pop_prompt', `pop_dir' and `pop_default'.
    recursive subroutine pop_read_file_name(status)
        type(status_type), intent(out) :: status
        if (pop_has_default) then
            call read_file_name(pop_prompt, pop_result, status, dir=pop_dir, default_filename=pop_default)
        else
            call read_file_name(pop_prompt, pop_result, status, dir=pop_dir)
        end if
    end subroutine pop_read_file_name

    !> `completing_read' with `pop_prompt', the collection `pop_collection'
    !> and `pop_default' (`pop_used_default' tells whether the default
    !> was returned for empty input).
    recursive subroutine pop_completing_read(status)
        type(status_type), intent(out) :: status
        if (pop_has_default) then
            call completing_read(pop_prompt, completion_list_table(pop_collection), pop_result, status, &
                                 default=pop_default, used_default=pop_used_default)
        else
            pop_used_default = .false.
            call completing_read(pop_prompt, completion_list_table(pop_collection), pop_result, status)
        end if
    end subroutine pop_completing_read

    !> `read_from_minibuffer' with `pop_prompt' and the default `pop_default'.
    recursive subroutine pop_read_from_minibuffer(status)
        type(status_type), intent(out) :: status
        pop_used_default = .false.
        if (pop_has_default) then
            call read_from_minibuffer(pop_prompt, pop_result, status, default=pop_default)
        else
            call read_from_minibuffer(pop_prompt, pop_result, status)
        end if
    end subroutine pop_read_from_minibuffer

    !> `y_or_n_p' with `pop_prompt' (the answer in `pop_answer').
    recursive subroutine pop_y_or_n_p(status)
        type(status_type), intent(out) :: status
        call y_or_n_p(pop_prompt, pop_answer, status)
    end subroutine pop_y_or_n_p

    !> PROMPT with its "%s" replaced by WHAT.
    function format_s(prompt, what) result(s)
        character(len=*), intent(in) :: prompt, what
        character(len=:), allocatable :: s
        integer :: k
        k = index(prompt, "%s")
        if (k == 0) then
            s = prompt
        else
            s = prompt(1:k - 1)//what//prompt(k + 2:)
        end if
    end function format_s

    !> Read args for a Dired marked-files command, prompting with PROMPT.
    !> RESULT is the user input (a string).
    !>
    !> OP_SYMBOL is an operation symbol (see `dired_no_confirm').
    !> ARG_KIND and ARG_N are normally the prefix argument for the calling
    !> command; they are passed to `dired_marker_prompt'.
    !> FILES should be a list of marked files' names.
    !>
    !> DEFAULT_VALUE, if present, is a default value, passed to
    !> `completing_read' (or, for `touch', `read_from_minibuffer');
    !> USED_DEFAULT tells whether it was returned for empty input.
    !>
    !> COLLECTION, if present, is a list of possible completions.  Port
    !> of dired-aux.el:dired-mark-read-string (no INITIAL).
    recursive subroutine dired_mark_read_string(prompt, op_symbol, arg_kind, arg_n, files, result, used_default, &
                                                status, default_value, collection)
        character(len=*), intent(in) :: prompt, op_symbol
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        type(dired_string_type), intent(in) :: files(:)
        character(len=:), allocatable, intent(out) :: result
        logical, intent(out) :: used_default
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: default_value
        type(value_type), intent(in), optional :: collection
        pop_prompt = format_s(prompt, dired_marker_prompt(arg_kind, arg_n, files))
        pop_has_default = present(default_value)
        if (pop_has_default) pop_default = default_value
        if (present(collection)) then
            pop_collection = collection
        else
            pop_collection = nil()
        end if
        if (op_symbol == "touch") then
            call dired_mark_pop_up_body("", op_symbol, files, .false., pop_read_from_minibuffer, status)
        else
            call dired_mark_pop_up_body("", op_symbol, files, .false., pop_completing_read, status)
        end if
        used_default = pop_used_default
        if (status%ok()) then
            result = pop_result
        else
            result = ""
        end if
    end subroutine dired_mark_read_string

    !> Read a file name for a marked-files command, perhaps popping up the
    !> list of marked files.  ARG is the prefix arg and indicates whether
    !> the files came from marks (DIRED_ARG_NIL) or a repeat factor.
    !> DEFAULT is the default value to return if the user just hits RET.
    !> Port of dired-aux.el:dired-mark-read-file-name.
    recursive subroutine dired_mark_read_file_name(prompt, dir, op_symbol, arg_kind, arg_n, files, result, status, &
                                                   default)
        character(len=*), intent(in) :: prompt, dir, op_symbol
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        type(dired_string_type), intent(in) :: files(:)
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: default
        pop_prompt = format_s(prompt, dired_marker_prompt(arg_kind, arg_n, files))
        pop_dir = dir
        pop_has_default = present(default)
        if (pop_has_default) pop_default = default
        call dired_mark_pop_up_body("", op_symbol, files, .false., pop_read_file_name, status)
        if (status%ok()) then
            result = pop_result
        else
            result = ""
        end if
    end subroutine dired_mark_read_file_name

    module procedure dired_query
    integer :: chars(7), c
    character(len=:), allocatable :: full
    answer = .false.
    status = status_type()
    if (state == iachar("!")) then
        ! accept, and don't ask again
        answer = .true.
        return
    end if
    if (state == iachar("q") .or. state == 27) then
        ! skip, and don't ask again
        return
    end if
    ! no previous answer - ask now
    chars = [iachar("y"), iachar(" "), iachar("n"), 127, iachar("!"), iachar("q"), 27]
    if (present(help_form)) then
        full = prompt//" [Type yn!q or C-h] "
        call read_char_choice(full, chars, c, status, help_form=help_form)
    else
        full = prompt//" [Type y, n, q or !] "
        call read_char_choice(full, chars, c, status)
    end if
    if (.not. status%ok()) return
    state = c
    answer = c == iachar("y") .or. c == iachar(" ") .or. c == iachar("!")
    end procedure dired_query

    !> Request confirmation from the user that the operation described
    !> by OP_SYMBOL is to be performed on the marked files.
    !> Confirmation consists in a y-or-n question with a file list
    !> pop-up unless OP_SYMBOL is in `dired_no_confirm'.
    !> The files used are determined by ARG (as in `dired_get_marked_files').
    !> Port of dired-aux.el:dired-mark-confirm.
    recursive subroutine dired_mark_confirm(op_symbol, arg_kind, arg_n, confirmed, status)
        character(len=*), intent(in) :: op_symbol
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        logical, intent(out) :: confirmed
        type(status_type), intent(out) :: status
        type(dired_string_type), allocatable :: files(:)
        logical :: one_marked
        integer :: k
        status = status_type()
        confirmed = .true.
        if (dired_no_confirm%all) return
        if (allocated(dired_no_confirm%commands)) then
            do k = 1, size(dired_no_confirm%commands)
                if (trim(dired_no_confirm%commands(k)) == op_symbol) return
            end do
        end if
        ! Pass t for DISTINGUISH-ONE-MARKED so that a single file which
        ! is marked pops up a window.  That will help the user see
        ! it isn't the current line file.
        call dired_get_marked_files(files, status, DIRED_LOCALP_T, arg_kind, arg_n, distinguish_one_marked=.true., &
                                    one_marked=one_marked, error="")
        if (.not. status%ok()) return
        pop_prompt = operation_string(op_symbol)//" "//dired_marker_prompt(arg_kind, arg_n, files)//"? "
        call dired_mark_pop_up_body("", op_symbol, files, one_marked, pop_y_or_n_p, status)
        confirmed = status%ok() .and. pop_answer
    end subroutine dired_mark_confirm

    !> What an operation is called in messages: "Compress or uncompress"
    !> for `compress', else the capitalized symbol name.
    function operation_string(op_symbol) result(s)
        character(len=*), intent(in) :: op_symbol
        character(len=:), allocatable :: s
        if (op_symbol == "compress") then
            s = "Compress or uncompress"
        else
            s = capitalize_string(op_symbol)
        end if
    end function operation_string

    !> FORMAT with N: (ngettext SINGULAR PLURAL N).
    function ngettext(singular, plural, n) result(s)
        character(len=*), intent(in) :: singular, plural
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        if (n == 1) then
            s = singular
        else
            s = plural
        end if
    end function ngettext

    !> N in decimal.
    function aux_itoa(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function aux_itoa

    ! ------------------------------------------------------------------
    ! Change file attributes
    ! ------------------------------------------------------------------

    !> The program `dired_check_process' runs, logging what it writes:
    !> MSG is displayed, then PROGRAM runs with ARGS; FAILED tells
    !> whether it exited abnormally.  If it did, the command that invoked
    !> PROGRAM and the messages it emitted are saved in
    !> `dired_log_buffer'.  Port of dired-aux.el:dired-check-process.
    recursive subroutine dired_check_process(msg, program, args, failed, status)
        character(len=*), intent(in) :: msg, program
        type(dired_string_type), intent(in) :: args(:)
        logical, intent(out) :: failed
        type(status_type), intent(out) :: status
        type(progress_reporter_type) :: reporter
        type(handle_type) :: old, err_buffer
        type(value_type) :: result, arglist
        type(value_type), allocatable :: elems(:)
        character(len=:), allocatable :: dir, output
        type(status_type) :: st
        integer :: k
        failed = .false.
        dir = aux_default_directory()
        call make_progress_reporter(reporter, msg)
        old = current_buffer()
        ! Get a clean buffer for error output:
        call get_buffer_create(" *dired-check-process output*", err_buffer, status)
        if (.not. status%ok()) return
        call set_buffer(err_buffer, status)
        if (.not. status%ok()) return
        call erase_buffer(st)
        ! caller's default-directory
        buffers(cur)%directory = dir
        allocate (elems(size(args)))
        do k = 1, size(args)
            elems(k) = make_string(args(k)%s)
        end do
        arglist = make_list(elems)
        call call_process(program, result, status, destination=t_value(), args=arglist)
        if (.not. status%ok()) then
            call set_buffer(old, st)
            return
        end if
        failed = .true.
        if (fixnump(result)) failed = xfixnum(result) /= 0
        if (failed) then
            call buffer_substring(point_min(), point_max(), output, st)
            call set_buffer(old, st)
            call dired_log(program//" "//fortran_repr(arglist)//LF)
            call dired_log(output)
        else
            call set_buffer(old, st)
            call kill_buffer(err_buffer, st)
            call progress_reporter_done(reporter)
        end if
    end subroutine dired_check_process

    !> Process all the files in FILES in batches of a convenient size, by
    !> running PROGRAM (with `dired_check_process', with message MSG) on
    !> ARGS and some of the files.  Batches are chosen to need less than
    !> MAX_CHARS chars for the file names, allowing 3 extra characters of
    !> separator per file name.  FAILED tells whether a batch failed.
    !> Port of dired-aux.el:dired-bunch-files (for dired-check-process).
    recursive subroutine dired_bunch_files(max_chars, msg, program, args, files, failed, status)
        integer, intent(in) :: max_chars
        character(len=*), intent(in) :: msg, program
        type(dired_string_type), intent(in) :: args(:), files(:)
        logical, intent(out) :: failed
        type(status_type), intent(out) :: status
        integer :: first, k, pending_length, thislength
        logical :: f
        status = status_type()
        failed = .false.
        first = 1
        pending_length = 0
        do k = 1, size(files)
            thislength = len(files(k)%s) + 3
            ! If we have at least 1 pending file and this file won't fit
            ! in the length limit, process now.
            if (k > first .and. thislength + pending_length > max_chars) then
                call run_batch(first, k - 1)
                if (.not. status%ok()) return
                first = k
                pending_length = 0
            end if
            pending_length = pending_length + thislength
        end do
        call run_batch(first, size(files))
    contains

        recursive subroutine run_batch(from, to)
            integer, intent(in) :: from, to
            type(dired_string_type), allocatable :: argv(:)
            integer :: j
            allocate (argv(size(args) + max(0, to - from + 1)))
            do j = 1, size(args)
                argv(j)%s = args(j)%s
            end do
            do j = from, to
                argv(size(args) + j - from + 1)%s = files(j)%s
            end do
            call dired_check_process(msg, program, argv, f, status)
            if (f) failed = .true.
        end subroutine run_batch
    end subroutine dired_bunch_files

    !> (format-time-string "%Y%m%d%H%M.%S" SECONDS): the local time as
    !> touch's -t option takes it.
    function touch_time_string(seconds) result(s)
        integer(int64), intent(in) :: seconds
        character(len=:), allocatable :: s
        character(len=32) :: buf
        integer :: parts(6)
        parts = emacs_localtime(seconds)
        write (buf, "(i0,4i2.2,'.',i2.2)") parts(1), parts(2), parts(3), parts(4), parts(5), parts(6)
        s = trim(buf)
    end function touch_time_string

    !> Change file attributes (group, owner, timestamp) of marked files and
    !> refresh their file lines.
    !> ATTRIBUTE_NAME is a string describing the attribute to the user.
    !> PROGRAM is the program used to change the attribute.
    !> OP_SYMBOL is the type of operation (for use in `dired_mark_pop_up').
    !> ARG describes which files to use, as in `dired_get_marked_files'.
    !> Port of dired-aux.el:dired-do-chxxx.
    recursive subroutine dired_do_chxxx(attribute_name, program, op_symbol, arg_kind, arg_n, status)
        character(len=*), intent(in) :: attribute_name, program, op_symbol
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        type(status_type), intent(out) :: status
        type(dired_string_type), allocatable :: files(:), args(:)
        character(len=:), allocatable :: default_file, default, prompt, new_attribute, operation, user, group
        type(value_type) :: collection
        type(file_stat_type) :: attrs
        type(status_type) :: st
        logical :: found, have_default, used_default, failed
        integer :: n
        call dired_get_marked_files(files, status, DIRED_LOCALP_T, arg_kind, arg_n, error="")
        if (.not. status%ok()) return
        ! The source of default file attributes is the file at point.
        call dired_get_filename(default_file, found, st, DIRED_LOCALP_T, .true.)
        have_default = .false.
        if (st%ok() .and. found) then
            select case (op_symbol)
            case ("touch")
                call file_attributes(default_file, attrs, have_default, nofollow=.true.)
                if (have_default) default = touch_time_string(attrs%mtime_sec)
            case ("chown")
                call file_attribute_names(default_file, user, group, have_default)
                if (have_default) default = user
            case ("chgrp")
                call file_attribute_names(default_file, user, group, have_default)
                if (have_default) default = group
            case default
                continue
            end select
        end if
        ! (format-prompt "Change %s of %%s to" (when touch "now") ATTRIBUTE-NAME)
        if (op_symbol == "touch") then
            prompt = "Change "//attribute_name//" of %s to (default now): "
        else
            prompt = "Change "//attribute_name//" of %s to: "
        end if
        if (op_symbol == "chown") then
            call system_users(collection)
        else if (op_symbol == "chgrp") then
            call system_groups(collection)
        else
            collection = nil()
        end if
        if (have_default) then
            call dired_mark_read_string(prompt, op_symbol, arg_kind, arg_n, files, new_attribute, used_default, &
                                        status, default_value=default, collection=collection)
        else
            call dired_mark_read_string(prompt, op_symbol, arg_kind, arg_n, files, new_attribute, used_default, &
                                        status, collection=collection)
        end if
        if (.not. status%ok()) return
        operation = program//" "//new_attribute
        n = 0
        allocate (args(3))
        ! Use `eq' instead of `equal' to detect empty input (bug#12399).
        if (.not. (len(new_attribute) == 0 .or. used_default)) then
            if (op_symbol == "touch") then
                args(1)%s = "-t"
                args(2)%s = new_attribute
                n = 2
            else
                args(1)%s = new_attribute
                n = 1
            end if
        end if
        ! (string-match-p "gnu" system-configuration): a GNU/Linux system.
        n = n + 1
        args(n)%s = "--"
        call dired_bunch_files(10000, operation, program, args(1:n), files, failed, status)
        if (.not. status%ok()) return
        ! moves point if ARG is an integer
        call dired_do_redisplay(arg_kind, arg_n, .false., status)
        if (.not. status%ok()) return
        if (failed) call dired_log_summary(operation//": error", files(1:0))
    end subroutine dired_do_chxxx

    module procedure dired_do_chmod
    type(dired_string_type), allocatable :: files(:)
    character(len=:), allocatable :: default_file, modestr, default, modes
    type(file_stat_type) :: attrs
    type(status_type) :: st
    integer(int32) :: num_modes, mode
    logical :: found, have_default, used_default, have_num
    integer :: k
    call dired_get_marked_files(files, status, DIRED_LOCALP_T, arg_kind, arg_n, error="")
    if (.not. status%ok()) return
    ! The source of default file attributes is the file at point.
    call dired_get_filename(default_file, found, st, DIRED_LOCALP_T, .true.)
    have_default = .false.
    if (st%ok() .and. found) then
        call file_attributes(default_file, attrs, have_default, nofollow=.true.)
        if (have_default) then
            modestr = file_modes_number_to_symbolic(attrs%mode)
            default = "u="//strip_dashes(modestr(2:4))//",g="//strip_dashes(modestr(5:7))//",o="// &
                      strip_dashes(modestr(8:10))
        end if
    end if
    if (have_default) then
        call dired_mark_read_string("Change mode of %s to: ", "chmod", arg_kind, arg_n, files, modes, used_default, &
                                    status, default_value=default)
    else
        call dired_mark_read_string("Change mode of %s to: ", "chmod", arg_kind, arg_n, files, modes, used_default, &
                                    status)
    end if
    if (.not. status%ok()) return
    ! Use `eq' instead of `equal' to detect empty input (bug#12399).
    if (len(modes) == 0 .or. used_default) then
        ! We used to treat empty input as DEFAULT, but that is not such
        ! a good idea (Bug#9361).
        call set_error(status, FM_GENERIC_ERROR, "No file mode specified")
        return
    end if
    have_num = string_match("^[0-7]+", modes, st) > 0
    if (have_num) num_modes = string_to_octal(modes)
    do k = 1, size(files)
        if (have_num) then
            mode = num_modes
        else
            call file_modes_symbolic_to_number(modes, mode, status, file_modes(files(k)%s, nofollow=.true.))
            if (.not. status%ok()) return
        end if
        call set_file_modes(files(k)%s, mode, status, nofollow=.true.)
        if (.not. status%ok()) return
    end do
    call dired_do_redisplay(arg_kind, arg_n, .false., status)
contains

    function strip_dashes(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: j
        t = ""
        do j = 1, len(s)
            if (s(j:j) /= "-") t = t//s(j:j)
        end do
    end function strip_dashes
    end procedure dired_do_chmod

    !> (string-to-number S 8): the octal number at the start of S (after
    !> spaces and tabs, with an optional sign), 0 if none.
    integer(int32) function string_to_octal(s) result(n)
        character(len=*), intent(in) :: s
        integer :: i
        logical :: negative
        n = 0
        i = 1
        do while (i <= len(s))
            if (s(i:i) /= " " .and. s(i:i) /= TAB) exit
            i = i + 1
        end do
        negative = .false.
        if (i <= len(s)) then
            if (s(i:i) == "-" .or. s(i:i) == "+") then
                negative = s(i:i) == "-"
                i = i + 1
            end if
        end if
        do while (i <= len(s))
            if (index("01234567", s(i:i)) == 0) exit
            n = n*8 + (iachar(s(i:i)) - iachar("0"))
            i = i + 1
        end do
        if (negative) n = -n
    end function string_to_octal

    !> Change the mode of the marked (or next ARG) files.
    !> Both octal numeric modes like `644' and symbolic modes like `g+w'
    !> are supported.
    !> After invoking the command, type
    !> \<minibuffer_local_completion_map>\[next_history_element] to pull
    !> the file attributes of the file at point into the minibuffer.
    !>
    !> See Info node `(coreutils)File permissions' for more information.
    !> Alternatively, see the man page for "chmod(1)".  Port of
    !> dired-aux.el:dired-do-chmod (interactive).
    recursive subroutine cmd_dired_do_chmod(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_do_chmod(kind, n, status)
    end subroutine cmd_dired_do_chmod

    module procedure dired_do_chgrp
    call dired_do_chxxx("Group", "chgrp", "chgrp", arg_kind, arg_n, status)
    end procedure dired_do_chgrp

    !> Change the group of the marked (or next ARG) files.
    !> After invoking the command, type
    !> \<minibuffer_local_completion_map>\[next_history_element] to pull
    !> the file attributes of the file at point into the minibuffer.  Port
    !> of dired-aux.el:dired-do-chgrp (interactive).
    recursive subroutine cmd_dired_do_chgrp(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_do_chgrp(kind, n, status)
    end subroutine cmd_dired_do_chgrp

    module procedure dired_do_chown
    call dired_do_chxxx("Owner", dired_chown_program, "chown", arg_kind, arg_n, status)
    end procedure dired_do_chown

    !> Change the owner of the marked (or next ARG) files.
    !> After invoking the command, type
    !> \<minibuffer_local_completion_map>\[next_history_element] to pull
    !> the file attributes of the file at point into the minibuffer.  Port
    !> of dired-aux.el:dired-do-chown (interactive).
    recursive subroutine cmd_dired_do_chown(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_do_chown(kind, n, status)
    end subroutine cmd_dired_do_chown

    module procedure dired_do_touch
    call dired_do_chxxx("Timestamp", dired_touch_program, "touch", arg_kind, arg_n, status)
    end procedure dired_do_touch

    !> Change the timestamp of the marked (or next ARG) files.
    !> This calls touch.
    !> After invoking the command, type
    !> \<minibuffer_local_completion_map>\[next_history_element] to pull
    !> the file attributes of the file at point into the minibuffer.  Port
    !> of dired-aux.el:dired-do-touch (interactive).
    recursive subroutine cmd_dired_do_touch(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_do_touch(kind, n, status)
    end subroutine cmd_dired_do_touch

    ! ------------------------------------------------------------------
    ! Commands that delete or redisplay part of the Dired buffer
    ! ------------------------------------------------------------------

    module procedure dired_kill_line
    type(value_type) :: saved_ro
    character(len=:), allocatable :: file
    type(status_type) :: st
    integer(int64) :: n, b, p
    logical :: found
    status = status_type()
    n = arg
    call aux_inhibit_read_only_begin(saved_ro)
    do while (n /= 0)
        call dired_get_filename(file, found, status, no_error_if_not_filep=.true.)
        if (.not. status%ok()) exit
        if (.not. found) then
            call set_error(status, FM_GENERIC_ERROR, "Can only kill file lines")
            exit
        end if
        p = point()
        if (dired_goto_subdir(file) /= 0) call dired_kill_subdir(st)
        call goto_char(min(p, point_max()))
        b = aux_bol()
        call delete_region(b, aux_bol(2_int64), status)
        if (.not. status%ok()) exit
        if (n > 0) then
            n = n - 1
        else
            n = n + 1
            call aux_fwd_line(-1_int64)
        end if
    end do
    call aux_inhibit_read_only_end(saved_ro)
    p = dired_move_to_filename(st)
    end procedure dired_kill_line

    !> Kill the current line (not the files).
    !> With a prefix argument, kill that many lines starting with the current line.
    !> (A negative argument kills backward.)  Port of
    !> dired-aux.el:dired-kill-line (interactive).
    recursive subroutine cmd_dired_kill_line(status)
        type(status_type), intent(out) :: status
        call dired_kill_line(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_dired_kill_line

    module procedure dired_do_kill_lines
    type(value_type) :: saved_ro
    integer(int64) :: opoint, b, n
    status = status_type()
    n = 0
    if (arg_kind /= DIRED_ARG_NIL) then
        if (len(dired_get_subdir()) > 0) then
            call dired_kill_subdir(status)
        else
            call dired_kill_line(arg_n, status)
        end if
        if (present(count)) count = 0
        return
    end if
    opoint = point()
    call goto_char(point_min())
    if (present(init_count)) n = init_count
    call aux_inhibit_read_only_begin(saved_ro)
    do while (.not. eobp())
        if (.not. aux_re_search_forward("^"//regexp_quote(dired_marker_char))) exit
        n = n + 1
        b = aux_bol()
        call delete_region(b, aux_bol(2_int64), status)
        if (.not. status%ok()) exit
    end do
    call aux_inhibit_read_only_end(saved_ro)
    call goto_char(min(opoint, point_max()))
    if (present(fmt)) then
        if (len(fmt) > 0) call message(format_d(fmt, n))
    else
        if (n == 1) then
            call message("Killed "//aux_itoa(n)//" line.")
        else
            call message("Killed "//aux_itoa(n)//" lines.")
        end if
    end if
    if (present(count)) count = n
contains

    !> FMT with its "%d" replaced by N and its "%s" by the plural s.
    function format_d(fmt, n) result(s)
        character(len=*), intent(in) :: fmt
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        integer :: k
        s = fmt
        k = index(s, "%d")
        if (k > 0) s = s(1:k - 1)//aux_itoa(n)//s(k + 2:)
        k = index(s, "%s")
        if (k > 0) then
            if (n == 1) then
                s = s(1:k - 1)//s(k + 2:)
            else
                s = s(1:k - 1)//"s"//s(k + 2:)
            end if
        end if
    end function format_d
    end procedure dired_do_kill_lines

    !> Remove all marked lines, or the next ARG lines.
    !> The files or directories on those lines are _not_ deleted.  Only the
    !> Dired listing is affected.  To restore the removals, use \[revert_buffer].
    !>
    !> With a numeric prefix arg, remove that many lines going forward,
    !> starting with the current line.  (A negative prefix arg removes lines
    !> going backward.)
    !>
    !> If you use a prefix arg to remove the line for a subdir whose listing
    !> you have inserted into the Dired buffer, then that subdir listing is
    !> also removed.
    !>
    !> To remove a subdir listing _without_ removing the subdir's line in its
    !> parent listing, go to the header line of the subdir listing and use
    !> this command with any prefix arg.  Port of
    !> dired-aux.el:dired-do-kill-lines (interactive).
    recursive subroutine cmd_dired_do_kill_lines(status)
        type(status_type), intent(out) :: status
        if (nilp(current_prefix_arg)) then
            call dired_do_kill_lines(DIRED_ARG_NIL, 0_int64, status)
        else
            call dired_do_kill_lines(DIRED_ARG_INTEGER, prefix_numeric_value(current_prefix_arg), status)
        end if
    end subroutine cmd_dired_do_kill_lines

    ! ------------------------------------------------------------------
    ! Compression
    ! ------------------------------------------------------------------

    !> Run CMD (with the shell), and check for output.
    !> On error, pop up the log buffer.
    !> RES is the result of `process_file': zero for success (or a signal
    !> description).  Port of dired-aux.el:dired-shell-command.
    recursive subroutine dired_shell_command(cmd, res, status)
        character(len=*), intent(in) :: cmd
        type(value_type), intent(out) :: res
        type(status_type), intent(out) :: status
        type(handle_type) :: old, out_buffer
        type(value_type) :: args
        character(len=:), allocatable :: dir
        type(status_type) :: st
        logical :: zero
        dir = aux_default_directory()
        old = current_buffer()
        call get_buffer_create(" *dired-check-process output*", out_buffer, status)
        if (.not. status%ok()) return
        call set_buffer(out_buffer, status)
        if (.not. status%ok()) return
        call erase_buffer(st)
        buffers(cur)%directory = dir
        args = list(make_string("-c"), make_string(cmd))
        call call_process(aux_shell_file_name(), res, status, destination=t_value(), args=args)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        zero = .false.
        if (fixnump(res)) zero = xfixnum(res) == 0
        if (.not. zero) call pop_to_buffer(out_buffer, st)
    end subroutine dired_shell_command

    !> The shell `shell_command' uses: $SHELL, else "/bin/sh" (callproc.c
    !> shell-file-name).
    function aux_shell_file_name() result(sh)
        character(len=:), allocatable :: sh
        integer :: length, stat
        call get_environment_variable("SHELL", length=length, status=stat)
        if (stat == 0 .and. length > 0) then
            allocate (character(len=length) :: sh)
            call get_environment_variable("SHELL", sh)
        else
            sh = "/bin/sh"
        end if
    end function aux_shell_file_name

    !> COMMAND with every %i replaced by I and every %o by O, literally
    !> (the replace-regexp-in-string calls of dired-compress-file).
    function replace_io(command, i, o) result(s)
        character(len=*), intent(in) :: command, i, o
        character(len=:), allocatable :: s, t
        t = replace_all(command, "%i", i)
        s = replace_all(t, "%o", o)
    end function replace_io

    !> TEXT with every occurrence of FROM replaced by TO, left to right.
    function replace_all(text, from, to) result(s)
        character(len=*), intent(in) :: text, from, to
        character(len=:), allocatable :: s
        integer :: k, start
        s = ""
        start = 1
        do
            k = index(text(start:), from)
            if (k == 0) exit
            s = s//text(start:start + k - 2)//to
            start = start + k - 1 + len(from)
        end do
        s = s//text(start:)
    end function replace_all

    module procedure dired_compress_file
    character(len=:), allocatable :: command, suffix, out_name, dir_saved, msg
    type(dired_string_type), allocatable :: args(:)
    type(value_type) :: res
    type(status_type) :: st
    integer(int64) :: mb
    integer :: k, rule, match
    logical :: saved_fold, has_newname, failed, answer, matched
    found = .false.
    newname = ""
    status = status_type()
    ! See if any suffix rule matches this file name.
    saved_fold = case_fold_search%value
    case_fold_search%value = .false.
    rule = 0
    do k = 1, size(dired_compress_file_suffixes)
        if (string_match(dired_compress_file_suffixes(k)%regexp, file, st) > 0) then
            rule = k
            exit
        end if
    end do
    has_newname = .false.
    if (rule > 0) then
        mb = match_beginning(0, st)
        newname = file(1:string_index_to_offset(file, mb))//dired_compress_file_suffixes(rule)%new_suffix
        has_newname = .true.
    end if
    case_fold_search%value = saved_fold
    if (file_symlink_p(file)) return
    if (rule > 0) then
        if (len(dired_compress_file_suffixes(rule)%program) > 0) then
            command = dired_compress_file_suffixes(rule)%program
            matched = index(command, "%i") > 0 .or. index(command, "%o") > 0
            if (matched) then
                newname = file_name_as_directory(newname)
                found = .true.
                call dired_shell_command(replace_io(command, shell_quote_argument(file), shell_quote_argument(newname)), &
                                         res, status)
                return
            end if
            ! We found an uncompression rule.
            match = index(command, " ")
            msg = "Uncompressing "//file
            if (match > 0) then
                allocate (args(2))
                args(1)%s = command(match + 1:)
                args(2)%s = file
                call dired_check_process(msg, command(1:match - 1), args, failed, status)
            else
                allocate (args(1))
                args(1)%s = file
                call dired_check_process(msg, command, args, failed, status)
            end if
            if (.not. status%ok()) return
            found = .not. failed
            return
        end if
    end if
    ! We don't recognize the file as compressed, so compress it.
    ! Try gzip; if we don't have that, use compress.
    if (file_directory_p(file)) then
        suffix = dired_compress_directory_default_suffix
        if (len(suffix) == 0) suffix = ".tar.gz"
        k = rule_for(dired_compress_files_alist, suffix)
        if (k == 0) then
            call set_error(status, FM_USER_ERROR, format_message("No compression rule found for "// &
                                                                 "`dired_compress_directory_default_suffix' ")//suffix// &
                           format_message(", see `dired_compress_files_alist' for the supported suffixes list"))
            return
        end if
        out_name = file//suffix
        dir_saved = aux_default_directory()
        buffers(cur)%directory = file_name_directory(file)
        call dired_shell_command(replace_io(dired_compress_files_alist(k)%command, &
                                            shell_quote_argument(file_name_nondirectory(file)), &
                                            shell_quote_argument(out_name)), res, status)
        buffers(cur)%directory = dir_saved
        if (.not. status%ok()) then
            if (condition_matches(status%code, FM_FILE_ERROR)) call compress_with_compress()
            return
        end if
        newname = out_name
        found = .true.
        return
    end if
    suffix = dired_compress_file_default_suffix
    if (len(suffix) == 0) suffix = ".gz"
    out_name = file//suffix
    k = rule_for(dired_compress_file_alist, suffix)
    if (k == 0) then
        call set_error(status, FM_USER_ERROR, "No compression rule found for suffix "//dired_compress_file_default_suffix// &
                       format_message(", see `dired_compress_file_alist' for the supported suffixes list"))
        return
    end if
    if (.not. file_exists_p(file)) return
    if (file_exists_p(out_name)) then
        call y_or_n_p("File "//out_name//" already exists.  Really compress? ", answer, status)
        if (.not. status%ok()) return
        if (.not. answer) return
    end if
    call dired_shell_command(replace_io(dired_compress_file_alist(k)%command, shell_quote_argument(file), &
                                        shell_quote_argument(out_name)), res, status)
    if (.not. status%ok()) then
        if (condition_matches(status%code, FM_FILE_ERROR)) call compress_with_compress()
        return
    end if
    if (.not. file_exists_p(out_name)) out_name = file//".z"
    ! Rename the compressed file to NEWNAME if it hasn't got that name
    ! already.
    if (has_newname .and. newname /= out_name) then
        call rename_file(out_name, newname, status, ok_if_already_exists=.true.)
        if (.not. status%ok()) then
            if (condition_matches(status%code, FM_FILE_ERROR)) call compress_with_compress()
            return
        end if
    else
        newname = out_name
    end if
    found = .true.
contains

    !> The `file_error' handler: try compress.  Don't use NEWNAME with
    !> `compress'.
    recursive subroutine compress_with_compress()
        type(dired_string_type) :: cargs(2)
        logical :: cfailed
        status = status_type()
        cargs(1)%s = "-f"
        cargs(2)%s = file
        call dired_check_process("Compressing "//file, "compress", cargs, cfailed, status)
        if (.not. status%ok()) return
        if (.not. cfailed) then
            newname = file//".Z"
            found = .true.
        end if
    end subroutine compress_with_compress

    !> The first rule of ALIST whose regexp matches SUFFIX (0 if none).
    integer function rule_for(alist, suffix) result(r)
        type(dired_compress_rule_type), intent(in) :: alist(:)
        character(len=*), intent(in) :: suffix
        type(status_type) :: s
        integer :: j
        r = 0
        do j = 1, size(alist)
            if (string_match(alist(j)%regexp, suffix, s, inhibit_modify=.true.) > 0) then
                r = j
                return
            end if
        end do
    end function rule_for
    end procedure dired_compress_file

    !> Compress or uncompress the current file.  FAILURE is the offending
    !> file name, or "" for success.  Port of dired-aux.el:dired-compress.
    recursive subroutine dired_compress(failure, status)
        character(len=:), allocatable, intent(out) :: failure
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from_file, new_file
        type(handle_type), allocatable :: bufs(:)
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: start
        logical :: found, added
        integer :: k
        failure = ""
        call dired_get_filename(from_file, found, status)
        if (.not. status%ok()) return
        call dired_compress_file(from_file, new_file, found, status)
        if (.not. status%ok()) return
        if (found) then
            start = point()
            ! Remove any preexisting entry for the name NEW-FILE.
            call dired_remove_entry(new_file)
            call goto_char(start)
            ! Now replace the current line with an entry for NEW-FILE, if
            ! it exists.  But don't remove the current line if either
            ! FROM-FILE or NEW-FILE is a directory, because
            ! compressing/uncompressing directories doesn't remove the
            ! original.  If NEW-FILE doesn't exist, assume that we are out
            ! of sync with the current directory, and revert it.
            if (file_exists_p(new_file)) then
                if (file_directory_p(from_file) .or. file_directory_p(new_file)) then
                    call dired_add_entry(new_file, added, status, relative=.true.)
                else
                    call dired_update_file_line(status, new_file)
                end if
            else
                bufs = dired_buffers_for_dir(dired_current_directory(st))
                old = current_buffer()
                do k = 1, size(bufs)
                    call set_buffer(bufs(k), st)
                    call revert_buffer(status)
                    if (.not. status%ok()) exit
                end do
                call set_buffer(old, st)
            end if
        else
            call dired_log("Failed to (un)compress "//from_file)
            failure = from_file
        end if
    end subroutine dired_compress

    !> The body of `dired_do_compress': `dired_compress', recording the
    !> results, then `sit_for' 0 (SHOW-PROGRESS).
    recursive subroutine compress_body(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: failure
        call dired_compress(failure, status)
        if (.not. status%ok()) return
        call push_check_result(failure)
        call sit_for(0)
    end subroutine compress_body

    !> Record one result of a `dired_map_over_marks_check' body.
    subroutine push_check_result(failure)
        character(len=*), intent(in) :: failure
        type(dired_string_type), allocatable :: grown(:)
        integer :: n
        n = size(check_results)
        allocate (grown(n + 1))
        grown(1:n) = check_results
        grown(n + 1)%s = failure
        call move_alloc(grown, check_results)
    end subroutine push_check_result

    !> Map FUN over marked files (with ARG like in `dired_map_over_marks')
    !> and display failures.  FUN records, with `push_check_result', the
    !> offending object (e.g. the short form of the filename) for a
    !> failure, or "", and probably logs a detailed error explanation
    !> using `dired_log'.  OP_SYMBOL is a symbol describing the operation
    !> performed (e.g. `compress').  Port of
    !> dired-aux.el:dired-map-over-marks-check.
    recursive subroutine dired_map_over_marks_check(fun, arg_kind, arg_n, op_symbol, status)
        interface
            recursive subroutine fun(status)
                import :: status_type
                implicit none(type, external)
                type(status_type), intent(out) :: status
            end subroutine fun
        end interface
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        character(len=*), intent(in) :: op_symbol
        type(status_type), intent(out) :: status
        type(dired_string_type), allocatable :: failures(:)
        character(len=:), allocatable :: string
        integer(int64) :: total, count
        logical :: confirmed, found, upward
        integer :: k
        call dired_mark_confirm(op_symbol, arg_kind, arg_n, confirmed, status)
        if (.not. status%ok() .or. .not. confirmed) return
        if (allocated(check_results)) deallocate (check_results)
        allocate (check_results(0))
        call dired_map_over_marks(fun, arg_kind, arg_n, status, found, upward)
        if (.not. status%ok()) return
        total = size(check_results)
        count = 0
        do k = 1, size(check_results)
            if (len(check_results(k)%s) > 0) count = count + 1
        end do
        allocate (failures(count))
        count = 0
        do k = 1, size(check_results)
            if (len(check_results(k)%s) == 0) cycle
            count = count + 1
            failures(count)%s = check_results(k)%s
        end do
        string = operation_string(op_symbol)
        if (count == 0) then
            call message(string//": "//aux_itoa(total)//ngettext(" file.", " files.", total))
        else
            ! end this bunch of errors:
            call dired_log_summary("Failed to "//downcase_string(string)//" "//aux_itoa(count)//" of "// &
                                   aux_itoa(total)//ngettext(" file", " files", total), failures)
        end if
    end subroutine dired_map_over_marks_check

    module procedure dired_do_compress
    call dired_map_over_marks_check(compress_body, arg_kind, arg_n, "compress", status)
    end procedure dired_do_compress

    !> Compress or uncompress marked (or next ARG) files.
    !> If invoked on a directory, compress all of the files in
    !> the directory and all of its subdirectories, recursively,
    !> into a .tar.gz archive.
    !> If invoked on a .tar.gz or a .tgz or a .zip or a .7z archive,
    !> uncompress and unpack all the files in the archive.  Port of
    !> dired-aux.el:dired-do-compress (interactive).
    recursive subroutine cmd_dired_do_compress(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_do_compress(kind, n, status)
    end subroutine cmd_dired_do_compress

    !> FMT with %o replaced by O and %i by I, as format-spec does with
    !> the specification ((?o . O) (?i . I)): flags, width and precision
    !> are honored, %% is a %.  Port of format-spec.el:format-spec (no
    !> IGNORE-MISSING or SPLIT).
    subroutine format_spec_io(fmt, o, i, result, status)
        character(len=*), intent(in) :: fmt, o, i
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: flags, val
        integer :: k, j, width, precision
        status = status_type()
        result = ""
        k = 1
        do while (k <= len(fmt))
            if (fmt(k:k) /= "%") then
                result = result//fmt(k:k)
                k = k + 1
                cycle
            end if
            j = k + 1
            if (j <= len(fmt)) then
                if (fmt(j:j) == "%") then
                    result = result//"%"
                    k = j + 1
                    cycle
                end if
            end if
            flags = ""
            do while (j <= len(fmt))
                if (index("-0 ^_<>", fmt(j:j)) == 0) exit
                flags = flags//fmt(j:j)
                j = j + 1
            end do
            width = -1
            do while (j <= len(fmt))
                if (index("0123456789", fmt(j:j)) == 0) exit
                if (width < 0) width = 0
                width = width*10 + iachar(fmt(j:j)) - iachar("0")
                j = j + 1
            end do
            precision = -1
            if (j <= len(fmt)) then
                if (fmt(j:j) == ".") then
                    j = j + 1
                    precision = 0
                    do while (j <= len(fmt))
                        if (index("0123456789", fmt(j:j)) == 0) exit
                        precision = precision*10 + iachar(fmt(j:j)) - iachar("0")
                        j = j + 1
                    end do
                end if
            end if
            if (j > len(fmt)) then
                call set_error(status, FM_GENERIC_ERROR, format_message("Invalid format string"))
                return
            end if
            select case (fmt(j:j))
            case ("o")
                val = o
            case ("i")
                val = i
            case default
                call set_error(status, FM_GENERIC_ERROR, format_message("Invalid format character: `%")//fmt(j:j)// &
                               format_message("'"))
                return
            end select
            if (precision >= 0 .and. len(val) > precision) val = val(1:precision)
            if (index(flags, "^") > 0) val = upcase_string(val)
            if (index(flags, "_") > 0) val = downcase_string(val)
            if (width >= 0 .and. len(val) > width) then
                if (index(flags, "<") > 0) then
                    val = val(len(val) - width + 1:)
                else if (index(flags, ">") > 0) then
                    val = val(1:width)
                end if
            end if
            if (width > len(val)) then
                if (index(flags, "-") > 0) then
                    val = val//repeat(" ", width - len(val))
                else if (index(flags, "0") > 0) then
                    val = repeat("0", width - len(val))//val
                else
                    val = repeat(" ", width - len(val))//val
                end if
            end if
            result = result//val
            k = j + 1
        end do
    end subroutine format_spec_io

    module procedure dired_do_compress_to
    type(dired_string_type), allocatable :: in_files(:)
    character(len=:), allocatable :: out_file, input, ins, command
    type(value_type) :: res
    type(status_type) :: st
    integer :: k, rule
    logical :: answer, zero
    call dired_get_marked_files(in_files, status, error="")
    if (.not. status%ok()) return
    call read_file_name("Compress to: ", input, status)
    if (.not. status%ok()) return
    out_file = aux_expand(input)
    rule = 0
    do k = 1, size(dired_compress_files_alist)
        if (string_match(dired_compress_files_alist(k)%regexp, out_file, st) > 0) then
            rule = k
            exit
        end if
    end do
    if (rule == 0) then
        call set_error(status, FM_GENERIC_ERROR, "No compression rule found for "//out_file// &
                       format_message(", see `dired_compress_files_alist'"))
        return
    end if
    if (file_exists_p(out_file)) then
        call y_or_n_p(abbreviate_file_name(out_file)//" exists, overwrite?", answer, status)
        if (.not. status%ok()) return
        if (.not. answer) then
            call message("Compression aborted")
            return
        end if
    end if
    ins = ""
    do k = 1, size(in_files)
        if (k > 1) ins = ins//" "
        ins = ins//shell_quote_argument(file_relative_name(in_files(k)%s))
    end do
    call format_spec_io(dired_compress_files_alist(rule)%command, shell_quote_argument(out_file), ins, command, status)
    if (.not. status%ok()) return
    call dired_shell_command(command, res, status)
    if (.not. status%ok()) return
    zero = .false.
    if (fixnump(res)) zero = xfixnum(res) == 0
    if (zero) then
        call message(ngettext("Compressed "//aux_itoa(int(size(in_files), int64))//" file to ", &
                              "Compressed "//aux_itoa(int(size(in_files), int64))//" files to ", &
                              int(size(in_files), int64))//file_name_nondirectory(out_file))
    end if
    end procedure dired_do_compress_to

    !> Compress selected files and directories to an archive.
    !> Prompt for the archive file name.
    !> Choose the archiving command based on the archive file-name extension
    !> and `dired_compress_files_alist'.  Port of
    !> dired-aux.el:dired-do-compress-to (interactive).
    recursive subroutine cmd_dired_do_compress_to(status)
        type(status_type), intent(out) :: status
        call dired_do_compress_to(status)
    end subroutine cmd_dired_do_compress_to

    ! ------------------------------------------------------------------
    ! Updating Dired buffers
    ! ------------------------------------------------------------------

    module procedure dired_remove_file
    type(handle_type), allocatable :: bufs(:)
    type(handle_type) :: old
    type(status_type) :: st
    integer :: k
    bufs = dired_buffers_for_dir(file_name_directory(file), file_name_nondirectory(file))
    old = current_buffer()
    do k = 1, size(bufs)
        call set_buffer(bufs(k), st)
        call dired_remove_entry(file)
    end do
    call set_buffer(old, st)
    end procedure dired_remove_file

    module procedure dired_relist_file
    type(handle_type), allocatable :: bufs(:)
    type(handle_type) :: old
    type(status_type) :: st
    integer :: k
    status = status_type()
    bufs = dired_buffers_for_dir(file_name_directory(file), file_name_nondirectory(file))
    old = current_buffer()
    do k = 1, size(bufs)
        call set_buffer(bufs(k), st)
        call dired_relist_entry(file, status)
        if (.not. status%ok()) exit
    end do
    call set_buffer(old, st)
    end procedure dired_relist_file

    !> Relist the line for FILE, or just add it if it did not exist.
    !> FILE must be an absolute file name.  Port of
    !> dired-aux.el:dired-relist-entry.
    recursive subroutine dired_relist_entry(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        type(status_type) :: st
        integer(int64) :: opoint, b
        character :: marker
        logical :: added, have_marker
        status = status_type()
        opoint = point()
        have_marker = .false.
        marker = " "
        call aux_inhibit_read_only_begin(saved_ro)
        if (dired_goto_file(file, st) /= 0) then
            call beginning_of_line()
            marker = aux_char_at()
            have_marker = .true.
            b = point()
            call delete_region(b, aux_bol(2_int64), st)
        end if
        if (have_marker .and. marker /= " " .and. marker /= achar(0)) then
            call dired_add_entry(directory_file_name(file), added, status, marker_char=marker)
        else
            call dired_add_entry(directory_file_name(file), added, status)
        end if
        call aux_inhibit_read_only_end(saved_ro)
        call goto_char(min(opoint, point_max()))
    end subroutine dired_relist_entry

    !> Return FILE's marker (MARKER), or FOUND false if unmarked.  Port of
    !> dired.el:dired-file-marker.
    subroutine dired_file_marker(file, marker, found)
        character(len=*), intent(in) :: file
        character, intent(out) :: marker
        logical, intent(out) :: found
        type(status_type) :: st
        integer(int64) :: opoint
        found = .false.
        marker = " "
        opoint = point()
        if (dired_goto_file(file, st) /= 0) then
            call beginning_of_line()
            marker = aux_char_at()
            found = marker /= " "
        end if
        call goto_char(opoint)
    end subroutine dired_file_marker

    !> Remove the entry of DIR from `dired_buffers'.  Port of
    !> dired.el:dired-unadvertise.
    subroutine dired_unadvertise(dir)
        character(len=*), intent(in) :: dir
        type(dired_buffer_entry_type), allocatable :: kept(:)
        character(len=:), allocatable :: expanded
        integer :: k, n
        logical :: removed
        expanded = aux_expand(dir)
        allocate (kept(size(dired_buffers)))
        n = 0
        removed = .false.
        do k = 1, size(dired_buffers)
            ! (delq (assoc ...)): the first matching entry
            if (.not. removed .and. dired_buffers(k)%dir == expanded) then
                removed = .true.
                cycle
            end if
            n = n + 1
            kept(n) = dired_buffers(k)
        end do
        deallocate (dired_buffers)
        allocate (dired_buffers(n))
        dired_buffers(:) = kept(1:n)
    end subroutine dired_unadvertise

    ! ------------------------------------------------------------------
    ! Copy, move/rename, making hard and symbolic links
    ! ------------------------------------------------------------------

    !> Save old version of file TO that is to be overwritten.  Port of
    !> dired-aux.el:dired-handle-overwrite: `dired_backup_overwrite' is
    !> not ported (it is false), so nothing is backed up.
    subroutine dired_handle_overwrite(to)
        character(len=*), intent(in) :: to
        if (len(to) < 0) return
    end subroutine dired_handle_overwrite

    module procedure dired_copy_file
    call dired_handle_overwrite(to)
    call dired_copy_file_recursive(from, to, ok_flag, status, dired_copy_preserve_time, .true., dired_recursive_copies, &
                                   dired_copy_dereference)
    end procedure dired_copy_file

    !> Create DIR if doesn't exist according to `dired_create_destination_dirs'.
    !> Port of dired-aux.el:dired-maybe-create-dirs.
    recursive subroutine dired_maybe_create_dirs(dir, status)
        character(len=*), intent(in) :: dir
        type(status_type), intent(out) :: status
        logical :: answer
        status = status_type()
        if (dired_create_destination_dirs == DIRED_CREATE_DESTINATION_DIRS_NIL) return
        if (file_exists_p(dir)) return
        answer = dired_create_destination_dirs == DIRED_CREATE_DESTINATION_DIRS_ALWAYS
        if (.not. answer) then
            call yes_or_no_p("Create destination dir `"//dir//"'? ", answer, status)
            if (.not. status%ok()) return
        end if
        if (answer) call dired_create_directory(dir, status)
    end subroutine dired_maybe_create_dirs

    module procedure dired_copy_file_recursive
    type(file_stat_type) :: attrs
    integer :: rec
    logical :: keep, at_top, deref
    character(len=:), allocatable :: target
    logical :: found, is_dir, is_link, answer, do_dir
    call file_attributes(from, attrs, found, nofollow=.true.)
    is_dir = .false.
    is_link = .false.
    if (found) then
        is_dir = iand(attrs%mode, S_IFMT) == S_IFDIR
        is_link = iand(attrs%mode, S_IFMT) == S_IFLNK
    end if
    status = status_type()
    keep = .false.
    if (present(preserve_time)) keep = preserve_time
    at_top = .false.
    if (present(top)) at_top = top
    rec = DIRED_RECURSIVE_NEVER
    if (present(recursive)) rec = recursive
    deref = .false.
    if (present(dereference)) deref = dereference
    if (is_dir) then
        if (file_in_directory_p(to, from)) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Cannot copy `")//from// &
                           format_message("' into its subdirectory `")//to//format_message("'"))
            return
        end if
    end if
    do_dir = .false.
    if (rec /= DIRED_RECURSIVE_NEVER .and. is_dir) then
        if (rec == DIRED_RECURSIVE_ALWAYS) then
            do_dir = .true.
        else
            call yes_or_no_p("Copy "//from//" recursively? ", answer, status)
            if (.not. status%ok()) return
            do_dir = answer
        end if
    end if
    if (do_dir) then
        call copy_directory(from, to, status, keep_time=keep)
        return
    end if
    if (.not. at_top) call dired_handle_overwrite(to)
    if (.not. deref .and. is_link) then
        ! It is a symlink
        target = file_symlink_target(from)
        call make_symbolic_link(target, to, status, ok_flag)
    else
        call dired_maybe_create_dirs(file_name_directory(to), status)
        if (.not. status%ok()) return
        call copy_file(from, to, status, ok_if_already_exists=ok_flag, keep_time=keep)
    end if
    if (status%code == FM_FILE_DATE_ERROR) then
        call push_create_files_failure(dired_make_relative(from))
        call dired_log(format_message("Can't set date on ")//from//":"//LF//format_error(status)//LF)
        status = status_type()
    end if
    end procedure dired_copy_file_recursive

    !> Push NAME on `dired_create_files_failures'.
    subroutine push_create_files_failure(name)
        character(len=*), intent(in) :: name
        type(dired_string_type), allocatable :: grown(:)
        integer :: n
        if (.not. allocated(create_files_failures)) allocate (create_files_failures(0))
        n = size(create_files_failures)
        allocate (grown(n + 1))
        grown(1)%s = name
        grown(2:) = create_files_failures
        call move_alloc(grown, create_files_failures)
    end subroutine push_create_files_failure

    module procedure dired_rename_file
    type(handle_type) :: b, old
    type(status_type) :: st
    character(len=:), allocatable :: absname
    logical :: file_is_dir_p
    file_is_dir_p = file_directory_p(file)
    call dired_handle_overwrite(newname)
    call dired_maybe_create_dirs(file_name_directory(newname), status)
    if (.not. status%ok()) return
    ! error is caught in -create-files
    call rename_file(file, newname, status, ok_if_already_exists)
    if (.not. status%ok()) return
    ! Silently rename the visited file of any buffer visiting this file.
    absname = aux_expand(file)
    b = get_file_buffer(absname)
    if (b%index /= 0) then
        old = current_buffer()
        call set_buffer(b, st)
        call set_visited_file_name(newname, st, no_query=.true., along_with_file=.true.)
        call set_buffer(old, st)
    end if
    call dired_remove_file(file)
    ! See if it's an inserted subdir, and rename that, too.
    if (file_is_dir_p) call dired_rename_subdir(file, newname, status)
    end procedure dired_rename_file

    !> Rename the subdirectory FROM_DIR to TO_DIR in all Dired buffers, and
    !> the visited file names of the buffers visiting files in it.  Port
    !> of dired-aux.el:dired-rename-subdir.
    recursive subroutine dired_rename_subdir(from_dir_in, to_dir_in, status)
        character(len=*), intent(in) :: from_dir_in, to_dir_in
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from_dir, to_dir, expanded_from_dir, to_file
        type(handle_type), allocatable :: bufs(:), blist(:)
        type(handle_type) :: old
        type(status_type) :: st
        logical :: modflag
        integer :: k
        integer(int64) :: b
        status = status_type()
        from_dir = file_name_as_directory(from_dir_in)
        to_dir = file_name_as_directory(to_dir_in)
        bufs = dired_buffers_for_dir(from_dir)
        old = current_buffer()
        do k = 1, size(bufs)
            call set_buffer(bufs(k), st)
            call dired_rename_subdir_1(from_dir, to_dir, status)
            if (.not. status%ok()) exit
        end do
        call set_buffer(old, st)
        if (.not. status%ok()) return
        ! Update visited file name of all affected buffers
        expanded_from_dir = aux_expand(from_dir)
        blist = buffer_list()
        do k = 1, size(blist)
            call resolve_buffer(blist(k), b, st)
            if (.not. st%ok()) cycle
            if (.not. allocated(buffers(b)%filename)) cycle
            if (.not. dired_in_this_tree_p(buffers(b)%filename, expanded_from_dir)) cycle
            call set_buffer(blist(k), st)
            call buffer_modified_p(blist(k), modflag, st)
            ! (replace-regexp-in-string (concat "^" (regexp-quote from-dir)) to-dir buffer-file-name)
            if (index(buffers(cur)%filename, from_dir) == 1) then
                to_file = to_dir//buffers(cur)%filename(len(from_dir) + 1:)
            else
                to_file = buffers(cur)%filename
            end if
            call set_visited_file_name(to_file, st)
            call restore_buffer_modified_p(modflag)
        end do
        call set_buffer(old, st)
    end subroutine dired_rename_subdir

    !> Rename DIR to TO in headerlines and `dired_subdir_alist', if DIR or
    !> one of its subdirectories is expanded in this buffer.  Port of
    !> dired-aux.el:dired-rename-subdir-1.
    recursive subroutine dired_rename_subdir_1(dir, to, status)
        character(len=*), intent(in) :: dir, to
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: expanded_dir, new_name, ddir, actual
        type(handle_type) :: existing
        integer :: k, i
        status = status_type()
        expanded_dir = aux_expand(dir)
        k = aux_local_index()
        if (k > 0) then
            if (allocated(locals(k)%subdir_alist)) then
                do i = 1, size(locals(k)%subdir_alist)
                    ! ELT's subdir is affected by the rename
                    if (dired_in_this_tree_p(locals(k)%subdir_alist(i)%dir, expanded_dir)) then
                        call dired_rename_subdir_2(i, dir, to, status)
                        if (.not. status%ok()) return
                    end if
                end do
            end if
        end if
        if (dir == aux_default_directory()) then
            ! if top level directory was renamed, lots of things have to be
            ! updated:
            ! we no longer dired DIR...
            call dired_unadvertise(dir)
            buffers(cur)%directory = to
            if (k > 0) then
                ddir = ""
                if (allocated(locals(k)%directory)) ddir = locals(k)%directory
                ! this is correct with and without wildcards
                locals(k)%directory = aux_expand_in(file_name_nondirectory(ddir), to)
                new_name = file_name_nondirectory(directory_file_name(locals(k)%directory))
                ! try to rename buffer, but just leave old name if new name
                ! would already exist (don't try appending "<%d>")
                existing = get_buffer(new_name)
                if (existing%index == 0) call rename_buffer(new_name, .false., actual, status)
            end if
            ! ... we dired TO now:
            call dired_advertise()
        end if
    end subroutine dired_rename_subdir_1

    !> `expand_file_name' of NAME in DIR as a function.
    function aux_expand_in(name, dir) result(full)
        character(len=*), intent(in) :: name, dir
        character(len=:), allocatable :: full
        type(status_type) :: st
        call expand_file_name(name, full, st, dir)
        if (.not. st%ok()) full = name
    end function aux_expand_in

    !> Update the headerline and `dired_subdir_alist' element I of the
    !> directory to reflect the moving of DIR to TO.  Thus, the element
    !> describes either DIR itself or a subdir of DIR.  Port of
    !> dired-aux.el:dired-rename-subdir-2 (no `dired_switches_alist').
    recursive subroutine dired_rename_subdir_2(i, dir, to, status)
        integer, intent(in) :: i
        character(len=*), intent(in) :: dir, to
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp, newtext, elt_dir
        type(value_type) :: saved_ro
        type(status_type) :: st
        integer(int64) :: opoint, b1, e1, found
        integer :: k
        status = status_type()
        k = aux_local_index()
        opoint = point()
        regexp = regexp_quote(directory_file_name(dir))
        newtext = directory_file_name(to)
        elt_dir = locals(k)%subdir_alist(i)%dir
        call aux_inhibit_read_only_begin(saved_ro)
        call goto_char(aux_mpos(locals(k)%subdir_alist(i)%marker))
        ! Update subdir headerline in buffer
        if (.not. looking_at("^. \(.+\)\(:\)"//LF, st)) then
            call set_error(status, FM_GENERIC_ERROR, dir//" not found where expected - dired_subdir_alist broken?")
        else
            b1 = match_beginning(1, st)
            e1 = match_end(1, st)
            call goto_char(b1)
            call re_search_forward(regexp, found, st, bound=e1, noerror=SEARCH_NOERROR)
            if (st%ok() .and. found /= 0) then
                call replace_match(newtext, st, fixedcase=.true., literal=.true.)
            else
                call set_error(status, FM_GENERIC_ERROR, format_message("Expected to find `")//dir// &
                               format_message("' in headerline of ")//elt_dir)
            end if
        end if
        call aux_inhibit_read_only_end(saved_ro)
        call goto_char(opoint)
        if (.not. status%ok()) return
        ! Update buffer-local dired-subdir-alist
        locals(k)%subdir_alist(i)%dir = dired_normalize_subdir(replace_all(elt_dir, directory_file_name(dir), newtext))
    end subroutine dired_rename_subdir_2

    !> Default is to create files with the same names as the originals.

    module procedure dired_hardlink
    call dired_handle_overwrite(newname)
    ! error is caught in -create-files
    call add_name_to_file(file, newname, status, ok_if_already_exists)
    if (.not. status%ok()) return
    ! Update the link count
    call dired_relist_file(file, status)
    end procedure dired_hardlink

    module procedure dired_make_relative_symlink
    character(len=:), allocatable :: f1, f2, name1, name2, sub, tem
    integer :: len1, len2, idx, next, start, cnt, k
    f1 = aux_expand(file1)
    f2 = aux_expand(file2)
    len1 = len(f1)
    len2 = len(f2)
    idx = 0
    ! Find common initial file name components:
    do
        k = index(f1(idx + 1:), "/")
        if (k == 0) exit
        next = idx + k
        ! For the comparison, both substrings must end in `/', so NEXT is
        ! *one plus* the result of the string-search.
        if (.not. (next < min(len1, len2))) exit
        if (f1(1:next) /= f2(1:next)) exit
        idx = next
    end do
    name2 = f2
    sub = f1(1:idx)
    name1 = f1(idx + 1:)
    if (sub == "/" .and. len(sub) == 1) then
        ! No common initial file name found
        name1 = f1
    else
        ! Else they have a common parent directory
        tem = f2(idx + 1:)
        start = 0
        cnt = 0
        ! Count number of slashes we must compensate for ...
        do
            k = index(tem(start + 1:), "/")
            if (k == 0) exit
            cnt = cnt + 1
            start = start + k
        end do
        ! ... and prepend a "../" for each slash found:
        do k = 1, cnt
            name1 = "../"//name1
        end do
    end if
    ! must not link to foo/ (trailing slash!)
    call make_symbolic_link(directory_file_name(name1), name2, status, ok_if_already_exists)
    end procedure dired_make_relative_symlink

    !> Make the symbolic link `dired_do_symlink' makes (a file creator:
    !> `make_symbolic_link' with FROM as the target).
    recursive subroutine symlink_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status
        call make_symbolic_link(from, to, status, ok_flag)
    end subroutine symlink_creator

    !> `add_name_to_file' as a file creator (dired-do-hardlink-regexp).
    recursive subroutine add_name_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status
        call add_name_to_file(from, to, status, ok_flag)
    end subroutine add_name_creator

    ! ------------------------------------------------------------------
    ! The basic function for half a dozen variations on cp/mv/ln/ln -s
    ! ------------------------------------------------------------------

    recursive module subroutine dired_create_files(file_creator, creator_kind, operation, fn_list, name_constructor, &
                                                   marker_char, status)
        procedure(dired_file_creator_proc) :: file_creator
        integer, intent(in) :: creator_kind
        character(len=*), intent(in) :: operation
        type(dired_string_type), intent(in) :: fn_list(:)
        procedure(dired_name_constructor_proc) :: name_constructor
        character, intent(in) :: marker_char
        type(status_type), intent(out) :: status
        type(dired_string_type), allocatable :: failures(:), skipped(:), all(:)
        type(dired_string_type), allocatable :: grown(:)
        character(len=:), allocatable :: from, to, destname, help, lower_op
        type(file_stat_type) :: dattrs
        type(status_type) :: st
        integer(int64) :: success_count, total, p, nf
        integer :: k, overwrite_query
        character :: actual_marker_char
        logical :: skip, overwrite, overwrite_confirmed, has_marker, found, ok
        status = status_type()
        if (allocated(create_files_failures)) deallocate (create_files_failures)
        allocate (create_files_failures(0), failures(0), skipped(0))
        success_count = 0
        total = size(fn_list)
        overwrite_query = 0
        lower_op = downcase_string(operation)
        do k = 1, size(fn_list)
            from = fn_list(k)%s
            ! Position point on the current file -- this is useful if
            ! handling a number of files to show where we're working at.
            p = dired_goto_file(from, st)
            call name_constructor(from, to, skip, status)
            if (.not. status%ok()) return
            if (.not. skip) then
                if (to == from .and. len(to) == len(from)) then
                    skip = .true.
                    call dired_log("Cannot "//lower_op//" to same file: "//from//LF)
                end if
            end if
            if (skip) then
                allocate (grown(size(skipped) + 1))
                grown(1)%s = dired_make_relative(from)
                grown(2:) = skipped
                call move_alloc(grown, skipped)
                cycle
            end if
            overwrite = file_exists_p(to)
            overwrite_confirmed = .false.
            if (overwrite) then
                help = substitute_command_keys("Type \`SPC' or \`y' to overwrite file `")//to// &
                       substitute_command_keys("',"//LF//"\`DEL' or \`n' to skip to next,"//LF// &
                                               "\`ESC' or \`q' to not overwrite any of the remaining "// &
                                               "files,"//LF//"\`!' to overwrite all remaining files "// &
                                               "with no more questions.")
                call dired_query(overwrite_query, format_message("Overwrite `")//to//format_message("'?"), &
                                 overwrite_confirmed, status, help_form=help)
                if (.not. status%ok()) return
            end if
            ! must determine if FROM is marked before file-creator gets a
            ! chance to delete it (in case of a move).
            has_marker = .false.
            if (marker_char == DIRED_KEEP_MARKER) then
                call dired_file_marker(from, actual_marker_char, has_marker)
            else if (marker_char /= " ") then
                actual_marker_char = marker_char
                has_marker = .true.
            end if
            ! Handle the `dired_copy_file' file-creator specially When copying
            ! a directory to another directory or possibly to itself or one of
            ! its subdirectories.
            destname = file_name_directory(to)
            if (file_directory_p(from) .and. file_directory_p(to) .and. creator_kind == DIRED_CREATOR_COPY) to = destname
            ! If DESTNAME is a subdirectory of FROM, not a symlink, and the
            ! method in use is copying, signal an error.
            call file_attributes(destname, dattrs, found, nofollow=.true.)
            if (found .and. creator_kind == DIRED_CREATOR_COPY) then
                if (iand(dattrs%mode, S_IFMT) == S_IFDIR) then
                    if (file_in_directory_p(destname, from)) then
                        call set_error(status, FM_GENERIC_ERROR, format_message("Cannot copy `")//from// &
                                       format_message("' into its subdirectory `")//to//format_message("'"))
                        return
                    end if
                end if
            end if
            call file_creator(from, to, overwrite_confirmed, st)
            ok = st%ok()
            if (ok) then
                ! If we get here, file-creator hasn't been aborted and the old
                ! entry (if any) has to be deleted before adding the new entry.
                if (overwrite) call dired_remove_file(to)
                success_count = success_count + 1
                call message(operation//": "//aux_itoa(success_count)//" of "//aux_itoa(total))
                if (has_marker) then
                    call dired_add_file(to, status, actual_marker_char)
                else
                    call dired_add_file(to, status)
                end if
                if (.not. status%ok()) return
            else if (condition_matches(st%code, FM_FILE_ERROR)) then
                ! FILE-CREATOR aborted
                allocate (grown(size(failures) + 1))
                grown(1)%s = dired_make_relative(from)
                grown(2:) = failures
                call move_alloc(grown, failures)
                call dired_log(operation//format_message(": `")//from//format_message("' to `")//to// &
                               format_message("' failed:")//LF//"("//error_name(st%code)//" "//format_error(st)//")"//LF)
            else
                status = st
                return
            end if
        end do
        if (size(create_files_failures) > 0) then
            allocate (all(size(failures) + size(create_files_failures)))
            all(1:size(failures)) = failures
            all(size(failures) + 1:) = create_files_failures
            nf = size(all)
            call dired_log_summary(operation//" failed for "//aux_itoa(nf)//ngettext(" file", " files", nf)//" in "// &
                                   aux_itoa(total)//" requests", all)
        else if (size(failures) > 0) then
            nf = size(failures)
            call dired_log_summary(operation//": "//aux_itoa(nf)//" of "//aux_itoa(total)//ngettext(" file", " files", total)// &
                                   " failed", failures)
        else if (size(skipped) > 0) then
            nf = size(skipped)
            call dired_log_summary(operation//": "//aux_itoa(nf)//" of "//aux_itoa(total)//ngettext(" file", " files", total)// &
                                   " skipped", skipped)
        else
            call message(operation//": "//aux_itoa(success_count)//ngettext(" file done", " files done", success_count))
        end if
        p = dired_move_to_filename(st)
    end subroutine dired_create_files

    !> The name constructor of a target directory: FROM's name in
    !> `create_target'.
    recursive subroutine into_dir_constructor(from, to, skip, status)
        character(len=*), intent(in) :: from
        character(len=:), allocatable, intent(out) :: to
        logical, intent(out) :: skip
        type(status_type), intent(out) :: status
        status = status_type()
        skip = .false.
        to = aux_expand_in(file_name_nondirectory(from), create_target)
    end subroutine into_dir_constructor

    !> The name constructor of a single target: `create_target'.
    recursive subroutine target_constructor(from, to, skip, status)
        character(len=*), intent(in) :: from
        character(len=:), allocatable, intent(out) :: to
        logical, intent(out) :: skip
        type(status_type), intent(out) :: status
        status = status_type()
        skip = .false.
        to = create_target
        if (len(from) < 0) skip = .true.
    end subroutine target_constructor

    !> The target directory the user may want: if `dired_dwim_target' is
    !> set and a Dired buffer is displayed in one of the next windows, its
    !> current subdir, else the current subdir of this Dired buffer.  Port
    !> of dired-aux.el:dired-dwim-target-directory.
    function dired_dwim_target_directory() result(dir)
        character(len=:), allocatable :: dir
        type(dired_string_type), allocatable :: dirs(:)
        type(status_type) :: st
        dir = ""
        if (allocated(buffers(cur)%major_mode)) then
            if (buffers(cur)%major_mode == "dired_mode") dir = dired_current_directory(st)
        end if
        ! non-dired buffer may want to profit from this function, e.g.
        ! vm-uudecode
        if (dired_dwim_target /= DIRED_DWIM_TARGET_NIL) then
            call dired_dwim_target_directories(dirs)
            if (size(dirs) > 0) dir = dirs(1)%s
        end if
    end function dired_dwim_target_directory

    !> The directories of the Dired buffers in the other windows, as
    !> `dired_dwim_target' chooses them.  Port of
    !> dired-aux.el:dired-dwim-target-directories with
    !> dired-dwim-target-next, -next-visible and -recent (one frame; a
    !> function as `dired_dwim_target' is deferred).
    subroutine dired_dwim_target_directories(dirs)
        type(dired_string_type), allocatable, intent(out) :: dirs(:)
        type(handle_type), allocatable :: leaves(:), order(:)
        type(handle_type) :: sel, old
        type(status_type) :: st
        integer(int64), allocatable :: times(:)
        integer(int64) :: b, tk
        type(dired_string_type) :: td
        integer :: k, n, first, j
        allocate (dirs(0))
        call window_leaves(selected_frame%root_window, leaves)
        sel = selected_window()
        n = size(leaves)
        first = 0
        do k = 1, n
            if (same_handle(leaves(k), sel)) first = k
        end do
        ! (window-list-1 (next-window nil 'nomini) 'nomini): the windows
        ! after the selected one, cyclically, without it.
        allocate (order(0))
        do j = 1, n - 1
            k = mod(first + j - 1, n) + 1
            if (.not. same_handle(leaves(k), sel)) order = [order, leaves(k)]
        end do
        if (first == 0) order = leaves
        old = current_buffer()
        allocate (times(0))
        do k = 1, size(order)
            call resolve_buffer(windows(order(k)%index)%buffer, b, st)
            if (.not. st%ok()) cycle
            if (.not. allocated(buffers(b)%major_mode)) cycle
            if (buffers(b)%major_mode /= "dired_mode") cycle
            call set_buffer(windows(order(k)%index)%buffer, st)
            call push_string(dirs, dired_current_directory(st))
            times = [times, windows(order(k)%index)%use_time]
        end do
        call set_buffer(old, st)
        if (dired_dwim_target == DIRED_DWIM_TARGET_RECENT) then
            ! Ordered by most-recently-used.
            do k = 2, size(dirs)
                j = k
                do while (j > 1)
                    if (times(j - 1) >= times(j)) exit
                    tk = times(j)
                    times(j) = times(j - 1)
                    times(j - 1) = tk
                    td = dirs(j)
                    dirs(j) = dirs(j - 1)
                    dirs(j - 1) = td
                    j = j - 1
                end do
            end do
        end if
    end subroutine dired_dwim_target_directories

    recursive module subroutine dired_do_create_files(op_symbol, file_creator, creator_kind, operation, arg_kind, arg_n, &
                                                      marker_char, status, op1, how_to_plain)
        character(len=*), intent(in) :: op_symbol, operation
        procedure(dired_file_creator_proc) :: file_creator
        integer, intent(in) :: creator_kind, arg_kind
        integer(int64), intent(in) :: arg_n
        character, intent(in) :: marker_char
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: op1
        logical, intent(in), optional :: how_to_plain
        type(dired_string_type), allocatable :: fn_list(:), rfn_list(:)
        character(len=:), allocatable :: target_dir, default, target, input, op, prompt
        type(handle_type), allocatable :: bufs(:)
        type(handle_type) :: old
        type(status_type) :: st
        logical :: one_file, into_dir
        integer :: k
        call dired_get_marked_files(fn_list, status, arg_kind=arg_kind, arg_n=arg_n, error="")
        if (.not. status%ok()) return
        allocate (rfn_list(size(fn_list)))
        do k = 1, size(fn_list)
            rfn_list(k)%s = dired_make_relative(fn_list(k)%s)
        end do
        one_file = size(fn_list) == 1
        target_dir = dired_dwim_target_directory()
        ! The first of dired-dwim-target-defaults: TARGET-DIR, else the
        ! current directory (the other defaults, for M-n, are deferred:
        ! `read_file_name' takes one default).
        default = target_dir
        if (len(default) == 0) default = dired_current_directory(st)
        op = operation
        if (one_file .and. present(op1)) op = op1
        if (op_symbol == "symlink" .or. op_symbol == "hardlink") then
            ! Linking operations create links from the prompted file name;
            ! the other operations copy (etc) to the prompted file name.
            prompt = op//" %s from: "
        else
            prompt = op//" %s to: "
        end if
        call dired_mark_read_file_name(prompt, target_dir, op_symbol, arg_kind, arg_n, rfn_list, input, status, default)
        if (.not. status%ok()) return
        target = aux_expand(input)
        if (.not. one_file .or. (dired_create_destination_dirs_on_trailing_dirsep .and. directory_name_p(target))) then
            call dired_maybe_create_dirs(target, status)
            if (.not. status%ok()) return
        end if
        if (present(how_to_plain)) then
            into_dir = .not. how_to_plain .and. file_directory_p(target)
        else
            into_dir = file_directory_p(target)
        end if
        if (.not. (one_file .or. into_dir)) then
            call set_error(status, FM_GENERIC_ERROR, "Marked "//operation//": target must be a directory: "//target)
            return
        end if
        if (.not. file_directory_p(fn_list(1)%s) .and. .not. file_directory_p(target) .and. directory_name_p(target)) then
            call set_error(status, FM_GENERIC_ERROR, operation//": Target directory does not exist: "//target)
            return
        end if
        ! rename-file bombs when moving directories unless we do this:
        if (.not. into_dir) target = directory_file_name(target)
        create_target = target
        if (into_dir) then
            call dired_create_files(file_creator, creator_kind, operation, fn_list, into_dir_constructor, marker_char, status)
        else
            call dired_create_files(file_creator, creator_kind, operation, fn_list, target_constructor, marker_char, status)
        end if
        if (.not. status%ok()) return
        if (dired_do_revert_buffer) then
            bufs = dired_buffers_for_dir(file_name_directory(target))
            old = current_buffer()
            do k = 1, size(bufs)
                call set_buffer(bufs(k), st)
                call revert_buffer(status)
                if (.not. status%ok()) exit
            end do
            call set_buffer(old, st)
        end if
    end subroutine dired_do_create_files

    !> `dired_copy_file' as a file creator.
    recursive subroutine copy_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status
        call dired_copy_file(from, to, ok_flag, status)
    end subroutine copy_creator

    !> `dired_rename_file' as a file creator.
    recursive subroutine rename_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status
        call dired_rename_file(from, to, ok_flag, status)
    end subroutine rename_creator

    !> `dired_hardlink' as a file creator.
    recursive subroutine hardlink_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status
        call dired_hardlink(from, to, ok_flag, status)
    end subroutine hardlink_creator

    !> `dired_make_relative_symlink' as a file creator.
    recursive subroutine relsymlink_creator(from, to, ok_flag, status)
        character(len=*), intent(in) :: from, to
        logical, intent(in) :: ok_flag
        type(status_type), intent(out) :: status
        call dired_make_relative_symlink(from, to, status, ok_flag)
    end subroutine relsymlink_creator

    module procedure dired_do_copy
    logical :: saved_dereference
    integer :: saved_recursive
    ! (let ((dired-recursive-copies dired-recursive-copies)
    !       (dired-copy-dereference (if (equal arg '(4)) ...))) ...)
    saved_recursive = dired_recursive_copies
    saved_dereference = dired_copy_dereference
    if (present(invert_dereference)) then
        if (invert_dereference) dired_copy_dereference = .not. dired_copy_dereference
    end if
    call dired_do_create_files("copy", copy_creator, DIRED_CREATOR_COPY, "Copy", arg_kind, arg_n, &
                               dired_keep_marker_copy, status)
    dired_recursive_copies = saved_recursive
    dired_copy_dereference = saved_dereference
    end procedure dired_do_copy

    !> Copy all marked (or next ARG) files, or copy the current file.
    !> ARG has to be numeric for above functionality.  See
    !> `dired_get_marked_files' for more details.
    !>
    !> When operating on just the current file, prompt for the new name.
    !>
    !> When operating on multiple or marked files, prompt for a target
    !> directory, and make the new copies in that directory, with the
    !> same names as the original files.  The initial suggestion for the
    !> target directory is the Dired buffer's current directory (or, if
    !> `dired_dwim_target' is set, the current directory of a
    !> neighboring Dired window).
    !>
    !> If `dired_copy_preserve_time' is true, this command preserves
    !> the modification time of each old file in the copy, similar to
    !> the "-p" option for the "cp" shell command.
    !>
    !> The `dired_keep_marker_copy' user option controls how this
    !> command handles file marking.  The default is to mark all new
    !> copies of files with a "C" mark.
    !>
    !> This command copies symbolic links by creating new ones,
    !> similar to the "-d" option for the "cp" shell command.
    !> But if `dired_copy_dereference' is true, the symbolic
    !> links are dereferenced and then copied, similar to the "-L"
    !> option for the "cp" shell command.  If ARG is
    !> \[universal_argument] alone, the inverted value of
    !> `dired_copy_dereference' will be used.
    !>
    !> Also see `dired_do_revert_buffer'.  Port of
    !> dired-aux.el:dired-do-copy (interactive; `dired_copy_how_to_fn' is
    !> deferred).
    recursive subroutine cmd_dired_do_copy(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        logical :: invert
        call aux_prefix_kind(kind, n)
        invert = .false.
        if (consp(current_prefix_arg)) invert = prefix_numeric_value(current_prefix_arg) == 4
        call dired_do_copy(kind, n, status, invert)
    end subroutine cmd_dired_do_copy

    module procedure dired_do_symlink
    call dired_do_create_files("symlink", symlink_creator, DIRED_CREATOR_SYMLINK, "Symlink", arg_kind, arg_n, &
                               dired_keep_marker_symlink, status)
    end procedure dired_do_symlink

    !> Make symbolic links to current file or all marked (or next ARG) files.
    !> When operating on just the current file, you specify the new name.
    !> When operating on multiple or marked files, you specify a directory
    !> and new symbolic links are made in that directory
    !> with the same names that the files currently have.  The default
    !> suggested for the target directory depends on the value of
    !> `dired_dwim_target', which see.
    !>
    !> For relative symlinks, use \[dired_do_relsymlink].
    !>
    !> Also see `dired_do_revert_buffer'.  Port of
    !> dired-aux.el:dired-do-symlink (interactive).
    recursive subroutine cmd_dired_do_symlink(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_do_symlink(kind, n, status)
    end subroutine cmd_dired_do_symlink

    module procedure dired_do_relsymlink
    call dired_do_create_files("relsymlink", relsymlink_creator, DIRED_CREATOR_OTHER, "RelSymLink", arg_kind, arg_n, &
                               dired_keep_marker_relsymlink, status)
    end procedure dired_do_relsymlink

    !> Relative symlink all marked (or next ARG) files into a directory.
    !> Otherwise make a relative symbolic link to the current file.
    !> This creates relative symbolic links like
    !>
    !>     foo -> ../bar/foo
    !>
    !> not absolute ones like
    !>
    !>     foo -> /ugly/file/name/that/may/change/any/day/bar/foo
    !>
    !> For absolute symlinks, use \[dired_do_symlink].  Port of
    !> dired-aux.el:dired-do-relsymlink (interactive).
    recursive subroutine cmd_dired_do_relsymlink(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_do_relsymlink(kind, n, status)
    end subroutine cmd_dired_do_relsymlink

    module procedure dired_do_hardlink
    call dired_do_create_files("hardlink", hardlink_creator, DIRED_CREATOR_OTHER, "Hardlink", arg_kind, arg_n, &
                               dired_keep_marker_hardlink, status)
    end procedure dired_do_hardlink

    !> Add names (hard links) current file or all marked (or next ARG) files.
    !> When operating on just the current file, you specify the new name.
    !> When operating on multiple or marked files, you specify a directory
    !> and new hard links are made in that directory
    !> with the same names that the files currently have.  The default
    !> suggested for the target directory depends on the value of
    !> `dired_dwim_target', which see.
    !>
    !> Also see `dired_do_revert_buffer'.  Port of
    !> dired-aux.el:dired-do-hardlink (interactive).
    recursive subroutine cmd_dired_do_hardlink(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_do_hardlink(kind, n, status)
    end subroutine cmd_dired_do_hardlink

    module procedure dired_do_rename
    type(dired_string_type), allocatable :: files(:)
    character(len=:), allocatable :: name
    integer :: k
    call dired_get_marked_files(files, status, arg_kind=arg_kind, arg_n=arg_n)
    if (.not. status%ok()) return
    do k = 1, size(files)
        name = file_name_nondirectory(files(k)%s)
        if (name == "." .or. name == "..") then
            call set_error(status, FM_USER_ERROR, "Can't rename ""."" or "".."" files")
            return
        end if
    end do
    call dired_do_create_files("move", rename_creator, DIRED_CREATOR_OTHER, "Move", arg_kind, arg_n, &
                               dired_keep_marker_rename, status, op1="Rename")
    end procedure dired_do_rename

    !> Rename current file or all marked (or next ARG) files.
    !> When renaming just the current file, you specify the new name.
    !> When renaming multiple or marked files, you specify a directory.
    !> This command also renames any buffers that are visiting the files.
    !> The default suggested for the target directory depends on the value
    !> of `dired_dwim_target', which see.
    !>
    !> Also see `dired_do_revert_buffer'.  Port of
    !> dired-aux.el:dired-do-rename (interactive; `dired_vc_rename_file' is
    !> deferred).
    recursive subroutine cmd_dired_do_rename(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_do_rename(kind, n, status)
    end subroutine cmd_dired_do_rename

    ! ------------------------------------------------------------------
    ! Operate on files matched by regexp
    ! ------------------------------------------------------------------

    !> Replace first match of REGEXP in STRING with NEWTEXT (NEW; FOUND
    !> false if it does not match).  Port of
    !> dired.el:dired-string-replace-match (LITERAL and GLOBAL nil).
    recursive subroutine dired_string_replace_match(regexp, string, newtext, new, found, status)
        character(len=*), intent(in) :: regexp, string, newtext
        character(len=:), allocatable, intent(out) :: new
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        new = ""
        found = string_match(regexp, string, status) > 0
        if (.not. status%ok() .or. .not. found) then
            found = .false.
            return
        end if
        call replace_match(newtext, status, fixedcase=.true., literal=.false., string=string, result=new)
        if (.not. status%ok()) found = .false.
    end subroutine dired_string_replace_match

    !> The name constructor of `dired_do_create_files_regexp'.
    recursive subroutine regexp_name_constructor(from, to, skip, status)
        character(len=*), intent(in) :: from
        character(len=:), allocatable, intent(out) :: to
        logical, intent(out) :: skip
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: new, prompt
        logical :: found, answer
        skip = .true.
        to = ""
        if (regexp_whole_name) then
            ! easy (but rare) case
            call dired_string_replace_match(regexp_regexp, from, regexp_newname, new, found, status)
            if (.not. status%ok()) return
            if (found) then
                to = new
                prompt = regexp_operation//format_message(" `")//from//format_message("' to `")//to// &
                         format_message("'?")
                call dired_query(regexp_query, prompt, answer, status, help_form=regexp_help)
                if (.not. status%ok()) return
                skip = .not. answer
            else
                call dired_log(regexp_operation//": "//from//" did not match regexp "//regexp_regexp//LF)
            end if
        else
            ! not whole-name, replace non-directory part only
            call dired_string_replace_match(regexp_regexp, file_name_nondirectory(from), regexp_newname, new, found, &
                                            status)
            if (.not. status%ok()) return
            if (found) then
                ! nil means there was no match
                to = aux_expand_in(new, file_name_directory(from))
                prompt = regexp_operation//format_message(" `")//dired_make_relative(from)// &
                         format_message("' to `")//dired_make_relative(to)//format_message("'?")
                call dired_query(regexp_query, prompt, answer, status, help_form=regexp_help)
                if (.not. status%ok()) return
                skip = .not. answer
            else
                call dired_log(regexp_operation//": "//file_name_nondirectory(from)//" did not match regexp "// &
                               regexp_regexp//LF)
            end if
        end if
    end subroutine regexp_name_constructor

    !> Create a new file for each marked file using regexps.
    !> FILE_CREATOR and OPERATION as in `dired_create_files'.
    !> ARG as in `dired_get_marked_files'.
    !> Matches each marked file against REGEXP and constructs the new
    !> filename from NEWNAME (like in function `replace_match').
    !> WHOLE_NAME means match/replace the whole file name instead of only
    !> the non-directory part of the file.
    !> MARKER_CHAR as in `dired_create_files'.  Port of
    !> dired-aux.el:dired-do-create-files-regexp.
    recursive subroutine dired_do_create_files_regexp(file_creator, creator_kind, operation, arg_kind, arg_n, &
                                                      regexp, newname, whole_name, marker_char, status)
        procedure(dired_file_creator_proc) :: file_creator
        integer, intent(in) :: creator_kind, arg_kind
        character(len=*), intent(in) :: operation, regexp, newname
        integer(int64), intent(in) :: arg_n
        logical, intent(in) :: whole_name
        character, intent(in) :: marker_char
        type(status_type), intent(out) :: status
        type(dired_string_type), allocatable :: fn_list(:)
        character(len=:), allocatable :: lower
        call dired_get_marked_files(fn_list, status, arg_kind=arg_kind, arg_n=arg_n)
        if (.not. status%ok()) return
        lower = downcase_string(operation)
        regexp_help = substitute_command_keys("Type \`SPC' or \`y' to ")//lower// &
                      substitute_command_keys(" one match, \`DEL' or \`n' to skip to next,"//LF//"\`!' to ")//lower// &
                      " all remaining matches with no more questions."
        regexp_operation = operation
        regexp_regexp = regexp
        regexp_newname = newname
        regexp_whole_name = whole_name
        regexp_query = 0
        call dired_create_files(file_creator, creator_kind, operation, fn_list, regexp_name_constructor, marker_char, &
                                status)
    end subroutine dired_do_create_files_regexp

    !> Prompt user about performing OPERATION.  Read the REGEXP and
    !> NEWNAME, and the ARG and WHOLE_NAME of the regexp commands from the
    !> prefix argument.  Port of dired-aux.el:dired-mark-read-regexp.
    recursive subroutine dired_mark_read_regexp(operation, regexp, newname, arg_kind, arg_n, whole_name, status)
        character(len=*), intent(in) :: operation
        character(len=:), allocatable, intent(out) :: regexp, newname
        integer, intent(out) :: arg_kind
        integer(int64), intent(out) :: arg_n
        logical, intent(out) :: whole_name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: abs
        whole_name = prefix_numeric_value(current_prefix_arg) == 0 .and. .not. nilp(current_prefix_arg)
        if (whole_name) then
            arg_kind = DIRED_ARG_NIL
            arg_n = 0
            abs = "Abs. "
        else
            call aux_prefix_kind(arg_kind, arg_n)
            abs = ""
        end if
        newname = ""
        call read_regexp(abs//operation//" from (regexp): ", regexp, status, history="dired_regexp_history")
        if (.not. status%ok()) return
        call read_string(abs//operation//" "//regexp//" to: ", newname, status)
    end subroutine dired_mark_read_regexp

    module procedure dired_do_rename_regexp
    call dired_do_create_files_regexp(rename_creator, DIRED_CREATOR_OTHER, "Rename", arg_kind, arg_n, regexp, newname, &
                                      whole_name, dired_keep_marker_rename, status)
    end procedure dired_do_rename_regexp

    !> Rename selected files whose names match REGEXP to NEWNAME.
    !>
    !> With non-zero prefix argument ARG, the command operates on the next ARG
    !> files.  Otherwise, it operates on all the marked files, or the current
    !> file if none are marked.
    !>
    !> As each match is found, the user must type a character saying
    !>   what to do with it.  For directions, type \[help_command] at that time.
    !> NEWNAME may contain \<n> or \& as in `query_replace_regexp'.
    !> REGEXP defaults to the last regexp used.
    !>
    !> With a zero prefix arg, renaming by regexp affects the absolute file name.
    !> Normally, only the non-directory part of the file name is used and changed.
    !> Port of dired-aux.el:dired-do-rename-regexp (interactive).
    recursive subroutine cmd_dired_do_rename_regexp(status)
        type(status_type), intent(out) :: status
        call regexp_command("Rename", status)
    end subroutine cmd_dired_do_rename_regexp

    !> Read the arguments of a regexp command (OPERATION) and run it.
    recursive subroutine regexp_command(operation, status)
        character(len=*), intent(in) :: operation
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp, newname
        integer :: kind
        integer(int64) :: n
        logical :: whole
        call dired_mark_read_regexp(operation, regexp, newname, kind, n, whole, status)
        if (.not. status%ok()) return
        select case (operation)
        case ("Rename")
            call dired_do_rename_regexp(regexp, newname, kind, n, whole, status)
        case ("Copy")
            call dired_do_copy_regexp(regexp, newname, kind, n, whole, status)
        case ("HardLink")
            call dired_do_hardlink_regexp(regexp, newname, kind, n, whole, status)
        case ("SymLink")
            call dired_do_symlink_regexp(regexp, newname, kind, n, whole, status)
        case default
            call dired_do_relsymlink_regexp(regexp, newname, kind, n, whole, status)
        end select
    end subroutine regexp_command

    module procedure dired_do_copy_regexp
    integer :: saved
    ! No recursive copies.
    saved = dired_recursive_copies
    dired_recursive_copies = DIRED_RECURSIVE_NEVER
    if (dired_copy_preserve_time) then
        call dired_do_create_files_regexp(copy_creator, DIRED_CREATOR_COPY, "Copy [-p]", arg_kind, arg_n, regexp, &
                                          newname, whole_name, dired_keep_marker_copy, status)
    else
        call dired_do_create_files_regexp(copy_creator, DIRED_CREATOR_COPY, "Copy", arg_kind, arg_n, regexp, newname, &
                                          whole_name, dired_keep_marker_copy, status)
    end if
    dired_recursive_copies = saved
    end procedure dired_do_copy_regexp

    !> Copy selected files whose names match REGEXP to NEWNAME.
    !> See function `dired_do_rename_regexp' for more info.  Port of
    !> dired-aux.el:dired-do-copy-regexp (interactive).
    recursive subroutine cmd_dired_do_copy_regexp(status)
        type(status_type), intent(out) :: status
        call regexp_command("Copy", status)
    end subroutine cmd_dired_do_copy_regexp

    module procedure dired_do_hardlink_regexp
    call dired_do_create_files_regexp(add_name_creator, DIRED_CREATOR_OTHER, "HardLink", arg_kind, arg_n, regexp, &
                                      newname, whole_name, dired_keep_marker_hardlink, status)
    end procedure dired_do_hardlink_regexp

    !> Hardlink selected files whose names match REGEXP to NEWNAME.
    !> See function `dired_do_rename_regexp' for more info.  Port of
    !> dired-aux.el:dired-do-hardlink-regexp (interactive).
    recursive subroutine cmd_dired_do_hardlink_regexp(status)
        type(status_type), intent(out) :: status
        call regexp_command("HardLink", status)
    end subroutine cmd_dired_do_hardlink_regexp

    module procedure dired_do_symlink_regexp
    call dired_do_create_files_regexp(symlink_creator, DIRED_CREATOR_SYMLINK, "SymLink", arg_kind, arg_n, regexp, &
                                      newname, whole_name, dired_keep_marker_symlink, status)
    end procedure dired_do_symlink_regexp

    !> Symlink selected files whose names match REGEXP to NEWNAME.
    !> See function `dired_do_rename_regexp' for more info.  Port of
    !> dired-aux.el:dired-do-symlink-regexp (interactive).
    recursive subroutine cmd_dired_do_symlink_regexp(status)
        type(status_type), intent(out) :: status
        call regexp_command("SymLink", status)
    end subroutine cmd_dired_do_symlink_regexp

    module procedure dired_do_relsymlink_regexp
    call dired_do_create_files_regexp(relsymlink_creator, DIRED_CREATOR_OTHER, "RelSymLink", arg_kind, arg_n, regexp, &
                                      newname, whole_name, dired_keep_marker_relsymlink, status)
    end procedure dired_do_relsymlink_regexp

    !> RelSymlink all marked files containing REGEXP to NEWNAME.
    !> See functions `dired_do_rename_regexp' and `dired_do_relsymlink'
    !> for more info.  Port of dired-aux.el:dired-do-relsymlink-regexp
    !> (interactive).
    recursive subroutine cmd_dired_do_relsymlink_regexp(status)
        type(status_type), intent(out) :: status
        call regexp_command("RelSymLink", status)
    end subroutine cmd_dired_do_relsymlink_regexp

    ! ------------------------------------------------------------------
    ! Change case of file names
    ! ------------------------------------------------------------------

    !> The name constructor of `dired_create_files_non_directory'.
    recursive subroutine non_directory_constructor(from, to, skip, status)
        character(len=*), intent(in) :: from
        character(len=:), allocatable, intent(out) :: to
        logical, intent(out) :: skip
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: prompt
        logical :: answer
        if (non_directory_upcase) then
            to = file_name_directory(from)//upcase_string(file_name_nondirectory(from))
        else
            to = file_name_directory(from)//downcase_string(file_name_nondirectory(from))
        end if
        prompt = non_directory_operation//format_message(" `")//dired_make_relative(from)//format_message("' to `")// &
                 dired_make_relative(to)//format_message("'")
        call dired_query(non_directory_query, prompt, answer, status, help_form=non_directory_help)
        skip = .not. (status%ok() .and. answer)
    end subroutine non_directory_constructor

    !> Perform `dired_rename_file' on the non-directory part of marked
    !> files using upcase (UPCASE) or downcase, with query for each file.
    !> OPERATION like in `dired_create_files', ARG as in
    !> `dired_get_marked_files'.  Port of
    !> dired-aux.el:dired-create-files-non-directory with
    !> dired-rename-non-directory.
    recursive subroutine dired_rename_non_directory(upcase, operation, arg_kind, arg_n, status)
        logical, intent(in) :: upcase
        character(len=*), intent(in) :: operation
        integer, intent(in) :: arg_kind
        integer(int64), intent(in) :: arg_n
        type(status_type), intent(out) :: status
        type(dired_string_type), allocatable :: fn_list(:)
        character(len=:), allocatable :: lower
        call dired_get_marked_files(fn_list, status, arg_kind=arg_kind, arg_n=arg_n)
        if (.not. status%ok()) return
        lower = downcase_string(operation)
        non_directory_help = substitute_command_keys("Type \`SPC' or \`y' to ")//lower// &
                             substitute_command_keys(" one file, \`DEL' or \`n' to skip to next,"//LF//"\`!' to ")// &
                             lower//" all remaining matches with no more questions."
        non_directory_upcase = upcase
        non_directory_operation = operation
        non_directory_query = 0
        call dired_create_files(rename_creator, DIRED_CREATOR_OTHER, operation, fn_list, non_directory_constructor, &
                                dired_keep_marker_rename, status)
    end subroutine dired_rename_non_directory

    module procedure dired_upcase
    call dired_rename_non_directory(.true., "Rename upcase", arg_kind, arg_n, status)
    end procedure dired_upcase

    !> Rename all marked (or next ARG) files to upper case.  Port of
    !> dired-aux.el:dired-upcase (interactive).
    recursive subroutine cmd_dired_upcase(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_upcase(kind, n, status)
    end subroutine cmd_dired_upcase

    module procedure dired_downcase
    call dired_rename_non_directory(.false., "Rename downcase", arg_kind, arg_n, status)
    end procedure dired_downcase

    !> Rename all marked (or next ARG) files to lower case.  Port of
    !> dired-aux.el:dired-downcase (interactive).
    recursive subroutine cmd_dired_downcase(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n
        call aux_prefix_kind(kind, n)
        call dired_downcase(kind, n, status)
    end subroutine cmd_dired_downcase

    ! ------------------------------------------------------------------
    ! Creating files, viewing them and their types
    ! ------------------------------------------------------------------

    module procedure dired_create_empty_file
    character(len=:), allocatable :: expanded, new
    integer(int64) :: pos
    call expand_file_name(file, expanded, status)
    if (.not. status%ok()) return
    if (file_exists_p(expanded)) then
        call set_error(status, FM_GENERIC_ERROR, "Cannot create file "//expanded//": file exists")
        return
    end if
    new = find_topmost_parent_dir(expanded)
    call make_empty_file(file, status, parents=.true.)
    if (.not. status%ok()) return
    if (len(new) > 0) then
        call dired_add_file(new, status)
        if (.not. status%ok()) return
        pos = dired_move_to_filename(status)
    end if
    end procedure dired_create_empty_file

    !> Create an empty file called FILE.
    !> Parent directories of FILE are created as needed.
    !> Add an entry in the Dired buffer for the topmost new parent
    !> directory of FILE, if created, otherwise for the new file.
    !> If user option `dired_create_empty_file_in_current_directory' is
    !> true, act on the current subdirectory by default, otherwise act on
    !> the default directory by default.
    !> If FILE already exists, signal an error.  Port of
    !> dired-aux.el:dired-create-empty-file (interactive).
    recursive subroutine cmd_dired_create_empty_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, dir
        if (dired_create_empty_file_in_current_directory) then
            dir = dired_current_directory(status)
            if (.not. status%ok()) return
            call read_file_name("Create empty file: ", file, status, dir=dir)
        else
            call read_file_name("Create empty file: ", file, status)
        end if
        if (.not. status%ok()) return
        call dired_create_empty_file(file, status)
    end subroutine cmd_dired_create_empty_file

    module procedure dired_view_file
    character(len=:), allocatable :: file
    integer :: k
    logical :: shown
    call dired_get_file_for_visit(file, status)
    if (.not. status%ok()) return
    if (file_directory_p(file)) then
        shown = .false.
        k = aux_local_index()
        if (k > 0) then
            if (allocated(locals(k)%subdir_alist)) then
                if (size(locals(k)%subdir_alist) > 1) shown = dired_goto_subdir(file) /= 0
            end if
        end if
        if (.not. shown) call dired(file, status)
    else
        call view_file(file, status)
    end if
    end procedure dired_view_file

    !> In Dired, examine a file in view mode, returning to Dired when done.
    !> When file is a directory, show it in this buffer if it is inserted.
    !> Otherwise, display it in another buffer.  Port of
    !> dired.el:dired-view-file (interactive).
    recursive subroutine cmd_dired_view_file(status)
        type(status_type), intent(out) :: status
        call dired_view_file(status)
    end subroutine cmd_dired_view_file

    module procedure dired_show_file_type
    type(handle_type) :: old, tmp
    type(value_type) :: res, args
    character(len=:), allocatable :: dir, text
    type(status_type) :: st
    dir = aux_default_directory()
    old = current_buffer()
    call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, status)
    if (.not. status%ok()) return
    call set_buffer(tmp, status)
    if (.not. status%ok()) return
    buffers(cur)%directory = dir
    if (deref_symlinks) then
        args = list(make_string("-L"), make_string("--"), make_string(file))
    else
        args = list(make_string("--"), make_string(file))
    end if
    call call_process("file", res, status, destination=t_value(), args=args)
    if (status%ok()) then
        if (bolp() .and. point() > point_min()) call delete_char(-1_int64, st)
        call buffer_substring(point_min(), point_max(), text, st)
    end if
    call set_buffer(old, st)
    call kill_buffer(tmp, st)
    if (status%ok()) call message(text)
    end procedure dired_show_file_type

    !> Print the type of FILE, according to the `file' command.
    !> If you give a prefix to this command, and FILE is a symbolic
    !> link, then the command will print the type of the target of the link
    !> instead.  Port of dired-aux.el:dired-show-file-type (interactive).
    recursive subroutine cmd_dired_show_file_type(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        logical :: found
        call dired_get_filename(file, found, status, DIRED_LOCALP_T)
        if (.not. status%ok()) return
        if (.not. found) then
            call set_error(status, FM_GENERIC_ERROR, "No file on this line")
            return
        end if
        call dired_show_file_type(file,.not. nilp(current_prefix_arg), status)
    end subroutine cmd_dired_show_file_type

    !> The first program named NAME on PATH (as executable-find finds it
    !> with `exec_path' from $PATH), or "" if none.
    function find_on_path(name) result(path)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path
        character(len=:), allocatable :: env, dir
        integer :: length, stat, start, k
        path = ""
        call get_environment_variable("PATH", length=length, status=stat)
        if (stat /= 0 .or. length == 0) return
        allocate (character(len=length) :: env)
        call get_environment_variable("PATH", env)
        start = 1
        do while (start <= len(env) + 1)
            k = index(env(start:), ":")
            if (k == 0) then
                dir = env(start:)
                start = len(env) + 2
            else
                dir = env(start:start + k - 2)
                start = start + k
            end if
            if (len(dir) == 0) dir = "."
            if (file_executable_p(file_name_as_directory(dir)//name)) then
                if (.not. file_directory_p(file_name_as_directory(dir)//name)) then
                    path = file_name_as_directory(dir)//name
                    return
                end if
            end if
        end do
    end function find_on_path

    !> Add a rule to `dired_compress_file_suffixes'.
    subroutine add_compress_suffix(regexp, new_suffix, program)
        character(len=*), intent(in) :: regexp, new_suffix, program
        type(dired_compress_suffix_type), allocatable :: grown(:)
        integer :: n
        n = size(dired_compress_file_suffixes)
        allocate (grown(n + 1))
        grown(1:n) = dired_compress_file_suffixes
        grown(n + 1)%regexp = regexp
        grown(n + 1)%new_suffix = new_suffix
        grown(n + 1)%program = program
        call move_alloc(grown, dired_compress_file_suffixes)
    end subroutine add_compress_suffix

    !> Add a rule (REGEXP . COMMAND) to ALIST.
    subroutine add_compress_rule(alist, regexp, command)
        type(dired_compress_rule_type), allocatable, intent(inout) :: alist(:)
        character(len=*), intent(in) :: regexp, command
        type(dired_compress_rule_type), allocatable :: grown(:)
        integer :: n
        n = size(alist)
        allocate (grown(n + 1))
        grown(1:n) = alist
        grown(n + 1)%regexp = regexp
        grown(n + 1)%command = command
        call move_alloc(grown, alist)
    end subroutine add_compress_rule

    !> Give the dired-aux.el options that are not set yet their standard
    !> values.
    subroutine init_dired_aux_options()
        if (.not. allocated(dired_chown_program)) then
            ! (cond ((executable-find "chown") "chown") ...)
            if (len(find_on_path("chown")) > 0) then
                dired_chown_program = "chown"
            else if (file_executable_p("/usr/sbin/chown")) then
                dired_chown_program = "/usr/sbin/chown"
            else if (file_executable_p("/etc/chown")) then
                dired_chown_program = "/etc/chown"
            else
                dired_chown_program = "chown"
            end if
        end if
        if (.not. allocated(dired_touch_program)) dired_touch_program = "touch"
        if (.not. allocated(dired_compress_file_default_suffix)) dired_compress_file_default_suffix = ""
        if (.not. allocated(dired_compress_directory_default_suffix)) dired_compress_directory_default_suffix = ""
        if (.not. allocated(dired_compress_file_suffixes)) then
            allocate (dired_compress_file_suffixes(0))
            ! "tar -zxf" isn't used because it's not available on the
            ! Solaris 10 version of tar (obsolete in 2024?).
            ! Same thing on AIX 7.1 (obsolete 2023?) and 7.2 (obsolete 2022?).
            call add_compress_suffix("\.tar\.gz\'", "", "gzip -dc %i | tar -xf -")
            call add_compress_suffix("\.tar\.xz\'", "", "xz -dc %i | tar -xf -")
            call add_compress_suffix("\.tgz\'", "", "gzip -dc %i | tar -xf -")
            call add_compress_suffix("\.gz\'", "", "gzip -d")
            call add_compress_suffix("\.lz\'", "", "lzip -d")
            call add_compress_suffix("\.Z\'", "", "uncompress")
            ! For .z, try gunzip.  It might be an old gzip file, or it might
            ! be from compact? pack? (which?) but gunzip handles both.
            call add_compress_suffix("\.z\'", "", "gzip -d")
            call add_compress_suffix("\.dz\'", "", "dictunzip")
            call add_compress_suffix("\.tbz\'", ".tar", "bunzip2")
            call add_compress_suffix("\.bz2\'", "", "bunzip2")
            call add_compress_suffix("\.xz\'", "", "unxz")
            call add_compress_suffix("\.zip\'", "", "unzip -o -d %o %i")
            call add_compress_suffix("\.tar\.zst\'", "", "unzstd -c %i | tar -xf -")
            call add_compress_suffix("\.tzst\'", "", "unzstd -c %i | tar -xf -")
            call add_compress_suffix("\.zst\'", "", "unzstd --rm")
            call add_compress_suffix("\.7z\'", "", "7z x -aoa -o%o %i")
            ! This item controls naming for compression.
            call add_compress_suffix("\.tar\'", ".tgz", "")
        end if
        if (.not. allocated(dired_compress_file_alist)) then
            allocate (dired_compress_file_alist(0))
            call add_compress_rule(dired_compress_file_alist, "\.gz\'", "gzip -9f %i")
            call add_compress_rule(dired_compress_file_alist, "\.bz2\'", "bzip2 -9f %i")
            call add_compress_rule(dired_compress_file_alist, "\.xz\'", "xz -9f %i")
            call add_compress_rule(dired_compress_file_alist, "\.zst\'", "zstd -qf -19 --rm -o %o %i")
        end if
        if (.not. allocated(dired_compress_files_alist)) then
            allocate (dired_compress_files_alist(0))
            call add_compress_rule(dired_compress_files_alist, "\.tar\.gz\'", "tar -cf - %i | gzip -c9 > %o")
            call add_compress_rule(dired_compress_files_alist, "\.tar\.bz2\'", "tar -cf - %i | bzip2 -c9 > %o")
            call add_compress_rule(dired_compress_files_alist, "\.tar\.xz\'", "tar -cf - %i | xz -c9 > %o")
            call add_compress_rule(dired_compress_files_alist, "\.tar\.zst\'", "tar -cf - %i | zstd -19 -o %o")
            call add_compress_rule(dired_compress_files_alist, "\.tar\.lz\'", "tar -cf - %i | lzip -c9 > %o")
            call add_compress_rule(dired_compress_files_alist, "\.tar\.lzo\'", "tar -cf - %i | lzop -c9 > %o")
            call add_compress_rule(dired_compress_files_alist, "\.zip\'", "zip %o -r --filesync %i")
            call add_compress_rule(dired_compress_files_alist, "\.pax\'", "pax -wf %o %i")
        end if
    end subroutine init_dired_aux_options

    module procedure dired_aux_keys
    ! Upper case keys (except !) for operating on the marked files
    call aux_bind("C", "dired_do_copy")
    call aux_bind("G", "dired_do_chgrp")
    call aux_bind("H", "dired_do_hardlink")
    call aux_bind("M", "dired_do_chmod")
    call aux_bind("O", "dired_do_chown")
    call aux_bind("R", "dired_do_rename")
    call aux_bind("S", "dired_do_symlink")
    call aux_bind("T", "dired_do_touch")
    call aux_bind("Y", "dired_do_relsymlink")
    call aux_bind("Z", "dired_do_compress")
    call aux_bind("c", "dired_do_compress_to")
    ! Make all regexp commands share a `%' prefix:
    call aux_bind("% u", "dired_upcase")
    call aux_bind("% l", "dired_downcase")
    call aux_bind("% r", "dired_do_rename_regexp")
    call aux_bind("% C", "dired_do_copy_regexp")
    call aux_bind("% H", "dired_do_hardlink_regexp")
    call aux_bind("% R", "dired_do_rename_regexp")
    call aux_bind("% S", "dired_do_symlink_regexp")
    call aux_bind("% Y", "dired_do_relsymlink_regexp")
    ! Lower keys for commands not operating on all the marked files
    call aux_bind("k", "dired_do_kill_lines")
    call aux_bind("v", "dired_view_file")
    call aux_bind("y", "dired_show_file_type")
    end procedure dired_aux_keys

    !> Append S to the strings LIST.
    subroutine push_string(list, s)
        type(dired_string_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(dired_string_type), allocatable :: grown(:)
        integer :: n
        n = size(list)
        allocate (grown(n + 1))
        grown(1:n) = list
        grown(n + 1)%s = s
        call move_alloc(grown, list)
    end subroutine push_string

    !> (re-search-forward REGEXP nil t) as a truth value.
    logical function aux_re_search_forward(regexp) result(found)
        character(len=*), intent(in) :: regexp
        integer(int64) :: pos
        type(status_type) :: st
        call re_search_forward(regexp, pos, st, noerror=SEARCH_NOERROR)
        found = st%ok() .and. pos /= 0
    end function aux_re_search_forward
    ! ------------------------------------------------------------------
    ! Helpers.  Copies of dired_mode_fm's private ones: gfortran drops
    ! private parent procedures that submodules call.
    ! ------------------------------------------------------------------

    !> The position N lines after this one's start (line-beginning-position N).
    integer(int64) function aux_bol(n)
        integer(int64), intent(in), optional :: n
        aux_bol = line_beginning_position(n)
    end function aux_bol

    !> Move N lines (forward-line), ignoring the shortage.
    subroutine aux_fwd_line(n)
        integer(int64), intent(in) :: n
        integer(int64) :: shortage
        shortage = forward_line(n)
    end subroutine aux_fwd_line

    !> The character after POS (default point) as an ASCII character, or
    !> achar(0) when there is none or it is not ASCII.
    character function aux_char_at(pos)
        integer(int64), intent(in), optional :: pos
        integer(int32) :: c
        logical :: found
        if (present(pos)) then
            call char_after(c, found, pos)
        else
            call char_after(c, found)
        end if
        aux_char_at = achar(0)
        if (found .and. c >= 0 .and. c < 128) aux_char_at = achar(c)
    end function aux_char_at

    !> The position of marker M (0 if it points nowhere).
    integer(int64) function aux_mpos(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st
        call marker_position(m, aux_mpos, st)
        if (.not. st%ok()) aux_mpos = 0
    end function aux_mpos

    !> Free marker M, ignoring a stale handle.
    subroutine aux_release_marker(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st
        call free_marker(m, st)
    end subroutine aux_release_marker

    !> Bind `inhibit_read_only' to true (SAVED keeps the old value).
    subroutine aux_inhibit_read_only_begin(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st
        saved = inhibit_read_only()
        call set(sym("inhibit_read_only"), t_value(), st)
    end subroutine aux_inhibit_read_only_begin

    !> Restore `inhibit_read_only' to SAVED.
    subroutine aux_inhibit_read_only_end(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st
        call set(sym("inhibit_read_only"), saved, st)
    end subroutine aux_inhibit_read_only_end

    !> The current buffer's `default_directory'.
    function aux_default_directory() result(dir)
        character(len=:), allocatable :: dir
        dir = "./"
        if (cur == 0) return
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
    end function aux_default_directory

    !> `expand_file_name' as a function (relative to `default_directory').
    function aux_expand(name) result(full)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: full
        type(status_type) :: st
        call expand_file_name(name, full, st)
        if (.not. st%ok()) full = name
    end function aux_expand

    !> The index of the current buffer's entry in `locals' (0 if none).
    integer function aux_local_index() result(k)
        integer :: i
        k = 0
        if (cur == 0) return
        if (.not. allocated(locals)) return
        do i = 1, size(locals)
            if (locals(i)%set .and. locals(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do
    end function aux_local_index

    !> The kind and number of the prefix argument ("P"): none, a number
    !> (or `-'), or another value (C-u).
    subroutine aux_prefix_kind(arg_kind, arg_n)
        integer, intent(out) :: arg_kind
        integer(int64), intent(out) :: arg_n
        arg_n = 0
        if (nilp(current_prefix_arg)) then
            arg_kind = DIRED_ARG_NIL
        else if (consp(current_prefix_arg)) then
            arg_kind = DIRED_ARG_OTHER
        else
            arg_kind = DIRED_ARG_INTEGER
            arg_n = prefix_numeric_value(current_prefix_arg)
        end if
    end subroutine aux_prefix_kind

    !> Bind KEYS to COMMAND in `dired_mode_map'.
    subroutine aux_bind(keys, command)
        character(len=*), intent(in) :: keys, command
        type(status_type) :: st
        call keymap_set(dired_mode_map, keys, command, st)
    end subroutine aux_bind
end submodule dired_aux_fm
