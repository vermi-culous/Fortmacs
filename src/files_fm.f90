! files_fm.f90 -- file handling commands.
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

!> Visiting files, saving buffers with backups, and leaving Emacs.
!>
!> Directory listings for Dired (`insert_directory', running `ls'),
!> `file_truename', deleting files and directories and `revert_buffer'
!> are in the submodule files_dir_fm.
!>
!> Not yet: wildcards in `find_file', auto-save, file locks, local
!> variables, uniquify (buffer names get "<2>"), version-control backups
!> and `backup_directory_alist', `file_precious_flag', reverting a
!> file-visiting buffer (`revert_buffer__default'), `ls_lisp', file name
!> handlers, trash (`delete_by_moving_to_trash').
module files_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use commands_fm, only: commands
    use error_fm, only: format_message, status_type, set_error, condition_matches, format_error, FM_GENERIC_ERROR, &
                        FM_USER_ERROR, FM_FILE_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_fixnum, nilp
    use data_fm, only: integer_setting
    use lread_fm, only: sym
    use buffer_fm, only: buffers, cur, current_buffer, resolve_buffer, set_buffer, &
                         get_buffer_create, generate_new_buffer_name, get_file_buffer, &
                         buffer_list, kill_buffer, erase_buffer, set_buffer_read_only, &
                         rename_buffer, restore_buffer_modified_p, buffer_live_p, buffer_handle
    use fileio_fm, only: file_stat_type, expand_file_name, file_name_directory, directory_name_p, &
                         file_name_nondirectory, directory_file_name, file_name_as_directory, &
                         get_homedir, file_exists_p, file_readable_p, file_writable_p, &
                         file_directory_p, file_accessible_directory_p, file_attributes, &
                         file_modes, set_file_modes, rename_file, copy_file, &
                         insert_file_contents, write_region, verify_visited_file_modtime, &
                         file_regular_p, file_symlink_p, user_uid, group_gid, &
                         make_directory_internal
    use keymap_fm, only: define_key
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: request_exit
    use xdisp_fm, only: message
    use editfns_fm, only: point, goto_char, point_min, point_max, char_after, widen, narrow_to_region, &
                          format_message
    use insdel_fm, only: insert
    use subr_fm, only: y_or_n_p
    use fns_fm, only: yes_or_no_p
    use derived_fm, only: major_mode_type, major_mode_named
    use map_ynp_fm, only: map_y_or_n_p, YNP_SKIP, YNP_ASK
    use window_fm, only: pop_to_buffer_same_window, switch_to_buffer_other_window
    implicit none(type, external)
    private

    public :: syms_of_files
    public :: find_file_noselect, find_file, create_file_buffer, abbreviate_file_name
    public :: save_buffer, basic_save_buffer, backup_buffer, make_backup_file_name, &
              normal_backup_enable_predicate, file_ownership_preserved_p
    public :: save_some_buffers, save_buffers_kill_emacs, temporary_file_directory
    public :: file_relative_name
    public :: set_visited_file_name, write_file
    public :: make_directory
    public :: normal_mode, set_auto_mode, add_auto_mode, file_name_sans_versions, auto_mode_spec
    public :: find_file_other_window, backup_file_name_p, revert_buffer, revert_function_proc, find_directory_proc
    public :: insert_directory, insert_directory_clean, insert_directory_wildcard_in_dir_p, file_truename, &
              delete_file, delete_directory, file_name_extension, auto_save_file_name_p, &
              file_size_human_readable, byte_count_to_string, get_free_disk_space, split_string_and_unquote, &
              shell_quote_wildcard_pattern, string_prefix_p
    public :: copy_directory, file_in_directory_p, file_equal_p, make_empty_file, file_modes_symbolic_to_number, &
              file_modes_number_to_symbolic, find_file_read_only, find_file_read_only_other_window

    !> This option influences the handling of symbolic links in `copy_directory'.
    !> If true, `copy_directory' will create a symbolic link if the
    !> source directory is a symbolic link.  If false, it'll follow the
    !> symbolic link and copy the contents instead.  Port of files.el
    !> copy-directory-create-symlink.
    logical, public :: copy_directory_create_symlink = .false.

    !> True means buffers visiting files read-only do so in view mode.
    !> In fact, this means that all read-only buffers normally have
    !> View mode enabled, including buffers that are read-only because
    !> you visit a file you cannot alter, and buffers you make read-only
    !> using \[read_only_mode].  Port of files.el view-read-only (the
    !> buffer-local values `read_only_mode' gives it are kept by view_fm).
    logical, public :: view_read_only = .false.

    interface
        !> Copy DIRECTORY to NEWNAME.  Both args must be strings.
        !> This function always sets the file modes of the output files to match
        !> the corresponding input file.
        !>
        !> KEEP_TIME true means give the output files the same
        !> last-modified time as the old ones.  (This works on only some systems.)
        !>
        !> The PARENTS argument says whether to create
        !> parent directories if they don't exist.
        !>
        !> If DIRECTORY is a symlink and `copy_directory_create_symlink' is
        !> true, create a symlink with the same target as DIRECTORY.
        !>
        !> If NEWNAME is a directory name, copy DIRECTORY as a subdirectory
        !> there.  However, if COPY_CONTENTS is true, copy the contents of
        !> DIRECTORY directly into NEWNAME instead.  Port of
        !> files.el:copy-directory (not interactive).  (In files_copy_fm.)
        recursive module subroutine copy_directory(directory, newname, status, keep_time, parents, copy_contents)
            implicit none(type, external)
            character(len=*), intent(in) :: directory, newname
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: keep_time, parents, copy_contents
        end subroutine copy_directory

        !> Return true if DIR is a parent directory of FILE.
        !> Value is true if FILE is inside DIR or inside a subdirectory of DIR.
        !> A directory is considered to be a "parent" of itself.
        !> DIR must be an existing directory, otherwise the function returns false.
        !> Port of files.el:file-in-directory-p.  (In files_copy_fm.)
        module function file_in_directory_p(file, dir) result(inside)
            implicit none(type, external)
            character(len=*), intent(in) :: file, dir
            logical :: inside
        end function file_in_directory_p

        !> Return true if files FILE1 and FILE2 name the same file.
        !> If FILE1 or FILE2 does not exist, the return value is unspecified.
        !> Port of files.el:file-equal-p (the attributes compared are those
        !> `file_attributes' gives).  (In files_copy_fm.)
        module function file_equal_p(file1, file2) result(same)
            implicit none(type, external)
            character(len=*), intent(in) :: file1, file2
            logical :: same
        end function file_equal_p

        !> Create an empty file FILENAME.
        !> Optional arg PARENTS, if true then creates parent dirs as needed.
        !> Port of files.el:make-empty-file (not interactive).  (In
        !> files_copy_fm.)
        module subroutine make_empty_file(filename, status, parents)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: parents
        end subroutine make_empty_file

        !> Convert symbolic file modes to numeric file modes.
        !> MODES is the string to convert, it should match
        !> "[ugoa]*([+=-][rwxXstugo]*)+,...".
        !> See Info node `(coreutils)File permissions' for more information on this
        !> notation.
        !> FROM (or 0 if absent) gives the mode bits on which to base permissions if
        !> MODES request to add, remove, or set permissions based on existing ones,
        !> as in "og+rX-w".  NUM_MODES is the result.  Port of
        !> files.el:file-modes-symbolic-to-number.  (In files_copy_fm.)
        module subroutine file_modes_symbolic_to_number(modes, num_modes, status, from)
            implicit none(type, external)
            character(len=*), intent(in) :: modes
            integer(int32), intent(out) :: num_modes
            type(status_type), intent(out) :: status
            integer(int32), intent(in), optional :: from
        end subroutine file_modes_symbolic_to_number

        !> Return a description of a file's MODE as a string of 10 letters and dashes.
        !> The returned string is like the mode description produced by "ls -l".
        !> For instance, if MODE is o'700', then it produces `-rwx------'.
        !> Note that this is NOT the same as the "chmod" style symbolic description
        !> accepted by `file_modes_symbolic_to_number'.
        !> FILETYPE, if provided, should be a character denoting the type of file,
        !> such as `d' for a directory, or `l' for a symbolic link, and will override
        !> the leading `-' character.  Port of
        !> files.el:file-modes-number-to-symbolic.  (In files_copy_fm.)
        module function file_modes_number_to_symbolic(mode, filetype) result(s)
            implicit none(type, external)
            integer(int32), intent(in) :: mode
            character, intent(in), optional :: filetype
            character(len=10) :: s
        end function file_modes_number_to_symbolic

        !> Give fileio_fm's `rename_file' the files.el procedures it calls
        !> across file systems.  (In files_copy_fm.)
        module subroutine syms_of_files_copy()
            implicit none(type, external)
        end subroutine syms_of_files_copy

        !> Edit file FILENAME but don't allow changes.
        !> Like \[find_file], but marks buffer as read-only.
        !> Use \[read_only_mode] to permit editing.  Port of
        !> files.el:find-file-read-only (no wildcards).  (In files_read_fm.)
        recursive module subroutine find_file_read_only(filename, status)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            type(status_type), intent(out) :: status
        end subroutine find_file_read_only

        !> Edit file FILENAME in another window but don't allow changes.
        !> Like \[find_file_other_window], but marks buffer as read-only.
        !> Use \[read_only_mode] to permit editing.
        !> If this command needs to split the current window, it by default obeys
        !> the user options `split_height_threshold' and `split_width_threshold',
        !> when it decides whether to split the window horizontally or vertically.
        !> Port of files.el:find-file-read-only-other-window (no wildcards).
        !> (In files_read_fm.)
        recursive module subroutine find_file_read_only_other_window(filename, status)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            type(status_type), intent(out) :: status
        end subroutine find_file_read_only_other_window

        !> Register `find_file_read_only' and its keys.  (In files_read_fm.)
        module subroutine syms_of_files_read(ctl_x_map, ctl_x_4_map)
            implicit none(type, external)
            type(handle_type), intent(in) :: ctl_x_map
            type(handle_type), intent(in), optional :: ctl_x_4_map
        end subroutine syms_of_files_read

        !> minibuffer.el:read-file-name.  With `confirm_nonexistent', a
        !> name that is not an existing file needs confirming right after
        !> completion (confirm-nonexistent-file-or-buffer's value
        !> `after_completion').  (In submodule files_read_fm, as the
        !> minibuffer code depends on this module.)
        recursive module subroutine read_file_name_in_minibuffer(prompt, result, status, dir, &
                                                                 default_filename, initial, &
                                                                 confirm_nonexistent)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            character(len=:), allocatable, intent(out) :: result
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: dir, default_filename, initial
            logical, intent(in), optional :: confirm_nonexistent
        end subroutine read_file_name_in_minibuffer

        !> Choose the major mode for this buffer automatically.
        !> Uses the visited file name and the -*- line.
        !>
        !> This function is called automatically from `find_file'.  Port of
        !> files.el:normal-mode (no file or directory local variables yet).
        !> (In files_mode_fm.)
        recursive module subroutine normal_mode(status, find_file)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: find_file
        end subroutine normal_mode

        !> Select major mode appropriate for current buffer.
        !>
        !> To find the right major mode, this function checks for a -*- mode tag,
        !> then compares the file name against the entries in `auto_mode_alist'
        !> (case-sensitively first, then ignoring case, as `auto_mode_case_fold').
        !>
        !> If the optional argument KEEP_MODE_IF_SAME is true, then we
        !> set the major mode only if that would change it.  Port of
        !> files.el:set-auto-mode (only the first mode of a -*- line; no
        !> interpreter-mode-alist or magic-mode-alist yet).  (In
        !> files_mode_fm.)
        recursive module subroutine set_auto_mode(status, keep_mode_if_same)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: keep_mode_if_same
        end subroutine set_auto_mode

        !> Add REGEXP and MODE to the front of `auto_mode_alist', so files
        !> whose names match REGEXP get major mode MODE.  (In
        !> files_mode_fm.)
        module subroutine add_auto_mode(regexp, mode)
            implicit none(type, external)
            character(len=*), intent(in) :: regexp, mode
        end subroutine add_auto_mode

        !> Return file NAME sans backup versions or strings.
        !> This is a separate procedure so your site-init or startup file can
        !> redefine it.  Port of files.el:file-name-sans-versions (no
        !> version-control numbering beyond .~N~).  (In files_mode_fm.)
        module function file_name_sans_versions(name) result(base)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            character(len=:), allocatable :: base
        end function file_name_sans_versions

        !> The mode named by the -*- line of TEXT (the start of a buffer),
        !> as "NAME-mode", or "" if there is none.  Port of the -*- part of
        !> files.el:set-auto-mode with set-auto-mode-1.  (In
        !> files_mode_fm.)
        module function auto_mode_spec(text) result(mode)
            implicit none(type, external)
            character(len=*), intent(in) :: text
            character(len=:), allocatable :: mode
        end function auto_mode_spec

        !> Insert directory listing for FILE, formatted according to SWITCHES.
        !> Leaves point after the inserted text.
        !> SWITCHES is a string of options.
        !> Optional third arg WILDCARD means treat FILE as shell wildcard.
        !> Optional fourth arg FULL_DIRECTORY_P means file is a directory and
        !> switches do not contain `d', so that a full listing is expected.
        !>
        !> This works by running a directory listing program
        !> whose name is in the variable `insert_directory_program'
        !> (and if WILDCARD, it also runs the shell `shell_file_name').
        !>
        !> When SWITCHES contains the long `--dired' option, this function
        !> treats it specially, for the sake of dired.  However, the
        !> normally equivalent short `-D' option is just passed on to
        !> `insert_directory_program', as any other option.  Port of
        !> files.el:insert-directory (no file name handlers or `ls_lisp';
        !> SWITCHES a string; the output is decoded as UTF-8 by
        !> `call_process'; the shell is $SHELL or /bin/sh, as callproc.c
        !> sets `shell_file_name').  (In files_dir_fm.)
        recursive module subroutine insert_directory(file, switches, status, wildcard, full_directory_p)
            implicit none(type, external)
            character(len=*), intent(in) :: file, switches
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: wildcard, full_directory_p
        end subroutine insert_directory

        !> After `ls' output inserted from BEG to point, read the positions of
        !> file names that `--dired' (if SWITCHES has it) put at the end,
        !> mark them with the `dired_filename' property and delete those
        !> lines.  Port of files.el:insert-directory-clean.  (In files_dir_fm.)
        recursive module subroutine insert_directory_clean(beg, switches, status)
            implicit none(type, external)
            integer(int64), intent(in) :: beg
            character(len=*), intent(in) :: switches
            type(status_type), intent(out) :: status
        end subroutine insert_directory_clean

        !> Return true if DIR contains shell wildcards in its parent directory part.
        !> DIRECTORY is the part of DIR up to and excluding the first
        !> component that includes wildcard characters, and WILDCARD is the
        !> rest of DIR's components.  The DIRECTORY part includes the
        !> trailing slash, to indicate that it is a directory.
        !>
        !> Valid wildcards are `*', `?', `[abc]' and `[a-z]'.  Port of
        !> files.el:insert-directory-wildcard-in-dir-p (the cons as
        !> DIRECTORY and WILDCARD).  (In files_dir_fm.)
        module function insert_directory_wildcard_in_dir_p(dir, directory, wildcard) result(found)
            implicit none(type, external)
            character(len=*), intent(in) :: dir
            character(len=:), allocatable, intent(out), optional :: directory, wildcard
            logical :: found
        end function insert_directory_wildcard_in_dir_p

        !> Return the truename of FILENAME.
        !> If FILENAME is not absolute, first expands it against `default_directory'.
        !> The truename of a file name is found by chasing symbolic links
        !> both at the level of the file and at the level of the directories
        !> containing it, until no links are left at any level.  Port of
        !> files.el:file-truename.  (In files_dir_fm.)
        recursive module subroutine file_truename(filename, truename, status)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            character(len=:), allocatable, intent(out) :: truename
            type(status_type), intent(out) :: status
        end subroutine file_truename

        !> Delete file named FILENAME.  If it is a symlink, remove the symlink.
        !> If file has multiple names, it continues to exist with the other names.
        !> Port of files.el:delete-file (TRASH deferred).  (In files_dir_fm.)
        module subroutine delete_file(filename, status)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            type(status_type), intent(out) :: status
        end subroutine delete_file

        !> Delete the directory named DIRECTORY.  Does not follow symlinks.
        !> If RECURSIVE is true, delete files in DIRECTORY as well, with
        !> no error if something else is simultaneously deleting them.  Port
        !> of files.el:delete-directory (TRASH deferred).  (In files_dir_fm.)
        recursive module subroutine delete_directory(directory, status, recursive)
            implicit none(type, external)
            character(len=*), intent(in) :: directory
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: recursive
        end subroutine delete_directory

        !> Return FILENAME's final "extension" sans any backup version strings.
        !> The extension, in a file name, is the part that begins with the last `.',
        !> except that a leading `.' of the file name, if there is one, doesn't count.
        !> This function calls `file_name_sans_versions', which see, to remove from
        !> the extension it returns any parts that indicate backup versions and
        !> version strings.
        !> Return no extension (FOUND false) for extensionless file names such as `foo'.
        !> Return the empty string for file names such as `foo.' that end in a period.
        !>
        !> By default, the returned value excludes the period that starts the
        !> extension, but if the optional argument PERIOD is true, the period
        !> is included in the value, and in that case, if FILENAME has no
        !> extension, the value is "".  Port of files.el:file-name-extension.
        !> (In files_dir_fm.)
        module subroutine file_name_extension(filename, extension, found, period)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            character(len=:), allocatable, intent(out) :: extension
            logical, intent(out) :: found
            logical, intent(in), optional :: period
        end subroutine file_name_extension

        !> Return true if FILENAME can be yielded by `make_auto_save_file_name'.
        !> FILENAME should lack slashes.  Port of
        !> files.el:auto-save-file-name-p.  (In files_dir_fm.)
        module function auto_save_file_name_p(filename) result(p)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            logical :: p
        end function auto_save_file_name_p

        !> Produce a string showing FILE_SIZE in human-readable form.
        !>
        !> FLAVOR controls the units and the display format:
        !>
        !>  If FLAVOR is "" or absent, each kilobyte is 1024 bytes and the
        !>     produced suffixes are "k", "M", "G", "T", etc.
        !>  If FLAVOR is "si", each kilobyte is 1000 bytes and the produced suffixes
        !>     are "k", "M", "G", "T", etc.
        !>  If FLAVOR is "iec", each kilobyte is 1024 bytes and the produced suffixes
        !>     are "KiB", "MiB", "GiB", "TiB", etc.
        !>
        !> SPACE is a string put between the number and unit.
        !> It defaults to the empty string.
        !>
        !> UNIT is the unit to use.  It defaults to "B" when FLAVOR is "iec"
        !> and the empty string otherwise.  Port of
        !> files.el:file-size-human-readable.  (In files_dir_fm.)
        module function file_size_human_readable(file_size, flavor, space, unit) result(text)
            implicit none(type, external)
            real(real64), intent(in) :: file_size
            character(len=*), intent(in), optional :: flavor, space, unit
            character(len=:), allocatable :: text
        end function file_size_human_readable

        !> Human-readable string for SIZE bytes, as `byte_count_to_string_function'
        !> makes it (file-size-human-readable-iec: IEC prefixes).  Port of
        !> files.el:file-size-human-readable-iec.  (In files_dir_fm.)
        module function byte_count_to_string(size) result(text)
            implicit none(type, external)
            real(real64), intent(in) :: size
            character(len=:), allocatable :: text
        end function byte_count_to_string

        !> String describing the amount of free space on DIR's file system.
        !> If DIR's free space cannot be obtained, FOUND is false.  Port
        !> of files.el:get-free-disk-space.  (In files_dir_fm.)
        module subroutine get_free_disk_space(dir, text, found, status)
            implicit none(type, external)
            character(len=*), intent(in) :: dir
            character(len=:), allocatable, intent(out) :: text
            logical, intent(out) :: found
            type(status_type), intent(out) :: status
        end subroutine get_free_disk_space

        !> Split the STRING at whitespace into a list of strings,
        !> understanding Emacs Lisp quoting within STRING ("..." with
        !> backslash escapes).  Port of subr.el:split-string-and-unquote
        !> (SEPARATOR "\s-+").  (In files_dir_fm.)
        module subroutine split_string_and_unquote(string, parts, status)
            implicit none(type, external)
            character(len=*), intent(in) :: string
            type(value_type), intent(out) :: parts
            type(status_type), intent(out) :: status
        end subroutine split_string_and_unquote

        !> Quote characters special to the shell in PATTERN, leave wildcards
        !> alone: each character from the set [ \t\n;<>&|()`'"#$] is quoted
        !> with a backslash.  Port of
        !> files.el:shell-quote-wildcard-pattern (GNU/Linux).  (In
        !> files_dir_fm.)
        module function shell_quote_wildcard_pattern(pattern) result(quoted)
            implicit none(type, external)
            character(len=*), intent(in) :: pattern
            character(len=:), allocatable :: quoted
        end function shell_quote_wildcard_pattern

        !> Return true if STRING begins with PREFIX (ignoring case with
        !> IGNORE_CASE).  Port of subr.el:string-prefix-p.  (In
        !> files_dir_fm.)
        module function string_prefix_p(prefix, string, ignore_case) result(p)
            implicit none(type, external)
            character(len=*), intent(in) :: prefix, string
            logical, intent(in), optional :: ignore_case
            logical :: p
        end function string_prefix_p

        !> Replace current buffer text with the text of the visited file on disk.
        !> This undoes all changes since the file was visited or saved.
        !>
        !> This command also implements an interface for special buffers
        !> that contain text that doesn't come from a file, but reflects
        !> some other data instead (e.g. Dired buffers).  This is done via
        !> the variable `revert_buffer_function'.  In these cases, it should
        !> reconstruct the buffer contents from the appropriate data.
        !>
        !> IGNORE_AUTO: offer to revert from the auto-save file only when
        !> this is false.
        !>
        !> NOCONFIRM means don't ask for confirmation at all.
        !>
        !> PRESERVE_MODES true means don't alter the files modes.
        !>
        !> This function calls the function that `revert_buffer_function'
        !> specifies to do the work, with arguments IGNORE_AUTO and
        !> NOCONFIRM.  Port of files.el:revert-buffer
        !> (`revert_buffer_restore_functions' and the default function,
        !> which reverts a file-visiting buffer, are deferred).  (In
        !> files_dir_fm.)
        recursive module subroutine revert_buffer(status, ignore_auto, noconfirm, preserve_modes)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: ignore_auto, noconfirm, preserve_modes
        end subroutine revert_buffer
        !> Convert FILENAME to be relative to DIRECTORY (default:
        !> `default_directory').
        !> This function returns a relative file name that is equivalent to
        !> FILENAME when used with that default directory as the default.
        !> If FILENAME is a relative file name, it will be interpreted as
        !> existing in `default_directory'.  Port of
        !> files.el:file-relative-name.  (In files_read_fm.)
        module function file_relative_name(filename, directory) result(rel)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            character(len=*), intent(in), optional :: directory
            character(len=:), allocatable :: rel
        end function file_relative_name
    end interface

    !> True means make a backup of a file the first time it is saved.
    !> This can be done by renaming the file or by copying.
    !>
    !> Renaming means that Emacs renames the existing file so that it is a
    !> backup file, then writes the buffer into a new file.  Any other names
    !> that the old file had will now refer to the backup file.  The new file
    !> is owned by you and its group is defaulted.
    !>
    !> Copying means that Emacs copies the existing file into the backup
    !> file, then writes the buffer on top of the existing file.  Any other
    !> names that the old file had will now refer to the new (edited) file.
    !> The file's owner and group are unchanged.  Port of files.el
    !> make-backup-files.
    logical, public :: make_backup_files = .true.

    !> Values of `require_final_newline' and `mode_require_final_newline'
    !> (upstream nil, t, visit, visit-save and any other value).
    integer, parameter, public :: FINAL_NEWLINE_NEVER = 0, FINAL_NEWLINE_SAVE = 1, FINAL_NEWLINE_VISIT = 2, &
                                  FINAL_NEWLINE_VISIT_SAVE = 3, FINAL_NEWLINE_ASK = 4

    !> Whether to add a newline automatically at the end of the file.
    !>
    !> A value of FINAL_NEWLINE_SAVE means do this only when the file is about to be saved.
    !> A value of FINAL_NEWLINE_VISIT means do this right after the file is visited.
    !> A value of FINAL_NEWLINE_VISIT_SAVE means do it at both of those times.
    !> A value of FINAL_NEWLINE_ASK means ask user whether to add a newline, when saving.
    !> A value of FINAL_NEWLINE_NEVER means don't add newlines.
    !>
    !> Certain major modes set this locally to the value obtained
    !> from `mode_require_final_newline'.
    !>
    !> This variable is heeded only when visiting files (or saving
    !> buffers into files they visit).  Writing data to the file system
    !> with `write_region' and the like is not influenced by this variable.
    !> Port of files.el require-final-newline (FINAL_NEWLINE_NEVER is
    !> nil, FINAL_NEWLINE_SAVE t, FINAL_NEWLINE_VISIT `visit',
    !> FINAL_NEWLINE_VISIT_SAVE `visit_save', FINAL_NEWLINE_ASK any other
    !> value).
    type(integer_setting), public :: require_final_newline = &
                                     integer_setting(name="require_final_newline", value=FINAL_NEWLINE_NEVER)

    !> Whether to add a newline at end of file, in certain major modes.
    !> Those modes set `require_final_newline' to this value when you enable them.
    !> They do so because they are often used for files that are supposed
    !> to end in newlines, and the question is how to arrange that.
    !>
    !> A value of FINAL_NEWLINE_SAVE means do this only when the file is about to be saved.
    !> A value of FINAL_NEWLINE_VISIT means do this right after the file is visited.
    !> A value of FINAL_NEWLINE_VISIT_SAVE means do it at both of those times.
    !> A value of FINAL_NEWLINE_ASK means ask user whether to add a newline, when saving.
    !>
    !> A value of FINAL_NEWLINE_NEVER means do not add newlines.  That is a risky choice in this
    !> variable since this value is used for modes for files that ought to have
    !> final newlines.  So if you set this to FINAL_NEWLINE_NEVER, you must explicitly check and
    !> add a final newline, whenever you save a file that really needs one.
    !> Port of files.el mode-require-final-newline (values as for
    !> `require_final_newline').
    type(integer_setting), public :: mode_require_final_newline = &
                                     integer_setting(name="mode_require_final_newline", value=FINAL_NEWLINE_SAVE)

    !> True means \[write_file] should set the major mode from the file name.
    !> However, the mode will not be changed if
    !> (1) a local variables list or the `-*-' line specifies a major mode, or
    !> (2) the current major mode is a "special" mode,
    !>     not suitable for ordinary files, or
    !> (3) the new file name does not particularly specify any mode.
    !> Port of files.el change-major-mode-with-file-name.
    logical, public :: change_major_mode_with_file_name = .true.

    !> Buffers save_some_buffers is asking about (its map-y-or-n-p list).
    type(handle_type), allocatable :: pending(:)

    abstract interface
        !> A `revert_buffer_function': revert the current buffer, with
        !> the arguments IGNORE_AUTO and NOCONFIRM of `revert_buffer'.
        recursive subroutine revert_function_proc(ignore_auto, noconfirm, status)
            import :: status_type
            implicit none(type, external)
            logical, intent(in) :: ignore_auto, noconfirm
            type(status_type), intent(out) :: status
        end subroutine revert_function_proc

        !> A function of `find_directory_functions': visit DIRNAME,
        !> returning its buffer in BUF (the null handle for none).
        recursive subroutine find_directory_proc(dirname, buf, status)
            import :: handle_type, status_type
            implicit none(type, external)
            character(len=*), intent(in) :: dirname
            type(handle_type), intent(out) :: buf
            type(status_type), intent(out) :: status
        end subroutine find_directory_proc
    end interface

    !> One buffer's `revert_buffer_function'.
    type :: revert_entry_type
        integer(int64) :: serial = 0
        procedure(revert_function_proc), pointer, nopass :: fn => null()
    end type revert_entry_type

    !> Function to use to revert this buffer.
    !> The function receives two arguments IGNORE_AUTO and NOCONFIRM,
    !> which are the arguments that `revert_buffer' received.
    !> It also has access to the `preserve_modes' argument of
    !> `revert_buffer' via the `revert_buffer_preserve_modes' dynamic
    !> variable.
    !>
    !> For historical reasons, a null value means to use the default
    !> function.  This should not be relied upon.  Port of files.el
    !> revert-buffer-function (permanently buffer-local; set with
    !> `set_local', null for the default).
    type, public :: revert_buffer_function_type
        character(len=48) :: name = "revert_buffer_function"
        type(revert_entry_type), allocatable :: entries(:)
    contains
        !> Make FN the current buffer's revert function (setq-local).
        procedure :: set_local => revert_set_local
        !> The current buffer's revert function (null for the default).
        procedure :: get => revert_get
    end type revert_buffer_function_type

    !> Function to use to revert this buffer (see the type).  Port of
    !> files.el revert-buffer-function.
    type(revert_buffer_function_type), public :: revert_buffer_function

    !> True within a `revert_buffer'.  Port of files.el
    !> revert-buffer-in-progress.
    logical, public :: revert_buffer_in_progress = .false.

    !> True means `revert_buffer' should not reinitialize the modes
    !> (the PRESERVE_MODES argument).  Port of files.el
    !> revert-buffer-preserve-modes.
    logical, public :: revert_buffer_preserve_modes = .false.

    !> True means allow `find_file' to visit directories.
    !> To visit the directory, `find_file' runs `find_directory_functions'.
    !> Port of files.el find-file-run-dired.
    logical, public :: find_file_run_dired = .true.

    !> Function to try to visit a directory.
    !> It is called with the directory name as the sole argument
    !> and should return either a buffer or the null handle.  Port of files.el
    !> find-directory-functions (one function: `dired_noselect', which
    !> Dired installs; cvs-dired-noselect is not ported).
    procedure(find_directory_proc), pointer, public :: find_directory_functions => null()

    !> Absolute or relative name of the `ls'-like program.
    !> This is used by `insert_directory' and `dired_insert_directory'
    !> (thus, also by `dired').  For Dired, this should ideally point to
    !> GNU ls, or another version of ls that supports the "--dired"
    !> flag.  See `dired_use_ls_dired'.
    !>
    !> On GNU/Linux and other capable systems, the default is "ls".  Port
    !> of files.el insert-directory-program.
    character(len=:), allocatable, public :: insert_directory_program

    !> Regular expression to match up to the file name in a directory listing.
    !> The default value is designed to recognize dates and times
    !> regardless of the language.  Port of files.el
    !> directory-listing-before-filename-regexp.
    character(len=:), allocatable, public :: directory_listing_before_filename_regexp

    !> When allocated, name of temporary file an `ls' error is written to.
    !> Port of dired.el dired--ls-error-file (set by `insert_directory').
    character(len=:), allocatable, public :: dired_ls_error_file

    !> Set if the current dired invocation yields an `ls' error: the
    !> buffer containing the error message (else the null handle).  Port
    !> of dired.el dired--ls-error-buffer (set by `insert_directory').
    type(handle_type), public :: dired_ls_error_buffer

contains

    ! ------------------------------------------------------------------
    ! Visiting
    ! ------------------------------------------------------------------

    !> `filename' with the home directory replaced by "~".  Port of
    !> files.el:abbreviate-file-name (no directory-abbrev-alist).
    function abbreviate_file_name(filename) result(abbrev)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: abbrev
        character(len=:), allocatable :: home
        integer :: n
        abbrev = filename
        home = directory_file_name(get_homedir())
        n = len(home)
        ! Don't abbreviate a home directory of "/" (abbreviated-home-dir).
        if (n <= 1) return
        if (len(filename) < n) return
        if (filename(1:n) /= home) return
        if (len(filename) == n) then
            abbrev = "~"
        else if (filename(n + 1:n + 1) == "/") then
            abbrev = "~"//filename(n + 1:)
        end if
    end function abbreviate_file_name

    !> A new buffer named after `filename' (sans directory), made unique
    !> with "<N>"; a leading space gets a "|" before it.
    !> Port of files.el:create-file-buffer (no uniquify).
    subroutine create_file_buffer(filename, h, status)
        character(len=*), intent(in) :: filename
        type(handle_type), intent(out) :: h
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: lastname
        lastname = file_name_nondirectory(directory_file_name(filename))
        if (len(lastname) == 0) lastname = filename
        if (lastname(1:1) == " ") lastname = "|"//lastname
        call get_buffer_create(generate_new_buffer_name(lastname), h, status)
    end subroutine create_file_buffer

    !> The buffer visiting `filename', reading the file into a new buffer
    !> if none does yet.  The buffer is not selected.  A file that doesn't
    !> exist still gets a buffer, which will create it when saved.
    !> Port of files.el:find-file-noselect with find-file-noselect-1 (no
    !> wildcards, truenames, raw files, or checks that an already-visited
    !> file changed on disk; `find_file_visit_truename' nil).
    recursive subroutine find_file_noselect(filename, buf, status)
        character(len=*), intent(in) :: filename
        type(handle_type), intent(out) :: buf
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: absname
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: inserted
        logical :: error
        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        if (file_directory_p(absname)) then
            ! find-directory-functions
            if (find_file_run_dired .and. associated(find_directory_functions)) then
                call find_directory_functions(abbreviate_file_name(absname), buf, status)
                if (.not. status%ok() .or. buf%index /= 0) return
            end if
            call set_error(status, FM_GENERIC_ERROR, abbreviate_file_name(absname)//" is a directory")
            return
        end if
        buf = get_file_buffer(absname)
        if (buf%index /= 0) return
        call create_file_buffer(absname, buf, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        ! find-file-noselect-1
        call erase_buffer(status)
        if (.not. status%ok()) return
        error = .false.
        call insert_file_contents(absname, .true., inserted, status)
        if (.not. status%ok()) then
            if (.not. condition_matches(status%code, FM_FILE_ERROR)) then
                call set_buffer(old, st)
                return
            end if
            if (file_exists_p(absname)) then
                ! The file is there but couldn't be read.
                call set_buffer(old, st)
                call kill_buffer(buf, st)
                buf = handle_type()
                if (.not. file_readable_p(absname)) then
                    status = status_type()
                    call set_error(status, FM_FILE_ERROR, "File is not readable: "//absname)
                end if
                return
            end if
            status = status_type()
            error = .true.
        end if
        buffers(cur)%directory = file_name_directory(buffers(cur)%filename)
        call after_find_file(error, .true.)
        call set_buffer(old, status)
    end subroutine find_file_noselect

    !> Set up the current buffer after visiting its file: read-only if the
    !> file can't be written, and a message about new or protected files
    !> when `warn', then the major mode.  Port of files.el:after-find-file
    !> (no auto-save, local variables or hooks).
    subroutine after_find_file(error, warn)
        logical, intent(in) :: error, warn
        character(len=:), allocatable :: msg, file
        integer(int32) :: modes
        type(status_type) :: st
        file = buffers(cur)%filename
        buffers(cur)%read_only = .not. file_writable_p(file)
        msg = ""
        if (error .and. file_exists_p(file)) then
            buffers(cur)%read_only = .true.
            msg = "File exists, but cannot be read"
        else if (error .and. file_symlink_p(file)) then
            msg = "Symbolic link that points to nonexistent file"
        else if (.not. buffers(cur)%read_only) then
            if (error) msg = "(New file)"
        else if (.not. error) then
            msg = "Note: file is write protected"
        else if (file_accessible_directory_p(buffers(cur)%directory)) then
            msg = "File not found and directory write-protected"
        else
            buffers(cur)%read_only = .false.
            if (.not. file_directory_p(buffers(cur)%directory)) then
                msg = "Use M-x make-directory RET RET to create the directory and its parents"
            end if
        end if
        if (warn .and. len(msg) > 0) call message(msg)
        ! A backup file, or one with no write bits, starts read-only.
        if (.not. buffers(cur)%read_only) then
            modes = file_modes(file)
            if (backup_file_name_p(file) .or. (modes >= 0 .and. iand(modes, int(o'222', int32)) == 0)) then
                call set_buffer_read_only(current_buffer(), .true., st)
            end if
        end if
        call normal_mode(st, .true.)
        ! If requested, add a newline at the end of the file.
        if (require_final_newline%value == FINAL_NEWLINE_VISIT .or. &
            require_final_newline%value == FINAL_NEWLINE_VISIT_SAVE) then
            if (missing_final_newline()) call insert_final_newline(st)
        end if
    end subroutine after_find_file

    !> Edit file FILENAME: visit it with `find_file_noselect' and switch to
    !> its buffer in the selected window (`pop_to_buffer_same_window').  Port
    !> of files.el:find-file (one file; WILDCARDS deferred).
    recursive subroutine find_file(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        type(handle_type) :: buf
        call find_file_noselect(filename, buf, status)
        if (.not. status%ok()) return
        call pop_to_buffer_same_window(buf, status)
    end subroutine find_file

    !> Edit file FILENAME, in another window.
    !>
    !> Like \[find_file] (which see), but creates a new window or reuses
    !> an existing one.  See the function `display_buffer'.
    !>
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of files.el:find-file-other-window (one file; WILDCARDS
    !> deferred).
    recursive subroutine find_file_other_window(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        type(handle_type) :: buf
        call find_file_noselect(filename, buf, status)
        if (.not. status%ok()) return
        call switch_to_buffer_other_window(buf, status)
    end subroutine find_file_other_window

    ! ------------------------------------------------------------------
    ! Backups
    ! ------------------------------------------------------------------

    !> Return true if FILE is a backup file name (numeric or not).
    !> This is a separate function so you can redefine it for customization.
    !> You may need to redefine `file_name_sans_versions' as well.  Port of
    !> files.el:backup-file-name-p.
    pure logical function backup_file_name_p(file)
        character(len=*), intent(in) :: file
        backup_file_name_p = .false.
        if (len(file) > 0) backup_file_name_p = file(len(file):len(file)) == "~"
    end function backup_file_name_p

    !> The backup name of `file': FILE~.  Port of
    !> files.el:make-backup-file-name (backup-directory-alist nil).
    pure function make_backup_file_name(file) result(backup)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: backup
        backup = file//"~"
    end function make_backup_file_name

    !> The temporary directory, as a directory name: $TMPDIR, else "/tmp".
    !> (callproc.c:init_callproc sets temporary-file-directory.)
    function temporary_file_directory() result(dir)
        character(len=:), allocatable :: dir
        integer :: length, stat
        call get_environment_variable("TMPDIR", length=length, status=stat)
        if (stat == 0 .and. length > 0) then
            allocate (character(len=length) :: dir)
            call get_environment_variable("TMPDIR", dir)
        else
            dir = "/tmp"
        end if
        dir = file_name_as_directory(dir)
    end function temporary_file_directory

    !> Whether to back up `name': not for files under the temporary
    !> directory or "/tmp".  Port of files.el:normal-backup-enable-predicate.
    logical function normal_backup_enable_predicate(name)
        character(len=*), intent(in) :: name
        normal_backup_enable_predicate = .not. (under(temporary_file_directory()) .or. under("/tmp"))
    contains

        !> compare-strings says `name' extends `dir'.
        logical function under(dir)
            character(len=*), intent(in) :: dir
            under = len(name) > len(dir)
            if (under) under = name(1:len(dir)) == dir
        end function under
    end function normal_backup_enable_predicate

    !> Whether deleting and rewriting `file' keeps its owner (and with
    !> `group', its group); true if it doesn't exist.
    !> Port of files.el:file-ownership-preserved-p.
    logical function file_ownership_preserved_p(file, group)
        character(len=*), intent(in) :: file
        logical, intent(in) :: group
        type(file_stat_type) :: attr, parent
        character(len=:), allocatable :: dir
        logical :: found
        call file_attributes(file, attr, found)
        file_ownership_preserved_p = .true.
        if (.not. found) return
        file_ownership_preserved_p = .false.
        if (attr%uid /= user_uid()) return
        if (group .and. attr%gid /= group_gid()) return
        dir = file_name_directory(file)
        if (len(dir) == 0) dir = "."
        call file_attributes(dir, parent, found)
        if (.not. found) return
        ! A file made in a setuid directory may take its owner, and one
        ! made in a setgid directory its group.
        if (.not. (parent%uid == user_uid() .or. iand(parent%mode, int(o'4000', int32)) == 0)) return
        if (group .and. parent%gid /= attr%gid) return
        file_ownership_preserved_p = .true.
    end function file_ownership_preserved_p

    !> Back up the current buffer's file before its first save, if
    !> appropriate, by renaming it to FILE~ or (to keep its owner, links
    !> or special bits) by copying it there.  After a rename, `setmodes'
    !> is the old file's permission bits, to give the new file; otherwise
    !> it is -1.  Port of files.el:backup-buffer (no numbered versions; a
    !> symbolic link is backed up by copying, beside the link).
    subroutine backup_buffer(setmodes)
        integer(int32), intent(out) :: setmodes
        character(len=:), allocatable :: file, backupname
        type(file_stat_type) :: attr
        type(status_type) :: status
        integer(int32) :: modes
        logical :: found, make_copy
        setmodes = -1
        if (.not. make_backup_files .or. buffers(cur)%backed_up) return
        if (.not. allocated(buffers(cur)%filename)) return
        file = buffers(cur)%filename
        ! backup-inhibited, set by find-file from backup-enable-predicate
        if (.not. normal_backup_enable_predicate(file)) return
        call file_attributes(file, attr, found)
        if (.not. found) return
        if (.not. file_regular_p(file)) return
        backupname = make_backup_file_name(file)
        modes = file_modes(file)
        make_copy = file_symlink_p(file) .or. &
                    (modes >= 0 .and. iand(modes, int(o'6000', int32)) /= 0) .or. &
                    .not. file_writable_p(file_name_directory(file)) .or. &
                    .not. file_ownership_preserved_p(file, .true.)
        if (make_copy) then
            ! backup-buffer-copy
            call copy_file(file, backupname, status, ok_if_already_exists=.true., keep_time=.true., &
                           preserve_uid_gid=.true.)
            if (status%ok() .and. modes >= 0) call set_file_modes(backupname, iand(modes, int(o'1777', int32)), status)
        else
            call rename_file(file, backupname, status, ok_if_already_exists=.true.)
            if (status%ok()) setmodes = modes
        end if
        ! A failed backup is ignored (condition-case file-error), and the
        ! fallback to %backup%~ in user-emacs-directory is not ported.
        if (status%ok()) then
            buffers(cur)%backed_up = .true.
        else
            call message("Cannot write backup file "//backupname)
        end if
    end subroutine backup_buffer

    ! ------------------------------------------------------------------
    ! Saving
    ! ------------------------------------------------------------------

    !> Save the current buffer in its visited file if modified.  `arg' is
    !> the numeric prefix: 4 (C-u) backs up the previous version on the
    !> next save, 16 (C-u C-u) now, 64 both; 0 never.
    !> Port of files.el:save-buffer.
    subroutine save_buffer(arg, called_interactively, status)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: called_interactively
        type(status_type), intent(out) :: status
        logical :: modp, saved_make_backup_files
        modp = buffers(cur)%text%save_modiff < buffers(cur)%text%modiff
        saved_make_backup_files = make_backup_files
        make_backup_files = (make_backup_files .and. arg /= 0) .or. arg == 16 .or. arg == 64
        if (modp .and. (arg == 16 .or. arg == 64)) buffers(cur)%backed_up = .false.
        if (modp .and. allocated(buffers(cur)%filename)) then
            call message("Saving file "//buffers(cur)%filename//"...")
        end if
        call basic_save_buffer(called_interactively, status)
        make_backup_files = saved_make_backup_files
        if (.not. status%ok()) return
        if (modp .and. (arg == 4 .or. arg == 64)) buffers(cur)%backed_up = .false.
    end subroutine save_buffer

    !> Save the current buffer in its visited file, if it was modified or
    !> the file doesn't exist; otherwise say so when called interactively.
    !> Port of files.el:basic-save-buffer with basic-save-buffer-2 (no
    !> hooks, final newline, file-precious-flag or break-hardlink-on-save).
    subroutine basic_save_buffer(called_interactively, status)
        logical, intent(in) :: called_interactively
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, dir, wrote
        integer(int32) :: setmodes
        logical :: modified, ok, tempsetmodes
        type(status_type) :: st
        modified = buffers(cur)%text%save_modiff < buffers(cur)%text%modiff
        if (.not. modified .and. allocated(buffers(cur)%filename)) then
            modified = .not. file_exists_p(buffers(cur)%filename)
        end if
        if (.not. modified) then
            if (called_interactively) call message("(No changes need to be saved)")
            return
        end if
        if (allocated(buffers(cur)%filename)) then
            file = buffers(cur)%filename
            if (.not. verify_visited_file_modtime() .and. file_exists_p(file)) then
                call yes_or_no_p(file_name_nondirectory(file)//" has changed since visited or saved.  Save anyway? ", &
                                 ok, status)
                if (.not. status%ok()) return
                if (.not. ok) then
                    call set_error(status, FM_USER_ERROR, "Save not confirmed")
                    return
                end if
            end if
        end if
        call final_newline_on_save(status)
        if (.not. status%ok()) return
        ! If buffer has no file name, ask user for one.
        if (.not. allocated(buffers(cur)%filename)) then
            call expand_file_name(buffers(cur)%name, dir, status)
            if (.not. status%ok()) return
            call read_file_name_in_minibuffer("File to save in: ", file, status, default_filename=dir)
            if (.not. status%ok()) return
            call expand_file_name(file, dir, status)
            if (.not. status%ok()) return
            file = dir
            if (file_exists_p(file)) then
                if (file_directory_p(file)) then
                    ! Signal an error if the user specified the name of an
                    ! existing directory.
                    call set_error(status, FM_GENERIC_ERROR, file//" is a directory")
                    return
                end if
                call y_or_n_p(format_message("File `")//file//format_message("' exists; overwrite? "), ok, status)
                if (.not. status%ok()) return
                if (.not. ok) then
                    call set_error(status, FM_GENERIC_ERROR, "Canceled")
                    return
                end if
            end if
            call set_visited_file_name(file, status)
            if (.not. status%ok()) return
        end if
        file = buffers(cur)%filename
        call expand_file_name(file, dir, status)
        if (.not. status%ok()) return
        dir = file_name_directory(dir)
        if (.not. file_exists_p(file) .and. .not. file_exists_p(dir)) then
            call y_or_n_p(format_message("Directory `")//dir//format_message("' does not exist; create? "), &
                          ok, status)
            if (.not. status%ok()) return
            if (.not. ok) then
                call set_error(status, FM_GENERIC_ERROR, "Canceled")
                return
            end if
            call make_directory(dir, status, parents=.true.)
            if (.not. status%ok()) return
        end if
        ! basic-save-buffer-2
        tempsetmodes = .false.
        if (.not. file_writable_p(file)) then
            if (.not. file_directory_p(dir)) then
                if (file_exists_p(dir)) then
                    call set_error(status, FM_GENERIC_ERROR, dir//" is not a directory")
                else
                    call set_error(status, FM_GENERIC_ERROR, dir//": No such directory")
                end if
                return
            end if
            if (.not. file_exists_p(file)) then
                call set_error(status, FM_GENERIC_ERROR, "Directory "//dir//" write-protected")
                return
            end if
            call yes_or_no_p("File "//file_name_nondirectory(file)// &
                             " is write-protected; try to save anyway? ", ok, status)
            if (.not. status%ok()) return
            if (.not. ok) then
                call set_error(status, FM_GENERIC_ERROR, &
                               format_message("Attempt to save to a file that you aren't allowed to write"))
                return
            end if
            tempsetmodes = .true.
        end if
        call backup_buffer(setmodes)
        ! Make a write-protected file writable while writing it; its bits
        ! are restored below.
        if (tempsetmodes .and. setmodes < 0) then
            setmodes = file_modes(file)
            if (.not. file_writable_p(file)) call set_file_modes(file, ior(setmodes, int(o'200', int32)), st)
        end if
        call write_region(file, .true., status, message=wrote)
        if (setmodes >= 0) call set_file_modes(file, setmodes, st)
        if (.not. status%ok()) return
        call message(wrote)
    end subroutine basic_save_buffer

    !> Add a newline at the end of the current buffer if
    !> `require_final_newline' says so (asking if it is neither t nor
    !> visit-save).  Part of files.el:basic-save-buffer.
    subroutine final_newline_on_save(status)
        type(status_type), intent(out) :: status
        logical :: add
        status = status_type()
        if (.not. missing_final_newline()) return
        select case (require_final_newline%value)
        case (FINAL_NEWLINE_NEVER)
            add = .false.
        case (FINAL_NEWLINE_SAVE, FINAL_NEWLINE_VISIT_SAVE)
            add = .true.
        case default
            call y_or_n_p("Buffer "//buffers(cur)%name//" does not end in newline.  Add one? ", add, status)
            if (.not. status%ok()) return
        end select
        if (add) call insert_final_newline(status)
    end subroutine final_newline_on_save

    !> Whether the whole current buffer is non-empty, writable and does
    !> not end in a newline.
    logical function missing_final_newline() result(missing)
        integer(int32) :: c
        logical :: found
        integer(int64) :: z
        missing = .false.
        if (buffers(cur)%read_only) return
        z = buffers(cur)%text%z
        if (z <= 1) return
        call char_after(c, found, z - 1)
        missing = found .and. c /= 10
    end function missing_final_newline

    !> (save-excursion (save-restriction (widen) (goto-char (point-max))
    !> (insert ?\n)))
    subroutine insert_final_newline(status)
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, savedb, savedz
        type(status_type) :: st
        opoint = point()
        savedb = buffers(cur)%begv
        savedz = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        call goto_char(point_max())
        call insert(achar(10), status)
        call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
        call goto_char(opoint)
    end subroutine insert_final_newline

    !> map-y-or-n-p prompter for save_some_buffers.
    subroutine save_prompter(k, how, prompt)
        integer, intent(in) :: k
        integer, intent(out) :: how
        character(len=:), allocatable, intent(out) :: prompt
        integer(int64) :: i
        type(status_type) :: status
        how = YNP_SKIP
        prompt = ""
        call resolve_buffer(pending(k), i, status)
        if (.not. status%ok()) return
        how = YNP_ASK
        if (.not. allocated(buffers(i)%filename)) then
            prompt = "Save buffer "//buffers(i)%name//"? "
        else if (buffers(i)%name == file_name_nondirectory(buffers(i)%filename) .or. &
                 index(buffers(i)%name, file_name_nondirectory(buffers(i)%filename)//"<") == 1) then
            prompt = "Save file "//buffers(i)%filename//"? "
        else
            prompt = "Save file "//buffers(i)%filename//" (buffer "//buffers(i)%name//")? "
        end if
    end subroutine save_prompter

    !> map-y-or-n-p actor for save_some_buffers: save that buffer.
    subroutine save_actor(k, status)
        integer, intent(in) :: k
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st
        old = current_buffer()
        call set_buffer(pending(k), status)
        if (.not. status%ok()) return
        call save_buffer(1_int64, .false., status)
        call set_buffer(old, st)
    end subroutine save_actor

    !> Ask about saving each modified file-visiting buffer, then save
    !> those the user accepts (all, without asking, if `arg').
    !> Port of files.el:save-some-buffers (PRED nil; no
    !> buffer-save-without-query, buffer-offer-save or
    !> save-some-buffers-functions).
    subroutine save_some_buffers(arg, called_interactively, status)
        logical, intent(in) :: arg, called_interactively
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: all(:)
        integer(int64) :: k, i, n
        integer :: done
        ! files--buffers-needing-to-be-saved
        all = buffer_list()
        allocate (pending(size(all)))
        n = 0
        do k = 1, size(all, kind=int64)
            call resolve_buffer(all(k), i, status)
            if (.not. status%ok()) cycle
            if (.not. allocated(buffers(i)%filename)) cycle
            if (buffers(i)%text%save_modiff >= buffers(i)%text%modiff) cycle
            n = n + 1
            pending(n) = all(k)
        end do
        status = status_type()
        done = 0
        if (arg) then
            do k = 1, n
                call save_actor(int(k), status)
                if (.not. status%ok()) exit
                done = done + 1
            end do
        else
            call map_y_or_n_p(int(n), save_prompter, save_actor, done, status, "buffer", "buffers", "save")
        end if
        deallocate (pending)
        if (.not. status%ok()) return
        if (n == 0 .and. called_interactively) call message("(No files need saving)")
    end subroutine save_some_buffers

    !> Whether any live buffer visits a file and is modified.
    logical function modified_file_buffers_exist()
        type(handle_type), allocatable :: all(:)
        integer(int64) :: k, i
        type(status_type) :: status
        modified_file_buffers_exist = .false.
        all = buffer_list()
        do k = 1, size(all, kind=int64)
            call resolve_buffer(all(k), i, status)
            if (.not. status%ok()) cycle
            if (.not. allocated(buffers(i)%filename)) cycle
            if (buffers(i)%text%save_modiff < buffers(i)%text%modiff) then
                modified_file_buffers_exist = .true.
                return
            end if
        end do
    end function modified_file_buffers_exist

    !> Offer to save each modified file buffer, then leave Emacs, asking
    !> first if any stay unsaved.  Port of files.el:save-buffers-kill-emacs
    !> (no processes, kill-emacs-query-functions or confirm-kill-emacs).
    subroutine save_buffers_kill_emacs(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        logical :: ok
        if (modified_file_buffers_exist()) then
            call save_some_buffers(arg, .false., status)
            if (.not. status%ok()) return
        end if
        if (modified_file_buffers_exist()) then
            call yes_or_no_p("Modified buffers exist; exit anyway? ", ok, status)
            if (.not. status%ok() .or. .not. ok) return
        end if
        call request_exit()
    end subroutine save_buffers_kill_emacs

    !> `name' in the current buffer's default directory.
    function file_in_default_directory(name) result(file)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: file
        type(status_type) :: status
        call expand_file_name(name, file, status)
        if (.not. status%ok()) file = name
    end function file_in_default_directory

    !> Make the current buffer visit `filename' (no file if ""): its name,
    !> default directory and backup state follow, and it counts as
    !> modified unless `along_with_file'.  Asks first if another buffer
    !> visits that file, unless `no_query'.  Port of
    !> files.el:set-visited-file-name (no truenames, locks, auto-save or
    !> major-mode change).
    subroutine set_visited_file_name(filename, status, no_query, along_with_file)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_query, along_with_file
        character(len=:), allocatable :: file, new_name, actual
        type(handle_type) :: other, cb
        logical :: ok, query, along
        type(major_mode_type), pointer :: mode
        type(status_type) :: st
        query = .true.
        if (present(no_query)) query = .not. no_query
        along = .false.
        if (present(along_with_file)) along = along_with_file
        if (len(filename) > 0) then
            call expand_file_name(filename, file, status)
            if (.not. status%ok()) return
            if (len(file_name_nondirectory(file)) == 0) then
                call set_error(status, FM_GENERIC_ERROR, "Empty file name")
                return
            end if
            other = get_file_buffer(file)
            cb = current_buffer()
            if (other%index /= 0 .and. other%index /= cb%index .and. query) then
                call y_or_n_p("A buffer is visiting "//file//"; proceed? ", ok, status)
                if (.not. status%ok()) return
                if (.not. ok) then
                    call set_error(status, FM_USER_ERROR, "Aborted")
                    return
                end if
            end if
            buffers(cur)%filename = file
            buffers(cur)%directory = file_name_directory(file)
            new_name = file_name_nondirectory(file)
            if (.not. (new_name == buffers(cur)%name .and. len(new_name) == len(buffers(cur)%name))) then
                call rename_buffer(new_name, .true., actual, status)
                if (.not. status%ok()) return
            end if
        else
            if (allocated(buffers(cur)%filename)) deallocate (buffers(cur)%filename)
        end if
        buffers(cur)%backed_up = .false.
        ! clear-visited-file-modtime
        if (.not. along) buffers(cur)%modtime_known = .false.
        if (allocated(buffers(cur)%filename) .and. .not. along) call restore_buffer_modified_p(.true.)
        ! Update the major mode, if the file name determines it; don't
        ! change the mode if it is special.  Errors are ignored.
        if (.not. change_major_mode_with_file_name) return
        mode => null()
        if (allocated(buffers(cur)%major_mode)) mode => major_mode_named(buffers(cur)%major_mode)
        if (associated(mode)) then
            if (mode%special) return
        end if
        call set_auto_mode(st, .true.)
    end subroutine set_visited_file_name

    !> Write the current buffer to `filename' and visit it there; a
    !> directory name means the buffer's file (or buffer) name in that
    !> directory.  With `confirm', ask before overwriting.  Port of
    !> files.el:write-file.
    subroutine write_file(filename, confirm, status)
        character(len=*), intent(in) :: filename
        logical, intent(in) :: confirm
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        logical :: ok
        integer(int32) :: old_modes
        type(status_type) :: st
        ! The old file's modes; it may have gone away (-1 then).
        old_modes = -1
        if (allocated(buffers(cur)%filename)) old_modes = file_modes(buffers(cur)%filename)
        file = filename
        if (len(file) > 0) then
            ! If arg is a directory name, use the default file name, but in
            ! that directory.
            if (directory_name_p(file)) then
                if (allocated(buffers(cur)%filename)) then
                    file = file//file_name_nondirectory(buffers(cur)%filename)
                else
                    file = file//file_name_nondirectory(buffers(cur)%name)
                end if
            end if
            if (confirm .and. file_exists_p(file)) then
                call y_or_n_p(format_message("File `")//file//format_message("' exists; overwrite? "), ok, status)
                if (.not. status%ok()) return
                if (.not. ok) then
                    call set_error(status, FM_USER_ERROR, "Canceled")
                    return
                end if
            end if
            call set_visited_file_name(file, status, no_query=.not. confirm)
            if (.not. status%ok()) return
        end if
        call restore_buffer_modified_p(.true.)
        ! Make the buffer writable if the file is.
        if (allocated(buffers(cur)%filename)) then
            if (file_writable_p(buffers(cur)%filename)) buffers(cur)%read_only = .false.
        end if
        call save_buffer(1_int64, .false., status)
        if (.not. status%ok()) return
        ! If the old file was executable, then make the new file executable,
        ! too.
        if (old_modes > 0 .and. allocated(buffers(cur)%filename)) then
            if (iand(old_modes, int(o'111', int32)) /= 0) then
                call set_file_modes(buffers(cur)%filename, &
                                    ior(iand(old_modes, int(o'111', int32)), file_modes(buffers(cur)%filename)), st)
            end if
        end if
    end subroutine write_file

    !> Edit file FILENAME.
    !> \<minibuffer_local_map>Switch to a buffer visiting file FILENAME, creating one if none
    !> already exists.
    !> Interactively, the default if you just type RET is the current directory,
    !> but the visited file name is available through the minibuffer history:
    !> type \[next_history_element] to pull it into the minibuffer.
    !> Port of files.el:find-file (interactive), with find-file-read-args (no
    !> wildcards, remote files or `file_name_at_point_functions').
    recursive subroutine cmd_find_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, dir
        dir = "~/"
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        call read_file_name_in_minibuffer("Find file: ", filename, status, default_filename=dir, &
                                          confirm_nonexistent=.true.)
        if (.not. status%ok()) return
        call find_file(filename, status)
    end subroutine cmd_find_file

    !> Edit file FILENAME, in another window.
    !>
    !> Like \[find_file] (which see), but creates a new window or reuses
    !> an existing one.  See the function `display_buffer'.
    !>
    !> \<minibuffer_local_map>Interactively, the default if you just type RET is the current directory,
    !> but the visited file name is available through the minibuffer history:
    !> type \[next_history_element] to pull it into the minibuffer.
    !>
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of files.el:find-file-other-window (interactive), with
    !> find-file-read-args (no wildcards or `file_name_at_point_functions').
    recursive subroutine cmd_find_file_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, dir
        dir = "~/"
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        call read_file_name_in_minibuffer("Find file in other window: ", filename, status, default_filename=dir, &
                                          confirm_nonexistent=.true.)
        if (.not. status%ok()) return
        call find_file_other_window(filename, status)
    end subroutine cmd_find_file_other_window

    !> Replace current buffer text with the text of the visited file on disk.
    !> This undoes all changes since the file was visited or saved.
    !> With a prefix argument, offer to revert from latest auto-save file, if
    !> that is more recent than the visited file.
    !>
    !> This command also implements an interface for special buffers
    !> that contain text that doesn't come from a file, but reflects
    !> some other data instead (e.g. Dired buffers, `buffer_list'
    !> buffers).  This is done via the variable `revert_buffer_function'.
    !> In these cases, it should reconstruct the buffer contents from the
    !> appropriate data.  Port of files.el:revert-buffer (the command).
    recursive subroutine cmd_revert_buffer(status)
        type(status_type), intent(out) :: status
        call revert_buffer(status, ignore_auto=nilp(current_prefix_arg))
    end subroutine cmd_revert_buffer

    ! ------------------------------------------------------------------
    ! revert-buffer-function
    ! ------------------------------------------------------------------

    !> Whether a live buffer has serial number SERIAL.
    logical function serial_live(serial)
        integer(int64), intent(in) :: serial
        integer(int64) :: k
        serial_live = .false.
        if (.not. allocated(buffers)) return
        do k = 1, size(buffers, kind=int64)
            if (buffers(k)%serial /= serial) cycle
            serial_live = buffer_live_p(buffer_handle(k))
            return
        end do
    end function serial_live

    subroutine revert_set_local(self, fn)
        class(revert_buffer_function_type), intent(inout) :: self
        procedure(revert_function_proc) :: fn
        type(revert_entry_type), allocatable :: grown(:)
        integer :: i, n
        if (cur == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                self%entries(i)%fn => fn
                return
            end if
        end do
        ! Reuse the entry of a dead buffer.
        do i = 1, size(self%entries)
            if (.not. serial_live(self%entries(i)%serial)) then
                self%entries(i)%serial = buffers(cur)%serial
                self%entries(i)%fn => fn
                return
            end if
        end do
        n = size(self%entries)
        allocate (grown(n + 1))
        grown(1:n) = self%entries
        grown(n + 1)%serial = buffers(cur)%serial
        grown(n + 1)%fn => fn
        call move_alloc(grown, self%entries)
    end subroutine revert_set_local

    function revert_get(self) result(fn)
        class(revert_buffer_function_type), intent(in) :: self
        procedure(revert_function_proc), pointer :: fn
        integer :: i
        fn => null()
        if (cur == 0) return
        if (.not. allocated(self%entries)) return
        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) fn => self%entries(i)%fn
        end do
    end function revert_get

    !> Write current buffer into file FILENAME.
    !> This makes the buffer visit that file, and marks it as not modified.
    !>
    !> Interactively, prompt for FILENAME.
    !> If you specify just a directory name as FILENAME, that means to write
    !> to a file in that directory.  In this case, the base name of the file
    !> is the same as that of the file visited in the buffer, or the buffer
    !> name sans leading directories, if any, if the buffer is not already
    !> visiting a file.
    !>
    !> You can also yank the file name into the minibuffer to edit it,
    !> using \<minibuffer_local_map>\[next_history_element].
    !>
    !> Confirmation is required before overwriting an existing file, unless
    !> you supply a prefix argument.  Port of files.el:write-file (interactive).
    subroutine cmd_write_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, dir, def
        if (allocated(buffers(cur)%filename)) then
            call read_file_name_in_minibuffer("Write file: ", filename, status)
        else
            dir = "~/"
            if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
            call expand_file_name(file_name_nondirectory(buffers(cur)%name), def, status, dir)
            if (.not. status%ok()) return
            call read_file_name_in_minibuffer("Write file: ", filename, status, dir=dir, default_filename=def)
        end if
        if (.not. status%ok()) return
        call write_file(filename, nilp(current_prefix_arg), status)
    end subroutine cmd_write_file

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Save current buffer in visited file if modified.
    !> Variations are described below.
    !>
    !> By default, makes the previous version into a backup file
    !>  if previously requested or if this is the first save.
    !> Prefixed with one \[universal_argument], marks this version
    !>  to become a backup when the next save is done.
    !> Prefixed with two \[universal_argument]'s,
    !>  makes the previous version into a backup file.
    !> Prefixed with three \[universal_argument]'s, marks this version
    !>  to become a backup when the next save is done,
    !>  and makes the previous version into a backup file.
    !>
    !> With a numeric prefix argument of 0, never make the previous version
    !> into a backup file.
    !>
    !> Note that `make_backup_files' and `backup_enable_predicate' still
    !> control whether a backup will actually be produced, even when you
    !> invoke this command prefixed with two or three \[universal_argument]'s.
    !>
    !> If a file's name is FOO, its backup file is called FOO~.
    !> Port of files.el:save-buffer (the command; no numbered backups).
    subroutine cmd_save_buffer(status)
        type(status_type), intent(out) :: status
        call save_buffer(prefix_numeric_value(current_prefix_arg), .true., status)
    end subroutine cmd_save_buffer

    !> Choose the major mode for this buffer automatically.
    !> Uses the visited file name and the -*- line.
    !>
    !> This function is called automatically from `find_file'.  Port of
    !> files.el:normal-mode as a command.
    recursive subroutine cmd_normal_mode(status)
        type(status_type), intent(out) :: status
        call normal_mode(status)
    end subroutine cmd_normal_mode

    !> Save some modified file-visiting buffers.  Asks user about each one.
    !> You can answer \`y' or \`SPC' to save, \`n' or \`DEL' not to save,
    !> \`!' to save the buffer and all remaining
    !> buffers without any further querying, \`.' to save only the
    !> current buffer and skip the remaining ones and \`q' or \`RET' to exit
    !> the function without saving any more buffers.
    !>
    !> A prefix argument means save all with no questions.
    !> Port of files.el:save-some-buffers (the command).
    subroutine cmd_save_some_buffers(status)
        type(status_type), intent(out) :: status
        call save_some_buffers(.not. nilp(current_prefix_arg), .true., status)
    end subroutine cmd_save_some_buffers

    !> Offer to save each buffer, then kill Emacs.
    !>
    !> With prefix ARG, silently save all file-visiting buffers, then kill.
    !> Port of files.el:save-buffers-kill-terminal (no emacsclient frames).
    subroutine cmd_save_buffers_kill_terminal(status)
        type(status_type), intent(out) :: status
        call save_buffers_kill_emacs(.not. nilp(current_prefix_arg), status)
    end subroutine cmd_save_buffers_kill_terminal

    !> Register the commands and their keys in `ctl_x_map' (and
    !> `ctl_x_4_map').
    subroutine syms_of_files(ctl_x_map, ctl_x_4_map)
        type(handle_type), intent(in) :: ctl_x_map
        type(handle_type), intent(in), optional :: ctl_x_4_map
        type(value_type) :: key(1)
        type(status_type) :: status
        if (.not. allocated(insert_directory_program)) insert_directory_program = "ls"
        if (.not. allocated(directory_listing_before_filename_regexp)) call init_directory_listing_regexp()
        commands%find_file%run => cmd_find_file
        commands%find_file_other_window%run => cmd_find_file_other_window
        commands%revert_buffer%run => cmd_revert_buffer
        call syms_of_files_copy()
        call syms_of_files_read(ctl_x_map, ctl_x_4_map)
        if (present(ctl_x_4_map)) then
            ! files.el: (define-key ctl-x-4-map "f" 'find-file-other-window)
            key(1) = make_fixnum(int(iachar("f"), int64))
            call define_key(ctl_x_4_map, key, sym("find_file_other_window"), status)
            ! files.el: (define-key ctl-x-4-map "\C-f" 'find-file-other-window)
            key(1) = make_fixnum(6_int64)
            call define_key(ctl_x_4_map, key, sym("find_file_other_window"), status)
        end if
        commands%normal_mode%run => cmd_normal_mode
        commands%write_file%run => cmd_write_file
        commands%save_buffer%run => cmd_save_buffer
        commands%save_some_buffers%run => cmd_save_some_buffers
        commands%save_buffers_kill_terminal%run => cmd_save_buffers_kill_terminal
        ! files.el: (define-key ctl-x-map "\C-f" 'find-file)
        key(1) = make_fixnum(6_int64)
        call define_key(ctl_x_map, key, sym("find_file"), status)
        ! files.el: (define-key ctl-x-map "\C-w" 'write-file)
        key(1) = make_fixnum(23_int64)
        call define_key(ctl_x_map, key, sym("write_file"), status)
        ! files.el: (define-key ctl-x-map "\C-s" 'save-buffer)
        key(1) = make_fixnum(19_int64)
        call define_key(ctl_x_map, key, sym("save_buffer"), status)
        ! files.el: (define-key ctl-x-map "s" 'save-some-buffers)
        key(1) = make_fixnum(int(iachar("s"), int64))
        call define_key(ctl_x_map, key, sym("save_some_buffers"), status)
        ! files.el: (define-key ctl-x-map "\C-c" 'save-buffers-kill-terminal)
        key(1) = make_fixnum(3_int64)
        call define_key(ctl_x_map, key, sym("save_buffers_kill_terminal"), status)
    end subroutine syms_of_files

    !> Set `directory_listing_before_filename_regexp' to its standard value.
    subroutine init_directory_listing_regexp()
        character(len=:), allocatable :: l, l_or_quote, month, sp, yyyy, dd, hh_mm, seconds, zone, iso_mm_dd, &
                                         iso_time, iso, western, western_comma, dd_mmm_yyyy, mm, east_asian
        l = "\([A-Za-z]\|[^"//achar(0)//"-"//achar(127)//"]\)"
        l_or_quote = "\([A-Za-z']\|[^"//achar(0)//"-"//achar(127)//"]\)"
        ! In some locales, month abbreviations are as short as 2 letters,
        ! and they can be followed by ".".
        ! In Breton, a month name  can include a quote character.
        month = l_or_quote//l_or_quote//"+\.?"
        sp = " "
        yyyy = "[0-9][0-9][0-9][0-9]"
        dd = "[ 0-3][0-9]"
        hh_mm = "[ 0-2][0-9][:.][0-5][0-9]"
        seconds = "[0-6][0-9]\([.,][0-9]+\)?"
        zone = "[-+][0-2][0-9][0-5][0-9]"
        iso_mm_dd = "[01][0-9]-[0-3][0-9]"
        iso_time = hh_mm//"\(:"//seconds//"\( ?"//zone//"\)?\)?"
        iso = "\(\("//yyyy//"-\)?"//iso_mm_dd//"[ T]"//iso_time//"\|"//yyyy//"-"//iso_mm_dd//"\)"
        western = "\("//month//sp//"+"//dd//"\|"//dd//"\.?"//sp//month//"\)"//sp//"+"//"\("//hh_mm// &
                  "\|"//yyyy//"\)"
        western_comma = month//sp//"+"//dd//","//sp//"+"//yyyy
        ! This represents the date in strftime(3) format "%e-%b-%Y"
        ! (aka "%v"), as it is the default for many ls incarnations.
        dd_mmm_yyyy = dd//"-"//month//"-"//yyyy//sp//hh_mm
        ! Japanese MS-Windows ls-lisp has one-digit months, and
        ! omits the Kanji characters after month and day-of-month.
        ! On Mac OS X 10.3, the date format in East Asian locales is
        ! day-of-month digits followed by month digits.
        mm = "[ 0-1]?[0-9]"
        east_asian = "\("//mm//l//"?"//sp//dd//l//"?"//sp//"+"//"\|"//dd//sp//mm//sp//"+"//"\)"//"\("//hh_mm// &
                     "\|"//yyyy//l//"?"//"\)"
        ! The "[0-9]" below requires the previous column to end in a digit.
        ! The "[BkKMGTPEZYRQ]?" below supports "ls -alh" output.
        ! For non-iso date formats, we add the ".*" in order to find
        ! the last possible match.
        directory_listing_before_filename_regexp = "\([0-9][BkKMGTPEZYRQ]? "//iso//"\|.*[0-9][BkKMGTPEZYRQ]? "// &
                                                   "\("//western//"\|"//western_comma//"\|"//dd_mmm_yyyy// &
                                                   "\|"//east_asian//"\)"//"\) +"
    end subroutine init_directory_listing_regexp

    !> Make directory DIR if it is not already a directory.
    !> `already' is true if DIR is already a directory.  Port of
    !> files.el:files--ensure-directory.
    subroutine ensure_directory(dir, already, status)
        character(len=*), intent(in) :: dir
        logical, intent(out) :: already
        type(status_type), intent(out) :: status
        already = .false.
        call make_directory_internal(dir, status)
        if (.not. status%ok()) then
            already = file_directory_p(dir)
            if (already) status = status_type()
        end if
    end subroutine ensure_directory

    !> Create the directory DIR and optionally any nonexistent parent dirs.
    !>
    !> PARENTS, if true, says whether to create parent directories that
    !> don't exist.
    !>
    !> ALREADY is true if PARENTS is true and DIR already exists as a
    !> directory, and false if DIR did not already exist but was created.
    !> Signal an error if unsuccessful.  Port of files.el:make-directory.
    subroutine make_directory(dir, status, parents, already)
        character(len=*), intent(in) :: dir
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: parents
        logical, intent(out), optional :: already
        character(len=:), allocatable :: absname, d, parent
        type(status_type) :: est
        logical :: already_dir
        integer :: k, n
        integer, allocatable :: ends(:)
        if (present(already)) already = .false.
        call expand_file_name(dir, absname, status)
        if (.not. status%ok()) return
        if (.not. present(parents)) then
            call make_directory_internal(absname, status)
            return
        end if
        if (.not. parents) then
            call make_directory_internal(absname, status)
            return
        end if
        ! Walk up to the first directory that exists (or can be made),
        ! remembering the ones to create below it.
        d = directory_file_name(absname)
        allocate (ends(0))
        do
            parent = directory_file_name(file_name_directory(d))
            call ensure_directory(d, already_dir, est)
            if (est%ok()) exit
            ! Do not loop if root does not exist (Bug#2309).
            if (d == parent) exit
            ends = [len(d), ends]
            d = parent
        end do
        n = size(ends)
        do k = 1, n
            call ensure_directory(absname(1:ends(k)), already_dir, status)
            if (.not. status%ok()) return
        end do
        if (n == 0 .and. .not. est%ok()) then
            status = est
            return
        end if
        if (present(already)) already = already_dir
    end subroutine make_directory
end module files_fm
