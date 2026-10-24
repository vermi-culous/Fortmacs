! abbrev_cmds_fm.f90 -- the procedures and commands of abbrev_fm.
! Ported from GNU Emacs 31.1 lisp/abbrev.el.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> The abbrev tables, expansion and the abbrev commands.  A submodule of
!> abbrev_fm because they use modules (modes, windows, the minibuffer)
!> that use abbrev_fm.
submodule(abbrev_fm) abbrev_cmds_fm
    use, intrinsic :: iso_fortran_env, only: int32
    use commands_fm, only: commands
    use error_fm, only: set_error, format_message, FM_GENERIC_ERROR
    use lisp_fm, only: nilp
    use data_fm, only: add_local_variables_killer
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, erase_buffer, kill_buffer, &
                         restore_buffer_modified_p, generate_new_buffer_name
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use editfns_fm, only: point, point_min, point_max, goto_char, buffer_substring, delete_region, char_after, &
                          line_beginning_position, region_beginning, region_end, eobp, point_marker
    use insdel_fm, only: insert, replace_range
    use syntax_fm, only: forward_word, skip_syntax_forward, skip_syntax_backward, syntax_table, set_syntax_table, &
                         standard_syntax_table
    use search_fm, only: string_match, match_beginning, match_end, case_fold_search, search_forward, &
                         SEARCH_NOERROR, SEARCH_MOVE
    use subr_fm, only: looking_back, y_or_n_p
    use casefiddle_fm, only: downcase_string, upcase_region, upcase_initials_region
    use cmds_fm, only: delete_char
    use indent_fm, only: indent_to
    use print_fm, only: fortran_string
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use undo_fm, only: undo_boundary
    use frame_fm, only: selected_frame
    use minibuf_fm, only: read_string
    use minibuffer_fm, only: read_file_name
    use simple_fm, only: use_region_p, push_mark
    use window_fm, only: display_buffer, switch_to_buffer, same_handle
    use fileio_fm, only: file_exists_p, file_name_directory, expand_file_name, insert_file_contents, write_region, &
                         file_directory_p
    use files_fm, only: make_directory, add_save_some_buffers_function
    use config_fm, only: init_file_name
    use lread_fm, only: user_init_file
    use xdisp_fm, only: message
    use modes_fm, only: major_modes, minor_modes
    implicit none(type, external)

    !> The local abbrev table of a buffer slot (`serial' tells buffers
    !> apart), when it has its own.
    type :: local_table_type
        integer(int64) :: serial = 0
        type(abbrev_table_type), pointer :: table => null()
    end type local_table_type

    !> Buffers' local abbrev tables, by buffer slot.
    type(local_table_type), allocatable :: local_tables(:)

    character(len=*), parameter :: LF = achar(10)

    !> The lines that delimit the abbrevs `write_abbrev_file' writes into
    !> the init file.
    character(len=*), parameter :: BLOCK_START = "! Abbrevs, written by write_abbrev_file:"
    character(len=*), parameter :: BLOCK_END = "! End of abbrevs."

contains

    ! ------------------------------------------------------------------
    ! Tables
    ! ------------------------------------------------------------------

    !> Whether A and B are the same string (Fortran's == ignores
    !> trailing blanks).
    pure logical function same_string(a, b)
        character(len=*), intent(in) :: a, b
        same_string = .false.
        if (len(a) /= len(b)) return
        same_string = a == b
    end function same_string

    !> Whether A sorts before B by character codes (string-lessp): a
    !> prefix sorts first.  UTF-8 keeps the order of the codes.
    pure logical function string_lessp(a, b)
        character(len=*), intent(in) :: a, b
        integer :: k
        do k = 1, min(len(a), len(b))
            if (a(k:k) /= b(k:k)) then
                string_lessp = ichar(a(k:k)) < ichar(b(k:k))
                return
            end if
        end do
        string_lessp = len(a) < len(b)
    end function string_lessp

    !> The index of the abbrev named NAME in TABLE (interned upstream), or 0.
    pure integer function find_abbrev(table, name) result(k)
        type(abbrev_table_type), intent(in) :: table
        character(len=*), intent(in) :: name
        integer :: i
        k = 0
        do i = 1, table%n
            if (same_string(table%abbrevs(i)%name, name)) then
                k = i
                return
            end if
        end do
    end function find_abbrev

    !> The index of the abbrev named NAME in TABLE, added undefined if it
    !> is not there (obarray-put).
    integer function intern_abbrev(table, name) result(k)
        type(abbrev_table_type), intent(inout) :: table
        character(len=*), intent(in) :: name
        type(abbrev_type), allocatable :: grown(:)
        k = find_abbrev(table, name)
        if (k > 0) return
        if (.not. allocated(table%abbrevs)) allocate (table%abbrevs(8))
        if (table%n == size(table%abbrevs)) then
            allocate (grown(2*table%n))
            grown(1:table%n) = table%abbrevs(1:table%n)
            call move_alloc(grown, table%abbrevs)
        end if
        table%n = table%n + 1
        k = table%n
        table%abbrevs(k)%name = name
    end function intern_abbrev

    !> Put the built-in tables in `abbrev_table_name_list' the first time:
    !> fundamental-mode-abbrev-table and global-abbrev-table.
    subroutine ensure_tables()
        if (allocated(abbrev_table_name_list)) return
        allocate (abbrev_table_name_list(2))
        abbrev_table_name_list(1)%p => fundamental_mode_abbrev_table
        abbrev_table_name_list(2)%p => global_abbrev_table
    end subroutine ensure_tables

    module procedure abbrev_table_define
    integer :: k, system_flag
    logical :: unchanged
    if (present(system)) then
        system_flag = merge(1, 0, system)
    else
        system_flag = 0
    end if
    if (present(force)) then
        if (force) system_flag = 2
    end if
    k = intern_abbrev(self, abbrev)
    ! Don't override a prior user-defined abbrev with a system abbrev,
    ! unless system-flag is `force'.
    if (system_flag == 1 .and. self%abbrevs(k)%bound .and. allocated(self%abbrevs(k)%expansion) .and. &
        .not. self%abbrevs(k)%system) return
    if (system_flag == 0) then
        unchanged = self%abbrevs(k)%bound
        if (unchanged) then
            if (present(expansion)) then
                unchanged = .false.
                if (allocated(self%abbrevs(k)%expansion)) unchanged = same_string(self%abbrevs(k)%expansion, expansion)
            else
                unchanged = .not. allocated(self%abbrevs(k)%expansion)
            end if
        end if
        if (unchanged) then
            if (present(hook)) then
                unchanged = associated(self%abbrevs(k)%hook, hook)
            else
                unchanged = .not. associated(self%abbrevs(k)%hook)
            end if
        end if
        if (.not. unchanged) abbrevs_changed = .true.
    end if
    if (present(expansion)) then
        self%abbrevs(k)%expansion = expansion
    else if (allocated(self%abbrevs(k)%expansion)) then
        deallocate (self%abbrevs(k)%expansion)
    end if
    self%abbrevs(k)%hook => null()
    if (present(hook)) self%abbrevs(k)%hook => hook
    self%abbrevs(k)%bound = .true.
    ! The properties are replaced (setplist).
    self%abbrevs(k)%count = 0
    if (present(count)) self%abbrevs(k)%count = count
    ! Don't store the `force' value of `system-flag' into the :system
    ! property.
    self%abbrevs(k)%system = system_flag /= 0
    self%abbrevs(k)%case_fixed = .false.
    if (present(case_fixed)) self%abbrevs(k)%case_fixed = case_fixed
    self%abbrevs(k)%no_self_insert = .false.
    if (present(no_self_insert)) self%abbrevs(k)%no_self_insert = no_self_insert
    self%abbrevs(k)%enable_function => null()
    if (present(enable_function)) self%abbrevs(k)%enable_function => enable_function
    self%modiff = self%modiff + 1
    end procedure abbrev_table_define

    module procedure abbrev_table_define_table
    integer :: k
    type(abbrev_table_ref_type), allocatable :: grown(:)
    type(abbrev_table_type), pointer :: table
    call ensure_tables()
    self%name = name
    table => null()
    select type (self)
    type is (abbrev_table_type)
        table => self
    end select
    if (.not. associated(table)) return
    do k = 1, size(abbrev_table_name_list)
        if (associated(abbrev_table_name_list(k)%p, table)) return
    end do
    allocate (grown(size(abbrev_table_name_list) + 1))
    grown(1)%p => table
    grown(2:) = abbrev_table_name_list
    call move_alloc(grown, abbrev_table_name_list)
    end procedure abbrev_table_define_table

    module procedure clear_abbrev_table
    abbrevs_changed = .true.
    if (allocated(self%abbrevs)) deallocate (self%abbrevs)
    self%n = 0
    ! Preserve the table's properties.
    self%modiff = self%modiff + 1
    end procedure clear_abbrev_table

    module procedure abbrev_table_empty_p
    integer :: k
    logical :: ignore
    ignore = .false.
    if (present(ignore_system)) ignore = ignore_system
    empty = .true.
    do k = 1, self%n
        if (len(self%abbrevs(k)%name) == 0) cycle
        if (ignore .and. self%abbrevs(k)%system) cycle
        empty = .false.
        return
    end do
    end procedure abbrev_table_empty_p

    module procedure abbrev_table_add_parent
    type(abbrev_table_ref_type), allocatable :: grown(:)
    integer :: n
    n = 0
    if (allocated(self%parents)) n = size(self%parents)
    allocate (grown(n + 1))
    if (n > 0) grown(1:n) = self%parents
    grown(n + 1)%p => parent
    call move_alloc(grown, self%parents)
    end procedure abbrev_table_add_parent

    module procedure kill_all_abbrevs
    integer :: k
    call ensure_tables()
    do k = 1, size(abbrev_table_name_list)
        call abbrev_table_name_list(k)%p%clear()
    end do
    end procedure kill_all_abbrevs

    module procedure copy_abbrev_table
    integer :: k
    do k = 1, table%n
        associate (a => table%abbrevs(k))
            if (allocated(a%expansion)) then
                if (associated(a%hook)) then
                    call new%define(a%name, a%expansion, hook=a%hook)
                else
                    call new%define(a%name, a%expansion)
                end if
            else if (associated(a%hook)) then
                call new%define(a%name, hook=a%hook)
            else
                call new%define(a%name)
            end if
        end associate
    end do
    end procedure copy_abbrev_table

    module procedure abbrev_table_name
    integer :: k
    call ensure_tables()
    name = ""
    do k = 1, size(abbrev_table_name_list)
        if (associated(abbrev_table_name_list(k)%p, table)) then
            name = trim(abbrev_table_name_list(k)%p%name)
            return
        end if
    end do
    end procedure abbrev_table_name

    module procedure abbrev_table_symbols
    integer :: k, m
    logical :: sys
    integer :: width
    sys = .false.
    if (present(system)) sys = system
    width = 1
    m = 0
    do k = 1, table%n
        if (.not. user_abbrev(k)) cycle
        m = m + 1
        width = max(width, len(table%abbrevs(k)%name))
    end do
    allocate (character(len=width) :: names(m))
    m = 0
    do k = 1, table%n
        if (.not. user_abbrev(k)) cycle
        m = m + 1
        names(m) = table%abbrevs(k)%name
    end do
contains

    logical function user_abbrev(i)
        integer, intent(in) :: i
        user_abbrev = allocated(table%abbrevs(i)%expansion)
        if (user_abbrev .and. .not. sys) user_abbrev = .not. table%abbrevs(i)%system
    end function user_abbrev
    end procedure abbrev_table_symbols

    !> The abbrevs of TABLE that `abbrev_table_symbols' gives (with SYSTEM),
    !> sorted by name (string-lessp), as indices.
    subroutine sorted_abbrevs(table, system, order)
        type(abbrev_table_type), intent(in) :: table
        logical, intent(in) :: system
        integer, allocatable, intent(out) :: order(:)
        integer :: k, m, j, x
        allocate (order(table%n))
        m = 0
        do k = 1, table%n
            if (.not. allocated(table%abbrevs(k)%expansion)) cycle
            if (.not. system .and. table%abbrevs(k)%system) cycle
            m = m + 1
            order(m) = k
        end do
        order = order(1:m)
        ! Insertion sort (few abbrevs).
        do k = 2, m
            x = order(k)
            j = k - 1
            do while (j >= 1)
                if (.not. string_lessp(table%abbrevs(x)%name, table%abbrevs(order(j))%name)) exit
                order(j + 1) = order(j)
                j = j - 1
            end do
            order(j + 1) = x
        end do
    end subroutine sorted_abbrevs

    ! ------------------------------------------------------------------
    ! Local tables (buffer.c's BVAR abbrev_table)
    ! ------------------------------------------------------------------

    module procedure local_abbrev_table
    table => fundamental_mode_abbrev_table
    if (cur == 0) return
    if (.not. allocated(local_tables)) return
    if (cur > size(local_tables, kind=int64)) return
    if (local_tables(cur)%serial /= buffers(cur)%serial) return
    if (associated(local_tables(cur)%table)) table => local_tables(cur)%table
    end procedure local_abbrev_table

    module procedure set_local_abbrev_table
    type(local_table_type), allocatable :: grown(:)
    if (cur == 0) return
    if (.not. allocated(local_tables)) allocate (local_tables(max(16_int64, cur)))
    if (cur > size(local_tables, kind=int64)) then
        allocate (grown(max(2*size(local_tables, kind=int64), cur)))
        grown(1:size(local_tables)) = local_tables
        call move_alloc(grown, local_tables)
    end if
    local_tables(cur)%serial = buffers(cur)%serial
    local_tables(cur)%table => table
    end procedure set_local_abbrev_table

    !> Forget the current buffer's local abbrev table, as
    !> kill-all-local-variables resets BVAR abbrev_table to its default.
    subroutine kill_local_abbrev_table()
        if (cur == 0) return
        if (.not. allocated(local_tables)) return
        if (cur > size(local_tables, kind=int64)) return
        local_tables(cur)%serial = 0
        local_tables(cur)%table => null()
    end subroutine kill_local_abbrev_table

    module procedure abbrev_no_self_insert
    integer :: k
    abbrev_no_self_insert = .false.
    if (.not. associated(abbrev%table)) return
    k = find_abbrev(abbrev%table, abbrev%name)
    if (k == 0) return
    abbrev_no_self_insert = associated(abbrev%table%abbrevs(k)%hook) .and. abbrev%table%abbrevs(k)%no_self_insert
    end procedure abbrev_no_self_insert

    module procedure abbrev_mode_p
    abbrev_mode_p = .false.
    if (associated(abbrev_mode)) abbrev_mode_p = abbrev_mode%value
    end procedure abbrev_mode_p

    ! ------------------------------------------------------------------
    ! Looking up abbrevs
    ! ------------------------------------------------------------------

    !> The list of abbrev tables that are currently active, or TABLE.
    !> Port of abbrev.el:abbrev--active-tables (no minor-mode tables).
    subroutine active_tables(tables, table)
        type(abbrev_table_ref_type), allocatable, intent(out) :: tables(:)
        type(abbrev_table_type), intent(in), target, optional :: table
        if (present(table)) then
            allocate (tables(1))
            tables(1)%p => table
            return
        end if
        allocate (tables(2))
        tables(1)%p => local_abbrev_table()
        tables(2)%p => global_abbrev_table
    end subroutine active_tables

    !> Take the first table off QUEUE into TABLE and put its parents in
    !> front of the rest.
    subroutine pop_table(queue, table)
        type(abbrev_table_ref_type), allocatable, intent(inout) :: queue(:)
        type(abbrev_table_type), pointer, intent(out) :: table
        type(abbrev_table_ref_type), allocatable :: rest(:)
        integer :: np
        table => queue(1)%p
        np = 0
        if (allocated(table%parents)) np = size(table%parents)
        allocate (rest(np + size(queue) - 1))
        if (np > 0) rest(1:np) = table%parents
        rest(np + 1:) = queue(2:)
        call move_alloc(rest, queue)
    end subroutine pop_table

    !> The index of the abbrev named ABBREV in TABLE, or 0 if such an
    !> abbrev is not defined.  Port of abbrev.el:abbrev--symbol.
    integer function table_symbol(abbrev, table) result(k)
        character(len=*), intent(in) :: abbrev
        type(abbrev_table_type), intent(in) :: table
        integer :: j
        ! First try without case-folding.
        k = find_abbrev(table, abbrev)
        if (k == 0 .and. .not. table%case_fixed) then
            ! We didn't find any abbrev, try case-folding.
            j = find_abbrev(table, downcase_string(abbrev))
            ! Only use it if it doesn't require :case-fixed.
            if (j > 0) then
                if (.not. table%abbrevs(j)%case_fixed) k = j
            end if
        end if
        if (k > 0) then
            if (.not. allocated(table%abbrevs(k)%expansion)) k = 0
        end if
    end function table_symbol

    !> abbrev-symbol over the tables TABLES.
    subroutine symbol_in(abbrev, tables, result)
        character(len=*), intent(in) :: abbrev
        type(abbrev_table_ref_type), allocatable, intent(inout) :: tables(:)
        type(abbrev_ref_type), intent(out) :: result
        type(abbrev_table_type), pointer :: table
        integer :: k
        do while (size(tables) > 0)
            call pop_table(tables, table)
            k = table_symbol(abbrev, table)
            if (k > 0) then
                result%table => table
                result%name = table%abbrevs(k)%name
                return
            end if
        end do
    end subroutine symbol_in

    module procedure abbrev_symbol
    type(abbrev_table_ref_type), allocatable :: tables(:)
    call active_tables(tables, table)
    call symbol_in(abbrev, tables, result)
    end procedure abbrev_symbol

    module procedure abbrev_expansion
    type(abbrev_ref_type) :: sym
    integer :: k
    call abbrev_symbol(abbrev, sym, table)
    if (.not. associated(sym%table)) return
    k = find_abbrev(sym%table, sym%name)
    if (k > 0) then
        if (allocated(sym%table%abbrevs(k)%expansion)) expansion = sym%table%abbrevs(k)%expansion
    end if
    end procedure abbrev_expansion

    ! ------------------------------------------------------------------
    ! Expanding
    ! ------------------------------------------------------------------

    !> Forget `abbrev_start_location' (freeing its marker).
    subroutine clear_start_location()
        type(status_type) :: st
        if (abbrev_start_location%index /= 0) call free_marker(abbrev_start_location, st)
        abbrev_start_location = handle_type()
    end subroutine clear_start_location

    !> Try and find an abbrev before point: SYM (no table if none), the
    !> NAME it was found by, and its WORDSTART and WORDEND.  Port of
    !> abbrev.el:abbrev--before-point.
    recursive subroutine before_point(sym, name, wordstart, wordend, status)
        type(abbrev_ref_type), intent(out) :: sym
        character(len=:), allocatable, intent(out) :: name
        integer(int64), intent(out) :: wordstart, wordend
        type(status_type), intent(out) :: status
        type(abbrev_ref_type) :: found
        type(abbrev_table_ref_type), allocatable :: tables(:)
        type(abbrev_table_type), pointer :: table
        integer(int64) :: pos, start, end, lim, moved
        integer(int32) :: c
        logical :: have, enabled
        integer :: k
        type(status_type) :: st
        status = status_type()
        wordstart = 0
        wordend = 0
        name = ""
        if (.not. same_handle(abbrev_start_location_buffer, current_buffer())) call clear_start_location()
        call active_tables(tables)
        pos = point()
        if (abbrev_start_location%index /= 0) then
            call marker_position(abbrev_start_location, start, st)
            call clear_start_location()
            ! Remove the hyphen inserted by `abbrev-prefix-mark'.
            if (start < point_max()) then
                call char_after(c, have, start)
                if (have .and. c == iachar("-")) then
                    call delete_region(start, start + 1, status)
                    if (.not. status%ok()) return
                    pos = pos - 1
                end if
            end if
            moved = skip_syntax_backward(" ")
            end = point()
            if (end > start) then
                call buffer_substring(start, end, name, status)
                if (.not. status%ok()) return
                ! Restore point.
                call goto_char(pos)
                call symbol_in(name, tables, sym)
                wordstart = start
                wordend = end
            end if
            return
        end if
        do while (size(tables) > 0)
            if (associated(found%table)) exit
            call pop_table(tables, table)
            enabled = .true.
            if (associated(table%enable_function)) enabled = table%enable_function()
            have = .false.
            if (enabled) then
                if (.not. allocated(table%regexp)) then
                    ! We used to default `re' to "\\<\\(\\w+\\)\\W*"
                    ! but when words-include-escapes is set, that
                    ! is not right and fixing it is boring.
                    lim = point()
                    call forward_word(-1_int64)
                    start = point()
                    call forward_word(1_int64)
                    end = min(point(), lim)
                    have = .true.
                else if (looking_back(table%regexp, status, limit=line_beginning_position())) then
                    start = match_beginning(1, st)
                    end = match_end(1, st)
                    have = .true.
                end if
                if (.not. status%ok()) return
            end if
            if (have) then
                call buffer_substring(start, end, name, status)
                if (.not. status%ok()) return
                k = table_symbol(name, table)
                if (k > 0) then
                    enabled = .true.
                    if (associated(table%abbrevs(k)%enable_function)) enabled = table%abbrevs(k)%enable_function()
                    if (enabled) then
                        ! This will also look it up in parent tables.
                        ! This is not on purpose, but it seems harmless.
                        found%table => table
                        found%name = table%abbrevs(k)%name
                        wordstart = start
                        wordend = end
                    end if
                end if
            end if
            ! Restore point.
            call goto_char(pos)
        end do
        if (associated(found%table)) sym = found
    end subroutine before_point

    module procedure abbrev_insert
    character(len=:), allocatable :: nm, expansion
    integer(int64) :: wstart, wend, p0, a, b, end, moved
    integer :: k
    logical :: saved_fold, value, upper, lower
    procedure(abbrev_hook_proc), pointer :: hook
    type(status_type) :: st
    status = status_type()
    if (.not. associated(abbrev%table)) return
    k = find_abbrev(abbrev%table, abbrev%name)
    if (k == 0) return
    nm = abbrev%name
    if (present(name)) nm = name
    wstart = point()
    if (present(wordstart)) wstart = wordstart
    wend = wstart
    if (present(wordend)) wend = wordend
    ! Increment use count.
    abbrev%table%abbrevs(k)%count = abbrev%table%abbrevs(k)%count + 1
    result = abbrev
    ! If this abbrev has an expansion, delete the abbrev
    ! and insert the expansion.
    if (allocated(abbrev%table%abbrevs(k)%expansion)) then
        expansion = abbrev%table%abbrevs(k)%expansion
        call goto_char(wstart)
        ! Insert at beginning so that markers at the end (e.g. point)
        ! are preserved.
        call insert(expansion, status)
        if (.not. status%ok()) return
        call delete_char(wend - wstart, status)
        if (.not. status%ok()) return
        saved_fold = case_fold_search%value
        case_fold_search = .false.
        ! If the abbrev's name is different from the buffer text (the
        ! only difference should be capitalization), then we may want
        ! to adjust the capitalization of the expansion.
        upper = .false.
        if (.not. same_string(nm, abbrev%name)) upper = string_match("[[:upper:]]", nm, st) > 0
        if (upper) then
            lower = string_match("[[:lower:]]", nm, st) > 0
            if (.not. lower) then
                ! Abbrev was all caps.  If expansion is multiple words,
                ! normally capitalize each word.
                a = 0
                b = 0
                if (.not. abbrev_all_caps) then
                    p0 = point()
                    call forward_word(-1_int64)
                    a = point()
                    call goto_char(wstart)
                    call forward_word(1_int64)
                    b = point()
                    call goto_char(p0)
                end if
                if (.not. abbrev_all_caps .and. a > b) then
                    call upcase_initials_region(wstart, point(), status)
                else
                    call upcase_region(wstart, point(), status)
                end if
            else
                ! Abbrev included some caps.  Cap first initial of expansion.
                end = point()
                ! Find the initial.
                call goto_char(wstart)
                moved = skip_syntax_forward("^w", end - 1)
                ! Change just that.
                call upcase_initials_region(point(), point() + 1, status)
                call goto_char(end)
            end if
        end if
        case_fold_search = saved_fold
        if (.not. status%ok()) return
    end if
    ! Now point is at the end of the expansion and the beginning is
    ! in last-abbrev-location.
    hook => abbrev%table%abbrevs(k)%hook
    if (associated(hook)) then
        ! If the abbrev has a hook function, run it.
        call hook(value, status)
        if (.not. status%ok()) return
        ! In addition, if the hook function has the `no_self_insert'
        ! property, let the value it returned specify whether we consider
        ! that an expansion took place.  If it returns false, no expansion
        ! has been done.
        k = find_abbrev(abbrev%table, abbrev%name)
        if (k > 0) then
            if (.not. value .and. abbrev%table%abbrevs(k)%no_self_insert) then
                result%table => null()
                if (allocated(result%name)) deallocate (result%name)
            end if
        end if
    end if
    end procedure abbrev_insert

    module procedure expand_abbrev
    type(abbrev_ref_type) :: sym
    if (associated(abbrev_expand_function)) then
        call abbrev_expand_function(sym, status)
    else
        call abbrev_default_expand(sym, status)
    end if
    if (present(expanded)) expanded = sym
    end procedure expand_abbrev

    module procedure abbrev_default_expand
    type(abbrev_ref_type) :: sym
    character(len=:), allocatable :: name
    integer(int64) :: wordstart, wordend, sp, ep
    type(handle_type) :: startpos, endmark
    type(status_type) :: st
    call before_point(sym, name, wordstart, wordend, status)
    if (.not. status%ok()) return
    if (.not. associated(sym%table)) return
    call copy_marker_at(point(), startpos, .true., st)
    call copy_marker_at(wordend, endmark, .true., st)
    ! Add an undo boundary, in case we are doing this for a
    ! self-inserting command which has avoided making one so far.
    if (.not. selected_frame%minibuffer_selected) call undo_boundary()
    ! Now sym is the abbrev symbol.
    last_abbrev_text = name
    last_abbrev = sym
    last_abbrev_location = wordstart
    ! If this abbrev has an expansion, delete the abbrev
    ! and insert the expansion.
    call abbrev_insert(sym, abbrev, status, name, wordstart, wordend)
    ! Yuck!!  If expand-abbrev is called with point slightly
    ! further than the end of the abbrev, move point back to
    ! where it started.
    call marker_position(startpos, sp, st)
    call marker_position(endmark, ep, st)
    if (status%ok() .and. sp > ep .and. point() == ep) call goto_char(sp)
    call free_marker(startpos, st)
    call free_marker(endmark, st)
    end procedure abbrev_default_expand

    module procedure unexpand_abbrev
    integer(int64) :: pos, vlen, tlen
    type(handle_type) :: saved
    type(status_type) :: st
    integer :: k
    character(len=:), allocatable :: val
    status = status_type()
    ! (save-excursion ...)
    call point_marker(saved)
    if (point_min() <= last_abbrev_location .and. last_abbrev_location <= point_max()) then
        call goto_char(last_abbrev_location)
        if (allocated(last_abbrev_text)) then
            ! This isn't correct if last-abbrev's hook was used
            ! to do the expansion.
            k = 0
            if (associated(last_abbrev%table)) k = find_abbrev(last_abbrev%table, last_abbrev%name)
            if (k > 0) then
                if (allocated(last_abbrev%table%abbrevs(k)%expansion)) val = last_abbrev%table%abbrevs(k)%expansion
            end if
            if (.not. allocated(val)) then
                call set_error(status, FM_GENERIC_ERROR, "Value of abbrev_symbol must be a string")
            else
                vlen = char_count(val)
                tlen = char_count(last_abbrev_text)
                if (point() + vlen > point_max()) then
                    ! replace-region-contents validates the region.
                    call delete_region(point(), point() + vlen, status)
                else
                    ! Don't inherit properties here; just copy from old contents.
                    pos = point()
                    call replace_range(pos, pos + vlen, last_abbrev_text, .true., status)
                    if (status%ok()) then
                        call goto_char(pos + tlen)
                        deallocate (last_abbrev_text)
                    end if
                end if
            end if
        end if
    end if
    call marker_position(saved, pos, st)
    call goto_char(pos)
    call free_marker(saved, st)
    end procedure unexpand_abbrev

    !> The number of characters of the UTF-8 text S.
    pure integer(int64) function char_count(s) result(n)
        character(len=*), intent(in) :: s
        integer :: k, b
        n = 0
        do k = 1, len(s)
            b = ichar(s(k:k))
            if (b < 128 .or. b >= 192) n = n + 1
        end do
    end function char_count

    ! ------------------------------------------------------------------
    ! Describing tables
    ! ------------------------------------------------------------------

    !> Describe abbrev K of TABLE.
    !> Print on the current buffer the abbrev, count of use, expansion.
    !> Port of abbrev.el:abbrev--describe (the hook is not shown).
    recursive subroutine describe_abbrev(table, k, status)
        type(abbrev_table_type), intent(in) :: table
        integer, intent(in) :: k
        type(status_type), intent(out) :: status
        integer(int64) :: col
        character(len=24) :: buf
        status = status_type()
        associate (a => table%abbrevs(k))
            if (.not. allocated(a%expansion)) return
            call insert(fortran_string(a%name), status)
            if (.not. status%ok()) return
            if (.not. a%system) then
                col = indent_to(15_int64, status, 1_int64)
            else
                call insert(" (sys)", status)
                if (status%ok()) col = indent_to(20_int64, status, 1_int64)
            end if
            if (.not. status%ok()) return
            write (buf, "(i0)") a%count
            call insert(trim(buf), status)
            if (status%ok()) col = indent_to(20_int64, status, 1_int64)
            if (status%ok()) call insert(fortran_string(a%expansion)//LF, status)
        end associate
    end subroutine describe_abbrev

    !> STRING as Fortran source: a literal in pieces of at most 60
    !> bytes joined by //, control characters as achar(N), the pieces
    !> continued over lines indented by IND.
    function fortran_literal(string, ind) result(s)
        character(len=*), intent(in) :: string, ind
        character(len=:), allocatable :: s, piece
        integer :: k, b
        character(len=8) :: buf
        s = ""
        piece = ""
        k = 1
        do while (k <= len(string))
            b = ichar(string(k:k))
            if (b < 32 .or. b == 127) then
                if (len(piece) > 0) s = joined(s, fortran_string(piece))
                piece = ""
                write (buf, "(i0)") b
                s = joined(s, "achar("//trim(buf)//")")
                k = k + 1
                cycle
            end if
            ! Break between characters, not inside one.
            if (len(piece) >= 60 .and. (b < 128 .or. b >= 192)) then
                s = joined(s, fortran_string(piece))
                piece = ""
            end if
            piece = piece//string(k:k)
            k = k + 1
        end do
        if (len(piece) > 0 .or. len(s) == 0) s = joined(s, fortran_string(piece))
    contains

        function joined(left, right) result(r)
            character(len=*), intent(in) :: left, right
            character(len=:), allocatable :: r
            if (len(left) == 0) then
                r = right
            else
                r = left//"// &"//LF//ind//right
            end if
        end function joined
    end function fortran_literal

    !> The Fortran statement defining abbrev K of TABLE in `configure'
    !> (upstream's abbrev--write).
    function define_statement(table, k) result(s)
        type(abbrev_table_type), intent(in) :: table
        integer, intent(in) :: k
        character(len=:), allocatable :: s
        character(len=24) :: buf
        character(len=*), parameter :: IND = "            "
        associate (a => table%abbrevs(k))
            write (buf, "(i0)") a%count
            s = "        call "//trim(table%name)//"%define("//fortran_literal(a%name, IND)//", &"//LF//IND// &
                fortran_literal(a%expansion, IND)//", count="//trim(buf)//"_int64"
            if (a%case_fixed) s = s//", case_fixed=.true."
            s = s//")"//LF
        end associate
    end function define_statement

    module procedure insert_abbrev_table_description
    integer, allocatable :: order(:)
    logical :: human
    integer :: k
    human = .false.
    if (present(readable)) human = readable
    call sorted_abbrevs(table, human, order)
    if (human) then
        call insert("("//trim(table%name)//")"//LF//LF, status)
        do k = 1, size(order)
            if (.not. status%ok()) return
            call describe_abbrev(table, order(k), status)
        end do
        if (status%ok()) call insert(LF//LF, status)
    else
        status = status_type()
        do k = 1, size(order)
            ! An abbrev with a hook or enable function is defined in
            ! code: its procedure has no name to write.
            if (associated(table%abbrevs(order(k))%hook)) cycle
            if (associated(table%abbrevs(order(k))%enable_function)) cycle
            call insert(define_statement(table, order(k)), status)
            if (.not. status%ok()) return
        end do
    end if
    end procedure insert_abbrev_table_description

    module procedure insert_abbrevs
    integer(int64) :: opoint, pos
    integer :: k
    type(handle_type) :: saved
    type(status_type) :: st
    call ensure_tables()
    status = status_type()
    opoint = point()
    call copy_marker_at(opoint, saved, .false., st)
    do k = 1, size(abbrev_table_name_list)
        call insert_abbrev_table_description(abbrev_table_name_list(k)%p, status, readable=.true.)
        if (.not. status%ok()) exit
    end do
    pos = point()
    call marker_position(saved, opoint, st)
    call free_marker(saved, st)
    call goto_char(opoint)
    if (status%ok()) call push_mark(pos)
    end procedure insert_abbrevs

    module procedure prepare_abbrev_list_buffer
    type(abbrev_table_type), pointer :: local_table
    type(handle_type) :: old
    type(status_type) :: st
    logical :: only_local
    integer :: k
    call ensure_tables()
    only_local = .false.
    if (present(local)) only_local = local
    local_table => local_abbrev_table()
    old = current_buffer()
    call get_buffer_create("*Abbrevs*", buffer, status)
    if (.not. status%ok()) return
    call set_buffer(buffer, status)
    if (.not. status%ok()) return
    call erase_buffer(status)
    if (status%ok()) then
        if (only_local) then
            if (len(abbrev_table_name(local_table)) > 0) then
                call insert_abbrev_table_description(local_table, status, readable=.true.)
            end if
        else
            do k = 1, size(abbrev_table_name_list)
                if (.not. abbrev_table_name_list(k)%p%empty()) then
                    call insert_abbrev_table_description(abbrev_table_name_list(k)%p, status, readable=.true.)
                end if
                if (.not. status%ok()) exit
            end do
            do k = 1, size(abbrev_table_name_list)
                if (.not. status%ok()) exit
                if (abbrev_table_name_list(k)%p%empty()) then
                    call insert_abbrev_table_description(abbrev_table_name_list(k)%p, status, readable=.true.)
                end if
            end do
            ! Note: `list_abbrevs' can display only local abbrevs, in
            ! which case editing could lose abbrevs of other tables.
            ! Thus enter `edit_abbrevs_mode' only if LOCAL is false.
            if (status%ok()) call edit_abbrevs_mode(status)
        end if
    end if
    call goto_char(point_min())
    call restore_buffer_modified_p(.false.)
    call set_buffer(old, st)
    end procedure prepare_abbrev_list_buffer

    module procedure list_abbrevs
    type(handle_type) :: buffer, window
    call prepare_abbrev_list_buffer(buffer, status, local)
    if (status%ok()) call display_buffer(buffer, window, status)
    end procedure list_abbrevs

    module procedure edit_abbrevs
    character(len=:), allocatable :: table_name
    type(handle_type) :: buffer
    integer(int64) :: found
    type(status_type) :: st
    type(abbrev_table_type), pointer :: local_table
    local_table => local_abbrev_table()
    table_name = abbrev_table_name(local_table)
    call prepare_abbrev_list_buffer(buffer, status)
    if (.not. status%ok()) return
    call switch_to_buffer("*Abbrevs*", status)
    if (.not. status%ok()) return
    if (len(table_name) > 0) then
        call search_forward("("//table_name//")"//LF//LF, found, st, noerror=SEARCH_NOERROR)
        if (found > 0) call goto_char(match_end(0, st))
    end if
    end procedure edit_abbrevs

    ! ------------------------------------------------------------------
    ! Saving abbrevs: the init file
    ! ------------------------------------------------------------------

    !> `abbrev_file_name', or the init file when it is not set.
    function abbrev_file() result(file)
        character(len=:), allocatable :: file
        if (allocated(abbrev_file_name)) then
            file = abbrev_file_name
        else if (allocated(user_init_file)) then
            file = user_init_file
            if (len(file) == 0) file = init_file_name()
        else
            file = init_file_name()
        end if
    end function abbrev_file

    module procedure write_abbrev_file
    character(len=:), allocatable :: target_file, block, msg, dir
    type(abbrev_table_ref_type), allocatable :: sorted(:)
    type(abbrev_table_ref_type) :: x
    character(len=:), allocatable :: names(:)
    type(handle_type) :: old, temp
    type(status_type) :: st
    integer(int64) :: inserted, found, beg
    integer :: k, j, m
    logical :: saved_fold
    target_file = ""
    if (present(file)) target_file = file
    if (len(target_file) == 0) target_file = abbrev_file()
    call ensure_tables()
    ! We sort the table in order to ease the automatic
    ! merging of different versions of the user's abbrevs
    ! file.  This is useful, for example, when the
    ! user keeps their home directory in a revision
    ! control system, and therefore keeps multiple
    ! slightly-differing loosely synchronized copies.
    sorted = abbrev_table_name_list
    m = size(sorted)
    do k = 2, m
        x = sorted(k)
        j = k - 1
        do while (j >= 1)
            if (.not. string_lessp(trim(x%p%name), trim(sorted(j)%p%name))) exit
            sorted(j + 1) = sorted(j)
            j = j - 1
        end do
        sorted(j + 1) = x
    end do
    old = current_buffer()
    call get_buffer_create(generate_new_buffer_name(" *temp*"), temp, status, inhibit_buffer_hooks=.true.)
    if (.not. status%ok()) return
    call set_buffer(temp, status)
    if (.not. status%ok()) return
    ! The abbrevs, as `define' calls.
    call insert("        "//BLOCK_START//LF, status)
    do k = 1, m
        if (.not. status%ok()) exit
        call abbrev_table_symbols(sorted(k)%p, names)
        if (size(names) > 0) call insert_abbrev_table_description(sorted(k)%p, status)
    end do
    if (status%ok()) call insert("        "//BLOCK_END//LF, status)
    if (status%ok()) call buffer_substring(point_min(), point_max(), block, status)
    if (status%ok()) call erase_buffer(status)
    ! The init file, with the abbrevs replacing those written before at the
    ! end of `configure'.
    if (status%ok() .and. file_exists_p(target_file)) call insert_file_contents(target_file, .false., inserted, status)
    if (status%ok() .and. point_max() == point_min()) then
        ! A new init file.
        call insert("module user_config"//LF//"    use fortmacs"//LF//"    implicit none(type, external)"//LF// &
                    "    private"//LF//LF//"    public :: configure"//LF//LF//"contains"//LF//LF// &
                    "    subroutine configure(status)"//LF//"        type(status_type), intent(out) :: status"// &
                    LF//"    end subroutine configure"//LF//"end module user_config"//LF, status)
    end if
    saved_fold = case_fold_search%value
    case_fold_search = .true.
    if (status%ok()) then
        call goto_char(point_min())
        call search_forward(BLOCK_START, found, st, noerror=SEARCH_NOERROR)
        if (found > 0) then
            beg = line_beginning_position()
            call search_forward(BLOCK_END, found, st, noerror=SEARCH_NOERROR)
            if (found > 0) then
                call search_forward(LF, found, st, noerror=SEARCH_MOVE)
                call delete_region(beg, point(), status)
                call goto_char(beg)
            else
                found = 0
                call goto_char(point_min())
            end if
        end if
        if (found == 0) then
            call search_forward("end subroutine configure", found, st, noerror=SEARCH_NOERROR)
            if (found == 0) then
                call set_error(status, FM_GENERIC_ERROR, format_message("No `end subroutine configure' in ")//target_file)
            else
                call goto_char(line_beginning_position())
            end if
        end if
    end if
    case_fold_search = saved_fold
    if (status%ok()) call insert(block, status)
    if (status%ok()) then
        dir = file_name_directory(target_file)
        if (len(dir) > 0) then
            if (.not. file_directory_p(dir)) call make_directory(dir, status, parents=.true.)
        end if
    end if
    if (status%ok()) call write_region(target_file, .false., status, message=msg)
    call set_buffer(old, st)
    call kill_buffer(temp, st)
    if (.not. status%ok()) return
    if (present(verbose)) then
        if (verbose .and. allocated(msg)) call message(msg)
    end if
    end procedure write_abbrev_file

    module procedure abbrev_possibly_save
    logical :: save
    status = status_type()
    ! Query mode.
    if (query) then
        result = save_abbrevs /= SAVE_ABBREVS_NEVER .and. abbrevs_changed
        return
    end if
    result = .false.
    if (save_abbrevs == SAVE_ABBREVS_NEVER .or. .not. abbrevs_changed) return
    if (arg .or. save_abbrevs == SAVE_ABBREVS_SILENTLY) then
        save = .true.
    else
        call y_or_n_p("Save abbrevs in "//abbrev_file()//"? ", save, status)
        if (.not. status%ok()) return
    end if
    if (save) then
        call write_abbrev_file(status)
        if (.not. status%ok()) return
        result = .false.
    else
        ! Inhibit message in `save-some-buffers'.
        result = .true.
    end if
    ! Don't ask again whether saved or user said no.
    abbrevs_changed = .false.
    end procedure abbrev_possibly_save

    ! ------------------------------------------------------------------
    ! Defining abbrevs from the buffer
    ! ------------------------------------------------------------------

    !> The table the mode-specific commands define abbrevs in:
    !> `global_abbrev_table' if `only_global_abbrevs', else the local table.
    function mode_table() result(table)
        type(abbrev_table_type), pointer :: table
        if (only_global_abbrevs) then
            table => global_abbrev_table
        else
            table => local_abbrev_table()
        end if
    end function mode_table

    module procedure add_mode_abbrev
    type(abbrev_table_type), pointer :: table
    table => mode_table()
    call add_abbrev(table, "Mode", arg, status)
    end procedure add_mode_abbrev

    module procedure add_global_abbrev
    call add_abbrev(global_abbrev_table, "Global", arg, status)
    end procedure add_global_abbrev

    module procedure add_abbrev
    character(len=:), allocatable :: exp, name, old
    integer(int64) :: b, e, opoint, n
    logical :: have_exp, redefine
    status = status_type()
    have_exp = .false.
    n = prefix_numeric_value(arg)
    if ((nilp(arg) .and. use_region_p()) .or. n == 0) then
        call region_beginning(b, status)
        if (status%ok()) call region_end(e, status)
        if (status%ok()) call buffer_substring(b, e, exp, status)
        if (.not. status%ok()) return
        have_exp = .true.
    else if (n > 0) then
        opoint = point()
        call forward_word(-n)
        e = point()
        call goto_char(opoint)
        call buffer_substring(e, opoint, exp, status)
        if (.not. status%ok()) return
        have_exp = .true.
    end if
    if (have_exp) then
        call read_string(type//" abbrev that expands into """//exp//""": ", name, status)
    else
        call read_string("Undefine "//type//" abbrev: ", name, status)
    end if
    if (.not. status%ok()) return
    redefine = .true.
    if (have_exp) then
        call abbrev_expansion(name, old, table)
        if (allocated(old)) then
            call y_or_n_p(name//" expands into """//old//"""; redefine? ", redefine, status)
            if (.not. status%ok()) return
        end if
    end if
    if (.not. redefine) return
    if (have_exp) then
        call table%define(downcase_string(name), exp)
    else
        call table%define(downcase_string(name))
    end if
    end procedure add_abbrev

    module procedure inverse_add_mode_abbrev
    type(abbrev_table_type), pointer :: table
    table => mode_table()
    call inverse_add_abbrev(table, "Mode", n, status)
    end procedure inverse_add_mode_abbrev

    module procedure inverse_add_global_abbrev
    call inverse_add_abbrev(global_abbrev_table, "Global", n, status)
    end procedure inverse_add_global_abbrev

    module procedure inverse_add_abbrev
    character(len=:), allocatable :: name, exp, old
    integer(int64) :: opoint, start, end, moved, pos
    type(handle_type) :: saved
    type(status_type) :: st
    logical :: define
    opoint = point()
    call forward_word(1 - arg)
    moved = skip_syntax_backward("^w")
    end = point()
    call forward_word(-1_int64)
    start = point()
    call buffer_substring(start, end, name, status)
    call goto_char(opoint)
    if (.not. status%ok()) return
    call read_string("Expansion for "//type//" abbrev """//name//""": ", exp, status)
    if (.not. status%ok()) return
    define = .true.
    call abbrev_expansion(name, old, table)
    if (allocated(old)) then
        call y_or_n_p(name//" expands into """//old//"""; redefine? ", define, status)
        if (.not. status%ok()) return
    end if
    if (.not. define) return
    call table%define(downcase_string(name), exp)
    ! (save-excursion (goto-char end) (expand-abbrev))
    call point_marker(saved)
    call goto_char(end)
    call expand_abbrev(status)
    call marker_position(saved, pos, st)
    call goto_char(pos)
    call free_marker(saved, st)
    end procedure inverse_add_abbrev

    module procedure abbrev_prefix_mark
    logical :: no_expand
    no_expand = .false.
    if (present(arg)) no_expand = arg
    status = status_type()
    if (.not. no_expand) call expand_abbrev(status)
    if (.not. status%ok()) return
    call clear_start_location()
    call point_marker(abbrev_start_location)
    abbrev_start_location_buffer = current_buffer()
    call insert("-", status)
    end procedure abbrev_prefix_mark

    module procedure expand_region_abbrevs
    integer(int64) :: lim, pnt, opoint, pos, wbeg
    character(len=:), allocatable :: string, exp
    type(handle_type) :: saved
    type(status_type) :: st
    logical :: query, yes
    query = .true.
    if (present(noquery)) query = .not. noquery
    status = status_type()
    opoint = point()
    call copy_marker_at(opoint, saved, .false., st)
    call goto_char(start)
    lim = point_max() - end
    do while (.not. eobp())
        call forward_word(1_int64)
        pnt = point()
        if (pnt > point_max() - lim) exit
        pos = point()
        call forward_word(-1_int64)
        wbeg = point()
        call goto_char(pos)
        call buffer_substring(wbeg, pnt, string, status)
        if (.not. status%ok()) exit
        call abbrev_expansion(string, exp)
        if (allocated(exp)) then
            deallocate (exp)
            yes = .true.
            if (query) call y_or_n_p(format_message("Expand `")//string//format_message("'? "), yes, status)
            if (.not. status%ok()) exit
            if (yes) call expand_abbrev(status)
            if (.not. status%ok()) exit
        end if
    end do
    call marker_position(saved, pos, st)
    call goto_char(pos)
    call free_marker(saved, st)
    end procedure expand_region_abbrevs

    !> Check if the characters in ABBREV have word syntax in either the
    !> current (if GLOBAL is false) or standard syntax table.  Port of
    !> abbrev.el:abbrev--check-chars.
    subroutine check_chars(abbrev, global, status)
        character(len=*), intent(in) :: abbrev
        logical, intent(in) :: global
        type(status_type), intent(out) :: status
        type(handle_type) :: saved_table
        character(len=:), allocatable :: badchars, c
        integer(int64) :: pos, at
        type(status_type) :: st
        status = status_type()
        saved_table = syntax_table()
        if (global) call set_syntax_table(standard_syntax_table(), st)
        if (string_match("\W", abbrev, st) > 0) then
            badchars = ""
            pos = 1
            do
                at = string_match("\W", abbrev, st, start=pos)
                if (at == 0) exit
                c = substring_at(abbrev, at)
                if (index(badchars, c) == 0) badchars = badchars//c
                pos = pos + 1
            end do
            if (global) then
                call set_error(status, FM_GENERIC_ERROR, "Some abbrev characters ("//badchars// &
                               ") are not word constituents in the standard syntax")
            else
                call set_error(status, FM_GENERIC_ERROR, "Some abbrev characters ("//badchars// &
                               ") are not word constituents in this mode")
            end if
        end if
        call set_syntax_table(saved_table, st)
    end subroutine check_chars

    !> The character of the UTF-8 string S at character index I (1-based,
    !> as string_match's positions).
    function substring_at(s, i) result(c)
        character(len=*), intent(in) :: s
        integer(int64), intent(in) :: i
        character(len=:), allocatable :: c
        integer :: k, n, b, start
        n = 0
        start = 0
        c = ""
        do k = 1, len(s)
            b = ichar(s(k:k))
            if (b < 128 .or. b >= 192) then
                n = n + 1
                if (n == i) start = k
                if (n == i + 1) then
                    c = s(start:k - 1)
                    return
                end if
            end if
        end do
        if (start > 0) c = s(start:)
    end function substring_at

    module procedure define_global_abbrev
    call check_chars(abbrev, .true., status)
    if (.not. status%ok()) return
    call global_abbrev_table%define(downcase_string(abbrev), expansion)
    end procedure define_global_abbrev

    module procedure define_mode_abbrev
    type(abbrev_table_type), pointer :: table
    table => local_abbrev_table()
    if (.not. associated(table)) then
        call set_error(status, FM_GENERIC_ERROR, "Major mode has no abbrev table")
        return
    end if
    call check_chars(abbrev, .false., status)
    if (.not. status%ok()) return
    call table%define(downcase_string(abbrev), expansion)
    end procedure define_mode_abbrev

    ! ------------------------------------------------------------------
    ! Edit-Abbrevs mode and the commands
    ! ------------------------------------------------------------------

    module procedure edit_abbrevs_mode
    logical :: saved
    call major_modes%edit_abbrevs_mode%begin(saved, status)
    if (.not. status%ok()) return
    call major_modes%edit_abbrevs_mode%finish(saved, status)
    end procedure edit_abbrevs_mode

    !> Toggle Abbrev mode in the current buffer.
    !>
    !> In Abbrev mode, inserting an abbreviation causes it to expand and
    !> be replaced by its expansion.  Port of abbrev.el:abbrev-mode.
    recursive subroutine cmd_abbrev_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%abbrev_mode%command(status)
    end subroutine cmd_abbrev_mode

    ! The interactive side of the commands.

    subroutine cmd_kill_all_abbrevs(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call kill_all_abbrevs()
    end subroutine cmd_kill_all_abbrevs

    recursive subroutine cmd_insert_abbrevs(status)
        type(status_type), intent(out) :: status
        call insert_abbrevs(status)
    end subroutine cmd_insert_abbrevs

    recursive subroutine cmd_list_abbrevs(status)
        type(status_type), intent(out) :: status
        call list_abbrevs(status, local=.not. nilp(current_prefix_arg))
    end subroutine cmd_list_abbrevs

    recursive subroutine cmd_edit_abbrevs(status)
        type(status_type), intent(out) :: status
        call edit_abbrevs(status)
    end subroutine cmd_edit_abbrevs

    recursive subroutine cmd_write_abbrev_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, expanded
        call expand_file_name(abbrev_file(), expanded, status)
        if (.not. status%ok()) return
        call read_file_name("Write abbrev file: ", file, status, dir=file_name_directory(expanded), &
                            default_filename=abbrev_file())
        if (status%ok()) call write_abbrev_file(status, file)
    end subroutine cmd_write_abbrev_file

    recursive subroutine cmd_add_mode_abbrev(status)
        type(status_type), intent(out) :: status
        call add_mode_abbrev(current_prefix_arg, status)
    end subroutine cmd_add_mode_abbrev

    recursive subroutine cmd_add_global_abbrev(status)
        type(status_type), intent(out) :: status
        call add_global_abbrev(current_prefix_arg, status)
    end subroutine cmd_add_global_abbrev

    recursive subroutine cmd_inverse_add_mode_abbrev(status)
        type(status_type), intent(out) :: status
        call inverse_add_mode_abbrev(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_inverse_add_mode_abbrev

    recursive subroutine cmd_inverse_add_global_abbrev(status)
        type(status_type), intent(out) :: status
        call inverse_add_global_abbrev(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_inverse_add_global_abbrev

    recursive subroutine cmd_abbrev_prefix_mark(status)
        type(status_type), intent(out) :: status
        call abbrev_prefix_mark(status, arg=.not. nilp(current_prefix_arg))
    end subroutine cmd_abbrev_prefix_mark

    recursive subroutine cmd_expand_region_abbrevs(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e
        call region_beginning(b, status)
        if (status%ok()) call region_end(e, status)
        if (status%ok()) call expand_region_abbrevs(b, e, status, noquery=.not. nilp(current_prefix_arg))
    end subroutine cmd_expand_region_abbrevs

    recursive subroutine cmd_define_global_abbrev(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: abbrev, expansion
        call read_string("Define global abbrev: ", abbrev, status)
        if (status%ok()) call read_string("Expansion for "//abbrev//": ", expansion, status)
        if (status%ok()) call define_global_abbrev(abbrev, expansion, status)
    end subroutine cmd_define_global_abbrev

    recursive subroutine cmd_define_mode_abbrev(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: abbrev, expansion
        call read_string("Define mode abbrev: ", abbrev, status)
        if (status%ok()) call read_string("Expansion for "//abbrev//": ", expansion, status)
        if (status%ok()) call define_mode_abbrev(abbrev, expansion, status)
    end subroutine cmd_define_mode_abbrev

    recursive subroutine cmd_expand_abbrev(status)
        type(status_type), intent(out) :: status
        call expand_abbrev(status)
    end subroutine cmd_expand_abbrev

    recursive subroutine cmd_unexpand_abbrev(status)
        type(status_type), intent(out) :: status
        call unexpand_abbrev(status)
    end subroutine cmd_unexpand_abbrev

    module procedure syms_of_abbrev
    call ensure_tables()
    call minor_modes%abbrev_mode%define("abbrev_mode", " Abbrev")
    abbrev_mode => minor_modes%abbrev_mode%state
    commands%abbrev_mode%run => cmd_abbrev_mode
    call major_modes%edit_abbrevs_mode%define("edit_abbrevs_mode", "Edit-Abbrevs", edit_abbrevs_mode, &
                                              edit_abbrevs_mode_hook, abbrev_table=edit_abbrevs_mode_abbrev_table)
    edit_abbrevs_mode_map = major_modes%edit_abbrevs_mode%map
    commands%kill_all_abbrevs%run => cmd_kill_all_abbrevs
    commands%insert_abbrevs%run => cmd_insert_abbrevs
    commands%list_abbrevs%run => cmd_list_abbrevs
    commands%edit_abbrevs%run => cmd_edit_abbrevs
    commands%write_abbrev_file%run => cmd_write_abbrev_file
    commands%add_mode_abbrev%run => cmd_add_mode_abbrev
    commands%add_global_abbrev%run => cmd_add_global_abbrev
    commands%inverse_add_mode_abbrev%run => cmd_inverse_add_mode_abbrev
    commands%inverse_add_global_abbrev%run => cmd_inverse_add_global_abbrev
    commands%abbrev_prefix_mark%run => cmd_abbrev_prefix_mark
    commands%expand_region_abbrevs%run => cmd_expand_region_abbrevs
    commands%define_global_abbrev%run => cmd_define_global_abbrev
    commands%define_mode_abbrev%run => cmd_define_mode_abbrev
    commands%expand_abbrev%run => cmd_expand_abbrev
    commands%unexpand_abbrev%run => cmd_unexpand_abbrev
    call add_local_variables_killer(kill_local_abbrev_table)
    call add_save_some_buffers_function(abbrev_possibly_save)
    end procedure syms_of_abbrev
end submodule abbrev_cmds_fm
