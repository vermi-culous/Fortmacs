! abbrev_fm.f90 -- abbrev tables and Abbrev mode.
! Ported from GNU Emacs 31.1 lisp/abbrev.el, with the abbrev parts of
! src/buffer.c (abbrev-mode, local-abbrev-table) and src/cmds.c.
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

!> Abbrevs: abbrev tables are `abbrev_table_type' objects named after
!> their Emacs variables (`global_abbrev_table',
!> `fundamental_mode_abbrev_table', and one per major mode, such as
!> `text_mode_abbrev_table', which the mode's `define' makes known), and
!> abbrevs are defined with the table's `define':
!>
!>     call global_abbrev_table%define("teh", "the")
!>     abbrev_mode = .true.
!>
!> An abbrev table's properties are components (`parents', `case_fixed',
!> `regexp', `enable_function'); upstream's obarray of symbols with
!> property lists becomes an array of `abbrev_type' entries.  An abbrev
!> as `expand_abbrev' and `last_abbrev' give it (upstream's abbrev
!> symbol) is an `abbrev_ref_type': its table and name.
!>
!> Saving abbrevs is a setting-saving command: `write_abbrev_file'
!> writes the abbrevs as `define' calls into the init file (Fortran
!> compiled in, see `abbrev_file_name'), so reading an abbrev file
!> (`read_abbrev_file', `quietly_read_abbrev_file') is not applicable.
!>
!> The procedures are in the submodule abbrev_cmds_fm, which can use the
!> modules (modes, windows, the minibuffer) that use this one: derived_fm
!> gives each major mode its abbrev table.
!>
!> Deferred: abbrevs whose EXPANSION is not a string (special abbrevs
!> that only run their hook), a list of tables as `local_abbrev_table',
!> `abbrev_minor_mode_table_alist', the obsolete wrapper hook
!> `abbrev_expand_functions', other properties of abbrevs and tables
!> than the special ones (`abbrev_table_get', `abbrev_table_put',
!> `abbrev_get', `abbrev_put'), `abbrev_suggest' (with
!> `abbrev_suggest_hint_threshold' and `abbrev_suggest_show_report'),
!> `abbrev_table_menu', `define_abbrevs' and `edit_abbrevs_redefine'
!> with the keys of `edit_abbrevs_mode_map' (`abbrev_edit_save_buffer',
!> `abbrev_edit_save_to_file'), Edit-Abbrevs mode's font-lock keywords
!> and the `abbrev_table_name' face, showing an abbrev's hook in the
!> *Abbrevs* buffer, and writing abbrevs that have a hook or an enable
!> function (a Fortran procedure has no name to write; the init file
!> defines them in code).  (Upstream 31.1 no longer has
!> `pre_abbrev_expand_hook'.)
module abbrev_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type
    use data_fm, only: logical_setting
    use eval_fm, only: hook_type
    implicit none(type, external)
    private

    public :: abbrev_hook_proc, abbrev_enable_proc, abbrev_expand_proc
    public :: kill_all_abbrevs, copy_abbrev_table, insert_abbrevs, list_abbrevs, abbrev_table_name, &
              prepare_abbrev_list_buffer, edit_abbrevs, write_abbrev_file, add_mode_abbrev, add_global_abbrev, &
              add_abbrev, inverse_add_mode_abbrev, inverse_add_global_abbrev, inverse_add_abbrev, abbrev_prefix_mark, &
              expand_region_abbrevs, define_global_abbrev, define_mode_abbrev, abbrev_symbol, abbrev_expansion, &
              abbrev_insert, expand_abbrev, abbrev_default_expand, unexpand_abbrev, insert_abbrev_table_description, &
              abbrev_table_symbols, abbrev_possibly_save, local_abbrev_table, set_local_abbrev_table, &
              edit_abbrevs_mode, abbrev_mode_p, abbrev_no_self_insert, syms_of_abbrev

    !> The values of `save_abbrevs': never save, ask (t upstream) or save
    !> without asking (`silently').
    integer, parameter, public :: SAVE_ABBREVS_NEVER = 0, SAVE_ABBREVS_ASK = 1, SAVE_ABBREVS_SILENTLY = 2

    abstract interface
        !> An abbrev's hook, called after its expansion is inserted;
        !> VALUE is its value (true for non-nil), which matters for an
        !> abbrev whose `no_self_insert' is true.
        subroutine abbrev_hook_proc(value, status)
            import :: status_type
            implicit none(type, external)
            logical, intent(out) :: value
            type(status_type), intent(out) :: status
        end subroutine abbrev_hook_proc

        !> An enable function: whether the abbrevs of a table (or one
        !> abbrev) should be used for this call of `expand_abbrev'.
        logical function abbrev_enable_proc()
            implicit none(type, external)
        end function abbrev_enable_proc
    end interface

    !> One abbrev of a table (upstream's symbol in the table's obarray,
    !> with its value, function and properties).
    type, public :: abbrev_type
        !> The abbrev, as defined (usually lower case).
        character(len=:), allocatable :: name
        !> What it expands into; unallocated when it is not defined (nil).
        character(len=:), allocatable :: expansion
        !> Called after the expansion is inserted (the symbol's function);
        !> null for none.
        procedure(abbrev_hook_proc), pointer, nopass :: hook => null()
        !> The hook's `no_self_insert' property: the value of the hook
        !> says whether the character that triggered the expansion is
        !> inserted.
        logical :: no_self_insert = .false.
        !> `:count': how many times it was expanded.
        integer(int64) :: count = 0
        !> `:system': a system abbrev, not saved in the abbrev file.
        logical :: system = .false.
        !> `:case_fixed': look it up without case folding, and do not
        !> capitalize or upcase the expansion.
        logical :: case_fixed = .false.
        !> `:enable_function'; null for none.
        procedure(abbrev_enable_proc), pointer, nopass :: enable_function => null()
        !> Whether it was ever defined (upstream's symbol is bound).
        logical :: bound = .false.
    end type abbrev_type

    !> A pointer to an abbrev table.
    type, public :: abbrev_table_ref_type
        type(abbrev_table_type), pointer :: p => null()
    end type abbrev_table_ref_type

    !> An abbrev table.  Port of the abbrev tables of abbrev.el (an
    !> obarray with the table's properties on its symbol "").
    type, public :: abbrev_table_type
        !> The name of the variable holding the table
        !> (`abbrev_table_name'), as in `abbrev_table_name_list'; empty for
        !> a table that is not listed.
        character(len=64) :: name = ""
        !> The abbrevs, in the order they were first defined.
        type(abbrev_type), allocatable :: abbrevs(:)
        integer :: n = 0
        !> `:parents': the tables from which this table inherits abbrevs.
        type(abbrev_table_ref_type), allocatable :: parents(:)
        !> `:case_fixed': look abbreviations up without case folding, and
        !> do not capitalize or upcase the expansion.
        logical :: case_fixed = .false.
        !> `:regexp': how to extract the name of the abbrev before point
        !> (its submatch 1); unallocated to use `backward_word' and
        !> `forward_word', so that a name is one word.
        character(len=:), allocatable :: regexp
        !> `:enable_function': whether the table's abbrevs should be used
        !> for this call of `expand_abbrev'; null for always.
        procedure(abbrev_enable_proc), pointer, nopass :: enable_function => null()
        !> `:abbrev_table_modiff': incremented when an abbrev is defined.
        integer(int64) :: modiff = 0
    contains
        !> Define an abbrev in the table.
        procedure :: define => abbrev_table_define
        !> Make the table known by NAME (`define_abbrev_table').
        procedure :: define_table => abbrev_table_define_table
        !> Undefine all abbrevs (`clear_abbrev_table').
        procedure :: clear => clear_abbrev_table
        !> Whether there are no abbrevs (`abbrev_table_empty_p').
        procedure :: empty => abbrev_table_empty_p
        !> Add a parent table.
        procedure :: add_parent => abbrev_table_add_parent
    end type abbrev_table_type

    !> An abbrev as expansion functions return it (upstream's abbrev
    !> symbol): its table and name.  No table is nil.
    type, public :: abbrev_ref_type
        type(abbrev_table_type), pointer :: table => null()
        character(len=:), allocatable :: name
    end type abbrev_ref_type

    abstract interface
        !> The function `expand_abbrev' calls: ABBREV is the abbrev that
        !> was expanded, if expansion took place.
        recursive subroutine abbrev_expand_proc(abbrev, status)
            import :: abbrev_ref_type, status_type
            implicit none(type, external)
            type(abbrev_ref_type), intent(out) :: abbrev
            type(status_type), intent(out) :: status
        end subroutine abbrev_expand_proc
    end interface

    !> Default name of file from which to read and where to save abbrevs.
    !> In Fortmacs it is the init file, whose `configure' gets the abbrevs
    !> as `define' calls (unallocated: the init file).  Port of abbrev.el
    !> abbrev-file-name.
    character(len=:), allocatable, public :: abbrev_file_name

    !> True means user plans to use only global abbrevs.
    !> This makes the commands that normally define mode-specific abbrevs
    !> define global abbrevs instead.  Port of abbrev.el only-global-abbrevs.
    logical, public :: only_global_abbrevs = .false.

    !> True if Abbrev mode is enabled.
    !> Use the command `abbrev_mode' to change the value of this variable in
    !> the current buffer.  Customize this variable to true to enable Abbrev
    !> mode by default in all buffers.  Port of buffer.c abbrev-mode (the
    !> state of the minor mode `abbrev_mode', which `syms_of_abbrev'
    !> associates this with).
    type(logical_setting), pointer, public :: abbrev_mode => null()

    !> List of the tables whose values are abbrev tables.  Port of
    !> abbrev.el abbrev-table-name-list (pointers to the tables, which
    !> carry their names).
    type(abbrev_table_ref_type), allocatable, public :: abbrev_table_name_list(:)

    !> The abbrev table whose abbrevs affect all buffers.
    !> Each buffer may also have a local abbrev table.
    !> If it does, the local table overrides the global one
    !> for any particular abbrev defined in both.  Port of abbrev.el
    !> global-abbrev-table.
    type(abbrev_table_type), target, public :: global_abbrev_table = abbrev_table_type(name="global_abbrev_table")

    !> The abbrev table of mode-specific abbrevs for Fundamental Mode.
    !> Port of abbrev.el fundamental-mode-abbrev-table.
    type(abbrev_table_type), target, public :: fundamental_mode_abbrev_table = &
                                               abbrev_table_type(name="fundamental_mode_abbrev_table")

    !> True if any word abbrevs were defined or altered.
    !> This causes `save_some_buffers' to offer to save the abbrevs.  Port
    !> of abbrev.el abbrevs-changed.
    logical, public :: abbrevs_changed = .false.

    !> True means expand multi-word abbrevs in all caps if the abbrev was so.
    !> Port of abbrev.el abbrev-all-caps.
    logical, public :: abbrev_all_caps = .false.

    !> Buffer position for `expand_abbrev' to use as the start of the abbrev
    !> (a marker; the null handle for none).
    !> When none, use the word before point as the abbrev.
    !> Calling `expand_abbrev' sets this to none.  Port of abbrev.el
    !> abbrev-start-location.
    type(handle_type), public :: abbrev_start_location

    !> Buffer that `abbrev_start_location' has been set for.
    !> Trying to expand an abbrev in any other buffer clears
    !> `abbrev_start_location'.  Port of abbrev.el
    !> abbrev-start-location-buffer.
    type(handle_type), public :: abbrev_start_location_buffer

    !> The abbrev of the last abbrev expanded.  See `abbrev_symbol'.
    !> Port of abbrev.el last-abbrev.
    type(abbrev_ref_type), public :: last_abbrev

    !> The exact text of the last abbrev that was expanded.
    !> It is unallocated if the abbrev has already been unexpanded.  Port of
    !> abbrev.el last-abbrev-text.
    character(len=:), allocatable, public :: last_abbrev_text

    !> The location of the start of the last abbrev that was expanded.
    !> Port of abbrev.el last-abbrev-location.
    integer(int64), public :: last_abbrev_location = 0

    !> Function that `expand_abbrev' uses to perform abbrev expansion.
    !> Takes no arguments, and should return the abbrev if expansion
    !> took place.  Port of abbrev.el abbrev-expand-function (null is
    !> `abbrev_default_expand', its default).
    procedure(abbrev_expand_proc), pointer, public :: abbrev_expand_function => null()

    !> True means save word abbrevs too when files are saved.
    !> If `SAVE_ABBREVS_SILENTLY', don't ask the user before saving.  Port
    !> of files.el save-abbrevs (SAVE_ABBREVS_ASK is t).
    integer, public :: save_abbrevs = SAVE_ABBREVS_ASK

    !> Keymap used in `edit_abbrevs'.  Port of abbrev.el
    !> edit-abbrevs-mode-map (its keys run commands not ported yet).
    type(handle_type), public :: edit_abbrevs_mode_map

    !> Hook run after entering `edit_abbrevs_mode'.  Port of abbrev.el
    !> edit-abbrevs-mode-hook.
    type(hook_type), target, public :: edit_abbrevs_mode_hook = hook_type(name="edit_abbrevs_mode_hook")

    !> Abbrev table for `edit_abbrevs_mode'.  Port of abbrev.el
    !> edit-abbrevs-mode-abbrev-table (made by define-derived-mode).
    type(abbrev_table_type), target, public :: edit_abbrevs_mode_abbrev_table

    interface
        !> Define ABBREV in the table, to expand into EXPANSION and optionally call HOOK.
        !> ABBREV must be a string, and should be lower-case.
        !> To undefine an abbrev, define it without EXPANSION.
        !> If HOOK is present, it is called after EXPANSION is inserted.
        !>
        !> If NO_SELF_INSERT is true (upstream a `no_self_insert' property of
        !> HOOK), HOOK can control whether the character that triggered abbrev
        !> expansion is inserted.  If such a HOOK returns true, the character is
        !> not inserted.  If such a HOOK returns false, then so does `abbrev_insert'
        !> (and `expand_abbrev'), as if no abbrev expansion had taken place.
        !>
        !> These arguments are special:
        !> - COUNT: the value for the abbrev's usage-count, which is incremented each
        !>   time the abbrev is used (the default is zero).
        !> - SYSTEM: if true, says that this is a "system" abbreviation
        !>   which should not be saved in the user's abbreviation file.
        !>   Unless FORCE is also true, a system abbreviation will not
        !>   overwrite a non-system abbreviation of the same name.
        !> - CASE_FIXED: true means that abbreviations are looked up without
        !>   case-folding, and the expansion is not capitalized/upcased.
        !> - ENABLE_FUNCTION: a function of no arguments which returns true
        !>   if the abbrev should be used for a particular call of `expand_abbrev'.
        !> Port of abbrev.el:define-abbrev.
        module subroutine abbrev_table_define(self, abbrev, expansion, hook, count, system, force, case_fixed, &
                                              enable_function, no_self_insert)
            implicit none(type, external)
            class(abbrev_table_type), intent(inout), target :: self
            character(len=*), intent(in) :: abbrev
            character(len=*), intent(in), optional :: expansion
            procedure(abbrev_hook_proc), optional :: hook
            integer(int64), intent(in), optional :: count
            logical, intent(in), optional :: system, force, case_fixed, no_self_insert
            procedure(abbrev_enable_proc), optional :: enable_function
        end subroutine abbrev_table_define

        !> Define NAME (the name of the variable holding the table) as an
        !> abbrev table name: the table is added to
        !> `abbrev_table_name_list'.  Its abbrevs are defined with
        !> `define', its properties are its components:
        !> - `parents' contains the abbrev tables from which this table inherits
        !>   abbreviations.
        !> - `case_fixed' true means that abbreviations are looked up without
        !>   case-folding, and the expansion is not capitalized/upcased.
        !> - `regexp' is a regular expression that specifies how to extract the
        !>   name of the abbrev before point.  The submatch 1 is treated
        !>   as the potential name of an abbrev.  If `regexp' is unallocated, the
        !>   default behavior uses `backward_word' and `forward_word' to extract
        !>   the name of the abbrev, which can therefore by default only be a
        !>   single word.
        !> - `enable_function' can be set to a function of no arguments which
        !>   returns true if and only if the abbrevs in this table should be used
        !>   for this instance of `expand_abbrev'.
        !> Port of abbrev.el:define-abbrev-table (DEFINITIONS, DOCSTRING and
        !> PROPS are the calls and assignments that follow).
        module subroutine abbrev_table_define_table(self, name)
            implicit none(type, external)
            class(abbrev_table_type), intent(inout), target :: self
            character(len=*), intent(in) :: name
        end subroutine abbrev_table_define_table

        !> Undefine all abbrevs in abbrev table TABLE, leaving TABLE empty.
        !> Port of abbrev.el:clear-abbrev-table.
        module subroutine clear_abbrev_table(self)
            implicit none(type, external)
            class(abbrev_table_type), intent(inout) :: self
        end subroutine clear_abbrev_table

        !> Return false if there are abbrev symbols in OBJECT.
        !> If IGNORE_SYSTEM is true, system definitions are ignored.  Port of
        !> abbrev.el:abbrev-table-empty-p.
        module logical function abbrev_table_empty_p(self, ignore_system) result(empty)
            implicit none(type, external)
            class(abbrev_table_type), intent(in) :: self
            logical, intent(in), optional :: ignore_system
        end function abbrev_table_empty_p

        !> Add PARENT to the tables from which the table inherits abbrevs
        !> (its `parents').
        module subroutine abbrev_table_add_parent(self, parent)
            implicit none(type, external)
            class(abbrev_table_type), intent(inout) :: self
            type(abbrev_table_type), intent(in), target :: parent
        end subroutine abbrev_table_add_parent

        !> Undefine all defined abbrevs.  Port of abbrev.el:kill-all-abbrevs.
        module subroutine kill_all_abbrevs()
            implicit none(type, external)
        end subroutine kill_all_abbrevs

        !> Make a new abbrev-table NEW with the same abbrevs as TABLE.
        !> This function does not copy property lists of the abbrevs.
        !> See `define_abbrev' for the documentation of abbrev properties.
        !> Port of abbrev.el:copy-abbrev-table.
        module subroutine copy_abbrev_table(table, new)
            implicit none(type, external)
            type(abbrev_table_type), intent(in) :: table
            type(abbrev_table_type), intent(out) :: new
        end subroutine copy_abbrev_table

        !> Insert the description of all defined abbrevs after point.
        !> Set mark after the inserted text.  Port of abbrev.el:insert-abbrevs.
        module subroutine insert_abbrevs(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine insert_abbrevs

        !> Display a list of the defined abbrevs.
        !> If LOCAL is true (interactively, when invoked with a
        !> prefix arg), display only local, i.e. mode-specific, abbrevs.
        !> Otherwise display all the abbrevs.  Port of abbrev.el:list-abbrevs.
        recursive module subroutine list_abbrevs(status, local)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: local
        end subroutine list_abbrevs

        !> Return the name of the specified abbrev TABLE ("" if it has
        !> none).  Port of abbrev.el:abbrev-table-name.
        module function abbrev_table_name(table) result(name)
            implicit none(type, external)
            type(abbrev_table_type), intent(in), target :: table
            character(len=:), allocatable :: name
        end function abbrev_table_name

        !> Return buffer listing abbreviations and expansions for each abbrev table.
        !>
        !> If LOCAL is true, include in the buffer only the local abbrevs.
        !> Port of abbrev.el:prepare-abbrev-list-buffer.
        recursive module subroutine prepare_abbrev_list_buffer(buffer, status, local)
            implicit none(type, external)
            type(handle_type), intent(out) :: buffer
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: local
        end subroutine prepare_abbrev_list_buffer

        !> Alter abbrev definitions by editing the list of abbrevs.
        !> This selects a buffer containing the list of abbrev definitions
        !> with point located in the abbrev table for the current buffer, and
        !> turns on `edit_abbrevs_mode' in the buffer with the list of abbrevs.
        !>
        !> The abbrevs editing buffer contains a header line for each
        !> abbrev table, which is the abbrev table name in parentheses.
        !>
        !> The header line is followed by one line per abbrev in that table:
        !>
        !>     NAME   USECOUNT   EXPANSION   HOOK
        !>
        !> where NAME and EXPANSION are strings with quotes,
        !> USECOUNT is an integer, and HOOK is any valid function
        !> that may be omitted (it is usually omitted).  Port of
        !> abbrev.el:edit-abbrevs (redefining the abbrevs from the
        !> edited buffer is not ported yet).
        recursive module subroutine edit_abbrevs(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine edit_abbrevs

        !> Write all user-level abbrev definitions to the init file, as
        !> Fortran.
        !> This does not include system abbrevs; it includes only the abbrev tables
        !> listed in `abbrev_table_name_list'.
        !> The file written defines the same abbrevs in the next session (after
        !> \[fortmacs --recompile]): they replace the abbrevs it defined before, at the
        !> end of `configure'.
        !> The argument FILE is the file name to write.  If omitted or empty, it defaults
        !> to the value of `abbrev_file_name'.
        !> If VERBOSE is true, display a message indicating the file where the
        !> abbrevs have been saved.  Port of abbrev.el:write-abbrev-file
        !> (written as Fortran: a setting-saving command).
        recursive module subroutine write_abbrev_file(status, file, verbose)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: file
            logical, intent(in), optional :: verbose
        end subroutine write_abbrev_file

        !> Define a mode-specific abbrev whose expansion is the last word before point.
        !> If there's an active region, use that as the expansion.
        !>
        !> Prefix argument ARG says how many words before point to use for the expansion;
        !> zero means the entire region is the expansion.
        !>
        !> A negative ARG means to undefine the specified abbrev.
        !>
        !> This command reads the abbreviation from the minibuffer.
        !>
        !> See also `inverse_add_mode_abbrev', which performs the opposite task:
        !> if the abbreviation is already in the buffer, use that command to define
        !> a mode-specific abbrev by specifying its expansion in the minibuffer.
        !>
        !> Don't use this function in a program; use `define_abbrev' instead.
        !> Port of abbrev.el:add-mode-abbrev.
        recursive module subroutine add_mode_abbrev(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine add_mode_abbrev

        !> Define a global (all modes) abbrev whose expansion is last word before point.
        !> If there's an active region, use that as the expansion.
        !>
        !> Prefix argument ARG says how many words before point to use for the expansion;
        !> zero means the entire region is the expansion.
        !>
        !> A negative ARG means to undefine the specified abbrev.
        !>
        !> This command reads the abbreviation from the minibuffer.
        !>
        !> See also `inverse_add_global_abbrev', which performs the opposite task:
        !> if the abbreviation is already in the buffer, use that command to define
        !> a global abbrev by specifying its expansion in the minibuffer.
        !>
        !> Don't use this function in a program; use `define_abbrev' instead.
        !> Port of abbrev.el:add-global-abbrev.
        recursive module subroutine add_global_abbrev(arg, status)
            implicit none(type, external)
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine add_global_abbrev

        !> Define abbrev in TABLE, whose expansion is ARG words before point.
        !> Read the abbreviation from the minibuffer, with prompt TYPE.
        !>
        !> ARG of zero means the entire region is the expansion.
        !>
        !> A negative ARG means to undefine the specified abbrev.
        !>
        !> TYPE is an arbitrary string used to prompt user for the kind of
        !> abbrev, such as "Global", "Mode".  (This has no influence on the
        !> choice of the actual TABLE).
        !>
        !> See `inverse_add_abbrev' for the opposite task.
        !>
        !> Don't use this function in a program; use `define_abbrev' instead.
        !> Port of abbrev.el:add-abbrev.
        recursive module subroutine add_abbrev(table, type, arg, status)
            implicit none(type, external)
            type(abbrev_table_type), intent(inout), target :: table
            character(len=*), intent(in) :: type
            type(value_type), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine add_abbrev

        !> Define the word before point as a mode-specific abbreviation.
        !> With prefix argument N, define the Nth word before point as the
        !> abbreviation.  Negative N means use the Nth word after point.
        !>
        !> If `only_global_abbrevs' is true, this command defines a
        !> global (mode-independent) abbrev instead of a mode-specific one.
        !>
        !> This command reads the expansion from the minibuffer, defines the
        !> abbrev, and then expands the abbreviation in the current buffer.
        !>
        !> See also `add_mode_abbrev', which performs the opposite task:
        !> if the expansion is already in the buffer, use that command
        !> to define an abbrev by specifying the abbreviation in the minibuffer.
        !> Port of abbrev.el:inverse-add-mode-abbrev.
        recursive module subroutine inverse_add_mode_abbrev(n, status)
            implicit none(type, external)
            integer(int64), intent(in) :: n
            type(status_type), intent(out) :: status
        end subroutine inverse_add_mode_abbrev

        !> Define the word before point as a global (mode-independent) abbreviation.
        !> With prefix argument N, define the Nth word before point as the
        !> abbreviation.  Negative N means use the Nth word after point.
        !>
        !> This command reads the expansion from the minibuffer, defines the
        !> abbrev, and then expands the abbreviation in the current buffer.
        !>
        !> See also `add_global_abbrev', which performs the opposite task:
        !> if the expansion is already in the buffer, use that command
        !> to define an abbrev by specifying the abbreviation in the minibuffer.
        !> Port of abbrev.el:inverse-add-global-abbrev.
        recursive module subroutine inverse_add_global_abbrev(n, status)
            implicit none(type, external)
            integer(int64), intent(in) :: n
            type(status_type), intent(out) :: status
        end subroutine inverse_add_global_abbrev

        !> Define the word before point as an abbrev in TABLE.
        !> Read the expansion from the minibuffer, using prompt TYPE, define
        !> the abbrev, and then expand the abbreviation in the current
        !> buffer.
        !>
        !> ARG means use the ARG-th word before point as the abbreviation.
        !> Negative ARG means use the ARG-th word after point.
        !>
        !> TYPE is an arbitrary string used to prompt user for the kind of
        !> abbrev, such as "Global", "Mode".  (This has no influence on the
        !> choice of the actual TABLE).
        !>
        !> See also `add_abbrev', which performs the opposite task.  Port of
        !> abbrev.el:inverse-add-abbrev.
        recursive module subroutine inverse_add_abbrev(table, type, arg, status)
            implicit none(type, external)
            type(abbrev_table_type), intent(inout), target :: table
            character(len=*), intent(in) :: type
            integer(int64), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine inverse_add_abbrev

        !> Mark point as the beginning of an abbreviation.
        !> The abbrev to be expanded starts at point rather than at the
        !> beginning of a word.  This way, you can expand an abbrev with
        !> a prefix: insert the prefix, use this command, then insert the
        !> abbrev.
        !>
        !> This command inserts a hyphen after the prefix, and if the abbrev
        !> is subsequently expanded, this hyphen will be removed.
        !>
        !> If the prefix is itself an abbrev, this command expands it,
        !> unless ARG is true.  Interactively, ARG is the prefix
        !> argument.  Port of abbrev.el:abbrev-prefix-mark.
        recursive module subroutine abbrev_prefix_mark(status, arg)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: arg
        end subroutine abbrev_prefix_mark

        !> For each abbrev occurrence in the region, offer to expand it.
        !> Ask the user to type `y' or `n' for each occurrence.
        !> A prefix argument means don't query; expand all abbrevs.  Port of
        !> abbrev.el:expand-region-abbrevs.
        recursive module subroutine expand_region_abbrevs(start, end, status, noquery)
            implicit none(type, external)
            integer(int64), intent(in) :: start, end
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: noquery
        end subroutine expand_region_abbrevs

        !> Define ABBREV as a global abbreviation that expands into EXPANSION.
        !> The characters in ABBREV must all be word constituents in the standard
        !> syntax table.  Port of abbrev.el:define-global-abbrev.
        module subroutine define_global_abbrev(abbrev, expansion, status)
            implicit none(type, external)
            character(len=*), intent(in) :: abbrev, expansion
            type(status_type), intent(out) :: status
        end subroutine define_global_abbrev

        !> Define ABBREV as a mode-specific abbreviation that expands into EXPANSION.
        !> The characters in ABBREV must all be word-constituents in the current mode.
        !> Port of abbrev.el:define-mode-abbrev.
        module subroutine define_mode_abbrev(abbrev, expansion, status)
            implicit none(type, external)
            character(len=*), intent(in) :: abbrev, expansion
            type(status_type), intent(out) :: status
        end subroutine define_mode_abbrev

        !> Return the abbrev named ABBREV in TABLE, in RESULT (no table if
        !> such an abbrev is not defined).
        !> Optional second arg TABLE is the abbrev table to look it up in.
        !> The default is to try buffer's mode-specific abbrev table, then global table.
        !> Port of abbrev.el:abbrev-symbol.
        module subroutine abbrev_symbol(abbrev, result, table)
            implicit none(type, external)
            character(len=*), intent(in) :: abbrev
            type(abbrev_ref_type), intent(out) :: result
            type(abbrev_table_type), intent(in), target, optional :: table
        end subroutine abbrev_symbol

        !> Return the string that ABBREV expands into in the current buffer,
        !> in EXPANSION (unallocated if none).
        !> Optionally specify an abbrev TABLE as second arg;
        !> then ABBREV is looked up in that table only.  Port of
        !> abbrev.el:abbrev-expansion.
        module subroutine abbrev_expansion(abbrev, expansion, table)
            implicit none(type, external)
            character(len=*), intent(in) :: abbrev
            character(len=:), allocatable, intent(out) :: expansion
            type(abbrev_table_type), intent(in), target, optional :: table
        end subroutine abbrev_expansion

        !> Insert abbrev ABBREV at point.
        !> If present, NAME is the name by which this abbrev was found.
        !> If present, WORDSTART is the buffer position where to insert the abbrev.
        !> If WORDEND is present, it is a buffer position; the abbrev replaces the
        !> previous text between WORDSTART and WORDEND.
        !> RESULT is ABBREV if the expansion should be considered as having taken place.
        !> The result can be influenced by a `no_self_insert' property;
        !> see `define_abbrev' for details.  Port of abbrev.el:abbrev-insert.
        recursive module subroutine abbrev_insert(abbrev, result, status, name, wordstart, wordend)
            implicit none(type, external)
            type(abbrev_ref_type), intent(in) :: abbrev
            type(abbrev_ref_type), intent(out) :: result
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: name
            integer(int64), intent(in), optional :: wordstart, wordend
        end subroutine abbrev_insert

        !> Expand the abbrev before point, if there is an abbrev there.
        !> Effective when explicitly called even when `abbrev_mode' is false.
        !> Calls the value of `abbrev_expand_function' with no argument to do
        !> the work, and returns whatever it does in EXPANDED.  (That value should
        !> be the abbrev if expansion occurred, else none.)  Port of
        !> abbrev.el:expand-abbrev.
        recursive module subroutine expand_abbrev(status, expanded)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            type(abbrev_ref_type), intent(out), optional :: expanded
        end subroutine expand_abbrev

        !> Default function to use for `abbrev_expand_function'.
        !> Calls `abbrev_insert' to insert any expansion, and returns what it does.
        !> Port of abbrev.el:abbrev--default-expand (without the obsolete
        !> wrapper hook `abbrev_expand_functions').
        recursive module subroutine abbrev_default_expand(abbrev, status)
            implicit none(type, external)
            type(abbrev_ref_type), intent(out) :: abbrev
            type(status_type), intent(out) :: status
        end subroutine abbrev_default_expand

        !> Undo the expansion of the last abbrev that expanded.
        !> This differs from ordinary undo in that other editing done since then
        !> is not undone.  Port of abbrev.el:unexpand-abbrev.
        recursive module subroutine unexpand_abbrev(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine unexpand_abbrev

        !> Insert before point a full description of abbrev table TABLE.
        !> If optional 2nd arg READABLE is true, insert a human-readable
        !> description.
        !>
        !> If READABLE is false, insert Fortran: the `define' calls that,
        !> in `configure', define the abbrevs of the table exactly as they
        !> are currently defined.
        !> Abbrevs marked as "system abbrevs" are ignored.  Port of
        !> abbrev.el:insert-abbrev-table-description.
        recursive module subroutine insert_abbrev_table_description(table, status, readable)
            implicit none(type, external)
            type(abbrev_table_type), intent(in) :: table
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: readable
        end subroutine insert_abbrev_table_description

        !> Return the user abbrevs in the abbrev table TABLE, in NAMES.
        !> System abbrevs are omitted unless SYSTEM is true.  Port of
        !> abbrev.el:abbrev--table-symbols.
        module subroutine abbrev_table_symbols(table, names, system)
            implicit none(type, external)
            type(abbrev_table_type), intent(in) :: table
            character(len=:), allocatable, intent(out) :: names(:)
            logical, intent(in), optional :: system
        end subroutine abbrev_table_symbols

        !> Hook function for use by `save_some_buffers_functions'.
        !>
        !> Maybe save abbrevs, and record whether we either saved them or asked to.
        !> With QUERY, RESULT tells whether there are abbrevs to save; else
        !> RESULT true inhibits the message of `save_some_buffers'.  Port of
        !> abbrev.el:abbrev--possibly-save.
        recursive module subroutine abbrev_possibly_save(query, arg, result, status)
            implicit none(type, external)
            logical, intent(in) :: query, arg
            logical, intent(out) :: result
            type(status_type), intent(out) :: status
        end subroutine abbrev_possibly_save

        !> Local (mode-specific) abbrev table of current buffer (null for
        !> none).  Port of buffer.c local-abbrev-table.
        module function local_abbrev_table() result(table)
            implicit none(type, external)
            type(abbrev_table_type), pointer :: table
        end function local_abbrev_table

        !> Make TABLE the local abbrev table of the current buffer, as
        !> `(setq local-abbrev-table TABLE)'.
        module subroutine set_local_abbrev_table(table)
            implicit none(type, external)
            type(abbrev_table_type), intent(in), target :: table
        end subroutine set_local_abbrev_table

        !> Major mode for editing the list of abbrev definitions.
        !> This mode is for editing abbrevs in a buffer prepared by `edit_abbrevs',
        !> which see.  Port of abbrev.el:edit-abbrevs-mode.
        recursive module subroutine edit_abbrevs_mode(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine edit_abbrevs_mode

        !> Whether ABBREV has a hook with the `no_self_insert' property
        !> (cmds.c's test after expanding in self-insert-command).
        module logical function abbrev_no_self_insert(abbrev)
            implicit none(type, external)
            type(abbrev_ref_type), intent(in) :: abbrev
        end function abbrev_no_self_insert

        !> Whether Abbrev mode is on in the current buffer (BVAR
        !> abbrev_mode, for cmds.c's self-insert-command).
        module logical function abbrev_mode_p()
            implicit none(type, external)
        end function abbrev_mode_p

        !> Define Abbrev mode, Edit-Abbrevs mode and the tables, and
        !> register the commands.
        module subroutine syms_of_abbrev()
            implicit none(type, external)
        end subroutine syms_of_abbrev
    end interface
end module abbrev_fm
