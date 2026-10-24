! derived_fm.f90 -- major modes derived from others.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/derived.el.
!
! Copyright (C) 1993-1994, 1999, 2001-2026 Free Software Foundation, Inc.
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

!> Major modes.  A major mode is a `major_mode_type' object: built-in
!> ones are the components of `major_modes' (modes_fm), others are
!> `target' variables made known with `define'.  Upstream's
!> `define_derived_mode' macro expands into a function; here the mode's
!> command brackets its body with `begin' and `finish':
!>
!>     subroutine my_mode(status)
!>         type(status_type), intent(out) :: status
!>         logical :: saved
!>         call my_mode_object%begin(saved, status)
!>         if (.not. status%ok()) return
!>         call fill_column%set_local(60)
!>         call my_mode_object%finish(saved, status)
!>     end subroutine my_mode
!>
!> with `call my_mode_object%define("my-mode", "My", my_mode, my_mode_hook,
!> parent=major_mode_named("text-mode"))', where `my_mode_hook' is a
!> `hook_type' target variable.
module derived_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: command_proc
    use data_fm, only: kill_all_local_variables
    use eval_fm, only: hook_type
    use buffer_fm, only: buffers, cur
    use keymap_fm, only: make_sparse_keymap, keymap_live_p, keymap_parent, set_keymap_parent, use_local_map
    use subr_fm, only: delay_mode_hooks_begin, delay_mode_hooks_end, run_mode_hooks
    use abbrev_fm, only: abbrev_table_type, local_abbrev_table, set_local_abbrev_table
    implicit none(type, external)
    private

    public :: major_mode_named, derived_mode_p, provided_mode_derived_p

    !> Return true if the current major mode is derived from one of
    !> MODES (mode objects, or names).  MODES can also be a single mode
    !> instead of an array.  Port of subr.el:derived-mode-p.
    interface derived_mode_p
        module procedure derived_mode_p_object, derived_mode_p_names, derived_mode_p_name
    end interface derived_mode_p

    !> True if MODE is derived from a mode that is a member of the array
    !> MODES.  MODES can also be a single mode instead of an array.  Port of
    !> subr.el:provided-mode-derived-p.
    interface provided_mode_derived_p
        module procedure provided_mode_derived_p_names, provided_mode_derived_p_name
    end interface provided_mode_derived_p

    !> A major mode.  Port of what derived.el:define-derived-mode
    !> defines.
    type, public :: major_mode_type
        !> The mode's name, as M-x and the help commands show it.
        character(len=64) :: name = ""
        !> Shown in the mode line (`mode_name').
        character(len=:), allocatable :: mode_name
        !> The mode's command.
        procedure(command_proc), pointer, nopass :: run => null()
        !> The mode it is derived from (`derived_mode_parent'), or null.
        type(major_mode_type), pointer :: parent => null()
        !> Its keymap (`MODE-map'), inheriting the parent's.
        type(handle_type) :: map
        !> Its syntax table, or the null handle to keep the parent's.
        type(handle_type) :: syntax_table
        !> Its abbrev table (`MODE_abbrev_table'), or null to keep the
        !> parent's.
        type(abbrev_table_type), pointer :: abbrev_table => null()
        !> Run after the mode is set up (`MODE-hook').
        !> (A pointer to the hook object, e.g. `text_mode_hook'; null for
        !> none.)
        type(hook_type), pointer :: hook => null()
        !> A special mode, not suitable for ordinary files (the
        !> `mode_class' property `special'); inherited from the parent when
        !> the mode runs.
        logical :: special = .false.
    contains
        !> Make the mode known by NAME.
        procedure :: define => major_mode_define
        !> Start the mode's body: run the parent (or kill all local
        !> variables), then set the mode's name, keymap and syntax table.
        procedure :: begin => major_mode_begin
        !> End the mode's body: run its hooks.
        procedure :: finish => major_mode_finish
        !> Whether the mode is derived from MODE (or is MODE).
        procedure :: derived_from => major_mode_derived_from
    end type major_mode_type

    !> A pointer to a major mode.
    type :: major_mode_ref_type
        type(major_mode_type), pointer :: p => null()
    end type major_mode_ref_type

    !> The defined major modes.
    type(major_mode_ref_type), allocatable :: defined(:)

contains

    !> The defined major mode named NAME, or null.
    function major_mode_named(name) result(p)
        character(len=*), intent(in) :: name
        type(major_mode_type), pointer :: p
        integer :: i
        p => null()
        if (.not. allocated(defined)) return
        do i = 1, size(defined)
            if (trim(defined(i)%p%name) == name) then
                p => defined(i)%p
                return
            end if
        end do
    end function major_mode_named

    !> Define major mode SELF named NAME, shown as MODE_NAME, whose command
    !> is RUN, derived from PARENT (none if absent: the body starts by
    !> killing all local variables).  SYNTAX_TABLE, if present, is its
    !> syntax table, ABBREV_TABLE its abbrev table (made known as
    !> NAME_abbrev_table).  Its keymap inherits PARENT's.  Port of
    !> derived.el:define-derived-mode.
    subroutine major_mode_define(self, name, mode_name, run, hook, parent, syntax_table, abbrev_table)
        class(major_mode_type), intent(inout), target :: self
        character(len=*), intent(in) :: name, mode_name
        procedure(command_proc) :: run
        type(hook_type), intent(inout), target, optional :: hook
        type(major_mode_type), intent(in), target, optional :: parent
        type(handle_type), intent(in), optional :: syntax_table
        type(abbrev_table_type), intent(inout), target, optional :: abbrev_table
        type(major_mode_ref_type), allocatable :: grown(:)
        type(status_type) :: st
        integer :: n
        self%name = name
        self%mode_name = mode_name
        self%run => run
        self%hook => null()
        if (present(hook)) then
            self%hook => hook
            if (len_trim(hook%name) == 0) hook%name = name//"_hook"
        end if
        if (present(syntax_table)) self%syntax_table = syntax_table
        if (present(abbrev_table)) then
            self%abbrev_table => abbrev_table
            call abbrev_table%define_table(trim(name)//"_abbrev_table")
        end if
        if (.not. keymap_live_p(self%map)) call make_sparse_keymap(self%map)
        self%parent => null()
        if (present(parent)) then
            self%parent => parent
            if (keymap_live_p(parent%map) .and. .not. keymap_live_p(keymap_parent(self%map))) then
                call set_keymap_parent(self%map, parent%map, st)
            end if
        end if
        if (associated(major_mode_named(name))) return
        if (.not. allocated(defined)) allocate (defined(0))
        n = size(defined)
        allocate (grown(n + 1))
        grown(1:n) = defined
        select type (self)
        type is (major_mode_type)
            grown(n + 1)%p => self
        end select
        call move_alloc(grown, defined)
    end subroutine major_mode_define

    !> Start the body of the mode: run the parent mode with its mode hooks
    !> delayed (or kill all local variables), then set `major_mode',
    !> `mode_name', the local keymap, syntax table and abbrev table (whose
    !> parent becomes the parent mode's).  SAVED is for
    !> `finish'.  Port of the first part of define-derived-mode's
    !> expansion.
    recursive subroutine major_mode_begin(self, saved, status)
        class(major_mode_type), intent(inout) :: self
        logical, intent(out) :: saved
        type(status_type), intent(out) :: status
        type(abbrev_table_type), pointer :: local_table
        status = status_type()
        saved = .false.
        if (cur == 0) return
        call delay_mode_hooks_begin(saved)
        if (associated(self%parent)) then
            if (associated(self%parent%run)) call self%parent%run(status)
            if (.not. status%ok()) then
                call delay_mode_hooks_end(saved)
                return
            end if
        else
            call kill_all_local_variables()
        end if
        buffers(cur)%major_mode = trim(self%name)
        buffers(cur)%mode_name = self%mode_name
        ! Identify special modes.
        if (associated(self%parent)) then
            if (self%parent%special) self%special = .true.
        end if
        if (associated(self%parent) .and. associated(self%abbrev_table)) then
            local_table => local_abbrev_table()
            if (.not. allocated(self%abbrev_table%parents)) then
                ! This can happen if the major mode defines the abbrev-table
                ! to be its parent's.
                if (.not. associated(self%abbrev_table, local_table)) call self%abbrev_table%add_parent(local_table)
            else if (size(self%abbrev_table%parents) == 0) then
                if (.not. associated(self%abbrev_table, local_table)) call self%abbrev_table%add_parent(local_table)
            end if
        end if
        call use_local_map(self%map, status)
        if (self%syntax_table%index /= 0) buffers(cur)%syntax_table = self%syntax_table
        if (associated(self%abbrev_table)) call set_local_abbrev_table(self%abbrev_table)
    end subroutine major_mode_begin

    !> End the body of the mode: end the delay and run its mode hooks.
    !> Port of the end of define-derived-mode's expansion.
    recursive subroutine major_mode_finish(self, saved, status)
        class(major_mode_type), intent(in), target :: self
        logical, intent(in) :: saved
        type(status_type), intent(out) :: status
        call delay_mode_hooks_end(saved)
        select type (self)
        type is (major_mode_type)
            if (associated(self%hook)) then
                call run_mode_hooks(status, self%hook)
            else
                call run_mode_hooks(status)
            end if
        end select
    end subroutine major_mode_finish

    !> Whether SELF is MODE or derived from it.  Port of
    !> subr.el:provided-mode-derived-p (one mode, no extra parents).
    logical function major_mode_derived_from(self, mode) result(derived)
        class(major_mode_type), intent(in), target :: self
        type(major_mode_type), intent(in), target :: mode
        type(major_mode_type), pointer :: m
        integer :: depth
        derived = .false.
        select type (self)
        type is (major_mode_type)
            m => self
        end select
        do depth = 1, 100
            if (.not. associated(m)) return
            if (associated(m, mode)) then
                derived = .true.
                return
            end if
            m => m%parent
        end do
    end function major_mode_derived_from

    !> True if MODE (a mode name) is derived from a mode that is a
    !> member of the list MODES (names).  Port of
    !> subr.el:provided-mode-derived-p (no extra parents).
    logical function provided_mode_derived_p_names(mode, modes) result(derived)
        character(len=*), intent(in) :: mode, modes(:)
        type(major_mode_type), pointer :: m
        integer :: i, depth
        derived = .false.
        do i = 1, size(modes)
            if (mode == trim(modes(i))) derived = .true.
        end do
        if (derived) return
        m => major_mode_named(mode)
        do depth = 1, 100
            if (.not. associated(m)) return
            do i = 1, size(modes)
                if (trim(m%name) == trim(modes(i))) then
                    derived = .true.
                    return
                end if
            end do
            m => m%parent
        end do
    end function provided_mode_derived_p_names

    !> True if MODE (a mode name) is derived from the mode named
    !> `parent'.  Port of subr.el:provided-mode-derived-p (MODES a single
    !> mode).
    logical function provided_mode_derived_p_name(mode, parent) result(derived)
        character(len=*), intent(in) :: mode, parent
        derived = provided_mode_derived_p_names(mode, [parent])
    end function provided_mode_derived_p_name

    !> Return true if the current major mode is derived from MODE.
    !> Port of subr.el:derived-mode-p (one mode).
    logical function derived_mode_p_object(mode) result(derived)
        type(major_mode_type), intent(in), target :: mode
        type(major_mode_type), pointer :: m
        derived = .false.
        if (cur == 0) return
        if (.not. allocated(buffers(cur)%major_mode)) return
        m => major_mode_named(buffers(cur)%major_mode)
        if (associated(m)) derived = m%derived_from(mode)
    end function derived_mode_p_object

    !> Return true if the current major mode is derived from one of
    !> MODES, given by name.  Port of subr.el:derived-mode-p.
    logical function derived_mode_p_names(modes) result(derived)
        character(len=*), intent(in) :: modes(:)
        derived = .false.
        if (cur == 0) return
        if (.not. allocated(buffers(cur)%major_mode)) return
        derived = provided_mode_derived_p_names(buffers(cur)%major_mode, modes)
    end function derived_mode_p_names

    !> Return true if the current major mode is derived from the mode
    !> named `mode'.  Port of subr.el:derived-mode-p (MODES a single mode).
    logical function derived_mode_p_name(mode) result(derived)
        character(len=*), intent(in) :: mode
        derived = derived_mode_p_names([mode])
    end function derived_mode_p_name
end module derived_fm
