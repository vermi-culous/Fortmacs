! prog_mode_fm.f90 -- Prog mode.
! Ported from GNU Emacs 31.1 lisp/progmodes/prog-mode.el.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> Prog mode, the parent of programming language modes.  Deferred: its
!> keys (C-M-q, M-q), the context menu and prettify-symbols.
module prog_mode_fm
    use commands_fm, only: commands
    use error_fm, only: status_type
    use modes_fm, only: major_modes
    use eval_fm, only: hook_type
    use abbrev_fm, only: abbrev_table_type
    use handle_fm, only: handle_type
    use files_fm, only: require_final_newline, mode_require_final_newline
    use syntax_fm, only: parse_sexp_ignore_comments
    implicit none(type, external)
    private

    public :: prog_mode, syms_of_prog_mode

    !> Normal hook run when entering programming modes.  Port of
    !> prog-mode.el prog-mode-hook.
    type(hook_type), target, public :: prog_mode_hook = hook_type(name="prog_mode_hook")

    !> Keymap used for programming modes.  Port of prog-mode.el
    !> prog-mode-map.
    type(handle_type), public :: prog_mode_map

    !> Abbrev table for `prog_mode'.  Port of prog-mode.el
    !> prog-mode-abbrev-table (made by define-derived-mode).
    type(abbrev_table_type), target, public :: prog_mode_abbrev_table

contains

    !> Major mode for editing programming language source code.  Port of
    !> prog-mode.el:prog-mode.
    recursive subroutine prog_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        call major_modes%prog_mode%begin(saved, status)
        if (.not. status%ok()) return
        call require_final_newline%set_local(mode_require_final_newline)
        call parse_sexp_ignore_comments%set_local(.true.)
        call major_modes%prog_mode%finish(saved, status)
    end subroutine prog_mode

    !> Define Prog mode.  Its parent is fundamental-mode, which
    !> define-derived-mode takes as no parent.
    subroutine syms_of_prog_mode()
        call major_modes%prog_mode%define("prog_mode", "Prog", prog_mode, prog_mode_hook, &
                                          abbrev_table=prog_mode_abbrev_table)
        prog_mode_map = major_modes%prog_mode%map
        commands%prog_mode%run => prog_mode
    end subroutine syms_of_prog_mode
end module prog_mode_fm
