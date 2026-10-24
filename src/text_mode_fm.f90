! text_mode_fm.f90 -- Text mode.
! Ported from GNU Emacs 31.1 lisp/textmodes/text-mode.el.
!
! Copyright (C) 1985, 1992, 1994, 2001-2026 Free Software Foundation, Inc.
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

!> Text mode, the major mode for prose.  Deferred: Ispell completion,
!> the context menu, text conversion and paragraph-indent modes.
module text_mode_fm
    use commands_fm, only: commands
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use data_fm, only: logical_setting
    use files_fm, only: require_final_newline, mode_require_final_newline
    use syntax_fm, only: make_syntax_table, modify_syntax_entry, syntax_table_live_p
    use modes_fm, only: major_modes
    use eval_fm, only: hook_type
    use abbrev_fm, only: abbrev_table_type
    implicit none(type, external)
    private

    public :: text_mode, syms_of_text_mode

    !> True if this buffer's major mode is a variant of Text mode.
    !> Obsolete since 27.1: don't set it, and instead of testing its value
    !> use `derived_mode_p'.  Port of text-mode.el text-mode-variant.
    type(logical_setting), public :: text_mode_variant = logical_setting(name="text_mode_variant", value=.false.)

    !> Normal hook run when entering Text mode and many related modes.
    !> Port of text-mode.el text-mode-hook.
    type(hook_type), target, public :: text_mode_hook = hook_type(name="text_mode_hook")

    !> Keymap for `text_mode'.
    !> Many other modes, such as `mail_mode' and `outline_mode', inherit
    !> all the commands defined in this map.  Port of text-mode.el
    !> text-mode-map.
    type(handle_type), public :: text_mode_map

    !> Syntax table used while in `text_mode'.
    type(handle_type), public :: text_mode_syntax_table

    !> Abbrev table for `text_mode'.  Port of text-mode.el
    !> text-mode-abbrev-table (made by define-derived-mode).
    type(abbrev_table_type), target, public :: text_mode_abbrev_table

contains

    !> Build text-mode-syntax-table.
    subroutine init_text_mode_syntax_table()
        type(status_type) :: st
        call make_syntax_table(text_mode_syntax_table, st)
        call modify_syntax_entry(iachar('"'), iachar('"'), ".   ", st, text_mode_syntax_table)
        call modify_syntax_entry(iachar("\"), iachar("\"), ".   ", st, text_mode_syntax_table)
        ! We add `p' so that M-c on 'hello' leads to 'Hello' rather than
        ! 'hello'.
        call modify_syntax_entry(iachar("'"), iachar("'"), "w p", st, text_mode_syntax_table)
        ! UAX #29 says HEBREW PUNCTUATION GERESH behaves like a letter for
        ! the purposes of finding word boundaries.
        call modify_syntax_entry(int(z'5f3'), int(z'5f3'), "w   ", st, text_mode_syntax_table)
        ! GERSHAYIM, MIDDLE DOT, HYPHENATION POINT and FULLWIDTH COLON
        ! should not be word boundaries between letters.
        call modify_syntax_entry(int(z'5f4'), int(z'5f4'), "w   ", st, text_mode_syntax_table)
        call modify_syntax_entry(int(z'b7'), int(z'b7'), "w   ", st, text_mode_syntax_table)
        call modify_syntax_entry(int(z'2027'), int(z'2027'), "w   ", st, text_mode_syntax_table)
        call modify_syntax_entry(int(z'ff1a'), int(z'ff1a'), "w   ", st, text_mode_syntax_table)
    end subroutine init_text_mode_syntax_table

    !> Major mode for editing text written for humans to read.
    !> In this mode, paragraphs are delimited only by blank or white lines.
    !> You can thus get the full benefit of adaptive filling
    !>  (see the variable `adaptive_fill_mode').
    !> Turning on Text mode runs the normal hook `text_mode_hook'.  Port of
    !> text-mode.el:text-mode.
    recursive subroutine text_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        if (.not. syntax_table_live_p(text_mode_syntax_table)) call syms_of_text_mode()
        call major_modes%text_mode%begin(saved, status)
        if (.not. status%ok()) return
        call text_mode_variant%set_local(.true.)
        call require_final_newline%set_local(mode_require_final_newline)
        call major_modes%text_mode%finish(saved, status)
    end subroutine text_mode

    !> Define Text mode.
    subroutine syms_of_text_mode()
        if (.not. syntax_table_live_p(text_mode_syntax_table)) call init_text_mode_syntax_table()
        call major_modes%text_mode%define("text_mode", "Text", text_mode, text_mode_hook, &
                                          syntax_table=text_mode_syntax_table, abbrev_table=text_mode_abbrev_table)
        text_mode_map = major_modes%text_mode%map
        commands%text_mode%run => text_mode
    end subroutine syms_of_text_mode
end module text_mode_fm
