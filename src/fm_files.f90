! fm_files.f90 -- file handling commands.
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

!> Only leaving Emacs so far: buffers don't visit files yet, so there is
!> nothing to offer to save.  Visiting and saving files come next.
module fm_files
    use, intrinsic :: iso_fortran_env, only: int64
    use fm_error, only: status_type
    use fm_handle, only: handle_type
    use fm_lisp, only: value_type, make_fixnum
    use fm_lread, only: sym
    use fm_keymap, only: define_key
    use fm_callint, only: defcommand
    use fm_keyboard, only: request_exit
    implicit none(type, external)
    private

    public :: syms_of_files

contains

    !> Leave Emacs.  Port of files.el:save-buffers-kill-terminal (no
    !> emacsclient frames) with files.el:save-buffers-kill-emacs; with no
    !> file-visiting buffers there is nothing to save or confirm.
    subroutine cmd_save_buffers_kill_terminal(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call request_exit()
    end subroutine cmd_save_buffers_kill_terminal

    !> Register the commands and their keys in `ctl_x_map`.
    subroutine syms_of_files(ctl_x_map)
        type(handle_type), intent(in) :: ctl_x_map
        type(value_type) :: key(1)
        type(status_type) :: status

        call defcommand("save-buffers-kill-terminal", cmd_save_buffers_kill_terminal)
        ! files.el: (define-key ctl-x-map "\C-c" 'save-buffers-kill-terminal)
        key(1) = make_fixnum(3_int64)
        call define_key(ctl_x_map, key, sym("save-buffers-kill-terminal"), status)
    end subroutine syms_of_files

end module fm_files
