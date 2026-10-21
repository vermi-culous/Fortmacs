! fm_frame.f90 -- frames: a terminal screen with its glyph matrices.
! Ported from GNU Emacs 31.1 src/frame.h (struct frame, the parts in use).
!
! Copyright (C) 1993-1994, 1999-2026 Free Software Foundation, Inc.
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

!> One frame per terminal, so a frame holds its terminal directly.  The
!> current matrix is what the screen shows; redisplay fills the desired
!> matrix, and update_frame (fm_dispnew) makes the screen match it.
!> The frame has a root window (no splitting yet) and a minibuffer window
!> on its last line; child frames are not ported.
module fm_frame
    use fm_termchar, only: tty_display_info
    use fm_dispextern, only: glyph_matrix_type, face_type
    use fm_window, only: window_type
    implicit none(type, external)
    private

    !> Port of the parts of frame.h:struct frame in use.
    type, public :: frame_type
        type(tty_display_info) :: tty
        type(glyph_matrix_type) :: current_matrix, desired_matrix
        type(window_type) :: root_window, minibuffer_window
        !> The realized faces, indexed by face id (FRAME_FACE_CACHE).
        type(face_type), allocatable :: faces(:)
        !> Where to leave the cursor after an update (set by redisplay from
        !> the selected window's cursor), 0-based; -1 for none.
        integer :: cursor_vpos = -1, cursor_hpos = -1
        !> Hide the cursor after the update (cursor_off_p).
        logical :: cursor_off_p = .false.
        !> The screen contents are unknown and must be redrawn.
        logical :: garbaged = .true.
        logical :: glyphs_initialized_p = .false.
    end type frame_type

end module fm_frame
