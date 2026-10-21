! fm_dispextern.f90 -- display structures shared by the display code.
! Ported from GNU Emacs 31.1 src/dispextern.h (the parts in use).
!
! Copyright (C) 1985, 1993-1994, 1997-2026 Free Software Foundation, Inc.
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

!> A glyph is one character cell on a terminal: a character and the face
!> to draw it in.  A face here holds only what a terminal can show.
!> Faces are numbered; display code passes the frame's face table
!> (indexed from DEFAULT_FACE_ID = 0) to the terminal functions.
module fm_dispextern
    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none(type, external)
    private

    !> dispextern.h: FACE_TTY_DEFAULT_COLOR and friends (unsigned -1, -2,
    !> -3 upstream).  Real colors are >= 0.
    integer(int64), parameter, public :: FACE_TTY_DEFAULT_COLOR = -1, &
                                         FACE_TTY_DEFAULT_FG_COLOR = -2, &
                                         FACE_TTY_DEFAULT_BG_COLOR = -3

    !> enum face_underline_type (the order of terminfo's Smulx).
    integer, parameter, public :: FACE_NO_UNDERLINE = 0, &
                                  FACE_UNDERLINE_SINGLE = 1, &
                                  FACE_UNDERLINE_DOUBLE_LINE = 2, &
                                  FACE_UNDERLINE_WAVE = 3, &
                                  FACE_UNDERLINE_DOTS = 4, &
                                  FACE_UNDERLINE_DASHES = 5

    !> enum face_id: the default face.
    integer(int32), parameter, public :: DEFAULT_FACE_ID = 0, &
                                         MODE_LINE_ACTIVE_FACE_ID = 1

    !> The parts of struct glyph a character cell needs.
    type, public :: glyph_type
        integer(int32) :: ch = 32
        integer(int32) :: face_id = DEFAULT_FACE_ID
        !> CHAR_GLYPH_PADDING_P: continuation cell of a wide character.
        logical :: padding = .false.
    end type glyph_type

    !> The terminal parts of struct face.
    type, public :: face_type
        integer(int64) :: foreground = FACE_TTY_DEFAULT_COLOR
        integer(int64) :: background = FACE_TTY_DEFAULT_COLOR
        logical :: tty_bold_p = .false., tty_italic_p = .false., &
                   tty_reverse_p = .false., tty_strike_through_p = .false.
        integer :: underline = FACE_NO_UNDERLINE
        !> 0 for the default underline color.
        integer(int64) :: underline_color = 0
    end type face_type

    !> The text area of one screen line (struct glyph_row).  `glyphs`
    !> holds a full line; the first `used` are meaningful.
    type, public :: glyph_row_type
        type(glyph_type), allocatable :: glyphs(:)
        integer :: used = 0
        !> The row holds valid glyphs (false: unknown / not to be drawn).
        logical :: enabled_p = .false.
    end type glyph_row_type

    !> A grid of rows (struct glyph_matrix); rows are numbered from 0.
    type, public :: glyph_matrix_type
        type(glyph_row_type), allocatable :: rows(:)
        integer :: nrows = 0, ncols = 0
    end type glyph_matrix_type

    public :: face_tty_specified_color, glyph_equal_p, char_glyph_space_p

contains

    !> Whether `color` is a real color rather than a default.
    !> Port of dispextern.h:face_tty_specified_color.
    elemental logical function face_tty_specified_color(color)
        integer(int64), intent(in) :: color

        face_tty_specified_color = color >= 0
    end function face_tty_specified_color

    !> dispextern.h:GLYPH_EQUAL_P for character glyphs.
    elemental logical function glyph_equal_p(x, y)
        type(glyph_type), intent(in) :: x, y

        glyph_equal_p = x%ch == y%ch .and. x%face_id == y%face_id .and. &
                        (x%padding .eqv. y%padding)
    end function glyph_equal_p

    !> A blank in the default face.  dispextern.h:CHAR_GLYPH_SPACE_P.
    elemental logical function char_glyph_space_p(g)
        type(glyph_type), intent(in) :: g

        char_glyph_space_p = g%ch == 32 .and. g%face_id == DEFAULT_FACE_ID
    end function char_glyph_space_p

end module fm_dispextern
