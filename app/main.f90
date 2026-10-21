! main.f90 -- Fortmacs entry point.
!
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

!> Entry point: with no arguments, run the editor in the terminal.
program fortmacs
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, int32
    use fm_version, only: version_string
    use fm_error, only: status_type, format_error
    use fm_sysdep, only: emacs_isatty
    use fm_emacs, only: emacs_main
    implicit none(type, external)

    character(len=:), allocatable :: arg, term
    integer :: nargs, arglen, stat
    type(status_type) :: status

    nargs = command_argument_count()
    if (nargs == 0) then
        ! emacs.c:main: a terminal is needed on standard input.
        if (.not. emacs_isatty(0_int32)) then
            write (error_unit, "(a)") "fortmacs: standard input is not a tty"
            stop 1, quiet = .true.
        end if
        call get_environment_variable("TERM", length=arglen, status=stat)
        if (stat /= 0 .or. arglen == 0) then
            write (error_unit, "(a)") "fortmacs: please set the environment variable TERM"
            stop 1, quiet = .true.
        end if
        allocate (character(len=arglen) :: term)
        call get_environment_variable("TERM", term)
        call emacs_main(term, 0_int32, status)
        if (.not. status%ok()) then
            write (error_unit, "(a)") "fortmacs: "//format_error(status)
            stop 1, quiet = .true.
        end if
        stop
    end if

    call get_command_argument(1, length=arglen)
    allocate (character(len=arglen) :: arg)
    call get_command_argument(1, value=arg)

    select case (arg)
    case ("--version")
        write (output_unit, "(a)") version_string()
    case ("--help")
        write (output_unit, "(a)") "Usage: fortmacs [--version | --help]"
        write (output_unit, "(a)") "With no arguments, edit *scratch* in the terminal (C-x C-c quits)."
    case default
        write (error_unit, "(a)") "fortmacs: unknown option '"//arg//"'"
        write (error_unit, "(a)") "Usage: fortmacs [--version | --help]"
        stop 2, quiet = .true.
    end select
end program fortmacs
