! error_fm.f90 -- status values that replace Emacs's signal/throw.
! The condition table is ported from GNU Emacs 31.1 src/data.c
! (syms_of_data), src/fileio.c (syms_of_fileio) and src/alloc.c.
!
! Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2026 Free Software
! Foundation, Inc.
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

!> Error handling.
!>
!> Upstream, a failing primitive calls `signal' (or `xsignal1', `error',
!> `args_out_of_range', ...) and unwinds with longjmp.  Fortmacs has no
!> non-local exits: every fallible procedure takes a trailing
!> `type(status_type), intent(out) :: status' argument and returns after
!> setting it.  `intent(out)' resets the status to OK on entry.
!>
!> Each error code corresponds to an Emacs error symbol.  Codes form the
!> same hierarchy as the symbols' `error_conditions' property, so
!> `condition_matches' behaves like a `condition_case' handler test.
module error_fm
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none(type, external)
    private

    public :: status_type, format_message
    public :: set_error, error_name, error_message, condition_matches, &
              format_error

    !> Error codes.  0 means success; the rest mirror Emacs error symbols.
    integer(int32), parameter, public :: FM_OK = 0
    !> The root `error' condition (named so it does not clash with the module).
    integer(int32), parameter, public :: FM_GENERIC_ERROR = 1
    integer(int32), parameter, public :: FM_QUIT = 2
    integer(int32), parameter, public :: FM_USER_ERROR = 3
    integer(int32), parameter, public :: FM_WRONG_TYPE_ARGUMENT = 4
    integer(int32), parameter, public :: FM_ARGS_OUT_OF_RANGE = 5
    integer(int32), parameter, public :: FM_VOID_VARIABLE = 6
    integer(int32), parameter, public :: FM_SETTING_CONSTANT = 7
    integer(int32), parameter, public :: FM_ARITH_ERROR = 8
    integer(int32), parameter, public :: FM_RANGE_ERROR = 9
    integer(int32), parameter, public :: FM_OVERFLOW_ERROR = 10
    integer(int32), parameter, public :: FM_BEGINNING_OF_BUFFER = 11
    integer(int32), parameter, public :: FM_END_OF_BUFFER = 12
    integer(int32), parameter, public :: FM_BUFFER_READ_ONLY = 13
    integer(int32), parameter, public :: FM_TEXT_READ_ONLY = 14
    integer(int32), parameter, public :: FM_FILE_ERROR = 15
    integer(int32), parameter, public :: FM_MEMORY_FULL = 16
    integer(int32), parameter, public :: FM_INVALID_HANDLE = 17
    integer(int32), parameter, public :: FM_VOID_FUNCTION = 18
    integer(int32), parameter, public :: FM_FILE_MISSING = 19
    integer(int32), parameter, public :: FM_FILE_ALREADY_EXISTS = 20
    integer(int32), parameter, public :: FM_MARK_INACTIVE = 21
    integer(int32), parameter, public :: FM_SEARCH_FAILED = 22
    integer(int32), parameter, public :: FM_INVALID_REGEXP = 23
    integer(int32), parameter, public :: FM_PERMISSION_DENIED = 24
    !> syntax.c's `scan-error': its data are the message, the start and
    !> the end of the obstacle, printed as print_error_message prints them
    !> ("\"Unbalanced parentheses\", 3, 9").
    integer(int32), parameter, public :: FM_SCAN_ERROR = 25
    !> lread.c's `end-of-file' and `invalid-read-syntax' (reading a value
    !> typed in the minibuffer: `set_variable').
    integer(int32), parameter, public :: FM_END_OF_FILE = 26
    integer(int32), parameter, public :: FM_INVALID_READ_SYNTAX = 27

    integer(int32), parameter :: last_code = FM_INVALID_READ_SYNTAX
    integer, parameter :: name_len = 24, msg_len = 40

    !> `error_conditions' symbol names, indexed by code.
    character(len=name_len), parameter :: names(0:last_code) = [character(len=name_len) :: &
                                                                "", "error", "quit", "user_error", &
                                                                "wrong_type_argument", "args_out_of_range", &
                                                                "void_variable", "setting_constant", &
                                                                "arith_error", "range_error", "overflow_error", &
                                                                "beginning_of_buffer", "end_of_buffer", &
                                                                "buffer_read_only", "text_read_only", &
                                                                "file_error", "memory_full", "invalid_handle", &
                                                                "void_function", "file_missing", &
                                                                "file_already_exists", "mark_inactive", "search_failed", &
                                                                "invalid_regexp", "permission_denied", "scan_error", &
                                                                "end_of_file", "invalid_read_syntax"]

    !> `error_message' strings, indexed by code.
    character(len=msg_len), parameter :: messages(0:last_code) = [character(len=msg_len) :: &
                                                                  "", "error", "Quit", "", &
                                                                  "Wrong type argument", "Args out of range", &
                                                                  "Symbol's value as variable is void", &
                                                                  "Attempt to set a constant symbol", &
                                                                  "Arithmetic error", "Arithmetic range error", &
                                                                  "Arithmetic overflow error", &
                                                                  "Beginning of buffer", "End of buffer", &
                                                                  "Buffer is read-only", "Text is read-only", &
                                                                  "File error", "Memory exhausted", &
                                                                  "Object has been deleted", &
                                                                  "Symbol's function definition is void", &
                                                                  "File is missing", "File already exists", &
                                                                  "The mark is not active now", "Search failed", &
                                                                  "Invalid regexp", "Cannot access file or directory", &
                                                                  "Scan error", "End of file during parsing", &
                                                                  "Invalid read syntax"]

    !> Parent condition of each code (the next element of its
    !> `error_conditions' list); -1 ends the chain.  `quit' is deliberately
    !> not an `error', as upstream.
    integer(int32), parameter :: parents(0:last_code) = [ &
                                 -1_int32, -1_int32, -1_int32, FM_GENERIC_ERROR, &
                                 FM_GENERIC_ERROR, FM_GENERIC_ERROR, &
                                 FM_GENERIC_ERROR, FM_GENERIC_ERROR, &
                                 FM_GENERIC_ERROR, FM_ARITH_ERROR, FM_RANGE_ERROR, &
                                 FM_GENERIC_ERROR, FM_GENERIC_ERROR, &
                                 FM_GENERIC_ERROR, FM_BUFFER_READ_ONLY, &
                                 FM_GENERIC_ERROR, FM_GENERIC_ERROR, FM_GENERIC_ERROR, &
                                 FM_GENERIC_ERROR, FM_FILE_ERROR, FM_FILE_ERROR, FM_GENERIC_ERROR, &
                                 FM_GENERIC_ERROR, FM_GENERIC_ERROR, FM_FILE_ERROR, FM_GENERIC_ERROR, &
                                 FM_GENERIC_ERROR, FM_GENERIC_ERROR]

    !> Outcome of a fallible operation.  `detail' plays the role of the
    !> signal's DATA (e.g. the offending arguments).
    type :: status_type
        integer(int32) :: code = FM_OK
        character(len=:), allocatable :: detail
    contains
        procedure :: ok => status_ok
    end type status_type

contains

    !> True when no error is recorded.
    elemental logical function status_ok(self)
        class(status_type), intent(in) :: self
        status_ok = self%code == FM_OK
    end function status_ok

    !> Record an error.  The analogue of upstream `xsignal'.
    pure subroutine set_error(status, code, detail)
        type(status_type), intent(inout) :: status
        integer(int32), intent(in) :: code
        character(len=*), intent(in), optional :: detail
        status%code = code
        if (present(detail)) then
            status%detail = detail
        else if (allocated(status%detail)) then
            deallocate (status%detail)
        end if
    end subroutine set_error

    pure logical function valid_code(code)
        integer(int32), intent(in) :: code
        valid_code = code >= 0 .and. code <= last_code
    end function valid_code

    !> Error symbol name of `code' (e.g. "args-out-of-range").
    pure function error_name(code) result(name)
        integer(int32), intent(in) :: code
        character(len=:), allocatable :: name
        if (valid_code(code)) then
            name = trim(names(code))
        else
            name = "error"
        end if
    end function error_name

    !> The `error_message' of `code' (e.g. "Args out of range").
    pure function error_message(code) result(msg)
        integer(int32), intent(in) :: code
        character(len=:), allocatable :: msg
        if (valid_code(code)) then
            msg = trim(messages(code))
        else
            msg = "error"
        end if
    end function error_message

    !> True when an error with `code' would be caught by a handler for
    !> `condition', i.e. `condition' is in its `error_conditions' chain.
    pure logical function condition_matches(code, condition)
        integer(int32), intent(in) :: code, condition
        integer(int32) :: c
        condition_matches = .false.
        if (.not. valid_code(code) .or. code == FM_OK) return
        c = code
        do while (c >= 0)
            if (c == condition) then
                condition_matches = .true.
                return
            end if
            c = parents(c)
        end do
    end function condition_matches

    !> Human-readable text for `status', like `error_message_string':
    !> "MESSAGE: DETAIL", or just DETAIL for user-error.
    !> Port of the message layout in print.c:print_error_message.
    pure function format_error(status) result(text)
        type(status_type), intent(in) :: status
        character(len=:), allocatable :: text
        character(len=:), allocatable :: msg
        if (status%code == FM_OK) then
            text = ""
            return
        end if
        ! print_error_message passes the message through
        ! substitute-command-keys, which curves its quotes.
        msg = format_message(error_message(status%code))
        if (.not. allocated(status%detail)) then
            text = msg
        else if (len(msg) == 0 .or. status%code == FM_GENERIC_ERROR) then
            ! (error "MSG") prints just MSG (print.c:print_error_message).
            text = status%detail
        else if (condition_matches(status%code, FM_FILE_ERROR)) then
            ! A file-error's data are its whole message ("Opening input
            ! file: No such file or directory, /x"), as print_error_message
            ! makes it from them.
            text = status%detail
        else
            text = msg//": "//status%detail
        end if
    end function format_error

    !> Format a string out of a format-string and arguments, converting
    !> grave accents and apostrophes in FMT to curved quotes (the
    !> `curve' style of `text_quoting_style', the style used on a UTF-8
    !> terminal).  Only the format string is converted, so concatenate
    !> the arguments after converting the literal parts.  Port of
    !> editfns.c:Fformat_message (the quoting).
    pure function format_message(fmt) result(s)
        character(len=*), intent(in) :: fmt
        character(len=:), allocatable :: s
        integer :: i
        s = ""
        do i = 1, len(fmt)
            select case (fmt(i:i))
            case ("`")
                s = s//char(int(z'E2'))//char(int(z'80'))//char(int(z'98'))
            case ("'")
                s = s//char(int(z'E2'))//char(int(z'80'))//char(int(z'99'))
            case default
                s = s//fmt(i:i)
            end select
        end do
    end function format_message
end module error_fm
