! simple_setvar_fm.f90 -- set-variable.
! Ported from GNU Emacs 31.1 lisp/simple.el (set-variable), with
! src/minibuf.c read-variable and the reading of the value.
!
! Copyright (C) 1985-1987, 1993-2026 Free Software Foundation, Inc.
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

!> `set_variable' (M-x set_variable): set a user option, read with
!> completion, to a value typed in Fortran notation.
!>
!> Scope.  Upstream reads the value as a Lisp object (read-from-minibuffer
!> with READ) and checks it against the option's custom type.  Here the
!> value is one Fortran literal of the option's type, and nothing else:
!> `.true.' or `.false.' for a logical option, an integer literal
!> (optional sign and digits) for an integer one, a real literal for a
!> real one, a character literal in double or single quotes (the quote
!> doubled inside) for a string one, and one of the option's named
!> constants for an enumerated one (as C-h v shows them; Fortran names
!> are case-insensitive).  No expressions, other variables or function
!> references are evaluated: this is not an expression evaluator, as
!> Fortmacs has no run-time Lisp and no run-time Fortran either.  A value
!> of another type is rejected with upstream's "Value `...' does not
!> match type ... of ..." error, naming the Fortran type (`logical',
!> `integer', `real', `character', or `choice' for named constants).
!>
!> The options are those tools/user-options lists (custom-variable-p);
!> their settings are assigned through generated code in docstrings_fm,
!> so the Fortran variables themselves change.  With a prefix argument
!> the option is set buffer-locally (make-local-variable), which needs a
!> setting object whose buffer-local values Fortmacs swaps (a variable
!> forwarded by data_buffer_fm); other options can't be buffer-local.
!>
!> Deferred: user options holding hooks, procedures, arrays and other
!> derived types (they are not offered), an unallocated (nil) string
!> value, the default from the variable at point, `variable_interactive'
!> properties, obsolescence messages, C-h in the minibuffer
!> (`minibuffer_help_form'), the `custom_set' functions of options
!> (upstream ignores them too) and buffer-local values of options that
!> are not setting objects.
submodule(simple_fm) simple_setvar_fm
    use docstrings_fm, only: user_option_names, user_option_info, set_user_option_logical, set_user_option_integer, &
                             set_user_option_real, set_user_option_string, set_user_option_choice, variable_doc, &
                             doc_entry_type, OPTION_NONE, OPTION_LOGICAL, OPTION_INTEGER, OPTION_REAL, OPTION_STRING, &
                             OPTION_CHOICE
    use data_fm, only: local_variable_p, local_variable_if_set_p, forwarded_variable_p
    use error_fm, only: FM_END_OF_FILE, FM_INVALID_READ_SYNTAX
    use minibuf_fm, only: read_from_minibuffer, completion_list_table
    use casetab_fm, only: upcase
    implicit none(type, external)

    character(len=*), parameter :: WS = " "//achar(9)//achar(10)

contains

    !> Whether the user option called `name' can be set by `set_variable'.
    logical function settable_option_p(name)
        character(len=*), intent(in) :: name
        integer :: kind
        logical :: setting
        call user_option_info(name, kind, setting)
        settable_option_p = kind /= OPTION_NONE
    end function settable_option_p

    module procedure set_variable
    integer :: kind, n
    logical :: setting, lv, ok, local
    character(len=:), allocatable :: v, typename
    integer(int64) :: i64
    real :: r
    status = status_type()
    call user_option_info(variable, kind, setting)
    if (kind == OPTION_NONE) then
        call set_error(status, FM_USER_ERROR, format_message("`"//variable//"' is not a user option"))
        return
    end if
    local = make_local
    if (.not. local) local = local_variable_if_set_p(sym(variable))
    if (local .and. .not. (setting .and. forwarded_variable_p(sym(variable)))) then
        call set_error(status, FM_GENERIC_ERROR, "Symbol "//variable//" may not be buffer-local")
        return
    end if
    ! The Lisp reader's part: one literal, and nothing after it.
    v = value
    n = verify(v, WS)
    if (n == 0) then
        call set_error(status, FM_END_OF_FILE)
        return
    end if
    v = v(n:verify(v, WS, back=.true.))
    ok = .true.
    select case (kind)
    case (OPTION_LOGICAL)
        typename = "logical"
        call literal_token(v, status)
        if (.not. status%ok()) return
        select case (upper(v))
        case (".TRUE.")
            lv = .true.
        case (".FALSE.")
            lv = .false.
        case default
            ok = .false.
        end select
        if (ok) call set_user_option_logical(variable, lv, local)
    case (OPTION_INTEGER)
        typename = "integer"
        call literal_token(v, status)
        if (.not. status%ok()) return
        ok = integer_literal(v, i64)
        if (ok) call set_user_option_integer(variable, i64, local, ok)
    case (OPTION_REAL)
        typename = "real"
        call literal_token(v, status)
        if (.not. status%ok()) return
        ok = real_literal(v, r)
        if (ok) call set_user_option_real(variable, r)
    case (OPTION_STRING)
        typename = "character"
        if (v(1:1) == """" .or. v(1:1) == "'") then
            call string_literal(v, lv, status)
            if (.not. status%ok()) return
            call set_user_option_string(variable, v, local)
        else
            call literal_token(v, status)
            if (.not. status%ok()) return
            ok = .false.
        end if
    case (OPTION_CHOICE)
        typename = "choice"
        call literal_token(v, status)
        if (.not. status%ok()) return
        call set_user_option_choice(variable, upper(v), local, ok)
    case default
    end select
    if (.not. ok) then
        call set_error(status, FM_USER_ERROR, format_message("Value `"//v//"' does not match type "//typename// &
                                                             " of "//variable))
        return
    end if
    end procedure set_variable

    !> Check that `v' (no surrounding blanks) is one token: else the
    !> reader's "Trailing garbage following expression".
    subroutine literal_token(v, status)
        character(len=*), intent(in) :: v
        type(status_type), intent(inout) :: status
        if (scan(v, WS) > 0) call set_error(status, FM_INVALID_READ_SYNTAX, """Trailing garbage following expression""")
    end subroutine literal_token

    !> `s' in upper case (ASCII).
    function upper(s) result(u)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: u
        integer :: k
        u = s
        do k = 1, len(s)
            if (s(k:k) >= "a" .and. s(k:k) <= "z") u(k:k) = char(ichar(s(k:k)) - 32)
        end do
    end function upper

    !> Whether `v' is an integer literal (an optional sign, then digits)
    !> whose value fits in `n'.
    logical function integer_literal(v, n) result(ok)
        character(len=*), intent(in) :: v
        integer(int64), intent(out) :: n
        integer :: start, ios
        n = 0
        start = 1
        if (v(1:1) == "+" .or. v(1:1) == "-") start = 2
        ok = start <= len(v)
        if (.not. ok) return
        ok = verify(v(start:), "0123456789") == 0 .and. len(v) - start + 1 <= 19
        if (.not. ok) return
        read (v, *, iostat=ios) n
        ok = ios == 0
    end function integer_literal

    !> Whether `v' is a real literal (or an integer one): digits with a
    !> decimal point and an exponent (E or D), either optional.
    logical function real_literal(v, r) result(ok)
        character(len=*), intent(in) :: v
        real, intent(out) :: r
        integer :: k, ios, ndigits
        logical :: point, expo
        r = 0
        k = 1
        if (v(1:1) == "+" .or. v(1:1) == "-") k = 2
        ndigits = 0
        point = .false.
        expo = .false.
        ok = .false.
        do while (k <= len(v))
            if (v(k:k) >= "0" .and. v(k:k) <= "9") then
                ndigits = ndigits + 1
            else if (v(k:k) == "." .and. .not. point) then
                point = .true.
            else
                exit
            end if
            k = k + 1
        end do
        if (ndigits == 0) return
        if (k <= len(v)) then
            if (index("eEdD", v(k:k)) == 0) return
            expo = .true.
            k = k + 1
            if (k <= len(v)) then
                if (v(k:k) == "+" .or. v(k:k) == "-") k = k + 1
            end if
            if (k > len(v)) return
            if (verify(v(k:), "0123456789") /= 0) return
        end if
        if (expo .and. .not. point) continue
        read (v, *, iostat=ios) r
        ok = ios == 0
    end function real_literal

    !> Read the character literal at the start of `v' (in double or single
    !> quotes, the quote doubled inside) into `v'.  An unterminated literal
    !> is the reader's end of file; text after it is trailing garbage.
    subroutine string_literal(v, dummy, status)
        character(len=:), allocatable, intent(inout) :: v
        logical, intent(out) :: dummy
        type(status_type), intent(inout) :: status
        character :: q
        character(len=:), allocatable :: out
        integer :: k
        dummy = .false.
        q = v(1:1)
        out = ""
        k = 2
        do
            if (k > len(v)) then
                call set_error(status, FM_END_OF_FILE)
                return
            end if
            if (v(k:k) == q) then
                if (k < len(v)) then
                    if (v(k + 1:k + 1) == q) then
                        out = out//q
                        k = k + 2
                        cycle
                    end if
                end if
                exit
            end if
            out = out//v(k:k)
            k = k + 1
        end do
        if (k < len(v)) then
            if (verify(v(k + 1:), WS) /= 0) then
                call set_error(status, FM_INVALID_READ_SYNTAX, """Trailing garbage following expression""")
                return
            end if
        end if
        v = out
    end subroutine string_literal

    module procedure cmd_set_variable
    character(len=:), allocatable :: name, prompt, value, current, scope
    type(value_type), allocatable :: names(:)
    type(doc_entry_type) :: entry
    logical :: found, make_local
    integer :: k, n
    ! current-prefix-arg, before the minibuffer's commands change it.
    make_local = .not. nilp(current_prefix_arg)
    ! (read-variable "Set variable: "): the user options set_variable
    ! can set (custom-variable-p).
    allocate (names(size(user_option_names)))
    n = 0
    do k = 1, size(user_option_names)
        if (.not. settable_option_p(trim(user_option_names(k)))) cycle
        n = n + 1
        names(n) = make_string(trim(user_option_names(k)))
    end do
    call completing_read("Set variable: ", completion_list_table(make_list(names(1:n))), name, status, &
                         require_match=REQUIRE_MATCH_T, history="custom_variable_history")
    if (.not. status%ok()) return
    if (len(name) == 0) then
        call set_error(status, FM_USER_ERROR, format_message("You didn't specify a variable"))
        return
    end if
    if (local_variable_p(sym(name))) then
        scope = "(buffer-local)"
    else if (make_local .or. local_variable_if_set_p(sym(name))) then
        scope = "buffer-locally"
    else
        scope = "globally"
    end if
    prompt = "Set "//name//" "//scope//" to value: "
    ! The current value, for M-n.
    call variable_doc(name, entry, found)
    current = ""
    if (found .and. allocated(entry%value)) current = entry%value
    call read_from_minibuffer(prompt, value, status, history="set_variable_value_history", default=current)
    if (.not. status%ok()) return
    ! Empty input reads the default.
    if (verify(value, WS) == 0 .and. len(current) > 0) value = current
    call set_variable(name, value, make_local, status)
    end procedure cmd_set_variable
end submodule simple_setvar_fm
