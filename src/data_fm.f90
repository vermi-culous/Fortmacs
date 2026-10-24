! data_fm.f90 -- primitive operations on values and symbols.
! Ported from GNU Emacs 31.1 src/data.c.
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

!> Type checks, `eq', list access, symbol values, and (in the submodule
!> data_buffer_fm) buffer-local values and the variables forwarded to
!> Fortran settings.
!>
!> Buffer-local values work as upstream's swapped-in bindings: the value
!> of a variable (its symbol slot, or the Fortran variable it forwards
!> to) is always the current buffer's; switching buffers saves the old
!> buffer's local values and loads the new buffer's.  So Fortran code
!> reads settings such as `fill_column' directly.
!>
!> Wrong-type errors carry the predicate name as detail (upstream's DATA is
!> `(PREDICATE VALUE)').  Dynamic binding (`let'), variable watchers and
!> aliases are not here yet.
module data_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_WRONG_TYPE_ARGUMENT, &
                        FM_VOID_VARIABLE, FM_SETTING_CONSTANT
    use lisp_fm, only: fixnump, list, list_elt, list_tail, listp, nil, stringp, symbolp, symbols, &
                       v_buffer, v_int, v_keymap, v_list, v_marker, v_nil, v_symbol, value_type, &
                       vtag, xsymbol
    use lread_fm, only: ensure_obarray, sym
    use lisp_fm, only: command_proc
    implicit none(type, external)
    private

    !> A setting: a variable that a buffer can have its own value of
    !> (upstream any variable can be buffer-local).  Assigning to it sets
    !> the value seen in the current buffer (the default, unless the
    !> buffer has its own); `set_local' gives the current buffer its own
    !> value, as upstream's `setq_local':
    !>
    !>     fill_column = 72               ! the default
    !>     call fill_column%set_local(60) ! this buffer only
    !>
    !> Fortran code reads `fill_column%value'.
    type, abstract, public :: setting_type
        !> The variable's name, as the help commands show it.
        character(len=48) :: name = ""
        !> The exact value last given through the variable's name (as
        !> `set_variable' does) when the Fortran value can't hold it, such
        !> as a symbol in a logical setting; cleared by any assignment.
        type(value_type) :: exact
        logical :: has_exact = .false.
        !> Void (`makunbound'); cleared by any assignment.
        logical :: void = .false.
    contains
        !> Make the current buffer use the default value again.  Port of
        !> data.c:Fkill_local_variable.
        procedure :: kill_local => setting_kill_local
        !> Whether the current buffer has its own value.  Port of
        !> data.c:Flocal_variable_p.
        procedure :: local_p => setting_local_p
        !> Start giving the current buffer its own value.
        procedure :: make_local => setting_make_local
    end type setting_type

    !> An integer setting.
    type, extends(setting_type), public :: integer_setting
        integer(int64) :: value = 0
    contains
        generic :: assignment(=) => assign_int, assign_int64
        generic :: set_local => set_local_int, set_local_int64, set_local_setting
        procedure, private :: assign_int => integer_assign_int, assign_int64 => integer_assign_int64
        procedure, private :: set_local_int => integer_set_local_int, set_local_int64 => integer_set_local_int64
        procedure, private :: set_local_setting => integer_set_local_setting
    end type integer_setting

    !> A true/false setting (nil or non-nil upstream).
    type, extends(setting_type), public :: logical_setting
        logical :: value = .false.
    contains
        generic :: assignment(=) => assign
        generic :: set_local => set_local_logical
        procedure, private :: assign => logical_assign
        procedure, private :: set_local_logical => logical_set_local
    end type logical_setting

    !> A string setting; unallocated is nil.
    type, extends(setting_type), public :: string_setting
        character(len=:), allocatable :: value
    contains
        generic :: assignment(=) => assign
        generic :: set_local => set_local_string
        procedure, private :: assign => string_assign
        procedure, private :: set_local_string => string_set_local
        !> Make it nil.
        procedure :: clear => string_clear
    end type string_setting

    !> A setting whose value is a function of no arguments (a
    !> `command_type' procedure); null is nil.  Assign with `=>' to
    !> `value'.
    type, extends(setting_type), public :: function_setting
        procedure(command_proc), pointer, nopass :: value => null()
    contains
        generic :: set_local => set_local_function
        procedure, private :: set_local_function => function_set_local
    end type function_setting

    public :: kill_locals_proc, add_local_variables_killer

    abstract interface
        !> Forget the current buffer's values of some buffer-local
        !> variables kept outside the symbols (Fortran objects), as
        !> `kill_all_local_variables' does.
        subroutine kill_locals_proc()
            implicit none(type, external)
        end subroutine kill_locals_proc
    end interface

    !> A function `kill_all_local_variables' runs.
    type :: killer_type
        procedure(kill_locals_proc), pointer, nopass :: fn => null()
    end type killer_type

    !> The functions forgetting buffer-local values kept in Fortran objects,
    !> run by `kill_all_local_variables'.
    type(killer_type), allocatable :: killers(:)

    !> A setting of a module above this one forwarded by its name, like
    !> the built-in table of data_buffer_fm (DEFVAR_LISP and friends).
    type :: forwarded_ref_type
        class(setting_type), pointer :: p => null()
        !> Buffer-local when set (make-variable-buffer-local).
        logical :: local_if_set = .false.
    end type forwarded_ref_type

    !> The settings forwarded with `forward_setting'.
    type(forwarded_ref_type), allocatable :: forwarded(:)

    public :: forward_setting

    public :: check_fixnum, check_symbol, check_string, check_list
    public :: value_eq, car, cdr, car_safe, cdr_safe
    public :: symbol_name, keywordp, boundp, symbol_value, set, makunbound
    public :: make_local_variable, make_variable_buffer_local, kill_local_variable, kill_all_local_variables, &
              local_variable_p, local_variable_if_set_p, default_value, set_default, default_boundp, &
              buffer_local_value, buffer_local_variables, setq_local

    interface
        !> The value of the Fortran variable forwarded to by the symbol
        !> with id ID: `found' is false if it is a plain variable.  (In
        !> data_buffer_fm.)
        module subroutine forwarded_get(id, v, found, bound)
            implicit none(type, external)
            integer(int64), intent(in) :: id
            type(value_type), intent(out) :: v
            logical, intent(out) :: found
            !> False when the variable is void.
            logical, intent(out), optional :: bound
        end subroutine forwarded_get

        !> makunbound on the forwarded variable of symbol ID; `found' is
        !> false if it is a plain variable.  (In data_buffer_fm.)
        module subroutine forwarded_makunbound(id, status, found)
            implicit none(type, external)
            integer(int64), intent(in) :: id
            type(status_type), intent(out) :: status
            logical, intent(out) :: found
        end subroutine forwarded_makunbound

        !> Set the Fortran variable forwarded to by symbol ID; `found' is
        !> false if it is a plain variable.  (In data_buffer_fm.)
        module subroutine forwarded_set(id, v, status, found)
            implicit none(type, external)
            integer(int64), intent(in) :: id
            type(value_type), intent(in) :: v
            type(status_type), intent(out) :: status
            logical, intent(out) :: found
        end subroutine forwarded_set

        !> Whether the current buffer has a local value for symbol ID.
        !> (In data_buffer_fm.)
        module logical function local_in_current(id)
            implicit none(type, external)
            integer(int64), intent(in) :: id
        end function local_in_current

        !> Whether symbol ID becomes buffer-local when set (made so by
        !> `make_variable_buffer_local', or a per-buffer variable of
        !> buffer.c).  (In data_buffer_fm.)
        module logical function auto_local(id)
            implicit none(type, external)
            integer(int64), intent(in) :: id
        end function auto_local

        !> Make VARIABLE have a separate value in the current buffer.
        !> Other buffers will continue to share a common default value.
        !> (The buffer-local value of VARIABLE starts out as the same value
        !> VARIABLE previously had.)
        !>
        !> If the variable is already arranged to become local when set,
        !> this function causes a local value to exist for this buffer,
        !> just as setting the variable would do.  Port of
        !> data.c:Fmake_local_variable.  (In data_buffer_fm.)
        module subroutine make_local_variable(variable, status)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            type(status_type), intent(out) :: status
        end subroutine make_local_variable

        !> Make VARIABLE become buffer-local whenever it is set.
        !> At any time, the value for the current buffer is in effect,
        !> unless the variable has never been set in this buffer,
        !> in which case the default value is in effect.
        !> Note that binding the variable with `let', or setting it while
        !> a `let'-style binding made in this buffer is in effect,
        !> does not make the variable buffer-local.  Port of
        !> data.c:Fmake_variable_buffer_local.  (In data_buffer_fm.)
        module subroutine make_variable_buffer_local(variable, status)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            type(status_type), intent(out) :: status
        end subroutine make_variable_buffer_local

        !> Make VARIABLE no longer have a separate value in the current buffer.
        !> From now on the default value will apply in this buffer.  Port of
        !> data.c:Fkill_local_variable.  (In data_buffer_fm.)
        module subroutine kill_local_variable(variable, status)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            type(status_type), intent(out) :: status
        end subroutine kill_local_variable

        !> Switch to Fundamental mode by killing current buffer's local variables.
        !> Most local variable bindings are eliminated so that the default values
        !> become effective once more.  Also, the syntax table is set from
        !> `standard_syntax_table', the local keymap is set to nil,
        !> and the abbrev table from `fundamental_mode_abbrev_table'.
        !> This function also forces redisplay of the mode line.
        !>
        !> Every function to select a new major mode starts by
        !> calling this function.
        !>
        !> As a special exception, local variables whose names have
        !> a non-nil `permanent_local' property are not eliminated by this function.
        !> If the optional KILL-PERMANENT argument is non-nil, clear out these local
        !> variables, too.
        !>
        !> The first thing this function does is run
        !> the normal hook `change_major_mode_hook'.  Port of
        !> buffer.c:Fkill_all_local_variables.  (In data_buffer_fm.)
        module subroutine kill_all_local_variables(kill_permanent)
            implicit none(type, external)
            logical, intent(in), optional :: kill_permanent
        end subroutine kill_all_local_variables

        !> Non-nil if VARIABLE has a local binding in buffer BUFFER (the
        !> slot number; the current buffer if absent).  Port of
        !> data.c:Flocal_variable_p.  (In data_buffer_fm.)
        module logical function local_variable_p(variable, buffer)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            integer(int64), intent(in), optional :: buffer
        end function local_variable_p

        !> Non-nil if VARIABLE is local in buffer BUFFER when set there.
        !> BUFFER defaults to the current buffer.
        !>
        !> More precisely, return non-nil if either VARIABLE already has a local
        !> value in BUFFER, or if VARIABLE is automatically buffer-local (see
        !> `make_variable_buffer_local').  Port of
        !> data.c:Flocal_variable_if_set_p.  (In data_buffer_fm.)
        module logical function local_variable_if_set_p(variable, buffer)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            integer(int64), intent(in), optional :: buffer
        end function local_variable_if_set_p

        !> Return SYMBOL's default value.
        !> This is the value that is seen in buffers that do not have their own values
        !> for this variable.  The default value is meaningful for variables with
        !> local bindings in certain buffers.  Port of data.c:Fdefault_value.
        !> (In data_buffer_fm.)
        module subroutine default_value(symbol, v, status)
            implicit none(type, external)
            type(value_type), intent(in) :: symbol
            type(value_type), intent(out) :: v
            type(status_type), intent(out) :: status
        end subroutine default_value

        !> Return t if SYMBOL has a non-void default value.
        !> A variable may have a buffer-local value.  This function says whether
        !> the variable has a non-void value outside of the current buffer
        !> context.  Port of data.c:Fdefault_boundp.  (In data_buffer_fm.)
        module logical function default_boundp(symbol)
            implicit none(type, external)
            type(value_type), intent(in) :: symbol
        end function default_boundp

        !> Set SYMBOL's default value to VALUE.  SYMBOL and VALUE are evaluated.
        !> The default value is seen in buffers that do not have their own values
        !> for this variable.  Port of data.c:Fset_default.  (In
        !> data_buffer_fm.)
        module subroutine set_default(symbol, v, status)
            implicit none(type, external)
            type(value_type), intent(in) :: symbol, v
            type(status_type), intent(out) :: status
        end subroutine set_default

        !> Return the value of VARIABLE in BUFFER (a slot number).
        !> If VARIABLE does not have a buffer-local binding in BUFFER, the value
        !> is the default binding of the variable.  Port of
        !> buffer.c:Fbuffer_local_value.  (In data_buffer_fm.)
        module subroutine buffer_local_value(variable, buffer, v, status)
            implicit none(type, external)
            type(value_type), intent(in) :: variable
            integer(int64), intent(in) :: buffer
            type(value_type), intent(out) :: v
            type(status_type), intent(out) :: status
        end subroutine buffer_local_value

        !> Return an alist of variables that are buffer-local in BUFFER.
        !> Most elements look like (SYMBOL VALUE), describing one variable.
        !> For a symbol that is locally unbound, just the symbol appears in the value.
        !> Note that storing new VALUEs in these elements doesn't change the variables.
        !> No argument or nil as argument means use current buffer as BUFFER.
        !> Port of buffer.c:Fbuffer_local_variables (elements are two-element
        !> lists, since values hold proper lists only).
        !> (In data_buffer_fm.)
        module function buffer_local_variables(buffer) result(alist)
            implicit none(type, external)
            integer(int64), intent(in), optional :: buffer
            type(value_type) :: alist
        end function buffer_local_variables

        !> Make VARIABLE buffer-local in the current buffer and set it to
        !> VALUE.  Port of subr.el:setq-local (one pair).  (In
        !> data_buffer_fm.)
        module subroutine setq_local(variable, v, status)
            implicit none(type, external)
            character(len=*), intent(in) :: variable
            type(value_type), intent(in) :: v
            type(status_type), intent(out) :: status
        end subroutine setq_local
    end interface

contains

    !> Make the variable named after setting S stand for S, as the
    !> built-in settings do: its value is S's, `make_local_variable' and
    !> `set_local' give a buffer its own value of S, swapped in with the
    !> buffer.  With LOCAL_IF_SET it is buffer-local whenever it is set
    !> (make-variable-buffer-local).  For the logical, integer and string
    !> settings of modules this one cannot see (data.c's DEFVAR_*
    !> forwarding).
    subroutine forward_setting(s, local_if_set)
        class(setting_type), intent(inout), target :: s
        logical, intent(in), optional :: local_if_set
        type(forwarded_ref_type), allocatable :: grown(:)
        integer(int64) :: id
        integer :: k, n
        if (.not. allocated(forwarded)) allocate (forwarded(0))
        do k = 1, size(forwarded)
            if (associated(forwarded(k)%p, s)) return
        end do
        n = size(forwarded)
        allocate (grown(n + 1))
        do k = 1, n
            grown(k)%p => forwarded(k)%p
            grown(k)%local_if_set = forwarded(k)%local_if_set
        end do
        grown(n + 1)%p => s
        if (present(local_if_set)) grown(n + 1)%local_if_set = local_if_set
        call move_alloc(grown, forwarded)
        ! Resolve the symbol again.
        call ensure_obarray()
        id = xsymbol(sym(trim(s%name)))
        symbols(id)%fwd = 0
    end subroutine forward_setting

    !> Have `kill_all_local_variables' run FN in the buffer whose local
    !> variables it kills (once, however often it is added): for modules
    !> keeping buffer-local variables of derived types in Fortran objects.
    subroutine add_local_variables_killer(fn)
        procedure(kill_locals_proc) :: fn
        type(killer_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(killers)) allocate (killers(0))
        do k = 1, size(killers)
            if (associated(killers(k)%fn, fn)) return
        end do
        n = size(killers)
        allocate (grown(n + 1))
        grown(1:n) = killers
        grown(n + 1)%fn => fn
        call move_alloc(grown, killers)
    end subroutine add_local_variables_killer

    ! ------------------------------------------------------------------
    ! Settings
    ! ------------------------------------------------------------------

    subroutine setting_make_local(self)
        class(setting_type), intent(inout) :: self
        type(status_type) :: st
        call make_local_variable(sym(trim(self%name)), st)
    end subroutine setting_make_local

    subroutine setting_kill_local(self)
        class(setting_type), intent(inout) :: self
        type(status_type) :: st
        call kill_local_variable(sym(trim(self%name)), st)
    end subroutine setting_kill_local

    logical function setting_local_p(self)
        class(setting_type), intent(in) :: self
        setting_local_p = local_variable_p(sym(trim(self%name)))
    end function setting_local_p

    !> Forget an exact Lisp value and voidness: the Fortran value is the
    !> value now.
    subroutine settled(self)
        class(setting_type), intent(inout) :: self
        self%has_exact = .false.
        self%void = .false.
        self%exact = nil()
    end subroutine settled

    subroutine integer_assign_int(self, v)
        class(integer_setting), intent(inout) :: self
        integer, intent(in) :: v
        self%value = v
        call settled(self)
    end subroutine integer_assign_int

    subroutine integer_assign_int64(self, v)
        class(integer_setting), intent(inout) :: self
        integer(int64), intent(in) :: v
        self%value = v
        call settled(self)
    end subroutine integer_assign_int64

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine integer_set_local_int(self, v)
        class(integer_setting), intent(inout) :: self
        integer, intent(in) :: v
        call self%make_local()
        self%value = v
        call settled(self)
    end subroutine integer_set_local_int

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine integer_set_local_int64(self, v)
        class(integer_setting), intent(inout) :: self
        integer(int64), intent(in) :: v
        call self%make_local()
        self%value = v
        call settled(self)
    end subroutine integer_set_local_int64

    !> Give the current buffer its own value, the value of V (as
    !> `(setq-local SELF V)').  Port of subr.el:setq-local.
    subroutine integer_set_local_setting(self, v)
        class(integer_setting), intent(inout) :: self
        class(integer_setting), intent(in) :: v
        call self%make_local()
        self%value = v%value
        self%exact = v%exact
        self%has_exact = v%has_exact
        self%void = .false.
    end subroutine integer_set_local_setting

    subroutine logical_assign(self, v)
        class(logical_setting), intent(inout) :: self
        logical, intent(in) :: v
        self%value = v
        call settled(self)
    end subroutine logical_assign

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine logical_set_local(self, v)
        class(logical_setting), intent(inout) :: self
        logical, intent(in) :: v
        call self%make_local()
        self%value = v
        call settled(self)
    end subroutine logical_set_local

    subroutine string_assign(self, v)
        class(string_setting), intent(inout) :: self
        character(len=*), intent(in) :: v
        self%value = v
        call settled(self)
    end subroutine string_assign

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine string_set_local(self, v)
        class(string_setting), intent(inout) :: self
        character(len=*), intent(in) :: v
        call self%make_local()
        self%value = v
        call settled(self)
    end subroutine string_set_local

    subroutine string_clear(self)
        class(string_setting), intent(inout) :: self
        if (allocated(self%value)) deallocate (self%value)
        call settled(self)
    end subroutine string_clear

    !> Give the current buffer its own value V.  Port of subr.el:setq-local.
    subroutine function_set_local(self, v)
        class(function_setting), intent(inout) :: self
        procedure(command_proc) :: v
        call self%make_local()
        self%value => v
        call settled(self)
    end subroutine function_set_local

    ! ------------------------------------------------------------------
    ! Type checks (lisp.h CHECK_*)
    ! ------------------------------------------------------------------

    subroutine wrong_type(status, predicate)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: predicate
        call set_error(status, FM_WRONG_TYPE_ARGUMENT, predicate)
    end subroutine wrong_type

    !> Port of lisp.h:CHECK_FIXNUM.
    subroutine check_fixnum(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status
        if (.not. fixnump(v)) call wrong_type(status, "fixnump")
    end subroutine check_fixnum

    !> Port of lisp.h:CHECK_SYMBOL.
    subroutine check_symbol(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status
        if (.not. symbolp(v)) call wrong_type(status, "symbolp")
    end subroutine check_symbol

    !> Port of lisp.h:CHECK_STRING.
    subroutine check_string(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status
        if (.not. stringp(v)) call wrong_type(status, "stringp")
    end subroutine check_string

    !> Port of lisp.h:CHECK_LIST.
    subroutine check_list(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status
        if (.not. listp(v)) call wrong_type(status, "listp")
    end subroutine check_list

    ! ------------------------------------------------------------------
    ! Identity and lists
    ! ------------------------------------------------------------------

    !> `eq': same integer, symbol or object handle.  Strings and lists have
    !> no identity once copied, so they are never `eq' (upstream: only the
    !> very same object is).  Port of data.c:Feq.
    elemental logical function value_eq(a, b)
        type(value_type), intent(in) :: a, b
        value_eq = .false.
        if (vtag(a) /= vtag(b)) return
        select case (vtag(a))
        case (V_NIL)
            value_eq = .true.
        case (V_INT, V_SYMBOL)
            value_eq = a%a(1)%i == b%a(1)%i
        case (V_BUFFER, V_MARKER, V_KEYMAP)
            value_eq = a%a(1)%h%index == b%a(1)%h%index .and. &
                       a%a(1)%h%generation == b%a(1)%h%generation
        case default
            ! Nothing to do for other values.
        end select
    end function value_eq

    !> First element of a list; nil for nil.  Port of data.c:Fcar.
    subroutine car(list, v, status)
        type(value_type), intent(in) :: list
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status
        select case (vtag(list))
        case (V_LIST)
            v = list_elt(list, 1_int64)
        case (V_NIL)
            v = nil()
        case default
            call wrong_type(status, "listp")
        end select
    end subroutine car

    !> The list without its first element; nil for nil.
    !> Port of data.c:Fcdr.
    subroutine cdr(list, v, status)
        type(value_type), intent(in) :: list
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status
        select case (vtag(list))
        case (V_LIST)
            v = list_tail(list, 1_int64)
        case (V_NIL)
            v = nil()
        case default
            call wrong_type(status, "listp")
        end select
    end subroutine cdr

    !> Like `car', but nil for a non-list.  Port of data.c:Fcar_safe.
    pure function car_safe(list) result(v)
        type(value_type), intent(in) :: list
        type(value_type) :: v
        if (vtag(list) == V_LIST) v = list_elt(list, 1_int64)
    end function car_safe

    !> Like `cdr', but nil for a non-list.  Port of data.c:Fcdr_safe.
    pure function cdr_safe(list) result(v)
        type(value_type), intent(in) :: list
        type(value_type) :: v
        if (vtag(list) == V_LIST) v = list_tail(list, 1_int64)
    end function cdr_safe

    ! ------------------------------------------------------------------
    ! Symbols
    ! ------------------------------------------------------------------

    !> Port of data.c:Fsymbol_name.
    subroutine symbol_name(symbol, name, status)
        type(value_type), intent(in) :: symbol
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        name = symbols(xsymbol(symbol))%name
    end subroutine symbol_name

    !> True for symbols whose name starts with ':'.  Port of data.c:Fkeywordp.
    logical function keywordp(v)
        type(value_type), intent(in) :: v
        keywordp = .false.
        if (vtag(v) /= V_SYMBOL) return
        call ensure_obarray()
        associate (name => symbols(xsymbol(v))%name)
            if (len(name) > 0) keywordp = name(1:1) == ":"
        end associate
    end function keywordp

    !> Port of data.c:Fboundp (global value only).
    subroutine boundp(symbol, bound, status)
        type(value_type), intent(in) :: symbol
        logical, intent(out) :: bound
        type(status_type), intent(out) :: status
        type(value_type) :: v
        logical :: found
        bound = .false.
        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        call forwarded_get(xsymbol(symbol), v, found, bound)
        if (.not. found) bound = symbols(xsymbol(symbol))%bound
    end subroutine boundp

    !> The value of `symbol'; void-variable if unbound.
    !> Port of data.c:Fsymbol_value (global value only).
    subroutine symbol_value(symbol, v, status)
        type(value_type), intent(in) :: symbol
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status
        integer(int64) :: id
        logical :: found, bound
        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        id = xsymbol(symbol)
        call forwarded_get(id, v, found, bound)
        if (found) then
            if (.not. bound) call set_error(status, FM_VOID_VARIABLE, symbols(id)%name)
            return
        end if
        if (.not. symbols(id)%bound) then
            call set_error(status, FM_VOID_VARIABLE, symbols(id)%name)
            return
        end if
        v = symbols(id)%val
    end subroutine symbol_value

    !> Give `symbol' the value `newval'.  Constants (nil, t, keywords)
    !> signal setting-constant, except a keyword set to itself.
    !> Port of data.c:Fset / set_internal (global value only).
    recursive subroutine set(symbol, newval, status)
        type(value_type), intent(in) :: symbol, newval
        type(status_type), intent(out) :: status
        integer(int64) :: id
        logical :: found
        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        id = xsymbol(symbol)
        if (symbols(id)%constant) then
            if (.not. (keywordp(symbol) .and. value_eq(newval, symbols(id)%val))) then
                call set_error(status, FM_SETTING_CONSTANT, symbols(id)%name)
            end if
            return
        end if
        ! An automatically buffer-local variable becomes local when set.
        if (auto_local(id)) then
            if (.not. local_in_current(id)) then
                call make_local_variable(symbol, status)
                if (.not. status%ok()) return
            end if
        end if
        call forwarded_set(id, newval, status, found)
        if (found) return
        symbols(id)%val = newval
        symbols(id)%bound = .true.
    end subroutine set

    !> Make `symbol' unbound.  Port of data.c:Fmakunbound.
    subroutine makunbound(symbol, status)
        type(value_type), intent(in) :: symbol
        type(status_type), intent(out) :: status
        integer(int64) :: id
        logical :: found
        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        id = xsymbol(symbol)
        if (symbols(id)%constant) then
            call set_error(status, FM_SETTING_CONSTANT, symbols(id)%name)
            return
        end if
        ! Fset (symbol, Qunbound): an automatically buffer-local variable
        ! gets a void local binding.
        if (auto_local(id)) then
            if (.not. local_in_current(id)) then
                call make_local_variable(symbol, status)
                if (.not. status%ok()) return
            end if
        end if
        call forwarded_makunbound(id, status, found)
        if (found) return
        symbols(id)%val = nil()
        symbols(id)%bound = .false.
    end subroutine makunbound
end module data_fm
