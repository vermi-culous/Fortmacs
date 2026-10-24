! easy_mmode_fm.f90 -- defining minor modes.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/easy-mmode.el.
!
! Copyright (C) 1997, 2000-2026 Free Software Foundation, Inc.
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

!> Minor modes.  A minor mode is a `minor_mode_type' object: built-in
!> ones are the components of `minor_modes' (modes_fm), others are
!> `target' variables made known with `define'.  Upstream's
!> `define_minor_mode' macro expands into a variable, a keymap, hooks
!> and a function; here they are the object's state, `map', `hook'
!> (with `on_hook' and `off_hook') and type-bound procedures:
!>
!>     call minor_modes%auto_fill_mode%enable(status)
!>     call minor_modes%auto_fill_mode%hook%add(my_procedure)
module easy_mmode_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, t_value, make_keymap_ref, symbols
    use lread_fm, only: sym, intern
    use data_fm, only: set, set_default, symbol_value, make_variable_buffer_local, buffer_local_value, logical_setting, &
                       default_value
    use eval_fm, only: hook_type
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use buffer_fm, only: buffers, cur, set_buffer_internal
    use keymap_fm, only: make_sparse_keymap, keymap_live_p, add_minor_mode_map
    use keyboard_fm, only: last_message
    use xdisp_fm, only: message, add_minor_mode_lighter
    use subr_fm, only: after_hook_proc, add_delayed_after_hook, delaying_mode_hooks
    implicit none(type, external)
    private

    public :: minor_mode_body_proc, minor_mode_getter_proc, easy_mmode_pretty_mode_name, minor_mode_named, minor_mode_index, &
              minor_mode_at

    abstract interface
        !> The body of a minor mode, run after its state is set: `enable'
        !> is the new state.  A mode whose state is another variable
        !> (upstream's :variable with a setter) sets that variable here.
        subroutine minor_mode_body_proc(enable, status)
            import :: status_type
            implicit none(type, external)
            logical, intent(in) :: enable
            type(status_type), intent(out) :: status
        end subroutine minor_mode_body_proc

        !> The state of a minor mode whose state is an expression (upstream's
        !> :variable (GETTER . SETTER)): whether it is on in the current
        !> buffer.
        logical function minor_mode_getter_proc()
            implicit none(type, external)
        end function minor_mode_getter_proc
    end interface

    !> A minor mode.  Port of what easy-mmode.el:define-minor-mode
    !> defines.
    type, public :: minor_mode_type
        !> The mode's name, as M-x and the help commands show it.
        character(len=64) :: name = ""
        !> Shown in the mode line while the mode is on; begins with a space.
        character(len=:), allocatable :: lighter
        !> A global mode (else each buffer has its own state).
        logical :: global = .false.
        !> The variable holding the state when it is not the mode's own
        !> (upstream's :variable), by name, e.g. `transient_mark_mode'.
        character(len=64) :: variable = ""
        !> Run every time the mode is turned on or off, after the state
        !> is set.
        procedure(minor_mode_body_proc), pointer, nopass :: body => null()
        !> The mode's state when it is an expression (upstream's :variable
        !> (GETTER . SETTER), the body being the setter); null otherwise.
        procedure(minor_mode_getter_proc), pointer, nopass :: getter => null()
        !> Run after the mode's hooks and message (upstream's :after-hook);
        !> while mode hooks are delayed, after the delayed mode hooks.
        procedure(after_hook_proc), pointer, nopass :: after_hook => null()
        !> The keymap active while the mode is on (`MODE-map'), made by
        !> `define' with KEYMAP; the null handle for none.
        type(handle_type) :: map
        !> Run after the mode is turned on or off (`MODE-hook'), and
        !> `MODE-on-hook' / `MODE-off-hook'.
        type(hook_type), pointer :: hook => null()
        type(hook_type) :: on_hook, off_hook
        !> The mode's own state (the variable named after the mode, unless
        !> `variable' names another); buffer-local unless `global'.
        type(logical_setting) :: state
    contains
        !> Make the mode known by NAME, with its lighter, keymap and hooks.
        procedure :: define => minor_mode_define
        !> Turn the mode on.
        procedure :: enable => minor_mode_enable
        !> Turn the mode off.
        procedure :: disable => minor_mode_disable
        !> Turn the mode on if it is off, else off.
        procedure :: toggle => minor_mode_toggle
        !> The mode's command: toggle it, or with a prefix argument turn
        !> it on if the argument is positive, else off.
        procedure :: command => minor_mode_command
        !> Whether the mode is on in the current buffer.
        procedure :: enabled => minor_mode_enabled
        !> Whether the mode is on in buffer slot B.
        procedure :: enabled_in => minor_mode_enabled_in
        !> Set the mode's own state (without running the mode).
        procedure :: set_state => minor_mode_set_state
    end type minor_mode_type

    !> A pointer to a minor mode.
    type :: minor_mode_ref_type
        type(minor_mode_type), pointer :: p => null()
    end type minor_mode_ref_type

    !> The defined minor modes.
    type(minor_mode_ref_type), allocatable :: defined(:)

    ! Characters of whitespace syntax in the standard syntax table.
    character(len=*), parameter :: WHITESPACE = " "//achar(9)//achar(10)//achar(12)//achar(13)
contains

    !> Turn the symbol MODE into a string intended for the user.
    !> If provided, LIGHTER will be used to help choose capitalization by,
    !> replacing its case-insensitive matches with the literal string in
    !> LIGHTER.  Port of easy-mmode.el:easy-mmode-pretty-mode-name.
    function easy_mmode_pretty_mode_name(mode, lighter) result(name)
        character(len=*), intent(in) :: mode
        character(len=*), intent(in), optional :: lighter
        character(len=:), allocatable :: name, base, light
        integer :: i
        logical :: word_start
        ! The name with Emacs's hyphens (the result is prose, as upstream's).
        base = mode
        do i = 1, len(base)
            if (base(i:i) == "_") base(i:i) = "-"
        end do
        ! "foo-bar-minor-mode" -> "foo-bar-minor": the matches of
        ! "toggle-\\|-mode\\'" (ignoring case), left to right.
        name = ""
        i = 1
        do while (i <= len(base))
            if (i + 6 <= len(base)) then
                if (lower(base(i:i + 6)) == "toggle-") then
                    i = i + 7
                    cycle
                end if
            end if
            if (i == len(base) - 4) then
                if (lower(base(i:)) == "-mode") exit
            end if
            name = name//base(i:i)
            i = i + 1
        end do
        base = name
        ! "foo-bar-minor" -> "Foo-Bar-Minor"
        name = base
        word_start = .true.
        do i = 1, len(name)
            if (is_alnum(name(i:i))) then
                if (word_start) then
                    name(i:i) = upcase(name(i:i))
                else
                    name(i:i) = downcase(name(i:i))
                end if
                word_start = .false.
            else
                word_start = .true.
            end if
        end do
        ! "-Minor" -> " minor"
        do
            i = index(name, "-Minor")
            if (i == 0) exit
            name = name(1:i - 1)//" minor"//name(i + 6:)
        end do
        name = name//" mode"
        if (len(name) >= 7) then
            if (name(1:7) == "Global-") name = "Global "//name(8:)
        end if
        if (.not. present(lighter)) return
        ! Strip leading and trailing whitespace from LIGHTER, then replace
        ! any (case-insensitive) matches for it in NAME with it.
        light = lighter
        do while (len(light) > 0)
            if (verify(light(1:1), WHITESPACE) /= 0) exit
            light = light(2:)
        end do
        do while (len(light) > 0)
            if (verify(light(len(light):), WHITESPACE) /= 0) exit
            light = light(1:len(light) - 1)
        end do
        if (len(light) == 0) return
        i = 1
        do while (i + len(light) - 1 <= len(name))
            if (lower(name(i:i + len(light) - 1)) == lower(light)) then
                name = name(1:i - 1)//light//name(i + len(light):)
                i = i + len(light)
            else
                i = i + 1
            end if
        end do
    end function easy_mmode_pretty_mode_name

    logical function is_alnum(c)
        character, intent(in) :: c
        is_alnum = (c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z") .or. (c >= "0" .and. c <= "9")
    end function is_alnum

    character function upcase(c)
        character, intent(in) :: c
        upcase = c
        if (c >= "a" .and. c <= "z") upcase = achar(iachar(c) - 32)
    end function upcase

    character function downcase(c)
        character, intent(in) :: c
        downcase = c
        if (c >= "A" .and. c <= "Z") downcase = achar(iachar(c) + 32)
    end function downcase

    function lower(s) result(t)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: t
        integer :: i
        do i = 1, len(s)
            t(i:i) = downcase(s(i:i))
        end do
    end function lower

    !> The defined minor mode named NAME, or null.
    function minor_mode_named(name) result(p)
        character(len=*), intent(in) :: name
        type(minor_mode_type), pointer :: p
        integer :: i
        p => null()
        if (.not. allocated(defined)) return
        do i = 1, size(defined)
            if (trim(defined(i)%p%name) == name) then
                p => defined(i)%p
                return
            end if
        end do
    end function minor_mode_named

    !> The index of the defined minor mode named NAME, or 0.
    integer function minor_mode_index(name) result(k)
        character(len=*), intent(in) :: name
        integer :: i
        k = 0
        if (.not. allocated(defined)) return
        do i = 1, size(defined)
            if (trim(defined(i)%p%name) == name) then
                k = i
                return
            end if
        end do
    end function minor_mode_index

    !> The defined minor mode with index K (from `minor_mode_index').
    function minor_mode_at(k) result(p)
        integer, intent(in) :: k
        type(minor_mode_type), pointer :: p
        p => defined(k)%p
    end function minor_mode_at

    !> Define the minor mode SELF named NAME.  It is buffer-local unless
    !> GLOBAL; its state is its own unless VARIABLE names the variable
    !> holding it.  LIGHTER (if not empty) is shown in the mode line
    !> while the mode is on.  BODY, if present, runs every time the mode
    !> is turned on or off.  KEYMAP true makes the mode's keymap
    !> (`MODE-map', upstream's :keymap), active while the mode is on.
    !> GETTER, if present, gives the state (upstream's :variable (GETTER
    !> . SETTER), BODY setting it).  Port of easy-mmode.el:define-minor-mode.
    subroutine minor_mode_define(self, name, lighter, global, variable, body, hook, keymap, after_hook, getter)
        class(minor_mode_type), intent(inout), target :: self
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: lighter, variable
        logical, intent(in), optional :: global
        procedure(minor_mode_body_proc), optional :: body
        procedure(after_hook_proc), optional :: after_hook
        procedure(minor_mode_getter_proc), optional :: getter
        type(hook_type), intent(inout), target, optional :: hook
        logical, intent(in), optional :: keymap
        type(minor_mode_ref_type), allocatable :: grown(:)
        type(status_type) :: st
        integer :: n
        integer(int64) :: id
        self%name = name
        self%state%name = name
        self%lighter = ""
        if (present(lighter)) self%lighter = lighter
        if (present(global)) self%global = global
        if (present(variable)) self%variable = variable
        self%body => null()
        if (present(body)) self%body => body
        self%after_hook => null()
        if (present(after_hook)) self%after_hook => after_hook
        self%getter => null()
        if (present(getter)) self%getter => getter
        self%hook => null()
        if (present(hook)) then
            self%hook => hook
            if (len_trim(hook%name) == 0) hook%name = name//"_hook"
        end if
        self%on_hook%name = name//"_on_hook"
        self%off_hook%name = name//"_off_hook"
        if (.not. associated(minor_mode_named(name))) then
            if (.not. allocated(defined)) allocate (defined(0))
            n = size(defined)
            allocate (grown(n + 1))
            grown(1:n) = defined
            select type (self)
            type is (minor_mode_type)
                grown(n + 1)%p => self
            end select
            call move_alloc(grown, defined)
        end if
        ! A mode with another variable makes that variable local when set
        ! (a buffer-local mode).
        if (len_trim(self%variable) > 0 .and. .not. self%global) then
            call make_variable_buffer_local(sym(trim(self%variable)), st)
        end if
        ! The mode's variable is now known by its name (data_fm forwards it
        ! to `state`).
        id = intern(name)
        symbols(id)%fwd = 0
        if (len(self%lighter) > 0) call add_minor_mode_lighter(state_name(self), self%lighter)
        if (present(keymap)) then
            if (keymap .and. .not. keymap_live_p(self%map)) then
                call make_sparse_keymap(self%map)
                call add_minor_mode_map(state_name(self), self%map)
            end if
        end if
    end subroutine minor_mode_define

    !> The name of the variable holding the mode's state.
    function state_name(self) result(name)
        class(minor_mode_type), intent(in) :: self
        character(len=:), allocatable :: name
        if (len_trim(self%variable) > 0) then
            name = trim(self%variable)
        else
            name = trim(self%name)
        end if
    end function state_name

    !> Whether the mode is on in buffer slot B.
    logical function minor_mode_enabled_in(self, b) result(on)
        class(minor_mode_type), intent(in) :: self
        integer(int64), intent(in) :: b
        type(value_type) :: v
        type(status_type) :: st
        on = .false.
        if (associated(self%getter)) then
            on = getter_in(self, b)
        else if (len_trim(self%variable) == 0 .and. self%global) then
            on = self%state%value
        else if (b > 0) then
            call buffer_local_value(sym(state_name(self)), b, v, st)
            on = st%ok() .and. .not. nilp(v)
        end if
    end function minor_mode_enabled_in

    logical function minor_mode_enabled(self) result(on)
        class(minor_mode_type), intent(in) :: self
        type(value_type) :: v
        type(status_type) :: st
        if (associated(self%getter)) then
            on = self%getter()
        else if (len_trim(self%variable) > 0) then
            call symbol_value(sym(trim(self%variable)), v, st)
            on = st%ok() .and. .not. nilp(v)
        else if (self%global .and. self%state%local_p()) then
            ! A global mode's state is its default value.
            call default_value(sym(trim(self%name)), v, st)
            on = st%ok() .and. .not. nilp(v)
        else
            on = self%state%value
        end if
    end function minor_mode_enabled

    !> The GETTER of SELF in buffer slot B.
    logical function getter_in(self, b) result(on)
        class(minor_mode_type), intent(in) :: self
        integer(int64), intent(in) :: b
        integer(int64) :: saved
        on = .false.
        if (b <= 0) return
        saved = cur
        call set_buffer_internal(b)
        on = self%getter()
        if (saved > 0) call set_buffer_internal(saved)
    end function getter_in

    !> Set the mode's own state: globally, or (a buffer-local mode) in
    !> the current buffer, as `(setq MODE ON)' does.
    subroutine minor_mode_set_state(self, on)
        class(minor_mode_type), intent(inout) :: self
        logical, intent(in) :: on
        type(status_type) :: st
        if (self%global) then
            ! (setq-default MODE ON)
            if (self%state%local_p()) then
                call set_default(sym(trim(self%name)), bool(on), st)
            else
                self%state = on
            end if
        else
            call self%state%set_local(on)
        end if
    end subroutine minor_mode_set_state

    !> Turn the mode on or off (ENABLE), run its body and hooks, and with
    !> INTERACTIVE say so.  Port of the function easy-mmode.el's
    !> define-minor-mode defines.
    recursive subroutine set_mode(self, enable, status, interactive)
        class(minor_mode_type), intent(inout) :: self
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        logical, intent(in) :: interactive
        logical :: on, had_message
        character(len=:), allocatable :: before
        status = status_type()
        had_message = allocated(last_message)
        if (had_message) before = last_message
        if (associated(self%getter)) then
            ! The state is an expression: the body is its setter.
            continue
        else if (len_trim(self%variable) > 0) then
            ! The state is a variable of its own; a mode with a body sets
            ! it there (:variable with a setter).
            if (.not. associated(self%body)) then
                if (self%global) then
                    call set_default(sym(trim(self%variable)), bool(enable), status)
                else
                    call set(sym(trim(self%variable)), bool(enable), status)
                end if
                if (.not. status%ok()) return
            end if
        else
            call self%set_state(enable)
        end if
        if (associated(self%body)) then
            call self%body(enable, status)
            if (.not. status%ok()) return
        end if
        on = self%enabled()
        ! The on/off hooks are here for backward compatibility only.
        if (associated(self%hook)) call self%hook%run(status)
        if (.not. status%ok()) return
        if (on) then
            call self%on_hook%run(status)
        else
            call self%off_hook%run(status)
        end if
        if (.not. status%ok()) return
        if (interactive) call say(on)
        if (associated(self%after_hook)) then
            if (delaying_mode_hooks()) then
                call add_delayed_after_hook(self%after_hook)
            else
                call self%after_hook(status)
            end if
        end if
    contains
        !> Say whether the mode is now on, avoiding overwriting a message
        !> shown by the body, but overwriting previous messages.
        subroutine say(on)
            logical, intent(in) :: on
            if (allocated(last_message)) then
                if (len(last_message) > 0) then
                    if (.not. had_message) return
                    if (last_message /= before) return
                end if
            end if
            if (on) then
                call message(easy_mmode_pretty_mode_name(trim(self%name), self%lighter)//" enabled"//local_text(self))
            else
                call message(easy_mmode_pretty_mode_name(trim(self%name), self%lighter)//" disabled"//local_text(self))
            end if
        end subroutine say
    end subroutine set_mode

    function bool(b) result(v)
        logical, intent(in) :: b
        type(value_type) :: v
        if (b) then
            v = t_value()
        else
            v = nil()
        end if
    end function bool

    function local_text(self) result(s)
        class(minor_mode_type), intent(in) :: self
        character(len=:), allocatable :: s
        if (self%global) then
            s = ""
        else
            s = " in current buffer"
        end if
    end function local_text

    !> Turn the mode on (upstream's (MODE 1), or (MODE) from Lisp).
    recursive subroutine minor_mode_enable(self, status)
        class(minor_mode_type), intent(inout) :: self
        type(status_type), intent(out) :: status
        call set_mode(self, .true., status, .false.)
    end subroutine minor_mode_enable

    !> Turn the mode off (upstream's (MODE -1)).
    recursive subroutine minor_mode_disable(self, status)
        class(minor_mode_type), intent(inout) :: self
        type(status_type), intent(out) :: status
        call set_mode(self, .false., status, .false.)
    end subroutine minor_mode_disable

    !> Toggle the mode (upstream's (MODE 'toggle)).
    recursive subroutine minor_mode_toggle(self, status)
        class(minor_mode_type), intent(inout) :: self
        type(status_type), intent(out) :: status
        call set_mode(self,.not. self%enabled(), status, .false.)
    end subroutine minor_mode_toggle

    !> Run the mode as its command: toggle it, or with a prefix argument
    !> turn it on if the argument is positive and off otherwise; say
    !> which.
    recursive subroutine minor_mode_command(self, status)
        class(minor_mode_type), intent(inout) :: self
        type(status_type), intent(out) :: status
        logical :: enable
        if (nilp(current_prefix_arg)) then
            enable = .not. self%enabled()
        else
            enable = prefix_numeric_value(current_prefix_arg) >= 1
        end if
        call set_mode(self, enable, status, .true.)
    end subroutine minor_mode_command
end module easy_mmode_fm
