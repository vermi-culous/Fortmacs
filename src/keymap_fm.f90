! keymap_fm.f90 -- keymaps: binding key sequences to commands.
! Ported from GNU Emacs 31.1 src/keymap.c, plus key-parse, key-valid-p,
! keymap-set and keymap-global-set from lisp/keymap.el.
!
! Copyright (C) 1985-1988, 1993-1995, 1998-2026 Free Software
! Foundation, Inc.
! Copyright (C) 2021-2026 Free Software Foundation, Inc. (keymap.el)
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

!> Keymaps are registry objects referenced by handle (values of tag
!> V_KEYMAP).  Upstream a keymap is the list (keymap ENTRIES... . PARENT);
!> here each keymap holds parallel arrays of events and bindings plus a
!> parent handle.  An entry whose binding is nil is an explicit "unbound"
!> that shadows the parent; `remove' deletes the entry instead.
!>
!> A key is a vector of events: fixnums (characters with modifier bits)
!> and symbols (function keys such as `f1' or `C-home').  Strings as keys
!> are not supported; `key_parse' (kbd) builds vectors.
!>
!> Composite keymaps, which upstream builds as (keymap MAP1 MAP2 ...)
!> when several active maps define the same prefix key, are lists of
!> keymap values.  The symbol `keymap' inside such a list marks the
!> start of an inherited part, exactly as upstream.  A list of keymaps
!> given to `lookup_key' is the same thing, so `keymapp' accepts it too
!> (upstream only accepts the (keymap ...) form).
!>
!> Lifetimes: keymaps made by `define_key' for new prefix keys, and the
!> copies of sub-keymaps made by `copy_keymap', are owned by the keymap
!> they were made for, and `free_keymap' frees them along with it.
!> Freeing a keymap that is still bound or used as a parent leaves stale
!> handles, which are then treated as "not a keymap".
!>
!> Not ported yet: char-table and vector entries (a full keymap is a
!> flag), menu items, `remap', symbols whose function definition is a
!> keymap, event ranges, minor-mode and text-property maps, where-is.
module keymap_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: format_message, status_type, set_error, FM_GENERIC_ERROR, &
                        FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type, slot_table_type, null_handle
    use lisp_fm, only: value_type, V_INT, V_SYMBOL, V_STRING, V_LIST, &
                       V_KEYMAP, SYM_T, symbols, nil, make_fixnum, &
                       make_symbol_ref, make_keymap_ref, make_list, list, vtag, &
                       nilp, fixnump, symbolp, xfixnum, xsymbol, xstring, &
                       xhandle, list_length, list_elements, list_tail, keymap_ref_p, list_elt
    use lread_fm, only: intern
    use data_fm, only: value_eq, symbol_value
    use character_fm, only: CHAR_ALT, CHAR_SUPER, CHAR_HYPER, CHAR_SHIFT, &
                            CHAR_CTL, CHAR_META, MAX_CHAR, &
                            MAX_MULTIBYTE_LENGTH, char_string, &
                            string_char_and_length, multibyte_length
    use buffer_fm, only: buffers, cur
    use lisp_fm, only: command_type, defined_command
    use commands_fm, only: builtin_command
    implicit none(type, external)
    private

    public :: make_keymap, make_sparse_keymap, free_keymap, keymap_live_p
    public :: keymapp, keymap_parent, set_keymap_parent, copy_keymap
    public :: keymap_prompt, keymap_full_p, keymap_entries, keymap_equal, value_equal_proc
    public :: access_keymap, define_key, define_key_range, lookup_key, &
              key_binding
    public :: current_global_map, use_global_map
    public :: current_local_map, use_local_map, current_active_maps, add_minor_mode_map, active_minor_mode_of_map
    public :: parse_modifiers, apply_modifiers
    public :: key_parse, key_description, single_key_description, &
              push_key_description
    public :: key_valid_p, keymap_set, keymap_global_set
    public :: where_is_internal, keymap_events, command_remapping
    public :: accessible_keymaps, map_keymap_items, current_minor_maps

    !> Bind a key given as a `key_valid_p' string.  A character `command'
    !> names a command; otherwise it is any key definition.
    interface keymap_set
        module procedure keymap_set_value, keymap_set_command
    end interface keymap_set

    !> Bind a key in the global map; see `keymap_set'.
    interface keymap_global_set
        module procedure keymap_global_set_value, keymap_global_set_command
    end interface keymap_global_set

    abstract interface
        !> Structural equality of two values (fns.c:Fequal).
        recursive logical function value_equal_proc(a, b)
            import :: value_type
            implicit none(type, external)
            type(value_type), intent(in) :: a, b
        end function value_equal_proc
    end interface

    !> Modifier bits as int64 (termhooks.h).
    integer(int64), parameter :: M_ALT = int(CHAR_ALT, int64), &
                                 M_SUPER = int(CHAR_SUPER, int64), &
                                 M_HYPER = int(CHAR_HYPER, int64), &
                                 M_SHIFT = int(CHAR_SHIFT, int64), &
                                 M_CTL = int(CHAR_CTL, int64), &
                                 M_META = int(CHAR_META, int64)
    integer(int64), parameter :: M_ALL = M_ALT + M_SUPER + M_HYPER + &
                                 M_SHIFT + M_CTL + M_META
    !> keyboard.c:meta_prefix_char (ESC).
    integer(int64), parameter :: META_PREFIX_CHAR = 27
    !> keymap.c:copy_keymap_1 recursion limit.
    integer, parameter :: MAX_COPY_DEPTH = 100

    !> One keymap.  Entries are stored oldest first; upstream's list order
    !> (newest first) is the reverse.
    type :: keymap_type
        type(value_type), allocatable :: events(:), bindings(:)
        integer(int64) :: n = 0
        type(handle_type) :: parent
        !> The keymap this one was made for (see the module note).
        type(handle_type) :: owner
        !> Overall prompt string (make-keymap's STRING argument).
        type(value_type) :: prompt
        !> Made by make-keymap rather than make-sparse-keymap.
        logical :: full = .false.
        !> Character ranges (upstream: char-table ranges), newest last;
        !> they apply to characters without modifiers that have no entry.
        integer(int64), allocatable :: range_from(:), range_to(:)
        type(value_type), allocatable :: range_bindings(:)
        integer(int64) :: nranges = 0
    end type keymap_type

    type(keymap_type), allocatable :: keymaps(:)
    type(slot_table_type) :: table
    !> keymap.c:current_global_map; made on first use.
    type(handle_type) :: global_map
    !> KVAR Voverriding_terminal_local_map: a keymap taking precedence
    !> over all others (null: none).  Used for transient maps.
    type(handle_type), public :: overriding_terminal_local_map
    !> The map a transient map was pushed over: subr.el:set-transient-map
    !> composes the new map with the old overriding-terminal-local-map
    !> (internal-push-keymap); this is the old one, active below it.
    type(handle_type), public :: pushed_terminal_local_map

    !> minor-mode-map-alist: keymaps active while their variable (a
    !> symbol id) is non-nil, the most recently added first.
    integer(int64), allocatable :: minor_mode_vars(:)
    type(handle_type), allocatable :: minor_mode_maps(:)
    integer :: n_minor_mode_maps = 0

    !> A key sequence and the keymap it leads to (where-is,
    !> `accessible_keymaps').
    type, public :: key_list_type
        type(value_type), allocatable :: keys(:)
        type(handle_type) :: map
    end type key_list_type

    !> State of one access_keymap_1 scan.
    type :: access_state
        !> The binding found so far; meaningful when `found' (else Qunbound).
        type(value_type) :: retval
        logical :: found = .false.
        !> `retval' is a composite list built by this scan (retval_tail).
        logical :: built = .false.
        type(value_type) :: t_binding
        logical :: t_found = .false.
        logical :: t_ok = .false.
    end type access_state

contains

    ! ------------------------------------------------------------------
    ! Registry
    ! ------------------------------------------------------------------

    subroutine new_keymap(km, full, prompt)
        type(handle_type), intent(out) :: km
        logical, intent(in) :: full
        type(value_type), intent(in), optional :: prompt
        type(keymap_type), allocatable :: grown(:)
        call table%acquire(km)
        if (.not. allocated(keymaps)) then
            allocate (keymaps(table%capacity()))
        else if (size(keymaps, kind=int64) < table%capacity()) then
            allocate (grown(table%capacity()))
            grown(1:size(keymaps)) = keymaps
            call move_alloc(grown, keymaps)
        end if
        keymaps(km%index) = keymap_type()
        keymaps(km%index)%full = full
        if (present(prompt)) keymaps(km%index)%prompt = prompt
    end subroutine new_keymap

    !> A new full keymap.  Port of keymap.c:Fmake_keymap.
    subroutine make_keymap(km, prompt)
        type(handle_type), intent(out) :: km
        type(value_type), intent(in), optional :: prompt
        call new_keymap(km, .true., prompt)
    end subroutine make_keymap

    !> A new sparse keymap.  Port of keymap.c:Fmake_sparse_keymap.
    subroutine make_sparse_keymap(km, prompt)
        type(handle_type), intent(out) :: km
        type(value_type), intent(in), optional :: prompt
        call new_keymap(km, .false., prompt)
    end subroutine make_sparse_keymap

    !> Slot of keymap `km', or a wrong-type-argument error.
    subroutine resolve_keymap(km, i, status)
        type(handle_type), intent(in) :: km
        integer(int64), intent(out) :: i
        type(status_type), intent(out) :: status
        call table%resolve(km, i, status)
        if (.not. status%ok()) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "keymapp")
        end if
    end subroutine resolve_keymap

    logical function keymap_live_p(km)
        type(handle_type), intent(in) :: km
        keymap_live_p = table%is_live(km)
    end function keymap_live_p

    logical function same_handle(a, b)
        type(handle_type), intent(in) :: a, b
        same_handle = a%index == b%index .and. a%generation == b%generation
    end function same_handle

    !> Free keymap `km' and the keymaps it owns.  Not a port: upstream
    !> relies on GC.
    recursive subroutine free_keymap(km, status)
        type(handle_type), intent(in) :: km
        type(status_type), intent(out) :: status
        type(handle_type) :: h
        type(status_type) :: ignored
        integer(int64) :: i, k
        call resolve_keymap(km, i, status)
        if (.not. status%ok()) return
        keymaps(i) = keymap_type()
        call table%release(km, status)
        if (.not. status%ok()) return
        do k = 1, table%capacity()
            h = table%handle_of(k)
            if (h%index == 0) cycle
            if (same_handle(keymaps(k)%owner, km)) call free_keymap(h, ignored)
        end do
    end subroutine free_keymap

    !> The prompt string of `km' (nil if none).
    function keymap_prompt(km) result(v)
        type(handle_type), intent(in) :: km
        type(value_type) :: v
        if (table%is_live(km)) v = keymaps(km%index)%prompt
    end function keymap_prompt

    !> The bindings of `km' in upstream's list order, newest first: its
    !> events and their definitions, and its character ranges with theirs.
    subroutine keymap_entries(km, events, bindings, range_from, range_to, range_bindings)
        type(handle_type), intent(in) :: km
        type(value_type), allocatable, intent(out) :: events(:), bindings(:), range_bindings(:)
        integer(int64), allocatable, intent(out) :: range_from(:), range_to(:)
        integer(int64) :: i, n, k
        allocate (events(0), bindings(0), range_bindings(0), range_from(0), range_to(0))
        if (.not. table%is_live(km)) return
        i = km%index
        n = keymaps(i)%n
        events = keymaps(i)%events(n:1:-1)
        bindings = keymaps(i)%bindings(n:1:-1)
        k = keymaps(i)%nranges
        range_from = keymaps(i)%range_from(k:1:-1)
        range_to = keymaps(i)%range_to(k:1:-1)
        range_bindings = keymaps(i)%range_bindings(k:1:-1)
    end subroutine keymap_entries

    !> Whether keymaps `a' and `b' are `equal': the same bindings
    !> (compared with `equal'), prompt and parent, as fns.c:internal_equal
    !> compares their lists (and a full keymap's char-tables).
    recursive logical function keymap_equal(a, b, equal) result(eq)
        type(handle_type), intent(in) :: a, b
        procedure(value_equal_proc) :: equal
        integer(int64), allocatable :: from_a(:), to_a(:), from_b(:), to_b(:)
        type(value_type), allocatable :: val_a(:), val_b(:)
        integer, allocatable :: order_a(:), order_b(:)
        integer(int64) :: i, j
        integer :: k
        eq = same_handle(a, b)
        if (eq) return
        if (.not. (table%is_live(a) .and. table%is_live(b))) return
        i = a%index
        j = b%index
        if (keymaps(i)%full .neqv. keymaps(j)%full) return
        if (.not. equal(keymaps(i)%prompt, keymaps(j)%prompt)) return
        if (keymaps(i)%full) then
            call char_table_runs(keymaps(i), from_a, to_a, val_a)
            call char_table_runs(keymaps(j), from_b, to_b, val_b)
            if (size(from_a) /= size(from_b)) return
            do k = 1, size(from_a)
                if (from_a(k) /= from_b(k) .or. to_a(k) /= to_b(k)) return
                if (.not. equal(val_a(k), val_b(k))) return
            end do
        else
            if (keymaps(i)%nranges /= keymaps(j)%nranges) return
            do k = 1, int(keymaps(i)%nranges)
                if (keymaps(i)%range_from(k) /= keymaps(j)%range_from(k)) return
                if (keymaps(i)%range_to(k) /= keymaps(j)%range_to(k)) return
                if (.not. equal(keymaps(i)%range_bindings(k), keymaps(j)%range_bindings(k))) return
            end do
        end if
        call entry_order(keymaps(i), order_a)
        call entry_order(keymaps(j), order_b)
        if (size(order_a) /= size(order_b)) return
        do k = 1, size(order_a)
            if (.not. equal(keymaps(i)%events(order_a(k)), keymaps(j)%events(order_b(k)))) return
            if (.not. equal(keymaps(i)%bindings(order_a(k)), keymaps(j)%bindings(order_b(k)))) return
        end do
        if (table%is_live(keymaps(i)%parent) .neqv. table%is_live(keymaps(j)%parent)) return
        if (table%is_live(keymaps(i)%parent)) then
            eq = keymap_equal(keymaps(i)%parent, keymaps(j)%parent, equal)
        else
            eq = .true.
        end if
    end function keymap_equal

    !> Whether `km' was made by make-keymap (upstream: has a char-table).
    logical function keymap_full_p(km)
        type(handle_type), intent(in) :: km
        keymap_full_p = .false.
        if (table%is_live(km)) keymap_full_p = keymaps(km%index)%full
    end function keymap_full_p

    ! ------------------------------------------------------------------
    ! Predicates and parents
    ! ------------------------------------------------------------------

    !> The symbol `keymap', which marks inherited parts of composites.
    function keymap_marker() result(v)
        type(value_type) :: v
        v = make_symbol_ref(intern("keymap"))
    end function keymap_marker

    logical function is_marker(v)
        type(value_type), intent(in) :: v
        is_marker = .false.
        if (symbolp(v)) is_marker = xsymbol(v) == intern("keymap")
    end function is_marker

    logical function is_t(v)
        type(value_type), intent(in) :: v
        is_t = .false.
        if (symbolp(v)) is_t = xsymbol(v) == SYM_T
    end function is_t

    !> Whether `v' is a live keymap or a composite (non-empty list of
    !> keymaps and `keymap' markers).  Port of keymap.c:Fkeymapp.
    recursive logical function keymapp(v) result(yes)
        type(value_type), intent(in) :: v
        type(value_type), allocatable :: elems(:)
        integer(int64) :: k
        yes = .false.
        select case (vtag(v))
        case (V_KEYMAP)
            yes = table%is_live(xhandle(v))
        case (V_LIST)
            call list_elements(v, elems)
            do k = 1, size(elems, kind=int64)
                if (is_marker(elems(k))) cycle
                if (.not. keymapp(elems(k))) return
                yes = .true.
            end do
        case default
            ! Nothing to do for other values.
        end select
    end function keymapp

    !> Parent of `km', or the null handle.  Port of keymap.c:Fkeymap_parent.
    function keymap_parent(km) result(parent)
        type(handle_type), intent(in) :: km
        type(handle_type) :: parent
        parent = null_handle()
        if (.not. table%is_live(km)) return
        if (table%is_live(keymaps(km%index)%parent)) then
            parent = keymaps(km%index)%parent
        end if
    end function keymap_parent

    !> The events `km' binds to non-nil definitions, as subr.el's
    !> set-transient-map collects them by pushing what map-keymap visits
    !> (so in the reverse order): the parent's first.  Character ranges
    !> are not included.
    recursive subroutine keymap_events(km, events)
        type(handle_type), intent(in) :: km
        type(value_type), allocatable, intent(out) :: events(:)
        type(value_type), allocatable :: inherited(:), all(:)
        integer, allocatable :: order(:)
        integer(int64) :: j, n, k
        if (.not. table%is_live(km)) then
            allocate (events(0))
            return
        end if
        if (table%is_live(keymaps(km%index)%parent)) then
            call keymap_events(keymaps(km%index)%parent, inherited)
        else
            allocate (inherited(0))
        end if
        call entry_order(keymaps(km%index), order)
        n = size(order, kind=int64)
        allocate (all(size(inherited, kind=int64) + n))
        k = size(inherited, kind=int64)
        if (k > 0) all(1:k) = inherited
        do j = n, 1, -1
            if (nilp(keymaps(km%index)%bindings(order(j)))) cycle
            k = k + 1
            all(k) = keymaps(km%index)%events(order(j))
        end do
        allocate (events(k))
        if (k > 0) events = all(1:k)
    end subroutine keymap_events

    !> Make `parent' (or the null handle for none) the parent of `km'.
    !> Port of keymap.c:Fset_keymap_parent.
    subroutine set_keymap_parent(km, parent, status)
        type(handle_type), intent(in) :: km, parent
        type(status_type), intent(out) :: status
        type(handle_type) :: p
        integer(int64) :: i
        call resolve_keymap(km, i, status)
        if (.not. status%ok()) return
        if (parent%index /= 0) then
            if (.not. table%is_live(parent)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "keymapp")
                return
            end if
            ! keymap.c:keymap_memberp
            p = parent
            do while (table%is_live(p))
                if (same_handle(p, km)) then
                    call set_error(status, FM_GENERIC_ERROR, &
                                   "Cyclic keymap inheritance")
                    return
                end if
                p = keymaps(p%index)%parent
            end do
        end if
        keymaps(i)%parent = parent
    end subroutine set_keymap_parent

    ! ------------------------------------------------------------------
    ! Events
    ! ------------------------------------------------------------------

    !> "A-C-H-M-S-s-" restricted to the modifiers in `bits'.  The order of
    !> keyboard.c:apply_modifiers_uncached.
    function modifier_prefix(bits) result(s)
        integer(int64), intent(in) :: bits
        character(len=:), allocatable :: s
        s = ""
        if (iand(bits, M_ALT) /= 0) s = s//"A-"
        if (iand(bits, M_CTL) /= 0) s = s//"C-"
        if (iand(bits, M_HYPER) /= 0) s = s//"H-"
        if (iand(bits, M_META) /= 0) s = s//"M-"
        if (iand(bits, M_SHIFT) /= 0) s = s//"S-"
        if (iand(bits, M_SUPER) /= 0) s = s//"s-"
    end function modifier_prefix

    !> Modifier bit for prefix letter `ch', or 0.
    integer(int64) function modifier_bit(ch)
        character, intent(in) :: ch
        select case (ch)
        case ("A")
            modifier_bit = M_ALT
        case ("C")
            modifier_bit = M_CTL
        case ("H")
            modifier_bit = M_HYPER
        case ("M")
            modifier_bit = M_META
        case ("S")
            modifier_bit = M_SHIFT
        case ("s")
            modifier_bit = M_SUPER
        case default
            modifier_bit = 0
        end select
    end function modifier_bit

    !> Split symbol event `v' into modifier bits and base name: `C-M-home'
    !> gives CTL+META and "home".  Port of the modifier part of
    !> keyboard.c:parse_modifiers (click modifiers are not handled).
    subroutine parse_modifiers(v, bits, base)
        type(value_type), intent(in) :: v
        integer(int64), intent(out) :: bits
        character(len=:), allocatable, intent(out) :: base
        character(len=:), allocatable :: name
        integer(int64) :: i
        name = symbols(xsymbol(v))%name
        bits = 0
        i = 1
        do while (i + 2 <= len(name, kind=int64))
            if (name(i + 1:i + 1) /= "-") exit
            if (modifier_bit(name(i:i)) == 0) exit
            bits = ior(bits, modifier_bit(name(i:i)))
            i = i + 2
        end do
        base = name(i:)
    end subroutine parse_modifiers

    !> The symbol event for `base' with modifiers `bits', prefixes in
    !> canonical order.  Port of keyboard.c:apply_modifiers (symbols only).
    function apply_modifiers(bits, base) result(e)
        integer(int64), intent(in) :: bits
        character(len=*), intent(in) :: base
        type(value_type) :: e
        e = make_symbol_ref(intern(modifier_prefix(bits)//base))
    end function apply_modifiers

    !> Symbol event `v' with its modifier prefixes in canonical order, so
    !> `M-C-x' and `C-M-x' are the same event.  Port of
    !> keyboard.c:reorder_modifiers.
    function reorder_modifiers(v) result(e)
        type(value_type), intent(in) :: v
        type(value_type) :: e
        character(len=:), allocatable :: base
        integer(int64) :: bits
        e = v
        call parse_modifiers(v, bits, base)
        if (bits == 0) return
        if (modifier_prefix(bits)//base /= symbols(xsymbol(v))%name) then
            e = apply_modifiers(bits, base)
        end if
    end function reorder_modifiers

    !> Event `c' in the form keymaps store: high bits of characters
    !> cleared, symbol modifiers reordered.
    function normalize_event(c) result(e)
        type(value_type), intent(in) :: c
        type(value_type) :: e
        select case (vtag(c))
        case (V_INT)
            e = make_fixnum(iand(xfixnum(c), ior(M_META, M_META - 1)))
        case (V_SYMBOL)
            e = reorder_modifiers(c)
        case default
            e = c
        end select
    end function normalize_event

    ! ------------------------------------------------------------------
    ! Lookup
    ! ------------------------------------------------------------------

    !> Record binding `val' found during a scan.  The "If we found a
    !> binding" part of keymap.c:access_keymap_1.  `stop' when it shadows
    !> everything after it.
    subroutine take_binding(st, val_in, stop)
        type(access_state), intent(inout) :: st
        type(value_type), intent(in) :: val_in
        logical, intent(out) :: stop
        type(value_type) :: val
        type(value_type) :: parts(1)
        stop = .false.
        val = val_in
        ! A t binding is like an explicit nil.
        if (is_t(val)) val = nil()
        if (.not. keymapp(val)) then
            if (.not. st%found .or. nilp(st%retval)) then
                st%retval = val
                st%found = .true.
            end if
            if (.not. nilp(val)) stop = .true.
        else if (.not. st%found .or. nilp(st%retval)) then
            st%retval = val
            st%found = .true.
        else
            parts(1) = val
            call add_parts(st, parts)
        end if
    end subroutine take_binding

    !> Append `parts' to the composite being built, starting one if needed.
    subroutine add_parts(st, parts)
        type(access_state), intent(inout) :: st
        type(value_type), intent(in) :: parts(:)
        type(value_type), allocatable :: elems(:), all(:)
        integer :: n
        if (st%built) then
            call list_elements(st%retval, elems)
        else
            allocate (elems(1))
            elems(1) = st%retval
        end if
        n = size(elems)
        allocate (all(n + size(parts)))
        all(1:n) = elems
        all(n + 1:) = parts
        st%retval = make_list(all)
        st%built = .true.
    end subroutine add_parts

    !> Reached the start of an inherited part, whose bindings are `tail'.
    !> The `EQ (binding, Qkeymap)' case of keymap.c:access_keymap_1.
    recursive subroutine at_parent(st, tail, idx, noinherit, stop)
        type(access_state), intent(inout) :: st
        type(value_type), intent(in) :: tail, idx
        logical, intent(in) :: noinherit
        logical, intent(out) :: stop
        type(value_type) :: entry
        type(value_type), allocatable :: members(:), parts(:)
        logical :: found
        stop = .true.
        if (noinherit) return
        if (st%found) then
            if (nilp(st%retval)) return
            ! A prefix keymap was found: compose it with the parent's.
            call access_1(tail, idx, st%t_ok, .false., entry, found)
            if (found .and. keymapp(entry)) then
                if (vtag(entry) == V_LIST) then
                    call list_elements(entry, members)
                else
                    allocate (members(1))
                    members(1) = entry
                end if
                allocate (parts(size(members) + 1))
                parts(1) = keymap_marker()
                parts(2:) = members
                call add_parts(st, parts)
            end if
            return
        end if
        stop = .false.
    end subroutine at_parent

    !> Look up event `idx_in' in `map' (a keymap or composite).  `found'
    !> is false for Qunbound.  Port of keymap.c:access_keymap_1.
    recursive subroutine access_1(map, idx_in, t_ok, noinherit, val, found)
        type(value_type), intent(in) :: map, idx_in
        logical, intent(in) :: t_ok, noinherit
        type(value_type), intent(out) :: val
        logical, intent(out) :: found
        type(access_state) :: st
        type(value_type) :: idx, m, meta_binding, b, tail
        type(value_type), allocatable :: elems(:)
        type(status_type) :: status
        integer(int64) :: i, j, k, c
        logical :: f, stop, matched
        found = .false.
        st%t_ok = t_ok
        idx = normalize_event(idx_in)
        m = map
        ! The meta -> ESC mapping.
        if (fixnump(idx)) then
            c = xfixnum(idx)
            if (iand(c, M_META) /= 0) then
                call access_1(map, make_fixnum(META_PREFIX_CHAR), t_ok, &
                              noinherit, meta_binding, f)
                if (f .and. keymapp(meta_binding)) then
                    m = meta_binding
                    idx = make_fixnum(iand(c, not(M_META)))
                else if (t_ok) then
                    ! Only a default binding can apply.
                    idx = make_symbol_ref(SYM_T)
                else
                    found = f .and. nilp(meta_binding)
                    return
                end if
            end if
        end if
        select case (vtag(m))
        case (V_KEYMAP)
            call table%resolve(xhandle(m), i, status)
            if (status%ok()) then
                scan: do
                    matched = .false.
                    do j = keymaps(i)%n, 1, -1
                        if (value_eq(keymaps(i)%events(j), idx)) then
                            matched = .true.
                            call take_binding(st, keymaps(i)%bindings(j), stop)
                            if (stop) exit scan
                        else if (st%t_ok .and. is_t(keymaps(i)%events(j))) then
                            st%t_binding = keymaps(i)%bindings(j)
                            st%t_found = .true.
                            st%t_ok = .false.
                        end if
                    end do
                    ! The char-table part: plain characters only.
                    if (.not. matched .and. fixnump(idx)) then
                        c = xfixnum(idx)
                        if (iand(c, M_ALL) == 0) then
                            do j = keymaps(i)%nranges, 1, -1
                                if (c < keymaps(i)%range_from(j) .or. &
                                    c > keymaps(i)%range_to(j)) cycle
                                ! nil in the char-table: no binding here.
                                if (nilp(keymaps(i)%range_bindings(j))) exit
                                call take_binding(st, keymaps(i)%range_bindings(j), stop)
                                if (stop) exit scan
                                exit
                            end do
                        end if
                    end if
                    if (.not. table%is_live(keymaps(i)%parent)) exit scan
                    tail = make_keymap_ref(keymaps(i)%parent)
                    call at_parent(st, tail, idx, noinherit, stop)
                    if (stop) exit scan
                    i = keymaps(i)%parent%index
                end do scan
            end if
        case (V_LIST)
            call list_elements(m, elems)
            do k = 1, size(elems, kind=int64)
                if (is_marker(elems(k))) then
                    tail = list_tail(m, k)
                    call at_parent(st, tail, idx, noinherit, stop)
                    if (stop) exit
                else if (keymapp(elems(k))) then
                    call access_1(elems(k), idx, st%t_ok, noinherit, b, f)
                    if (f) then
                        call take_binding(st, b, stop)
                        if (stop) exit
                    end if
                end if
            end do
        case default
            ! Nothing to do for other values.
        end select
        if (st%found) then
            val = st%retval
            found = .true.
        else if (st%t_found) then
            val = st%t_binding
            if (is_t(val)) val = nil()
            found = .true.
        end if
    end subroutine access_1

    !> Binding of the single event `idx' in `map'; nil if none.  With
    !> `t_ok', bindings of t are defaults.  With `noinherit', inherited
    !> bindings are ignored.  Port of keymap.c:access_keymap.
    function access_keymap(map, idx, t_ok, noinherit) result(val)
        type(value_type), intent(in) :: map, idx
        logical, intent(in) :: t_ok, noinherit
        type(value_type) :: val
        logical :: found
        call access_1(map, idx, t_ok, noinherit, val, found)
    end function access_keymap

    !> Binding of key sequence `key' in `map' (a keymap or composite).
    !> `val' is nil if undefined, or the fixnum N if the first N events
    !> already reach a non-prefix binding ("too long").  Port of
    !> keymap.c:Flookup_key (lookup_key_1; menu-bar case folding is not
    !> ported).
    subroutine lookup_key(map, key, val, status, accept_default)
        type(value_type), intent(in) :: map
        type(value_type), intent(in) :: key(:)
        type(value_type), intent(out) :: val
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: accept_default
        type(value_type) :: keymap, cmd
        logical :: t_ok
        integer(int64) :: k, n
        t_ok = .false.
        if (present(accept_default)) t_ok = accept_default
        if (.not. (nilp(map) .or. keymapp(map))) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "keymapp")
            return
        end if
        n = size(key, kind=int64)
        if (n == 0) then
            val = map
            return
        end if
        keymap = map
        do k = 1, n
            cmd = access_keymap(keymap, key(k), t_ok, .false.)
            if (k == n) then
                val = cmd
                return
            end if
            if (.not. keymapp(cmd)) then
                val = make_fixnum(k)
                return
            end if
            keymap = cmd
        end do
    end subroutine lookup_key

    ! ------------------------------------------------------------------
    ! Definition
    ! ------------------------------------------------------------------

    !> Append entry (ev, def) to keymap slot `i'.
    subroutine append_entry(i, ev, def)
        integer(int64), intent(in) :: i
        type(value_type), intent(in) :: ev, def
        type(value_type), allocatable :: grown(:)
        integer(int64) :: cap
        if (.not. allocated(keymaps(i)%events)) then
            allocate (keymaps(i)%events(8), keymaps(i)%bindings(8))
        else if (keymaps(i)%n == size(keymaps(i)%events, kind=int64)) then
            cap = 2*keymaps(i)%n
            allocate (grown(cap))
            grown(1:keymaps(i)%n) = keymaps(i)%events
            call move_alloc(grown, keymaps(i)%events)
            allocate (grown(cap))
            grown(1:keymaps(i)%n) = keymaps(i)%bindings
            call move_alloc(grown, keymaps(i)%bindings)
        end if
        keymaps(i)%n = keymaps(i)%n + 1
        keymaps(i)%events(keymaps(i)%n) = ev
        keymaps(i)%bindings(keymaps(i)%n) = def
    end subroutine append_entry

    !> Bind event `c' to `def' in keymap slot `i', or with `remove' delete
    !> its entry.  Port of keymap.c:store_in_keymap.
    subroutine store_in_keymap(i, c, def, remove, status)
        integer(int64), intent(in) :: i
        type(value_type), intent(in) :: c, def
        logical, intent(in) :: remove
        type(status_type), intent(out) :: status
        type(value_type) :: idx
        integer(int64) :: j, n
        if (is_marker(c)) then
            call set_error(status, FM_GENERIC_ERROR, &
                           format_message("`keymap' is reserved for embedded parent maps"))
            return
        end if
        idx = normalize_event(c)
        n = keymaps(i)%n
        ! Removing a character from a full keymap's char-table makes it nil
        ! there, even inside a range.
        if (remove .and. keymaps(i)%full .and. char_table_event_p(idx)) then
            do j = keymaps(i)%nranges, 1, -1
                if (xfixnum(idx) < keymaps(i)%range_from(j) .or. xfixnum(idx) > keymaps(i)%range_to(j)) cycle
                call add_range(i, xfixnum(idx), xfixnum(idx), nil())
                exit
            end do
        end if
        do j = 1, n
            if (.not. value_eq(keymaps(i)%events(j), idx)) cycle
            if (remove) then
                keymaps(i)%events(j:n - 1) = keymaps(i)%events(j + 1:n)
                keymaps(i)%bindings(j:n - 1) = keymaps(i)%bindings(j + 1:n)
                keymaps(i)%events(n) = nil()
                keymaps(i)%bindings(n) = nil()
                keymaps(i)%n = n - 1
            else
                keymaps(i)%bindings(j) = def
            end if
            return
        end do
        if (.not. remove) call append_entry(i, idx, def)
    end subroutine store_in_keymap

    !> Bind characters `from'..`to' (without modifiers) to `def' in `km',
    !> replacing their single-character entries.  Port of the
    !> (FROM . TO) case of keymap.c:store_in_keymap (Fset_char_table_range
    !> on the keymap's char-table).  A nil `def' is recorded as t, as
    !> upstream (nil in a char-table means no binding).
    subroutine define_key_range(km, from, to, def, status)
        type(handle_type), intent(in) :: km
        integer(int64), intent(in) :: from, to
        type(value_type), intent(in) :: def
        type(status_type), intent(out) :: status
        integer(int64) :: i, j, c, n
        call resolve_keymap(km, i, status)
        if (.not. status%ok()) return
        if (from < 0 .or. to > MAX_CHAR .or. from > to) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid character range")
            return
        end if
        ! Drop entries for characters in the range.
        n = 0
        do j = 1, keymaps(i)%n
            if (fixnump(keymaps(i)%events(j))) then
                c = xfixnum(keymaps(i)%events(j))
                if (c >= from .and. c <= to) cycle
            end if
            n = n + 1
            if (n /= j) then
                keymaps(i)%events(n) = keymaps(i)%events(j)
                keymaps(i)%bindings(n) = keymaps(i)%bindings(j)
            end if
        end do
        do j = n + 1, keymaps(i)%n
            keymaps(i)%events(j) = nil()
            keymaps(i)%bindings(j) = nil()
        end do
        keymaps(i)%n = n
        ! nil has a special meaning for char-tables, so we use something
        ! else to record an explicitly unbound entry.
        if (nilp(def)) then
            call add_range(i, from, to, make_symbol_ref(SYM_T))
        else
            call add_range(i, from, to, def)
        end if
    end subroutine define_key_range

    !> Add the range `from'..`to' with binding `def' (nil: none, the
    !> char-table's nil) to keymap slot `i'.
    subroutine add_range(i, from, to, def)
        integer(int64), intent(in) :: i, from, to
        type(value_type), intent(in) :: def
        integer(int64), allocatable :: grown_i(:)
        type(value_type), allocatable :: grown_v(:)
        integer(int64) :: cap
        if (.not. allocated(keymaps(i)%range_from)) then
            allocate (keymaps(i)%range_from(4), keymaps(i)%range_to(4), &
                      keymaps(i)%range_bindings(4))
        else if (keymaps(i)%nranges == size(keymaps(i)%range_from, kind=int64)) then
            cap = 2*keymaps(i)%nranges
            allocate (grown_i(cap))
            grown_i(1:keymaps(i)%nranges) = keymaps(i)%range_from
            call move_alloc(grown_i, keymaps(i)%range_from)
            allocate (grown_i(cap))
            grown_i(1:keymaps(i)%nranges) = keymaps(i)%range_to
            call move_alloc(grown_i, keymaps(i)%range_to)
            allocate (grown_v(cap))
            grown_v(1:keymaps(i)%nranges) = keymaps(i)%range_bindings
            call move_alloc(grown_v, keymaps(i)%range_bindings)
        end if
        keymaps(i)%nranges = keymaps(i)%nranges + 1
        keymaps(i)%range_from(keymaps(i)%nranges) = from
        keymaps(i)%range_to(keymaps(i)%nranges) = to
        keymaps(i)%range_bindings(keymaps(i)%nranges) = def
    end subroutine add_range

    !> The keymap to define into when the prefix binding is `cmd': a
    !> keymap, or the first member of a composite (as store_in_keymap
    !> descends into a leading sub-keymap).
    recursive subroutine prefix_target(cmd, km, ok)
        type(value_type), intent(in) :: cmd
        type(handle_type), intent(out) :: km
        logical, intent(out) :: ok
        type(value_type), allocatable :: elems(:)
        integer(int64) :: k
        ok = .false.
        km = null_handle()
        if (.not. keymapp(cmd)) return
        if (vtag(cmd) == V_KEYMAP) then
            km = xhandle(cmd)
            ok = .true.
            return
        end if
        call list_elements(cmd, elems)
        do k = 1, size(elems, kind=int64)
            if (is_marker(elems(k))) cycle
            call prefix_target(elems(k), km, ok)
            return
        end do
    end subroutine prefix_target

    !> In keymap `km', bind key sequence `key' to `def'; with `remove',
    !> delete the binding instead (so an inherited one shows through).
    !> Missing prefix keys get new sparse keymaps.  Port of
    !> keymap.c:Fdefine_key (with define_as_prefix).
    subroutine define_key(km, key, def, status, remove)
        type(handle_type), intent(in) :: km
        type(value_type), intent(in) :: key(:), def
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: remove
        type(handle_type) :: target, prefix
        type(value_type) :: c, cmd
        integer(int64) :: i, pos, n
        logical :: metized, rm, ok
        character(len=:), allocatable :: trailing
        rm = .false.
        if (present(remove)) rm = remove
        call resolve_keymap(km, i, status)
        if (.not. status%ok()) return
        n = size(key, kind=int64)
        if (n == 0) return
        target = km
        metized = .false.
        pos = 0
        do
            c = key(pos + 1)
            if (.not. (fixnump(c) .or. symbolp(c))) then
                call set_error(status, FM_GENERIC_ERROR, &
                               "Key sequence contains invalid event")
                return
            end if
            if (fixnump(c)) then
                if (iand(xfixnum(c), M_META) /= 0 .and. .not. metized) then
                    c = make_fixnum(META_PREFIX_CHAR)
                    metized = .true.
                else
                    c = make_fixnum(iand(xfixnum(c), not(M_META)))
                    metized = .false.
                    pos = pos + 1
                end if
            else
                metized = .false.
                pos = pos + 1
            end if
            i = target%index
            if (pos == n) then
                call store_in_keymap(i, c, def, rm, status)
                return
            end if
            cmd = access_keymap(make_keymap_ref(target), c, .false., .true.)
            if (nilp(cmd)) then
                ! keymap.c:define_as_prefix
                call make_sparse_keymap(prefix)
                keymaps(prefix%index)%owner = target
                cmd = make_keymap_ref(prefix)
                call store_in_keymap(i, c, cmd, .false., status)
                if (.not. status%ok()) return
            end if
            call prefix_target(cmd, target, ok)
            if (.not. ok) then
                trailing = ""
                if (metized) then
                    trailing = " ESC"
                    if (pos == 0) trailing = "ESC"
                end if
                call set_error(status, FM_GENERIC_ERROR, "Key sequence "// &
                               key_description(key)// &
                               " starts with non-prefix key "// &
                               key_description(key(1:pos))//trailing)
                return
            end if
        end do
    end subroutine define_key

    !> A copy of `km' whose sub-keymaps (bindings that are keymaps) are
    !> copied too; the parent is shared.  Port of keymap.c:Fcopy_keymap.
    subroutine copy_keymap(km, copy, status)
        type(handle_type), intent(in) :: km
        type(handle_type), intent(out) :: copy
        type(status_type), intent(out) :: status
        call copy_keymap_1(km, 0, copy, status)
    end subroutine copy_keymap

    !> Port of keymap.c:copy_keymap_1.
    recursive subroutine copy_keymap_1(km, depth, copy, status)
        type(handle_type), intent(in) :: km
        integer, intent(in) :: depth
        type(handle_type), intent(out) :: copy
        type(status_type), intent(out) :: status
        type(handle_type) :: sub
        type(value_type) :: b, ev, prompt
        type(status_type) :: ignored
        integer(int64) :: i, k
        logical :: full
        copy = null_handle()
        if (depth > MAX_COPY_DEPTH) then
            call set_error(status, FM_GENERIC_ERROR, &
                           "Possible infinite recursion when copying keymap")
            return
        end if
        call resolve_keymap(km, i, status)
        if (.not. status%ok()) return
        ! Copy fields first: new_keymap may reallocate the registry.
        prompt = keymaps(i)%prompt
        full = keymaps(i)%full
        call new_keymap(copy, full, prompt)
        keymaps(copy%index)%parent = keymaps(i)%parent
        do k = 1, keymaps(i)%n
            ev = keymaps(i)%events(k)
            b = keymaps(i)%bindings(k)
            if (vtag(b) == V_KEYMAP) then
                if (table%is_live(xhandle(b))) then
                    call copy_keymap_1(xhandle(b), depth + 1, sub, status)
                    if (.not. status%ok()) then
                        call free_keymap(copy, ignored)
                        copy = null_handle()
                        return
                    end if
                    keymaps(sub%index)%owner = copy
                    b = make_keymap_ref(sub)
                end if
            end if
            call append_entry(copy%index, ev, b)
        end do
        if (keymaps(i)%nranges > 0) then
            keymaps(copy%index)%range_from = keymaps(i)%range_from
            keymaps(copy%index)%range_to = keymaps(i)%range_to
            keymaps(copy%index)%range_bindings = keymaps(i)%range_bindings
            keymaps(copy%index)%nranges = keymaps(i)%nranges
        end if
    end subroutine copy_keymap_1

    ! ------------------------------------------------------------------
    ! Active maps
    ! ------------------------------------------------------------------

    !> The global keymap, made (full) on first use.
    !> Port of keymap.c:Fcurrent_global_map.
    function current_global_map() result(km)
        type(handle_type) :: km
        if (.not. table%is_live(global_map)) call make_keymap(global_map)
        km = global_map
    end function current_global_map

    !> Port of keymap.c:Fuse_global_map.
    subroutine use_global_map(km, status)
        type(handle_type), intent(in) :: km
        type(status_type), intent(out) :: status
        integer(int64) :: i
        call resolve_keymap(km, i, status)
        if (status%ok()) global_map = km
    end subroutine use_global_map

    !> The current buffer's local keymap, or the null handle.
    !> Port of keymap.c:Fcurrent_local_map.
    function current_local_map() result(km)
        type(handle_type) :: km
        km = null_handle()
        if (cur == 0) return
        if (table%is_live(buffers(cur)%keymap)) km = buffers(cur)%keymap
    end function current_local_map

    !> Make `km' (or the null handle for none) the current buffer's local
    !> keymap.  Port of keymap.c:Fuse_local_map.
    subroutine use_local_map(km, status)
        type(handle_type), intent(in) :: km
        type(status_type), intent(out) :: status
        integer(int64) :: i
        if (km%index /= 0) then
            call resolve_keymap(km, i, status)
            if (.not. status%ok()) return
        end if
        if (cur == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No current buffer")
            return
        end if
        buffers(cur)%keymap = km
    end subroutine use_local_map

    !> Make keymap MAP active while variable VARIABLE is non-nil, ahead
    !> of the minor-mode maps added earlier (replacing VARIABLE's earlier
    !> map).  Port of the keymap part of subr.el:add-minor-mode.
    subroutine add_minor_mode_map(variable, map)
        character(len=*), intent(in) :: variable
        type(handle_type), intent(in) :: map
        integer(int64), allocatable :: vars(:)
        type(handle_type), allocatable :: maps(:)
        integer(int64) :: id
        integer :: i, n
        id = intern(variable)
        n = n_minor_mode_maps
        do i = 1, n
            if (minor_mode_vars(i) == id) then
                minor_mode_maps(i) = map
                return
            end if
        end do
        allocate (vars(n + 1), maps(n + 1))
        vars(1) = id
        maps(1) = map
        if (n > 0) then
            vars(2:) = minor_mode_vars(1:n)
            maps(2:) = minor_mode_maps(1:n)
        end if
        call move_alloc(vars, minor_mode_vars)
        call move_alloc(maps, minor_mode_maps)
        n_minor_mode_maps = n + 1
    end subroutine add_minor_mode_map

    !> The variable of an enabled minor mode whose keymap (in
    !> minor-mode-map-alist) is `map'; "" if none.
    function active_minor_mode_of_map(map) result(variable)
        type(handle_type), intent(in) :: map
        character(len=:), allocatable :: variable
        type(value_type) :: v
        type(status_type) :: st
        integer :: i
        variable = ""
        do i = 1, n_minor_mode_maps
            if (.not. same_handle(minor_mode_maps(i), map)) cycle
            call symbol_value(make_symbol_ref(minor_mode_vars(i)), v, st)
            if (.not. st%ok()) cycle
            if (nilp(v)) cycle
            variable = symbols(minor_mode_vars(i))%name
            return
        end do
    end function active_minor_mode_of_map

    !> The keymaps of the enabled minor modes, in the order the active
    !> maps have them, and their variables.  Port of
    !> keymap.c:current_minor_maps (minor-mode-map-alist only).
    subroutine current_minor_maps(modes, maps)
        character(len=48), allocatable, intent(out) :: modes(:)
        type(handle_type), allocatable, intent(out) :: maps(:)
        type(value_type) :: v
        type(status_type) :: st
        integer :: i, n
        allocate (modes(n_minor_mode_maps), maps(n_minor_mode_maps))
        n = 0
        do i = 1, n_minor_mode_maps
            if (.not. table%is_live(minor_mode_maps(i))) cycle
            call symbol_value(make_symbol_ref(minor_mode_vars(i)), v, st)
            if (.not. st%ok()) cycle
            if (nilp(v)) cycle
            n = n + 1
            modes(n) = symbols(minor_mode_vars(i))%name
            maps(n) = minor_mode_maps(i)
        end do
        modes = modes(1:n)
        maps = maps(1:n)
    end subroutine current_minor_maps

    !> The bindings of keymap `km' and its parents, in the order
    !> map_keymap visits them: for each map, a full keymap's char-table
    !> first, as runs of characters bound alike (an event FROM, or the
    !> list (FROM TO) for a run), then the other entries, newest first.
    !> Port of keymap.c:map_keymap (with map_keymap_internal and
    !> map_keymap_char_table_item).
    subroutine map_keymap_items(km, events, bindings)
        type(handle_type), intent(in) :: km
        type(value_type), allocatable, intent(out) :: events(:), bindings(:)
        type(handle_type) :: map
        integer(int64), allocatable :: run_from(:), run_to(:)
        type(value_type), allocatable :: run_val(:)
        integer, allocatable :: order(:)
        type(value_type) :: ev
        integer :: m, n
        allocate (events(0), bindings(0))
        map = km
        do while (table%is_live(map))
            associate (mp => keymaps(map%index))
                if (mp%full) then
                    call char_table_runs(mp, run_from, run_to, run_val)
                    do m = 1, size(run_from)
                        if (run_from(m) == run_to(m)) then
                            ev = make_fixnum(run_from(m))
                        else
                            ev = list(make_fixnum(run_from(m)), make_fixnum(run_to(m)))
                        end if
                        call append_value(events, ev)
                        call append_value(bindings, run_val(m))
                    end do
                end if
                call entry_order(mp, order)
                do m = 1, size(order)
                    n = order(m)
                    call append_value(events, mp%events(n))
                    call append_value(bindings, mp%bindings(n))
                end do
                map = mp%parent
            end associate
        end do
    end subroutine map_keymap_items

    !> Find all keymaps accessible via prefix characters from KEYMAP.
    !> Returns elements of the form (KEYS . MAP), where the sequence KEYS
    !> starting from KEYMAP gets you to MAP.  These elements are ordered
    !> so that the KEYS increase in length.  The first element is
    !> ([] . KEYMAP).  An optional argument PREFIX, if present, should be a
    !> key sequence; then the value includes only maps for prefixes that
    !> start with PREFIX.  Port of keymap.c:Faccessible_keymaps (with
    !> accessible_keymaps_1).
    subroutine accessible_keymaps(keymap, maps, prefix)
        type(handle_type), intent(in) :: keymap
        type(key_list_type), allocatable, intent(out) :: maps(:)
        type(value_type), intent(in), optional :: prefix(:)
        type(key_list_type), allocatable :: trimmed(:)
        type(value_type), allocatable :: events(:), bindings(:), thisseq(:), seq(:)
        type(value_type) :: tem
        type(status_type) :: st
        type(handle_type) :: cmd
        integer :: n, k, j, prefixlen, last
        logical :: is_metized
        allocate (maps(8))
        n = 0
        prefixlen = 0
        if (present(prefix)) prefixlen = size(prefix)
        if (prefixlen > 0) then
            ! Start with the keymap (if any) for that prefix.
            call lookup_key(make_keymap_ref(keymap), prefix, tem, st, accept_default=.true.)
            if (.not. keymap_ref_p(tem)) then
                deallocate (maps)
                allocate (maps(0))
                return
            end if
            call push(maps, n, prefix, xhandle(tem))
        else
            allocate (seq(0))
            call push(maps, n, seq, keymap)
        end if
        k = 0
        do while (k < n)
            k = k + 1
            thisseq = maps(k)%keys
            last = size(thisseq)
            ! Does the current sequence end in the meta-prefix-char?
            is_metized = .false.
            if (last >= 1 .and. last > prefixlen) then
                if (fixnump(thisseq(last))) is_metized = xfixnum(thisseq(last)) == META_PREFIX_CHAR
            end if
            call map_keymap_items(maps(k)%map, events, bindings)
            do j = 1, size(events)
                if (.not. keymap_ref_p(bindings(j))) cycle
                cmd = xhandle(bindings(j))
                if (cycle_seen(cmd, thisseq)) cycle
                if (is_metized .and. fixnump(events(j))) then
                    ! Turn it into a meta-ized keystroke, right after this
                    ! entry.
                    seq = thisseq
                    seq(last) = make_fixnum(ior(xfixnum(events(j)), M_META))
                    call insert_after(k, seq, cmd)
                else
                    seq = [thisseq, events(j)]
                    call push(maps, n, seq, cmd)
                end if
            end do
        end do
        allocate (trimmed(n))
        do k = 1, n
            trimmed(k) = maps(k)
        end do
        call move_alloc(trimmed, maps)
    contains

        !> Whether `map' is in the list with a prefix of `key' (a cycle).
        logical function cycle_seen(map, key)
            type(handle_type), intent(in) :: map
            type(value_type), intent(in) :: key(:)
            integer :: i, lim
            cycle_seen = .false.
            do i = 1, n
                if (.not. same_handle(maps(i)%map, map)) cycle
                lim = size(maps(i)%keys)
                if (lim > size(key)) cycle
                if (same_keys(maps(i)%keys, key(1:lim))) then
                    cycle_seen = .true.
                    return
                end if
            end do
        end function cycle_seen

        !> Insert (`keys', `map') after element `at' of the list.
        subroutine insert_after(at, keys, map)
            integer, intent(in) :: at
            type(value_type), intent(in) :: keys(:)
            type(handle_type), intent(in) :: map
            integer :: i
            call push(maps, n, keys, map)
            do i = n, at + 2, -1
                maps(i) = maps(i - 1)
            end do
            maps(at + 1)%keys = keys
            maps(at + 1)%map = map
        end subroutine insert_after
    end subroutine accessible_keymaps

    !> The active keymaps, highest precedence first, as one composite:
    !> overriding-terminal-local-map, the minor-mode maps, the local map,
    !> the global map.  Port of keymap.c:Fcurrent_active_maps without
    !> overriding-local-map and text-property maps.
    function current_active_maps() result(maps)
        type(value_type) :: maps
        type(value_type), allocatable :: elems(:)
        type(value_type) :: v
        type(handle_type) :: local
        type(status_type) :: st
        integer :: n, i
        allocate (elems(4 + n_minor_mode_maps))
        n = 0
        if (table%is_live(overriding_terminal_local_map)) then
            n = n + 1
            elems(n) = make_keymap_ref(overriding_terminal_local_map)
        end if
        if (table%is_live(pushed_terminal_local_map)) then
            n = n + 1
            elems(n) = make_keymap_ref(pushed_terminal_local_map)
        end if
        do i = 1, n_minor_mode_maps
            if (.not. table%is_live(minor_mode_maps(i))) cycle
            call symbol_value(make_symbol_ref(minor_mode_vars(i)), v, st)
            if (.not. st%ok()) cycle
            if (nilp(v)) cycle
            n = n + 1
            elems(n) = make_keymap_ref(minor_mode_maps(i))
        end do
        local = current_local_map()
        if (local%index /= 0) then
            n = n + 1
            elems(n) = make_keymap_ref(local)
        end if
        n = n + 1
        elems(n) = make_keymap_ref(current_global_map())
        if (n == 1) then
            maps = elems(1)
        else
            maps = make_list(elems(1:n))
        end if
    end function current_active_maps

    !> Binding of `key' in the active maps (local, then global); nil if
    !> none.  If the binding is a command, its remapping (a binding of
    !> [remap COMMAND]) is returned instead, unless NO_REMAP is true.
    !> Port of keymap.c:Fkey_binding (no POSITION).
    recursive subroutine key_binding(key, val, status, accept_default, no_remap)
        type(value_type), intent(in) :: key(:)
        type(value_type), intent(out) :: val
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: accept_default, no_remap
        type(value_type) :: maps, remapped
        logical :: remap
        maps = current_active_maps()
        call lookup_key(maps, key, val, status, accept_default)
        if (fixnump(val)) val = nil()
        remap = .true.
        if (present(no_remap)) remap = .not. no_remap
        ! If the result of the ordinary keymap lookup is an interactive
        ! command, look for a key binding (ie. remapping) for that command.
        if (remap .and. symbolp(val) .and. .not. nilp(val)) then
            call command_remapping(val, remapped)
            if (.not. nilp(remapped)) val = remapped
        end if
    end subroutine key_binding

    !> Return the remapping for command COMMAND.
    !> Returns nil if COMMAND is not remapped (or not a symbol).
    !>
    !> If the optional argument KEYMAPS is present, it should be a keymap
    !> or list of keymaps to search for command remapping.  Otherwise,
    !> search for the remapping in all currently active keymaps.  Port of
    !> keymap.c:Fcommand_remapping (no POSITION).
    recursive subroutine command_remapping(command, remapped, keymaps)
        type(value_type), intent(in) :: command
        type(value_type), intent(out) :: remapped
        type(value_type), intent(in), optional :: keymaps
        type(value_type) :: key(2)
        type(status_type) :: st
        remapped = nil()
        if (.not. symbolp(command)) return
        key(1) = make_symbol_ref(intern("remap"))
        key(2) = command
        if (present(keymaps)) then
            call lookup_key(keymaps, key, remapped, st)
        else
            call key_binding(key, remapped, st, no_remap=.true.)
        end if
        if (.not. st%ok() .or. fixnump(remapped)) remapped = nil()
    end subroutine command_remapping

    ! ------------------------------------------------------------------
    ! Descriptions
    ! ------------------------------------------------------------------

    function int_text(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function int_text

    !> Description of character event `ch', such as "C-x" or "M-<".
    !> Port of keymap.c:push_key_description.
    function push_key_description(ch) result(s)
        integer(int64), intent(in) :: ch
        character(len=:), allocatable :: s
        integer(int64) :: c, c2
        logical :: tab_as_ci
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        integer(int32) :: nbytes
        type(status_type) :: status
        c = iand(ch, ior(M_META, M_META - 1))
        c2 = iand(c, not(M_ALL))
        if (c2 < 0 .or. c2 > MAX_CHAR) then
            s = "["//int_text(c)//"]"
            return
        end if
        tab_as_ci = c2 == 9 .and. iand(c, M_META) /= 0
        s = ""
        if (iand(c, M_ALT) /= 0) then
            s = s//"A-"
            c = c - M_ALT
        end if
        if (iand(c, M_CTL) /= 0 .or. (c2 < 32 .and. c2 /= 27 .and. c2 /= 9 &
                                      .and. c2 /= 13) .or. tab_as_ci) then
            s = s//"C-"
            c = iand(c, not(M_CTL))
        end if
        if (iand(c, M_HYPER) /= 0) then
            s = s//"H-"
            c = c - M_HYPER
        end if
        if (iand(c, M_META) /= 0) then
            s = s//"M-"
            c = c - M_META
        end if
        if (iand(c, M_SHIFT) /= 0) then
            s = s//"S-"
            c = c - M_SHIFT
        end if
        if (iand(c, M_SUPER) /= 0) then
            s = s//"s-"
            c = c - M_SUPER
        end if
        if (c < 32) then
            if (c == 27) then
                s = s//"ESC"
            else if (tab_as_ci) then
                s = s//"i"
            else if (c == 9) then
                s = s//"TAB"
            else if (c == 13) then
                s = s//"RET"
            else if (c > 0 .and. c <= 26) then
                s = s//achar(c + 96)
            else
                s = s//achar(c + 64)
            end if
        else if (c == 127) then
            s = s//"DEL"
        else if (c == 32) then
            s = s//"SPC"
        else if (c < 128) then
            s = s//achar(c)
        else
            call char_string(int(c, int32), buf, nbytes, status)
            s = s//buf(1:nbytes)
        end if
    end function push_key_description

    !> Description of one event: "C-x", "<f1>", "C-s-<home>".  With
    !> `no_angles', symbols are given by name only.  Port of
    !> keymap.c:Fsingle_key_description; a character interval from a
    !> char-table, the list (FROM TO) here, is "FROM..TO"; other values
    !> give "".
    recursive function single_key_description(key, no_angles) result(s)
        type(value_type), intent(in) :: key
        logical, intent(in), optional :: no_angles
        character(len=:), allocatable :: s
        type(value_type), allocatable :: parts(:)
        character(len=:), allocatable :: name
        integer :: i, n
        logical :: bare
        bare = .false.
        if (present(no_angles)) bare = no_angles
        select case (vtag(key))
        case (V_INT)
            s = push_key_description(xfixnum(key))
        case (V_SYMBOL)
            name = symbols(xsymbol(key))%name
            if (bare) then
                s = name
                return
            end if
            ! Find the modifier prefix, like "C-M-".
            n = len(name)
            i = 1
            do while (i <= n - 3)
                if (name(i + 1:i + 1) /= "-") exit
                if (index("CMSsHA", name(i:i)) == 0) exit
                i = i + 2
            end do
            s = name(1:i - 1)//"<"//name(i:)//">"
        case (V_STRING)
            s = xstring(key)
        case (V_LIST)
            call list_elements(key, parts)
            if (size(parts) == 2) then
                if (fixnump(parts(1)) .and. fixnump(parts(2))) then
                    ! An interval from a map-char-table.
                    s = single_key_description(parts(1), no_angles)//".."//single_key_description(parts(2), no_angles)
                    return
                end if
            end if
            ! EVENT_HEAD
            s = ""
            if (size(parts) > 0) s = single_key_description(parts(1), no_angles)
        case default
            s = ""
        end select
    end function single_key_description

    !> Description of key sequence `keys', preceded by `prefix', such as
    !> "C-x <right>"; ESC followed by a character reads as M-.  Port of
    !> keymap.c:Fkey_description.
    function key_description(keys, prefix) result(s)
        type(value_type), intent(in) :: keys(:)
        type(value_type), intent(in), optional :: prefix(:)
        character(len=:), allocatable :: s
        type(value_type), allocatable :: all(:)
        type(value_type) :: key
        integer :: k, np
        logical :: add_meta
        np = 0
        if (present(prefix)) np = size(prefix)
        allocate (all(np + size(keys)))
        if (np > 0) all(1:np) = prefix
        all(np + 1:) = keys
        s = ""
        add_meta = .false.
        do k = 1, size(all)
            key = all(k)
            if (add_meta) then
                if (.not. fixnump(key)) then
                    call add_word(s, "ESC")
                else if (xfixnum(key) == META_PREFIX_CHAR .or. &
                         iand(xfixnum(key), M_META) /= 0) then
                    call add_word(s, "ESC")
                    if (xfixnum(key) == META_PREFIX_CHAR) cycle
                else
                    key = make_fixnum(ior(xfixnum(key), M_META))
                end if
                add_meta = .false.
            else if (fixnump(key)) then
                if (xfixnum(key) == META_PREFIX_CHAR) then
                    add_meta = .true.
                    cycle
                end if
            end if
            call add_word(s, single_key_description(key))
        end do
        if (add_meta) call add_word(s, "ESC")
    contains

        subroutine add_word(s, word)
            character(len=:), allocatable, intent(inout) :: s
            character(len=*), intent(in) :: word
            if (len(s) > 0) then
                s = s//" "//word
            else
                s = word
            end if
        end subroutine add_word
    end function key_description

    ! ------------------------------------------------------------------
    ! kbd
    ! ------------------------------------------------------------------

    logical function is_space(ch)
        character, intent(in) :: ch
        is_space = ch == " " .or. ch == achar(9) .or. ch == achar(10) .or. &
                   ch == achar(12)
    end function is_space

    !> Code points of UTF-8 string `s'.
    subroutine decode(s, codes)
        character(len=*), intent(in) :: s
        integer(int64), allocatable, intent(out) :: codes(:)
        integer(int64) :: pos, n
        integer(int32) :: ch, length
        allocate (codes(len(s)))
        n = 0
        pos = 1
        do while (pos <= len(s, kind=int64))
            call string_char_and_length(s, pos, ch, length)
            n = n + 1
            codes(n) = ch
            pos = pos + length
        end do
        codes = codes(1:n)
    end subroutine decode

    !> End of a "<as df>" word starting at `wb' in `keys', or 0.
    integer(int64) function angle_end(keys, wb) result(q)
        character(len=*), intent(in) :: keys
        integer(int64), intent(in) :: wb
        character :: ch
        q = 0
        if (keys(wb:wb) /= "<" .or. wb + 1 > len(keys, kind=int64)) return
        ch = keys(wb + 1:wb + 1)
        if (ch == "<" .or. ch == ">" .or. is_space(ch)) return
        do q = wb + 2, len(keys, kind=int64)
            ch = keys(q:q)
            if (ch == ">") return
            if (ch /= " " .and. is_space(ch)) exit
        end do
        q = 0
    end function angle_end

    !> Whether `word' ends in one of the names NUL RET LFD TAB ESC SPC DEL
    !> at a word boundary.
    logical function ends_in_name(word)
        character(len=*), intent(in) :: word
        character(len=3), parameter :: names(7) = &
                                       ["NUL", "RET", "LFD", "TAB", "ESC", "SPC", "DEL"]
        integer :: n
        character :: before
        ends_in_name = .false.
        n = len(word)
        if (n < 3) return
        if (all(names /= word(n - 2:n))) return
        if (n == 3) then
            ends_in_name = .true.
        else
            before = word(n - 3:n - 3)
            ends_in_name = .not. (("A" <= before .and. before <= "Z") .or. &
                                  ("a" <= before .and. before <= "z") .or. &
                                  ("0" <= before .and. before <= "9") .or. &
                                  iachar(before) >= 128)
        end if
    end function ends_in_name

    !> Append `n' copies of `key' to `events(1:nev)'.
    subroutine push_events(events, nev, key, n)
        type(value_type), allocatable, intent(inout) :: events(:)
        integer(int64), intent(inout) :: nev
        type(value_type), intent(in) :: key(:)
        integer(int64), intent(in) :: n
        type(value_type), allocatable :: grown(:)
        integer(int64) :: k, need
        need = nev + n*size(key, kind=int64)
        if (need > size(events, kind=int64)) then
            allocate (grown(max(need, 2*size(events, kind=int64))))
            grown(1:nev) = events(1:nev)
            call move_alloc(grown, events)
        end if
        do k = 1, n
            events(nev + 1:nev + size(key)) = key
            nev = nev + size(key)
        end do
    end subroutine push_events

    !> Fixnum events for `codes'.
    subroutine char_events(codes, key)
        integer(int64), intent(in) :: codes(:)
        type(value_type), allocatable, intent(out) :: key(:)
        integer :: k
        allocate (key(size(codes)))
        do k = 1, size(codes)
            key(k) = make_fixnum(codes(k))
        end do
    end subroutine char_events

    !> The key sequence described by `keys', such as "C-x C-f" or
    !> "C-M-<return>".  Port of keymap.el:key-parse (the kbd parser).
    subroutine key_parse(keys, events, status)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable, intent(out) :: events(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: word, orig, mods
        type(value_type), allocatable :: key(:), trimmed(:)
        integer(int64), allocatable :: codes(:)
        integer(int64) :: pos, n, wb, we, q, times, nev, bits, prefix, k, c
        logical :: is_vector
        allocate (events(8))
        nev = 0
        n = len(keys, kind=int64)
        pos = 1
        words: do
            do while (pos <= n)
                if (.not. is_space(keys(pos:pos))) exit
                pos = pos + 1
            end do
            if (pos > n) exit words
            wb = pos
            we = wb
            do while (we < n)
                if (is_space(keys(we + 1:we + 1))) exit
                we = we + 1
            end do
            q = angle_end(keys, wb)
            if (q > 0) then
                word = keys(wb:q)
                pos = q + 1
            else
                word = keys(wb:we)
                pos = we + 1
            end if
            ! "N*KEY" repeats KEY.
            times = 1
            k = verify(word, "0123456789")
            if (k > 1 .and. k < len(word)) then
                if (word(k:k) == "*") then
                    read (word(1:k - 1), *) times
                    word = word(k + 1:)
                end if
            end if
            if (allocated(key)) deallocate (key)
            if (len(word) >= 5) then
                if (word(1:2) == "<<" .and. word(len(word) - 1:) == ">>") then
                    ! <<command>>: M-x command RET.
                    call decode(word(3:len(word) - 2), codes)
                    codes = [M_META + iachar("x"), codes, 13_int64]
                    call char_events(codes, key)
                    call push_events(events, nev, key, times)
                    cycle words
                end if
            end if
            ! [MODS]<NAME>: a function key, unless NAME is RET etc.
            k = 1
            do while (k + 1 <= len(word))
                if (modifier_bit(word(k:k)) == 0 .or. word(k + 1:k + 1) /= "-") exit
                k = k + 2
            end do
            if (len(word) >= k + 2) then
                if (word(k:k) == "<" .and. word(len(word):) == ">") then
                    mods = word(1:k - 1)
                    word = mods//word(k + 1:len(word) - 1)
                    if (.not. ends_in_name(word)) then
                        allocate (key(1))
                        key(1) = make_symbol_ref(intern(word))
                        call push_events(events, nev, key, times)
                        cycle words
                    end if
                end if
            end if
            ! REM or ;; comments out the rest of the line.
            if (word == "REM" .or. index(word, ";;") == 1) then
                q = index(keys(pos:), achar(10))
                if (q == 0) exit words
                pos = pos + q - 1
                cycle words
            end if
            orig = word
            prefix = 0
            bits = 0
            do while (len(word) >= 3)
                if (modifier_bit(word(1:1)) == 0 .or. word(2:2) /= "-") exit
                bits = bits + modifier_bit(word(1:1))
                prefix = prefix + 2
                word = word(3:)
            end do
            if (len(word) >= 2) then
                if (word(1:1) == "^") then
                    call decode(word(2:), codes)
                    if (size(codes) == 1) then
                        bits = bits + M_CTL
                        prefix = prefix + 1
                        word = word(2:)
                    end if
                end if
            end if
            select case (word)
            case ("NUL")
                word = achar(0)
            case ("RET")
                word = achar(13)
            case ("LFD")
                word = achar(10)
            case ("TAB")
                word = achar(9)
            case ("ESC")
                word = achar(27)
            case ("SPC")
                word = " "
            case ("DEL")
                word = achar(127)
            case default
                ! Nothing to do for other values.
            end select
            is_vector = .false.
            if (len(word) >= 2) then
                if (word(1:1) == "\" .and. verify(word(2:), "01234567") == 0) then
                    is_vector = .true.
                    c = 0
                    do k = 2, len(word)
                        c = 8*c + iachar(word(k:k)) - 48
                    end do
                    codes = [c]
                end if
            end if
            if (.not. is_vector) call decode(word, codes)
            if (bits == 0) then
                call char_events(codes, key)
            else if (bits == M_META .and. .not. is_vector .and. &
                     numeric_word(word)) then
                call char_events(codes + bits, key)
            else if (size(codes) /= 1) then
                call set_error(status, FM_GENERIC_ERROR, &
                               orig(1:prefix)//" must prefix a single character, not "// &
                               word)
                return
            else if (iand(bits, M_CTL) /= 0 .and. .not. is_vector .and. &
                     ((codes(1) >= 64 .and. codes(1) <= 95) .or. &
                      (codes(1) >= 97 .and. codes(1) <= 122))) then
                allocate (key(1))
                key(1) = make_fixnum(bits - M_CTL + iand(codes(1), 31_int64))
            else
                allocate (key(1))
                key(1) = make_fixnum(bits + codes(1))
            end if
            call push_events(events, nev, key, times)
        end do words
        allocate (trimmed(nev))
        trimmed = events(1:nev)
        call move_alloc(trimmed, events)
    contains

        !> Whether `w' matches ^-?[0-9]+$.
        logical function numeric_word(w)
            character(len=*), intent(in) :: w
            integer :: s
            s = 1
            if (len(w) > 0) then
                if (w(1:1) == "-") s = 2
            end if
            numeric_word = len(w) >= s .and. verify(w(s:), "0123456789") == 0
        end function numeric_word
    end subroutine key_parse

    !> Return true if KEYS, a key sequence, is valid.
    !> This function checks key sequences that are being input to
    !> `keymap_set', `keymap_global_set' and the like.
    !>
    !> A key has the form "A-C-H-M-S-s-key" (the modifiers in this order),
    !> where "key" is a single character, a name in angle brackets such as
    !> "<f1>" or "<mouse-1>", or one of NUL, RET, TAB, LFD, ESC, SPC, DEL.
    !> Keys of a sequence are separated by single spaces.  Control and
    !> Meta'd characters are not accepted as keys: write "C-a" and "M-a".
    !> Port of keymap.el:key-valid-p.
    pure logical function key_valid_p(keys)
        character(len=*), intent(in) :: keys
        integer :: b, e, n
        key_valid_p = .false.
        n = len(keys)
        ! "\`[^ ]+\( [^ ]+\)*\'"
        if (n == 0) return
        if (keys(1:1) == " " .or. keys(n:n) == " ") return
        if (index(keys, "  ") > 0) return
        b = 1
        do while (b <= n)
            e = index(keys(b:), " ")
            if (e == 0) then
                e = n
            else
                e = b + e - 2
            end if
            if (.not. valid_key(keys(b:e))) return
            b = e + 2
        end do
        key_valid_p = .true.
    end function key_valid_p

    !> Whether one space-free word of `key_valid_p' input is a valid key.
    pure logical function valid_key(word)
        character(len=*), intent(in) :: word
        integer :: s, m
        s = after_modifiers(word, 1)
        associate (key => word(s:))
            if (len(key) == 1) then
                ! A printing ASCII character (unibyte 127..255 are Meta'd).
                valid_key = iachar(key) >= 32 .and. iachar(key) < 127
            else if (len(key) >= 2 .and. multibyte_length(key, 1_int64, .false.) == len(key)) then
                ! One multibyte (non-ASCII) character.
                valid_key = .true.
            else if (index(key, "<") == 1 .and. len(key) >= 3 .and. index(key, ">", back=.true.) == len(key)) then
                ! "\`<[-_A-Za-z0-9]+>\'", with no modifiers inside.
                m = len(key) - 1
                valid_key = verify(key(2:m), "-_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") == 0 &
                            .and. after_modifiers(key, 2) == 2
            else
                select case (key)
                case ("NUL", "RET", "TAB", "LFD", "ESC", "SPC", "DEL")
                    valid_key = .true.
                case default
                    valid_key = .false.
                end select
            end if
        end associate
    end function valid_key

    !> Skip the modifier prefixes "A-C-H-M-S-s-" (each optional, in this
    !> order) starting at `start' of `word'; the position after them.
    pure integer function after_modifiers(word, start) result(s)
        character(len=*), intent(in) :: word
        integer, intent(in) :: start
        character(len=*), parameter :: order = "ACHMSs"
        integer :: k
        s = start
        do k = 1, len(order)
            if (s + 1 > len(word)) exit
            if (word(s:s) == order(k:k) .and. word(s + 1:s + 1) == "-") s = s + 2
        end do
    end function after_modifiers

    !> Signal an error if KEY doesn't have a valid syntax.  Port of
    !> keymap.el:keymap--check.
    subroutine keymap_check(key, status)
        character(len=*), intent(in) :: key
        type(status_type), intent(out) :: status
        if (.not. key_valid_p(key)) then
            call set_error(status, FM_GENERIC_ERROR, &
                           """"//key//""""//format_message(" is not a valid key definition; see `key_valid_p'"))
        end if
    end subroutine keymap_check

    !> Set KEY to DEFINITION in KEYMAP.
    !> KEY is a string that satisfies `key_valid_p'.
    !> If DEFINITION is a string, it must also satisfy `key_valid_p'.
    !>
    !> DEFINITION is anything that can be a key's definition:
    !>  nil (means key is undefined in this keymap),
    !>  a command (a symbol naming a registered command),
    !>  a string (treated as a keyboard macro or a sequence of input events),
    !>  a keymap (to define a prefix key).
    !> Port of keymap.el:keymap-set.
    subroutine keymap_set_value(keymap, key, definition, status)
        type(handle_type), intent(in) :: keymap
        character(len=*), intent(in) :: key
        type(value_type), intent(in) :: definition
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: events(:), macro(:)
        call keymap_check(key, status)
        if (.not. status%ok()) return
        call key_parse(key, events, status)
        if (.not. status%ok()) return
        ! If we're binding this key to another key, then parse that other
        ! key, too (a keyboard macro, which is a list of events here).
        if (vtag(definition) == V_STRING) then
            call keymap_check(xstring(definition), status)
            if (.not. status%ok()) return
            call key_parse(xstring(definition), macro, status)
            if (.not. status%ok()) return
            call define_key(keymap, events, make_list(macro), status)
        else
            call define_key(keymap, events, definition, status)
        end if
    end subroutine keymap_set_value

    !> Set KEY to the command named COMMAND in KEYMAP: `keymap_set' with
    !> the symbol COMMAND as the definition.
    subroutine keymap_set_command(keymap, key, command, status)
        type(handle_type), intent(in) :: keymap
        character(len=*), intent(in) :: key, command
        type(status_type), intent(out) :: status
        call keymap_set_value(keymap, key, make_symbol_ref(intern(command)), status)
    end subroutine keymap_set_command

    !> Give KEY a global binding as COMMAND.
    !> KEY is a string that must satisfy `key_valid_p'.
    !>
    !> COMMAND is the command definition to use: anything that
    !> `keymap_set' accepts as its DEFINITION argument.
    !>
    !> Note that if KEY has a local binding in the current buffer,
    !> that local binding will continue to shadow any global binding
    !> that you make with this function.  Port of keymap.el:keymap-global-set.
    subroutine keymap_global_set_value(key, command, status)
        character(len=*), intent(in) :: key
        type(value_type), intent(in) :: command
        type(status_type), intent(out) :: status
        call keymap_set_value(current_global_map(), key, command, status)
    end subroutine keymap_global_set_value

    !> Give KEY a global binding as the command named COMMAND, e.g.
    !>
    !>     call keymap_global_set("C-c f", "find-file", status)
    subroutine keymap_global_set_command(key, command, status)
        character(len=*), intent(in) :: key, command
        type(status_type), intent(out) :: status
        call keymap_set_command(current_global_map(), key, command, status)
    end subroutine keymap_global_set_command

    ! ------------------------------------------------------------------
    ! where-is
    ! ------------------------------------------------------------------

    !> Return 2 if SEQ uses only chars without modifiers other than Meta
    !> (where-is-preferred-modifier nil), 1 if it uses only chars, and 0
    !> otherwise.  Port of keymap.c:preferred_sequence_p.
    integer function preferred_sequence_p(seq) result(result)
        type(value_type), intent(in) :: seq(:)
        integer(int64) :: modifiers
        integer :: i
        result = 1
        do i = 1, size(seq)
            if (.not. fixnump(seq(i))) then
                result = 0
                return
            end if
            modifiers = iand(xfixnum(seq(i)), M_ALL - M_META)
            if (modifiers == 0) then
                result = 2
            else
                result = 0
                return
            end if
        end do
    end function preferred_sequence_p

    !> Return list of keys that invoke DEFINITION.
    !> The keymaps searched are the currently active ones, or KEYMAP and
    !> the global map.  If FIRSTONLY
    !> is true, return the first key sequence found, preferring sequences
    !> consisting of characters without modifiers other than Meta, as a
    !> list of events (nil for none); else a list of all of them, each a
    !> list of events.  Prefix keys of the meta-prefix-char (ESC) followed
    !> by a character are given as one Meta character.  Bindings shadowed
    !> by another binding of the same key are left out.  With FIRSTONLY,
    !> the command's `advertised_binding' comes first if it runs it.
    !> Port of keymap.c:Fwhere_is_internal (no remapping, menus or
    !> composed keymaps).  A run of characters bound alike in a full
    !> keymap's char-table is found as one event, the interval (FROM TO).
    subroutine where_is_internal(definition, keys, firstonly, keymap)
        type(value_type), intent(in) :: definition
        type(value_type), intent(out) :: keys
        logical, intent(in), optional :: firstonly
        type(handle_type), intent(in), optional :: keymap
        type(key_list_type), allocatable :: queue(:), found(:)
        type(value_type), allocatable :: maps(:), seq(:), elems(:)
        type(value_type) :: active, binding, one
        type(handle_type) :: km
        type(status_type) :: status
        integer :: qhead, qtail, nfound, k
        logical :: first
        first = .false.
        if (present(firstonly)) first = firstonly
        keys = nil()
        if (present(keymap)) then
            allocate (maps(2))
            maps(1) = make_keymap_ref(keymap)
            maps(2) = make_keymap_ref(current_global_map())
            active = make_list(maps)
            deallocate (maps)
        else
            active = current_active_maps()
        end if
        if (keymap_ref_p(active)) then
            maps = [active]
        else
            call list_elements(active, maps)
        end if
        if (first .and. symbolp(definition) .and. .not. nilp(definition)) then
            ! We have an advertised binding.
            call advertised_key(symbols(xsymbol(definition))%name, seq)
            if (size(seq) > 0) then
                call lookup_key(active, seq, binding, status)
                if (status%ok() .and. value_eq(binding, definition)) then
                    keys = make_list(seq)
                    return
                end if
            end if
            deallocate (seq)
        end if
        ! Faccessible_keymaps of each active map, breadth first.
        allocate (queue(16), found(16))
        qtail = 0
        do k = 1, size(maps)
            if (.not. keymap_ref_p(maps(k))) cycle
            allocate (seq(0))
            call push(queue, qtail, seq, xhandle(maps(k)))
            deallocate (seq)
        end do
        qhead = 0
        nfound = 0
        do while (qhead < qtail)
            qhead = qhead + 1
            km = queue(qhead)%map
            do while (table%is_live(km))
                call scan_map(km)
                km = keymaps(km%index)%parent
            end do
        end do
        ! Verify that each binding is not shadowed by another binding for
        ! the same key, then pick the result.
        allocate (elems(0))
        do k = 1, nfound
            call lookup_key(active, event_heads(found(k)%keys), binding, status)
            if (.not. status%ok() .or. .not. value_eq(binding, definition)) cycle
            if (first) then
                if (preferred_sequence_p(found(k)%keys) == 2) then
                    keys = make_list(found(k)%keys)
                    return
                end if
                if (nilp(keys)) keys = make_list(found(k)%keys)
            else
                ! (Not [elems, make_list(...)]: that leaks in gfortran.)
                one = make_list(found(k)%keys)
                call append_value(elems, one)
            end if
        end do
        if (.not. first) keys = make_list(elems)
    contains

        !> Look at the bindings of one map: matches go to `found', prefix
        !> maps to the queue.
        subroutine scan_map(map)
            type(handle_type), intent(in) :: map
            integer, allocatable :: order(:)
            integer(int64), allocatable :: run_from(:), run_to(:)
            type(value_type), allocatable :: run_val(:)
            type(value_type) :: ev_value
            integer(int64) :: i, n
            integer :: m
            associate (mp => keymaps(map%index))
                n = mp%n
                ! A full keymap's char-table first: runs of characters with
                ! the same binding, in order (map_keymap_char_table_item).
                if (mp%full) then
                    call char_table_runs(mp, run_from, run_to, run_val)
                    do m = 1, size(run_from)
                        if (run_from(m) == run_to(m)) then
                            ev_value = make_fixnum(run_from(m))
                        else
                            ev_value = list(make_fixnum(run_from(m)), make_fixnum(run_to(m)))
                        end if
                        call scan_binding(ev_value, run_val(m))
                    end do
                end if
                call entry_order(mp, order)
                do m = 1, size(order)
                    i = order(m)
                    call scan_binding(mp%events(i), mp%bindings(i))
                end do
            end associate
        end subroutine scan_map

        !> Look at the binding `def' of event `event' in the map being
        !> scanned.  Port of keymap.c:where_is_internal_1.
        subroutine scan_binding(event, def)
            type(value_type), intent(in) :: event, def
            type(value_type), allocatable :: prefix(:), key(:)
            integer(int64) :: ev
            integer :: j
            logical :: last_is_meta
            prefix = queue(qhead)%keys
            last_is_meta = .false.
            if (size(prefix) > 0) then
                if (fixnump(prefix(size(prefix)))) last_is_meta = xfixnum(prefix(size(prefix))) == META_PREFIX_CHAR
            end if
            ! A prefix key: its map is searched too.
            if (keymap_ref_p(def)) then
                key = [prefix, event]
                if (.not. cycle_p(xhandle(def), key) .and. size(prefix) < 10) then
                    call push(queue, qtail, key, xhandle(def))
                end if
            end if
            if (.not. value_eq(def, definition)) return
            ! We have found a match.  Construct the key sequence where we
            ! found it.
            if (fixnump(event) .and. last_is_meta) then
                ev = ior(xfixnum(event), M_META)
                key = prefix
                key(size(key)) = make_fixnum(ev)
            else
                key = [prefix, event]
            end if
            ! Record it, unless it's already been seen (as could happen
            ! when inheriting keymaps).
            do j = 1, nfound
                if (same_keys(found(j)%keys, key)) exit
            end do
            if (j > nfound) call push(found, nfound, key, null_handle())
        end subroutine scan_binding

        !> Whether `map' was reached already through a prefix of `key' (a
        !> cycle; keymap.c:accessible_keymaps_1).  A map reached through
        !> other prefixes is searched again.
        logical function cycle_p(map, key)
            type(handle_type), intent(in) :: map
            type(value_type), intent(in) :: key(:)
            integer :: j, lim
            cycle_p = .false.
            do j = 1, qtail
                if (queue(j)%map%index /= map%index .or. queue(j)%map%generation /= map%generation) cycle
                lim = size(queue(j)%keys)
                if (lim > size(key)) cycle
                if (same_keys(queue(j)%keys, key(1:lim))) then
                    cycle_p = .true.
                    return
                end if
            end do
        end function cycle_p
    end subroutine where_is_internal

    !> The `advertised_binding' of the command named `name' as events;
    !> none if it has none (or it doesn't parse).
    subroutine advertised_key(name, key)
        character(len=*), intent(in) :: name
        type(value_type), allocatable, intent(out) :: key(:)
        type(command_type), pointer :: p
        type(status_type) :: status
        allocate (key(0))
        p => defined_command(name)
        if (.not. associated(p)) p => builtin_command(name)
        if (.not. associated(p)) return
        if (.not. allocated(p%advertised_binding)) return
        if (len(p%advertised_binding) == 0) return
        call key_parse(p%advertised_binding, key, status)
        if (.not. status%ok()) key = key(1:0)
    end subroutine advertised_key

    !> Whether entry event `ev' of a full keymap is in its char-table (a
    !> character without modifiers).
    logical function char_table_event_p(ev)
        type(value_type), intent(in) :: ev
        char_table_event_p = fixnump(ev)
        if (char_table_event_p) char_table_event_p = iand(xfixnum(ev), M_ALL) == 0
    end function char_table_event_p

    !> The runs of characters with the same non-nil binding in the
    !> char-table of full keymap `mp' (its single characters over its
    !> ranges), in character order, as map_char_table gives them.
    subroutine char_table_runs(mp, run_from, run_to, run_val)
        type(keymap_type), intent(in) :: mp
        integer(int64), allocatable, intent(out) :: run_from(:), run_to(:)
        type(value_type), allocatable, intent(out) :: run_val(:)
        integer(int64), allocatable :: bounds(:)
        integer(int64) :: b, c, key
        type(value_type) :: val
        integer :: i, j, nb, nr
        logical :: have
        allocate (bounds(2*int(mp%n) + 2*int(mp%nranges)))
        nb = 0
        do i = 1, int(mp%n)
            if (.not. char_table_event_p(mp%events(i))) cycle
            nb = nb + 2
            bounds(nb - 1) = xfixnum(mp%events(i))
            bounds(nb) = xfixnum(mp%events(i)) + 1
        end do
        do i = 1, int(mp%nranges)
            nb = nb + 2
            bounds(nb - 1) = mp%range_from(i)
            bounds(nb) = mp%range_to(i) + 1
        end do
        ! Sort and drop duplicates.
        do i = 2, nb
            key = bounds(i)
            j = i - 1
            do while (j >= 1)
                if (bounds(j) <= key) exit
                bounds(j + 1) = bounds(j)
                j = j - 1
            end do
            bounds(j + 1) = key
        end do
        j = 0
        do i = 1, nb
            if (j > 0) then
                if (bounds(j) == bounds(i)) cycle
            end if
            j = j + 1
            bounds(j) = bounds(i)
        end do
        nb = j
        allocate (run_from(max(nb, 1)), run_to(max(nb, 1)), run_val(max(nb, 1)))
        nr = 0
        do i = 1, nb - 1
            c = bounds(i)
            ! The binding of characters c .. bounds(i+1)-1.
            have = .false.
            do j = 1, int(mp%n)
                if (.not. char_table_event_p(mp%events(j))) cycle
                if (xfixnum(mp%events(j)) /= c) cycle
                val = mp%bindings(j)
                have = .true.
            end do
            if (.not. have) then
                do j = int(mp%nranges), 1, -1
                    if (c < mp%range_from(j) .or. c > mp%range_to(j)) cycle
                    val = mp%range_bindings(j)
                    have = .true.
                    exit
                end do
            end if
            if (.not. have) cycle
            if (nilp(val)) cycle
            b = bounds(i + 1) - 1
            if (nr > 0) then
                if (run_to(nr) + 1 == c .and. value_eq(run_val(nr), val)) then
                    run_to(nr) = b
                    cycle
                end if
            end if
            nr = nr + 1
            run_from(nr) = c
            run_to(nr) = b
            run_val(nr) = val
        end do
        run_from = run_from(1:nr)
        run_to = run_to(1:nr)
        run_val = run_val(1:nr)
    end subroutine char_table_runs

    !> `keys' with each event replaced by its head (a character interval
    !> by its first character), as lookup sees them (EVENT_HEAD).
    function event_heads(keys) result(heads)
        type(value_type), intent(in) :: keys(:)
        type(value_type), allocatable :: heads(:)
        integer :: k
        heads = keys
        do k = 1, size(keys)
            if (vtag(keys(k)) == V_LIST) heads(k) = list_elt(keys(k), 1_int64)
        end do
    end function event_heads

    !> The order upstream's map_keymap visits a keymap's entries in after
    !> a full keymap's char-table (see `char_table_runs'): the other
    !> entries, newest first.
    subroutine entry_order(mp, order)
        type(keymap_type), intent(in) :: mp
        integer, allocatable, intent(out) :: order(:)
        integer :: i, n, m
        n = int(mp%n)
        allocate (order(n))
        m = 0
        do i = n, 1, -1
            if (mp%full) then
                if (char_table_event_p(mp%events(i))) cycle
            end if
            m = m + 1
            order(m) = i
        end do
        order = order(1:m)
    end subroutine entry_order

    logical function same_keys(a, b)
        type(value_type), intent(in) :: a(:), b(:)
        integer :: i
        same_keys = size(a) == size(b)
        if (.not. same_keys) return
        do i = 1, size(a)
            if (.not. value_eq(a(i), b(i))) then
                same_keys = .false.
                return
            end if
        end do
    end function same_keys

    !> Append `v' to the array `list'.
    subroutine append_value(list, v)
        type(value_type), allocatable, intent(inout) :: list(:)
        type(value_type), intent(in) :: v
        type(value_type), allocatable :: grown(:)
        integer :: n
        n = size(list)
        allocate (grown(n + 1))
        grown(1:n) = list
        grown(n + 1) = v
        call move_alloc(grown, list)
    end subroutine append_value

    !> Append (`keys', `map') to `list', which holds `n' entries.
    subroutine push(list, n, keys, map)
        type(key_list_type), allocatable, intent(inout) :: list(:)
        integer, intent(inout) :: n
        type(value_type), intent(in) :: keys(:)
        type(handle_type), intent(in) :: map
        type(key_list_type), allocatable :: grown(:)
        integer :: j
        if (n == size(list)) then
            allocate (grown(2*size(list)))
            do j = 1, n
                grown(j) = list(j)
            end do
            call move_alloc(grown, list)
        end if
        n = n + 1
        list(n)%keys = keys
        list(n)%map = map
    end subroutine push
end module keymap_fm
