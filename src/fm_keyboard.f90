! fm_keyboard.f90 -- keyboard input and the editor command loop.
! Ported from GNU Emacs 31.1 src/keyboard.c.
!
! Copyright (C) 1985-1989, 1993-1997, 1999-2026 Free Software Foundation,
! Inc.
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

!> Input flows: terminal bytes -> `kbd_store_bytes` (meta handling and
!> keyboard decoding) -> the event queue -> `read_event` (after
!> `unread_command_events`) -> `read_key_sequence` (active keymaps plus
!> input-decode-map, local-function-key-map and key-translation-map)
!> -> `command_loop_step`, which runs the bound command.
!>
!> Events are fixnums (characters with modifier bits) and symbols
!> (function keys).  Without a terminal (`set_input_fd` not called) the
!> event queue is the only source, which is how tests drive the loop;
!> `read_event` then reports end of input instead of blocking.
!>
!> Not ported yet: echoing, the echo area and `message` (the last message
!> is kept in `last_message`), timers, idle hooks and redisplay, mouse and
!> switch-frame events, recursive edits, keyboard macro recording,
!> pre/post-command hooks, `help-char`, case tables (upper-case fallback
!> is ASCII only), quit while computing (C-g is an ordinary key).
module fm_keyboard
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use fm_error, only: status_type, set_error, format_error, FM_GENERIC_ERROR
    use fm_handle, only: handle_type, null_handle
    use fm_lisp, only: value_type, V_LIST, nil, make_fixnum, &
                       make_keymap_ref, vtag, nilp, fixnump, symbolp, &
                       stringp, xfixnum, xsymbol, xstring, list_elements
    use fm_lread, only: intern
    use fm_character, only: CHAR_SHIFT, CHAR_META, CHAR_MODIFIER_MASK, &
                            bytes_by_char_head, byte8_to_char, &
                            multibyte_length, string_char_and_length
    use fm_keymap, only: keymapp, keymap_live_p, access_keymap, &
                         current_active_maps, &
                         key_description, parse_modifiers, apply_modifiers, &
                         overriding_terminal_local_map
    use fm_callint, only: call_interactively, current_prefix_arg, prefix_arg, &
                          prefix_numeric_value
    use fm_sysdep, only: emacs_read, input_available
    implicit none(type, external)
    private

    public :: set_input_meta_mode, set_input_fd, kbd_store_bytes, &
              tty_read_avail_input, unread_command_events, &
              discard_input, input_pending_p, read_event
    public :: read_key_sequence, command_loop_step, command_loop, &
              request_exit, set_transient_map
    public :: READ_KEY_ELTS, redisplay_proc

    !> keyboard.c:READ_KEY_ELTS, the longest key sequence.
    integer, parameter :: READ_KEY_ELTS = 30

    integer(int64), parameter :: M_META = int(CHAR_META, int64), &
                                 M_SHIFT = int(CHAR_SHIFT, int64), &
                                 M_MASK = int(CHAR_MODIFIER_MASK, int64)

    !> Keymaps translating key sequences before lookup (null: none).
    !> KVAR input_decode_map: terminal escape sequences -> function keys.
    type(handle_type), public :: input_decode_map
    !> KVAR Vlocal_function_key_map: applied only to undefined sequences.
    type(handle_type), public :: local_function_key_map
    !> keyboard.c:Vkey_translation_map.
    type(handle_type), public :: key_translation_map

    !> keyboard.c:last_command_event, Vthis_command, KVAR last_command and
    !> friends.
    type(value_type), public :: last_command_event, this_command, &
                                real_this_command, last_command, &
                                real_last_command, last_prefix_arg
    !> keyboard.c:Vthis_command_keys_shift_translated.
    logical, public :: this_command_keys_shift_translated = .false.
    !> The keys of the command being run (this-command-keys).
    type(value_type), allocatable, public :: this_command_keys(:)
    !> Stand-in for the echo area: the last message shown.
    character(len=:), allocatable, public :: last_message

    !> tty->meta_key: 0 strips the 8th bit, 1 makes it Meta, 2 passes
    !> bytes on to keyboard decoding (UTF-8).
    integer :: meta_key = 2
    !> Terminal input file descriptor, or -1 for none.
    integer(int32) :: input_fd = -1
    !> Bytes of an incomplete UTF-8 sequence.
    character(len=:), allocatable :: pending_bytes

    !> The event queue (keyboard.c:kbd_buffer): events(head:tail).
    type(value_type), allocatable :: events(:)
    integer(int64) :: head = 1, tail = 0
    !> Vunread_command_events, read before the queue.
    type(value_type), allocatable :: unread(:)
    integer(int64) :: nunread = 0

    abstract interface
        !> Redisplay all frames (xdisp.c:redisplay).
        subroutine redisplay_proc()
            implicit none(type, external)
        end subroutine redisplay_proc
    end interface

    !> Called by read_event before it waits for terminal input, so the
    !> screen is current.  Set by the program (fm_xdisp sits above this
    !> module); null in tests.
    procedure(redisplay_proc), pointer, public :: redisplay_hook => null()

    !> Set by `request_exit` to end `command_loop`.
    logical :: exit_requested = .false.
    !> overriding-terminal-local-map holds a transient map, to be dropped
    !> before the next command runs.
    logical :: transient_active = .false.

    !> keyboard.c:struct keyremap.  `start` and `end` are 0-based offsets
    !> into the key buffer, as upstream.
    type :: keyremap_type
        type(value_type) :: parent, map
        integer :: start = 0, end = 0
    end type keyremap_type

contains

    ! ------------------------------------------------------------------
    ! Input
    ! ------------------------------------------------------------------

    !> Port of keyboard.c:Fset_input_meta_mode (META: 0 nil, 1 t, 2
    !> encoded).
    subroutine set_input_meta_mode(mode)
        integer, intent(in) :: mode

        meta_key = mode
    end subroutine set_input_meta_mode

    !> Read keyboard input from `fd` (-1 for none).
    subroutine set_input_fd(fd)
        integer(int32), intent(in) :: fd

        input_fd = fd
    end subroutine set_input_fd

    !> Append `ev` to the event queue.  Port of keyboard.c:kbd_buffer_store_event.
    subroutine store_event(ev)
        type(value_type), intent(in) :: ev
        type(value_type), allocatable :: grown(:)
        integer(int64) :: n

        if (.not. allocated(events)) allocate (events(64))
        if (tail == size(events, kind=int64)) then
            n = tail - head + 1
            if (head > 1 .and. n < size(events, kind=int64)/2) then
                ! Compact in place.
                events(1:n) = events(head:tail)
            else
                allocate (grown(2*size(events, kind=int64)))
                grown(1:n) = events(head:tail)
                call move_alloc(grown, events)
            end if
            head = 1
            tail = n
        end if
        tail = tail + 1
        events(tail) = ev
    end subroutine store_event

    !> Store character event `c`.
    subroutine store_char(c)
        integer(int64), intent(in) :: c

        call store_event(make_fixnum(c))
    end subroutine store_char

    !> Flush incomplete UTF-8 bytes as raw-byte characters.
    subroutine flush_pending()
        integer(int64) :: k

        if (.not. allocated(pending_bytes)) return
        do k = 1, len(pending_bytes, kind=int64)
            call store_char(int(byte8_to_char(ichar(pending_bytes(k:k), int32)), int64))
        end do
        deallocate (pending_bytes)
    end subroutine flush_pending

    !> Decode byte `b` with the keyboard coding system (UTF-8): complete
    !> characters become events, invalid bytes raw-byte characters.  The
    !> decoding part of keyboard.c:read_decoded_event_from_main_queue.
    subroutine decode_byte(b)
        integer(int32), intent(in) :: b
        integer(int32) :: need, ch, length

        if (allocated(pending_bytes)) then
            if (iand(b, int(z'C0', int32)) /= int(z'80', int32)) then
                ! Not a trailing byte: the pending sequence is invalid.
                call flush_pending()
            end if
        end if
        if (.not. allocated(pending_bytes)) then
            if (b < 128) then
                call store_char(int(b, int64))
                return
            end if
            if (iand(b, int(z'C0', int32)) == int(z'80', int32)) then
                call store_char(int(byte8_to_char(b), int64))
                return
            end if
            pending_bytes = char(b)
            return
        end if
        pending_bytes = pending_bytes//char(b)
        need = bytes_by_char_head(ichar(pending_bytes(1:1), int32))
        if (len(pending_bytes) < need) return
        if (multibyte_length(pending_bytes, 1_int64, .false.) == need) then
            call string_char_and_length(pending_bytes, 1_int64, ch, length)
            call store_char(int(ch, int64))
            deallocate (pending_bytes)
        else
            call flush_pending()
        end if
    end subroutine decode_byte

    !> Turn terminal input `bytes` into events.  The per-byte part of
    !> keyboard.c:tty_read_avail_input.
    subroutine kbd_store_bytes(bytes)
        character(len=*), intent(in) :: bytes
        integer(int64) :: k
        integer(int32) :: b

        do k = 1, len(bytes, kind=int64)
            b = ichar(bytes(k:k), int32)
            select case (meta_key)
            case (1)
                if (b >= 128) then
                    call store_char(M_META + b - 128)
                else
                    call store_char(int(b, int64))
                end if
            case (2)
                call decode_byte(b)
            case default
                call store_char(int(iand(b, 127_int32), int64))
            end select
        end do
    end subroutine kbd_store_bytes

    !> Read whatever input the terminal has, without waiting; `nread` is
    !> the number of bytes (0 at end of file).  Port of
    !> keyboard.c:tty_read_avail_input.
    subroutine tty_read_avail_input(nread, status)
        integer(int64), intent(out) :: nread
        type(status_type), intent(out) :: status
        character(len=4096) :: buf

        nread = 0
        if (input_fd < 0) return
        if (.not. input_available(input_fd, 0_int32)) return
        call emacs_read(input_fd, buf, nread, status)
        if (.not. status%ok()) return
        if (nread > 0) call kbd_store_bytes(buf(1:nread))
    end subroutine tty_read_avail_input

    !> Push `evs` to be read before any other input, in order.
    !> Like setting keyboard.c:Vunread_command_events.
    subroutine unread_command_events(evs)
        type(value_type), intent(in) :: evs(:)
        type(value_type), allocatable :: merged(:)
        integer(int64) :: n

        n = size(evs, kind=int64)
        allocate (merged(n + nunread))
        merged(1:n) = evs
        if (nunread > 0) merged(n + 1:) = unread(1:nunread)
        call move_alloc(merged, unread)
        nunread = nunread + n
    end subroutine unread_command_events

    !> Drop all pending input.  Port of keyboard.c:Fdiscard_input.
    subroutine discard_input()
        nunread = 0
        if (allocated(unread)) deallocate (unread)
        head = 1
        tail = 0
        if (allocated(events)) deallocate (events)
        if (allocated(pending_bytes)) deallocate (pending_bytes)
    end subroutine discard_input

    !> Whether input is waiting.  Port of keyboard.c:Finput_pending_p.
    logical function input_pending_p()
        input_pending_p = nunread > 0 .or. tail >= head
        if (input_pending_p .or. input_fd < 0) return
        input_pending_p = input_available(input_fd, 0_int32)
    end function input_pending_p

    !> The next input event.  Waits for terminal input if there is a
    !> terminal; otherwise `have` is false when the queue is empty (end of
    !> input).  The core of keyboard.c:read_char.
    subroutine read_event(ev, have, status)
        type(value_type), intent(out) :: ev
        logical, intent(out) :: have
        type(status_type), intent(out) :: status
        integer(int64) :: nread

        have = .false.
        do
            if (nunread > 0) then
                ev = unread(1)
                unread(1:nunread - 1) = unread(2:nunread)
                unread(nunread) = nil()
                nunread = nunread - 1
                have = .true.
                return
            end if
            if (tail >= head) then
                ev = events(head)
                events(head) = nil()
                head = head + 1
                if (head > tail) then
                    head = 1
                    tail = 0
                end if
                have = .true.
                return
            end if
            if (input_fd < 0) return
            ! Nothing typed yet: show the screen before waiting (read_char's
            ! redisplay).
            if (associated(redisplay_hook)) then
                if (.not. input_available(input_fd, 0_int32)) call redisplay_hook()
            end if
            if (.not. input_available(input_fd, -1_int32)) cycle
            call tty_read_avail_input(nread, status)
            if (.not. status%ok()) return
            ! End of file: the terminal went away.
            if (nread == 0) return
        end do
    end subroutine read_event

    ! ------------------------------------------------------------------
    ! Key sequences
    ! ------------------------------------------------------------------

    function map_value(km) result(v)
        type(handle_type), intent(in) :: km
        type(value_type) :: v

        if (keymap_live_p(km)) v = make_keymap_ref(km)
    end function map_value

    !> Port of keyboard.c:test_undefined (without command remapping).
    logical function test_undefined(binding)
        type(value_type), intent(in) :: binding

        test_undefined = nilp(binding)
        if (.not. test_undefined .and. symbolp(binding)) then
            test_undefined = xsymbol(binding) == intern("undefined")
        end if
    end function test_undefined

    !> Events of translation `next` (a list of events or a string).
    subroutine sequence_events(next, seq)
        type(value_type), intent(in) :: next
        type(value_type), allocatable, intent(out) :: seq(:)
        character(len=:), allocatable :: s
        integer(int64) :: pos, n
        integer(int32) :: ch, length

        if (vtag(next) == V_LIST) then
            call list_elements(next, seq)
            return
        end if
        s = xstring(next)
        allocate (seq(len(s)))
        n = 0
        pos = 1
        do while (pos <= len(s, kind=int64))
            call string_char_and_length(s, pos, ch, length)
            n = n + 1
            seq(n) = make_fixnum(int(ch, int64))
            pos = pos + length
        end do
        seq = seq(1:n)
    end subroutine sequence_events

    !> One step of key remapping: extend `fk`'s scanned key by one event
    !> and, if the map binds it to a key sequence and `doit`, replace the
    !> scanned events with it (`done`; `diff` is the change in length).
    !> `input` is the number of events in `keybuf`.  Port of
    !> keyboard.c:keyremap_step (translation functions are not supported).
    subroutine keyremap_step(keybuf, fk, input, doit, diff, done, status)
        type(value_type), intent(inout) :: keybuf(:)
        type(keyremap_type), intent(inout) :: fk
        integer, intent(in) :: input
        logical, intent(in) :: doit
        integer, intent(out) :: diff
        logical, intent(out) :: done
        type(status_type), intent(out) :: status
        type(value_type) :: key, next
        type(value_type), allocatable :: seq(:)
        integer :: i, n

        done = .false.
        diff = 0
        key = keybuf(fk%end + 1)
        fk%end = fk%end + 1
        if (keymapp(fk%parent)) next = access_keymap(fk%map, key, .true., .false.)
        if (doit .and. (stringp(next) .or. (vtag(next) == V_LIST .and. &
                                            .not. keymapp(next)))) then
            call sequence_events(next, seq)
            n = size(seq)
            diff = n - (fk%end - fk%start)
            if (READ_KEY_ELTS - input <= diff) then
                call set_error(status, FM_GENERIC_ERROR, "Key sequence too long")
                return
            end if
            if (diff < 0) then
                do i = fk%end, input - 1
                    keybuf(i + diff + 1) = keybuf(i + 1)
                end do
            else if (diff > 0) then
                do i = input - 1, fk%end, -1
                    keybuf(i + diff + 1) = keybuf(i + 1)
                end do
            end if
            keybuf(fk%start + 1:fk%start + n) = seq
            fk%end = fk%end + diff
            fk%start = fk%end
            fk%map = fk%parent
            done = .true.
            return
        end if
        if (keymapp(next)) then
            fk%map = next
        else
            fk%start = fk%start + 1
            fk%end = fk%start
            fk%map = fk%parent
        end if
    end subroutine keyremap_step

    subroutine reset_remap(fk, pos)
        type(keyremap_type), intent(inout) :: fk
        integer, intent(in) :: pos

        fk%start = pos
        fk%end = pos
        fk%map = fk%parent
    end subroutine reset_remap

    !> Read a key sequence: events until the active keymaps give a
    !> non-prefix binding (or none), with terminal escape sequences and
    !> other translations applied.  `keys` is the sequence and `binding`
    !> its binding (nil if undefined).  `eof` is set when input ended;
    !> `keys` then holds what was read.  Port of
    !> keyboard.c:read_key_sequence (no echoing, mouse or frame events).
    subroutine read_key_sequence(keys, binding, eof, status)
        type(value_type), allocatable, intent(out) :: keys(:)
        type(value_type), intent(out) :: binding
        logical, intent(out) :: eof
        type(status_type), intent(out) :: status
        type(value_type) :: keybuf(READ_KEY_ELTS)
        type(value_type) :: current_binding, new_binding, key, &
                            original_uppercase
        type(keyremap_type) :: indec, fkey, keytran
        character(len=:), allocatable :: base
        integer :: t, mock_input, first_unbound, diff, i, &
                   original_uppercase_position
        integer(int64) :: k, c, bits
        logical :: have, done, shift_translated

        eof = .false.
        t = 0
        mock_input = 0
        original_uppercase_position = -1
        shift_translated = .false.
        indec%parent = map_value(input_decode_map)
        fkey%parent = map_value(local_function_key_map)
        keytran%parent = map_value(key_translation_map)
        call reset_remap(indec, 0)
        call reset_remap(fkey, 0)
        call reset_remap(keytran, 0)

        replay: do
            first_unbound = READ_KEY_ELTS + 1
            current_binding = current_active_maps()
            t = 0
            keyloop: do
                if (nilp(current_binding)) then
                    if (keytran%start >= t) exit replay
                else if (.not. keymapp(current_binding)) then
                    exit replay
                end if

                if (first_unbound < keytran%start) then
                    ! The prefix up to first_unbound is unbound and has no
                    ! translation left: drop it and start over.
                    do i = first_unbound + 1, t - 1
                        keybuf(i - first_unbound) = keybuf(i + 1)
                    end do
                    mock_input = t - first_unbound - 1
                    call reset_remap(indec, indec%start - (first_unbound + 1))
                    call reset_remap(fkey, fkey%start - (first_unbound + 1))
                    call reset_remap(keytran, keytran%start - (first_unbound + 1))
                    cycle replay
                end if

                if (t >= READ_KEY_ELTS) then
                    call set_error(status, FM_GENERIC_ERROR, "Key sequence too long")
                    return
                end if

                if (t < mock_input) then
                    key = keybuf(t + 1)
                else
                    call read_event(key, have, status)
                    if (.not. status%ok()) return
                    if (.not. have) then
                        eof = .true.
                        exit replay
                    end if
                end if

                ! keyboard.c:follow_key
                new_binding = access_keymap(current_binding, key, .true., .false.)
                if (.not. nilp(new_binding)) then
                    first_unbound = max(t + 1, first_unbound)
                else
                    first_unbound = min(t, first_unbound)
                end if
                current_binding = new_binding
                keybuf(t + 1) = key
                t = t + 1

                ! input-decode-map: applies wherever it matches.
                do while (indec%end < t)
                    call keyremap_step(keybuf, indec, max(t, mock_input), .true., &
                                       diff, done, status)
                    if (.not. status%ok()) return
                    if (done) then
                        mock_input = diff + max(t, mock_input)
                        cycle replay
                    end if
                end do

                ! local-function-key-map: only for undefined sequences.
                if (.not. keymapp(current_binding) .and. &
                    .not. test_undefined(current_binding) .and. indec%start >= t) then
                    if (fkey%start < t) call reset_remap(fkey, t)
                else
                    do while (fkey%end < indec%start)
                        call keyremap_step(keybuf, fkey, max(t, mock_input), &
                                           fkey%end + 1 == t .and. &
                                           test_undefined(current_binding), &
                                           diff, done, status)
                        if (.not. status%ok()) return
                        if (done) then
                            mock_input = diff + max(t, mock_input)
                            indec%end = indec%end + diff
                            indec%start = indec%start + diff
                            cycle replay
                        end if
                    end do
                end if

                ! key-translation-map.
                do while (keytran%end < fkey%start)
                    call keyremap_step(keybuf, keytran, max(t, mock_input), .true., &
                                       diff, done, status)
                    if (.not. status%ok()) return
                    if (done) then
                        mock_input = diff + max(t, mock_input)
                        indec%end = indec%end + diff
                        indec%start = indec%start + diff
                        fkey%end = fkey%end + diff
                        fkey%start = fkey%start + diff
                        cycle replay
                    end if
                end do

                ! An undefined upper-case or shifted character: try it without
                ! the shift.
                if (nilp(current_binding) .and. keytran%start >= t .and. &
                    fixnump(key)) then
                    k = xfixnum(key)
                    c = iand(k, not(M_MASK))
                    if (iand(k, M_SHIFT) /= 0) then
                        k = k - M_SHIFT
                    else if (c >= iachar("A") .and. c <= iachar("Z")) then
                        k = k + 32
                    else
                        cycle keyloop
                    end if
                    original_uppercase = key
                    original_uppercase_position = t - 1
                    keybuf(t) = make_fixnum(k)
                    mock_input = max(t, mock_input)
                    shift_translated = .true.
                    cycle replay
                end if

                ! An undefined shifted function key: try it unshifted.
                if (nilp(current_binding) .and. keytran%start >= t .and. &
                    symbolp(key)) then
                    call parse_modifiers(key, bits, base)
                    if (iand(bits, M_SHIFT) /= 0) then
                        original_uppercase = key
                        original_uppercase_position = t - 1
                        keybuf(t) = apply_modifiers(bits - M_SHIFT, base)
                        mock_input = max(t, mock_input)
                        call reset_remap(fkey, 0)
                        call reset_remap(keytran, 0)
                        shift_translated = .true.
                        cycle replay
                    end if
                end if
            end do keyloop
        end do replay

        ! An undefined key keeps its original case.
        if (nilp(current_binding) .and. t > 0 .and. &
            t - 1 == original_uppercase_position) then
            keybuf(t) = original_uppercase
            shift_translated = .false.
        end if
        this_command_keys_shift_translated = shift_translated
        binding = current_binding
        if (keymapp(binding)) binding = nil()
        allocate (keys(t))
        keys = keybuf(1:t)
    end subroutine read_key_sequence

    ! ------------------------------------------------------------------
    ! The command loop
    ! ------------------------------------------------------------------

    !> Make `km` take precedence over all other keymaps for the next key
    !> sequence only.  Port of subr.el:set-transient-map with KEEP-PRED
    !> nil (no ON-EXIT, MESSAGE or TIMEOUT); like upstream, the map is
    !> dropped from the pre-command hook.
    subroutine set_transient_map(km)
        type(handle_type), intent(in) :: km

        overriding_terminal_local_map = km
        transient_active = .true.
    end subroutine set_transient_map

    !> Make `command_loop` return after the current command.
    subroutine request_exit()
        exit_requested = .true.
    end subroutine request_exit

    !> Run keyboard macro `macro` (a string or list of events): its events
    !> are read before other input.  A count from the prefix argument
    !> repeats it.  Simplified macros.c:Fexecute_kbd_macro.
    subroutine execute_kbd_macro(macro, count)
        type(value_type), intent(in) :: macro
        integer(int64), intent(in) :: count
        type(value_type), allocatable :: seq(:)
        integer(int64) :: k

        call sequence_events(macro, seq)
        do k = 1, max(count, 1_int64)
            call unread_command_events(seq)
        end do
    end subroutine execute_kbd_macro

    !> Read one key sequence and run its command.  `done` at end of input.
    !> Errors from the command are reported in `last_message` (as the
    !> command loop's error handler displays them) and returned in
    !> `status`.  One iteration of keyboard.c:command_loop_1, with
    !> simple.el:command-execute.
    subroutine command_loop_step(done, status)
        logical, intent(out) :: done
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        type(value_type) :: cmd
        logical :: eof

        done = .false.
        this_command = nil()
        real_this_command = nil()
        call read_key_sequence(keys, cmd, eof, status)
        if (.not. status%ok()) then
            last_message = format_error(status)
            return
        end if
        if (eof .and. size(keys) == 0) then
            done = .true.
            return
        end if
        ! A new command clears the echo area.
        if (allocated(last_message)) last_message = ""
        ! Input ended in the middle of a key sequence: it is unbound.
        if (eof) cmd = nil()
        last_command_event = keys(size(keys))
        this_command_keys = keys
        this_command = cmd
        real_this_command = cmd

        ! pre-command-hook: a transient map lasts for one key sequence.
        if (transient_active) then
            overriding_terminal_local_map = null_handle()
            transient_active = .false.
        end if

        ! simple.el:command-execute
        current_prefix_arg = prefix_arg
        prefix_arg = nil()
        if (test_undefined(cmd)) then
            ! subr.el:undefined
            last_message = key_description(keys)//" is undefined"
        else if (stringp(cmd) .or. vtag(cmd) == V_LIST) then
            call execute_kbd_macro(cmd, prefix_numeric_value(current_prefix_arg))
        else
            call call_interactively(cmd, status)
            if (.not. status%ok()) last_message = format_error(status)
        end if

        last_prefix_arg = current_prefix_arg
        last_command = this_command
        real_last_command = real_this_command
        done = eof
    end subroutine command_loop_step

    !> Run commands until input ends or `request_exit` is called.  Command
    !> errors don't stop the loop.  Port of keyboard.c:command_loop_1.
    subroutine command_loop(status)
        type(status_type), intent(out) :: status
        type(status_type) :: step_status
        logical :: done

        exit_requested = .false.
        do
            call command_loop_step(done, step_status)
            if (done .or. exit_requested) exit
        end do
        exit_requested = .false.
    end subroutine command_loop

end module fm_keyboard
