! keyboard_fm.f90 -- keyboard input and the editor command loop.
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

!> Input flows: terminal bytes -> `kbd_store_bytes' (meta handling and
!> keyboard decoding) -> the event queue -> `read_event' (after
!> `unread_command_events') -> `read_key_sequence' (active keymaps plus
!> input-decode-map, local-function-key-map and key-translation-map)
!> -> `command_loop_step', which runs the bound command.
!>
!> Events are fixnums (characters with modifier bits) and symbols
!> (function keys).  Without a terminal (`set_input_fd' not called) the
!> event queue is the only source, which is how tests drive the loop;
!> `read_event' then reports end of input instead of blocking.
!>
!> Keyboard macros run as upstream (macros.c:Fexecute_kbd_macro): a
!> command loop reads the macro's events until they run out, and an
!> error ends the macro and reaches the command loop's error handler.
!> They are recorded as upstream: every event `read_event' returns while
!> no macro is executing goes into the macro being defined
!> (keyboard.c:record_char), the events of a command become part of it
!> once the command has finished without leaving a prefix argument
!> (`finalize_kbd_macro_chars' in command_loop_1), and `discard_input'
!> (so any command error) drops the events of the current command and
!> ends the definition.  macros.c is the submodule macros_fm.
!>
!> Not ported yet: echoing, the echo area and `message' (the last message
!> is kept in `last_message'), timers, idle hooks and redisplay, mouse and
!> switch-frame events, the (t . EVENT) and (no-record . EVENT) forms of
!> `unread_command_events', `help_char', case
!> tables (upper-case fallback is ASCII only), quit while computing (C-g
!> is an ordinary key), `ring_bell_function', the `minibuffer_quit'
!> condition (a quit in the minibuffer is handled as any quit) and
!> `minibuffer_error_function'.
module keyboard_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR, FM_QUIT, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use data_fm, only: value_eq
    use lisp_fm, only: value_type, V_LIST, nil, make_fixnum, &
                       make_keymap_ref, vtag, nilp, fixnump, symbolp, &
                       stringp, xfixnum, xsymbol, xstring, list_elements
    use lread_fm, only: intern, sym
    use character_fm, only: CHAR_SHIFT, CHAR_META, CHAR_MODIFIER_MASK, &
                            bytes_by_char_head, byte8_to_char, &
                            multibyte_length, string_char_and_length
    use keymap_fm, only: keymapp, keymap_live_p, access_keymap, &
                         current_active_maps, &
                         key_description, parse_modifiers, apply_modifiers, &
                         overriding_terminal_local_map, pushed_terminal_local_map, keymap_events, lookup_key, command_remapping
    use callint_fm, only: call_interactively, funcall, current_prefix_arg, prefix_arg, &
                          prefix_numeric_value
    use buffer_fm, only: buffers, cur, vdeactivate_mark, transient_mark_mode
    use undo_fm, only: undo_auto_add_boundary, point_before_last_command_or_undo, &
                       buffer_before_last_command_or_undo
    use sysdep_fm, only: emacs_read, input_available, interrupt_pending, inputs_available
    use eval_fm, only: hook_type
    implicit none(type, external)
    private

    public :: set_input_meta_mode, set_input_fd, kbd_store_bytes, &
              tty_read_avail_input, unread_command_events, &
              discard_input, input_pending_p, read_event
    public :: read_key_sequence, command_loop_step, command_loop, &
              request_exit, exit_requested_p, set_transient_map, recursive_edit_1, &
              throw_exit, throw_exit_string, exit_recursive_edit, abort_recursive_edit, sit_for, &
              run_at_time, cancel_timer
    public :: execute_kbd_macro, ring_bell, bitch_at_user, executing_kbd_macro_p
    public :: start_kbd_macro, end_kbd_macro, call_last_kbd_macro, store_kbd_macro_char, store_kbd_macro_event, &
              finalize_kbd_macro_chars, cancel_kbd_macro_events, finish_kbd_macro, defining_kbd_macro_p, &
              syms_of_macros
    public :: recent_keys, lossage_size, message_log_proc
    public :: READ_KEY_ELTS, redisplay_proc, transient_exit_proc, kbd_macro_loop_proc

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
    !> Stand-in for minibuffer.el's minibuffer--message-overlay: text shown
    !> after the active minibuffer's contents until the next command
    !> (`minibuffer_message'); unallocated for none.
    character(len=:), allocatable, public :: minibuffer_message_text
    !> Stand-in for minibuffer.el's minibuffer-message-overlay: the text
    !> `message' shows after the active minibuffer's contents
    !> (set-minibuffer-message) until the next input
    !> (clear-minibuffer-message); unallocated for none.
    character(len=:), allocatable, public :: set_minibuffer_message_text

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
        !> Redisplay all frames (xdisp.c:redisplay); also the form of a
        !> post-command hook.
        subroutine redisplay_proc()
            implicit none(type, external)
        end subroutine redisplay_proc

        !> The ON-EXIT function of a transient map.
        subroutine transient_exit_proc()
            implicit none(type, external)
        end subroutine transient_exit_proc

        !> A keyboard macro's LOOPFUNC: called before each iteration of the
        !> macro, which stops when it returns false.
        logical function kbd_macro_loop_proc()
            implicit none(type, external)
        end function kbd_macro_loop_proc

        !> Log `text' in the *Messages* buffer as echo-area output
        !> (xdisp.c via print.c:printchar).
        subroutine message_log_proc(text)
            implicit none(type, external)
            character(len=*), intent(in) :: text
        end subroutine message_log_proc

        !> Run a disabled command's handler instead of the command, if
        !> CMD is disabled: `handled' says it was.
        subroutine disabled_command_proc(cmd, handled, status)
            import :: value_type, status_type
            implicit none(type, external)
            type(value_type), intent(in) :: cmd
            logical, intent(out) :: handled
            type(status_type), intent(out) :: status
        end subroutine disabled_command_proc
    end interface

    !> Checks for disabled commands (simple.el:command-execute's `disabled'
    !> property test and `disabled_command_function'), set by novice_fm;
    !> null for none.
    procedure(disabled_command_proc), pointer, public :: disabled_command_hook => null()

    !> Logs the error messages of the command loop in *Messages*
    !> (xdisp_fm sits above this module; set by simple_fm); null for
    !> none.
    procedure(message_log_proc), pointer, public :: message_log_hook => null()

    !> keyboard.c:lossage_limit, the number of events and commands
    !> `recent_keys' keeps (3 * MIN_NUM_RECENT_KEYS).
    integer, parameter :: LOSSAGE_LIMIT = 300

    !> keyboard.c:recent_keys, a ring of the last LOSSAGE_LIMIT input
    !> events not from keyboard macros, and of the commands run, as
    !> upstream's pseudo-events (nil . COMMAND): `recent_is_command' marks
    !> those, whose command (nil for an undefined key) is in
    !> `recent_events'.  The next entry goes at `recent_keys_index'
    !> (1-based); `total_keys' entries are filled.
    type(value_type) :: recent_events(LOSSAGE_LIMIT)
    logical :: recent_is_command(LOSSAGE_LIMIT) = .false.
    integer :: recent_keys_index = 1, total_keys = 0

    !> Called by read_event before it waits for terminal input, so the
    !> screen is current.  Set by the program (xdisp_fm sits above this
    !> module); null in tests.
    procedure(redisplay_proc), pointer, public :: redisplay_hook => null()

    !> Run after each command (keyboard.c's post-command-hook, with one
    !> function); null for none.
    procedure(redisplay_proc), pointer, public :: post_command_hook => null()

    !> Run before each command, after its keys are read and `this_command'
    !> is set (keyboard.c's pre-command-hook, with one function); null
    !> for none.
    procedure(redisplay_proc), pointer, public :: pre_command_hook => null()

    !> Ring the bell of the selected frame's terminal (terminal.c:
    !> ring_bell's FRAME_TERMINAL (f)->ring_bell_hook); set by the
    !> program, null without a terminal.
    procedure(redisplay_proc), pointer, public :: ring_bell_hook => null()

    !> Subprocess output: while waiting for input, `process_wake_fd' (the
    !> descriptor the process workers wake the main thread through, -1 for
    !> none) is watched too, and when it is ready `process_output_hook'
    !> takes the output (runs filters and sentinels), as
    !> process.c:wait_reading_process_output does.  Set by process_fm.
    integer(int32), public :: process_wake_fd = -1
    procedure(redisplay_proc), pointer, public :: process_output_hook => null()

    !> macros.c:Vexecuting_kbd_macro: `macro_events' are the events of
    !> the keyboard macro being executed (when `macro_active'); a string
    !> macro (`macro_string') gives characters 128..255 the meta bit.
    !> `macro_forced_end' stands for the value t, which ends the macro.
    type(value_type), allocatable :: macro_events(:)
    logical :: macro_active = .false., macro_string = .false., macro_forced_end = .false.
    !> macros.c:executing_kbd_macro_index, the index of the next event.
    integer(int64), public, protected :: executing_kbd_macro_index = 0
    !> macros.c:executing_kbd_macro (non-nil): the most recently started
    !> macro, for the "After N kbd macro iterations" error context.
    logical :: macro_started = .false.
    !> macros.c:executing_kbd_macro_iterations.
    integer(int64), public, protected :: executing_kbd_macro_iterations = 0
    !> Values of `defining_kbd_macro': nil, t and `append'.
    integer, parameter, public :: DEFINING_KBD_MACRO_NIL = 0, DEFINING_KBD_MACRO_T = 1, DEFINING_KBD_MACRO_APPEND = 2

    !> Not `DEFINING_KBD_MACRO_NIL' while a keyboard macro is being defined.
    !> Don't set this!
    !> The value is `DEFINING_KBD_MACRO_APPEND' while appending to the
    !> definition of an existing macro.  Port of macros.c defining-kbd-macro
    !> (DEFINING_KBD_MACRO_NIL for nil, DEFINING_KBD_MACRO_T for t).
    integer, public :: defining_kbd_macro = DEFINING_KBD_MACRO_NIL

    !> Last kbd macro defined, as an array of events; not allocated if none
    !> defined.  Port of macros.c last-kbd-macro (a vector: no string
    !> macros).
    type(value_type), allocatable, public :: last_kbd_macro(:)

    !> Normal hook run whenever a keyboard macro terminates.
    !> This is run whether the macro ends normally or prematurely due to
    !> an error.  Port of macros.c kbd-macro-termination-hook.
    type(hook_type), target, public :: kbd_macro_termination_hook = hook_type(name="kbd_macro_termination_hook")

    !> The last input event received.
    !> This is the last event read by any means, also from a keyboard
    !> macro.  Port of keyboard.c last-input-event.
    type(value_type), public :: last_input_event

    !> The keyboard macro being defined (struct kboard's kbd_macro_buffer):
    !> `kbd_macro_ptr' events stored, of which the first `kbd_macro_end'
    !> belong to it (the rest are the current command's).
    type(value_type), allocatable :: kbd_macro_buffer(:)
    integer(int64) :: kbd_macro_ptr = 0, kbd_macro_end = 0

    !> Whether errors of the innermost command loop are handled there
    !> (command_loop_2 with handlers `error'): false in a keyboard macro's
    !> loop, where they end the macro.
    logical :: loop_handles_errors = .true.

    !> How long to display an echo-area message when the minibuffer is active.
    !> If the value is a number, it should be specified in seconds.
    !> Port of keyboard.c minibuffer-message-timeout (a non-number, no
    !> timeout, is not ported: Deferred).
    integer, public :: minibuffer_message_timeout = 2

    !> A timer (timer.el run-at-time, one at a time): the function to run
    !> while waiting for input once `system_clock' reaches `timer_due';
    !> null for none.
    procedure(redisplay_proc), pointer :: timer_function => null()
    integer(int64) :: timer_due = 0

    !> Set by `request_exit' to end `command_loop'.
    logical :: exit_requested = .false.
    !> keyboard.c:command_loop_level, the depth of recursive edits.
    integer, public, protected :: command_loop_level = 0
    !> A (throw 'exit VALUE) is pending; `exit_abort' for VALUE t.
    logical :: exit_pending = .false., exit_abort = .false.
    !> How many recursive edits the pending abort leaves
    !> (minibuffer-quit-recursive-edit's LEVELS).
    integer :: exit_levels = 1
    !> VALUE is this string: the recursive edit signals it as an error.
    character(len=:), allocatable :: exit_string
    !> overriding-terminal-local-map holds a transient map, to be dropped
    !> before the next command runs.
    logical :: transient_active = .false.
    ! The active transient map's KEEP-PRED (t), ON-EXIT and MESSAGE
    ! (unallocated for nil).
    logical :: transient_keep = .false.
    procedure(transient_exit_proc), pointer :: transient_on_exit => null()
    character(len=:), allocatable :: transient_message

    !> keyboard.c:struct keyremap.  `start' and `end' are 0-based offsets
    !> into the key buffer, as upstream.
    type :: keyremap_type
        type(value_type) :: parent, map
        integer :: start = 0, end = 0
    end type keyremap_type

    !> Execute MACRO as a sequence of events.
    !> If MACRO is a string or vector, then the events in it are executed
    !> exactly as if they had been input by the user.
    !>
    !> COUNT is a repeat count, or nil for once, or 0 for infinite loop.
    !>
    !> Optional third arg LOOPFUNC may be a function that is called prior to
    !> each iteration of the macro.  Iteration stops if LOOPFUNC returns
    !> false.
    !>
    !> The buffer shown in the currently selected window will be made the current
    !> buffer before the macro is executed.  Port of
    !> macros.c:Fexecute_kbd_macro (MACRO is a string, a list of events or
    !> an array of events; no symbols' function definitions).
    interface execute_kbd_macro
        recursive module subroutine execute_kbd_macro_value(macro, count, status, loopfunc)
            implicit none(type, external)
            type(value_type), intent(in) :: macro, count
            type(status_type), intent(out) :: status
            procedure(kbd_macro_loop_proc), optional :: loopfunc
        end subroutine execute_kbd_macro_value

        recursive module subroutine execute_kbd_macro_vector(macro, count, status, loopfunc)
            implicit none(type, external)
            type(value_type), intent(in) :: macro(:), count
            type(status_type), intent(out) :: status
            procedure(kbd_macro_loop_proc), optional :: loopfunc
        end subroutine execute_kbd_macro_vector
    end interface execute_kbd_macro

    interface
        !> Record subsequent keyboard input, defining a keyboard macro.
        !> The commands are recorded even as they are executed.
        !> Use \[end_kbd_macro] to finish recording and make the macro available.
        !> True APPEND (prefix arg) means append to last macro defined;
        !> this begins by re-executing that macro as if you typed it again.
        !> If optional second arg, NO_EXEC, is true, do not re-execute last
        !> macro before appending to it.  Port of macros.c:Fstart_kbd_macro.
        recursive module subroutine start_kbd_macro(append, no_exec, status)
            implicit none(type, external)
            logical, intent(in) :: append, no_exec
            type(status_type), intent(out) :: status
        end subroutine start_kbd_macro

        !> Finish defining a keyboard macro.
        !> The definition was started by \[start_kbd_macro].
        !> The macro is now available for use via \[call_last_kbd_macro].
        !>
        !> With numeric arg, repeat macro now that many times,
        !> counting the definition just completed as the first repetition.
        !> An argument of zero means repeat until error.
        !>
        !> Optional second arg LOOPFUNC may be a function that is called prior to
        !> each iteration of the macro.  Iteration stops if LOOPFUNC returns
        !> false.  Port of macros.c:Fend_kbd_macro (REPEAT is nil or a
        !> fixnum).
        recursive module subroutine end_kbd_macro(repeat, status, loopfunc)
            implicit none(type, external)
            type(value_type), intent(in) :: repeat
            type(status_type), intent(out) :: status
            procedure(kbd_macro_loop_proc), optional :: loopfunc
        end subroutine end_kbd_macro

        !> Call the last keyboard macro that you defined with \[start_kbd_macro].
        !>
        !> A prefix argument serves as a repeat count.  Zero means repeat until error.
        !>
        !> Optional second arg LOOPFUNC may be a function that is called prior to
        !> each iteration of the macro.  Iteration stops if LOOPFUNC returns
        !> false.  Port of macros.c:Fcall_last_kbd_macro.
        recursive module subroutine call_last_kbd_macro(prefix, status, loopfunc)
            implicit none(type, external)
            type(value_type), intent(in) :: prefix
            type(status_type), intent(out) :: status
            procedure(kbd_macro_loop_proc), optional :: loopfunc
        end subroutine call_last_kbd_macro

        !> Store character C into kbd macro being defined.  Port of
        !> macros.c:store_kbd_macro_char.
        module subroutine store_kbd_macro_char(c)
            implicit none(type, external)
            type(value_type), intent(in) :: c
        end subroutine store_kbd_macro_char

        !> Store EVENT into the keyboard macro being defined.  Port of
        !> macros.c:Fstore_kbd_macro_event.
        module subroutine store_kbd_macro_event(event)
            implicit none(type, external)
            type(value_type), intent(in) :: event
        end subroutine store_kbd_macro_event

        !> Declare that all chars stored so far in the kbd macro being defined
        !> really belong to it.  This is done in between editor commands.
        !> Port of macros.c:finalize_kbd_macro_chars.
        module subroutine finalize_kbd_macro_chars()
            implicit none(type, external)
        end subroutine finalize_kbd_macro_chars

        !> Cancel the events added to a keyboard macro for this command.
        !> Port of macros.c:Fcancel_kbd_macro_events.
        module subroutine cancel_kbd_macro_events()
            implicit none(type, external)
        end subroutine cancel_kbd_macro_events

        !> Finish defining the current keyboard macro.  Port of
        !> macros.c:end_kbd_macro.
        module subroutine finish_kbd_macro()
            implicit none(type, external)
        end subroutine finish_kbd_macro

        !> Register the commands of macros.c (syms_of_macros).
        module subroutine syms_of_macros()
            implicit none(type, external)
        end subroutine syms_of_macros
    end interface

contains

    !> Whether a keyboard macro is being defined (`defining_kbd_macro' is
    !> not nil).
    logical function defining_kbd_macro_p()
        defining_kbd_macro_p = defining_kbd_macro /= DEFINING_KBD_MACRO_NIL
    end function defining_kbd_macro_p

    ! ------------------------------------------------------------------
    ! Input
    ! ------------------------------------------------------------------

    !> Port of keyboard.c:Fset_input_meta_mode (META: 0 nil, 1 t, 2
    !> encoded).
    subroutine set_input_meta_mode(mode)
        integer, intent(in) :: mode
        meta_key = mode
    end subroutine set_input_meta_mode

    !> Read keyboard input from `fd' (-1 for none).
    subroutine set_input_fd(fd)
        integer(int32), intent(in) :: fd
        input_fd = fd
    end subroutine set_input_fd

    !> Append `ev' to the event queue.  Port of keyboard.c:kbd_buffer_store_event.
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

    !> Store character event `c'.
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

    !> Decode byte `b' with the keyboard coding system (UTF-8): complete
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

    !> Turn terminal input `bytes' into events.  The per-byte part of
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

    !> Read whatever input the terminal has, without waiting; `nread' is
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

    !> Push `evs' to be read before any other input, in order.
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

    !> Discard the contents of the terminal input buffer.
    !> Also end any kbd macro being defined.  Port of
    !> keyboard.c:Fdiscard_input.
    subroutine discard_input()
        if (defining_kbd_macro /= DEFINING_KBD_MACRO_NIL) then
            ! Discard the last command from the macro.
            call cancel_kbd_macro_events()
            call finish_kbd_macro()
        end if
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
    !> terminal; otherwise `have' is false when the queue is empty (end of
    !> input).  The event becomes `last_input_event' and, unless it comes
    !> from a keyboard macro being executed, goes into the keyboard macro
    !> being defined.  The core of keyboard.c:read_char, with the macro
    !> part of record_char.
    subroutine read_event(ev, have, status)
        type(value_type), intent(out) :: ev
        logical, intent(out) :: have
        type(status_type), intent(out) :: status
        call read_event_1(ev, have, status)
        if (.not. have) return
        last_input_event = ev
        if (.not. macro_active) call store_kbd_macro_char(ev)
        ! recent_keys should not include events from keyboard macros.
        if (.not. macro_active) call record_recent(ev, .false.)
    end subroutine read_event

    !> Store `ev' (an event, or the command run if `command') into the
    !> ring of recent keys.  The recent_keys part of keyboard.c:record_char
    !> (no help-echo or mouse-movement events) and of command_loop_1.
    subroutine record_recent(ev, command)
        type(value_type), intent(in) :: ev
        logical, intent(in) :: command
        if (total_keys < LOSSAGE_LIMIT) total_keys = total_keys + 1
        recent_events(recent_keys_index) = ev
        recent_is_command(recent_keys_index) = command
        recent_keys_index = recent_keys_index + 1
        if (recent_keys_index > LOSSAGE_LIMIT) recent_keys_index = 1
    end subroutine record_recent

    !> Return the last few events, not counting those from keyboard macros.
    !> If INCLUDE_CMDS is true, include the commands that were run, as
    !> pseudo-events: `is_command' marks them, their command (.false.
    !> for none) being the event.  Port of keyboard.c:Frecent_keys.
    subroutine recent_keys(include_cmds, keys, is_command)
        logical, intent(in) :: include_cmds
        type(value_type), allocatable, intent(out) :: keys(:)
        logical, allocatable, intent(out) :: is_command(:)
        integer :: i, k, n, first
        if (total_keys < LOSSAGE_LIMIT) then
            first = 1
        else
            first = recent_keys_index
        end if
        n = total_keys
        if (.not. include_cmds) n = total_keys - count(recent_is_command(1:total_keys))
        allocate (keys(n), is_command(n))
        n = 0
        i = first
        do k = 1, total_keys
            if (include_cmds .or. .not. recent_is_command(i)) then
                n = n + 1
                keys(n) = recent_events(i)
                is_command(n) = recent_is_command(i)
            end if
            i = i + 1
            if (i > LOSSAGE_LIMIT) i = 1
        end do
    end subroutine recent_keys

    !> Return the maximum number of keystrokes and commands recorded by
    !> `recent_keys'.  Port of keyboard.c:Flossage_size (no ARG: the size
    !> can't be changed yet).
    integer function lossage_size()
        lossage_size = LOSSAGE_LIMIT
    end function lossage_size

    !> The next input event (read_event without recording it).
    subroutine read_event_1(ev, have, status)
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
            ! If we're executing a macro, process it unless we are at its
            ! end.
            if (macro_active) then
                if (.not. at_end_of_macro_p()) then
                    executing_kbd_macro_index = executing_kbd_macro_index + 1
                    ev = macro_events(executing_kbd_macro_index)
                    if (macro_string .and. fixnump(ev)) then
                        if (iand(xfixnum(ev), 128_int64) /= 0 .and. xfixnum(ev) <= 255) then
                            ev = make_fixnum(ior(M_META, iand(xfixnum(ev), not(128_int64))))
                        end if
                    end if
                    have = .true.
                    return
                end if
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
            ! C-g on the terminal arrives as SIGINT; while waiting for
            ! input it reads as the quit character (keyboard.c:
            ! handle_interrupt's quit_throw_to_read_char).
            if (interrupt_pending()) then
                ev = make_fixnum(7_int64)
                have = .true.
                return
            end if
            ! Nothing typed yet: show the screen before waiting (read_char's
            ! redisplay).
            if (associated(redisplay_hook)) then
                if (.not. input_available(input_fd, 0_int32)) call redisplay_hook()
            end if
            if (.not. wait_for_input(timer_wait())) then
                call run_due_timer()
                cycle
            end if
            call tty_read_avail_input(nread, status)
            if (.not. status%ok()) return
            ! End of file: the terminal went away.
            if (nread == 0) return
        end do
    end subroutine read_event_1

    ! ------------------------------------------------------------------
    ! Key sequences
    ! ------------------------------------------------------------------

    function map_value(km) result(v)
        type(handle_type), intent(in) :: km
        type(value_type) :: v
        if (keymap_live_p(km)) v = make_keymap_ref(km)
    end function map_value

    !> Whether BINDING is undefined: nil, `undefined', or a command
    !> remapped to `undefined'.  Port of keyboard.c:test_undefined.
    logical function test_undefined(binding)
        type(value_type), intent(in) :: binding
        type(value_type) :: remapped
        test_undefined = nilp(binding)
        if (.not. test_undefined .and. symbolp(binding)) then
            test_undefined = xsymbol(binding) == intern("undefined")
            if (.not. test_undefined) then
                call command_remapping(binding, remapped)
                if (symbolp(remapped)) test_undefined = xsymbol(remapped) == intern("undefined")
            end if
        end if
    end function test_undefined

    !> Events of translation `next' (a list of events or a string).
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

    !> One step of key remapping: extend `fk''s scanned key by one event
    !> and, if the map binds it to a key sequence and `doit', replace the
    !> scanned events with it (`done'; `diff' is the change in length).
    !> `input' is the number of events in `keybuf'.  Port of
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
    !> other translations applied.  `keys' is the sequence and `binding'
    !> its binding (nil if undefined).  `eof' is set when input ended;
    !> `keys' then holds what was read.  Port of
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
        logical :: have, done, shift_translated, macro_end
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
                macro_end = .false.
                if (macro_active .and. nunread == 0) macro_end = at_end_of_macro_p()
                if (t < mock_input) then
                    key = keybuf(t + 1)
                else if (macro_end) then
                    ! If we're at the end of a macro, exit it by returning
                    ! 0, unless there are unread events pending.
                    t = 0
                    eof = .true.
                    exit replay
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

    !> Set MAP as a temporary keymap taking precedence over other keymaps.
    !> Normally, MAP is used only once, to look up the very next key.
    !> However, if the optional argument KEEP_PRED is true, MAP stays
    !> active if a key from MAP is used.
    !>
    !> Optional arg ON_EXIT, if present, specifies a procedure that is
    !> called, with no arguments, after MAP is deactivated.
    !>
    !> Optional arg MESSAGE, if present, requests display of an informative
    !> message after activating the transient map: the %k specifier in the
    !> string is replaced with the list of keys from the transient map.
    !> Upon deactivating the map, the displayed message will be cleared
    !> out.  Port of subr.el:set-transient-map (no KEEP-PRED function,
    !> TIMEOUT or nested transient maps); like upstream, the map is
    !> dropped from the pre-command hook.
    subroutine set_transient_map(km, keep_pred, on_exit, message)
        type(handle_type), intent(in) :: km
        logical, intent(in), optional :: keep_pred
        procedure(transient_exit_proc), optional :: on_exit
        character(len=*), intent(in), optional :: message
        type(value_type), allocatable :: events(:), key(:)
        character(len=:), allocatable :: keys
        integer :: k, at
        ! internal-push-keymap: the old map stays active below.
        if (.not. transient_active) pushed_terminal_local_map = overriding_terminal_local_map
        overriding_terminal_local_map = km
        transient_active = .true.
        transient_keep = .false.
        if (present(keep_pred)) transient_keep = keep_pred
        transient_on_exit => null()
        if (present(on_exit)) transient_on_exit => on_exit
        if (allocated(transient_message)) deallocate (transient_message)
        if (.not. present(message)) return
        call keymap_events(km, events)
        keys = ""
        do k = 1, size(events)
            key = [events(k)]
            if (k > 1) keys = keys//", "
            keys = keys//key_description(key)
        end do
        ! format-spec with %k.
        transient_message = message
        at = index(transient_message, "%k")
        if (at > 0) transient_message = transient_message(1:at - 1)//keys//transient_message(at + 2:)
        last_message = transient_message
    end subroutine set_transient_map

    !> The pre-command-hook function of the active transient map: keep it
    !> (repeating its message) when KEEP_PRED is t and the command is the
    !> map's binding of the keys, else deactivate it.  The clearfun of
    !> subr.el:set-transient-map, with its exitfun.
    subroutine clear_transient_map(keys)
        type(value_type), intent(in) :: keys(:)
        type(value_type) :: mc
        type(status_type) :: st
        procedure(transient_exit_proc), pointer :: on_exit
        if (transient_keep) then
            call lookup_key(map_value(overriding_terminal_local_map), keys, mc, st)
            if (st%ok() .and. .not. nilp(mc) .and. value_eq(this_command, mc)) then
                ! Repeat the message for the next command.
                if (allocated(transient_message)) last_message = transient_message
                return
            end if
        end if
        overriding_terminal_local_map = pushed_terminal_local_map
        pushed_terminal_local_map = null_handle()
        transient_active = .false.
        ! Clear the prompt after exiting.
        if (allocated(transient_message)) then
            last_message = ""
            deallocate (transient_message)
        end if
        on_exit => transient_on_exit
        transient_on_exit => null()
        if (associated(on_exit)) call on_exit()
    end subroutine clear_transient_map

    !> Run `fn' once after `ms' milliseconds, while waiting for input,
    !> replacing any timer set before.  Port of timer.el:run-at-time (one
    !> timer; it runs only while reading terminal input).
    subroutine run_at_time(ms, fn)
        integer(int32), intent(in) :: ms
        procedure(redisplay_proc) :: fn
        integer(int64) :: count, rate
        call system_clock(count, rate)
        timer_due = count + int(ms, int64)*rate/1000
        timer_function => fn
    end subroutine run_at_time

    !> Cancel the timer.  Port of timer.el:cancel-timer.
    subroutine cancel_timer()
        timer_function => null()
    end subroutine cancel_timer

    !> Milliseconds until the timer is due (0 if it is), or -1 (wait
    !> forever) when there is none.
    integer(int32) function timer_wait() result(ms)
        integer(int64) :: count, rate
        ms = -1_int32
        if (.not. associated(timer_function)) return
        call system_clock(count, rate)
        ms = int(max(0_int64, min((timer_due - count)*1000/max(rate, 1_int64), int(huge(0_int32), int64))), int32)
    end function timer_wait

    !> Run the timer if it is due (timer.c:timer_check), then redisplay.
    subroutine run_due_timer()
        procedure(redisplay_proc), pointer :: fn
        if (.not. associated(timer_function)) return
        if (timer_wait() > 0) return
        fn => timer_function
        timer_function => null()
        call fn()
        if (associated(redisplay_hook)) call redisplay_hook()
    end subroutine run_due_timer

    !> Redisplay, then wait `ms' milliseconds or until input arrives,
    !> taking subprocess output meanwhile.  Port of subr.el:sit-for (for a
    !> terminal; without one, only subprocess output is waited for).
    subroutine sit_for(ms)
        integer(int32), intent(in) :: ms
        integer(int64) :: start, now, rate, left
        logical :: ready(2)
        integer(int32) :: fds(2)
        if (associated(redisplay_hook)) call redisplay_hook()
        if (input_fd >= 0) then
            if (input_pending_p()) return
        end if
        call system_clock(start, rate)
        do
            call system_clock(now)
            left = int(ms, int64) - (now - start)*1000/max(rate, 1_int64)
            if (left < 0) return
            fds = [input_fd, -1_int32]
            if (associated(process_output_hook)) fds(2) = process_wake_fd
            if (all(fds < 0)) return
            call inputs_available(fds, int(left, int32), ready)
            if (ready(1)) return
            if (.not. ready(2)) return
            call process_output_hook()
            if (associated(redisplay_hook)) call redisplay_hook()
        end do
    end subroutine sit_for

    !> Wait up to `timeout' milliseconds (negative: no limit) for terminal
    !> input, taking subprocess output if it arrives first (then
    !> redisplaying and returning false, as for a timeout).
    logical function wait_for_input(timeout) result(ready)
        integer(int32), intent(in) :: timeout
        logical :: got(2)
        if (process_wake_fd < 0 .or. .not. associated(process_output_hook)) then
            ready = input_available(input_fd, timeout)
            return
        end if
        call inputs_available([input_fd, process_wake_fd], timeout, got)
        ready = got(1)
        if (got(2) .and. .not. ready) then
            call process_output_hook()
            if (associated(redisplay_hook)) call redisplay_hook()
        end if
    end function wait_for_input

    !> Make `command_loop' return after the current command.
    subroutine request_exit()
        exit_requested = .true.
    end subroutine request_exit

    !> Whether a command asked the command loop to stop.
    logical function exit_requested_p()
        exit_requested_p = exit_requested
    end function exit_requested_p

    !> Whether the execution of a macro has reached its end.
    !> This should be called only while executing a macro.  Port of
    !> macros.c:at_end_of_macro_p.
    logical function at_end_of_macro_p()
        at_end_of_macro_p = macro_forced_end .or. .not. allocated(macro_events)
        if (.not. at_end_of_macro_p) then
            at_end_of_macro_p = executing_kbd_macro_index >= size(macro_events, kind=int64)
        end if
    end function at_end_of_macro_p

    !> Whether a keyboard macro is executing (`executing_kbd_macro'
    !> true).
    logical function executing_kbd_macro_p()
        executing_kbd_macro_p = macro_active
    end function executing_kbd_macro_p

    !> Make the selected window's buffer current (set_buffer_internal
    !> (XBUFFER (XWINDOW (selected_window)->contents)) in
    !> keyboard.c:command_loop_1).
    subroutine select_window_buffer()
        use frame_fm, only: selected_window
        use window_fm, only: windows, window_live_p
        use buffer_fm, only: set_buffer, buffer_live_p
        type(handle_type) :: sw
        type(status_type) :: st
        sw = selected_window()
        if (.not. window_live_p(sw)) return
        if (buffer_live_p(windows(sw%index)%buffer)) call set_buffer(windows(sw%index)%buffer, st)
    end subroutine select_window_buffer

    !> Ring the bell (flash the screen with `visible_bell').  Port of
    !> terminal.c:ring_bell (no `ring_bell_function').
    subroutine ring_bell()
        if (associated(ring_bell_hook)) call ring_bell_hook()
    end subroutine ring_bell

    !> Ring the bell, or, while a keyboard macro is executing, stop it with
    !> an error.  Port of dispnew.c:bitch_at_user.
    subroutine bitch_at_user(status)
        type(status_type), intent(inout) :: status
        if (macro_active) then
            ! Stop executing a keyboard macro.
            call set_error(status, FM_USER_ERROR, "Keyboard macro terminated by a command ringing the bell")
        else
            call ring_bell()
        end if
    end subroutine bitch_at_user

    !> Report the error `status' of a command: end any keyboard macro,
    !> discard input, ring the bell and show the message, after "After N
    !> kbd macro iterations: " if a macro was running.  Port of
    !> keyboard.c:cmd_error with Fcommand_error_default_function.
    subroutine cmd_error(status)
        type(status_type), intent(in) :: status
        type(status_type) :: st
        character(len=:), allocatable :: macroerror
        character(len=24) :: buf
        macroerror = ""
        if (macro_started) then
            if (executing_kbd_macro_iterations == 1) then
                macroerror = "After 1 kbd macro iteration: "
            else
                write (buf, "(i0)") executing_kbd_macro_iterations
                macroerror = "After "//trim(buf)//" kbd macro iterations: "
            end if
        end if
        macro_active = .false.
        macro_started = .false.
        if (allocated(macro_events)) deallocate (macro_events)
        prefix_arg = nil()
        last_prefix_arg = nil()
        ! command-error-default-function
        call discard_input()
        call bitch_at_user(st)
        last_message = macroerror//format_error(status)
        if (associated(message_log_hook)) call message_log_hook(last_message)
    end subroutine cmd_error

    !> Read one key sequence and run its command.  `done' at end of input
    !> (or of the keyboard macro being executed).  Errors from the command
    !> are returned in `status'; the command loop's error handler reports
    !> them in `last_message', with the bell (`cmd_error'), except in a
    !> keyboard macro, which they end.  One iteration of
    !> keyboard.c:command_loop_1, with simple.el:command-execute.
    recursive subroutine command_loop_step(done, status)
        logical, intent(out) :: done
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        type(value_type) :: cmd, remapped
        logical :: eof, handled
        done = .false.
        ! Make sure the current window's buffer is selected.
        call select_window_buffer()
        this_command = nil()
        real_this_command = nil()
        call read_key_sequence(keys, cmd, eof, status)
        if (.not. status%ok()) then
            if (loop_handles_errors) call cmd_error(status)
            return
        end if
        if (eof .and. size(keys) == 0) then
            done = .true.
            return
        end if
        ! A new command clears the echo area (read_char wipes it, except
        ! for events from a keyboard macro).
        if (.not. macro_active) then
            if (allocated(last_message)) last_message = ""
        end if
        ! minibuffer--delete-message-overlay runs from pre-command-hook.
        if (allocated(minibuffer_message_text)) deallocate (minibuffer_message_text)
        ! read_char's clear_message runs clear-message-function.
        if (.not. macro_active) then
            if (allocated(set_minibuffer_message_text)) deallocate (set_minibuffer_message_text)
        end if
        ! Input ended in the middle of a key sequence: it is unbound.
        if (eof) cmd = nil()
        ! Remap command through active keymaps (read_key_sequence_remapped).
        if (symbolp(cmd) .and. .not. nilp(cmd)) then
            call command_remapping(cmd, remapped)
            if (.not. nilp(remapped)) cmd = remapped
        end if
        last_command_event = keys(size(keys))
        this_command_keys = keys
        ! Execute the command: recorded in recent_keys as (nil . cmd).
        call record_recent(cmd, .true.)
        this_command = cmd
        real_this_command = cmd
        ! pre-command-hook: a transient map lasts for one key sequence,
        ! unless KEEP-PRED keeps it.
        if (transient_active) call clear_transient_map(keys)
        if (associated(pre_command_hook)) then
            call pre_command_hook()
            ! The hook may have changed the command (command-execute runs
            ! this-command).
            cmd = this_command
        end if
        ! keyboard.c:command_loop_1: undo boundaries for the changes of the
        ! last command, and where point is for record_point.
        if (cur /= 0) then
            call undo_auto_add_boundary()
            point_before_last_command_or_undo = buffers(cur)%pt
            buffer_before_last_command_or_undo = buffers(cur)%serial
        end if
        vdeactivate_mark = .false.
        if (nilp(cmd)) then
            ! nil means key is undefined.
            call funcall(sym("undefined"), status)
        else
            ! simple.el:command-execute
            current_prefix_arg = prefix_arg
            prefix_arg = nil()
            if (stringp(cmd) .or. vtag(cmd) == V_LIST) then
                call execute_kbd_macro(cmd, current_prefix_arg, status)
            else
                handled = .false.
                if (associated(disabled_command_hook)) call disabled_command_hook(cmd, handled, status)
                if (.not. handled) call call_interactively(cmd, status)
            end if
        end if
        if (.not. status%ok()) then
            ! The error leaves command_loop_1: in a keyboard macro's loop it
            ! ends the macro; else the handler reports it and the next
            ! command_loop_1 runs post-command-hook and sets last-command.
            if (.not. loop_handles_errors) return
            call cmd_error(status)
            last_command = this_command
            real_last_command = real_this_command
            if (associated(post_command_hook)) call post_command_hook()
            done = eof
            return
        end if
        last_prefix_arg = current_prefix_arg
        last_command = this_command
        real_last_command = real_this_command
        ! post-command-hook
        if (associated(post_command_hook)) call post_command_hook()
        ! A command that changed the buffer (or asked to) ends an active
        ! region in Transient Mark mode (simple.el:deactivate-mark).
        if (cur /= 0) then
            if (buffers(cur)%mark_active .and. vdeactivate_mark .and. transient_mark_mode%value) then
                buffers(cur)%mark_active = .false.
            end if
        end if
        ! Install chars successfully executed in kbd macro.
        if (defining_kbd_macro /= DEFINING_KBD_MACRO_NIL .and. nilp(prefix_arg)) call finalize_kbd_macro_chars()
        done = eof
    end subroutine command_loop_step

    !> Run commands until input ends or `request_exit' is called.  Command
    !> errors don't stop the loop.  Port of keyboard.c:command_loop_1.
    recursive subroutine command_loop(status)
        type(status_type), intent(out) :: status
        type(status_type) :: step_status
        logical :: done, saved_handles
        exit_requested = .false.
        saved_handles = loop_handles_errors
        loop_handles_errors = .true.
        do
            call command_loop_step(done, step_status)
            if (done .or. exit_requested) exit
        end do
        macro_started = .false.
        loop_handles_errors = saved_handles
        exit_requested = .false.
    end subroutine command_loop

    !> Run a command loop until a command throws to `exit': return when
    !> it throws nil, signal quit when it throws t.  End of input, or a
    !> request to leave Emacs, also ends it with a quit; the end of the
    !> keyboard macro being executed ends it as an exit.  Its command loop
    !> handles errors (internal_catch (Qexit, command_loop_2, Qerror)).
    !> Port of keyboard.c:recursive_edit_1 with Frecursive_edit's catch.
    recursive subroutine recursive_edit_1(status)
        type(status_type), intent(out) :: status
        type(status_type) :: step_status
        logical :: done, saved_handles, macro_end
        command_loop_level = command_loop_level + 1
        exit_pending = .false.
        saved_handles = loop_handles_errors
        loop_handles_errors = .true.
        macro_end = .false.
        exit_levels = 1
        if (allocated(exit_string)) deallocate (exit_string)
        do
            call command_loop_step(done, step_status)
            if (done .and. macro_active) macro_end = at_end_of_macro_p()
            if (exit_pending .or. done .or. exit_requested) exit
        end do
        loop_handles_errors = saved_handles
        ! command_loop: executing_kbd_macro = Qnil.
        macro_started = .false.
        if (macro_end .and. .not. exit_pending) then
            ! command_loop_1 returned at the end of the macro: as (throw
            ! 'exit nil).
            exit_pending = .true.
            exit_abort = .false.
        end if
        if (allocated(exit_string)) then
            call set_error(status, FM_GENERIC_ERROR, exit_string)
            deallocate (exit_string)
        else if (.not. exit_pending .or. exit_abort) then
            call set_error(status, FM_QUIT)
        end if
        command_loop_level = command_loop_level - 1
        if (exit_pending .and. exit_abort .and. exit_levels > 1) then
            ! Quit the enclosing recursive edits too.
            exit_levels = exit_levels - 1
        else
            exit_pending = .false.
            exit_abort = .false.
            exit_levels = 1
        end if
    end subroutine recursive_edit_1

    !> (throw 'exit nil), or with `abort' (throw 'exit t): end the
    !> innermost recursive edit once the current command returns.  With
    !> `abort', `levels' (default 1) is the number of nested recursive
    !> edits to quit (as minibuffer.el:minibuffer-quit-recursive-edit).
    subroutine throw_exit(abort, levels)
        logical, intent(in) :: abort
        integer, intent(in), optional :: levels
        exit_pending = .true.
        exit_abort = abort
        exit_levels = 1
        if (present(levels)) exit_levels = max(1, levels)
    end subroutine throw_exit

    !> (throw 'exit STRING): end the innermost recursive edit once the
    !> current command returns; it signals an error with message `msg'.
    !> Port of keyboard.c:Frecursive_edit's handling of a string thrown
    !> to `exit'.
    subroutine throw_exit_string(msg)
        character(len=*), intent(in) :: msg
        exit_pending = .true.
        exit_abort = .false.
        exit_levels = 1
        exit_string = msg
    end subroutine throw_exit_string

    !> Leave the innermost recursive edit or minibuffer.
    !> Port of keyboard.c:Fexit_recursive_edit.
    subroutine exit_recursive_edit(status)
        type(status_type), intent(out) :: status
        if (command_loop_level > 0) then
            call throw_exit(.false.)
        else
            call set_error(status, FM_USER_ERROR, "No recursive edit is in progress")
        end if
    end subroutine exit_recursive_edit

    !> Abort the command that started the innermost recursive edit or
    !> minibuffer.  Port of keyboard.c:Fabort_recursive_edit.
    subroutine abort_recursive_edit(status)
        type(status_type), intent(out) :: status
        if (command_loop_level > 0) then
            call throw_exit(.true.)
        else
            call set_error(status, FM_USER_ERROR, "No recursive edit is in progress")
        end if
    end subroutine abort_recursive_edit
end module keyboard_fm
