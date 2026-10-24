! minibuf_fm.f90 -- minibuffer input.
! Ported from GNU Emacs 31.1 src/minibuf.c.
!
! Copyright (C) 1985-1986, 1993-2026 Free Software Foundation, Inc.
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

!> Reading a line of input in the minibuffer: the prompt is read-only
!> text in a field at the start of buffer " *Minibuf-N*", shown in the
!> frame's minibuffer window, which is selected while a recursive edit
!> runs; a command throws to `exit' to finish.
!>
!> The completion primitives (`try_completion', `all_completions',
!> `test_completion') are here too; the completion UI is minibuffer_fm's.
!>
!> Not yet: `completion_regexp_list' (no regexps), hash-table
!> collections, minibuffer-only frames, input methods, Lisp
!> reading (`read_minibuffer'), `minibuffer_setup_hook', the
!> minibuffer-prompt face, and restoring window configurations.
module minibuf_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, t_value, list, nilp, make_string, make_list, list_elements, &
                       make_symbol_ref, stringp, xstring, symbolp, xsymbol, symbols, nsymbols, V_LIST, vtag
    use lread_fm, only: intern_soft
    use fns_fm, only: compare_strings, COMPARE_EQUAL, schars, substring
    use lread_fm, only: sym
    use data_fm, only: set, symbol_value, kill_all_local_variables, logical_setting
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, &
                         erase_buffer, resolve_buffer, inhibit_read_only
    use insdel_fm, only: insert
    use editfns_fm, only: field_end, buffer_substring, goto_char
    use textprop_fm, only: add_text_properties, get_text_property
    use keymap_fm, only: make_sparse_keymap, use_local_map, keymap_live_p
    use keyboard_fm, only: recursive_edit_1, throw_exit, throw_exit_string, command_loop_level, last_message
    use frame_fm, only: selected_frame, frame_selected_window_ref, minibuffer_window_ref
    use window_fm, only: window_type, set_window_buffer, window_configuration_type, current_window_configuration, &
                         set_window_configuration, free_window_configuration, minibuffer_scroll_window
    use subr_fm, only: add_to_history
    implicit none(type, external)
    private

    public :: read_from_minibuffer, read_string, minibuffer_prompt_end, minibuffer_contents, &
              minibufferp, abort_minibuffers, init_minibuf, minibuffer_innermost_command_loop_p, &
              innermost_minibuffer_p, minibuffer_depth_of, minibuffer_setup_proc
    public :: try_completion, all_completions, test_completion, completion_list_table, &
              completion_obarray_table, completion_function_table, completion_predicate, &
              completion_function

    !> Actions passed to a completion function (programmed completion): as
    !> `try_completion' (nil), `all_completions' (t), `test_completion'
    !> (lambda), and `(boundaries . SUFFIX)'.
    integer, parameter, public :: COMPLETE_TRY = 0, COMPLETE_ALL = 1, COMPLETE_TEST = 2, &
                                  COMPLETE_BOUNDARIES = 3

    !> Kinds of completion table: a list (of strings, symbols, or lists
    !> whose first element is one: an alist), the obarray, or a function.
    integer, parameter, public :: TABLE_LIST = 0, TABLE_OBARRAY = 1, TABLE_FUNCTION = 2

    abstract interface
        !> A completion predicate: whether the element `elt' (the list
        !> element or the symbol) is an acceptable completion.
        logical function completion_predicate(elt)
            import :: value_type
            implicit none(type, external)
            type(value_type), intent(in) :: elt
        end function completion_predicate

        !> A function completion table.  ACTION says what to compute for
        !> STRING: COMPLETE_TRY (the `try_completion' value: nil, t or a
        !> string), COMPLETE_ALL (a list of strings), COMPLETE_TEST (t or
        !> nil) or COMPLETE_BOUNDARIES (a list (START END) for STRING and
        !> SUFFIX, or nil for the default).
        recursive subroutine completion_function(string, action, result, status, pred, suffix)
            import :: value_type, status_type, completion_predicate
            implicit none(type, external)
            character(len=*), intent(in) :: string
            integer, intent(in) :: action
            type(value_type), intent(out) :: result
            type(status_type), intent(out) :: status
            procedure(completion_predicate), optional :: pred
            character(len=*), intent(in), optional :: suffix
        end subroutine completion_function

        !> A function run once the minibuffer is set up (see
        !> `minibuffer_setup_function').
        subroutine minibuffer_setup_proc()
            implicit none(type, external)
        end subroutine minibuffer_setup_proc
    end interface

    !> A completion table (COLLECTION).
    type, public :: completion_table_type
        integer :: kind = TABLE_LIST
        !> TABLE_LIST: the list.
        type(value_type) :: list
        !> TABLE_FUNCTION: the function.
        procedure(completion_function), pointer, nopass :: fn => null()
    end type completion_table_type

    !> True means don't consider case significant in completion.
    !> For file-name completion, `read_file_name_completion_ignore_case'
    !> controls the behavior, rather than this variable.
    !> For buffer name completion, `read_buffer_completion_ignore_case'
    !> controls the behavior, rather than this variable.  Port of minibuf.c
    !> completion-ignore-case.
    type(logical_setting), public :: completion_ignore_case = logical_setting(name="completion_ignore_case")
    !> minibuf_level: the depth of active minibuffers.
    integer, public, protected :: minibuf_level = 0
    !> True means to allow minibuffer commands while in the minibuffer.
    !> This variable makes a difference whenever the minibuffer window is active.
    !> Port of minibuf.c enable-recursive-minibuffers.
    logical, public :: enable_recursive_minibuffers = .false.
    !> True means restore window configurations on exit from minibuffer.
    !> If this is true (the default), reading input with the minibuffer
    !> will restore, on exit, the window configuration of the frame where
    !> the minibuffer was entered from.
    !>
    !> If this is false, window configurations are not restored upon
    !> exiting the minibuffer.  Port of minibuf.c
    !> read-minibuffer-restore-windows (`minibuffer_restore_windows' in
    !> `minibuffer_exit_hook' is deferred).
    logical, public :: read_minibuffer_restore_windows = .true.
    !> minibuffer-local-map, the default keymap for reading.
    type(handle_type), public :: minibuffer_local_map
    !> True means to add new elements in history.
    !> If set to false, minibuffer reading functions don't add new elements to the
    !> history list, so it is possible to do this afterwards by calling
    !> `add_to_history' explicitly.  Port of minibuf.c history-add-new-input.
    logical, public :: history_add_new_input = .true.

    !> Vminibuffer_history_variable: the history list's symbol name.
    character(len=:), allocatable, public :: minibuffer_history_variable
    !> Vminibuffer_history_position: the element shown (0: the input).
    integer, public :: minibuffer_history_position = 0
    !> A string in a list of strings.
    type, public :: minibuffer_string_type
        character(len=:), allocatable :: s
    end type minibuffer_string_type

    !> minibuffer-default: the default values, for M-n ("future
    !> history"); unallocated (or empty) for nil.
    type(minibuffer_string_type), allocatable, public :: minibuffer_default(:)
    !> simple.el minibuffer-temporary-goal-position: where M-n and M-p
    !> leave point (0 for the end), kept while they repeat.
    integer(int64), public :: minibuffer_temporary_goal_position = 0
    !> simple.el minibuffer-text-before-history: the input before moving
    !> into the history; unallocated for nil.
    character(len=:), allocatable, public :: minibuffer_text_before_history

    !> A string in an array.
    type :: string_box
        character(len=:), allocatable :: s
    end type string_box

    !> The buffer that was current when the innermost active minibuffer
    !> was entered (the buffer of minibuffer-selected-window); the null
    !> handle when no minibuffer is active.
    type(handle_type), public, protected :: minibuffer_calling_buffer

    !> Vminibuffer_list: the minibuffer of each depth (index depth + 1).
    type(handle_type), allocatable :: minibuffers(:)
    !> Vcommand_loop_level_list: the command loop level of each depth's
    !> recursive edit (index depth + 1).
    integer, allocatable :: minibuffer_loop_levels(:)

    !> A function to run once by the next `read_from_minibuffer', after
    !> the minibuffer is current and set up (its local keymap made, the
    !> initial input inserted), as minibuffer-setup-hook would run one
    !> added by subr.el:minibuffer-with-setup-hook; null for none.  It is
    !> cleared before it runs, so recursive minibuffers don't run it.
    procedure(minibuffer_setup_proc), pointer, public :: minibuffer_setup_function => null()

    !> Whether the last `read_from_minibuffer' added its input (or
    !> default) to the history list, so that it is the list's first
    !> element (what read-file-name tests with `eq').
    logical, public, protected :: history_added = .false.

contains

    !> Create minibuffer-local-map (its bindings are made by minibuffer.el
    !> and bindings.el, i.e. minibuffer_fm and bindings_fm).
    subroutine init_minibuf()
        if (.not. keymap_live_p(minibuffer_local_map)) call make_sparse_keymap(minibuffer_local_map)
        minibuf_level = 0
    end subroutine init_minibuf

    !> The minibuffer for depth `depth', " *Minibuf-DEPTH*", made if
    !> needed.  Port of minibuf.c:get_minibuffer.
    subroutine get_minibuffer(depth, buf, status)
        integer, intent(in) :: depth
        type(handle_type), intent(out) :: buf
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: grown(:)
        integer, allocatable :: grown_levels(:)
        character(len=20) :: num
        integer(int64) :: i
        if (.not. allocated(minibuffers)) allocate (minibuffers(4))
        if (.not. allocated(minibuffer_loop_levels)) then
            allocate (minibuffer_loop_levels(4))
            minibuffer_loop_levels = 0
        end if
        if (depth + 1 > size(minibuffers)) then
            allocate (grown(2*(depth + 1)))
            grown(1:size(minibuffers)) = minibuffers
            call move_alloc(grown, minibuffers)
            allocate (grown_levels(2*(depth + 1)))
            grown_levels = 0
            grown_levels(1:size(minibuffer_loop_levels)) = minibuffer_loop_levels
            call move_alloc(grown_levels, minibuffer_loop_levels)
        end if
        call resolve_buffer(minibuffers(depth + 1), i, status)
        if (status%ok()) then
            buf = minibuffers(depth + 1)
            return
        end if
        write (num, "(i0)") depth
        call get_buffer_create(" *Minibuf-"//trim(num)//"*", buf, status)
        if (.not. status%ok()) return
        minibuffers(depth + 1) = buf
    end subroutine get_minibuffer

    !> Whether `buf' (default: the current buffer) is a minibuffer.
    !> Port of minibuf.c:Fminibufferp.
    logical function minibufferp(buf)
        type(handle_type), intent(in), optional :: buf
        type(handle_type) :: b
        integer :: k
        if (present(buf)) then
            b = buf
        else
            b = current_buffer()
        end if
        minibufferp = .false.
        if (.not. allocated(minibuffers)) return
        do k = 1, size(minibuffers)
            if (minibuffers(k)%index == b%index .and. minibuffers(k)%generation == b%generation .and. &
                b%index /= 0) then
                minibufferp = .true.
                return
            end if
        end do
    end function minibufferp

    !> The depth of the active minibuffer `buf' (default: the current
    !> buffer), or 0 if it is not one.  Port of
    !> minibuf.c:this_minibuffer_depth.
    integer function minibuffer_depth_of(buf) result(depth)
        type(handle_type), intent(in), optional :: buf
        type(handle_type) :: b
        integer :: k
        if (present(buf)) then
            b = buf
        else
            b = current_buffer()
        end if
        depth = 0
        if (.not. allocated(minibuffers) .or. b%index == 0) return
        do k = 2, min(minibuf_level + 1, size(minibuffers))
            if (minibuffers(k)%index == b%index .and. minibuffers(k)%generation == b%generation) then
                depth = k - 1
                return
            end if
        end do
    end function minibuffer_depth_of

    !> Return true if BUFFER is a minibuffer at the current command loop level.
    !> BUFFER defaults to the current buffer.  Port of
    !> minibuf.c:Fminibuffer_innermost_command_loop_p.
    logical function minibuffer_innermost_command_loop_p(buffer) result(innermost)
        type(handle_type), intent(in), optional :: buffer
        integer :: depth
        depth = minibuffer_depth_of(buffer)
        innermost = depth > 0
        if (innermost) innermost = minibuffer_loop_levels(depth + 1) == command_loop_level
    end function minibuffer_innermost_command_loop_p

    !> Return true if BUFFER is the most nested active minibuffer.
    !> No recursive minibuffers are active if BUFFER is the minibuffer.
    !> BUFFER defaults to the current buffer.  Port of
    !> minibuf.c:Finnermost_minibuffer_p.
    logical function innermost_minibuffer_p(buffer) result(innermost)
        type(handle_type), intent(in), optional :: buffer
        innermost = minibuffer_depth_of(buffer) == minibuf_level .and. minibuf_level > 0
    end function innermost_minibuffer_p

    !> Where the prompt ends in the current minibuffer (point-min if the
    !> current buffer is not one).  Port of minibuf.c:Fminibuffer_prompt_end.
    integer(int64) function minibuffer_prompt_end() result(pos)
        type(value_type) :: field
        type(status_type) :: status
        pos = buffers(cur)%begv
        if (.not. minibufferp()) return
        pos = field_end(buffers(cur)%begv, .false.)
        if (pos == buffers(cur)%zv) then
            call get_text_property(buffers(cur)%begv, sym("field"), field, status)
            if (nilp(field)) pos = buffers(cur)%begv
        end if
    end function minibuffer_prompt_end

    !> The text of the current minibuffer after the prompt.  Port of
    !> minibuf.c:Fminibuffer_contents_no_properties.
    function minibuffer_contents() result(text)
        character(len=:), allocatable :: text
        type(status_type) :: status
        call buffer_substring(minibuffer_prompt_end(), buffers(cur)%zv, text, status)
    end function minibuffer_contents

    !> Read a string in the minibuffer with `prompt', starting with
    !> `initial' (point after it), using `keymap' (default
    !> minibuffer-local-map) and history list `history' (default
    !> "minibuffer-history").  C-g (abort-minibuffers) gives a quit.
    !> `default' is the default value (for M-n and the history), or
    !> `defaults' a list of them (the first is the default).  Port of
    !> minibuf.c:read_minibuf with Fread_from_minibuffer (no READ, no
    !> INHERIT-INPUT-METHOD; DEFAULT only for the history).
    !> minibuffer-mode is not ported: as set_minibuffer_mode does when it
    !> is not defined, the minibuffer's local variables are killed.
    recursive subroutine read_from_minibuffer(prompt, result, status, initial, keymap, history, default, defaults, &
                                              position, history_position)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: initial, history, default
        type(handle_type), intent(in), optional :: keymap
        type(minibuffer_string_type), intent(in), optional :: defaults(:)
        !> With INITIAL, point goes to this one-indexed position in it (as
        !> for an INITIAL of the form (STRING . POSITION)).
        integer(int64), intent(in), optional :: position
        !> The initial history position (as for a HIST of the form
        !> (SYMBOL . POSITION)); default 0.
        integer, intent(in), optional :: history_position
        type(handle_type) :: calling_buffer, minibuffer, old_mini_buffer, saved_calling
        type(window_configuration_type) :: config
        type(window_type), pointer :: sw, mw
        type(status_type) :: st, edit_status
        type(value_type) :: histval, saved_inhibit
        character(len=:), allocatable :: ambient_dir, saved_histvar, histvar, saved_before, histstring
        type(minibuffer_string_type), allocatable :: saved_default(:)
        integer(int64) :: saved_goal
        integer :: saved_histpos
        logical :: had_default, had_before, have_histstring
        logical :: changed, old_selected, have_dir
        procedure(minibuffer_setup_proc), pointer :: setup
        result = ""
        history_added = .false.
        if (.not. enable_recursive_minibuffers .and. minibuf_level > 0) then
            if (selected_frame%minibuffer_selected) then
                call set_error(status, FM_USER_ERROR, "Command attempted to use minibuffer while in minibuffer")
            else
                ! If we're in another window, cancel the minibuffer that's
                ! active.
                call throw_exit_string("Command attempted to use minibuffer while in minibuffer")
                call set_error(status, FM_GENERIC_ERROR, "Command attempted to use minibuffer while in minibuffer")
            end if
            return
        end if
        histvar = "minibuffer_history"
        if (present(history)) histvar = history
        calling_buffer = current_buffer()
        have_dir = allocated(buffers(cur)%directory)
        if (have_dir) ambient_dir = buffers(cur)%directory
        call get_minibuffer(minibuf_level + 1, minibuffer, status)
        if (.not. status%ok()) return
        minibuf_level = minibuf_level + 1
        ! Vcommand_loop_level_list: the level of this minibuffer's
        ! recursive edit.
        minibuffer_loop_levels(minibuf_level + 1) = command_loop_level + 1
        saved_calling = minibuffer_calling_buffer
        minibuffer_calling_buffer = calling_buffer
        ! minibuf_save_list
        if (allocated(minibuffer_history_variable)) then
            saved_histvar = minibuffer_history_variable
        else
            saved_histvar = ""
        end if
        saved_histpos = minibuffer_history_position
        minibuffer_history_variable = histvar
        minibuffer_history_position = 0
        if (present(history_position)) minibuffer_history_position = history_position
        had_default = allocated(minibuffer_default)
        if (had_default) call move_alloc(minibuffer_default, saved_default)
        if (present(defaults)) then
            if (size(defaults) > 0) minibuffer_default = defaults
        else if (present(default)) then
            allocate (minibuffer_default(1))
            minibuffer_default(1)%s = default
        end if
        saved_goal = minibuffer_temporary_goal_position
        minibuffer_temporary_goal_position = 0
        had_before = allocated(minibuffer_text_before_history)
        if (had_before) call move_alloc(minibuffer_text_before_history, saved_before)
        ! If variable is unbound, make it nil.
        call symbol_value(sym(histvar), histval, st)
        if (.not. st%ok()) call set(sym(histvar), nil(), st)
        old_selected = selected_frame%minibuffer_selected
        sw => frame_selected_window_ref()
        mw => minibuffer_window_ref()
        old_mini_buffer = mw%buffer
        if (read_minibuffer_restore_windows) call current_window_configuration(config)
        minibuffer_scroll_window = selected_frame%selected_window
        call set_buffer(minibuffer, status)
        if (.not. status%ok()) then
            minibuf_level = minibuf_level - 1
            minibuffer_calling_buffer = saved_calling
            return
        end if
        ! set_minibuffer_mode
        call kill_all_local_variables()
        ! The minibuffer works in the caller's directory.
        if (have_dir) buffers(cur)%directory = ambient_dir
        call set_window_buffer(selected_frame%minibuffer_window, minibuffer, st)
        selected_frame%minibuffer_selected = .true.
        mw%hscroll = 0
        ! Erase the buffer.
        ! (specbind Qinhibit_read_only Qnil), restored on exit
        saved_inhibit = inhibit_read_only()
        call set(sym("inhibit_read_only"), t_value(), st)
        call erase_buffer(st)
        buffers(cur)%multibyte = .true.
        ! Insert the prompt, record where it ends.
        if (len(prompt) > 0) then
            call insert(prompt, st)
            ! front-sticky, rear-nonsticky and field, then
            ! minibuffer-prompt-properties (by default read-only t and the
            ! face minibuffer_prompt, added after any faces of the prompt).
            call add_text_properties(1_int64, buffers(cur)%pt, &
                                     list(sym("front_sticky"), t_value(), sym("rear_nonsticky"), t_value(), &
                                          sym("field"), t_value(), sym("read_only"), t_value()), changed, st)
            call add_text_properties(1_int64, buffers(cur)%pt, list(sym("face"), sym("minibuffer_prompt")), &
                                     changed, st)
        end if
        call set(sym("inhibit_read_only"), nil(), st)
        if (present(initial)) then
            call insert(initial, st)
            if (present(position)) then
                ! Convert to distance from end of input.
                if (position < 1) then
                    ! A number too small means the beginning of the string.
                    call goto_char(buffers(cur)%pt - schars(initial))
                else
                    call goto_char(buffers(cur)%pt + max(position - 1 - schars(initial), -schars(initial)))
                end if
            end if
        end if
        if (present(keymap)) then
            call use_local_map(keymap, st)
        else
            call use_local_map(minibuffer_local_map, st)
        end if
        ! The minibuffer replaces whatever the echo area showed.
        if (allocated(last_message)) last_message = ""
        ! minibuffer-setup-hook (a function `minibuffer_with_setup_hook'
        ! added for this minibuffer).
        if (associated(minibuffer_setup_function)) then
            setup => minibuffer_setup_function
            minibuffer_setup_function => null()
            call setup()
        end if
        call recursive_edit_1(edit_status)
        ! Make the minibuffer contents into a string.
        call set_buffer(minibuffer, st)
        result = minibuffer_contents()
        ! Choose the string to add to the history.
        have_histstring = .true.
        if (len(result) /= 0) then
            histstring = result
        else if (present(defaults)) then
            if (size(defaults) > 0) then
                histstring = defaults(1)%s
            else
                have_histstring = .false.
            end if
        else if (present(default)) then
            histstring = default
        else
            have_histstring = .false.
        end if
        ! read_minibuf_unwind
        minibuf_level = minibuf_level - 1
        call set(sym("inhibit_read_only"), t_value(), st)
        call erase_buffer(st)
        call set(sym("inhibit_read_only"), saved_inhibit, st)
        minibuffer_calling_buffer = saved_calling
        selected_frame%minibuffer_selected = old_selected
        ! (Windows may have been made meanwhile: fetch them again.)
        mw => minibuffer_window_ref()
        mw%buffer = old_mini_buffer
        if (read_minibuffer_restore_windows) then
            call set_window_configuration(config)
            call free_window_configuration(config)
        end if
        call set_buffer(calling_buffer, st)
        minibuffer_history_variable = saved_histvar
        minibuffer_history_position = saved_histpos
        if (allocated(minibuffer_default)) deallocate (minibuffer_default)
        if (had_default) call move_alloc(saved_default, minibuffer_default)
        minibuffer_temporary_goal_position = saved_goal
        if (allocated(minibuffer_text_before_history)) deallocate (minibuffer_text_before_history)
        if (had_before) call move_alloc(saved_before, minibuffer_text_before_history)
        if (.not. edit_status%ok()) then
            status = edit_status
            result = ""
            return
        end if
        ! Add the value to the appropriate history list, if any.
        if (history_add_new_input .and. have_histstring) then
            call add_to_history(histvar, histstring, added=history_added)
        end if
    end subroutine read_from_minibuffer

    !> Read a string; empty input gives `default' if present.
    !> Port of minibuf.c:Fread_string.
    recursive subroutine read_string(prompt, result, status, initial, history, default, position)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: initial, history, default
        integer(int64), intent(in), optional :: position
        call read_from_minibuffer(prompt, result, status, initial=initial, history=history, &
                                  default=default, position=position)
        if (.not. status%ok()) return
        if (len(result) == 0 .and. present(default)) result = default
    end subroutine read_string

    !> Abort the current minibuffer.
    !> If we are not currently in the innermost minibuffer, prompt the user to
    !> confirm the aborting of the current minibuffer and all contained ones.
    !> Port of minibuf.c:Fabort_minibuffers (`ask' stands for
    !> `yes_or_no_p', which this module cannot use).
    recursive subroutine abort_minibuffers(status, ask)
        type(status_type), intent(out) :: status
        interface
            recursive subroutine ask(prompt, answer, status)
                import :: status_type
                implicit none(type, external)
                character(len=*), intent(in) :: prompt
                logical, intent(out) :: answer
                type(status_type), intent(out) :: status
            end subroutine ask
        end interface
        integer :: depth
        character(len=20) :: num
        logical :: answer
        status = status_type()
        depth = minibuffer_depth_of()
        if (depth == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Not in a minibuffer")
            return
        end if
        if (.not. minibuffer_innermost_command_loop_p()) then
            call set_error(status, FM_GENERIC_ERROR, "Not in most nested command loop")
            return
        end if
        if (depth < minibuf_level) then
            write (num, "(i0)") minibuf_level - depth + 1
            call ask("Abort "//trim(num)//" minibuffer levels? ", answer, status)
            if (.not. status%ok()) return
            ! Due to the above check, the current minibuffer is in the
            ! most nested command loop, which means that we don't have to
            ! abort any extra non-minibuffer recursive edits.  Thus, the
            ! number of recursive edits we have to abort equals the number
            ! of minibuffers we have to abort.
            if (answer) call throw_exit(.true., minibuf_level - depth + 1)
        else
            ! minibuffer-quit-recursive-edit
            call throw_exit(.true.)
        end if
    end subroutine abort_minibuffers

    ! ------------------------------------------------------------------
    ! Completion primitives
    ! ------------------------------------------------------------------

    !> A completion table holding the list `list' (strings, symbols, or
    !> lists whose first element is the completion).
    function completion_list_table(list) result(table)
        type(value_type), intent(in) :: list
        type(completion_table_type) :: table
        table%kind = TABLE_LIST
        table%list = list
    end function completion_list_table

    !> A completion table holding the names of all interned symbols.
    function completion_obarray_table() result(table)
        type(completion_table_type) :: table
        table%kind = TABLE_OBARRAY
    end function completion_obarray_table

    !> A completion table computed by `fn' (programmed completion).
    function completion_function_table(fn) result(table)
        procedure(completion_function) :: fn
        type(completion_table_type) :: table
        table%kind = TABLE_FUNCTION
        table%fn => fn
    end function completion_function_table

    !> The elements of a non-function table: `elts' (the list elements or
    !> symbols, given to the predicate) and the string each one completes
    !> to (`names'; `ok' is false for elements that are not strings or
    !> symbols).
    subroutine table_elements(table, elts, names, ok)
        type(completion_table_type), intent(in) :: table
        type(value_type), allocatable, intent(out) :: elts(:)
        type(string_box), allocatable, intent(out) :: names(:)
        logical, allocatable, intent(out) :: ok(:)
        type(value_type), allocatable :: parts(:)
        type(value_type) :: eltstring
        integer(int64) :: k
        if (table%kind == TABLE_OBARRAY) then
            allocate (elts(nsymbols), names(nsymbols), ok(nsymbols))
            do k = 1, nsymbols
                elts(k) = make_symbol_ref(k)
                names(k)%s = symbols(k)%name
                ok(k) = .true.
            end do
            return
        end if
        call list_elements(table%list, elts)
        allocate (names(size(elts)), ok(size(elts)))
        do k = 1, size(elts, kind=int64)
            ! An alist element: its car is the completion.
            eltstring = elts(k)
            if (vtag(elts(k)) == V_LIST) then
                call list_elements(elts(k), parts)
                eltstring = nil()
                if (size(parts) > 0) eltstring = parts(1)
            end if
            ok(k) = .true.
            if (stringp(eltstring)) then
                names(k)%s = xstring(eltstring)
            else if (symbolp(eltstring)) then
                names(k)%s = symbols(xsymbol(eltstring))%name
            else
                ok(k) = .false.
                names(k)%s = ""
            end if
        end do
    end subroutine table_elements

    !> Whether completion candidate `name' begins with STRING.
    logical function prefix_matches(string, name)
        character(len=*), intent(in) :: string, name
        integer(int64) :: n
        n = schars(string)
        prefix_matches = n <= schars(name)
        if (prefix_matches) prefix_matches = compare_strings(name, string, completion_ignore_case%value, end1=n) &
                                             == COMPARE_EQUAL
    end function prefix_matches

    !> Return longest common substring of all completions of STRING in
    !> COLLECTION.
    !>
    !> Test each possible completion specified by COLLECTION to see if it
    !> begins with STRING.  The possible completions may be strings or
    !> symbols.  Symbols are converted to strings before testing, by using
    !> `symbol_name'.
    !>
    !> If no possible completions match, the function returns nil; if
    !> there's just one exact match, it returns t; otherwise it returns
    !> the longest initial substring common to all possible completions
    !> that begin with STRING.
    !>
    !> If COLLECTION is an alist, the keys (cars of elements) are the
    !> possible completions.  If an element is not a cons cell, then the
    !> element itself is a possible completion.  If COLLECTION is the
    !> obarray, the names of all symbols in the obarray are the possible
    !> completions.
    !>
    !> COLLECTION can also be a function to do the completion itself.  It
    !> receives STRING, PREDICATE and COMPLETE_TRY.  Whatever it returns
    !> becomes the value of `try_completion'.
    !>
    !> If optional argument PREDICATE is present, it is used to test each
    !> possible completion.  A possible completion is accepted only if
    !> PREDICATE returns true.  The argument given to PREDICATE is either a
    !> string or a list (whose first element is a string) from the alist,
    !> or a symbol from the obarray.
    !>
    !> If `completion_ignore_case' is non-nil, possible completions are
    !> matched while ignoring letter-case, but no guarantee is made about
    !> the letter-case of the return value, except that it comes either
    !> from the user's input or from one of the possible completions.
    !> Port of minibuf.c:Ftry_completion (no `completion_regexp_list').
    recursive subroutine try_completion(string, collection, result, status, predicate)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: collection
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        type(value_type), allocatable :: elts(:)
        type(string_box), allocatable :: names(:)
        logical, allocatable :: ok(:)
        character(len=:), allocatable :: bestmatch, old_bestmatch, eltstring
        integer(int64) :: bestmatchsize, compare, matchsize, tem, k, nstring, neltstring, nbest
        integer :: matchcount
        logical :: have_best
        status = status_type()
        result = nil()
        if (collection%kind == TABLE_FUNCTION) then
            call collection%fn(string, COMPLETE_TRY, result, status, predicate)
            return
        end if
        call table_elements(collection, elts, names, ok)
        have_best = .false.
        bestmatchsize = 0
        matchcount = 0
        nstring = schars(string)
        do k = 1, size(elts, kind=int64)
            ! Is this element a possible completion?
            if (.not. ok(k)) cycle
            eltstring = names(k)%s
            if (.not. prefix_matches(string, eltstring)) cycle
            ! Ignore this element if there is a predicate and the
            ! predicate doesn't like it.
            if (present(predicate)) then
                if (.not. predicate(elts(k))) cycle
            end if
            neltstring = schars(eltstring)
            ! Update computation of how much all possible completions match.
            if (.not. have_best) then
                matchcount = 1
                bestmatch = eltstring
                bestmatchsize = neltstring
                have_best = .true.
            else
                compare = min(bestmatchsize, neltstring)
                tem = compare_strings(bestmatch, eltstring, completion_ignore_case%value, end1=compare, &
                                      end2=compare)
                matchsize = compare
                if (tem /= COMPARE_EQUAL) matchsize = abs(tem) - 1
                old_bestmatch = bestmatch
                nbest = schars(bestmatch)
                if (completion_ignore_case%value) then
                    ! If this is an exact match except for case, use it as
                    ! the best match rather than one that is not an exact
                    ! match.  This way, we get the case pattern of the
                    ! actual match.  If there is more than one exact match
                    ! ignoring case, and one of them is exact including
                    ! case, prefer that one.  If there is no exact match
                    ! ignoring case, prefer a match that does not change
                    ! the case of the input.
                    if ((matchsize == neltstring .and. matchsize < nbest) .or. &
                        ((matchsize == neltstring) .eqv. (matchsize == nbest)) .and. &
                        compare_strings(eltstring, string, end1=nstring) == COMPARE_EQUAL .and. &
                        compare_strings(bestmatch, string, end1=nstring) /= COMPARE_EQUAL) then
                        bestmatch = eltstring
                    end if
                end if
                ! Don't count the same string multiple times.
                if (bestmatchsize /= neltstring .or. bestmatchsize /= matchsize) then
                    if (matchcount <= 1) matchcount = matchcount + 1
                else if (completion_ignore_case%value) then
                    if (compare_strings(old_bestmatch, eltstring, end1=compare, end2=compare) /= COMPARE_EQUAL &
                        .and. matchcount <= 1) matchcount = matchcount + 1
                end if
                bestmatchsize = matchsize
                ! No need to look any further (unless ignoring case, to find
                ! the best possible match *including* case differences).
                if (matchsize <= nstring .and. .not. completion_ignore_case%value .and. matchcount > 1) exit
            end if
        end do
        ! No completions found.
        if (.not. have_best) return
        ! Return t if the supplied string is an exact match (counting
        ! case); it does not require any change to be made.
        if (matchcount == 1 .and. bestmatch == string .and. len(bestmatch) == len(string)) then
            result = t_value()
            return
        end if
        ! Else extract the part in which all completions agree.
        result = make_string(substring(bestmatch, 0_int64, bestmatchsize))
    end subroutine try_completion

    !> Search for partial matches of STRING in COLLECTION.
    !>
    !> Test each possible completion specified by COLLECTION to see if it
    !> begins with STRING.  The possible completions may be strings or
    !> symbols.  Symbols are converted to strings before testing, by using
    !> `symbol_name'.
    !>
    !> The value is a list of all the possible completions that match
    !> STRING.
    !>
    !> If COLLECTION is an alist, the keys (cars of elements) are the
    !> possible completions.  If COLLECTION is the obarray, the names of
    !> all symbols in the obarray are the possible completions.
    !>
    !> COLLECTION can also be a function to do the completion itself.  It
    !> receives STRING, PREDICATE and COMPLETE_ALL.  Whatever it returns
    !> becomes the value of `all_completions'.
    !>
    !> If optional argument PREDICATE is present, it is used to test each
    !> possible completion.  A possible completion is accepted only if
    !> PREDICATE returns true.  Port of minibuf.c:Fall_completions (no
    !> `completion_regexp_list').
    recursive subroutine all_completions(string, collection, result, status, predicate)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: collection
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: predicate
        type(value_type), allocatable :: elts(:), matches(:)
        type(string_box), allocatable :: names(:)
        logical, allocatable :: ok(:)
        integer(int64) :: k, n
        status = status_type()
        if (collection%kind == TABLE_FUNCTION) then
            call collection%fn(string, COMPLETE_ALL, result, status, predicate)
            return
        end if
        call table_elements(collection, elts, names, ok)
        allocate (matches(size(elts)))
        n = 0
        do k = 1, size(elts, kind=int64)
            ! Is this element a possible completion?
            if (.not. ok(k)) cycle
            if (.not. prefix_matches(string, names(k)%s)) cycle
            ! Ignore this element if there is a predicate and the
            ! predicate doesn't like it.
            if (present(predicate)) then
                if (.not. predicate(elts(k))) cycle
            end if
            ! Ok => put it on the list.
            n = n + 1
            matches(n) = make_string(names(k)%s)
        end do
        result = make_list(matches(1:n))
    end subroutine all_completions

    !> Return non-nil if STRING is a valid completion.
    !> For instance, if COLLECTION is a list of strings, STRING is a valid
    !> completion if it appears in the list and PREDICATE is satisfied.
    !>
    !> Takes the same arguments as `all_completions' and `try_completion'.
    !>
    !> If COLLECTION is a function, it is called with STRING, PREDICATE and
    !> COMPLETE_TEST (`lambda').  Port of minibuf.c:Ftest_completion (no
    !> `completion_regexp_list').
    recursive logical function test_completion(string, collection, predicate) result(valid)
        character(len=*), intent(in) :: string
        type(completion_table_type), intent(in) :: collection
        procedure(completion_predicate), optional :: predicate
        type(value_type), allocatable :: elts(:)
        type(string_box), allocatable :: names(:)
        logical, allocatable :: ok(:)
        type(value_type) :: tem, result
        type(status_type) :: status
        integer(int64) :: k, id
        valid = .false.
        select case (collection%kind)
        case (TABLE_FUNCTION)
            call collection%fn(string, COMPLETE_TEST, result, status, predicate)
            valid = status%ok() .and. .not. nilp(result)
            return
        case (TABLE_OBARRAY)
            id = intern_soft(string)
            if (id /= 0) then
                tem = make_symbol_ref(id)
            else if (completion_ignore_case%value) then
                do k = 1, nsymbols
                    if (compare_strings(string, symbols(k)%name, .true.) == COMPARE_EQUAL) then
                        id = k
                        tem = make_symbol_ref(k)
                        exit
                    end if
                end do
            end if
            if (id == 0) return
        case default
            ! assoc-string
            call table_elements(collection, elts, names, ok)
            id = 0
            do k = 1, size(elts, kind=int64)
                if (.not. ok(k)) cycle
                if (compare_strings(string, names(k)%s, completion_ignore_case%value) == COMPARE_EQUAL) then
                    id = k
                    tem = elts(k)
                    exit
                end if
            end do
            if (id == 0) return
        end select
        ! Finally, check the predicate.
        valid = .true.
        if (present(predicate)) valid = predicate(tem)
    end function test_completion
end module minibuf_fm
