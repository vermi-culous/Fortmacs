! simple_shell_fm.f90 -- shell commands (M-!, M-|).
! Ported from GNU Emacs 31.1 lisp/simple.el (the shell command part),
! with lisp/subr.el call-process-shell-command and call-shell-region,
! lisp/window.el count-screen-lines and src/buffer.c
! set-buffer-major-mode.
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

!> Synchronous shell commands: `shell_command' (M-!) and
!> `shell_command_on_region' (M-|), their output in the echo area or in
!> *Shell Command Output*, error buffers, and
!> `shell_command_dont_erase_buffer'.  A submodule of simple_fm, as it
!> runs processes (callproc_fm sits above simple_fm).
!>
!> Asynchronous shell commands (a COMMAND ending in `&',
!> `async_shell_command' on M-&) run in *Async Shell Command* in Shell
!> Command mode, through comint's output filter.
!>
!> Not ported yet (Deferred): the `revert_buffer_function' of the
!> asynchronous output buffer, the help text of
!> shell-command--same-buffer-confirm (help-form), `display_buffer_alist'
!> actions such as display-buffer-no-window for the output buffer; shell
!> completion in the minibuffer (`minibuffer_local_shell_command_map'
!> has no TAB, M-<up>, M-<down> or M-RET bindings) and
!> `minibuffer_default_add_shell_commands'; the dired default of the
!> command; noncontiguous regions (REGION-NONCONTIGUOUS-P); coding
!> systems (C-x RET c); file name handlers for remote directories;
!> `mode_line_process' and `revert_buffer_function' in the output buffer
!> (the mode line shows no " - Exit [N]"); the `save-window-excursion'
!> display of the output buffer in `shell_command_set_point_after_cmd'
!> when no window shows it (point is set in the buffer); a resizable echo
!> area (`resize_mini_windows': the echo area is one line, so output of
!> more than one screen line is shown in its buffer); the default
!> `major_mode' other than Fundamental mode in `set_buffer_major_mode'.
submodule(simple_fm) simple_shell_fm
    use lisp_fm, only: nilp, stringp, xstring, xfixnum, make_buffer_ref
    use data_fm, only: integer_setting, logical_setting
    use buffer_fm, only: get_buffer, get_buffer_create, set_buffer, current_buffer, buffer_live_p, erase_buffer, &
                         generate_new_buffer_name, rename_buffer
    use search_fm, only: string_match
    use editfns_fm, only: point_min, delete_region, buffer_substring, buffer_size, bolp, bobp, mark_marker, &
                          mark_position, widen, narrow_to_region
    use marker_fm, only: set_marker
    use fileio_fm, only: expand_file_name, file_exists_p, file_attributes, file_stat_type, insert_file_contents, &
                         delete_file_internal, report_file_errno
    use files_fm, only: temporary_file_directory, abbreviate_file_name, file_relative_name
    use sysdep_fm, only: emacs_mkostemp, close_fd
    use callproc_fm, only: call_process, call_process_region, shell_file_name, init_callproc
    use window_fm, only: window_type, windows, get_buffer_window, display_buffer, set_window_point
    use frame_fm, only: selected_frame
    use keymap_fm, only: make_sparse_keymap, set_keymap_parent, keymap_live_p
    use minibuf_fm, only: read_from_minibuffer, minibuffer_local_map, minibuffer_string_type
    use help_fm, only: register_keymap_name
    use fns_fm, only: yes_or_no_p
    use process_fm, only: process_arg_type, make_process, get_buffer_process, kill_process, process_status, &
                          process_buffer, process_command, set_process_sentinel, set_process_filter
    use comint_fm, only: comint_output_filter, comint_term_environment
    use shell_fm, only: shell_command_mode
    use xdisp_fm, only: set_mode_line_process, mode_line_part_type
    implicit none(type, external)

    !> The processes of asynchronous shell commands whose buffer is to be
    !> displayed on their first output (async_shell_command_display_buffer
    !> false): upstream's disposable advice on their filter.
    type(handle_type), allocatable :: display_on_output(:)

    !> simple.el shell-command-saved-pos: (BUFFER . POS) pairs.
    type :: saved_pos_type
        type(handle_type) :: buffer
        integer(int64) :: pos = 0
    end type saved_pos_type

    type(saved_pos_type), allocatable :: shell_command_saved_pos(:)

contains

    !> Read a shell command from the minibuffer, and return it as a string.
    !> The arguments are the same as the ones of `read_from_minibuffer',
    !> except READ and KEYMAP are missing and HIST defaults
    !> to `shell_command_history'.  Port of simple.el:read-shell-command
    !> (without shell completion).
    module procedure read_shell_command
    type(minibuffer_string_type) :: defaults(1)
    character(len=:), allocatable :: hist
    hist = "shell_command_history"
    if (present(history)) hist = history
    if (.not. keymap_live_p(minibuffer_local_shell_command_map)) call syms_of_simple_shell()
    ! (minibuffer-local-map may have been rebuilt since.)
    if (keymap_live_p(minibuffer_local_map)) call set_keymap_parent(minibuffer_local_shell_command_map, minibuffer_local_map, &
                                                                    status)
    if (present(default)) then
        defaults(1)%s = default
        call read_from_minibuffer(prompt, result, status, initial=initial, keymap=minibuffer_local_shell_command_map, &
                                  history=hist, defaults=defaults, history_position=history_position)
    else
        call read_from_minibuffer(prompt, result, status, initial=initial, keymap=minibuffer_local_shell_command_map, &
                                  history=hist, history_position=history_position)
    end if
    end procedure read_shell_command

    !> Store a buffer position or erase the buffer.
    !> Optional argument OUTPUT_TO_CURRENT_BUFFER, if true, means that the
    !> output of the shell command goes to the caller current buffer.
    !>
    !> See `shell_command_dont_erase_buffer'.  Port of
    !> simple.el:shell-command-save-pos-or-erase.
    subroutine shell_command_save_pos_or_erase(output_to_current_buffer)
        logical, intent(in) :: output_to_current_buffer
        type(saved_pos_type), allocatable :: grown(:)
        type(status_type) :: st
        integer(int64) :: pos
        integer :: n
        buffers(cur)%read_only = .false.
        pos = 0
        select case (shell_command_dont_erase_buffer%value)
        case (DONT_ERASE_SAVE_POINT)
            if (.not. output_to_current_buffer) pos = point()
        case (DONT_ERASE_BEG_LAST_OUT)
            if (.not. output_to_current_buffer) pos = point_max()
        case (DONT_ERASE_ERASE)
            call erase_buffer(st)
        case (DONT_ERASE_NIL)
            if (.not. output_to_current_buffer) call erase_buffer(st)
        case default
        end select
        if (pos /= 0) then
            call goto_char(point_max())
            if (.not. allocated(shell_command_saved_pos)) allocate (shell_command_saved_pos(0))
            n = size(shell_command_saved_pos)
            allocate (grown(n + 1))
            grown(1)%buffer = current_buffer()
            grown(1)%pos = pos
            grown(2:) = shell_command_saved_pos
            call move_alloc(grown, shell_command_saved_pos)
        end if
    end subroutine shell_command_save_pos_or_erase

    !> Set point in BUFFER after command complete.
    !> BUFFER is the output buffer of the command.
    !> Set point to the position recorded for BUFFER in
    !> `shell_command_saved_pos'.  Port of
    !> simple.el:shell-command-set-point-after-cmd.
    subroutine shell_command_set_point_after_cmd(buffer)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: win, old
        type(status_type) :: st
        integer(int64) :: pos, pmax
        integer :: k
        if (shell_command_dont_erase_buffer%value == DONT_ERASE_NIL) return
        pos = 0
        if (allocated(shell_command_saved_pos)) then
            do k = 1, size(shell_command_saved_pos)
                if (same_buffer(shell_command_saved_pos(k)%buffer, buffer)) then
                    pos = shell_command_saved_pos(k)%pos
                    exit
                end if
            end do
            ! assq-delete-all.
            shell_command_saved_pos = pack(shell_command_saved_pos, &
                                           [(.not. same_buffer(shell_command_saved_pos(k)%buffer, buffer), &
                                             k=1, size(shell_command_saved_pos))])
        end if
        if (.not. buffer_live_p(buffer)) return
        win = get_buffer_window(buffer)
        old = current_buffer()
        call set_buffer(buffer, st)
        pmax = point_max()
        ! The first time we run a command in a freshly created buffer we
        ! have not saved positions yet; advance to `point_max', so that
        ! successive commands know where to start.
        if (pos == 0 .or. .not. (shell_command_dont_erase_buffer%value == DONT_ERASE_SAVE_POINT &
                                 .or. shell_command_dont_erase_buffer%value == DONT_ERASE_BEG_LAST_OUT &
                                 .or. shell_command_dont_erase_buffer%value == DONT_ERASE_END_LAST_OUT)) pos = pmax
        ! Set point in the window displaying buf, if any; otherwise in the
        ! buffer.
        if (win%index /= 0 .and. .not. same_buffer(win, selected_frame%selected_window)) then
            call set_window_point(windows(win%index), pos, st)
        else
            call goto_char(pos)
        end if
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine shell_command_set_point_after_cmd

    logical function same_buffer(a, b)
        type(handle_type), intent(in) :: a, b
        same_buffer = a%index == b%index .and. a%generation == b%generation
    end function same_buffer

    !> A new empty file for error output: "scor" with a unique suffix in
    !> `temporary_file_directory'.  Port of the make-temp-file call of
    !> simple.el:shell-command (small-temporary-file-directory is nil).
    subroutine make_error_file(file, status)
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pattern
        integer(int32) :: fd, err
        call expand_file_name("scorXXXXXX", pattern, status, temporary_file_directory())
        if (.not. status%ok()) return
        file = pattern
        err = emacs_mkostemp(file, fd)
        if (err /= 0) then
            call report_file_errno("Creating file with prefix", pattern(1:len(pattern) - 6), err, status)
            return
        end if
        call close_fd(fd)
    end subroutine make_error_file

    !> The size of file `file' (0 if it doesn't exist).
    integer(int64) function file_size(file)
        character(len=*), intent(in) :: file
        type(file_stat_type) :: st
        logical :: found
        call file_attributes(file, st, found)
        file_size = 0
        if (found) file_size = st%size
    end function file_size

    !> Execute string COMMAND in inferior shell; display output, if any.
    !> With prefix argument, insert the COMMAND's output at point.
    !>
    !> Interactively, prompt for COMMAND in the minibuffer.
    !> If `shell_command_prompt_show_cwd' is true, show the current
    !> directory in the prompt.
    !>
    !> If COMMAND ends in `&', execute it asynchronously (Deferred: an
    !> error).
    !>
    !> Otherwise, COMMAND is executed synchronously.  The output appears in
    !> the buffer named by `shell_command_buffer_name'.  If the output is
    !> short enough to display in the echo area, it is shown there, but it
    !> is nonetheless available in buffer named by
    !> `shell_command_buffer_name' even though that buffer is not
    !> automatically displayed.
    !>
    !> The optional argument OUTPUT_BUFFER (or OUTPUT_BUFFER_NAME), says
    !> to put the output in some other buffer.  If OUTPUT_BUFFER is a
    !> buffer or buffer name, erase that buffer and insert the output
    !> there; a value of `shell_command_dont_erase_buffer' other than
    !> DONT_ERASE_NIL or DONT_ERASE_ERASE prevents the buffer from being
    !> erased.  If INSERT_HERE is true (which happens
    !> interactively when the prefix argument is given), insert the output
    !> in current buffer after point leaving mark after it.  This cannot be
    !> done asynchronously.
    !>
    !> If OUTPUT_BUFFER is a buffer or buffer name different from the
    !> current buffer, instead of outputting at point in that buffer, the
    !> output will be appended at the end of that buffer.
    !>
    !> The user option `shell_command_dont_erase_buffer', which see,
    !> controls whether the output buffer is erased and where to put point
    !> after the shell command.
    !>
    !> If the command terminates without error, but generates output, and
    !> you did not specify "insert it in the current buffer", the output
    !> can be displayed in the echo area or in its buffer.  If the output
    !> is short enough to display in the echo area, it is shown there.
    !> Otherwise, the buffer containing the output is displayed.
    !>
    !> If there is output and an error, and you did not specify "insert it
    !> in the current buffer", a message about the error goes at the end of
    !> the output.
    !>
    !> If the optional argument ERROR_BUFFER is present, it is the name of
    !> a buffer to which to direct the command's standard error output.  If
    !> it is absent, error output is mingled with regular output.  In an
    !> interactive call, the variable `shell_command_default_error_buffer'
    !> specifies the value of ERROR_BUFFER.
    !>
    !> In Fortran code, you will often be better served by calling
    !> `call_process' or `start_process' directly, since they offer more
    !> control and do not impose the use of a shell (with its need to quote
    !> arguments).  Port of simple.el:shell-command.
    module procedure shell_command
    character(len=:), allocatable :: error_file
    type(handle_type) :: ebuf, win, here
    type(status_type) :: st
    type(value_type) :: result
    integer(int64) :: pos_from_end, inserted, omark
    logical :: to_current, insert_flag, has_output_buffer
    insert_flag = .false.
    if (present(insert_here)) insert_flag = insert_here
    has_output_buffer = present(output_buffer) .or. present(output_buffer_name) .or. insert_flag
    to_current = .false.
    if (has_output_buffer .and. .not. async_command_p(command)) then
        here = current_buffer()
        if (present(output_buffer)) then
            to_current = same_buffer(output_buffer, here)
        else if (present(output_buffer_name)) then
            to_current = same_buffer(get_buffer(output_buffer_name), here)
        else
            to_current = .true.
        end if
    end if
    if (to_current) then
        ! Synchronous command with output in current buffer.
        error_file = ""
        if (present(error_buffer)) then
            call make_error_file(error_file, status)
            if (.not. status%ok()) return
        end if
        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) then
            if (len(error_file) > 0) call delete_file_internal(error_file, st)
            return
        end if
        call push_mark(nomsg=.true.)
        call shell_command_save_pos_or_erase(.true.)
        if (len(error_file) > 0) then
            call call_process_shell_command(command, result, status, error_file)
        else
            call call_process_shell_command(command, result, status)
        end if
        if (len(error_file) > 0) then
            if (file_exists_p(error_file)) then
                if (file_size(error_file) > 0) then
                    here = current_buffer()
                    call get_buffer_create(error_buffer, ebuf, st)
                    call set_buffer(ebuf, st)
                    pos_from_end = point_max() - point()
                    if (.not. bobp()) call insert(achar(12)//new_line("a"), st)
                    ! Do no formatting while reading error file, because
                    ! that can run a shell command.
                    call insert_file_contents(error_file, .false., inserted, st)
                    ! Put point after the inserted errors.
                    call goto_char(point_max() - pos_from_end)
                    call display_buffer(current_buffer(), win, st)
                    call set_buffer(here, st)
                end if
                call delete_file_internal(error_file, st)
            end if
        end if
        if (.not. status%ok()) return
        ! This is like exchange-point-and-mark, but doesn't activate the
        ! mark.
        omark = mark_position()
        call set_marker_to_point()
        call goto_char(omark)
    else if (async_command_p(command)) then
        ! Command ending with ampersand means asynchronous.
        call async_shell_command_1(command, status, output_buffer, output_buffer_name, insert_flag)
    else
        ! Otherwise, command is executed synchronously.
        call shell_command_on_region(point(), point(), command, status, output_buffer=output_buffer, &
                                     output_buffer_name=output_buffer_name, insert_here=insert_here, &
                                     error_buffer=error_buffer)
    end if
contains

    subroutine set_marker_to_point()
        type(handle_type) :: m
        type(status_type) :: s
        call mark_marker(m)
        call set_marker(m, point(), s)
    end subroutine set_marker_to_point
    end procedure shell_command

    !> The asynchronous branch of shell-command: run COMMAND (ending in
    !> `&') with its output in OUTPUT_BUFFER, the buffer OUTPUT_BUFFER_NAME
    !> or `shell_command_buffer_name_async' (an error with INSERT_HERE).
    !> Part of simple.el:shell-command.
    subroutine async_shell_command_1(command_in, status, output_buffer, output_buffer_name, insert_here)
        character(len=*), intent(in) :: command_in
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: output_buffer
        character(len=*), intent(in), optional :: output_buffer_name
        logical, intent(in) :: insert_here
        type(handle_type) :: buffer, proc, old, win
        type(process_arg_type), allocatable :: env(:), argv(:)
        type(mode_line_part_type) :: parts(1)
        type(status_type) :: st
        character(len=:), allocatable :: command, bname, directory
        character(len=16) :: cols
        integer :: k
        if (insert_here .and. .not. (present(output_buffer) .or. present(output_buffer_name))) then
            call set_error(status, FM_GENERIC_ERROR, "Asynchronous shell commands cannot output to current buffer")
            return
        end if
        ! Remove the ampersand and test if the command is not empty before
        ! creating a new buffer.
        k = len(command_in)
        do while (k >= 1)
            if (command_in(k:k) /= " " .and. command_in(k:k) /= achar(9)) exit
            k = k - 1
        end do
        k = k - 1
        do while (k >= 1)
            if (command_in(k:k) /= " " .and. command_in(k:k) /= achar(9)) exit
            k = k - 1
        end do
        command = command_in(1:k)
        if (len(command) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Empty asynchronous command")
            return
        end if
        if (present(output_buffer)) then
            buffer = output_buffer
        else if (present(output_buffer_name)) then
            call get_buffer_create(output_buffer_name, buffer, status)
        else
            call get_buffer_create(shell_command_buffer_name_async, buffer, status)
        end if
        if (.not. status%ok()) return
        bname = buffers(buffer%index)%name
        proc = get_buffer_process(buffer)
        directory = ""
        if (allocated(buffers(cur)%directory)) directory = buffers(cur)%directory
        ! Ask the user what to do with already running process.
        if (proc%index /= 0) then
            select case (async_shell_command_buffer%value)
            case (ASYNC_CONFIRM_KILL_PROCESS)
                ! If will kill a process, query first.
                call same_buffer_confirm("Kill it", status)
                if (.not. status%ok()) return
                call kill_process(st, proc)
            case (ASYNC_CONFIRM_NEW_BUFFER)
                ! If will create a new buffer, query first.
                call same_buffer_confirm("Use a new buffer", status)
                if (.not. status%ok()) return
                call get_buffer_create(generate_new_buffer_name(bname), buffer, status)
            case (ASYNC_NEW_BUFFER)
                ! It will create a new buffer.
                call get_buffer_create(generate_new_buffer_name(bname), buffer, status)
            case (ASYNC_CONFIRM_RENAME_BUFFER)
                ! If will rename the buffer, query first.
                call same_buffer_confirm("Rename it", status)
                if (.not. status%ok()) return
                call rename_uniquely_in(buffer)
                call get_buffer_create(bname, buffer, status)
            case (ASYNC_RENAME_BUFFER)
                ! It will rename the buffer.
                call rename_uniquely_in(buffer)
                call get_buffer_create(bname, buffer, status)
            case default
            end select
            if (.not. status%ok()) return
        end if
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call shell_command_save_pos_or_erase(.false.)
        buffers(cur)%directory = directory
        ! (Connection-local values of shell_file_name and
        ! shell_command_switch are not ported.)
        allocate (env(0))
        if (async_shell_command_width%value >= 0) then
            write (cols, "(i0)") async_shell_command_width%value
            call append_arg(env, "COLUMNS="//trim(cols))
        end if
        block
            type(process_arg_type), allocatable :: term(:)
            call comint_term_environment(term)
            env = [env, term]
        end block
        allocate (argv(3))
        argv(1)%s = shell_program()
        argv(2)%s = shell_switch()
        argv(3)%s = command
        call make_process("Shell", argv, proc, status, buffer=buffer, environment=env)
        if (.not. status%ok()) then
            if (buffer_live_p(old)) call set_buffer(old, st)
            return
        end if
        parts(1)%text = ":%s"
        parts(1)%face = ""
        call set_mode_line_process(parts)
        if (.not. associated(async_shell_command_mode%value)) call syms_of_simple_shell()
        call async_shell_command_mode%value(status)
        if (.not. status%ok()) return
        call set_process_sentinel(proc, st, shell_command_sentinel)
        ! Use the comint filter for proper handling of carriage motion (see
        ! comint-inhibit-carriage-motion).
        call set_process_filter(proc, st, comint_output_filter)
        if (async_shell_command_display_buffer%value) then
            ! Display buffer immediately.
            call display_buffer(buffer, win, st)
        else
            ! Defer displaying buffer until first process output.
            call set_process_filter(proc, st, display_then_filter)
            if (.not. allocated(display_on_output)) allocate (display_on_output(0))
            display_on_output = [display_on_output, proc]
        end if
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine async_shell_command_1

    !> Ask before using the default buffer taken by another command; an
    !> error if the answer is no.  Port of
    !> simple.el:shell-command--same-buffer-confirm.
    subroutine same_buffer_confirm(action, status)
        character(len=*), intent(in) :: action
        type(status_type), intent(out) :: status
        logical :: answer
        call yes_or_no_p("A command is running in the default buffer.  "//action//"? ", answer, status)
        if (.not. status%ok()) return
        if (.not. answer) call set_error(status, FM_USER_ERROR, "Shell command in progress")
    end subroutine same_buffer_confirm

    !> Rename BUFFER to a similar name not already taken.  Port of
    !> files.el:rename-uniquely (in BUFFER).
    subroutine rename_uniquely_in(buffer)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: base_name, actual
        integer(int64) :: idx
        old = current_buffer()
        call set_buffer(buffer, st)
        base_name = buffers(cur)%name
        idx = string_match("<[0-9]+>\'", base_name, st)
        if (idx /= 0) then
            ! If the existing buffer name has a <NNN>, which isn't part of
            ! the file name (if any), then get rid of that.
            if (.not. allocated(buffers(cur)%filename)) then
                base_name = base_name(1:idx - 1)
            else if (base_name /= file_nondirectory(buffers(cur)%filename)) then
                base_name = base_name(1:idx - 1)
            end if
        end if
        call rename_buffer(generate_new_buffer_name(base_name), .false., actual, st)
        call set_buffer(old, st)
    end subroutine rename_uniquely_in

    function file_nondirectory(file) result(name)
        use fileio_fm, only: file_name_nondirectory
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: name
        name = file_name_nondirectory(file)
    end function file_nondirectory

    !> The filter of an asynchronous shell command whose buffer is
    !> displayed on its first output: display it, then filter as Comint
    !> does from then on.
    recursive subroutine display_then_filter(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, win
        type(status_type) :: st
        logical, allocatable :: keep(:)
        integer :: k
        buf = process_buffer(process)
        if (buffer_live_p(buf)) then
            if (allocated(display_on_output)) then
                allocate (keep(size(display_on_output)))
                do k = 1, size(display_on_output)
                    keep(k) = .not. same_buffer(display_on_output(k), process)
                end do
                display_on_output = pack(display_on_output, keep)
            end if
            call set_process_filter(process, st, comint_output_filter)
            call display_buffer(buf, win, st)
        end if
        call comint_output_filter(process, string, status)
    end subroutine display_then_filter

    module procedure shell_command_sentinel
    type(process_arg_type), allocatable :: cmd(:)
    character(len=:), allocatable :: state
    status = status_type()
    state = process_status(process)
    if (state /= "exit" .and. state /= "signal") return
    call shell_command_set_point_after_cmd(process_buffer(process))
    call process_command(process, cmd)
    if (size(cmd) >= 3 .and. len(signal) >= 1) then
        call message(cmd(3)%s//": "//signal(1:len(signal) - 1)//".")
    end if
    end procedure shell_command_sentinel

    module procedure async_shell_command
    character(len=:), allocatable :: cmd
    type(status_type) :: st
    cmd = command
    if (string_match("&["//achar(9)//" ]*\'", cmd, st) == 0) cmd = cmd//" &"
    ! (ERROR_BUFFER is ignored.)
    if (present(error_buffer)) then
        if (len(error_buffer) < 0) return
    end if
    if (present(output_buffer)) then
        call shell_command(cmd, status, output_buffer=output_buffer)
    else if (present(output_buffer_name)) then
        call shell_command(cmd, status, output_buffer_name=output_buffer_name)
    else
        call shell_command(cmd, status)
    end if
    end procedure async_shell_command

    !> The command: read COMMAND (with the visited file as default).  The
    !> interactive form of simple.el:async-shell-command.
    !>
    !> Execute string COMMAND asynchronously in background.
    subroutine cmd_async_shell_command(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: command
        if (allocated(buffers(cur)%filename)) then
            call read_shell_command(shell_prompt("Async shell command"), command, status, &
                                    default=file_relative_name(buffers(cur)%filename))
        else
            call read_shell_command(shell_prompt("Async shell command"), command, status)
        end if
        if (.not. status%ok()) return
        call async_shell_command(command, status)
    end subroutine cmd_async_shell_command

    !> `shell_file_name' (set from SHELL when not set yet).
    function shell_program() result(name)
        character(len=:), allocatable :: name
        if (.not. allocated(shell_file_name%value)) call init_callproc()
        name = shell_file_name%value
    end function shell_program

    !> `shell_command_switch' ("-c" when not set yet).
    function shell_switch() result(switch)
        character(len=:), allocatable :: switch
        if (.not. allocated(shell_command_switch%value)) shell_command_switch = "-c"
        switch = shell_command_switch%value
    end function shell_switch

    !> Whether COMMAND ends in `&' (with blanks around it): the
    !> "[ \t]*&[ \t]*\\'" match of shell-command.
    logical function async_command_p(command)
        character(len=*), intent(in) :: command
        integer :: k
        async_command_p = .false.
        k = len(command)
        do while (k >= 1)
            if (command(k:k) /= " " .and. command(k:k) /= achar(9)) exit
            k = k - 1
        end do
        if (k < 1) return
        async_command_p = command(k:k) == "&"
    end function async_command_p

    !> Execute the shell command COMMAND synchronously in separate process,
    !> inserting the output in the current buffer before point, with the
    !> standard error in ERROR_FILE if present.  Port of
    !> subr.el:call-process-shell-command (BUFFER t or (t ERROR-FILE)).
    subroutine call_process_shell_command(command, result, status, error_file)
        character(len=*), intent(in) :: command
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: error_file
        call call_process(shell_program(), result, status, destination=t_value(), &
                          args=list(make_string(shell_switch()), make_string(command)), &
                          error_file=error_file)
    end subroutine call_process_shell_command

    !> Display MESSAGE (the contents of buffer BUFFER) in the echo area if
    !> possible, otherwise in a pop-up buffer.  A pop-up buffer is
    !> displayed using `display_buffer' if MESSAGE is too long for the echo
    !> area (one line).  Port of simple.el:display-message-or-buffer
    !> (MESSAGE a buffer; `resize_mini_windows' is not ported).
    module procedure display_message_or_buffer
    type(handle_type) :: old, win
    type(status_type) :: st
    character(len=:), allocatable :: text
    integer(int64) :: lines
    old = current_buffer()
    call set_buffer(buffer, status)
    if (.not. status%ok()) return
    if (buffer_size() == 0) then
        lines = 0
    else
        lines = count_screen_lines()
    end if
    ! Don't use the echo area if the output buffer is already displayed
    ! in the selected frame.
    win = get_buffer_window(current_buffer())
    if (lines == 0) then
        continue
    else if (lines <= 1 .and. win%index == 0) then
        ! Echo area.
        call goto_char(point_max())
        if (bolp()) call goto_char(point() - 1)
        call buffer_substring(point_min(), point(), text, st)
        call message(text)
    else
        ! Buffer.
        call goto_char(point_min())
        call display_buffer(current_buffer(), win, status)
    end if
    if (buffer_live_p(old)) call set_buffer(old, st)
    end procedure display_message_or_buffer

    !> Return the number of screen lines in the current buffer, as lines
    !> as wide as the minibuffer window, not counting a final newline.
    !> Port of window.el:count-screen-lines (BEG and END the whole buffer,
    !> WINDOW the minibuffer window).
    integer(int64) function count_screen_lines() result(lines)
        integer(int64) :: opoint, obegv, ozv, e, moved
        type(status_type) :: st
        opoint = point()
        obegv = buffers(cur)%begv
        ozv = buffers(cur)%zv
        call widen()
        e = ozv
        if (e > obegv) then
            if (char_before_at(e) == 10) e = e - 1
        end if
        call narrow_to_region(obegv, e, st)
        call goto_char(point_min())
        if (selected_frame%minibuffer_window%index /= 0) then
            call vertical_motion(buffer_size(), moved, window=windows(selected_frame%minibuffer_window%index))
        else
            call vertical_motion(buffer_size(), moved)
        end if
        lines = moved + 1
        call widen()
        call narrow_to_region(obegv, ozv, st)
        call goto_char(opoint)
    end function count_screen_lines

    !> The character before position `pos' (0 if none).
    integer function char_before_at(pos)
        integer(int64), intent(in) :: pos
        integer(int32) :: c
        logical :: found
        call char_before(c, found, pos)
        char_before_at = c
    end function char_before_at

    !> Set an appropriate major mode for BUFFER: the default `major_mode'
    !> (Fundamental mode).  Port of buffer.c:Fset_buffer_major_mode.
    subroutine set_buffer_major_mode(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st
        if (.not. buffer_live_p(buffer)) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to set major mode for a dead buffer")
            return
        end if
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call fundamental_mode(status)
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine set_buffer_major_mode

    !> Execute string COMMAND in inferior shell with region as input.
    !> Normally display output (if any) in temp buffer specified by
    !> `shell_command_buffer_name'; prefix arg means replace the region
    !> with it.  Return the exit code of COMMAND.
    !>
    !> If the command generates output, the output may be displayed in the
    !> echo area or in a buffer.  If the output is short enough to display
    !> in the echo area, it is shown there.  Otherwise it is displayed in
    !> the buffer named by `shell_command_buffer_name'.  The output is
    !> available in that buffer in both cases.
    !>
    !> If there is output and an error, a message about the error appears
    !> at the end of the output.
    !>
    !> Optional OUTPUT_BUFFER (or OUTPUT_BUFFER_NAME) specifies where to
    !> put the command's output: erase that buffer and insert the output
    !> there; a value of `shell_command_dont_erase_buffer' other than
    !> DONT_ERASE_NIL or DONT_ERASE_ERASE prevents erasing the buffer.  If absent, use the buffer specified by
    !> `shell_command_buffer_name'.  A true INSERT_HERE means to insert the
    !> output in the current buffer after START.
    !>
    !> Optional REPLACE, if true, means to insert the output in place of
    !> text from START to END, putting point and mark around it.  If
    !> NO_MARK is also true, don't set the mark.
    !>
    !> Optional ERROR_BUFFER, if present, names a buffer to which to direct
    !> the command's standard error output.  If absent, error output is
    !> mingled with regular output.  When called interactively,
    !> `shell_command_default_error_buffer' is used for ERROR_BUFFER.
    !>
    !> Optional DISPLAY_ERROR_BUFFER, if true, means to display the error
    !> buffer if there were any errors.  When called interactively, this is
    !> true.
    !>
    !> EXIT_STATUS is the exit code (-1 if the command was killed by a
    !> signal, whose description is then SIGNAL_DESCRIPTION).
    !>
    !> If COMMAND names a shell (e.g., via `shell_file_name'), keep in mind
    !> that behavior of various shells when commands are piped to their
    !> standard input is shell- and system-dependent, and thus non-portable.
    !> Port of simple.el:shell-command-on-region (contiguous regions).
    module procedure shell_command_on_region
    character(len=:), allocatable :: error_file, output, directory, num
    type(handle_type) :: buffer, ebuf, old, win
    type(value_type) :: exit_value
    type(status_type) :: st, cst
    integer(int64) :: inserted
    logical :: replacing, no_mark_flag, insert_flag, swap, have_exit, use_current
    replacing = .false.
    if (present(replace)) replacing = replace
    no_mark_flag = .false.
    if (present(no_mark)) no_mark_flag = no_mark
    insert_flag = .false.
    if (present(insert_here)) insert_flag = insert_here .and. .not. (present(output_buffer) .or. present(output_buffer_name))
    if (present(exit_status)) exit_status = 0
    have_exit = .false.
    error_file = ""
    if (present(error_buffer)) then
        call make_error_file(error_file, status)
        if (.not. status%ok()) return
    end if
    if (replacing .or. insert_flag) then
        ! Replace specified region with output from command.
        swap = replacing .and. start < end
        ! Don't muck with mark unless REPLACE says we should.
        call goto_char(start)
        if (replacing .and. .not. no_mark_flag) call push_mark(point(), nomsg=.true.)
        call call_shell_region(start, end, command, replacing, exit_value, cst, error_file)
        if (cst%ok()) then
            have_exit = .true.
            ! Don't muck with mark unless REPLACE says we should.
            if (replacing .and. swap .and. .not. no_mark_flag) call exchange_point_and_mark(.false., st)
        end if
    else
        ! No prefix argument: put the output in a temp buffer, replacing
        ! its entire contents.
        if (present(output_buffer)) then
            buffer = output_buffer
        else if (present(output_buffer_name)) then
            call get_buffer_create(output_buffer_name, buffer, status)
            if (.not. status%ok()) return
        else
            call get_buffer_create(shell_command_buffer_name, buffer, status)
            if (.not. status%ok()) return
        end if
        ! Enable globalized modes (bug#38111).
        call set_buffer_major_mode(buffer, st)
        use_current = same_buffer(buffer, current_buffer())
        if (use_current) then
            use_current = shell_command_dont_erase_buffer%value == DONT_ERASE_NIL .or. &
                          shell_command_dont_erase_buffer%value == DONT_ERASE_ERASE
            if (.not. use_current) then
                use_current = .not. same_buffer(buffer, get_buffer(shell_command_buffer_name)) .and. &
                              .not. region_active_p()
            end if
        end if
        if (use_current) then
            ! If the input is the same buffer as the output, delete
            ! everything but the specified region, then replace that region
            ! with the output.
            buffers(cur)%read_only = .false.
            call delete_region(max(start, end), point_max(), st)
            call delete_region(point_min(), min(start, end), st)
            if (len(error_file) > 0) then
                call call_process_region(shell_program(), exit_value, cst, point_min(), point_max(), .true., &
                                         t_value(), list(make_string(shell_switch()), make_string(command)), &
                                         error_file)
            else
                call call_process_region(shell_program(), exit_value, cst, point_min(), point_max(), .true., &
                                         t_value(), list(make_string(shell_switch()), make_string(command)))
            end if
        else
            ! Clear the output buffer, then run the command with output
            ! there.
            directory = ""
            if (allocated(buffers(cur)%directory)) directory = buffers(cur)%directory
            old = current_buffer()
            call set_buffer(buffer, st)
            if (.not. (present(output_buffer) .or. present(output_buffer_name))) buffers(cur)%directory = directory
            call shell_command_save_pos_or_erase(.false.)
            call set_buffer(old, st)
            call call_shell_region(start, end, command, .false., exit_value, cst, error_file, buffer)
        end if
        have_exit = cst%ok()
        ! Report the output.
        old = current_buffer()
        call set_buffer(buffer, st)
        if (buffer_size() > 0) then
            ! There's some output, display it.
            call set_buffer(old, st)
            call display_message_or_buffer(buffer, st)
            call shell_command_set_point_after_cmd(buffer)
        else
            call set_buffer(old, st)
            ! No output; error?
            output = "no output"
            if (len(error_file) > 0) then
                if (file_size(error_file) > 0) then
                    output = "some error output"
                    if (allocated(shell_command_default_error_buffer%value)) then
                        output = output//" to the """//shell_command_default_error_buffer%value//""" buffer"
                    end if
                end if
            end if
            if (.not. have_exit) then
                call message("(Shell command failed with error)")
            else if (stringp(exit_value)) then
                call message("(Shell command killed by signal "//xstring(exit_value)//")")
            else if (xfixnum(exit_value) == 0) then
                call message("(Shell command succeeded with "//output//")")
            else
                allocate (character(len=24) :: num)
                write (num, "(i0)") xfixnum(exit_value)
                call message("(Shell command failed with code "//trim(num)//" and "//output//")")
            end if
            ! Don't kill: there might be useful info in the undo-log.
        end if
    end if
    if (len(error_file) > 0) then
        if (file_exists_p(error_file)) then
            if (file_size(error_file) > 0) then
                old = current_buffer()
                call get_buffer_create(error_buffer, ebuf, st)
                call set_buffer(ebuf, st)
                call goto_char(point_max())
                ! Insert a separator if there's already text here.
                if (.not. bobp()) call insert(achar(12)//new_line("a"), st)
                ! Do no formatting while reading error file, because that
                ! can run a shell command.
                call insert_file_contents(error_file, .false., inserted, st)
                if (present(display_error_buffer)) then
                    if (display_error_buffer) call display_buffer(current_buffer(), win, st)
                end if
                if (buffer_live_p(old)) call set_buffer(old, st)
            end if
            call delete_file_internal(error_file, st)
        end if
    end if
    status = cst
    if (.not. have_exit) return
    if (present(exit_status)) then
        if (stringp(exit_value)) then
            exit_status = -1
        else
            exit_status = int(xfixnum(exit_value))
        end if
    end if
    if (present(signal_description)) then
        signal_description = ""
        if (stringp(exit_value)) signal_description = xstring(exit_value)
    end if
    end procedure shell_command_on_region

    !> Send text from START to END as input to an inferior shell running
    !> COMMAND.  Delete the text if DELETE is true.  Insert output in
    !> BUFFER (the current buffer if absent) before point, the standard
    !> error going to ERROR_FILE unless it is "".  Port of
    !> subr.el:call-shell-region.
    subroutine call_shell_region(start, end, command, delete, result, status, error_file, buffer)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: command, error_file
        logical, intent(in) :: delete
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        type(value_type) :: destination
        if (present(buffer)) then
            destination = make_buffer_ref(buffer)
        else
            destination = t_value()
        end if
        if (len(error_file) > 0) then
            call call_process_region(shell_program(), result, status, start, end, delete, destination, &
                                     list(make_string(shell_switch()), make_string(command)), error_file)
        else
            call call_process_region(shell_program(), result, status, start, end, delete, destination, &
                                     list(make_string(shell_switch()), make_string(command)))
        end if
    end subroutine call_shell_region

    !> The prompt of `shell_command': with the current directory if
    !> `shell_command_prompt_show_cwd'.
    function shell_prompt(what) result(prompt)
        character(len=*), intent(in) :: what
        character(len=:), allocatable :: prompt
        character(len=:), allocatable :: dir
        if (shell_command_prompt_show_cwd%value) then
            dir = ""
            if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
            prompt = format_message(what//" in `")//abbreviate_file_name(dir)//format_message("': ")
        else
            prompt = what//": "
        end if
    end function shell_prompt

    !> The command: read COMMAND (with the visited file as default), and
    !> insert the output at point with a prefix argument.  The interactive
    !> form of simple.el:shell-command.
    !>
    !> Execute string COMMAND in inferior shell; display output, if any.
    !> With prefix argument, insert the COMMAND's output at point.
    subroutine cmd_shell_command(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: command
        logical :: prefix
        prefix = .not. nilp(current_prefix_arg)
        if (allocated(buffers(cur)%filename)) then
            call read_shell_command(shell_prompt("Shell command"), command, status, &
                                    default=file_relative_name(buffers(cur)%filename))
        else
            call read_shell_command(shell_prompt("Shell command"), command, status)
        end if
        if (.not. status%ok()) return
        if (allocated(shell_command_default_error_buffer%value)) then
            call shell_command(command, status, insert_here=prefix, error_buffer=shell_command_default_error_buffer%value)
        else
            call shell_command(command, status, insert_here=prefix)
        end if
    end subroutine cmd_shell_command

    !> The command: the region, then COMMAND read in the minibuffer; with
    !> a prefix argument the output replaces the region.  The interactive
    !> form of simple.el:shell-command-on-region.
    !>
    !> Execute string COMMAND in inferior shell with region as input.
    !> Normally display output (if any) in temp buffer specified by
    !> `shell_command_buffer_name'; prefix arg means replace the region
    !> with it.
    subroutine cmd_shell_command_on_region(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: string
        integer(int64) :: beg, end, m
        logical :: prefix
        prefix = .not. nilp(current_prefix_arg)
        call mark(m, status)
        if (.not. status%ok()) return
        if (m == 0) then
            call set_error(status, FM_USER_ERROR, "The mark is not set now, so there is no region")
            return
        end if
        ! Do this before calling region-beginning and region-end, in case
        ! subprocess output relocates them while we are in the minibuffer.
        call read_shell_command("Shell command on region: ", string, status)
        if (.not. status%ok()) return
        call region_bounds(beg, end, status)
        if (.not. status%ok()) return
        if (allocated(shell_command_default_error_buffer%value)) then
            call shell_command_on_region(beg, end, string, status, insert_here=prefix, replace=prefix, &
                                         error_buffer=shell_command_default_error_buffer%value, &
                                         display_error_buffer=.true.)
        else
            call shell_command_on_region(beg, end, string, status, insert_here=prefix, replace=prefix, &
                                         display_error_buffer=.true.)
        end if
    end subroutine cmd_shell_command_on_region

    !> The region's limits, as region-beginning and region-end give them.
    subroutine region_bounds(beg, end, status)
        use editfns_fm, only: region_beginning, region_end
        integer(int64), intent(out) :: beg, end
        type(status_type), intent(out) :: status
        call region_beginning(beg, status)
        if (.not. status%ok()) return
        call region_end(end, status)
    end subroutine region_bounds

    !> Register the shell commands and build
    !> `minibuffer_local_shell_command_map'.
    module procedure syms_of_simple_shell
    type(status_type) :: st
    if (.not. allocated(shell_command_switch%value)) shell_command_switch = "-c"
    commands%shell_command%run => cmd_shell_command
    commands%shell_command_on_region%run => cmd_shell_command_on_region
    commands%async_shell_command%run => cmd_async_shell_command
    if (.not. associated(async_shell_command_mode%value)) async_shell_command_mode%value => shell_command_mode
    if (.not. keymap_live_p(minibuffer_local_shell_command_map)) then
        call make_sparse_keymap(minibuffer_local_shell_command_map)
    end if
    if (keymap_live_p(minibuffer_local_map)) then
        call set_keymap_parent(minibuffer_local_shell_command_map, minibuffer_local_map, st)
    end if
    call register_keymap_name("minibuffer_local_shell_command_map", minibuffer_local_shell_command_map)
    end procedure syms_of_simple_shell

    !> Add S at the end of LIST.
    subroutine append_arg(list, s)
        type(process_arg_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(process_arg_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))
        do k = 1, n
            if (allocated(list(k)%s)) call move_alloc(list(k)%s, grown(k)%s)
        end do
        grown(n + 1)%s = s
        call move_alloc(grown, list)
    end subroutine append_arg

    !> Add S at the start of LIST.
    subroutine prepend_arg(list, s)
        type(process_arg_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(process_arg_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))
        grown(1)%s = s
        do k = 1, n
            if (allocated(list(k)%s)) call move_alloc(list(k)%s, grown(k + 1)%s)
        end do
        call move_alloc(grown, list)
    end subroutine prepend_arg
end submodule simple_shell_fm
