! process_fm.f90 -- asynchronous subprocesses.
! Ported from GNU Emacs 31.1 src/process.c.
!
! Copyright (C) 1985-1988, 1993-1996, 1998-1999, 2001-2026 Free Software
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

!> Asynchronous subprocesses: `make_process', `start_process', their
!> status, filters and sentinels, signals and input.
!>
!>     type(handle_type) :: proc
!>     type(process_arg_type) :: args(1)
!>     args(1)%s = "-l"
!>     call start_process("ls", buffer, "ls", proc, status, args)
!>
!> A process object is a handle into a registry (with a generation
!> counter, so a freed one is detected).  Each running child has a worker
!> thread (src/sys/sys_worker_fm) that reads its output and reaps it; the
!> main thread takes the workers' messages while waiting for input
!> (keyboard_fm's `process_output_hook'), in `accept_process_output', or
!> in `sit_for', and only then runs the filters and sentinels, as upstream
!> does from wait_reading_process_output.  Output is decoded as UTF-8
!> (incomplete sequences at the end of a read wait for the next one).
!>
!> Process objects are not garbage collected: `free_process' frees one
!> (deleting it first if needed).  Deleted and exited processes keep
!> their object, as upstream, until freed.
!>
!> Not ported yet (Deferred): network, serial and pipe processes
!> (`make_network_process', `make_pipe_process', `make_serial_process')
!> and `:stderr'; a `:connection_type' of the form (INPUT . OUTPUT); coding
!> systems (`:coding', `set_process_coding_system'; output is UTF-8,
!> input is sent as UTF-8); `exec_path' (PATH is searched) and
!> `process_environment' (the editor's environment is passed); remote
!> directories and file name handlers; stopped processes (`stop_process',
!> `continue_process', `stop' statuses); `set_process_filter' with t
!> (stopping output); a CURRENT_GROUP of `lambda' (signal only when the
!> shell does not own the terminal); `signal_process'
!> with names, `interrupt_process_functions', `signal_process_functions';
!> the process plist (`process_get', `process_put'); threads locking
!> processes (`set_process_thread'); `set_process_window_size';
!> `process_running_child_p'; adaptive read buffering; `inhibit_sentinels';
!> the saving of the match data around filters and sentinels; quitting
!> (C-g) in `accept_process_output'; `list_processes' and the
!> `process_menu_mode'; `read_process_name' and the interactive form of
!> `kill_process'; the exec failure of a pty process is reported as an
!> error, as for a pipe process, not as its output and exit status 126 or
!> 127; `process-kill-buffer-query-function' (killing a buffer with a
!> process asks nothing); the query about active processes on exit.
module process_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR, FM_FILE_ERROR
    use handle_fm, only: handle_type, slot_table_type, null_handle
    use data_fm, only: logical_setting
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, set_buffer_internal, buffer_live_p, &
                         resolve_buffer, kill_buffer_processes_hook, vdeactivate_mark, BEG, buffer_handle
    use marker_fm, only: make_marker, free_marker, marker_buffer, marker_position, set_marker_both, &
                         set_marker, char_to_byte
    use insdel_fm, only: insert, insert_before_markers
    use editfns_fm, only: goto_char, widen, narrow_to_region, set_point_from_marker
    use character_fm, only: str_as_multibyte, str_as_unibyte
    use fileio_fm, only: file_directory_p, report_file_errno, expand_file_name
    use callproc_fm, only: get_current_directory, locate_program, init_callproc
    use sysdep_fm, only: sys_string_type, emacs_spawn_async, emacs_kill, ignore_sigpipe, emacs_full_write, &
                         close_fd, emacs_strsignal, emacs_strerror, emacs_dup_cloexec, emacs_num_processors, ENOENT, EPIPE
    use sys_worker_fm, only: worker_message_type, start_reader, take_worker_messages, worker_wake_fd, &
                             wait_for_worker_messages, worker_count, WORKER_OUTPUT, WORKER_EXIT
    use xdisp_fm, only: message, mode_line_process_status
    use keyboard_fm, only: process_output_hook, process_wake_fd
    use commands_fm, only: commands
    implicit none(type, external)
    private

    public :: process_arg_type, process_filter_proc, process_sentinel_proc
    public :: make_process, start_process, delete_process, free_process, processp, process_live_p, &
              get_process, get_buffer_process, process_list, process_status, process_exit_status, process_id, &
              process_name, process_command, process_buffer, set_process_buffer, process_mark, &
              set_process_filter, set_process_sentinel, process_tty_name, set_process_query_on_exit_flag, &
              process_filter, process_sentinel, &
              process_query_on_exit_flag, interrupt_process, kill_process, quit_process, signal_process, &
              process_send_string, process_send_region, process_send_eof, accept_process_output, &
              internal_default_process_filter, internal_default_process_sentinel, kill_buffer_processes, &
              status_message, syms_of_process, process_shutdown, dispatch_process_output, num_processors
    public :: PROCESS_CONNECTION_DEFAULT, PROCESS_CONNECTION_PIPE, PROCESS_CONNECTION_PTY

    !> One element of a command line.
    type :: process_arg_type
        character(len=:), allocatable :: s
    end type process_arg_type

    abstract interface
        !> A process filter: called with the process and each piece of its
        !> output (decoded text).
        subroutine process_filter_proc(process, string, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: process
            character(len=*), intent(in) :: string
            type(status_type), intent(out) :: status
        end subroutine process_filter_proc

        !> A process sentinel: called with the process and a string
        !> describing the change of its state ("finished\n").
        subroutine process_sentinel_proc(process, event, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: process
            character(len=*), intent(in) :: event
            type(status_type), intent(out) :: status
        end subroutine process_sentinel_proc
    end interface

    !> `:connection_type' values: `process_connection_type' decides, a
    !> pipe, or a pty.
    integer, parameter :: PROCESS_CONNECTION_DEFAULT = 0, PROCESS_CONNECTION_PIPE = 1, &
                          PROCESS_CONNECTION_PTY = 2

    ! Process states (the car of p->status).
    integer, parameter :: ST_RUN = 1, ST_EXIT = 2, ST_SIGNAL = 3

    !> Control type of device used to communicate with subprocesses.
    !> Values are false to use a pipe, or true to use a pty.
    !> The value takes effect when `start_process' is called.  Port of
    !> process.c process-connection-type.
    type(logical_setting), public :: process_connection_type = &
                                     logical_setting(name="process_connection_type", value=.true.)

    !> True means delete processes immediately when they exit.
    !> False means don't delete them until `list_processes' is run.  Port
    !> of process.c delete-exited-processes.
    type(logical_setting), public :: delete_exited_processes = &
                                     logical_setting(name="delete_exited_processes", value=.true.)

    !> struct Lisp_Process (the part a subprocess uses).
    type :: process_type
        character(len=:), allocatable :: name
        type(process_arg_type), allocatable :: command(:)
        !> The associated buffer (a null handle for none).
        type(handle_type) :: buffer
        !> Marker set to end of last buffer-inserted output from this process.
        type(handle_type) :: mark
        integer(int32) :: pid = 0
        !> Symbol indicating status of process (ST_*), with the exit code or
        !> signal number and whether a core was dumped.
        integer :: state = ST_RUN
        integer(int32) :: code = 0
        logical :: coredump = .false.
        !> The raw wait status not yet converted (raw_status_new).
        integer(int32) :: raw_status = 0
        logical :: raw_status_new = .false.
        !> The child has not been reaped (p->alive).
        logical :: alive = .false.
        !> Output is still being read (p->infd >= 0): the worker reporting it.
        logical :: reading = .false.
        integer(int32) :: worker = 0
        !> Descriptor for sending input (p->outfd), -1 when closed.
        integer(int32) :: outfd = -1
        logical :: pty = .false.
        character(len=:), allocatable :: tty_name
        logical :: kill_without_query = .false.
        !> In the process list (not deleted).
        logical :: listed = .false.
        integer(int64) :: tick = 0, update_tick = 0
        procedure(process_filter_proc), pointer, nopass :: filter => null()
        procedure(process_sentinel_proc), pointer, nopass :: sentinel => null()
        !> Bytes of an incomplete UTF-8 sequence at the end of the last read
        !> (p->decoding_carryover).
        character(len=:), allocatable :: carryover
    end type process_type

    type(slot_table_type) :: table
    type(process_type), allocatable :: procs(:)

    !> Vprocess_alist: the listed processes, the newest first.
    type(handle_type), allocatable :: alist(:)
    integer :: alist_count = 0

    !> Incremented for each change in any process status (process_tick),
    !> and the value status_notify last saw (update_tick).
    integer(int64) :: process_tick = 0, update_tick = 0

contains

    ! ------------------------------------------------------------------
    ! The registry
    ! ------------------------------------------------------------------

    !> Slot of process `proc', or an error for a freed one.
    subroutine resolve(proc, i, status)
        type(handle_type), intent(in) :: proc
        integer(int64), intent(out) :: i
        type(status_type), intent(out) :: status
        call table%resolve(proc, i, status)
        if (.not. status%ok()) status%detail = "Wrong type argument: processp"
    end subroutine resolve

    !> Return t if OBJECT is a process.  Port of process.c:Fprocessp.
    logical function processp(object)
        type(handle_type), intent(in) :: object
        processp = table%is_live(object)
    end function processp

    !> Make room for slot `n'.
    subroutine grow(n)
        integer(int64), intent(in) :: n
        type(process_type), allocatable :: bigger(:)
        integer(int64) :: k
        if (allocated(procs)) then
            if (size(procs, kind=int64) >= n) return
            allocate (bigger(n))
            do k = 1, size(procs, kind=int64)
                call move_process(procs(k), bigger(k))
            end do
            call move_alloc(bigger, procs)
        else
            allocate (procs(n))
        end if
    end subroutine grow

    subroutine move_process(from, to)
        type(process_type), intent(inout) :: from
        type(process_type), intent(out) :: to
        to = from
    end subroutine move_process

    !> Allocate a process named NAME (made unique with <N>), running,
    !> with a fresh mark and the default filter and sentinel, and put it
    !> first in the process list.  Port of process.c:make_process.
    subroutine new_process(name, h)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: h
        character(len=:), allocatable :: name1
        character(len=24) :: suffix
        integer(int64) :: i, k
        name1 = name
        k = 1
        do while (processp(get_process(name1)))
            write (suffix, "('<', i0, '>')") k
            name1 = name//trim(suffix)
            k = k + 1
        end do
        call table%acquire(h)
        call grow(table%capacity())
        i = h%index
        procs(i) = process_type()
        procs(i)%name = name1
        procs(i)%state = ST_RUN
        call make_marker(procs(i)%mark)
        procs(i)%tty_name = ""
        procs(i)%carryover = ""
        allocate (procs(i)%command(0))
        call alist_push(h)
        procs(i)%listed = .true.
    end subroutine new_process

    subroutine alist_push(h)
        type(handle_type), intent(in) :: h
        type(handle_type), allocatable :: bigger(:)
        if (.not. allocated(alist)) allocate (alist(8))
        if (alist_count == size(alist)) then
            allocate (bigger(2*alist_count))
            bigger(1:alist_count) = alist(1:alist_count)
            call move_alloc(bigger, alist)
        end if
        alist(2:alist_count + 1) = alist(1:alist_count)
        alist(1) = h
        alist_count = alist_count + 1
    end subroutine alist_push

    !> Take process `proc' off the process list and deactivate it.  Port of
    !> process.c:remove_process.
    recursive subroutine remove_process(i)
        integer(int64), intent(in) :: i
        integer :: k
        do k = 1, alist_count
            if (alist(k)%index == i) then
                alist(k:alist_count - 1) = alist(k + 1:alist_count)
                alist_count = alist_count - 1
                exit
            end if
        end do
        procs(i)%listed = .false.
        call deactivate_process(i)
    end subroutine remove_process

    !> Stop reading the process's output and close its input.  Port of
    !> process.c:deactivate_process.  (The worker finishes by itself once
    !> the child has exited; its remaining messages are dropped.)
    recursive subroutine deactivate_process(i)
        integer(int64), intent(in) :: i
        if (procs(i)%outfd >= 0) call close_fd(procs(i)%outfd)
        procs(i)%outfd = -1
        procs(i)%reading = .false.
        procs(i)%worker = 0
    end subroutine deactivate_process

    !> Free process `proc' (deleting it first if it is listed): the
    !> handle, and every copy of it, becomes invalid.  Ours: process
    !> objects are not garbage collected.
    subroutine free_process(proc, status)
        type(handle_type), intent(in) :: proc
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: i
        call resolve(proc, i, status)
        if (.not. status%ok()) return
        if (procs(i)%listed) call delete_process(proc, st)
        call free_marker(procs(i)%mark, st)
        procs(i) = process_type()
        call table%release(proc, status)
    end subroutine free_process

    ! ------------------------------------------------------------------
    ! Status
    ! ------------------------------------------------------------------

    !> Compute p->status from the raw status returned by `wait'.  Port of
    !> process.c:update_status and status_convert (bits/waitstatus.h).
    subroutine update_status(i)
        integer(int64), intent(in) :: i
        integer(int32) :: w
        w = procs(i)%raw_status
        if (iand(w, 127) == 0) then
            procs(i)%state = ST_EXIT
            procs(i)%code = iand(ishft(w, -8), 255)
            procs(i)%coredump = .false.
        else
            procs(i)%state = ST_SIGNAL
            procs(i)%code = iand(w, 127)
            procs(i)%coredump = iand(w, 128) /= 0
        end if
        procs(i)%raw_status_new = .false.
    end subroutine update_status

    !> Return a string describing a process status.  Port of
    !> process.c:status_message.
    function status_message(proc) result(msg)
        type(handle_type), intent(in) :: proc
        character(len=:), allocatable :: msg
        type(status_type) :: st
        integer(int64) :: i
        msg = ""
        call resolve(proc, i, st)
        if (.not. st%ok()) return
        msg = status_message_1(i)
    end function status_message

    function status_message_1(i) result(msg)
        integer(int64), intent(in) :: i
        character(len=:), allocatable :: msg
        character(len=24) :: num
        character(len=:), allocatable :: suffix
        if (procs(i)%raw_status_new) call update_status(i)
        suffix = new_line("a")
        if (procs(i)%coredump) suffix = " (core dumped)"//new_line("a")
        select case (procs(i)%state)
        case (ST_SIGNAL)
            msg = emacs_strsignal(procs(i)%code)
            ! Downcase the first letter (as upstream's c1/c2).
            if (len(msg) > 0) then
                if (msg(1:1) >= "A" .and. msg(1:1) <= "Z") msg(1:1) = achar(iachar(msg(1:1)) + 32)
            end if
            msg = msg//suffix
        case (ST_EXIT)
            if (procs(i)%code == 0) then
                msg = "finished"//new_line("a")
            else
                write (num, "(i0)") procs(i)%code
                msg = "exited abnormally with code "//trim(num)//suffix
            end if
        case default
            msg = "run"
        end select
    end function status_message_1

    !> Return the status of PROCESS.
    !> The returned value is one of the following:
    !> "run"  -- for a process that is running.
    !> "exit" -- for a process that has exited.
    !> "signal" -- for a process that has got a fatal signal.
    !> "" -- if PROCESS is not a process.
    !> Port of process.c:Fprocess_status (status symbols as strings).
    function process_status(process) result(status_name)
        type(handle_type), intent(in) :: process
        character(len=:), allocatable :: status_name
        type(status_type) :: st
        integer(int64) :: i
        status_name = ""
        call resolve(process, i, st)
        if (.not. st%ok()) return
        if (procs(i)%raw_status_new) call update_status(i)
        select case (procs(i)%state)
        case (ST_EXIT)
            status_name = "exit"
        case (ST_SIGNAL)
            status_name = "signal"
        case default
            status_name = "run"
        end select
    end function process_status

    !> Return the exit status of PROCESS or the signal number that killed it.
    !> If PROCESS has not yet exited or died, return 0.  Port of
    !> process.c:Fprocess_exit_status.
    integer function process_exit_status(process)
        type(handle_type), intent(in) :: process
        type(status_type) :: st
        integer(int64) :: i
        process_exit_status = 0
        call resolve(process, i, st)
        if (.not. st%ok()) return
        if (procs(i)%raw_status_new) call update_status(i)
        if (procs(i)%state /= ST_RUN) process_exit_status = procs(i)%code
    end function process_exit_status

    !> Return true if PROCESS is alive.
    !> A process is considered alive if its status is `run'.  Port of
    !> subr.el:process-live-p.
    logical function process_live_p(process)
        type(handle_type), intent(in) :: process
        process_live_p = .false.
        if (processp(process)) process_live_p = process_status(process) == "run"
    end function process_live_p

    !> Return the process id of PROCESS.
    !> This is the pid of the external process which PROCESS uses or talks
    !> to (0 if there is none).  Port of process.c:Fprocess_id.
    integer function process_id(process)
        type(handle_type), intent(in) :: process
        type(status_type) :: st
        integer(int64) :: i
        process_id = 0
        call resolve(process, i, st)
        if (st%ok()) process_id = procs(i)%pid
    end function process_id

    !> Return the name of PROCESS, as a string.
    !> This is the name of the program invoked in PROCESS,
    !> possibly modified to make it unique among process names.  Port of
    !> process.c:Fprocess_name.
    function process_name(process) result(name)
        type(handle_type), intent(in) :: process
        character(len=:), allocatable :: name
        type(status_type) :: st
        integer(int64) :: i
        name = ""
        call resolve(process, i, st)
        if (st%ok()) name = procs(i)%name
    end function process_name

    !> Return the command that was executed to start PROCESS.
    !> This is an array of strings, the first string being the program
    !> executed and the rest of the strings being the arguments given to
    !> it.  Port of process.c:Fprocess_command.
    subroutine process_command(process, command)
        type(handle_type), intent(in) :: process
        type(process_arg_type), allocatable, intent(out) :: command(:)
        type(status_type) :: st
        integer(int64) :: i
        call resolve(process, i, st)
        if (st%ok()) then
            command = procs(i)%command
        else
            allocate (command(0))
        end if
    end subroutine process_command

    !> Return the name of the terminal PROCESS uses, or "" if none.
    !> This is the terminal that the process itself reads and writes on,
    !> not the name of the pty that Emacs uses to talk with that terminal.
    !> Port of process.c:Fprocess_tty_name.
    function process_tty_name(process) result(name)
        type(handle_type), intent(in) :: process
        character(len=:), allocatable :: name
        type(status_type) :: st
        integer(int64) :: i
        name = ""
        call resolve(process, i, st)
        if (st%ok()) name = procs(i)%tty_name
    end function process_tty_name

    !> Return the buffer PROCESS is associated with (a null handle for none).
    !> The default process filter inserts output from PROCESS into this
    !> buffer.  Port of process.c:Fprocess_buffer.
    function process_buffer(process) result(buffer)
        type(handle_type), intent(in) :: process
        type(handle_type) :: buffer
        type(status_type) :: st
        integer(int64) :: i
        buffer = null_handle()
        call resolve(process, i, st)
        if (st%ok()) buffer = procs(i)%buffer
    end function process_buffer

    !> Make the process marker point into the process buffer (if any).
    !> Port of process.c:update_process_mark.
    subroutine update_process_mark(i)
        integer(int64), intent(in) :: i
        type(handle_type) :: mb
        type(status_type) :: st
        integer(int64) :: b
        if (.not. buffer_live_p(procs(i)%buffer)) return
        call marker_buffer(procs(i)%mark, mb, st)
        if (mb%index == procs(i)%buffer%index .and. mb%generation == procs(i)%buffer%generation) return
        call resolve_buffer(procs(i)%buffer, b, st)
        call set_marker_both(procs(i)%mark, procs(i)%buffer, buffers(b)%zv, buffers(b)%zv_byte, st)
    end subroutine update_process_mark

    !> Set buffer associated with PROCESS to BUFFER (a buffer, or a null
    !> handle for none).  Port of process.c:Fset_process_buffer.
    subroutine set_process_buffer(process, buffer, status)
        type(handle_type), intent(in) :: process, buffer
        type(status_type), intent(out) :: status
        integer(int64) :: i
        call resolve(process, i, status)
        if (.not. status%ok()) return
        if (buffer%index /= 0 .and. .not. buffer_live_p(buffer)) then
            call set_error(status, FM_GENERIC_ERROR, "Wrong type argument: bufferp")
            return
        end if
        if (procs(i)%buffer%index /= buffer%index .or. procs(i)%buffer%generation /= buffer%generation) then
            procs(i)%buffer = buffer
            call update_process_mark(i)
        end if
    end subroutine set_process_buffer

    !> Return the marker for the end of the last output from PROCESS.
    !> Port of process.c:Fprocess_mark.
    function process_mark(process) result(mark)
        type(handle_type), intent(in) :: process
        type(handle_type) :: mark
        type(status_type) :: st
        integer(int64) :: i
        mark = null_handle()
        call resolve(process, i, st)
        if (st%ok()) mark = procs(i)%mark
    end function process_mark

    !> Give PROCESS the filter function FILTER; absent means default.
    !>
    !> When a process has a non-default filter, its buffer is not used for
    !> output.  Instead, each time it does output, the entire string of
    !> output is passed to the filter.
    !>
    !> The filter gets two arguments: the process and the string of
    !> output.  Port of process.c:Fset_process_filter (a filter of t is
    !> Deferred).
    subroutine set_process_filter(process, status, filter)
        type(handle_type), intent(in) :: process
        type(status_type), intent(out) :: status
        procedure(process_filter_proc), optional :: filter
        integer(int64) :: i
        call resolve(process, i, status)
        if (.not. status%ok()) return
        if (present(filter)) then
            procs(i)%filter => filter
        else
            procs(i)%filter => null()
        end if
    end subroutine set_process_filter

    !> FILTER is the filter function of PROCESS (null for the default,
    !> `internal_default_process_filter').  Port of
    !> process.c:Fprocess_filter.
    subroutine process_filter(process, filter)
        type(handle_type), intent(in) :: process
        procedure(process_filter_proc), pointer, intent(out) :: filter
        type(status_type) :: st
        integer(int64) :: i
        filter => null()
        call resolve(process, i, st)
        if (st%ok()) filter => procs(i)%filter
    end subroutine process_filter

    !> SENTINEL is the sentinel of PROCESS (null for the default,
    !> `internal_default_process_sentinel').  Port of
    !> process.c:Fprocess_sentinel.
    subroutine process_sentinel(process, sentinel)
        type(handle_type), intent(in) :: process
        procedure(process_sentinel_proc), pointer, intent(out) :: sentinel
        type(status_type) :: st
        integer(int64) :: i
        sentinel => null()
        call resolve(process, i, st)
        if (st%ok()) sentinel => procs(i)%sentinel
    end subroutine process_sentinel

    !> Give PROCESS the sentinel SENTINEL; absent for default.
    !> The sentinel is called as a function when the process changes state.
    !> It gets two arguments: the process, and a string describing the
    !> change.  Port of process.c:Fset_process_sentinel.
    subroutine set_process_sentinel(process, status, sentinel)
        type(handle_type), intent(in) :: process
        type(status_type), intent(out) :: status
        procedure(process_sentinel_proc), optional :: sentinel
        integer(int64) :: i
        call resolve(process, i, status)
        if (.not. status%ok()) return
        if (present(sentinel)) then
            procs(i)%sentinel => sentinel
        else
            procs(i)%sentinel => null()
        end if
    end subroutine set_process_sentinel

    !> Specify if query is needed for PROCESS when Emacs is exited.
    !> If the second argument FLAG is false, Emacs will not query the user
    !> before exiting if PROCESS is running.  Port of
    !> process.c:Fset_process_query_on_exit_flag.
    subroutine set_process_query_on_exit_flag(process, flag, status)
        type(handle_type), intent(in) :: process
        logical, intent(in) :: flag
        type(status_type), intent(out) :: status
        integer(int64) :: i
        call resolve(process, i, status)
        if (status%ok()) procs(i)%kill_without_query = .not. flag
    end subroutine set_process_query_on_exit_flag

    !> Return the current value of query-on-exit flag for PROCESS.  Port of
    !> process.c:Fprocess_query_on_exit_flag.
    logical function process_query_on_exit_flag(process)
        type(handle_type), intent(in) :: process
        type(status_type) :: st
        integer(int64) :: i
        process_query_on_exit_flag = .false.
        call resolve(process, i, st)
        if (st%ok()) process_query_on_exit_flag = .not. procs(i)%kill_without_query
    end function process_query_on_exit_flag

    !> Return the process named NAME, or a null handle if there is none.
    !> Port of process.c:Fget_process.
    function get_process(name) result(proc)
        character(len=*), intent(in) :: name
        type(handle_type) :: proc
        integer :: k
        proc = null_handle()
        do k = 1, alist_count
            if (procs(alist(k)%index)%name == name .and. len(procs(alist(k)%index)%name) == len(name)) then
                proc = alist(k)
                return
            end if
        end do
    end function get_process

    !> Return the (or a) live process associated with BUFFER.
    !> BUFFER may be a buffer or the name of one.
    !> Return a null handle if all processes associated with BUFFER have
    !> been deleted or killed.  Port of process.c:Fget_buffer_process.
    function get_buffer_process(buffer) result(proc)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: proc
        integer :: k
        integer(int64) :: i
        proc = null_handle()
        if (buffer%index == 0) return
        ! From the newest process, as FOR_EACH_PROCESS does.
        do k = 1, alist_count
            i = alist(k)%index
            if (procs(i)%buffer%index == buffer%index .and. procs(i)%buffer%generation == buffer%generation) then
                proc = alist(k)
                return
            end if
        end do
    end function get_buffer_process

    !> Return an array of all processes that are Emacs sub-processes.  Port
    !> of process.c:Fprocess_list.
    function process_list() result(list)
        type(handle_type), allocatable :: list(:)
        if (alist_count == 0) then
            allocate (list(0))
        else
            list = alist(1:alist_count)
        end if
    end function process_list

    ! ------------------------------------------------------------------
    ! Starting processes
    ! ------------------------------------------------------------------

    !> Start a program in a subprocess.  Return the process object for it.
    !>
    !> NAME is name for process.  It is modified if necessary to make it
    !> unique.
    !>
    !> BUFFER is the buffer to associate with the process.  Process output
    !> goes at end of that buffer, unless you specify a filter function to
    !> handle the output.  BUFFER may be also absent, meaning that this
    !> process is not associated with any buffer.
    !>
    !> COMMAND is an array starting with the program file name, followed by
    !> strings to give to the program as arguments.  If the program file
    !> name is not an absolute file name, `make_process' will look for the
    !> program file name in `exec_path' (which is an array of directories).
    !>
    !> NOQUERY -- When exiting Emacs, query the user if NOQUERY is false
    !> and the process is running.  If NOQUERY is not given, query before
    !> exiting.
    !>
    !> CONNECTION_TYPE is control type of device used to communicate with
    !> subprocesses.  Values are `PROCESS_CONNECTION_PIPE' to use a pipe,
    !> `PROCESS_CONNECTION_PTY' to use a pty, or
    !> `PROCESS_CONNECTION_DEFAULT' to use the default specified through
    !> `process_connection_type'.
    !>
    !> FILTER -- Install FILTER as the process filter.
    !>
    !> SENTINEL -- Install SENTINEL as the process sentinel.
    !>
    !> ENVIRONMENT -- "NAME=VALUE" entries put first in the process's
    !> environment (as binding `process_environment' to an array starting
    !> with them does).
    !>
    !> Port of process.c:Fmake_process and create_process (PATH stands
    !> for `exec_path').
    subroutine make_process(name, command, process, status, buffer, noquery, connection_type, filter, sentinel, &
                            environment)
        character(len=*), intent(in) :: name
        type(process_arg_type), intent(in) :: command(:)
        type(handle_type), intent(out) :: process
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        logical, intent(in), optional :: noquery
        integer, intent(in), optional :: connection_type
        procedure(process_filter_proc), optional :: filter
        procedure(process_sentinel_proc), optional :: sentinel
        type(process_arg_type), intent(in), optional :: environment(:)
        character(len=:), allocatable :: current_dir, path, tty_name
        type(sys_string_type), allocatable :: argv(:), env(:)
        integer(int64) :: i, k
        integer(int32) :: err, to_child, from_child, pid, worker
        logical :: pty
        process = null_handle()
        if (present(buffer)) then
            if (buffer%index /= 0 .and. .not. buffer_live_p(buffer)) then
                call set_error(status, FM_GENERIC_ERROR, "Selecting deleted buffer")
                return
            end if
        end if
        ! Make sure that the child will be able to chdir to the current
        ! buffer's current directory.
        call get_current_directory(current_dir, status)
        if (.not. status%ok()) return
        if (size(command) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Starting a process without a program is not supported yet")
            return
        end if
        call new_process(name, process)
        i = process%index
        if (present(buffer)) procs(i)%buffer = buffer
        if (present(filter)) procs(i)%filter => filter
        if (present(sentinel)) procs(i)%sentinel => sentinel
        procs(i)%command = command
        if (present(noquery)) procs(i)%kill_without_query = noquery
        pty = process_connection_type%value
        if (present(connection_type)) then
            if (connection_type == PROCESS_CONNECTION_PIPE) pty = .false.
            if (connection_type == PROCESS_CONNECTION_PTY) pty = .true.
        end if
        ! Make the process marker point into the process buffer (if any).
        call update_process_mark(i)
        ! If program file name is not absolute, search our path for it.
        if (index(command(1)%s, "/") /= 1) then
            call locate_program(command(1)%s, current_dir, path)
            if (len(path) == 0) then
                call report_file_errno("Searching for program", command(1)%s, ENOENT, status)
                call start_process_unwind()
                return
            end if
        else
            if (file_directory_p(command(1)%s)) then
                call set_error(status, FM_GENERIC_ERROR, "Specified program for new process is a directory")
                call start_process_unwind()
                return
            end if
            path = command(1)%s
        end if
        allocate (argv(size(command)))
        argv(1)%s = path
        do k = 2, size(command)
            argv(k)%s = command(k)%s
        end do
        call ignore_sigpipe()
        if (present(environment)) then
            allocate (env(size(environment)))
            do k = 1, size(environment)
                env(k)%s = environment(k)%s
            end do
            err = emacs_spawn_async(path, argv, current_dir, pty, to_child, from_child, pid, tty_name, env)
        else
            err = emacs_spawn_async(path, argv, current_dir, pty, to_child, from_child, pid, tty_name)
        end if
        if (err /= 0) then
            call report_file_errno("Doing vfork", "", err, status)
            call start_process_unwind()
            return
        end if
        procs(i)%pid = pid
        procs(i)%alive = .true.
        procs(i)%pty = pty
        procs(i)%tty_name = tty_name
        procs(i)%outfd = to_child
        if (pty) then
            ! The worker reads a descriptor of its own.
            from_child = emacs_dup_cloexec(to_child)
        end if
        call start_reader(pid, from_child, worker, err)
        if (err /= 0) then
            call set_error(status, FM_FILE_ERROR, "Creating thread: "//emacs_strerror(err))
            call emacs_kill_group(i, 9)
            return
        end if
        procs(i)%worker = worker
        procs(i)%reading = .true.
        process_wake_fd = worker_wake_fd()
        if (.not. associated(process_output_hook)) process_output_hook => dispatch_process_output
        if (.not. associated(kill_buffer_processes_hook)) kill_buffer_processes_hook => kill_buffer_processes
    contains

        !> If the process wasn't started successfully, remove it.  Port of
        !> process.c:start_process_unwind.
        subroutine start_process_unwind()
            type(status_type) :: ignore
            call remove_process(i)
            call free_process(process, ignore)
            process = null_handle()
        end subroutine start_process_unwind
    end subroutine make_process

    !> Start a program in a subprocess.  Return the process object for it.
    !> NAME is name for process.  It is modified if necessary to make it
    !> unique.
    !> BUFFER is the buffer (or a null handle) to associate with the
    !> process.
    !>
    !> Process output (both standard output and standard error streams)
    !> goes at end of BUFFER, unless you specify a filter function to
    !> handle the output.  BUFFER may also be absent, meaning that this
    !> process is not associated with any buffer.
    !>
    !> PROGRAM is the program file name.  It is searched for in `exec_path'
    !> (which see).  If PROGRAM is an empty string, start a process
    !> without a program (Deferred).  Remaining arguments ARGS are strings
    !> to give the program as arguments.
    !>
    !> The process runs in `default_directory' of the current buffer.  Port
    !> of subr.el:start-process.
    subroutine start_process(name, buffer, program, process, status, args)
        character(len=*), intent(in) :: name, program
        type(handle_type), intent(in) :: buffer
        type(handle_type), intent(out) :: process
        type(status_type), intent(out) :: status
        type(process_arg_type), intent(in), optional :: args(:)
        type(process_arg_type), allocatable :: command(:)
        integer :: k, n
        n = 0
        if (present(args)) n = size(args)
        allocate (command(n + 1))
        command(1)%s = program
        do k = 1, n
            command(k + 1)%s = args(k)%s
        end do
        call make_process(name, command, process, status, buffer=buffer)
    end subroutine start_process

    ! ------------------------------------------------------------------
    ! Output: taking the workers' messages
    ! ------------------------------------------------------------------

    !> Take what the workers have reported and act on it: run the filters
    !> on new output, record exits, then report status changes
    !> (status_notify).  `got' is true if output (or, for `wait_proc' a
    !> process's change) arrived from `wait_proc' (any process if absent).
    !> The part of process.c:wait_reading_process_output that reads
    !> process output, with handle_child_signal's status recording.
    recursive subroutine take_output(got, wait_proc)
        logical, intent(out) :: got
        integer(int64), intent(in), optional :: wait_proc
        type(worker_message_type), allocatable :: msgs(:)
        integer(int64) :: i
        integer :: k
        got = .false.
        call take_worker_messages(msgs)
        do k = 1, size(msgs)
            i = process_of_worker(msgs(k)%worker)
            if (i == 0) cycle
            select case (msgs(k)%kind)
            case (WORKER_OUTPUT)
                if (present(wait_proc)) then
                    if (wait_proc == i) got = .true.
                else
                    got = .true.
                end if
                call read_process_output(i, msgs(k)%bytes, .false.)
            case (WORKER_EXIT)
                ! The decoder's carryover is flushed at the end of the
                ! output (CODING_MODE_LAST_BLOCK).
                if (len(procs(i)%carryover) > 0) call read_process_output(i, "", .true.)
                procs(i)%tick = next_tick()
                procs(i)%raw_status = msgs(k)%wait_status
                procs(i)%raw_status_new = .true.
                procs(i)%alive = .false.
                ! If process has terminated, stop waiting for its output.
                procs(i)%reading = .false.
                procs(i)%worker = 0
            case default
            end select
        end do
        do while (update_tick /= process_tick)
            call status_notify(0_int64)
        end do
    end subroutine take_output

    integer(int64) function next_tick()
        process_tick = process_tick + 1
        next_tick = process_tick
    end function next_tick

    !> The process whose output worker `w' reads (0 if none: deleted).
    integer(int64) function process_of_worker(w) result(i)
        integer(int32), intent(in) :: w
        integer :: k
        i = 0
        do k = 1, alist_count
            if (procs(alist(k)%index)%worker == w .and. procs(alist(k)%index)%reading) then
                i = alist(k)%index
                return
            end if
        end do
        ! A process taken off the list (deleted, or removed after exit)
        ! is deactivated, so it has no worker.
    end function process_of_worker

    !> Handle the output `bytes' of process `i': decode them (keeping an
    !> incomplete UTF-8 sequence for the next time, unless `last') and give
    !> the text to the filter, keeping the current buffer and
    !> `deactivate_mark'.  Port of process.c:read_process_output and
    !> read_and_dispose_of_process_output.
    recursive subroutine read_process_output(i, bytes, last)
        integer(int64), intent(in) :: i
        character(len=*), intent(in) :: bytes
        logical, intent(in) :: last
        character(len=:), allocatable :: chars, text
        type(handle_type) :: old, proc
        type(status_type) :: st
        logical :: odeactivate
        integer(int64) :: n
        chars = procs(i)%carryover//bytes
        procs(i)%carryover = ""
        if (.not. last) then
            n = complete_utf8_length(chars)
            if (n < len(chars, kind=int64)) then
                procs(i)%carryover = chars(n + 1:)
                chars = chars(1:n)
            end if
        end if
        if (len(chars) == 0) return
        call str_as_multibyte(chars, text)
        odeactivate = vdeactivate_mark
        old = current_buffer()
        proc = table%handle_of(i)
        if (associated(procs(i)%filter)) then
            call procs(i)%filter(proc, text, st)
        else
            call internal_default_process_filter(proc, text, st)
        end if
        if (.not. st%ok()) then
            call message("error in process filter: "//format_error(st))
        end if
        ! There's no good reason to let process filters change the current
        ! buffer.
        if (buffer_live_p(old)) call set_buffer(old, st)
        ! Handling the process output should not deactivate the mark.
        vdeactivate_mark = odeactivate
    end subroutine read_process_output

    !> The length of `s' without an incomplete UTF-8 sequence at its end.
    pure integer(int64) function complete_utf8_length(s) result(n)
        character(len=*), intent(in) :: s
        integer(int64) :: k
        integer :: b, need
        n = len(s, kind=int64)
        do k = n, max(1_int64, n - 3), -1
            b = ichar(s(k:k))
            if (b < 128) return
            if (b >= 192) then
                if (b < 224) then
                    need = 2
                else if (b < 240) then
                    need = 3
                else if (b < 248) then
                    need = 4
                else
                    return
                end if
                if (n - k + 1 < need) n = k - 1
                return
            end if
        end do
    end function complete_utf8_length

    !> Function used as default process filter.
    !> This inserts the process's output into its buffer, if there is one.
    !> Otherwise it discards the output.  Port of
    !> process.c:Finternal_default_process_filter (with
    !> read_process_output_before_insert and _after_insert).
    recursive subroutine internal_default_process_filter(proc, text, status)
        type(handle_type), intent(in) :: proc
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: mb
        type(status_type) :: st
        integer(int64) :: i, b, opoint, before, old_begv, old_zv, after
        logical :: old_read_only
        call resolve(proc, i, status)
        if (.not. status%ok()) return
        if (.not. buffer_live_p(procs(i)%buffer)) return
        call set_buffer(procs(i)%buffer, status)
        if (.not. status%ok()) return
        b = cur
        opoint = buffers(b)%pt
        old_read_only = buffers(b)%read_only
        old_begv = buffers(b)%begv
        old_zv = buffers(b)%zv
        buffers(b)%read_only = .false.
        ! Insert new output into buffer at the current end-of-output marker,
        ! thus preserving logical ordering of input and output.
        call marker_buffer(procs(i)%mark, mb, st)
        if (mb%index /= 0) then
            call set_point_from_marker(procs(i)%mark, st)
        else
            call goto_char(buffers(b)%zv)
        end if
        before = buffers(b)%pt
        ! If the output marker is outside of the visible region, save the
        ! restriction and widen.
        if (.not. (buffers(b)%begv <= before .and. before <= buffers(b)%zv)) call widen()
        ! Insert before markers in case we are inserting where the
        ! buffer's mark is, and the user's next command is Meta-y.
        if (buffers(b)%multibyte) then
            call insert_before_markers(text, status)
        else
            call insert_before_markers(str_as_unibyte(text), status)
        end if
        after = buffers(b)%pt
        ! Make sure the process marker's position is valid.
        call set_marker(procs(i)%mark, after, st, procs(i)%buffer)
        ! Make sure opoint and the old restrictions float ahead of any new
        ! text just as point would.
        if (opoint >= before) opoint = opoint + (after - before)
        if (old_begv > before) old_begv = old_begv + (after - before)
        if (old_zv >= before) old_zv = old_zv + (after - before)
        ! If the restriction isn't what it should be, set it.
        if (old_begv /= buffers(b)%begv .or. old_zv /= buffers(b)%zv) call narrow_to_region(old_begv, old_zv, st)
        buffers(b)%read_only = old_read_only
        call goto_char(opoint)
    end subroutine internal_default_process_filter

    !> Report all recent events of a change in process status (run the
    !> sentinels).  `deleting' is a process being deleted (0 for none),
    !> whose remaining output is not read.  Port of process.c:status_notify.
    recursive subroutine status_notify(deleting)
        integer(int64), intent(in) :: deleting
        type(handle_type), allocatable :: snapshot(:)
        character(len=:), allocatable :: msg
        integer(int64) :: i
        integer :: k
        ! Set this now, so that if new processes are created by sentinels
        ! that we run, we get called again to handle their status changes.
        update_tick = process_tick
        snapshot = process_list()
        do k = 1, size(snapshot)
            if (.not. table%is_live(snapshot(k))) cycle
            i = snapshot(k)%index
            if (procs(i)%tick == procs(i)%update_tick) cycle
            procs(i)%update_tick = procs(i)%tick
            ! (The worker has read any output that remains before
            ! reporting the exit, and it has been handled; a process being
            ! deleted has none to read.)
            ! Get the text to use for the message.
            msg = status_message_1(i)
            ! If process is terminated, deactivate it or delete it.
            if (procs(i)%state == ST_SIGNAL .or. procs(i)%state == ST_EXIT) then
                if (delete_exited_processes%value) then
                    call remove_process(i)
                else
                    call deactivate_process(i)
                end if
            end if
            ! The actions above may have further incremented p->tick.
            procs(i)%update_tick = procs(i)%tick
            ! Now output the message suitably.
            call exec_sentinel(i, msg)
        end do
    end subroutine status_notify

    !> Run the sentinel of process `i' with REASON, keeping the current
    !> buffer and `deactivate_mark'; an error is reported in the echo area.
    !> Port of process.c:exec_sentinel.
    recursive subroutine exec_sentinel(i, reason)
        integer(int64), intent(in) :: i
        character(len=*), intent(in) :: reason
        type(handle_type) :: old, proc
        type(status_type) :: st
        logical :: odeactivate
        odeactivate = vdeactivate_mark
        old = current_buffer()
        proc = table%handle_of(i)
        if (associated(procs(i)%sentinel)) then
            call procs(i)%sentinel(proc, reason, st)
        else
            call internal_default_process_sentinel(proc, reason, st)
        end if
        if (.not. st%ok()) call message("error in process sentinel: "//format_error(st))
        if (buffer_live_p(old)) call set_buffer(old, st)
        vdeactivate_mark = odeactivate
    end subroutine exec_sentinel

    !> Function used as default sentinel for processes.
    !> This inserts a status message into the process's buffer, if there is
    !> one.  Port of process.c:Finternal_default_process_sentinel.
    recursive subroutine internal_default_process_sentinel(proc, msg, status)
        type(handle_type), intent(in) :: proc
        character(len=*), intent(in) :: msg
        type(status_type), intent(out) :: status
        type(handle_type) :: old, mb
        type(status_type) :: st
        integer(int64) :: i, b, opoint, before, after, mpos
        logical :: tem
        call resolve(proc, i, status)
        if (.not. status%ok()) return
        if (procs(i)%raw_status_new) call update_status(i)
        if (procs(i)%state == ST_RUN .or. procs(i)%buffer%index == 0) return
        ! Avoid error if buffer is deleted (probably that's why the process
        ! is dead, too).
        if (.not. buffer_live_p(procs(i)%buffer)) return
        old = current_buffer()
        call set_buffer(procs(i)%buffer, status)
        if (.not. status%ok()) return
        b = cur
        opoint = buffers(b)%pt
        ! Insert new output into buffer at the current end-of-output marker,
        ! thus preserving logical ordering of input and output.
        call marker_buffer(procs(i)%mark, mb, st)
        if (mb%index /= 0) then
            call marker_position(procs(i)%mark, mpos, st)
            call goto_char(mpos)
        else
            call goto_char(buffers(b)%zv)
        end if
        before = buffers(b)%pt
        tem = buffers(b)%read_only
        buffers(b)%read_only = .false.
        call insert(new_line("a")//"Process "//procs(i)%name//" "//msg, status)
        buffers(b)%read_only = tem
        after = buffers(b)%pt
        call set_marker(procs(i)%mark, after, st, procs(i)%buffer)
        if (opoint >= before) then
            call goto_char(opoint + (after - before))
        else
            call goto_char(opoint)
        end if
        if (buffer_live_p(old)) then
            call set_buffer(old, st)
        else
            call set_buffer_internal(0_int64)
        end if
    end subroutine internal_default_process_sentinel

    !> Take what the workers reported, for keyboard_fm while it waits for
    !> input (and for tests).
    recursive subroutine dispatch_process_output()
        logical :: got
        call take_output(got)
    end subroutine dispatch_process_output

    !> Allow any pending output from subprocesses to be read by Emacs.
    !> The subprocess output is given to the respective process filter
    !> functions.
    !> Optional argument PROCESS means to return only after output is
    !> received from PROCESS or PROCESS closes the connection.
    !>
    !> Optional argument SECONDS specifies a timeout; return after that
    !> much time even if there is no subprocess output.
    !>
    !> GOT is true if we received any output from PROCESS (or, if PROCESS
    !> is absent, from any process) before the timeout expired or the
    !> corresponding connection was closed.  Port of
    !> process.c:Faccept_process_output (JUST-THIS-ONE is Deferred).
    recursive subroutine accept_process_output(got, status, process, seconds)
        logical, intent(out) :: got
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: process
        real, intent(in), optional :: seconds
        integer(int64) :: i, start, now, rate, limit_ms, waited
        integer(int32) :: ms
        logical :: some
        got = .false.
        i = 0
        if (present(process)) then
            call resolve(process, i, status)
            if (.not. status%ok()) return
        end if
        limit_ms = -1
        if (present(seconds)) then
            if (seconds > 0) limit_ms = int(seconds*1000.0, int64)
        end if
        call system_clock(start, rate)
        do
            if (i /= 0) then
                call take_output(some, i)
            else
                call take_output(some)
            end if
            if (some) then
                got = .true.
                return
            end if
            ! Waiting for a process that no longer sends output.
            if (i /= 0) then
                if (.not. procs(i)%reading) return
            end if
            if (worker_count() == 0) return
            if (limit_ms >= 0) then
                call system_clock(now)
                waited = (now - start)*1000/max(rate, 1_int64)
                if (waited >= limit_ms) return
                ms = int(limit_ms - waited, int32)
            else
                ms = -1
            end if
            if (.not. wait_for_worker_messages(ms)) then
                if (limit_ms >= 0) return
            end if
        end do
    end subroutine accept_process_output

    ! ------------------------------------------------------------------
    ! Deleting and signalling
    ! ------------------------------------------------------------------

    !> Delete PROCESS: kill it and forget about it immediately.
    !> PROCESS may be a process, or absent, indicating the current
    !> buffer's process.
    !>
    !> Interactively, it will kill the current buffer's process.  Port of
    !> process.c:Fdelete_process.
    recursive subroutine delete_process(process, status)
        type(handle_type), intent(in), optional :: process
        type(status_type), intent(out) :: status
        integer(int64) :: i
        call get_process_arg(i, status, process)
        if (.not. status%ok()) return
        procs(i)%raw_status_new = .false.
        if (procs(i)%alive) call record_kill_process(i)
        if (procs(i)%reading) then
            if (procs(i)%state /= ST_SIGNAL .and. procs(i)%state /= ST_EXIT) then
                procs(i)%state = ST_SIGNAL
                procs(i)%code = 9
                procs(i)%coredump = .false.
            end if
            procs(i)%tick = next_tick()
            call status_notify(i)
        end if
        call remove_process(i)
    end subroutine delete_process

    !> The command: delete the current buffer's process, saying so.
    recursive subroutine cmd_delete_process(status)
        type(status_type), intent(out) :: status
        call delete_process(status=status)
        if (status%ok()) call message("Deleted process")
    end subroutine cmd_delete_process

    !> Kill the process (and its process group) with SIGKILL and stop
    !> treating it as alive.  Port of process.c:record_kill_process.
    subroutine record_kill_process(i)
        integer(int64), intent(in) :: i
        if (.not. procs(i)%alive) return
        procs(i)%alive = .false.
        call emacs_kill_group(i, 9)
    end subroutine record_kill_process

    subroutine emacs_kill_group(i, sig)
        integer(int64), intent(in) :: i
        integer, intent(in) :: sig
        integer(int32) :: err
        if (procs(i)%pid > 0) err = emacs_kill(-procs(i)%pid, int(sig, int32))
    end subroutine emacs_kill_group

    !> The process PROCESS stands for: the current buffer's when absent.
    !> Port of process.c:get_process.
    subroutine get_process_arg(i, status, process)
        integer(int64), intent(out) :: i
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: process
        type(handle_type) :: proc
        i = 0
        if (present(process)) then
            call resolve(process, i, status)
            return
        end if
        if (cur == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to get process for a dead buffer")
            return
        end if
        proc = get_buffer_process(current_buffer())
        if (proc%index == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Buffer "//buffers(cur)%name//" has no process")
            return
        end if
        call resolve(proc, i, status)
    end subroutine get_process_arg

    !> Send signal `signo' to the process group of process `i' (if it has
    !> not been reaped).  With CURRENT_GROUP (only for a pty), send it to
    !> the terminal's current process group instead: SIGINT and SIGQUIT by
    !> "typing" the terminal's INTR or QUIT character on the pty
    !> (SIGNALS_VIA_CHARACTERS), other signals to the terminal's foreground
    !> group.  Port of process.c:process_send_signal.
    subroutine process_send_signal(i, signo, status, current_group)
        use sysdep_fm, only: emacs_get_tty_pgrp, emacs_tty_signal_char
        integer(int64), intent(in) :: i
        integer, intent(in) :: signo
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: current_group
        logical :: group
        integer(int32) :: gid, err
        integer :: c
        status = status_type()
        if (.not. procs(i)%reading) then
            call set_error(status, FM_GENERIC_ERROR, "Process "//procs(i)%name//" is not active")
            return
        end if
        group = .false.
        if (present(current_group)) group = current_group
        if (.not. procs(i)%pty) group = .false.
        ! Send the signal to the shell's process group.
        gid = procs(i)%pid
        if (group .and. procs(i)%outfd >= 0) then
            ! If possible, send signals to the entire pgrp by sending an
            ! input character to it.
            c = emacs_tty_signal_char(procs(i)%outfd, signo)
            if (c >= 0) then
                call send_process(i, achar(c), status)
                return
            end if
            ! Get the current pgrp using the tty itself.
            gid = emacs_get_tty_pgrp(procs(i)%outfd)
            ! If we can't get the information, assume the shell owns the
            ! tty.
            if (gid == -1) gid = procs(i)%pid
        end if
        ! Do not kill an already-reaped process, as that could kill an
        ! innocent bystander that happens to have the same process ID.
        if (procs(i)%alive .and. gid > 0) err = emacs_kill(-gid, int(signo, int32))
    end subroutine process_send_signal

    !> Interrupt process PROCESS.
    !> PROCESS may be a process, or absent for the current buffer's
    !> process.
    !> Second arg CURRENT_GROUP true means send signal to
    !> the current process-group of the process's controlling terminal
    !> rather than to the process's own process group.
    !> If the process is a shell, this means interrupt current subjob
    !> rather than the shell.  Port of process.c:Finterrupt_process
    !> (through internal-default-interrupt-process).
    subroutine interrupt_process(status, process, current_group)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: process
        logical, intent(in), optional :: current_group
        integer(int64) :: i
        call get_process_arg(i, status, process)
        if (status%ok()) call process_send_signal(i, 2, status, current_group)
    end subroutine interrupt_process

    !> Kill process PROCESS.  May be process or name of one.
    !> See function `interrupt_process' for more details on usage.  Port
    !> of process.c:Fkill_process.
    subroutine kill_process(status, process, current_group)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: process
        logical, intent(in), optional :: current_group
        integer(int64) :: i
        call get_process_arg(i, status, process)
        if (status%ok()) call process_send_signal(i, 9, status, current_group)
    end subroutine kill_process

    !> Send QUIT signal to process PROCESS.  May be process or name of one.
    !> See function `interrupt_process' for more details on usage.  Port
    !> of process.c:Fquit_process.
    subroutine quit_process(status, process, current_group)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: process
        logical, intent(in), optional :: current_group
        integer(int64) :: i
        call get_process_arg(i, status, process)
        if (status%ok()) call process_send_signal(i, 3, status, current_group)
    end subroutine quit_process

    !> Send PROCESS the signal with code SIGCODE.  `result' is kill's
    !> result (0, or -1).  Port of process.c:Finternal_default_signal_process
    !> (a process object and a signal number).
    subroutine signal_process(process, sigcode, result, status)
        type(handle_type), intent(in) :: process
        integer, intent(in) :: sigcode
        integer, intent(out) :: result
        type(status_type), intent(out) :: status
        integer(int64) :: i
        result = 0
        call resolve(process, i, status)
        if (.not. status%ok()) return
        if (procs(i)%pid <= 0) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot signal process "//procs(i)%name)
            return
        end if
        if (emacs_kill(procs(i)%pid, int(sigcode, int32)) /= 0) result = -1
    end subroutine signal_process

    !> Kill the processes of buffer `buffer' (all if null): send them
    !> SIGHUP.  Port of process.c:kill_buffer_processes.
    subroutine kill_buffer_processes(buffer)
        type(handle_type), intent(in) :: buffer
        type(status_type) :: st
        integer(int64) :: i
        integer :: k
        do k = 1, alist_count
            i = alist(k)%index
            if (buffer%index == 0 .or. (procs(i)%buffer%index == buffer%index &
                                        .and. procs(i)%buffer%generation == buffer%generation)) then
                if (procs(i)%reading) call process_send_signal(i, 1, st)
            end if
        end do
    end subroutine kill_buffer_processes

    ! ------------------------------------------------------------------
    ! Input
    ! ------------------------------------------------------------------

    !> Send PROCESS the contents of STRING as input.
    !> PROCESS may be a process, or absent for the current buffer's
    !> process.  Port of process.c:Fprocess_send_string and send_process.
    subroutine process_send_string(string, status, process)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: process
        integer(int64) :: i
        call get_process_arg(i, status, process)
        if (.not. status%ok()) return
        call send_process(i, str_as_unibyte(string), status)
    end subroutine process_send_string

    !> Send current contents of region as input to PROCESS.
    !> Called from program, takes three arguments, PROCESS, START and END.
    !> Port of process.c:Fprocess_send_region.
    subroutine process_send_region(start, end, status, process)
        use editfns_fm, only: buffer_substring
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: process
        character(len=:), allocatable :: text
        integer(int64) :: i
        call get_process_arg(i, status, process)
        if (.not. status%ok()) return
        call buffer_substring(start, end, text, status)
        if (.not. status%ok()) return
        call send_process(i, str_as_unibyte(text), status)
    end subroutine process_send_region

    !> Write `bytes' to process `i'.  Port of process.c:send_process
    !> (writes block until done).
    subroutine send_process(i, bytes, status)
        integer(int64), intent(in) :: i
        character(len=*), intent(in) :: bytes
        type(status_type), intent(out) :: status
        integer(int64) :: written
        integer(int32) :: err
        type(status_type) :: wst
        status = status_type()
        if (procs(i)%raw_status_new) call update_status(i)
        if (procs(i)%state /= ST_RUN) then
            call set_error(status, FM_GENERIC_ERROR, "Process "//procs(i)%name//" not running: "//status_message_1(i))
            return
        end if
        if (procs(i)%outfd < 0) then
            call set_error(status, FM_GENERIC_ERROR, "Output file descriptor of "//procs(i)%name//" is closed")
            return
        end if
        call emacs_full_write(procs(i)%outfd, bytes, written, wst, err)
        if (err == EPIPE) then
            procs(i)%raw_status_new = .false.
            procs(i)%state = ST_EXIT
            procs(i)%code = 256
            procs(i)%tick = next_tick()
            call deactivate_process(i)
            call set_error(status, FM_GENERIC_ERROR, "Process "//procs(i)%name//" no longer connected to pipe; closed it")
        else if (err /= 0) then
            status = wst
        end if
    end subroutine send_process

    !> Make PROCESS see end-of-file in its input.
    !> EOF comes after any text already sent to it.
    !> PROCESS may be a process, or absent, indicating the current
    !> buffer's process.
    !> If PROCESS is communicating through a pipe (as opposed to a pty),
    !> then you cannot send any more text to PROCESS after you call this
    !> function.  Port of process.c:Fprocess_send_eof.
    subroutine process_send_eof(status, process)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: process
        integer(int64) :: i
        call get_process_arg(i, status, process)
        if (.not. status%ok()) return
        if (procs(i)%raw_status_new) call update_status(i)
        if (procs(i)%state /= ST_RUN) then
            call set_error(status, FM_GENERIC_ERROR, "Process "//procs(i)%name//" not running: "//status_message_1(i))
            return
        end if
        if (procs(i)%pty) then
            call send_process(i, achar(4), status)
        else if (procs(i)%outfd >= 0) then
            call close_fd(procs(i)%outfd)
            procs(i)%outfd = -1
        end if
    end subroutine process_send_eof

    !> Return the number of processors, a positive integer.
    !> Each usable thread execution unit counts as a processor.
    !> By default, count the number of available processors,
    !> overridable via the OMP_NUM_THREADS environment variable.
    !> If CURRENT is true, ignore OMP_NUM_THREADS.  Port of
    !> process.c:Fnum_processors (QUERY `all' is Deferred).
    integer function num_processors(current)
        logical, intent(in), optional :: current
        logical :: overridable
        overridable = .true.
        if (present(current)) overridable = .not. current
        num_processors = emacs_num_processors(overridable)
    end function num_processors

    ! ------------------------------------------------------------------
    ! Setup and shutdown
    ! ------------------------------------------------------------------

    !> Register the commands and hook into kill-buffer.  Port of
    !> process.c:syms_of_process (the part ported).
    subroutine syms_of_process()
        call init_callproc()
        commands%delete_process%run => cmd_delete_process
        kill_buffer_processes_hook => kill_buffer_processes
        mode_line_process_status => mode_line_status
    end subroutine syms_of_process

    !> What %s in a mode line shows for buffer slot BI: the status of its
    !> process, or "no process".  The %s case of xdisp.c:decode_mode_spec.
    subroutine mode_line_status(bi, status_name)
        integer(int64), intent(in) :: bi
        character(len=:), allocatable, intent(out) :: status_name
        type(handle_type) :: proc
        proc = get_buffer_process(buffer_handle(bi))
        if (proc%index == 0) then
            status_name = "no process"
        else
            status_name = process_status(proc)
        end if
    end subroutine mode_line_status

    !> Kill every process still running with `sig' (SIGKILL if absent),
    !> and wait for the workers to finish (for exiting, and for tests).
    !> Statuses are not reported (upstream's inhibit_sentinels on exit).
    subroutine process_shutdown(sig)
        integer, intent(in), optional :: sig
        type(worker_message_type), allocatable :: msgs(:)
        integer(int64) :: i
        integer :: k, signo
        logical :: ready
        signo = 9
        if (present(sig)) signo = sig
        do i = 1, table%capacity()
            if (.not. table%is_live(table%handle_of(i))) cycle
            if (procs(i)%alive) call emacs_kill_group(i, signo)
        end do
        do while (worker_count() > 0)
            ready = wait_for_worker_messages(-1_int32)
            call take_worker_messages(msgs)
            do k = 1, size(msgs)
                if (msgs(k)%kind /= WORKER_EXIT) cycle
                i = process_of_worker(msgs(k)%worker)
                if (i /= 0) then
                    procs(i)%alive = .false.
                    procs(i)%tick = next_tick()
                    procs(i)%raw_status = msgs(k)%wait_status
                    procs(i)%raw_status_new = .true.
                    call deactivate_process(i)
                end if
            end do
        end do
    end subroutine process_shutdown
end module process_fm
