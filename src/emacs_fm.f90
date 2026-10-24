! emacs_fm.f90 -- starting and running the editor.
! Ported from GNU Emacs 31.1 src/emacs.c (main) with the terminal setup,
! options, init file and file arguments of lisp/startup.el and
! lisp/faces.el (tty-run-terminal-initialization).
!
! Copyright (C) 1985-1987, 1993-1995, 1997-1999, 2001-2026 Free Software
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

!> `emacs_main' sets up the *scratch* buffer, the key bindings and the
!> terminal, runs the init file's `configure' (unless -q), visits the
!> files named on the command line, then runs the command loop until
!> C-x C-c, redisplaying whenever it waits for input.  `run' is the
!> program around it.
module emacs_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, output_unit, error_unit
    use error_fm, only: format_message, status_type, set_error, format_error, FM_GENERIC_ERROR
    use version_fm, only: version_string
    use handle_fm, only: handle_type, null_handle
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer
    use editfns_fm, only: goto_char, point_min
    use cmds_fm, only: forward_line
    use fileio_fm, only: init_fileio, emacs_wd, file_name_as_directory, file_name_absolute_p, &
                         file_exists_p, file_executable_p, file_newer_than_file_p
    use files_fm, only: find_file_noselect
    use frame_fm, only: selected_frame
    use window_fm, only: delete_window_tree, delete_window_object
    use sysdep_fm, only: install_interrupt_handler, emacs_isatty, sys_string_type, emacs_execv, emacs_strerror
    use term_fm, only: init_tty, delete_tty, tty_set_terminal_modes
    use termchar_fm, only: tty_flush
    use dispnew_fm, only: adjust_frame_glyphs, frame_ring_bell
    use xdisp_fm, only: redisplay, init_frame_windows, message
    use xfaces_fm, only: init_frame_faces
    use faces_fm, only: init_faces
    use modes_fm, only: minor_modes
    use keyboard_fm, only: set_input_fd, command_loop, redisplay_hook, ring_bell_hook
    use process_fm, only: kill_buffer_processes
    use bindings_fm, only: init_bindings
    use term_xterm_fm, only: terminal_init_xterm
    use tty_colors_fm, only: tty_register_colors
    use lread_fm, only: user_init_file
    use config_fm, only: install_prefix, running_executable, recompile, init_file_name, current_build_name, &
                         generation_of, current_generation, switch_generation, rollback, list_generations
    use indent_fm, only: move_to_column
    use abbrev_fm, only: abbrevs_changed
    implicit none(type, external)
    private

    public :: emacs_main, command_line, command_line_1, arg_type, init_proc, run, sort_args
    public :: load_init_file

    !> One command-line argument.
    type :: arg_type
        character(len=:), allocatable :: s
    end type arg_type

    abstract interface
        !> The user's configuration: `configure' in init.f90, run once at
        !> startup the way upstream loads the init file.
        subroutine init_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine init_proc
    end interface

    !> startup.el:init-file-had-error: whether the init file's
    !> `configure' failed at startup.
    public :: init_file_had_error
    logical, protected :: init_file_had_error = .false.

    !> A message for the echo area once the editor is up ("" for none).
    character(len=:), allocatable :: startup_message

    !> One of emacs.c's standard_args: an option, its long name, its
    !> priority for sort_args and its number of arguments.
    type :: standard_arg_type
        character(len=24) :: name = "", longname = ""
        integer :: priority = 0, nargs = 0
    end type standard_arg_type

    !> The options Emacs knows, for sort_args (emacs.c standard_args, as
    !> built on GNU/Linux with the portable dumper, modules and seccomp).
    type(standard_arg_type), parameter :: standard_args(82) = [ &
                                          standard_arg_type("-version", "--version", 150, 0), &
                                          standard_arg_type("-fingerprint", "--fingerprint", 140, 0), &
                                          standard_arg_type("-chdir", "--chdir", 130, 1), &
                                          standard_arg_type("-t", "--terminal", 120, 1), &
                                          standard_arg_type("-nw", "--no-window-system", 110, 0), &
                                          standard_arg_type("-nw", "--no-windows", 110, 0), &
                                          standard_arg_type("-batch", "--batch", 100, 0), &
                                          standard_arg_type("-script", "--script", 100, 1), &
                                          standard_arg_type("-daemon", "--daemon", 99, 0), &
                                          standard_arg_type("-bg-daemon", "--bg-daemon", 99, 0), &
                                          standard_arg_type("-fg-daemon", "--fg-daemon", 99, 0), &
                                          standard_arg_type("-help", "--help", 90, 0), &
                                          standard_arg_type("-nl", "--no-loadup", 70, 0), &
                                          standard_arg_type("-nsl", "--no-site-lisp", 65, 0), &
                                          standard_arg_type("-no-build-details", "--no-build-details", 63, 0), &
                                          standard_arg_type("-module-assertions", "--module-assertions", 62, 0), &
                                          standard_arg_type("-d", "--display", 60, 1), &
                                          standard_arg_type("-display", "", 60, 1), &
                                          standard_arg_type("-Q", "--quick", 55, 0), &
                                          standard_arg_type("-quick", "", 55, 0), &
                                          standard_arg_type("-x", "", 55, 0), &
                                          standard_arg_type("-q", "--no-init-file", 50, 0), &
                                          standard_arg_type("-no-init-file", "", 50, 0), &
                                          standard_arg_type("-init-directory", "--init-directory", 30, 1), &
                                          standard_arg_type("-no-x-resources", "--no-x-resources", 40, 0), &
                                          standard_arg_type("-no-site-file", "--no-site-file", 40, 0), &
                                          standard_arg_type("-no-comp-spawn", "--no-comp-spawn", 60, 0), &
                                          standard_arg_type("-u", "--user", 30, 1), &
                                          standard_arg_type("-user", "", 30, 1), &
                                          standard_arg_type("-debug-init", "--debug-init", 20, 0), &
                                          standard_arg_type("-iconic", "--iconic", 15, 0), &
                                          standard_arg_type("-D", "--basic-display", 12, 0), &
                                          standard_arg_type("-basic-display", "", 12, 0), &
                                          standard_arg_type("-nbc", "--no-blinking-cursor", 12, 0), &
                                          standard_arg_type("-nbi", "--no-bitmap-icon", 10, 0), &
                                          standard_arg_type("-bg", "--background-color", 10, 1), &
                                          standard_arg_type("-background", "", 10, 1), &
                                          standard_arg_type("-fg", "--foreground-color", 10, 1), &
                                          standard_arg_type("-foreground", "", 10, 1), &
                                          standard_arg_type("-bd", "--border-color", 10, 1), &
                                          standard_arg_type("-bw", "--border-width", 10, 1), &
                                          standard_arg_type("-ib", "--internal-border", 10, 1), &
                                          standard_arg_type("-ms", "--mouse-color", 10, 1), &
                                          standard_arg_type("-cr", "--cursor-color", 10, 1), &
                                          standard_arg_type("-fn", "--font", 10, 1), &
                                          standard_arg_type("-font", "", 10, 1), &
                                          standard_arg_type("-fs", "--fullscreen", 10, 0), &
                                          standard_arg_type("-fw", "--fullwidth", 10, 0), &
                                          standard_arg_type("-fh", "--fullheight", 10, 0), &
                                          standard_arg_type("-mm", "--maximized", 10, 0), &
                                          standard_arg_type("-g", "--geometry", 10, 1), &
                                          standard_arg_type("-geometry", "", 10, 1), &
                                          standard_arg_type("-T", "--title", 10, 1), &
                                          standard_arg_type("-title", "", 10, 1), &
                                          standard_arg_type("-name", "--name", 10, 1), &
                                          standard_arg_type("-xrm", "--xrm", 10, 1), &
                                          standard_arg_type("-parent-id", "--parent-id", 10, 1), &
                                          standard_arg_type("-r", "--reverse-video", 5, 0), &
                                          standard_arg_type("-rv", "", 5, 0), &
                                          standard_arg_type("-reverse", "", 5, 0), &
                                          standard_arg_type("-hb", "--horizontal-scroll-bars", 5, 0), &
                                          standard_arg_type("-vb", "--vertical-scroll-bars", 5, 0), &
                                          standard_arg_type("-color", "--color", 5, 0), &
                                          standard_arg_type("-no-splash", "--no-splash", 3, 0), &
                                          standard_arg_type("-no-desktop", "--no-desktop", 3, 0), &
                                          standard_arg_type("-temacs", "--temacs", 1, 1), &
                                          standard_arg_type("-dump-file", "--dump-file", 1, 1), &
                                          standard_arg_type("-seccomp", "--seccomp", 1, 1), &
                                          standard_arg_type("-L", "--directory", 0, 1), &
                                          standard_arg_type("-directory", "", 0, 1), &
                                          standard_arg_type("-l", "--load", 0, 1), &
                                          standard_arg_type("-load", "", 0, 1), &
                                          standard_arg_type("-scriptload", "", 0, 1), &
                                          standard_arg_type("-f", "--funcall", 0, 1), &
                                          standard_arg_type("-funcall", "", 0, 1), &
                                          standard_arg_type("-eval", "--eval", 0, 1), &
                                          standard_arg_type("-execute", "--execute", 0, 1), &
                                          standard_arg_type("-find-file", "--find-file", 0, 1), &
                                          standard_arg_type("-visit", "--visit", 0, 1), &
                                          standard_arg_type("-file", "--file", 0, 1), &
                                          standard_arg_type("-insert", "--insert", 0, 1), &
                                          standard_arg_type("-kill", "--kill", -10, 0)]

contains

    !> keyboard.c's redisplay before waiting for input.
    subroutine do_redisplay()
        type(status_type) :: status
        call redisplay(selected_frame, status)
    end subroutine do_redisplay

    !> Whether terminal type `term' gets term/xterm.el's setup, as
    !> faces.el:tty-run-terminal-initialization finds term/xterm.el for
    !> "xterm*" (and term/screen.el and term/tmux.el, which use it).
    logical function xterm_like(term)
        character(len=*), intent(in) :: term
        xterm_like = index(term, "xterm") == 1 .or. index(term, "screen") == 1 .or. &
                     index(term, "tmux") == 1
    end function xterm_like

    !> Whether `s' is "+" followed by digits, then optionally ":" and
    !> digits; `line' gets the first number and `column' the second (0
    !> if absent).
    logical function line_arg_p(s, line, column)
        character(len=*), intent(in) :: s
        integer(int64), intent(out) :: line, column
        integer :: colon, stat
        line_arg_p = .false.
        line = 0
        column = 0
        if (len(s) < 2) return
        if (s(1:1) /= "+") return
        colon = index(s, ":")
        if (colon == 0) colon = len(s) + 1
        if (.not. all_digits(s(2:colon - 1))) return
        if (colon <= len(s)) then
            if (.not. all_digits(s(colon + 1:))) return
        end if
        read (s(2:colon - 1), *, iostat=stat) line
        line_arg_p = stat == 0
        if (line_arg_p .and. colon <= len(s)) then
            read (s(colon + 1:), *, iostat=stat) column
            line_arg_p = stat == 0
        end if
    contains

        pure logical function all_digits(t)
            character(len=*), intent(in) :: t
            all_digits = len(t) > 0 .and. verify(t, "0123456789") == 0
        end function all_digits
    end function line_arg_p

    !> Visit the files named in `args', leaving the last one current:
    !> "+LINE" puts point on that line of the next file, "--" makes the
    !> rest file names, and "-file", "-find-file" or "-visit" (or with
    !> "--") name a file.  Port of the file part of
    !> startup.el:command-line-1 (other options are unknown).
    subroutine command_line_1(args, status)
        type(arg_type), intent(in) :: args(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: argi, dir
        integer(int64) :: line, n, column, c
        integer :: k
        logical :: just_files
        line = 0
        column = 0
        just_files = .false.
        k = 0
        do while (k < size(args))
            k = k + 1
            argi = args(k)%s
            if (.not. just_files .and. line_arg_p(argi, n, c)) then
                line = n
                column = c
                cycle
            end if
            if (.not. just_files .and. len(argi) > 0) then
                if (argi == "--") then
                    just_files = .true.
                    cycle
                end if
                if (argi(1:1) == "-") then
                    ! --foo is the same as -foo.
                    if (len(argi) > 2) then
                        if (argi(1:2) == "--") argi = argi(2:)
                    end if
                    select case (argi)
                    case ("-file", "-find-file", "-visit")
                        if (k == size(args)) then
                            call set_error(status, FM_GENERIC_ERROR, format_message("File name omitted from `")//argi// &
                                           format_message("' option"))
                            return
                        end if
                        k = k + 1
                        argi = args(k)%s
                    case default
                        call set_error(status, FM_GENERIC_ERROR, format_message("Unknown option `")//args(k)%s//format_message("'"))
                        return
                    end select
                end if
            end if
            ! process-file-arg
            dir = file_name_as_directory(emacs_wd)
            if (len(emacs_wd) == 0 .and. .not. file_name_absolute_p(argi)) then
                call message("Ignoring relative file name ("//argi//") due to unset default_directory")
                cycle
            end if
            call visit(argi, dir, line, column, status)
            if (.not. status%ok()) return
            line = 0
            column = 0
        end do
    end subroutine command_line_1

    !> Visit `name' (relative to `dir'), make its buffer current and go to
    !> line `line' if positive.
    subroutine visit(name, dir, line, column, status)
        character(len=*), intent(in) :: name, dir
        integer(int64), intent(in) :: line, column
        type(status_type), intent(out) :: status
        type(handle_type) :: buf
        integer(int64) :: shortage, moved
        if (file_name_absolute_p(name)) then
            call find_file_noselect(name, buf, status)
        else
            call find_file_noselect(dir//name, buf, status)
        end if
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        if (line /= 0) then
            call goto_char(point_min())
            shortage = forward_line(line - 1)
        end if
        if (column >= 1) call move_to_column(column - 1, moved)
    end subroutine visit

    !> Run the editor on terminal type `terminal_type' at file descriptor
    !> `fd' until the user leaves, visiting the files in `args' first.
    !> Port of the terminal path of emacs.c:main (init_display, then the
    !> file part of startup.el:command-line, then Frecursive_edit).
    subroutine emacs_main(terminal_type, fd, status, args, configure)
        character(len=*), intent(in) :: terminal_type
        integer(int32), intent(in) :: fd
        type(status_type), intent(out) :: status
        type(arg_type), intent(in), optional :: args(:)
        procedure(init_proc), optional :: configure
        type(handle_type) :: scratch
        type(status_type) :: st, opt_status, color_status
        type(arg_type), allocatable :: rest(:)
        logical :: init
        call init_fileio()
        call init_bindings()
        call get_buffer_create("*scratch*", scratch, status)
        if (.not. status%ok()) return
        call set_buffer(scratch, status)
        if (.not. status%ok()) return
        ! buffer.c:init_buffer: *scratch* starts in the working directory.
        if (len(emacs_wd) > 0) buffers(cur)%directory = file_name_as_directory(emacs_wd)
        ! A fresh frame (emacs_main may run more than once in tests).
        call delete_window_tree(selected_frame%root_window)
        call delete_window_object(selected_frame%minibuffer_window)
        selected_frame%root_window = null_handle()
        selected_frame%selected_window = null_handle()
        selected_frame%minibuffer_window = null_handle()
        selected_frame%minibuffer_selected = .false.
        selected_frame%garbaged = .true.
        call init_tty(selected_frame%tty, terminal_type, fd, status)
        if (.not. status%ok()) return
        ! startup.el:command-line registers the 8 default colors, then
        ! faces.el:tty-run-terminal-initialization runs term/xterm.el,
        ! which replaces them by xterm's for the terminal's color count.
        ! Its error is shown once the editor is up.
        call tty_register_colors(selected_frame%tty%tn_max_colors, .false., color_status)
        if (xterm_like(terminal_type)) call terminal_init_xterm(color_status)
        call set_input_fd(fd)
        call install_interrupt_handler()
        call adjust_frame_glyphs(selected_frame)
        call init_frame_windows(selected_frame)
        ! The faces of faces.el (defined when it is loaded), set up for
        ! the terminal (frame-set-background-mode).
        call init_faces()
        call init_frame_faces(selected_frame, terminal_type)
        ! global-font-lock-mode's delayed initialization (its :init-value
        ! is true when not in batch mode), before the init file.
        call minor_modes%global_font_lock_mode%enable(st)
        call tty_set_terminal_modes(selected_frame%tty)
        ! startup.el:command-line: the options, then the init file, then
        ! command-line-1 for the rest.  Errors are shown once the editor
        ! is up.
        if (present(args)) then
            call command_line(args, rest, init, opt_status)
        else
            allocate (rest(0))
            init = .true.
        end if
        ! A reminder from `run`; errors from what follows replace it.
        if (allocated(startup_message)) then
            if (len(startup_message) > 0) call message(startup_message)
            startup_message = ""
        end if
        if (.not. color_status%ok()) call message(format_error(color_status))
        init_file_had_error = .false.
        if (opt_status%ok()) then
            if (init .and. present(configure)) then
                call load_init_file(configure)
                ! The faces as the configuration set them
                ! (tty-run-terminal-initialization after the init file).
                call init_frame_faces(selected_frame, terminal_type)
            end if
            ! If the abbrevs came entirely from the init file or the
            ! abbrevs file, they do not need saving.
            abbrevs_changed = .false.
            call command_line_1(rest, st)
            if (.not. st%ok()) call message(format_error(st))
        else
            ! An error in command-line's options ends it there.
            call message(format_error(opt_status))
        end if
        redisplay_hook => do_redisplay
        ring_bell_hook => frame_ring_bell
        call command_loop(st)
        ring_bell_hook => null()
        redisplay_hook => null()
        call delete_tty(selected_frame%tty, status)
        call tty_flush(selected_frame%tty, st)
        call set_input_fd(-1_int32)
        ! shut_down_emacs: hang up the subprocesses.
        call kill_buffer_processes(null_handle())
    end subroutine emacs_main

    !> Run the init file's `configure'.  An error is shown in the echo area
    !> and sets `init_file_had_error'; startup goes on.  Port of
    !> startup.el:startup--load-user-init-file (the init file is compiled
    !> in, so loading it is calling it).
    subroutine load_init_file(configure)
        procedure(init_proc) :: configure
        type(status_type) :: status
        init_file_had_error = .false.
        user_init_file = init_file_name()
        call configure(status)
        if (.not. status%ok()) then
            init_file_had_error = .true.
            call message("An error occurred while loading the init file: "//format_error(status))
        end if
    end subroutine load_init_file

    !> Handle the options that startup.el handles before the init file:
    !> -q (--no-init-file) and -Q (--quick) turn off the init file
    !> (`init' false), -no-site-file is accepted (there is no site file);
    !> long options may be abbreviated and take --OPTION=VALUE.  `rest'
    !> gets the arguments from the first other one on, for
    !> `command_line_1'.  Port of the option loop of startup.el:command-line
    !> (the options for X, the debugger, other users' and directories'
    !> init files, the dump and native compilation are not ported).
    subroutine command_line(args, rest, init, status)
        type(arg_type), intent(in) :: args(:)
        type(arg_type), allocatable, intent(out) :: rest(:)
        logical, intent(out) :: init
        type(status_type), intent(out) :: status
        character(len=16), parameter :: longopts(14) = [character(len=16) :: &
                                                        "--no-init-file", "--no-site-file", "--no-x-resources", &
                                                        "--debug-init", "--user", "--iconic", "--icon-type", "--quick", &
                                                        "--no-blinking-cursor", "--basic-display", "--dump-file", &
                                                        "--temacs", "--seccomp", "--init-directory"]
        character(len=:), allocatable :: argi, orig_argi, completion
        logical :: has_argval, done, exact
        integer :: k, eq
        status = status_type()
        init = .true.
        k = 0
        done = .false.
        do while (.not. done .and. k < size(args))
            k = k + 1
            argi = args(k)%s
            orig_argi = argi
            has_argval = .false.
            ! Handle --OPTION=VALUE format.
            eq = index(argi, "=")
            if (index(argi, "--") == 1 .and. eq > 0) then
                has_argval = .true.
                argi = argi(1:eq - 1)
            end if
            if (len(orig_argi) >= 3) then
                if (orig_argi(1:2) == "--") then
                    call try_completion_list(argi, longopts, completion, exact)
                    if (exact) then
                        argi = argi(2:)
                    else if (len(completion) > 0) then
                        if (.not. any(longopts == completion)) then
                            call set_error(status, FM_GENERIC_ERROR, format_message("Option `")//argi// &
                                           format_message("' is ambiguous"))
                            return
                        end if
                        argi = completion(2:)
                    else
                        has_argval = .false.
                        argi = orig_argi
                    end if
                end if
            end if
            select case (argi)
            case ("-Q", "-quick", "-q", "-no-init-file")
                init = .false.
            case ("-no-site-file")
                ! site-run-file: Fortmacs has no site file.
                continue
            case default
                ! Put the argument back for command-line-1.
                k = k - 1
                done = .true.
            end select
            ! Was argval set but not used?
            if (has_argval) then
                call set_error(status, FM_GENERIC_ERROR, format_message("Option `")//argi// &
                               format_message("' doesn't allow an argument"))
                return
            end if
        end do
        rest = args(k + 1:)
    end subroutine command_line

    !> Put the options in ARGS in order of decreasing priority (ties in
    !> their order), each with its arguments, removing duplicates of
    !> options without arguments; everything after "--" stays at the end.
    !> OK is false (and ERROR says why) when an option lacks its argument.
    !> Port of emacs.c:sort_args.
    subroutine sort_args(args, ok, error)
        type(arg_type), allocatable, intent(inout) :: args(:)
        logical, intent(out) :: ok
        character(len=:), allocatable, intent(out) :: error
        type(arg_type), allocatable :: new(:)
        integer, allocatable :: options(:), priority(:)
        logical, allocatable :: used(:)
        integer :: argc, from, i, match, thislen, equals, to, incoming_used, best, best_priority
        ok = .true.
        error = ""
        argc = size(args)
        allocate (options(argc), priority(argc), used(argc), new(argc))
        options = -1
        priority = 0
        used = .false.
        ! Categorize all the options, and figure out which args are option
        ! arguments.
        from = 1
        categorize: do while (from <= argc)
            options(from) = -1
            priority(from) = 0
            if (index(args(from)%s, "-") == 1) then
                ! If we have found "--", don't consider any more arguments
                ! as options.
                if (args(from)%s == "--") then
                    priority(from:) = -100
                    options(from:) = -1
                    exit categorize
                end if
                ! Look for a match with a known old-fashioned option.
                do i = 1, size(standard_args)
                    if (args(from)%s == trim(standard_args(i)%name)) then
                        options(from) = standard_args(i)%nargs
                        priority(from) = standard_args(i)%priority
                        if (from + standard_args(i)%nargs > argc) then
                            ok = .false.
                            error = "Option '"//args(from)%s//"' requires an argument"
                            return
                        end if
                        from = from + standard_args(i)%nargs + 1
                        cycle categorize
                    end if
                end do
                ! Look for a match with a known long option.
                if (index(args(from)%s, "--") == 1) then
                    equals = index(args(from)%s, "=")
                    thislen = len(args(from)%s)
                    if (equals > 0) thislen = equals - 1
                    match = -1
                    do i = 1, size(standard_args)
                        if (len_trim(standard_args(i)%longname) == 0) cycle
                        if (standard_args(i)%longname(1:min(thislen, 24)) == args(from)%s(1:thislen) .and. &
                            thislen <= 24) then
                            if (match == -1) then
                                match = i
                            else
                                match = -2
                            end if
                        end if
                    end do
                    if (match > 0) then
                        options(from) = standard_args(match)%nargs
                        priority(from) = standard_args(match)%priority
                        ! If --OPTION=VALUE syntax is used, this option
                        ! uses just one argument.
                        if (equals > 0) options(from) = 0
                        if (from + options(from) > argc) then
                            ok = .false.
                            error = "Option '"//args(from)%s//"' requires an argument"
                            return
                        end if
                        from = from + options(from)
                    else if (match == -2) then
                        write (error_unit, "(a)") "Option '"//args(from)%s//"' matched multiple standard arguments"
                    end if
                end if
            end if
            from = from + 1
        end do categorize
        ! Copy the arguments, in order of decreasing priority, to NEW.
        to = 0
        incoming_used = 0
        do while (incoming_used < argc)
            best = -1
            best_priority = -9999
            ! Find the highest priority remaining option.  If several have
            ! equal priority, take the first of them.
            from = 1
            do while (from <= argc)
                if (.not. used(from) .and. priority(from) > best_priority) then
                    best_priority = priority(from)
                    best = from
                end if
                ! Skip option arguments: they are tied to the options.
                if (options(from) > 0) from = from + options(from)
                from = from + 1
            end do
            ! Copy it, with its args, unless it is a duplicate of the
            ! previous one.
            if (.not. dup_of_previous(best)) then
                to = to + 1
                new(to) = args(best)
                do i = 1, options(best)
                    to = to + 1
                    new(to) = args(best + i)
                end do
            end if
            incoming_used = incoming_used + 1 + max(options(best), 0)
            used(best:best + max(options(best), 0)) = .true.
        end do
        args = new(1:to)
    contains
        logical function dup_of_previous(b)
            integer, intent(in) :: b
            dup_of_previous = .false.
            if (options(b) /= 0 .or. to == 0) return
            dup_of_previous = new(to)%s == args(b)%s
        end function dup_of_previous
    end subroutine sort_args

    !> Whether the argument after the first SKIP of ARGS is the option
    !> SSTR or an abbreviation of LSTR at least MINLEN long (taking no
    !> argument); if so SKIP counts it.  Port of emacs.c:argmatch
    !> (without VALPTR).
    logical function argmatch(args, sstr, lstr, minlen, skip) result(found)
        type(arg_type), intent(in) :: args(:)
        character(len=*), intent(in) :: sstr, lstr
        integer, intent(in) :: minlen
        integer, intent(inout) :: skip
        character(len=:), allocatable :: arg
        found = .false.
        if (size(args) <= skip) return
        arg = args(skip + 1)%s
        if (arg == sstr .and. len(arg) == len(sstr)) then
            found = .true.
        else if (len(arg) >= minlen .and. len(arg) <= len(lstr)) then
            found = arg == lstr(1:len(arg))
        end if
        if (found) skip = skip + 1
    end function argmatch

    !> What `try_completion' gives for STRING against the names
    !> COLLECTION (blank-padded): EXACT when STRING is the only completion
    !> and matches exactly (t), else the longest common prefix of the
    !> completions in COMPLETION ("" for none, nil).
    subroutine try_completion_list(string, collection, completion, exact)
        character(len=*), intent(in) :: string, collection(:)
        character(len=:), allocatable, intent(out) :: completion
        logical, intent(out) :: exact
        integer :: i, n, j
        character(len=:), allocatable :: c
        logical :: first
        exact = .false.
        completion = ""
        n = 0
        first = .true.
        do i = 1, size(collection)
            c = trim(collection(i))
            if (len(c) < len(string)) cycle
            if (c(1:len(string)) /= string) cycle
            n = n + 1
            if (first) then
                completion = c
                first = .false.
            else
                j = 0
                do while (j < min(len(completion), len(c)))
                    if (completion(j + 1:j + 1) /= c(j + 1:j + 1)) exit
                    j = j + 1
                end do
                completion = completion(1:j)
            end if
        end do
        if (n == 1 .and. completion == string .and. len(completion) == len(string)) exact = .true.
    end subroutine try_completion_list

    !> The program: handle --version, --help and --recompile, check for a
    !> terminal, then run the editor with the user's `configure'.
    !> `exit_code' is the process's exit status (the main program stops
    !> with it).  An installed fortmacs runs the user's build instead, if
    !> there is an up-to-date one; a user build passes `prefix', the
    !> installation it was built against.  Port of the startup part of
    !> emacs.c:main.
    subroutine run(configure, exit_code, prefix)
        procedure(init_proc) :: configure
        integer, intent(out) :: exit_code
        character(len=*), intent(in), optional :: prefix
        character(len=*), parameter :: usage = "Usage: fortmacs [--version | --help] [-q] [+LINE] [FILE]..."
        character(len=:), allocatable :: term, install, reminder
        type(arg_type), allocatable :: args(:)
        type(status_type) :: status
        integer :: nargs, arglen, stat, k
        logical :: installed, ok
        character(len=:), allocatable :: error
        integer :: skip
        exit_code = 0
        nargs = command_argument_count()
        allocate (args(nargs))
        do k = 1, nargs
            call get_command_argument(k, length=arglen)
            allocate (character(len=arglen) :: args(k)%s)
            call get_command_argument(k, value=args(k)%s)
        end do
        ! Which installation this is: a user build knows; an installed
        ! fortmacs finds out from where it is.
        if (present(prefix)) then
            install = prefix
            installed = .true.
        else
            call install_prefix(running_executable(), install, installed)
        end if
        if (nargs > 0) then
            if (args(1)%s == "--recompile" .or. args(1)%s == "-recompile") then
                call do_recompile(installed, install, exit_code)
                return
            end if
            select case (args(1)%s)
            case ("--list-generations", "-list-generations", "--rollback", "-rollback", &
                  "--switch-generation", "-switch-generation")
                call do_generations(args, exit_code)
                return
            case default
                ! Not a generation command.
            end select
        end if
        reminder = ""
        if (.not. present(prefix) .and. installed) call run_user_build(args, reminder)
        if (present(prefix)) then
            if (file_newer_than_file_p(init_file_name(), running_executable())) then
                reminder = init_file_name()//" has changed; run fortmacs --recompile to use it"
            end if
        end if
        call sort_args(args, ok, error)
        if (.not. ok) then
            write (error_unit, "(a)") "fortmacs: "//error
            exit_code = 1
            return
        end if
        skip = 0
        if (argmatch(args, "-version", "--version", 3, skip)) then
            write (output_unit, "(a)") version_string()
            if (present(prefix)) call write_configuration()
            return
        end if
        ! -nw: Fortmacs only uses the terminal.
        if (.not. argmatch(args, "-nw", "--no-window-system", 6, skip)) then
            if (argmatch(args, "-nw", "--no-windows", 6, skip)) continue
        end if
        if (argmatch(args, "-help", "--help", 3, skip)) then
            write (output_unit, "(a)") usage
            write (output_unit, "(a)") "       fortmacs --recompile | --list-generations | --rollback | --switch-generation N"
            write (output_unit, "(a)") "Edit the FILEs (or *scratch*) in the terminal; C-x C-s saves, C-x C-c quits."
            write (output_unit, "(a)") "+LINE puts point on line LINE of the next FILE."
            write (output_unit, "(a)") "-q (--no-init-file) skips the configure of your init.f90."
            write (output_unit, "(a)") "--recompile builds "//init_file_name()//" into a new generation of your own"
            write (output_unit, "(a)") "fortmacs, which an installed fortmacs runs; --rollback goes back to the generation"
            write (output_unit, "(a)") "before, --switch-generation N to generation N."
            return
        end if
        ! Everything else is for startup.el:command-line.
        args = args(skip + 1:)
        ! emacs.c:main: a terminal is needed on standard input.
        if (.not. emacs_isatty(0_int32)) then
            write (error_unit, "(a)") "fortmacs: standard input is not a tty"
            exit_code = 1
            return
        end if
        call get_environment_variable("TERM", length=arglen, status=stat)
        if (stat /= 0 .or. arglen == 0) then
            write (error_unit, "(a)") "fortmacs: please set the environment variable TERM"
            exit_code = 1
            return
        end if
        allocate (character(len=arglen) :: term)
        call get_environment_variable("TERM", term)
        startup_message = reminder
        call emacs_main(term, 0_int32, status, args, configure)
        if (.not. status%ok()) then
            write (error_unit, "(a)") "fortmacs: "//format_error(status)
            exit_code = 1
        end if
    end subroutine run

    !> `fortmacs --recompile': build the init file into the user's own
    !> fortmacs, reporting on standard output and error.
    subroutine do_recompile(installed, install, exit_code)
        logical, intent(in) :: installed
        character(len=*), intent(in) :: install
        integer, intent(out) :: exit_code
        character(len=:), allocatable :: log
        type(status_type) :: status
        integer :: generation
        exit_code = 1
        if (.not. installed) then
            write (error_unit, "(a)") "fortmacs: --recompile needs an installed Fortmacs "// &
                "(fpm install puts PREFIX/bin/fortmacs next to PREFIX/lib/libfortmacs.a)"
            return
        end if
        call init_fileio()
        call recompile(install, log, status, generation)
        if (len(log) > 0) write (error_unit, "(a)", advance="no") log
        if (.not. status%ok()) then
            write (error_unit, "(a)") "fortmacs: "//format_error(status)
            return
        end if
        write (output_unit, "(a, i0)") "fortmacs: built and switched to generation ", generation
        exit_code = 0
    end subroutine do_recompile

    !> --version in a user build: which configuration it carries.
    subroutine write_configuration()
        integer :: n
        n = generation_of(running_executable())
        if (n > 0) then
            write (output_unit, "(a, i0, a)") "With generation ", n, " of the configuration in "//init_file_name()
        else
            write (output_unit, "(a)") "With the configuration in "//init_file_name()
        end if
    end subroutine write_configuration

    !> --list-generations, --rollback and --switch-generation N, reporting
    !> as Guix does ("switched from generation 3 to 2").
    subroutine do_generations(args, exit_code)
        type(arg_type), intent(in) :: args(:)
        integer, intent(out) :: exit_code
        character(len=:), allocatable :: text, command
        type(status_type) :: status
        integer :: from, to, stat
        exit_code = 1
        call init_fileio()
        command = args(1)%s
        if (command(1:2) == "--") command = command(2:)
        select case (command)
        case ("-list-generations")
            call list_generations(text, status)
            if (status%ok()) write (output_unit, "(a)", advance="no") text
        case ("-rollback")
            call rollback(from, to, status)
        case default
            to = 0
            stat = 1
            if (size(args) >= 2) read (args(2)%s, *, iostat=stat) to
            if (stat /= 0 .or. to <= 0) then
                write (error_unit, "(a)") "fortmacs: --switch-generation needs a generation number"
                return
            end if
            from = current_generation()
            call switch_generation(to, status)
        end select
        if (.not. status%ok()) then
            write (error_unit, "(a)") "fortmacs: "//format_error(status)
            return
        end if
        if (command /= "-list-generations") then
            write (output_unit, "(a, i0, a, i0)") "switched from generation ", from, " to ", to
        end if
        exit_code = 0
    end subroutine do_generations

    !> Replace this process with the user's build, if there is one and it
    !> is newer than this fortmacs; otherwise `reminder' says what to do.
    !> Returns only if the build is not run.
    subroutine run_user_build(args, reminder)
        type(arg_type), intent(in) :: args(:)
        character(len=:), allocatable, intent(inout) :: reminder
        type(sys_string_type), allocatable :: argv(:)
        character(len=:), allocatable :: build, init
        integer(int32) :: err
        integer :: k
        build = current_build_name()
        init = init_file_name()
        if (.not. file_executable_p(build)) then
            if (file_exists_p(init)) reminder = init//" is not built yet; run fortmacs --recompile to use it"
            return
        end if
        if (file_newer_than_file_p(running_executable(), build)) then
            reminder = "Fortmacs was updated since "//init//" was built; run fortmacs --recompile"
            return
        end if
        allocate (argv(size(args) + 1))
        argv(1)%s = build
        do k = 1, size(args)
            argv(k + 1)%s = args(k)%s
        end do
        err = emacs_execv(build, argv)
        reminder = "Can't run "//build//": "//emacs_strerror(err)
    end subroutine run_user_build
end module emacs_fm
