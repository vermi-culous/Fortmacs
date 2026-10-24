! config_fm.f90 -- where the configuration lives, and building it.
!
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

!> The user's configuration is a Fortran module, `user_config', in
!> init.f90.  `fortmacs --recompile' compiles it with gfortran together
!> with a small main program against the installed library
!> (PREFIX/lib/libfortmacs.a and the module files in PREFIX/include) into a
!> binary of its own, and an installed fortmacs runs that binary instead
!> of itself.  As in Guix, each build is a numbered generation, kept with
!> the init.f90 it was built from, and `current' points at the one in use:
!> `--list-generations' shows them, `--rollback' goes back to the one
!> before, and `--switch-generation N' picks one.
!>
!> The files follow the XDG base directory specification:
!>
!> - init file: $XDG_CONFIG_HOME/fortmacs/init.f90 (~/.config/fortmacs/)
!> - generations: $XDG_STATE_HOME/fortmacs/generations/N/ (~/.local/state/fortmacs/),
!>   each with `fortmacs' and `init.f90', and the symbolic link `current'
!> - build scratch: $XDG_CACHE_HOME/fortmacs/ (~/.cache/fortmacs/)
!>
!> Not a port: upstream loads its init file at startup instead.
module config_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_string, make_list, t_value, fixnump, xfixnum, xstring, list_elements
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, current_buffer
    use editfns_fm, only: buffer_string
    use env_fm, only: getenv
    use fileio_fm, only: file_name_as_directory, file_name_directory, directory_file_name, &
                         file_name_nondirectory, file_name_absolute_p, get_homedir, file_exists_p, &
                         file_readable_p, file_executable_p, rename_file, report_file_errno, &
                         delete_file_internal, delete_directory_internal, copy_file, &
                         make_symbolic_link, file_attributes, file_stat_type
    use dired_fm, only: directory_files
    use files_fm, only: make_directory
    use callproc_fm, only: call_process
    use sysdep_fm, only: emacs_readlink, emacs_localtime, ENOENT
    implicit none(type, external)
    private

    public :: config_directory, state_directory, cache_directory, init_file_name
    public :: generations_directory, current_build_name, generation_numbers, current_generation, &
              generation_of, switch_generation, rollback, list_generations
    public :: running_executable, install_prefix, driver_source, recompile

    !> The compiler `--recompile' runs.
    character(len=*), parameter :: compiler = "gfortran"

contains

    !> $VARIABLE/fortmacs/ if VARIABLE is set to an absolute directory,
    !> else ~/FALLBACK/fortmacs/ (the XDG base directory rule).
    function xdg_directory(variable, fallback) result(dir)
        character(len=*), intent(in) :: variable, fallback
        character(len=:), allocatable :: dir
        character(len=:), allocatable :: value
        logical :: found
        call getenv(variable, value, found)
        if (found .and. len(value) > 0) then
            if (file_name_absolute_p(value)) then
                dir = file_name_as_directory(value)//"fortmacs/"
                return
            end if
        end if
        dir = file_name_as_directory(get_homedir())//fallback//"/fortmacs/"
    end function xdg_directory

    !> The directory of init.f90.
    function config_directory() result(dir)
        character(len=:), allocatable :: dir
        dir = xdg_directory("XDG_CONFIG_HOME", ".config")
    end function config_directory

    !> The directory of the built binary.
    function state_directory() result(dir)
        character(len=:), allocatable :: dir
        dir = xdg_directory("XDG_STATE_HOME", ".local/state")
    end function state_directory

    !> The directory `--recompile' builds in.
    function cache_directory() result(dir)
        character(len=:), allocatable :: dir
        dir = xdg_directory("XDG_CACHE_HOME", ".cache")
    end function cache_directory

    !> The user's init file.
    function init_file_name() result(name)
        character(len=:), allocatable :: name
        name = config_directory()//"init.f90"
    end function init_file_name

    !> The directory of the generations.
    function generations_directory() result(dir)
        character(len=:), allocatable :: dir
        dir = state_directory()//"generations/"
    end function generations_directory

    !> The binary of the current generation.
    function current_build_name() result(name)
        character(len=:), allocatable :: name
        name = generations_directory()//"current/fortmacs"
    end function current_build_name

    !> The file name of the running program ("" if unknown).
    function running_executable() result(name)
        character(len=:), allocatable :: name
        if (emacs_readlink("/proc/self/exe", name) /= 0) name = ""
    end function running_executable

    !> The installation `executable' belongs to: PREFIX when it is
    !> PREFIX/bin/fortmacs and PREFIX/lib/libfortmacs.a exists (`found').
    subroutine install_prefix(executable, prefix, found)
        character(len=*), intent(in) :: executable
        character(len=:), allocatable, intent(out) :: prefix
        logical, intent(out) :: found
        character(len=:), allocatable :: bin
        found = .false.
        prefix = ""
        bin = directory_file_name(file_name_directory(executable))
        if (len(bin) < 4) return
        if (bin(len(bin) - 3:) /= "/bin") return
        prefix = file_name_directory(bin)
        found = file_exists_p(prefix//"lib/libfortmacs.a")
    end subroutine install_prefix

    !> The main program of a user build: `run' with the user's
    !> `configure', knowing the installation it was built against.
    function driver_source(prefix) result(text)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable :: text
        character(len=*), parameter :: nl = new_line("a")
        text = "! Written by fortmacs --recompile."//nl// &
               "program fortmacs_user"//nl// &
               "    use emacs_fm, only: run"//nl// &
               "    use user_config, only: configure"//nl// &
               "    implicit none(type, external)"//nl// &
               nl// &
               "    integer :: exit_code"//nl// &
               nl// &
               "    call run(configure, exit_code, "//fortran_string(prefix)//")"//nl// &
               "    if (exit_code /= 0) stop exit_code, quiet = .true."//nl// &
               "end program fortmacs_user"//nl
    end function driver_source

    !> `s' as a Fortran string literal, split over continuation lines so
    !> that no line gets too long.
    function fortran_string(s) result(literal)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: literal
        character(len=:), allocatable :: quoted
        integer :: k, n
        quoted = ""
        do k = 1, len(s)
            if (s(k:k) == '"') then
                quoted = quoted//'""'
            else
                quoted = quoted//s(k:k)
            end if
        end do
        literal = '"'
        n = 0
        do k = 1, len(quoted)
            literal = literal//quoted(k:k)
            n = n + 1
            ! Break between characters, never inside a doubled quote.
            if (n >= 60 .and. k < len(quoted) .and. quoted(k:k) /= '"') then
                literal = literal//'" // &'//new_line("a")//'        "'
                n = 0
            end if
        end do
        literal = literal//'"'
    end function fortran_string

    !> Build the init file against the installation at `prefix' into a new
    !> generation, `generation', which becomes the current one.  `log' gets
    !> the compiler's output.  Errors: no init file, no installed library,
    !> or the compilation failed (no generation is made then).
    subroutine recompile(prefix, log, status, generation)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable, intent(out) :: log
        type(status_type), intent(out) :: status
        integer, intent(out), optional :: generation
        character(len=:), allocatable :: init, cache, built, driver, library, gen
        integer, allocatable :: numbers(:)
        integer :: n
        type(value_type) :: args(14), result
        type(handle_type) :: old, buf
        type(status_type) :: ignore
        integer :: unit, ios
        log = ""
        init = init_file_name()
        if (.not. file_readable_p(init)) then
            call report_file_errno("Opening init file", init, ENOENT, status)
            return
        end if
        library = prefix//"lib/libfortmacs.a"
        if (.not. file_exists_p(library)) then
            call set_error(status, FM_GENERIC_ERROR, "Can't find "//library// &
                           "; install Fortmacs with `fpm install`")
            return
        end if
        cache = cache_directory()
        call make_directory(cache, status, parents=.true.)
        if (.not. status%ok()) return
        call generation_numbers(numbers, status)
        if (.not. status%ok()) return
        n = 1
        if (size(numbers) > 0) n = maxval(numbers) + 1
        gen = generations_directory()//itoa(n)//"/"
        call make_directory(gen, status, parents=.true.)
        if (.not. status%ok()) return
        driver = cache//"main.f90"
        open (newunit=unit, file=driver, status="replace", action="write", access="stream", &
              form="unformatted", iostat=ios)
        if (ios == 0) write (unit, iostat=ios) driver_source(prefix)
        if (ios == 0) close (unit, iostat=ios)
        if (ios /= 0) then
            call set_error(status, FM_GENERIC_ERROR, "Can't write "//driver)
            return
        end if
        ! gfortran -O2 -J CACHE -I PREFIX/include INIT DRIVER
        !     -L PREFIX/lib -lfortmacs -o GENERATION/fortmacs
        built = gen//"fortmacs"
        args(1) = make_string("-O2")
        args(2) = make_string("-J")
        args(3) = make_string(cache)
        args(4) = make_string("-I")
        args(5) = make_string(prefix//"include")
        args(6) = make_string("-o")
        args(7) = make_string(built)
        args(8) = make_string(init)
        args(9) = make_string(driver)
        args(10) = make_string("-L")
        args(11) = make_string(prefix//"lib")
        args(12) = make_string("-lfortmacs")
        args(13) = make_string("-fdiagnostics-plain-output")
        args(14) = make_string("-fno-diagnostics-color")
        ! The compiler's output goes to a buffer, made in the cache
        ! directory so the compiler runs there.
        old = current_buffer()
        call get_buffer_create(" *fortmacs-recompile*", buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        buffers(cur)%directory = cache
        call call_process(compiler, result, status, destination=t_value(), args=make_list(args))
        log = buffer_string()
        call kill_buffer(buf, ignore)
        if (old%index /= 0) call set_buffer(old, ignore)
        if (.not. status%ok()) then
            call delete_directory_internal(gen, ignore)
            return
        end if
        if (.not. fixnump(result)) then
            call set_error(status, FM_GENERIC_ERROR, compiler//" was killed: "//xstring(result))
        else if (xfixnum(result) /= 0) then
            call set_error(status, FM_GENERIC_ERROR, "Compiling "//init//" failed")
        end if
        if (.not. status%ok()) then
            if (file_exists_p(built)) call delete_file_internal(built, ignore)
            call delete_directory_internal(gen, ignore)
            return
        end if
        ! The init file it was built from stays with it.
        call copy_file(init, gen//"init.f90", status)
        if (.not. status%ok()) return
        call switch_generation(n, status)
        if (present(generation)) generation = n
    end subroutine recompile

    !> `n' in decimal.
    pure function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=12) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa

    !> The number `s' is made of, or 0 if it isn't only digits.
    pure integer function number_of(s) result(n)
        character(len=*), intent(in) :: s
        integer :: k
        n = 0
        if (len(s) == 0 .or. len(s) > 9) return
        if (verify(s, "0123456789") /= 0) return
        do k = 1, len(s)
            n = 10*n + iachar(s(k:k)) - iachar("0")
        end do
    end function number_of

    !> The numbers of the generations, in increasing order (none if the
    !> directory doesn't exist yet).
    subroutine generation_numbers(numbers, status)
        integer, allocatable, intent(out) :: numbers(:)
        type(status_type), intent(out) :: status
        type(value_type) :: files
        type(value_type), allocatable :: names(:)
        integer, allocatable :: all(:)
        integer :: k, j, n, v
        allocate (numbers(0))
        if (.not. file_exists_p(generations_directory())) return
        call directory_files(generations_directory(), files, status)
        if (.not. status%ok()) return
        call list_elements(files, names)
        allocate (all(size(names)))
        n = 0
        do k = 1, size(names)
            v = number_of(xstring(names(k)))
            if (v == 0) cycle
            ! Sorted by number, not by name ("10" after "9").
            j = n
            do while (j >= 1)
                if (all(j) < v) exit
                all(j + 1) = all(j)
                j = j - 1
            end do
            all(j + 1) = v
            n = n + 1
        end do
        numbers = all(1:n)
    end subroutine generation_numbers

    !> The generation `current' points at, or 0 if none.
    integer function current_generation() result(n)
        character(len=:), allocatable :: target
        n = 0
        if (emacs_readlink(generations_directory()//"current", target) /= 0) return
        n = number_of(file_name_nondirectory(directory_file_name(target)))
    end function current_generation

    !> The generation `executable' belongs to (GENERATIONS/N/fortmacs), or 0.
    integer function generation_of(executable) result(n)
        character(len=*), intent(in) :: executable
        character(len=:), allocatable :: dir
        n = 0
        dir = file_name_directory(executable)
        if (index(dir, generations_directory()) /= 1) return
        n = number_of(directory_file_name(dir(len(generations_directory()) + 1:)))
    end function generation_of

    !> Make generation `n' the current one: `current' is replaced in one
    !> step (a new link renamed over it).
    subroutine switch_generation(n, status)
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir
        dir = generations_directory()
        if (n <= 0 .or. .not. file_executable_p(dir//itoa(n)//"/fortmacs")) then
            call set_error(status, FM_GENERIC_ERROR, "There is no generation "//itoa(n))
            return
        end if
        call make_symbolic_link(itoa(n), dir//"current.new", status, ok_if_already_exists=.true.)
        if (.not. status%ok()) return
        call rename_file(dir//"current.new", dir//"current", status, ok_if_already_exists=.true.)
    end subroutine switch_generation

    !> Go back to the generation before the current one: `from' was
    !> current, `to' is now.
    subroutine rollback(from, to, status)
        integer, intent(out) :: from, to
        type(status_type), intent(out) :: status
        integer, allocatable :: numbers(:)
        integer :: k
        to = 0
        from = current_generation()
        if (from == 0) then
            call set_error(status, FM_GENERIC_ERROR, "There are no generations yet; run fortmacs --recompile")
            return
        end if
        call generation_numbers(numbers, status)
        if (.not. status%ok()) return
        do k = 1, size(numbers)
            if (numbers(k) < from) to = numbers(k)
        end do
        if (to == 0) then
            call set_error(status, FM_GENERIC_ERROR, "There is no generation before generation "//itoa(from))
            return
        end if
        call switch_generation(to, status)
    end subroutine rollback

    !> One line per generation, as Guix lists them: its number, when it
    !> was built, and "(current)" for the current one.
    subroutine list_generations(text, status)
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        character(len=3), parameter :: months(12) = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", &
                                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
        integer, allocatable :: numbers(:)
        type(file_stat_type) :: st
        character(len=20) :: when
        integer :: k, current, t(6)
        logical :: found
        text = ""
        call generation_numbers(numbers, status)
        if (.not. status%ok()) return
        current = current_generation()
        do k = 1, size(numbers)
            call file_attributes(generations_directory()//itoa(numbers(k))//"/fortmacs", st, found)
            when = ""
            if (found) then
                t = emacs_localtime(st%mtime_sec)
                write (when, "(a3, 1x, i2.2, 1x, i4, 1x, i2.2, ':', i2.2, ':', i2.2)") months(t(2)), t(3), t(1), &
                    t(4), t(5), t(6)
            end if
            text = text//"Generation "//itoa(numbers(k))//achar(9)//trim(when)
            if (numbers(k) == current) text = text//achar(9)//"(current)"
            text = text//new_line("a")
        end do
    end subroutine list_generations
end module config_fm
