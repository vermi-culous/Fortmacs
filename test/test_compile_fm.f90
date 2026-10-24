! test_compile_fm.f90 -- tests for compile_fm.
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

!> Ports of compile-tests.el (compile-test-error-regexps and
!> compile-test-functions; compile-test-grep-regexps waits for grep), and
!> own tests.  The waits are bounded by a generous wall-clock limit, not by
!> fixed delays.
module test_compile_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer, get_buffer, &
                         buffer_live_p, set_buffer_read_only
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string, point_max
    use keyboard_fm, only: last_message
    use fileio_fm, only: file_name_as_directory
    use files_fm, only: temporary_file_directory, abbreviate_file_name
    use keymap_fm, only: key_binding, key_parse
    use lisp_fm, only: value_type, symbolp, xsymbol, symbols
    use bindings_fm, only: init_bindings
    use process_fm, only: accept_process_output, get_buffer_process, num_processors
    use compile_fm, only: compile, recompile, kill_compilation, compile_command, comint_carriage_motion, &
                          compilation_always_kill, compilation_parse_errors, compilation_forget_errors, &
                          compilation_message_info, compilation_message_info_type, compilation_message_counts, &
                          compilation_num_errors_found, compilation_num_warnings_found, compilation_num_infos_found, &
                          compilation_error_regexp_alist, compilation_error_regexp_alist_alist, compilation_rule_type, &
                          compilation_next_error, compile_goto_error, compilation_ensure_parse
    use editfns_fm, only: point, point_min, goto_char
    use simple_fm, only: next_error
    use xdisp_fm, only: mode_line_process_text
    implicit none(type, external)
    private

    public :: collect_compile_fm

    integer, parameter :: TIMEOUT = 120

contains

    subroutine collect_compile_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("compile-command-default", test_default_command), &
                    new_unittest("compile-finished", test_finished), &
                    new_unittest("compile-exited-abnormally", test_abnormal), &
                    new_unittest("compile-cd", test_cd), &
                    new_unittest("recompile", test_recompile), &
                    new_unittest("kill-compilation", test_kill), &
                    new_unittest("comint-carriage-motion", test_carriage_motion), &
                    new_unittest("compilation-mode", test_mode), &
                    new_unittest("compile-test-error-regexps", test_error_regexps), &
                    new_unittest("compile-test-functions", test_functions), &
                    new_unittest("compile-directory-tracking", test_directory_tracking), &
                    new_unittest("compile-next-error", test_next_error), &
                    new_unittest("compile-comint", test_comint_compile) &
                    ]
    end subroutine collect_compile_fm

    !> A fresh source buffer in `dir', shown alone.
    subroutine source_buffer(h, dir)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: dir
        type(status_type) :: st
        call get_buffer_create("*compile-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        buffers(cur)%directory = dir
        call show_alone()
        ! (Deleting the other windows makes the selected window's buffer
        ! current.)
        call set_buffer(h, st)
        call show_alone()
    end subroutine source_buffer

    !> Wait until the compilation in *compilation* has finished.
    subroutine wait_compilation()
        type(handle_type) :: buf, proc
        type(status_type) :: st
        integer(int64) :: start, now, rate
        logical :: got
        buf = get_buffer("*compilation*")
        call system_clock(start, rate)
        do
            proc = get_buffer_process(buf)
            if (proc%index == 0) exit
            call system_clock(now)
            if ((now - start)/max(rate, 1_int64) > TIMEOUT) exit
            call accept_process_output(got, st, proc, 1.0)
        end do
    end subroutine wait_compilation

    !> The text of *compilation*.
    function compilation_text() result(text)
        character(len=:), allocatable :: text
        type(handle_type) :: old, buf
        type(status_type) :: st
        text = ""
        buf = get_buffer("*compilation*")
        if (.not. buffer_live_p(buf)) return
        old = buffers_current()
        call set_buffer(buf, st)
        text = buffer_string()
        call set_buffer(old, st)
    end function compilation_text

    function buffers_current() result(h)
        use buffer_fm, only: current_buffer
        type(handle_type) :: h
        h = current_buffer()
    end function buffers_current

    !> Line `n' of `text'.
    function line(text, n) result(l)
        character(len=*), intent(in) :: text
        integer, intent(in) :: n
        character(len=:), allocatable :: l
        integer :: k, b, e
        b = 1
        do k = 1, n - 1
            e = index(text(b:), new_line("a"))
            if (e == 0) then
                l = ""
                return
            end if
            b = b + e
        end do
        e = index(text(b:), new_line("a"))
        if (e == 0) then
            l = text(b:)
        else
            l = text(b:b + e - 2)
        end if
    end function line

    subroutine test_default_command(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=16) :: n
        character(len=:), allocatable :: expected
        if (allocated(compile_command%value)) deallocate (compile_command%value)
        call init_bindings()
        write (n, "(i0)") (2*num_processors() + 2)/3
        expected = "make -k -j"//trim(n)//" "
        call check(error, compile_command%value, expected)
    end subroutine test_default_command

    !> The header, the output and the end line.
    subroutine test_finished(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text, dir
        call init_bindings()
        dir = file_name_as_directory(temporary_file_directory())
        call source_buffer(h, dir)
        last_message = ""
        call compile("echo built", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        text = compilation_text()
        call check(error, line(text, 1), "-*- mode: compilation; default-directory: """//abbreviate_file_name(dir)//""" -*-")
        if (.not. allocated(error)) call check(error, index(line(text, 2), "Compilation started at ") == 1, line(text, 2))
        if (.not. allocated(error)) call check(error, len(line(text, 2)), len("Compilation started at Fri Oct 24 10:00:00"))
        if (.not. allocated(error)) call check(error, line(text, 3), "")
        if (.not. allocated(error)) call check(error, line(text, 4), "echo built")
        if (.not. allocated(error)) call check(error, line(text, 5), "built")
        if (.not. allocated(error)) call check(error, line(text, 6), "")
        if (.not. allocated(error)) then
            call check(error, index(line(text, 7), "Compilation finished at ") == 1, line(text, 7))
        end if
        if (.not. allocated(error)) call check(error, index(line(text, 7), ", duration ") > 0, line(text, 7))
        if (.not. allocated(error)) call check(error, last_message, "Compilation finished")
        call kill_buffer(h, st)
    end subroutine test_finished

    subroutine test_abnormal(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text
        call source_buffer(h, file_name_as_directory(temporary_file_directory()))
        call compile("exit 2", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        text = compilation_text()
        call check(error, index(line(text, 6), "Compilation exited abnormally with code 2 at ") == 1, line(text, 6))
        if (.not. allocated(error)) call check(error, last_message, "Compilation exited abnormally with code 2")
        call kill_buffer(h, st)
    end subroutine test_abnormal

    !> A leading "cd DIR;" sets the buffer's directory (the shell does the
    !> cd).
    subroutine test_cd(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf
        type(status_type) :: st
        call source_buffer(h, "/")
        call compile("cd /tmp; pwd", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        call check(error, line(compilation_text(), 5), "/tmp")
        if (allocated(error)) return
        buf = get_buffer("*compilation*")
        call check(error, buffers(buf%index)%directory, "/tmp/")
        call kill_buffer(h, st)
    end subroutine test_cd

    !> recompile in *compilation* runs the same command again.
    subroutine test_recompile(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf
        type(status_type) :: st
        call source_buffer(h, file_name_as_directory(temporary_file_directory()))
        call compile("echo again", st)
        call wait_compilation()
        buf = get_buffer("*compilation*")
        call set_buffer(buf, st)
        compile_command = "echo other"
        call recompile(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        call check(error, line(compilation_text(), 4), "echo again")
        if (.not. allocated(error)) call check(error, line(compilation_text(), 5), "again")
        call kill_buffer(h, st)
    end subroutine test_recompile

    !> kill-compilation interrupts it.
    subroutine test_kill(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call source_buffer(h, file_name_as_directory(temporary_file_directory()))
        compilation_always_kill = .true.
        call compile("sleep 100", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call kill_compilation(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        call check(error, index(line(compilation_text(), 6), "Compilation interrupt at ") == 1, line(compilation_text(), 6))
        compilation_always_kill = .false.
        call kill_buffer(h, st)
    end subroutine test_kill

    !> CR and BS in output.
    subroutine test_carriage_motion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call source_buffer(h, "/")
        call insert("abc"//achar(13)//"xy"//new_line("a")//"ab"//achar(8)//"c"//new_line("a")//"end"//achar(13), st)
        call comint_carriage_motion(1_int64, point_max())
        call check(error, buffer_string(), "xy"//new_line("a")//"ac"//new_line("a")//"end")
        call kill_buffer(h, st)
    end subroutine test_carriage_motion

    !> The mode of *compilation*: read-only, its keys.
    subroutine test_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf
        type(status_type) :: st
        type(value_type), allocatable :: key(:)
        type(value_type) :: b
        call init_bindings()
        call source_buffer(h, file_name_as_directory(temporary_file_directory()))
        call compile("true", st)
        call wait_compilation()
        buf = get_buffer("*compilation*")
        call set_buffer(buf, st)
        call check(error, buffers(cur)%mode_name, "Compilation")
        if (.not. allocated(error)) call check(error, buffers(cur)%read_only, "read-only")
        if (.not. allocated(error)) then
            call key_parse("g", key, st)
            call key_binding(key, b, st)
            call check(error, symbolp(b), "bound")
            if (.not. allocated(error)) call check(error, trim(symbols(xsymbol(b))%name), "recompile")
        end if
        call set_buffer_read_only(buf, .false., st)
        call kill_buffer(buf, st)
        call kill_buffer(h, st)
    end subroutine test_mode
    !> A buffer for parsing, empty, in Fundamental mode.
    subroutine parse_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        call get_buffer_create("*compile-regexps*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
    end subroutine parse_buffer

    !> Port of compile-tests.el compile--test-error-line: parse STR and
    !> check the message at POS: its column (and END_COL), line (and
    !> END_LINE), file, TYPE (-1 for any) and RULE ("" for none).
    subroutine error_line(error, rule, str, pos, has_col, col, has_end_col, end_col, has_line, line, has_end_line, &
                          end_line, has_file, file, type)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: rule, str, file
        integer(int64), intent(in) :: pos, col, end_col, line, end_line
        logical, intent(in) :: has_col, has_end_col, has_line, has_end_line, has_file
        integer, intent(in) :: type
        type(compilation_message_info_type) :: info
        type(status_type) :: st
        logical :: found, last_has
        integer(int64) :: last_line
        character(len=:), allocatable :: what
        what = "testcase "//rule//": "//str
        call erase_buffer(st)
        call compilation_forget_errors()
        call insert(str, st)
        call compilation_parse_errors(point_min(), point_max(), st)
        call check(error, st%ok(), what//": "//format_error(st))
        if (allocated(error)) return
        call compilation_message_info(pos, info, found)
        call check(error, found, what//": no message")
        if (allocated(error)) return
        call check(error, info%has_loc, what//": no location")
        if (allocated(error)) return
        call check(error, info%has_column .eqv. has_col, what//": column")
        if (.not. allocated(error) .and. has_col) call check(error, info%column, col, what//": column")
        if (.not. allocated(error)) call check(error, info%has_line .eqv. has_line, what//": line")
        if (.not. allocated(error) .and. has_line) call check(error, info%line, line, what//": line")
        if (.not. allocated(error) .and. has_file) call check(error, info%file, file, what//": file")
        if (allocated(error)) return
        if (has_end_col) then
            ! The computed END-COL is exclusive; subtract one to get the
            ! number in the error message.
            call check(error, info%has_end .and. info%has_end_column, what//": end column")
            if (.not. allocated(error)) call check(error, info%end_column - 1, end_col, what//": end column")
            if (allocated(error)) return
        end if
        if (info%has_end) then
            last_has = info%has_end_line
            last_line = info%end_line
        else
            last_has = info%has_line
            last_line = info%line
        end if
        if (has_end_line) then
            call check(error, last_has .and. last_line == end_line, what//": end line")
        else
            call check(error, last_has .eqv. has_line, what//": end line")
            if (.not. allocated(error) .and. has_line) call check(error, last_line, line, what//": end line")
        end if
        if (allocated(error)) return
        if (type >= 0) call check(error, info%type, type, what//": type")
        if (allocated(error)) return
        if (len(rule) == 0) then
            call check(error,.not. allocated(info%rule), what//": rule")
        else
            call check(error, allocated(info%rule), what//": rule")
            if (.not. allocated(error)) call check(error, info%rule, rule, what//": rule")
        end if
    end subroutine error_line

    !> Port of compile-tests.el compile-test-error-regexps: the rules of
    !> `compilation_error_regexp_alist_alist' on the test data of
    !> compile-tests--test-regexps-data.
    subroutine test_error_regexps(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: s, f
        character(len=64), allocatable :: saved(:)
        character(len=*), parameter :: NL = achar(10), TB = achar(9)
        integer :: k
        call init_bindings()
        call parse_buffer(h)
        compilation_num_errors_found = 0
        compilation_num_warnings_found = 0
        compilation_num_infos_found = 0
        call all_but_omake()
        if (allocated(error)) return
        call omake_case()
        if (allocated(error)) return
        call check(error, compilation_num_errors_found, 111_int64, "errors")
        if (.not. allocated(error)) call check(error, compilation_num_warnings_found, 37_int64, "warnings")
        if (.not. allocated(error)) call check(error, compilation_num_infos_found, 36_int64, "infos")
        call kill_buffer(h, st)
    contains

        !> Test all built-in rules except `omake' to avoid interference.
        subroutine all_but_omake()
            s = "Error on line 3 of t.f: Execution error unclassifiable state"// &
                "ment"
            f = "t.f"
            call error_line(error, "absoft", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 3_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = 'Line 45 of "foo.c": bloofle undefined'
            f = "foo.c"
            call error_line(error, "absoft", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 45_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "error on line 19 of fplot.f: spelling error?"
            f = "fplot.f"
            call error_line(error, "absoft", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 19_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "warning on line 17 of fplot.f: data type is undefined for va"// &
                "riable d"
            f = "fplot.f"
            call error_line(error, "absoft", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 17_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "foo.adb:61:11:  [...] in call to size declared at foo.ads:11"
            f = "foo.adb"
            call error_line(error, "gnu", s, 1_int64, .true., 11_int64, .false., 0_int64, &
                            .true., 61_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "foo.adb:61:11:  [...] in call to size declared at foo.ads:11"
            f = "foo.ads"
            call error_line(error, "ada", s, 52_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 11_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "     0x8008621 main+16 at error.c:17"
            f = "error.c"
            call error_line(error, "ada", s, 23_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 17_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "****** Error number 140 in line 8 of file errors.c ******"
            f = "errors.c"
            call error_line(error, "aix", s, 25_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "[javac] /src/DataBaseTestCase.java:27: unreported exception "// &
                "..."
            f = "/src/DataBaseTestCase.java"
            call error_line(error, "ant", s, 13_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 27_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "[javac] /src/DataBaseTestCase.java:49: warning: finally clau"// &
                "se cannot complete normally"
            f = "/src/DataBaseTestCase.java"
            call error_line(error, "ant", s, 13_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 49_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "[jikes]  foo.java:3:5:7:9: blah blah"
            f = "foo.java"
            call error_line(error, "ant", s, 14_int64, .true., 5_int64, .true., 9_int64, &
                            .true., 3_int64, .true., 7_int64, .true., f, 2)
            if (allocated(error)) return
            s = "[javac] c:/cygwin/Test.java:12: error: foo: bar"
            f = "c:/cygwin/Test.java"
            call error_line(error, "ant", s, 9_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 12_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "[javac] c:\cygwin\Test.java:87: error: foo: bar"
            f = "c:\cygwin\Test.java"
            call error_line(error, "ant", s, 9_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 87_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "[checkstyle] [ERROR] /src/Test.java:38: warning: foo"
            f = "/src/Test.java"
            call error_line(error, "ant", s, 22_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 38_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "a.sh: line 1: ls-l: command not found"
            f = "a.sh"
            call error_line(error, "bash", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 1_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "Error ping.c 15: Unable to open include file 'sys/types.h'"
            f = "ping.c"
            call error_line(error, "borland", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 15_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "Warning pong.c 68: Call to function 'func' with no prototype"
            f = "pong.c"
            call error_line(error, "borland", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 68_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "Error E2010 ping.c 15: Unable to open include file 'sys/type"// &
                "s.h'"
            f = "ping.c"
            call error_line(error, "borland", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 15_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "Warning W1022 pong.c 68: Call to function 'func' with no pro"// &
                "totype"
            f = "pong.c"
            call error_line(error, "borland", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 68_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = 'File "foobar.ml", lines 5-8, characters 20-155: blah blah'
            f = "foobar.ml"
            call error_line(error, "python_tracebacks_and_caml", s, 1_int64, .true., 20_int64, .true., 155_int64, &
                            .true., 5_int64, .true., 8_int64, .true., f, 2)
            if (allocated(error)) return
            s = 'File "F:\ocaml\sorting.ml", line 65, characters 2-145:'//NL//"Warni"// &
                "ng 26: unused variable equ."
            f = "F:\ocaml\sorting.ml"
            call error_line(error, "python_tracebacks_and_caml", s, 1_int64, .true., 2_int64, .true., 145_int64, &
                            .true., 65_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = 'File "/usr/share/gdesklets/display/TargetGauge.py", line 41,'// &
                " in add_children"
            f = "/usr/share/gdesklets/display/TargetGauge.py"
            call error_line(error, "python_tracebacks_and_caml", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 41_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "File \lib\python\Products\PythonScripts\PythonScript.py, lin"// &
                "e 302, in _exec"
            f = "\lib\python\Products\PythonScripts\PythonScript.py"
            call error_line(error, "python_tracebacks_and_caml", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 302_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = 'File "/tmp/foo.py", line 10'
            f = "/tmp/foo.py"
            call error_line(error, "python_tracebacks_and_caml", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 10_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "In file included from foo.cpp:2:"
            f = "foo.cpp"
            call error_line(error, "clang_include", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 2_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "CMake Error at CMakeLists.txt:23 (hurz):"
            f = "CMakeLists.txt"
            call error_line(error, "cmake", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 23_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "CMake Warning at cmake/modules/UseUG.cmake:73 (find_package)"// &
                ":"
            f = "cmake/modules/UseUG.cmake"
            call error_line(error, "cmake", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 73_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "  cmake/modules/DuneGridMacros.cmake:19 (include)"
            f = "cmake/modules/DuneGridMacros.cmake"
            call error_line(error, "cmake_info", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 19_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = '"foo.f", line 3: Error: syntax error near end of statement'
            f = "foo.f"
            call error_line(error, "comma", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 3_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = '"vvouch.c", line 19.5: 1506-046 (S) Syntax error.'
            f = "vvouch.c"
            call error_line(error, "comma", s, 1_int64, .true., 5_int64, .false., 0_int64, &
                            .true., 19_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = '"foo.c", line 32 pos 1; (E) syntax error; unexpected symbol:'// &
                ' "lossage"'
            f = "foo.c"
            call error_line(error, "comma", s, 1_int64, .true., 1_int64, .false., 0_int64, &
                            .true., 32_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = '"foo.adb", line 2(11): warning: file name does not match ...'
            f = "foo.adb"
            call error_line(error, "comma", s, 1_int64, .true., 11_int64, .false., 0_int64, &
                            .true., 2_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = '"src/swapping.c", line 30.34: 1506-342 (W) "/*" detected in '// &
                "comment."
            f = "src/swapping.c"
            call error_line(error, "comma", s, 1_int64, .true., 34_int64, .false., 0_int64, &
                            .true., 30_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "Scenario: undefined step  # features/cucumber.feature:3"
            f = "features/cucumber.feature"
            call error_line(error, "cucumber", s, 29_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 3_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "      /home/gusev/.rvm/foo/bar.rb:500:in `_wrap_assertion'"
            f = "/home/gusev/.rvm/foo/bar.rb"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 500_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "build/intel/debug/../../../struct.cpp(42): error: identifier"// &
                ' "foo" is undefined'
            f = "build/intel/debug/../../../struct.cpp"
            call error_line(error, "edg_1", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 42_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "build/intel/debug/struct.cpp(44): warning #1011: missing ret"// &
                "urn statement at end of"
            f = "build/intel/debug/struct.cpp"
            call error_line(error, "edg_1", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 44_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "build/intel/debug/iptr.h(302): remark #981: operands are eva"// &
                "luated in unspecified order"
            f = "build/intel/debug/iptr.h"
            call error_line(error, "edg_1", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 302_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = '   detected during ... at line 62 of "build/intel/debug/../.'// &
                './../trace.h"'
            f = "build/intel/debug/../../../trace.h"
            call error_line(error, "edg_2", s, 31_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 62_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "Error 24 at (2:progran.f90) : syntax error"
            f = "progran.f90"
            call error_line(error, "epc", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 2_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "    Dummy arg W in module SUBA line 8 file arrayclash.f is a"// &
                "rray"
            f = "arrayclash.f"
            call error_line(error, "ftnchek", s, 32_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "    L4 used at line 55 file test/assign.f; never set"
            f = "test/assign.f"
            call error_line(error, "ftnchek", s, 16_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 55_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "Warning near line 10 file arrayclash.f: Module contains no e"// &
                "xecutable"
            f = "arrayclash.f"
            call error_line(error, "ftnchek", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 10_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "Nonportable usage near line 31 col 9 file assign.f: mixed de"// &
                "fault and explicit"
            f = "assign.f"
            call error_line(error, "ftnchek", s, 24_int64, .true., 9_int64, .false., 0_int64, &
                            .true., 31_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = '"foo.c",3  Error[32]: Error message'
            f = "foo.c"
            call error_line(error, "iar", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 3_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = '"foo.c",3  Warning[32]: Error message'
            f = "foo.c"
            call error_line(error, "iar", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 3_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "foo.c(2:0) : informational EDC0804: Function foo is not refe"// &
                "renced."
            f = "foo.c"
            call error_line(error, "ibm", s, 1_int64, .true., 0_int64, .false., 0_int64, &
                            .true., 2_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "foo.c(3:8) : warning EDC0833: Implicit return statement enco"// &
                "untered."
            f = "foo.c"
            call error_line(error, "ibm", s, 1_int64, .true., 8_int64, .false., 0_int64, &
                            .true., 3_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "foo.c(5:5) : error EDC0350: Syntax error."
            f = "foo.c"
            call error_line(error, "ibm", s, 1_int64, .true., 5_int64, .false., 0_int64, &
                            .true., 5_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "ccom: Error: foo.c, line 2: syntax error"
            f = "foo.c"
            call error_line(error, "irix", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 2_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "cc: Severe: /src/Python-2.3.3/Modules/_curses_panel.c, line "// &
                "17: Cannot find file <panel.h> ..."
            f = "/src/Python-2.3.3/Modules/_curses_panel.c"
            call error_line(error, "irix", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 17_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "cc: Info: foo.c, line 27: ..."
            f = "foo.c"
            call error_line(error, "irix", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 27_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "cfe: Warning 712: foo.c, line 2: illegal combination of poin"// &
                "ter and ..."
            f = "foo.c"
            call error_line(error, "irix", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 2_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "cfe: Warning 600: xfe.c: 170: Not in a conditional directive"// &
                " while ..."
            f = "xfe.c"
            call error_line(error, "irix", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 170_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "/usr/lib/cmplrs/cc/cfe: Error: foo.c: 1: blah blah"
            f = "foo.c"
            call error_line(error, "irix", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 1_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "/usr/lib/cmplrs/cc/cfe: warning: foo.c: 1: blah blah"
            f = "foo.c"
            call error_line(error, "irix", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 1_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "foo bar: baz.f, line 27: ..."
            f = "baz.f"
            call error_line(error, "irix", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 27_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = TB//"at org.foo.ComponentGateway.doGet(ComponentGateway.java:172"// &
                ")"
            f = "ComponentGateway.java"
            call error_line(error, "java", s, 5_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 172_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = TB//"at javax.servlet.http.HttpServlet.service(HttpServlet.java:"// &
                "740)"
            f = "HttpServlet.java"
            call error_line(error, "java", s, 5_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 740_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "==1332==    at 0x4040743C: System::getErrorString() (../src/"// &
                "Lib/System.cpp:217)"
            f = "../src/Lib/System.cpp"
            call error_line(error, "java", s, 13_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 217_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "==1332==    by 0x8008621: main (vtest.c:180)"
            f = "vtest.c"
            call error_line(error, "java", s, 13_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 180_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "/src/Test.java:5: ';' expected"//NL//"        foo foo"//NL//"             "// &
                "  ^"//NL
            f = "/src/Test.java"
            call error_line(error, "javac", s, 1_int64, .true., 16_int64, .false., 0_int64, &
                            .true., 5_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "e:\src\Test.java:7: warning: ';' expected"//NL//"   foo foo"//NL//"       "// &
                "   ^"//NL
            f = "e:\src\Test.java"
            call error_line(error, "javac", s, 1_int64, .true., 11_int64, .false., 0_int64, &
                            .true., 7_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = 'Found 2 semantic errors compiling "../javax/swing/BorderFact'// &
                'ory.java":'
            f = "../javax/swing/BorderFactory.java"
            call error_line(error, "jikes_file", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .false., 0_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = 'Issued 1 semantic warning compiling "java/awt/Toolkit.java":'
            f = "java/awt/Toolkit.java"
            call error_line(error, "jikes_file", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .false., 0_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "In file included from /usr/include/c++/3.3/backward/warn.h:4"// &
                ","
            f = "/usr/include/c++/3.3/backward/warn.h"
            call error_line(error, "gcc_include", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 4_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "                 from /usr/include/c++/3.3/backward/iostream"// &
                ".h:31:0,"
            f = "/usr/include/c++/3.3/backward/iostream.h"
            call error_line(error, "gcc_include", s, 1_int64, .true., 0_int64, .false., 0_int64, &
                            .true., 31_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "                 from test_clt.cc:1:"
            f = "test_clt.cc"
            call error_line(error, "gcc_include", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 1_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "lua: database.lua:10: assertion failed!"//NL//"stack traceback:"//NL//TB
            f = "database.lua"
            call error_line(error, "lua", s, 6_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 10_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "lua 5.4: database 2.lua:10: assertion failed!"//NL//"stack tracebac"// &
                "k:"//NL//TB
            f = "database 2.lua"
            call error_line(error, "lua", s, 10_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 10_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "/usr/local/bin/lua: core/database.lua:20: assertion failed!"//NL// &
                "stack traceback:"//NL//TB
            f = "core/database.lua"
            call error_line(error, "lua", s, 21_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 20_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "C:\Lua\Lua.exe: Core\Database.lua:20: assertion failed!"//NL//"stac"// &
                "k traceback:"//NL//TB
            f = "Core\Database.lua"
            call error_line(error, "lua", s, 17_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 20_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "lua: /tmp/database.lua:20: assertion failed!"//NL//"stack traceback"// &
                ":"//NL//TB
            f = "/tmp/database.lua"
            call error_line(error, "lua", s, 6_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 20_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "Lua.exe: C:\Temp\Database.lua:20: assertion failed!"//NL//"stack tr"// &
                "aceback:"//NL//TB
            f = "C:\Temp\Database.lua"
            call error_line(error, "lua", s, 10_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 20_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = TB//"database.lua: in field 'statement'"
            f = "database.lua"
            call error_line(error, "lua_stack", s, 2_int64, .false., 0_int64, .false., 0_int64, &
                            .false., 0_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = TB//"database.lua:10: in field 'statement'"
            f = "database.lua"
            call error_line(error, "lua_stack", s, 2_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 10_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = TB//"core/database.lua:20: in field 'statement'"
            f = "core/database.lua"
            call error_line(error, "lua_stack", s, 2_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 20_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = TB//"database 2.lua: in field 'statement'"
            f = "database 2.lua"
            call error_line(error, "lua_stack", s, 2_int64, .false., 0_int64, .false., 0_int64, &
                            .false., 0_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = TB//"Core\Database.lua:20: in field 'statement'"
            f = "Core\Database.lua"
            call error_line(error, "lua_stack", s, 2_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 20_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = TB//"/tmp/database.lua: in field 'statement'"
            f = "/tmp/database.lua"
            call error_line(error, "lua_stack", s, 2_int64, .false., 0_int64, .false., 0_int64, &
                            .false., 0_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = TB//"C:\Core\Database.lua: in field 'statement'"
            f = "C:\Core\Database.lua"
            call error_line(error, "lua_stack", s, 2_int64, .false., 0_int64, .false., 0_int64, &
                            .false., 0_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "make: *** [Makefile:20: all] Error 2"
            f = "Makefile"
            call error_line(error, "gmake", s, 12_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 20_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "make[4]: *** [sub/make.mk:19: all] Error 127"
            f = "sub/make.mk"
            call error_line(error, "gmake", s, 15_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 19_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "gmake[4]: *** [sub/make.mk:19: all] Error 2"
            f = "sub/make.mk"
            call error_line(error, "gmake", s, 16_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 19_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "gmake-4.3[4]: *** [make.mk:1119: all] Error 2"
            f = "make.mk"
            call error_line(error, "gmake", s, 20_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 1119_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "Make-4.3: *** [make.INC:1119: dir/all] Error 2"
            f = "make.INC"
            call error_line(error, "gmake", s, 16_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 1119_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "foo.c:8: message"
            f = "foo.c"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "../foo.c:8: W: message"
            f = "../foo.c"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "/tmp/foo.c:8:warning message"
            f = "/tmp/foo.c"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "foo/bar.py:8: FutureWarning message"
            f = "foo/bar.py"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "foo.py:8: RuntimeWarning message"
            f = "foo.py"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "foo.c:8:I: message"
            f = "foo.c"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "foo.c:8.23: note: message"
            f = "foo.c"
            call error_line(error, "gnu", s, 1_int64, .true., 23_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "foo.c:8.23: info: message"
            f = "foo.c"
            call error_line(error, "gnu", s, 1_int64, .true., 23_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "foo.c:8:23:information: message"
            f = "foo.c"
            call error_line(error, "gnu", s, 1_int64, .true., 23_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "foo.c:8.23-45: Informational: message"
            f = "foo.c"
            call error_line(error, "gnu", s, 1_int64, .true., 23_int64, .true., 45_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "foo.c:8-23: message"
            f = "foo.c"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .true., 23_int64, .true., f, 2)
            if (allocated(error)) return
            s = "   |foo.c:8: message"
            f = "foo.c"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "foo.c:8-45.3: message"
            f = "foo.c"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .true., 3_int64, &
                            .true., 8_int64, .true., 45_int64, .true., f, 2)
            if (allocated(error)) return
            s = "foo.c:8.23-9.1: message"
            f = "foo.c"
            call error_line(error, "gnu", s, 1_int64, .true., 23_int64, .true., 1_int64, &
                            .true., 8_int64, .true., 9_int64, .true., f, 2)
            if (allocated(error)) return
            s = "jade:dbcommon.dsl:133:17:E: missing argument for function ca"// &
                "ll"
            f = "dbcommon.dsl"
            call error_line(error, "gnu", s, 1_int64, .true., 17_int64, .false., 0_int64, &
                            .true., 133_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "G:/cygwin/dev/build-myproj.xml:54: Compiler Adapter 'javac' "// &
                "can't be found."
            f = "G:/cygwin/dev/build-myproj.xml"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 54_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "file:G:/cygwin/dev/build-myproj.xml:54: Compiler Adapter 'ja"// &
                "vac' can't be found."
            f = "G:/cygwin/dev/build-myproj.xml"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 54_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "{standard input}:27041: Warning: end of file not at end of a"// &
                " line; newline inserted"
            f = "{standard input}"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 27041_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "boost/container/detail/flat_tree.hpp:589:25:   [ skipping 5 "// &
                "instantiation contexts, use -ftemplate-backtrace-limit=0 to "// &
                "disable ]"
            f = "boost/container/detail/flat_tree.hpp"
            call error_line(error, "gnu", s, 1_int64, .true., 25_int64, .false., 0_int64, &
                            .true., 589_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "  src/add.cp"// &
                "p:8:31: erro"// &
                "r: invalid o"// &
                "perands of t"// &
                "ypes "//achar(226)//achar(128)//achar(152)//"const "// &
                "double"//achar(226)//achar(128)//achar(153)//" and "// &
                achar(226)//achar(128)//achar(152)//"const doubl"// &
                "e*"//achar(226)//achar(128)//achar(153)//" to binar"// &
                "y "//achar(226)//achar(128)//achar(152)//"operator+"// &
                achar(226)//achar(128)//achar(153)
            f = "src/add.cpp"
            call error_line(error, "gnu", s, 1_int64, .true., 31_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "e: file:///src/Test.kt:267:5 foo: bar"
            f = "/src/Test.kt"
            call error_line(error, "gradle_kotlin", s, 4_int64, .true., 5_int64, .false., 0_int64, &
                            .true., 267_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "w: file:///src/Test.kt:267:5 foo: bar"
            f = "/src/Test.kt"
            call error_line(error, "gradle_kotlin", s, 4_int64, .true., 5_int64, .false., 0_int64, &
                            .true., 267_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "e: /src/Test.kt: (34, 15): foo: bar"
            f = "/src/Test.kt"
            call error_line(error, "gradle_kotlin_legacy", s, 4_int64, .true., 15_int64, .false., 0_int64, &
                            .true., 34_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "w: /src/Test.kt: (11, 98): foo: bar"
            f = "/src/Test.kt"
            call error_line(error, "gradle_kotlin_legacy", s, 4_int64, .true., 98_int64, .false., 0_int64, &
                            .true., 11_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "e: e:/cygwin/src/Test.kt: (34, 15): foo: bar"
            f = "e:/cygwin/src/Test.kt"
            call error_line(error, "gradle_kotlin_legacy", s, 4_int64, .true., 15_int64, .false., 0_int64, &
                            .true., 34_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "w: e:/cygwin/src/Test.kt: (11, 98): foo: bar"
            f = "e:/cygwin/src/Test.kt"
            call error_line(error, "gradle_kotlin_legacy", s, 4_int64, .true., 98_int64, .false., 0_int64, &
                            .true., 11_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "e: e:\src\Test.kt: (34, 15): foo: bar"
            f = "e:\src\Test.kt"
            call error_line(error, "gradle_kotlin_legacy", s, 4_int64, .true., 15_int64, .false., 0_int64, &
                            .true., 34_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "w: e:\src\Test.kt: (11, 98): foo: bar"
            f = "e:\src\Test.kt"
            call error_line(error, "gradle_kotlin_legacy", s, 4_int64, .true., 98_int64, .false., 0_int64, &
                            .true., 11_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "     ERROR:/Users/salutis/src/AndroidSchemeExperiment/app/bu"// &
                "ild/intermediates/incremental/debug/mergeDebugResources/stri"// &
                "pped.dir/layout/item.xml:3: AAPT: error: '16dpw' is incompat"// &
                "ible with attribute padding (attr) dimension."
            f = "/Users/salutis/src/AndroidSchemeExperiment/app/build/interme"// &
                "diates/incremental/debug/mergeDebugResources/stripped.dir/la"// &
                "yout/item.xml"
            call error_line(error, "gradle_android", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 3_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "In foo.scm:"//NL
            f = "foo.scm"
            call error_line(error, "guile_file", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .false., 0_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "  63:4 [call-with-prompt prompt0 ...]"
            f = ""
            call error_line(error, "guile_line", s, 1_int64, .true., 4_int64, .false., 0_int64, &
                            .true., 63_int64, .false., 0_int64, .false., f, -1)
            if (allocated(error)) return
            s = '1038: 1 [main ("gud-break.scm")]'
            f = ""
            call error_line(error, "guile_line", s, 1_int64, .true., 1_int64, .false., 0_int64, &
                            .true., 1038_int64, .false., 0_int64, .false., f, -1)
            if (allocated(error)) return
            s = "E, file.cc(35,52) Illegal operation on pointers"
            f = "file.cc"
            call error_line(error, "lcc", s, 1_int64, .true., 52_int64, .false., 0_int64, &
                            .true., 35_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "W, file.cc(36,52) blah blah"
            f = "file.cc"
            call error_line(error, "lcc", s, 1_int64, .true., 52_int64, .false., 0_int64, &
                            .true., 36_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "makepp: Scanning `/foo/bar.c'"
            f = "/foo/bar.c"
            call error_line(error, "makepp", s, 19_int64, .false., 0_int64, .false., 0_int64, &
                            .false., 0_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "makepp: warning: bla bla `/foo/bar.c' and `/foo/bar.h'"
            f = "/foo/bar.c"
            call error_line(error, "makepp", s, 27_int64, .false., 0_int64, .false., 0_int64, &
                            .false., 0_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "makepp: bla bla `/foo/Makeppfile:12' bla"
            f = "/foo/Makeppfile"
            call error_line(error, "makepp", s, 18_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 12_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "makepp: bla bla `/foo/bar.c' and `/foo/bar.h'"
            f = "/foo/bar.h"
            call error_line(error, "", s, 35_int64, .false., 0_int64, .false., 0_int64, &
                            .false., 0_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "FooBar.java:[111,53] no interface expected here"
            f = "FooBar.java"
            call error_line(error, "maven", s, 1_int64, .true., 53_int64, .false., 0_int64, &
                            .true., 111_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "[ERROR] /Users/cinsk/hello.java:[651,96] ';' expected"
            f = "/Users/cinsk/hello.java"
            call error_line(error, "maven", s, 15_int64, .true., 96_int64, .false., 0_int64, &
                            .true., 651_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "[WARNING] /foo/bar/Test.java:[27,43] unchecked conversion"
            f = "/foo/bar/Test.java"
            call error_line(error, "maven", s, 11_int64, .true., 43_int64, .false., 0_int64, &
                            .true., 27_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "TrimMask (255) in solomon.c may be indistinguishable from Tr"// &
                "imMasks (93) in solomo.c due to truncation"
            f = "solomon.c"
            call error_line(error, "mips_1", s, 11_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 255_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "TrimMask (255) in solomon.c may be indistinguishable from Tr"// &
                "imMasks (93) in solomo.c due to truncation"
            f = "solomo.c"
            call error_line(error, "mips_1", s, 70_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 93_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "name defined but never used: LinInt in cmap_calc.c(199)"
            f = "cmap_calc.c"
            call error_line(error, "mips_2", s, 40_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 199_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "keyboard handler.c(537) : warning C4005: 'min' : macro redef"// &
                "inition"
            f = "keyboard handler.c"
            call error_line(error, "msft", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 537_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "d:\tmp\test.c(23) : error C2143: syntax error : missing ';' "// &
                "before 'if'"
            f = "d:\tmp\test.c"
            call error_line(error, "msft", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 23_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "d:\tmp\test.c(1145) : see declaration of 'nsRefPtr'"
            f = "d:\tmp\test.c"
            call error_line(error, "msft", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 1145_int64, .false., 0_int64, .true., f, -1)
            if (allocated(error)) return
            s = "1>test_main.cpp(29): error C2144: syntax error : 'int' shoul"// &
                "d be preceded by ';'"
            f = "test_main.cpp"
            call error_line(error, "msft", s, 3_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 29_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "1>test_main.cpp(29): error C4430: missing type specifier - i"// &
                "nt assumed. Note: C++ does not support default-int"
            f = "test_main.cpp"
            call error_line(error, "msft", s, 3_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 29_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "C:\tmp\test.cpp(101,11): error C4101: 'bias0123': unreferenc"// &
                "ed local variable [C:\tmp\project.vcxproj]"
            f = "C:\tmp\test.cpp"
            call error_line(error, "msft", s, 1_int64, .true., 11_int64, .false., 0_int64, &
                            .true., 101_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "..\src\ctrl\lister.c(109): Error! E1009: Expecting ';' but f"// &
                "ound '{'"
            f = "..\src\ctrl\lister.c"
            call error_line(error, "watcom", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 109_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "..\src\ctrl\lister.c(120): Warning! W201: Unreachable code"
            f = "..\src\ctrl\lister.c"
            call error_line(error, "watcom", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 120_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "Semantic error at line 528, column 5, file erosacqdb.pc:"
            f = "erosacqdb.pc"
            call error_line(error, "oracle", s, 1_int64, .true., 5_int64, .false., 0_int64, &
                            .true., 528_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "Error at line 41, column 10 in file /usr/src/sb/ODBI_BHP.hpp"
            f = "/usr/src/sb/ODBI_BHP.hpp"
            call error_line(error, "oracle", s, 1_int64, .true., 10_int64, .false., 0_int64, &
                            .true., 41_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "PCC-02150: error at line 49, column 27 in file /usr/src/sb/O"// &
                "DBI_dxfgh.pc"
            f = "/usr/src/sb/ODBI_dxfgh.pc"
            call error_line(error, "oracle", s, 1_int64, .true., 27_int64, .false., 0_int64, &
                            .true., 49_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "PCC-00003: invalid SQL Identifier at column name in line 12 "// &
                "of file /usr/src/sb/ODBI_BHP.hpp"
            f = "/usr/src/sb/ODBI_BHP.hpp"
            call error_line(error, "oracle", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 12_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "PCC-00004: mismatched IF/ELSE/ENDIF block at line 27 in file"// &
                " /usr/src/sb/ODBI_BHP.hpp"
            f = "/usr/src/sb/ODBI_BHP.hpp"
            call error_line(error, "oracle", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 27_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "PCC-02151: line 21 column 40 file /usr/src/sb/ODBI_BHP.hpp:"
            f = "/usr/src/sb/ODBI_BHP.hpp"
            call error_line(error, "oracle", s, 1_int64, .true., 40_int64, .false., 0_int64, &
                            .true., 21_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = 'syntax error at automake line 922, near "'':''"'
            f = "automake"
            call error_line(error, "perl", s, 14_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 922_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "Died at test.pl line 27."
            f = "test.pl"
            call error_line(error, "perl", s, 6_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 27_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "store::odrecall('File_A', 'x2') called at store.pm line 90"
            f = "store.pm"
            call error_line(error, "perl", s, 40_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 90_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = TB//"(in cleanup) something bad at foo.pl line 3 during global d"// &
                "estruction."
            f = "foo.pl"
            call error_line(error, "perl", s, 29_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 3_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "GLib-GObject-WARNING **: /build/buildd/glib2.0-2.14.5/gobjec"// &
                "t/gsignal.c:1741: instance `0x8206790' has no handler with i"// &
                "d `1234' at t-compilation-perl-gtk.pl line 3."
            f = "t-compilation-perl-gtk.pl"
            call error_line(error, "perl", s, 130_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 3_int64, .false., 0_int64, .true., f, -1)
            if (allocated(error)) return
            s = "Parse error: parse error, unexpected $ in main.php on line 5"// &
                "9"
            f = "main.php"
            call error_line(error, "php", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 59_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "Fatal error: Call to undefined function: mysql_pconnect() in"// &
                " db.inc on line 66"
            f = "db.inc"
            call error_line(error, "php", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 66_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "error[E0277]: `Foo` is not an iterator"//NL//" --> src/main.rs:4:16"
            f = "src/main.rs"
            call error_line(error, "rust", s, 1_int64, .true., 16_int64, .false., 0_int64, &
                            .true., 4_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "warning: borrow of packed field is unsafe and requires unsaf"// &
                "e function or block (error E0133)"//NL//"  --> lint_example.rs:11:1"// &
                "3"
            f = "lint_example.rs"
            call error_line(error, "rust", s, 1_int64, .true., 13_int64, .false., 0_int64, &
                            .true., 11_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "note: required by a bound in `Trait`"//NL//"  --> src/auxiliary/tra"// &
                "it-debuginfo.rs:23:18"
            f = "src/auxiliary/trait-debuginfo.rs"
            call error_line(error, "rust", s, 1_int64, .true., 18_int64, .false., 0_int64, &
                            .true., 23_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "thread 'main' (1234) panicked at library/std/src/io/stdio.rs"// &
                ":30:40:"
            f = "library/std/src/io/stdio.rs"
            call error_line(error, "rust_panic", s, 1_int64, .true., 40_int64, .false., 0_int64, &
                            .true., 30_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "    thread 'main' (1234) panicked at library/std/src/io/stdi"// &
                "o.rs:50:60:"
            f = "library/std/src/io/stdio.rs"
            call error_line(error, "rust_panic", s, 1_int64, .true., 60_int64, .false., 0_int64, &
                            .true., 50_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "plain-exception.rb:7:in `fun': unhandled exception"
            f = "plain-exception.rb"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 7_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = TB//"from plain-exception.rb:3:in `proxy'"
            f = "plain-exception.rb"
            call error_line(error, "gcc_include", s, 2_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 3_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = TB//"from plain-exception.rb:12"
            f = "plain-exception.rb"
            call error_line(error, "gcc_include", s, 2_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 12_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "    [examples/test-unit.rb:28:in `here_is_a_deep_assert'"
            f = "examples/test-unit.rb"
            call error_line(error, "ruby_Test::Unit", s, 5_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 28_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "     examples/test-unit.rb:19:in `test_a_deep_assert']:"
            f = "examples/test-unit.rb"
            call error_line(error, "ruby_Test::Unit", s, 6_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 19_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "examples/test-unit.rb:10:in `test_assert_raise'"
            f = "examples/test-unit.rb"
            call error_line(error, "gnu", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 10_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "Error: Mismatched end tag: expected </geroup>, got </group>"//NL// &
                "in unnamed entity at line 71 char 8 of file:///home/reto/tes"// &
                "t/group.xml"
            f = "/home/reto/test/group.xml"
            call error_line(error, "rxp", s, 1_int64, .true., 8_int64, .false., 0_int64, &
                            .true., 71_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "Warning: Start tag for undeclared element geroup"//NL//"in unnamed "// &
                "entity at line 4 char 8 of file:///home/reto/test/group.xml"
            f = "/home/reto/test/group.xml"
            call error_line(error, "rxp", s, 1_int64, .true., 8_int64, .false., 0_int64, &
                            .true., 4_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "In autogen.sh line 48:"
            f = "autogen.sh"
            call error_line(error, "shellcheck", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 48_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "Thu May 14 10:46:12 1992  mom3.p:"
            f = "mom3.p"
            call error_line(error, "sparc_pascal_file", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .false., 0_int64, .false., 0_int64, .true., f, -1)
            if (allocated(error)) return
            s = "cc-1020 CC: REMARK File = CUI_App.h, Line = 735"
            f = "CUI_App.h"
            call error_line(error, "sun", s, 13_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 735_int64, .false., 0_int64, .true., f, 0)
            if (allocated(error)) return
            s = "cc-1070 cc: WARNING File = linkl.c, Line = 38"
            f = "linkl.c"
            call error_line(error, "sun", s, 13_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 38_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "cf90-113 f90comp: ERROR NSE, File = Hoved.f90, Line = 16, Co"// &
                "lumn = 3"
            f = "Hoved.f90"
            call error_line(error, "sun", s, 18_int64, .true., 3_int64, .false., 0_int64, &
                            .true., 16_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "/home3/xdhar/rcds_rc/main.a, line 361, char 6:syntax error: "// &
                '"," inserted'
            f = "/home3/xdhar/rcds_rc/main.a"
            call error_line(error, "sun_ada", s, 1_int64, .true., 6_int64, .false., 0_int64, &
                            .true., 361_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "/home/foo/greeter.ts(30,12): error TS2339: Property 'foo' do"// &
                "es not exist."
            f = "/home/foo/greeter.ts"
            call error_line(error, "typescript_tsc_plain", s, 1_int64, .true., 12_int64, .false., 0_int64, &
                            .true., 30_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "src/resources/document.ts:140:22 - error TS2362: something."
            f = "src/resources/document.ts"
            call error_line(error, "typescript_tsc_pretty", s, 1_int64, .true., 22_int64, .false., 0_int64, &
                            .true., 140_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "/usr/src/foo/foo.c(8): warning: w may be used before set"
            f = "/usr/src/foo/foo.c"
            call error_line(error, "edg_1", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 1)
            if (allocated(error)) return
            s = "/usr/src/foo/foo.c(9): error: w is used before set"
            f = "/usr/src/foo/foo.c"
            call error_line(error, "edg_1", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 9_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "strcmp: variable # of args. llib-lc(359)  ::  /usr/src/foo/f"// &
                "oo.c(8)"
            f = "/usr/src/foo/foo.c"
            call error_line(error, "4bsd", s, 44_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 8_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "bloofle defined( /users/wolfgang/foo.c(4) ), but never used"
            f = "/users/wolfgang/foo.c"
            call error_line(error, "4bsd", s, 18_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 4_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "# Failed test 1 in foo.t at line 6"
            f = "foo.t"
            call error_line(error, "perl__Test", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 6_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = 'NOK 1# Test 1 got: "1234" (t/foo.t at line 46)'
            f = "t/foo.t"
            call error_line(error, "perl__Test2", s, 1_int64, .false., 0_int64, .false., 0_int64, &
                            .true., 46_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
            s = "index.html (13:1) Unknown element <fdjsk>"
            f = "index.html"
            call error_line(error, "weblint", s, 1_int64, .true., 1_int64, .false., 0_int64, &
                            .true., 13_int64, .false., 0_int64, .true., f, 2)
            if (allocated(error)) return
        end subroutine all_but_omake

        !> Test the `omake' rule separately.  This doesn't actually test
        !> the `omake' rule itself but its indirect effects.
        subroutine omake_case()
            saved = compilation_error_regexp_alist
            deallocate (compilation_error_regexp_alist)
            allocate (compilation_error_regexp_alist(size(compilation_error_regexp_alist_alist)))
            do k = 1, size(compilation_error_regexp_alist_alist)
                compilation_error_regexp_alist(k) = compilation_error_regexp_alist_alist(k)%name
            end do
            s = "      alpha.c:5:15: error: expected ';' after expression"
            f = "alpha.c"
            call error_line(error, "gnu", s, 1_int64, .true., 15_int64, .false., 0_int64, .true., 5_int64, .false., &
                            0_int64, .true., f, -1)
            compilation_error_regexp_alist = saved
        end subroutine omake_case
    end subroutine test_error_regexps

    subroutine my_file(filename, directory, found)
        character(len=:), allocatable, intent(out) :: filename, directory
        logical, intent(out) :: found
        filename = "my-file"
        found = .true.
        if (allocated(directory)) deallocate (directory)
    end subroutine my_file

    subroutine line_start(number, found)
        integer(int64), intent(out) :: number
        logical, intent(out) :: found
        number = 123
        found = .true.
    end subroutine line_start

    subroutine line_end(number, found)
        integer(int64), intent(out) :: number
        logical, intent(out) :: found
        number = 134
        found = .true.
    end subroutine line_end

    subroutine col_start(number, found)
        integer(int64), intent(out) :: number
        logical, intent(out) :: found
        number = 39
        found = .true.
    end subroutine col_start

    subroutine col_end(number, found)
        integer(int64), intent(out) :: number
        logical, intent(out) :: found
        number = 24
        found = .true.
    end subroutine col_end

    !> Port of compile-tests.el compile-test-functions: rules using
    !> functions instead of regexp group numbers.
    subroutine test_functions(error)
        type(error_type), allocatable, intent(out) :: error
        type(compilation_rule_type), allocatable :: saved_rules(:)
        character(len=64), allocatable :: saved_names(:)
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call parse_buffer(h)
        saved_rules = compilation_error_regexp_alist_alist
        saved_names = compilation_error_regexp_alist
        deallocate (compilation_error_regexp_alist_alist, compilation_error_regexp_alist)
        allocate (compilation_error_regexp_alist_alist(1), compilation_error_regexp_alist(1))
        compilation_error_regexp_alist_alist(1)%name = "my_rule"
        compilation_error_regexp_alist_alist(1)%regexp = "^My error message"
        compilation_error_regexp_alist_alist(1)%file_function => my_file
        compilation_error_regexp_alist_alist(1)%line_function => line_start
        compilation_error_regexp_alist_alist(1)%end_line_function => line_end
        compilation_error_regexp_alist_alist(1)%column_function => col_start
        compilation_error_regexp_alist_alist(1)%end_column_function => col_end
        compilation_error_regexp_alist(1) = "my_rule"
        compilation_num_errors_found = 0
        compilation_num_warnings_found = 0
        compilation_num_infos_found = 0
        call error_line(error, "my_rule", "My error message", 1_int64, .true., 39_int64, .true., 24_int64, .true., &
                        123_int64, .true., 134_int64, .true., "my-file", 2)
        if (.not. allocated(error)) call check(error, compilation_num_errors_found, 1_int64)
        if (.not. allocated(error)) call check(error, compilation_num_warnings_found, 0_int64)
        if (.not. allocated(error)) call check(error, compilation_num_infos_found, 0_int64)
        compilation_error_regexp_alist_alist = saved_rules
        compilation_error_regexp_alist = saved_names
        call kill_buffer(h, st)
    end subroutine test_functions

    !> "Entering directory" messages give the directory of the file names
    !> after them, until "Leaving directory".
    subroutine test_directory_tracking(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(compilation_message_info_type) :: info
        logical :: found
        character(len=*), parameter :: NL = achar(10)
        integer(int64) :: second
        call init_bindings()
        call parse_buffer(h)
        call compilation_forget_errors()
        call insert("make: Entering directory '/fortmacs-no-such-dir/sub'"//NL//"foo.c:1: e"//NL, st)
        second = point()
        call insert("make: Leaving directory '/fortmacs-no-such-dir/sub'"//NL//"bar.c:2: e"//NL, st)
        call compilation_ensure_parse(point_max())
        call compilation_message_info(second - 10, info, found)
        call check(error, found, "foo.c message")
        if (.not. allocated(error)) call check(error, info%file, "foo.c")
        if (.not. allocated(error)) call check(error, allocated(info%directory), "foo.c directory")
        if (.not. allocated(error)) call check(error, info%directory, "/fortmacs-no-such-dir/sub")
        if (allocated(error)) return
        call compilation_message_info(point_max() - 10, info, found)
        call check(error, found, "bar.c message")
        if (.not. allocated(error)) call check(error, info%file, "bar.c")
        if (.not. allocated(error)) call check(error,.not. allocated(info%directory), "bar.c directory")
        call kill_buffer(h, st)
    end subroutine test_directory_tracking

    !> A compilation with an error and a warning: the counts and the mode
    !> line, the motion commands, `next_error' and `compile_goto_error'.
    subroutine test_next_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf
        type(status_type) :: st
        character(len=:), allocatable :: dir, path
        integer(int64) :: errors, warnings, infos
        integer :: unit
        call init_bindings()
        dir = file_name_as_directory(temporary_file_directory())
        path = dir//"fortmacs-compile-test.c"
        open (newunit=unit, file=path, status="replace", action="write")
        write (unit, "(a)") "line one"
        write (unit, "(a)") "line two"
        write (unit, "(a)") "    line three"
        close (unit)
        call source_buffer(h, dir)
        call compile("printf 'fortmacs-compile-test.c:2: error: e\nfortmacs-compile-test.c:3:6: warning: w\n'", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        buf = get_buffer("*compilation*")
        call set_buffer(buf, st)
        call compilation_message_counts(errors, warnings, infos)
        call check(error, errors, 1_int64, "errors")
        if (.not. allocated(error)) call check(error, warnings, 1_int64, "warnings")
        if (.not. allocated(error)) call check(error, infos, 0_int64, "infos")
        if (.not. allocated(error)) call check(error, mode_line_process_text(cur), ":exit [0] [1 1 0]")
        if (allocated(error)) return
        ! M-n from the start: the first message, on line 5.
        call goto_char(point_min())
        call compilation_next_error(1_int64, st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, current_line_start_text(), "fortmacs-compile-test.c:2")
        if (allocated(error)) return
        ! next-error visits the first error...
        call next_error(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffers(cur)%name, "fortmacs-compile-test.c")
        if (.not. allocated(error)) call check(error, point(), 10_int64, "line 2")
        if (allocated(error)) return
        ! ...then the warning, at its column (the first is column 1).
        call next_error(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, point(), 24_int64, "line 3, column 6")
        if (allocated(error)) return
        ! No more.
        call next_error(st)
        call check(error,.not. st%ok(), "past last error")
        if (.not. allocated(error)) call check(error, format_error(st), "Past last error")
        if (allocated(error)) return
        ! RET on the first message.
        call set_buffer(buf, st)
        call goto_char(point_min())
        call compilation_next_error(1_int64, st)
        call compile_goto_error(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffers(cur)%name, "fortmacs-compile-test.c")
        if (.not. allocated(error)) call check(error, point(), 10_int64, "line 2 again")
        call kill_buffer(get_buffer("fortmacs-compile-test.c"), st)
        call set_buffer_read_only(buf, .false., st)
        call kill_buffer(buf, st)
        call kill_buffer(h, st)
        open (newunit=unit, file=path, status="old")
        close (unit, status="delete")
    end subroutine test_next_error

    !> The text from the start of the current line to the end of the
    !> first number on it.
    function current_line_start_text() result(text)
        use editfns_fm, only: line_beginning_position, line_end_position, buffer_substring
        character(len=:), allocatable :: text
        type(status_type) :: st
        integer :: k
        call buffer_substring(line_beginning_position(), line_end_position(), text, st)
        k = index(text, ":")
        if (k == 0) return
        k = k + verify(text(k + 1:), "0123456789") - 1
        text = text(1:k)
    end function current_line_start_text

    !> C-u M-x compile: Comint mode with Compilation Shell minor mode.
    subroutine test_comint_compile(error)
        use modes_fm, only: minor_modes
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf
        type(status_type) :: st
        character(len=:), allocatable :: text
        call init_bindings()
        call source_buffer(h, file_name_as_directory(temporary_file_directory()))
        call compile("echo interactive", st, comint=.true.)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_compilation()
        buf = get_buffer("*compilation*")
        call set_buffer(buf, st)
        call check(error, buffers(cur)%major_mode, "comint_mode")
        if (.not. allocated(error)) call check(error, minor_modes%compilation_shell_minor_mode%enabled(), "minor mode")
        if (allocated(error)) return
        text = compilation_text()
        call check(error, index(line(text, 2), "Comint started at ") == 1, line(text, 2))
        if (.not. allocated(error)) call check(error, line(text, 4), "echo interactive")
        if (.not. allocated(error)) call check(error, line(text, 5), "interactive")
        if (.not. allocated(error)) call check(error, index(text, "Comint finished at ") > 0, text)
        call kill_buffer(buf, st)
        call kill_buffer(h, st)
    end subroutine test_comint_compile
end module test_compile_fm
