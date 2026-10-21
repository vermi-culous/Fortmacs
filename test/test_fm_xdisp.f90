! test_fm_xdisp.f90 -- tests for fm_xdisp, fm_window and fm_emacs: how
! buffer text is laid out on the screen, and the editor's main loop on a
! pty.  Own tests: upstream's xdisp-tests.el needs Lisp and GUI frames.
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

module test_fm_xdisp
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use fm_error, only: status_type
    use fm_handle, only: handle_type
    use fm_buffer, only: get_buffer_create, set_buffer, kill_buffer, resolve_buffer, &
                         erase_buffer
    use fm_insdel, only: insert
    use fm_editfns, only: goto_char, buffer_string
    use fm_dispextern, only: glyph_row_type, glyph_type
    use fm_terminfo, only: terminfo_type, builtin_xterm
    use fm_term, only: init_tty_entry
    use fm_termchar, only: tty_take_output
    use fm_frame, only: frame_type
    use fm_window, only: free_window
    use fm_xdisp, only: layout_window, redisplay, mode_line_text, glyph_width
    use fm_keyboard, only: last_message
    use fm_sysdep, only: open_pty, close_fd, set_tty_size, init_sys_modes, &
                         reset_sys_modes, emacs_write, emacs_read, input_available, &
                         emacs_tty_type
    use fm_emacs, only: emacs_main
    implicit none(type, external)
    private

    public :: collect_fm_xdisp

contains

    subroutine collect_fm_xdisp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("layout-lines-and-cursor", test_lines), &
                    new_unittest("layout-tab-control-wide", test_special_chars), &
                    new_unittest("layout-continuation", test_continuation), &
                    new_unittest("layout-end-of-buffer", test_end_of_buffer), &
                    new_unittest("mode-line-text", test_mode_line), &
                    new_unittest("redisplay-recenters", test_redisplay), &
                    new_unittest("emacs-main-on-a-pty", test_emacs_main) &
                    ]
    end subroutine collect_fm_xdisp

    !> A buffer holding `text`, current; its slot is `bi`.
    subroutine buffer_with(name, text, h, bi)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        integer(int64), intent(out) :: bi
        type(status_type) :: st

        call get_buffer_create(name, h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
        call resolve_buffer(h, bi, st)
    end subroutine buffer_with

    !> The text of a laid-out row (wide-character padding left out).
    function row_text(row) result(s)
        type(glyph_row_type), intent(in) :: row
        character(len=:), allocatable :: s
        integer :: k

        s = ""
        do k = 1, row%used
            if (row%glyphs(k)%padding) cycle
            if (row%glyphs(k)%ch < 128) then
                s = s//achar(row%glyphs(k)%ch)
            else
                s = s//"?"
            end if
        end do
    end function row_text

    subroutine layout(bi, start, pt, width, nrows, rows, crow, ccol, end_pos, at_end)
        integer(int64), intent(in) :: bi, start, pt
        integer, intent(in) :: width, nrows
        type(glyph_row_type), allocatable, intent(out) :: rows(:)
        integer, intent(out) :: crow, ccol
        integer(int64), intent(out) :: end_pos
        logical, intent(out) :: at_end
        integer :: r

        allocate (rows(nrows))
        do r = 1, nrows
            allocate (rows(r)%glyphs(width))
        end do
        call layout_window(bi, start, pt, width, rows, crow, ccol, end_pos, at_end)
    end subroutine layout

    subroutine test_lines(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(glyph_row_type), allocatable :: rows(:)
        integer(int64) :: bi, end_pos
        integer :: crow, ccol
        logical :: at_end
        type(status_type) :: st

        call buffer_with("*xdisp-lines*", "ab"//achar(10)//"cd", h, bi)
        call layout(bi, 1_int64, 4_int64, 10, 3, rows, crow, ccol, end_pos, at_end)
        call check(error, row_text(rows(1)) == "ab" .and. row_text(rows(2)) == "cd" .and. &
                   row_text(rows(3)) == "", "rows")
        if (allocated(error)) return
        call check(error, crow == 1 .and. ccol == 0, "cursor on c")
        if (allocated(error)) return
        call check(error, at_end .and. end_pos == 6, "end")
        call kill_buffer(h, st)
    end subroutine test_lines

    subroutine test_special_chars(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(glyph_row_type), allocatable :: rows(:)
        integer(int64) :: bi, end_pos
        integer :: crow, ccol
        logical :: at_end
        type(status_type) :: st

        ! a TAB b, C-a, DEL, then a wide character and x.
        call buffer_with("*xdisp-chars*", "a"//achar(9)//"b"//achar(10)//achar(1)//achar(127)// &
                         achar(10)//char(230)//char(151)//char(165)//"x", h, bi)
        call layout(bi, 1_int64, 9_int64, 20, 3, rows, crow, ccol, end_pos, at_end)
        call check(error, row_text(rows(1)), "a       b")
        if (allocated(error)) return
        call check(error, row_text(rows(2)), "^A^?")
        if (allocated(error)) return
        ! The wide character takes two cells; x is in column 2.
        call check(error, rows(3)%used == 3 .and. rows(3)%glyphs(2)%padding .and. &
                   rows(3)%glyphs(3)%ch == iachar("x"), "wide")
        if (allocated(error)) return
        call check(error, crow == 2 .and. ccol == 2, "cursor on x")
        if (allocated(error)) return
        call check(error, glyph_width(26085_int32) == 2 .and. glyph_width(97_int32) == 1, "widths")
        call kill_buffer(h, st)
    end subroutine test_special_chars

    subroutine test_continuation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(glyph_row_type), allocatable :: rows(:)
        integer(int64) :: bi, end_pos
        integer :: crow, ccol
        logical :: at_end
        type(status_type) :: st

        call buffer_with("*xdisp-cont*", "abcdefgh", h, bi)
        call layout(bi, 1_int64, 7_int64, 6, 3, rows, crow, ccol, end_pos, at_end)
        ! Five characters and \ in the last column, then the rest.
        call check(error, row_text(rows(1)), "abcde\")
        if (allocated(error)) return
        call check(error, row_text(rows(2)), "fgh")
        if (allocated(error)) return
        call check(error, crow == 1 .and. ccol == 1, "cursor on g")
        call kill_buffer(h, st)
    end subroutine test_continuation

    !> After a final newline there is one more (empty) line for the cursor.
    subroutine test_end_of_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(glyph_row_type), allocatable :: rows(:)
        integer(int64) :: bi, end_pos
        integer :: crow, ccol
        logical :: at_end
        type(status_type) :: st

        call buffer_with("*xdisp-eob*", "ab"//achar(10), h, bi)
        call layout(bi, 1_int64, 4_int64, 10, 4, rows, crow, ccol, end_pos, at_end)
        call check(error, crow == 1 .and. ccol == 0 .and. at_end, "cursor after the newline")
        if (allocated(error)) return
        ! One row shows "ab" and its newline, which reaches the end of the
        ! text (as window-end = point-max upstream: the mode line says All).
        call layout(bi, 1_int64, 1_int64, 10, 1, rows, crow, ccol, end_pos, at_end)
        call check(error, at_end .and. end_pos == 4, "first line only")
        if (allocated(error)) return
        ! Text that does not fit.
        call buffer_with("*xdisp-eob*", "ab"//achar(10)//"cd", h, bi)
        call layout(bi, 1_int64, 1_int64, 10, 1, rows, crow, ccol, end_pos, at_end)
        call check(error,.not. at_end .and. end_pos == 4, "more text")
        call kill_buffer(h, st)
    end subroutine test_end_of_buffer

    subroutine test_mode_line(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: s

        s = mode_line_text("*scratch*", .false., .false., "All", 1_int64, 60)
        call check(error, s, "-UUU:----F1  *scratch*      All   L1     (Fundamental) -----")
        if (allocated(error)) return
        s = mode_line_text("notes", .true., .false., "Top", 12_int64, 20)
        call check(error, s, "-UUU:**--F1  notes  ")
        if (allocated(error)) return
        s = mode_line_text("x", .true., .true., "Bot", 3_int64, 9)
        call check(error, s, "-UUU:%*--")
    end subroutine test_mode_line

    !> Point far below the window start: the window recenters on it.
    subroutine test_redisplay(error)
        type(error_type), allocatable, intent(out) :: error
        type(frame_type) :: f
        type(terminfo_type) :: ti
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: text, out
        character(len=12) :: line
        integer(int64) :: bi
        integer :: k

        text = ""
        do k = 1, 100
            write (line, '("line ",i0)') k
            text = text//trim(line)//achar(10)
        end do
        call buffer_with("*xdisp-big*", text, h, bi)
        call builtin_xterm(ti)
        call init_tty_entry(f%tty, ti, "xterm-256color", -1_int32, st)
        ! Point at the start of line 60.
        call goto_char(line_start(60))
        call redisplay(f, st)
        out = tty_take_output(f%tty)
        ! 22 text lines: point's line goes to row (22 - 1) / 2 = 10.
        call check(error, f%cursor_vpos == 10 .and. f%cursor_hpos == 0, "cursor row")
        if (allocated(error)) return
        call check(error, row_text(f%current_matrix%rows(10)), "line 60")
        if (allocated(error)) return
        call check(error, index(row_text(f%current_matrix%rows(22)), "*xdisp-big*   ") > 0 .and. &
                   index(row_text(f%current_matrix%rows(22)), "L60") > 0, "mode line")
        if (allocated(error)) return
        ! A message shows in the echo area.
        last_message = "Hello"
        call redisplay(f, st)
        call check(error, row_text(f%current_matrix%rows(23)), "Hello")
        last_message = ""
        call free_window(f%root_window)
        call kill_buffer(h, st)
    contains
        !> Position of the start of line `n` ("line 1" .. "line 9" take 7
        !> characters with the newline, "line 10" .. 8).
        integer(int64) function line_start(n)
            integer, intent(in) :: n

            if (n <= 10) then
                line_start = 1 + 7*(n - 1)
            else
                line_start = 1 + 7*9 + 8*(n - 10)
            end if
        end function line_start
    end subroutine test_redisplay

    !> The whole editor on a pty: type, then C-x C-c.
    subroutine test_emacs_main(error)
        type(error_type), allocatable, intent(out) :: error
        type(emacs_tty_type) :: old
        type(status_type) :: st
        integer(int32) :: master, slave
        character(len=4096) :: buf
        character(len=:), allocatable :: out
        integer(int64) :: n

        call open_pty(master, slave, st)
        call check(error, st%ok(), "open_pty")
        if (allocated(error)) return
        call set_tty_size(slave, 40_int32, 8_int32, st)
        ! Raw mode first, so C-c is not taken as an interrupt.
        call init_sys_modes(slave, old, st)
        call emacs_write(master, "hi"//achar(24)//achar(3), st)
        call emacs_main("xterm-256color", slave, st)
        call check(error, st%ok(), "emacs_main")
        if (.not. allocated(error)) call check(error, buffer_string(), "hi")
        out = ""
        do while (input_available(master, 100_int32))
            call emacs_read(master, buf, n, st)
            if (n <= 0) exit
            out = out//buf(1:n)
        end do
        if (.not. allocated(error)) then
            call check(error, index(out, achar(27)//"[?1049h") > 0 .and. &
                       index(out, achar(27)//"[?1049l") > 0, "enters and leaves full screen")
        end if
        call reset_sys_modes(slave, old, st)
        call close_fd(slave)
        call close_fd(master)
    end subroutine test_emacs_main

end module test_fm_xdisp
