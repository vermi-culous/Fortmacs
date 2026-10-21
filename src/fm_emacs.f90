! fm_emacs.f90 -- starting and running the editor.
! Ported from GNU Emacs 31.1 src/emacs.c (main) with the terminal setup of
! lisp/startup.el and lisp/faces.el (tty-run-terminal-initialization).
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

!> `emacs_main` sets up the *scratch* buffer, the key bindings and the
!> terminal, then runs the command loop until C-x C-c, redisplaying
!> whenever it waits for input.
module fm_emacs
    use, intrinsic :: iso_fortran_env, only: int32
    use fm_error, only: status_type
    use fm_handle, only: handle_type
    use fm_buffer, only: get_buffer_create, set_buffer
    use fm_frame, only: frame_type
    use fm_window, only: free_window
    use fm_term, only: init_tty, delete_tty, tty_set_terminal_modes
    use fm_termchar, only: tty_flush
    use fm_dispnew, only: adjust_frame_glyphs
    use fm_xdisp, only: redisplay, init_frame_windows, realize_basic_faces
    use fm_keyboard, only: set_input_fd, command_loop, redisplay_hook
    use fm_bindings, only: init_bindings
    use fm_term_xterm, only: terminal_init_xterm
    implicit none(type, external)
    private

    public :: emacs_main

    !> The terminal frame (frame.c:selected_frame).
    type(frame_type), target :: selected_frame

contains

    !> keyboard.c's redisplay before waiting for input.
    subroutine do_redisplay()
        type(status_type) :: status

        call redisplay(selected_frame, status)
    end subroutine do_redisplay

    !> Whether terminal type `term` gets term/xterm.el's setup, as
    !> faces.el:tty-run-terminal-initialization finds term/xterm.el for
    !> "xterm*" (and term/screen.el and term/tmux.el, which use it).
    logical function xterm_like(term)
        character(len=*), intent(in) :: term

        xterm_like = index(term, "xterm") == 1 .or. index(term, "screen") == 1 .or. &
                     index(term, "tmux") == 1
    end function xterm_like

    !> Run the editor on terminal type `terminal_type` at file descriptor
    !> `fd` until the user leaves.  Port of the terminal path of
    !> emacs.c:main (init_display, then Frecursive_edit).
    subroutine emacs_main(terminal_type, fd, status)
        character(len=*), intent(in) :: terminal_type
        integer(int32), intent(in) :: fd
        type(status_type), intent(out) :: status
        type(handle_type) :: scratch
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create("*scratch*", scratch, status)
        if (.not. status%ok()) return
        call set_buffer(scratch, status)
        if (.not. status%ok()) return

        ! A fresh frame (emacs_main may run more than once in tests).
        call free_window(selected_frame%root_window)
        selected_frame%garbaged = .true.
        call init_tty(selected_frame%tty, terminal_type, fd, status)
        if (.not. status%ok()) return
        if (xterm_like(terminal_type)) call terminal_init_xterm()
        call set_input_fd(fd)
        call adjust_frame_glyphs(selected_frame)
        call init_frame_windows(selected_frame)
        call realize_basic_faces(selected_frame)
        call tty_set_terminal_modes(selected_frame%tty)

        redisplay_hook => do_redisplay
        call command_loop(st)
        redisplay_hook => null()

        call delete_tty(selected_frame%tty, status)
        call tty_flush(selected_frame%tty, st)
        call set_input_fd(-1_int32)
    end subroutine emacs_main

end module fm_emacs
