! test_fm_sysdep.f90 -- tests for src/sys (fm_sys_linux, fm_sysdep).
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

!> Terminal tests run on a pseudo-terminal, so they need no real tty and
!> work in CI.  As tests of the binding layer itself, they may use
!> ISO_C_BINDING and fm_sys_linux directly.
module test_fm_sysdep
   use, intrinsic :: iso_fortran_env, only: int32, int64
   use, intrinsic :: iso_c_binding, only: c_sizeof, c_int
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use fm_error, only: status_type, FM_FILE_ERROR
   use fm_sys_linux
   use fm_sysdep
   implicit none(type, external)
   private

   public :: collect_fm_sysdep

contains

   subroutine collect_fm_sysdep(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("struct-layouts", test_layouts), &
                  new_unittest("raw-settings", test_raw_settings), &
                  new_unittest("pty-raw-roundtrip", test_pty_roundtrip), &
                  new_unittest("pty-size", test_pty_size), &
                  new_unittest("input-available-timeout", test_poll), &
                  new_unittest("window-change-signal", test_winch), &
                  new_unittest("read-bad-fd", test_bad_fd) &
                  ]
   end subroutine collect_fm_sysdep

   !> Sizes must match glibc's on both x86-64 and aarch64.
   subroutine test_layouts(error)
      type(error_type), allocatable, intent(out) :: error
      type(termios_type) :: t
      type(winsize_type) :: w
      type(pollfd_type) :: p

      call check(error, int(c_sizeof(t)), 60)
      if (allocated(error)) return
      call check(error, int(c_sizeof(w)), 8)
      if (allocated(error)) return
      call check(error, int(c_sizeof(p)), 8)
   end subroutine test_layouts

   subroutine test_raw_settings(error)
      type(error_type), allocatable, intent(out) :: error
      type(emacs_tty_type) :: old, raw

      old%main%c_iflag = ior(ior(ICRNL, INLCR), ior(ISTRIP, IXON))
      old%main%c_oflag = ior(ior(OPOST, ONLCR), TAB3)
      old%main%c_lflag = ior(ior(ECHO, ICANON), IEXTEN)
      old%main%c_cflag = PARENB
      old%main%c_cc(VSUSP) = 26

      raw = tty_raw_settings(old, .false., .true., .true.)
      associate (m => raw%main)
         call check(error, iand(m%c_iflag, ior(ior(ICRNL, INLCR), ior(ISTRIP, IXON))), 0_c_int)
         if (allocated(error)) return
         call check(error, iand(m%c_iflag, IGNBRK), IGNBRK)
         if (allocated(error)) return
         call check(error, iand(m%c_lflag, ior(ior(ECHO, ICANON), IEXTEN)), 0_c_int)
         if (allocated(error)) return
         call check(error, iand(m%c_lflag, ISIG), ISIG)
         if (allocated(error)) return
         call check(error, iand(m%c_oflag, ior(ONLCR, TAB3)), 0_c_int)
         if (allocated(error)) return
         call check(error, iand(m%c_oflag, OPOST), OPOST)   ! untouched
         if (allocated(error)) return
         call check(error, iand(m%c_cflag, CS8), CS8)
         if (allocated(error)) return
         call check(error, iand(m%c_cflag, PARENB), 0_c_int)
         if (allocated(error)) return
         call check(error, int(m%c_cc(VINTR)), 7)
         if (allocated(error)) return
         call check(error, int(m%c_cc(VMIN)) == 1 .and. int(m%c_cc(VTIME)) == 0, "VMIN/VTIME")
         if (allocated(error)) return
         call check(error, int(m%c_cc(VSUSP)), 0)
      end associate

      ! Not the controlling terminal: C-g must arrive as input.
      raw = tty_raw_settings(old, .true., .false., .false.)
      call check(error, int(raw%main%c_cc(VINTR)), 0)
      if (allocated(error)) return
      call check(error, iand(raw%main%c_iflag, IXON), IXON)
      if (allocated(error)) return
      call check(error, int(raw%main%c_cc(VSTOP)), 19)
   end subroutine test_raw_settings

   subroutine test_pty_roundtrip(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: master, slave
      type(emacs_tty_type) :: old, now
      type(status_type) :: st
      character(len=16) :: buf
      integer(int64) :: n

      call open_pty(master, slave, st)
      call check(error, st%ok(), "open_pty failed")
      if (allocated(error)) return

      run: block

         call init_sys_modes(slave, old, st)
         call check(error, st%ok(), "init_sys_modes failed")
         if (allocated(error)) exit run
         call emacs_get_tty(slave, now, st)
         call check(error, iand(now%main%c_lflag, ior(ECHO, ICANON)), 0_c_int)
         if (allocated(error)) exit run

         ! Raw input: bytes arrive without a newline, C-a included.
         call emacs_write(master, "ab"//char(1), st)
         call check(error, input_available(slave, 2000), "no input on slave")
         if (allocated(error)) exit run
         call emacs_read(slave, buf, n, st)
         call check(error, buf(1:n) == "ab"//char(1), "raw bytes garbled: "//buf(1:n))
         if (allocated(error)) exit run

         call reset_sys_modes(slave, old, st)
         call emacs_get_tty(slave, now, st)
         call check(error, iand(now%main%c_lflag, ICANON), ICANON)
      end block run
      call close_fd(slave)
      call close_fd(master)
   end subroutine test_pty_roundtrip

   subroutine test_pty_size(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: master, slave, w, h
      type(status_type) :: st

      call open_pty(master, slave, st)
      call check(error, st%ok(), "open_pty failed")
      if (allocated(error)) return

      run: block
         call set_tty_size(master, 132_int32, 43_int32, st)
         call get_tty_size(slave, w, h, st)
         call check(error, st%ok(), "get_tty_size failed")
         if (allocated(error)) exit run
         call check(error, w, 132_int32)
         if (allocated(error)) exit run
         call check(error, h, 43_int32)
      end block run
      call close_fd(slave)
      call close_fd(master)
   end subroutine test_pty_size

   subroutine test_poll(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: master, slave
      type(status_type) :: st

      call open_pty(master, slave, st)
      call check(error, st%ok(), "open_pty failed")
      if (allocated(error)) return

      run: block
         call check(error,.not. input_available(slave, 0_int32), "phantom input")
         if (allocated(error)) exit run
         call check(error,.not. input_available(slave, 50_int32), "phantom input after wait")
      end block run
      call close_fd(slave)
      call close_fd(master)
   end subroutine test_poll

   subroutine test_winch(error)
      type(error_type), allocatable, intent(out) :: error
      integer(c_int) :: r

      call install_window_change_handler()
      call check(error,.not. window_change_pending(), "flag set before any signal")
      if (allocated(error)) return
      r = c_raise(SIGWINCH)
      call check(error, r == 0 .and. window_change_pending(), "SIGWINCH not seen")
      if (allocated(error)) return
      call check(error,.not. window_change_pending(), "flag not cleared")
   end subroutine test_winch

   subroutine test_bad_fd(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=4) :: buf
      integer(int64) :: n
      type(status_type) :: st

      call emacs_read(-1_int32, buf, n, st)
      call check(error, st%code, FM_FILE_ERROR)
      if (allocated(error)) return
      call check(error, index(st%detail, "read failed") == 1, "detail: "//st%detail)
   end subroutine test_bad_fd

end module test_fm_sysdep
