! fm_sys_linux.f90 -- raw Linux/glibc bindings: types, constants, calls.
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

!> Bindings to the Linux C library, for x86-64 and aarch64 only.
!>
!> Not a port; this is the floor the src/sys ports stand on.  Layouts and
!> constants are copied from glibc and the kernel's asm-generic headers,
!> which x86-64 and aarch64 share for everything here:
!>
!> - struct termios: glibc bits/termios-struct.h (NCCS = 32, 60 bytes)
!> - termios flags and c_cc indices: asm-generic/termbits.h
!> - TIOCGWINSZ / TIOCSWINSZ: asm-generic/ioctls.h
!> - O_* flags: asm-generic/fcntl.h;  signals: asm-generic/signal.h
!> - errno values: asm-generic/errno-base.h
!>
!> Variadic functions (`open`, `ioctl`) are bound with fixed arguments.
!> That is safe on both targets: aarch64 Linux (standard AAPCS64, unlike
!> Apple's) passes variadic arguments like fixed ones, and on x86-64 the
!> only difference is %al, which glibc's prologue merely tests to decide
!> whether to spill vector registers.  `open` is never given O_CREAT here,
!> so it never reads a mode argument.
module fm_sys_linux
   use, intrinsic :: iso_c_binding, only: c_int, c_signed_char, &
                                                                             c_short, c_long, c_size_t, c_ptr, c_char, &
                                                                             c_funptr, c_null_char, c_f_pointer, c_associated
   implicit none(type, external)
   private

   !> bits/termios-struct.h: NCCS.
   integer, parameter, public :: NCCS = 32

   !> struct termios (tcflag_t and speed_t are unsigned int; cc_t is
   !> unsigned char, held in a signed byte: only the bits matter).
   type, bind(C), public :: termios_type
      integer(c_int) :: c_iflag = 0
      integer(c_int) :: c_oflag = 0
      integer(c_int) :: c_cflag = 0
      integer(c_int) :: c_lflag = 0
      integer(c_signed_char) :: c_line = 0_c_signed_char
      integer(c_signed_char) :: c_cc(NCCS) = 0_c_signed_char
      integer(c_int) :: c_ispeed = 0
      integer(c_int) :: c_ospeed = 0
   end type termios_type

   !> struct winsize (asm-generic/termios.h).
   type, bind(C), public :: winsize_type
      integer(c_short) :: ws_row = 0
      integer(c_short) :: ws_col = 0
      integer(c_short) :: ws_xpixel = 0
      integer(c_short) :: ws_ypixel = 0
   end type winsize_type

   !> struct pollfd (asm-generic/poll.h).
   type, bind(C), public :: pollfd_type
      integer(c_int) :: fd = -1
      integer(c_short) :: events = 0
      integer(c_short) :: revents = 0
   end type pollfd_type

   ! c_iflag (octal in the headers)
   integer(c_int), parameter, public :: IGNBRK = int(o'0000001', c_int)
   integer(c_int), parameter, public :: BRKINT = int(o'0000002', c_int)
   integer(c_int), parameter, public :: ISTRIP = int(o'0000040', c_int)
   integer(c_int), parameter, public :: INLCR = int(o'0000100', c_int)
   integer(c_int), parameter, public :: IGNCR = int(o'0000200', c_int)
   integer(c_int), parameter, public :: ICRNL = int(o'0000400', c_int)
   integer(c_int), parameter, public :: IXON = int(o'0002000', c_int)
   integer(c_int), parameter, public :: IXANY = int(o'0004000', c_int)
   integer(c_int), parameter, public :: IXOFF = int(o'0010000', c_int)
   integer(c_int), parameter, public :: IUTF8 = int(o'0040000', c_int)
   ! c_oflag
   integer(c_int), parameter, public :: OPOST = int(o'0000001', c_int)
   integer(c_int), parameter, public :: ONLCR = int(o'0000004', c_int)
   integer(c_int), parameter, public :: TABDLY = int(o'0014000', c_int)
   integer(c_int), parameter, public :: TAB3 = int(o'0014000', c_int)
   ! c_cflag
   integer(c_int), parameter, public :: CSIZE = int(o'0000060', c_int)
   integer(c_int), parameter, public :: CS8 = int(o'0000060', c_int)
   integer(c_int), parameter, public :: PARENB = int(o'0000400', c_int)
   ! c_lflag
   integer(c_int), parameter, public :: ISIG = int(o'0000001', c_int)
   integer(c_int), parameter, public :: ICANON = int(o'0000002', c_int)
   integer(c_int), parameter, public :: ECHO = int(o'0000010', c_int)
   integer(c_int), parameter, public :: ECHONL = int(o'0000100', c_int)
   integer(c_int), parameter, public :: IEXTEN = int(o'0100000', c_int)
   ! c_cc indices, 1-based for Fortran (header value + 1)
   integer, parameter, public :: VINTR = 1, VQUIT = 2, VERASE = 3, VKILL = 4, &
                                 VEOF = 5, VTIME = 6, VMIN = 7, VSWTC = 8, &
                                 VSTART = 9, VSTOP = 10, VSUSP = 11, VEOL = 12, &
                                 VREPRINT = 13, VDISCARD = 14, VWERASE = 15, &
                                 VLNEXT = 16, VEOL2 = 17
   !> _POSIX_VDISABLE on Linux.
   integer(c_signed_char), parameter, public :: CDISABLE = 0_c_signed_char
   ! tcsetattr actions
   integer(c_int), parameter, public :: TCSANOW = 0, TCSADRAIN = 1, TCSAFLUSH = 2
   ! ioctl requests
   integer(c_long), parameter, public :: TIOCGWINSZ = int(z'5413', c_long)
   integer(c_long), parameter, public :: TIOCSWINSZ = int(z'5414', c_long)
   ! open flags
   integer(c_int), parameter, public :: O_RDONLY = 0, O_WRONLY = 1, O_RDWR = 2
   integer(c_int), parameter, public :: O_NOCTTY = int(o'0000400', c_int)
   integer(c_int), parameter, public :: O_NONBLOCK = int(o'0004000', c_int)
   ! poll events
   integer(c_short), parameter, public :: POLLIN = 1_c_short
   ! signals
   integer(c_int), parameter, public :: SIGINT = 2, SIGTERM = 15, SIGCONT = 18, &
                                        SIGTSTP = 20, SIGWINCH = 28
   ! errno
   integer(c_int), parameter, public :: EINTR = 4, EAGAIN = 11

   public :: c_tcgetattr, c_tcsetattr, c_read, c_write, c_close, c_isatty, &
             c_poll, c_ioctl_winsize, c_signal, c_raise, c_posix_openpt, &
             c_grantpt, c_unlockpt, c_ptsname, c_open
   public :: errno, c_string_to_fortran

   interface
      integer(c_int) function c_tcgetattr(fd, t) bind(C, name="tcgetattr")
         import :: c_int, termios_type
         integer(c_int), value :: fd
         type(termios_type), intent(out) :: t
      end function c_tcgetattr

      integer(c_int) function c_tcsetattr(fd, action, t) bind(C, name="tcsetattr")
         import :: c_int, termios_type
         integer(c_int), value :: fd, action
         type(termios_type), intent(in) :: t
      end function c_tcsetattr

      integer(c_long) function c_read(fd, buf, count) bind(C, name="read")
         import :: c_int, c_long, c_size_t, c_char
         integer(c_int), value :: fd
         character(kind=c_char), intent(out) :: buf(*)
         integer(c_size_t), value :: count
      end function c_read

      integer(c_long) function c_write(fd, buf, count) bind(C, name="write")
         import :: c_int, c_long, c_size_t, c_char
         integer(c_int), value :: fd
         character(kind=c_char), intent(in) :: buf(*)
         integer(c_size_t), value :: count
      end function c_write

      integer(c_int) function c_close(fd) bind(C, name="close")
         import :: c_int
         integer(c_int), value :: fd
      end function c_close

      integer(c_int) function c_isatty(fd) bind(C, name="isatty")
         import :: c_int
         integer(c_int), value :: fd
      end function c_isatty

      integer(c_int) function c_poll(fds, nfds, timeout) bind(C, name="poll")
         import :: c_int, c_long, pollfd_type
         type(pollfd_type), intent(inout) :: fds(*)
         integer(c_long), value :: nfds
         integer(c_int), value :: timeout
      end function c_poll

      !> ioctl(fd, request, struct winsize *): see the module note.
      integer(c_int) function c_ioctl_winsize(fd, request, ws) bind(C, name="ioctl")
         import :: c_int, c_long, winsize_type
         integer(c_int), value :: fd
         integer(c_long), value :: request
         type(winsize_type), intent(inout) :: ws
      end function c_ioctl_winsize

      !> signal(2) from glibc: BSD semantics (SA_RESTART, handler stays).
      type(c_funptr) function c_signal(signum, handler) bind(C, name="signal")
         import :: c_int, c_funptr
         integer(c_int), value :: signum
         type(c_funptr), value :: handler
      end function c_signal

      integer(c_int) function c_raise(signum) bind(C, name="raise")
         import :: c_int
         integer(c_int), value :: signum
      end function c_raise

      integer(c_int) function c_posix_openpt(flags) bind(C, name="posix_openpt")
         import :: c_int
         integer(c_int), value :: flags
      end function c_posix_openpt

      integer(c_int) function c_grantpt(fd) bind(C, name="grantpt")
         import :: c_int
         integer(c_int), value :: fd
      end function c_grantpt

      integer(c_int) function c_unlockpt(fd) bind(C, name="unlockpt")
         import :: c_int
         integer(c_int), value :: fd
      end function c_unlockpt

      type(c_ptr) function c_ptsname(fd) bind(C, name="ptsname")
         import :: c_int, c_ptr
         integer(c_int), value :: fd
      end function c_ptsname

      !> open(path, flags) without a mode: see the module note.
      integer(c_int) function c_open(path, flags) bind(C, name="open")
         import :: c_int, c_char
         character(kind=c_char), intent(in) :: path(*)
         integer(c_int), value :: flags
      end function c_open

      type(c_ptr) function c_errno_location() bind(C, name="__errno_location")
         import :: c_ptr
      end function c_errno_location
   end interface

contains

   !> Current value of errno.
   integer(c_int) function errno()
      integer(c_int), pointer :: p

      call c_f_pointer(c_errno_location(), p)
      errno = p
   end function errno

   !> Copy of the NUL-terminated C string at `p` ("" for a null pointer).
   function c_string_to_fortran(p) result(s)
      type(c_ptr), intent(in) :: p
      character(len=:), allocatable :: s
      character(kind=c_char), pointer :: chars(:)
      integer :: n

      if (.not. c_associated(p)) then
         s = ""
         return
      end if
      call c_f_pointer(p, chars, [huge(0)])
      n = 0
      do while (chars(n + 1) /= c_null_char)
         n = n + 1
      end do
      allocate (character(len=n) :: s)
      do n = 1, len(s)
         s(n:n) = chars(n)
      end do
   end function c_string_to_fortran

end module fm_sys_linux
