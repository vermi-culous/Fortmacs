! fm_sysdep.f90 -- terminal modes, tty size and raw I/O for Linux.
! Ported from GNU Emacs 31.1 src/sysdep.c (and the window-size signal
! flag of src/dispnew.c).
!
! Copyright (C) 1985-1988, 1993-1995, 1999-2026 Free Software
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

!> System-dependent terminal handling.  Only Linux is supported, so the
!> DOS_NT and old-BSD branches of sysdep.c are not ported.
!>
!> File descriptors are `int32` (the same kind as C int on both targets),
!> so callers need no ISO_C_BINDING.  Errors from the C library come back
!> as file-error statuses whose detail names the call and errno.
module fm_sysdep
   use, intrinsic :: iso_fortran_env, only: int32, int64
   use, intrinsic :: iso_c_binding, only: c_int, c_long, c_size_t, c_char, &
                                                                             c_signed_char, c_funloc, c_funptr
   use fm_error, only: status_type, set_error, FM_FILE_ERROR
   use fm_sys_linux
   implicit none(type, external)
   private

   !> Port of systty.h:struct emacs_tty.
   type, public :: emacs_tty_type
      type(termios_type) :: main
   end type emacs_tty_type

   !> The quit character, C-g (keyboard.c:quit_char).
   integer(c_signed_char), parameter, public :: QUIT_CHAR = 7_c_signed_char

   public :: emacs_get_tty, emacs_set_tty, tty_raw_settings, init_sys_modes, &
             reset_sys_modes, get_tty_size, set_tty_size, emacs_read, &
             emacs_full_write, emacs_write, input_available
   public :: install_window_change_handler, window_change_pending
   public :: open_pty, close_fd

   !> Set (to the signal number) asynchronously by the SIGWINCH handler;
   !> read and cleared by `window_change_pending`.
   integer(c_int), volatile :: winch_flag = 0

contains

   !> Fill `status` from errno after the C call `what` failed.
   subroutine sys_error(status, what)
      type(status_type), intent(inout) :: status
      character(len=*), intent(in) :: what
      character(len=12) :: num

      write (num, '(i0)') errno()
      call set_error(status, FM_FILE_ERROR, what//" failed, errno "//trim(num))
   end subroutine sys_error

   !> Read the terminal modes of `fd`.  Port of sysdep.c:emacs_get_tty.
   subroutine emacs_get_tty(fd, settings, status)
      integer(int32), intent(in) :: fd
      type(emacs_tty_type), intent(out) :: settings
      type(status_type), intent(out) :: status

      if (c_tcgetattr(fd, settings%main) /= 0) call sys_error(status, "tcgetattr")
   end subroutine emacs_get_tty

   !> Set the terminal modes of `fd`, discarding pending input if `flushp`.
   !> Reads the settings back and retries (up to 10 times) until they
   !> stick, as POSIX lets tcsetattr succeed partially.
   !> Port of sysdep.c:emacs_set_tty.
   subroutine emacs_set_tty(fd, settings, flushp, status)
      integer(int32), intent(in) :: fd
      type(emacs_tty_type), intent(in) :: settings
      logical, intent(in) :: flushp
      type(status_type), intent(out) :: status
      type(termios_type) :: now
      integer(c_int) :: action
      integer :: i

      action = TCSADRAIN
      if (flushp) action = TCSAFLUSH
      do i = 1, 10
         if (c_tcsetattr(fd, action, settings%main) < 0) then
            if (errno() == EINTR) cycle
            call sys_error(status, "tcsetattr")
            return
         end if
         if (c_tcgetattr(fd, now) /= 0) cycle
         if (now%c_iflag == settings%main%c_iflag .and. &
             now%c_oflag == settings%main%c_oflag .and. &
             now%c_cflag == settings%main%c_cflag .and. &
             now%c_lflag == settings%main%c_lflag .and. &
             all(now%c_cc == settings%main%c_cc)) return
      end do
   end subroutine emacs_set_tty

   !> The modes Emacs runs a terminal in, derived from `old`: no echo, no
   !> line editing, no CR/NL mapping, 8-bit input, reads of at least one
   !> byte, and special characters disabled except C-g for SIGINT/SIGQUIT
   !> on the controlling terminal.  The termios part of
   !> sysdep.c:init_sys_modes, separated so it can be tested.
   pure function tty_raw_settings(old, flow_control, meta_key, controlling) result(tty)
      type(emacs_tty_type), intent(in) :: old
      logical, intent(in) :: flow_control, meta_key, controlling
      type(emacs_tty_type) :: tty

      tty = old
      associate (m => tty%main)
         m%c_iflag = ior(m%c_iflag, IGNBRK)          ! Ignore break condition
         m%c_iflag = iand(m%c_iflag, not(ICRNL))     ! No CR -> NL on input
         m%c_iflag = iand(m%c_iflag, not(INLCR))     ! No NL -> CR on input
         m%c_iflag = iand(m%c_iflag, not(ISTRIP))    ! Keep the 8th bit
         m%c_lflag = iand(m%c_lflag, not(ECHO))      ! No echo
         m%c_lflag = iand(m%c_lflag, not(ICANON))    ! No erase/kill processing
         m%c_lflag = iand(m%c_lflag, not(IEXTEN))    ! No other editing chars
         m%c_lflag = ior(m%c_lflag, ISIG)            ! Enable signals
         if (flow_control) then
            m%c_iflag = ior(m%c_iflag, IXON)
            m%c_iflag = iand(m%c_iflag, not(IXANY))
         else
            m%c_iflag = iand(m%c_iflag, not(IXON))
         end if
         m%c_oflag = iand(m%c_oflag, not(ONLCR))     ! No NL -> CR-NL on output
         m%c_oflag = iand(m%c_oflag, not(TAB3))      ! No tab expansion
         if (meta_key) then
            m%c_cflag = ior(m%c_cflag, CS8)          ! 8-bit input
            m%c_cflag = iand(m%c_cflag, not(PARENB)) ! No parity
         end if
         if (controlling) then
            m%c_cc(VINTR) = QUIT_CHAR                ! C-g gives SIGINT
            m%c_cc(VQUIT) = QUIT_CHAR
         else
            m%c_cc(VINTR) = CDISABLE
            m%c_cc(VQUIT) = CDISABLE
         end if
         m%c_cc(VMIN) = 1_c_signed_char              ! Wait for at least 1 byte
         m%c_cc(VTIME) = 0_c_signed_char             ! however long that takes
         m%c_cc(VSUSP) = CDISABLE                    ! C-z is ordinary input
         m%c_cc(VLNEXT) = CDISABLE
         m%c_cc(VREPRINT) = CDISABLE
         m%c_cc(VWERASE) = CDISABLE
         m%c_cc(VDISCARD) = CDISABLE
         if (flow_control) then
            m%c_cc(VSTART) = 17_c_signed_char        ! C-q
            m%c_cc(VSTOP) = 19_c_signed_char         ! C-s
         else
            m%c_cc(VSTART) = CDISABLE
            m%c_cc(VSTOP) = CDISABLE
         end if
      end associate
   end function tty_raw_settings

   !> Save the modes of `fd` in `old` and switch it to Emacs's modes.
   !> Port of the termios part of sysdep.c:init_sys_modes.
   subroutine init_sys_modes(fd, old, status, flow_control, meta_key, controlling)
      integer(int32), intent(in) :: fd
      type(emacs_tty_type), intent(out) :: old
      type(status_type), intent(out) :: status
      logical, intent(in), optional :: flow_control, meta_key, controlling
      logical :: flow, meta, ctl

      flow = .false.
      meta = .true.
      ctl = .true.
      if (present(flow_control)) flow = flow_control
      if (present(meta_key)) meta = meta_key
      if (present(controlling)) ctl = controlling

      call emacs_get_tty(fd, old, status)
      if (.not. status%ok()) return
      call emacs_set_tty(fd, tty_raw_settings(old, flow, meta, ctl), .false., status)
   end subroutine init_sys_modes

   !> Restore the modes saved by `init_sys_modes`.
   !> Port of the termios part of sysdep.c:reset_sys_modes.
   subroutine reset_sys_modes(fd, old, status)
      integer(int32), intent(in) :: fd
      type(emacs_tty_type), intent(in) :: old
      type(status_type), intent(out) :: status

      call emacs_set_tty(fd, old, .false., status)
   end subroutine reset_sys_modes

   !> Width and height of the terminal on `fd`.
   !> Port of sysdep.c:get_tty_size.
   subroutine get_tty_size(fd, width, height, status)
      integer(int32), intent(in) :: fd
      integer(int32), intent(out) :: width, height
      type(status_type), intent(out) :: status
      type(winsize_type) :: ws

      width = 0
      height = 0
      if (c_ioctl_winsize(fd, TIOCGWINSZ, ws) < 0) then
         call sys_error(status, "ioctl(TIOCGWINSZ)")
         return
      end if
      width = int(ws%ws_col, int32)
      height = int(ws%ws_row, int32)
   end subroutine get_tty_size

   !> Set the size of the terminal on `fd` (used on pseudo-terminals).
   !> Port of sysdep.c:set_window_size.
   subroutine set_tty_size(fd, width, height, status)
      integer(int32), intent(in) :: fd
      integer(int32), intent(in) :: width, height
      type(status_type), intent(out) :: status
      type(winsize_type) :: ws

      if (c_ioctl_winsize(fd, TIOCGWINSZ, ws) < 0) then
         call sys_error(status, "ioctl(TIOCGWINSZ)")
         return
      end if
      ws%ws_col = int(width, kind(ws%ws_col))
      ws%ws_row = int(height, kind(ws%ws_row))
      if (c_ioctl_winsize(fd, TIOCSWINSZ, ws) < 0) call sys_error(status, "ioctl(TIOCSWINSZ)")
   end subroutine set_tty_size

   !> Read up to len(buf) bytes from `fd` into `buf`, retrying on EINTR.
   !> `nread` is the count (0 at end of file).
   !> Port of sysdep.c:emacs_read (via emacs_intr_read, not interruptible).
   subroutine emacs_read(fd, buf, nread, status)
      integer(int32), intent(in) :: fd
      character(len=*), intent(out) :: buf
      integer(int64), intent(out) :: nread
      type(status_type), intent(out) :: status
      integer(c_long) :: r

      nread = 0
      do
         r = c_read(fd, buf, int(len(buf), c_size_t))
         if (r >= 0) exit
         if (errno() /= EINTR) then
            call sys_error(status, "read")
            return
         end if
      end do
      nread = int(r, int64)
   end subroutine emacs_read

   !> Write all of `buf` to `fd`, retrying on EINTR and partial writes.
   !> `written` is the number of bytes written before any error.
   !> Port of sysdep.c:emacs_full_write.
   subroutine emacs_full_write(fd, buf, written, status)
      integer(int32), intent(in) :: fd
      character(len=*), intent(in) :: buf
      integer(int64), intent(out) :: written
      type(status_type), intent(out) :: status
      integer(c_long) :: r
      integer(int64) :: n

      n = len(buf, kind=int64)
      written = 0
      do while (written < n)
         r = c_write(fd, buf(written + 1:), int(n - written, c_size_t))
         if (r < 0) then
            if (errno() == EINTR) cycle
            call sys_error(status, "write")
            return
         end if
         written = written + r
      end do
   end subroutine emacs_full_write

   !> Write `buf` to `fd`.  Port of sysdep.c:emacs_write.
   subroutine emacs_write(fd, buf, status)
      integer(int32), intent(in) :: fd
      character(len=*), intent(in) :: buf
      type(status_type), intent(out) :: status
      integer(int64) :: written

      call emacs_full_write(fd, buf, written, status)
   end subroutine emacs_write

   !> True if `fd` has input within `timeout_ms` milliseconds (0: just
   !> check; negative: wait indefinitely).  Not a port: the poll(2) core
   !> of what process.c:wait_reading_process_output does for keyboard input.
   logical function input_available(fd, timeout_ms)
      integer(int32), intent(in) :: fd, timeout_ms
      type(pollfd_type) :: pfd(1)
      integer(c_int) :: r

      pfd(1)%fd = fd
      pfd(1)%events = POLLIN
      pfd(1)%revents = 0
      do
         r = c_poll(pfd, 1_c_long, timeout_ms)
         if (r >= 0 .or. errno() /= EINTR) exit
      end do
      input_available = r > 0 .and. iand(pfd(1)%revents, POLLIN) /= 0
   end function input_available

   !> SIGWINCH handler: only sets a flag.  The work happens in the
   !> command loop.  Cf. dispnew.c:deliver_window_change_signal.
   subroutine handle_window_change_signal(signum) bind(C)
      integer(c_int), value :: signum

      winch_flag = signum
   end subroutine handle_window_change_signal

   !> Install the SIGWINCH handler.
   subroutine install_window_change_handler()
      type(c_funptr) :: old

      old = c_signal(SIGWINCH, c_funloc(handle_window_change_signal))
   end subroutine install_window_change_handler

   !> True if the terminal was resized since the last call.
   logical function window_change_pending()
      window_change_pending = winch_flag /= 0
      winch_flag = 0
   end function window_change_pending

   !> Open a pseudo-terminal pair: `master` and the matching `slave`.
   !> Port of the Unix98 path of process.c:allocate_pty (plus opening the
   !> slave, as process.c:create_process does).
   subroutine open_pty(master, slave, status)
      integer(int32), intent(out) :: master, slave
      type(status_type), intent(out) :: status
      character(len=:), allocatable :: name

      slave = -1
      master = c_posix_openpt(ior(O_RDWR, O_NOCTTY))
      if (master < 0) then
         call sys_error(status, "posix_openpt")
         return
      end if
      if (c_grantpt(master) /= 0 .or. c_unlockpt(master) /= 0) then
         call sys_error(status, "grantpt/unlockpt")
         call close_fd(master)
         return
      end if
      name = c_string_to_fortran(c_ptsname(master))
      slave = c_open(name//char(0), ior(O_RDWR, O_NOCTTY))
      if (slave < 0) then
         call sys_error(status, "open("//name//")")
         call close_fd(master)
      end if
   end subroutine open_pty

   !> Close `fd`, ignoring errors.  Cf. sysdep.c:emacs_close.
   subroutine close_fd(fd)
      integer(int32), intent(in) :: fd
      integer(c_int) :: r

      r = c_close(fd)
   end subroutine close_fd

end module fm_sysdep
