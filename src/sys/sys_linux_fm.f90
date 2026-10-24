! sys_linux_fm.f90 -- raw Linux/glibc bindings: types, constants, calls.
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
!> - struct statx: linux/stat.h (the same on every architecture, which is
!>   why it is used instead of struct stat, whose layout differs)
!> - struct passwd: glibc pwd.h (pointers and 32-bit ids, the same on both)
!> - struct statvfs: glibc bits/statvfs.h (the 64-bit layout, the same on
!>   both)
!> - access modes: unistd.h;  AT_FDCWD: linux/fcntl.h
!> - wait status bits: glibc bits/waitstatus.h
!> - struct dirent: glibc bits/dirent.h (d_name at byte 19 on both);  struct
!>   tm: glibc bits/types/struct_tm.h (nine ints, then tm_gmtoff and tm_zone)
!> - threads (glibc >= 2.34 has them in libc itself): pthread_t is an
!>   unsigned long; pthread_mutex_t is 40 bytes on x86-64 and 48 on
!>   aarch64, so mutexes are 64-byte arrays, and PTHREAD_MUTEX_INITIALIZER
!>   is all zeros on both; sigset_t is 128 bytes (bits/types/__sigset_t.h);
!>   SIG_BLOCK and SIG_SETMASK: asm-generic/signal-defs.h
!> - pidfd_open: glibc 2.36 sys/pidfd.h (Linux 5.3);  TIOCSCTTY:
!>   asm-generic/ioctls.h
!>
!> Variadic functions (`open', `ioctl') are bound with fixed arguments.
!> That is safe on both targets: aarch64 Linux (standard AAPCS64, unlike
!> Apple's) passes variadic arguments like fixed ones, and on x86-64 the
!> only difference is %al, which glibc's prologue merely tests to decide
!> whether to spill vector registers.  `open' is never given O_CREAT here,
!> so it never reads a mode argument; files are created with `creat'.
!> O_DIRECTORY differs between the two targets, so it is not used.
module sys_linux_fm
    use, intrinsic :: iso_c_binding, only: c_int, c_signed_char, c_int16_t, c_int32_t, c_int64_t, &
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
    integer(c_int), parameter, public :: IUCLC = int(o'0001000', c_int)
    integer(c_int), parameter, public :: ICRNL = int(o'0000400', c_int)
    integer(c_int), parameter, public :: IXON = int(o'0002000', c_int)
    integer(c_int), parameter, public :: IXANY = int(o'0004000', c_int)
    integer(c_int), parameter, public :: IXOFF = int(o'0010000', c_int)
    integer(c_int), parameter, public :: IUTF8 = int(o'0040000', c_int)
    ! c_oflag
    integer(c_int), parameter, public :: OPOST = int(o'0000001', c_int)
    integer(c_int), parameter, public :: OLCUC = int(o'0000002', c_int)
    integer(c_int), parameter, public :: NLDLY = int(o'0000400', c_int)
    integer(c_int), parameter, public :: CRDLY = int(o'0003000', c_int)
    integer(c_int), parameter, public :: BSDLY = int(o'0020000', c_int)
    integer(c_int), parameter, public :: VTDLY = int(o'0040000', c_int)
    integer(c_int), parameter, public :: FFDLY = int(o'0100000', c_int)
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
    integer(c_int), parameter, public :: O_CLOEXEC = int(o'2000000', c_int)
    ! access modes
    integer(c_int), parameter, public :: F_OK = 0, X_OK = 1, W_OK = 2, R_OK = 4
    ! *at() directory argument for the current directory
    integer(c_int), parameter, public :: AT_FDCWD = -100
    ! statx
    integer(c_int), parameter, public :: AT_STATX_SYNC_AS_STAT = 0
    integer(c_int), parameter, public :: AT_SYMLINK_NOFOLLOW = int(z'100', c_int)
    integer(c_int), parameter, public :: STATX_BASIC_STATS = int(z'7FF', c_int)
    ! poll events
    integer(c_short), parameter, public :: POLLIN = 1_c_short
    integer(c_short), parameter, public :: POLLHUP = 16_c_short, POLLERR = 8_c_short
    ! signals
    integer(c_int), parameter, public :: SIGINT = 2, SIGTERM = 15, SIGCONT = 18, &
                                         SIGTSTP = 20, SIGWINCH = 28
    integer(c_int), parameter, public :: SIGHUP = 1, SIGQUIT = 3, SIGKILL = 9, SIGPIPE = 13
    ! pthread_sigmask actions
    integer(c_int), parameter, public :: SIG_BLOCK = 0, SIG_SETMASK = 2
    ! waitpid options
    integer(c_int), parameter, public :: WNOHANG = 1
    ! ioctl request making a terminal the controlling terminal
    integer(c_long), parameter, public :: TIOCSCTTY = int(z'540E', c_long)
    ! fcntl commands (asm-generic/fcntl.h, linux/fcntl.h)
    integer(c_int), parameter, public :: F_SETFD = 2, F_GETFL = 3, F_SETFL = 4, F_DUPFD_CLOEXEC = 1030, &
                                         FD_CLOEXEC = 1
    ! errno
    integer(c_int), parameter, public :: ENOENT = 2, EINTR = 4, EAGAIN = 11, &
                                         EACCES = 13, EEXIST = 17, ENOTDIR = 20, &
                                         EISDIR = 21, ERANGE = 34
    integer(c_int), parameter, public :: EIO = 5, ECHILD = 10, EPIPE = 32, ENOSYS = 38
    ! file type bits of st_mode (bits/stat.h)
    integer(c_int), parameter, public :: S_IFMT = int(o'170000', c_int)
    integer(c_int), parameter, public :: S_IFDIR = int(o'040000', c_int)
    integer(c_int), parameter, public :: S_IFREG = int(o'100000', c_int)
    integer(c_int), parameter, public :: S_IFLNK = int(o'120000', c_int)

    !> struct statx_timestamp.
    type, bind(C), public :: statx_timestamp_type
        integer(c_int64_t) :: tv_sec = 0
        integer(c_int32_t) :: tv_nsec = 0
        integer(c_int32_t) :: reserved = 0
    end type statx_timestamp_type

    !> struct statx (256 bytes).
    type, bind(C), public :: statx_type
        integer(c_int32_t) :: stx_mask = 0
        integer(c_int32_t) :: stx_blksize = 0
        integer(c_int64_t) :: stx_attributes = 0
        integer(c_int32_t) :: stx_nlink = 0
        integer(c_int32_t) :: stx_uid = 0
        integer(c_int32_t) :: stx_gid = 0
        integer(c_int16_t) :: stx_mode = 0
        integer(c_int16_t) :: spare0 = 0
        integer(c_int64_t) :: stx_ino = 0
        integer(c_int64_t) :: stx_size = 0
        integer(c_int64_t) :: stx_blocks = 0
        integer(c_int64_t) :: stx_attributes_mask = 0
        type(statx_timestamp_type) :: stx_atime, stx_btime, stx_ctime, stx_mtime
        integer(c_int32_t) :: stx_rdev_major = 0
        integer(c_int32_t) :: stx_rdev_minor = 0
        integer(c_int32_t) :: stx_dev_major = 0
        integer(c_int32_t) :: stx_dev_minor = 0
        integer(c_int64_t) :: spare2(14) = 0
    end type statx_type

    !> struct statvfs (glibc bits/statvfs.h on a 64-bit target: eleven
    !> unsigned longs, then the spare ints; 112 bytes).
    type, bind(C), public :: statvfs_type
        integer(c_int64_t) :: f_bsize = 0, f_frsize = 0, f_blocks = 0, f_bfree = 0, f_bavail = 0, &
                              f_files = 0, f_ffree = 0, f_favail = 0, f_fsid = 0, f_flag = 0, f_namemax = 0
        integer(c_int32_t) :: spare(6) = 0
    end type statvfs_type

    !> struct tm (56 bytes).
    type, bind(C), public :: tm_type
        integer(c_int) :: tm_sec = 0, tm_min = 0, tm_hour = 0, tm_mday = 0, tm_mon = 0, &
                          tm_year = 0, tm_wday = 0, tm_yday = 0, tm_isdst = 0
        integer(c_long) :: tm_gmtoff = 0
        type(c_ptr) :: tm_zone
    end type tm_type

    !> struct passwd.
    type, bind(C), public :: passwd_type
        type(c_ptr) :: pw_name, pw_passwd
        integer(c_int32_t) :: pw_uid = 0
        integer(c_int32_t) :: pw_gid = 0
        type(c_ptr) :: pw_gecos, pw_dir, pw_shell
    end type passwd_type

    public :: c_tcgetattr, c_tcsetattr, c_read, c_write, c_close, c_isatty, &
              c_poll, c_ioctl_winsize, c_signal, c_raise, c_posix_openpt, &
              c_grantpt, c_unlockpt, c_ptsname, c_open
    public :: c_mkdir, c_rmdir, c_setenv, c_unsetenv
    public :: c_creat, c_access, c_rename, c_unlink, c_chmod, c_statx, c_getcwd, &
              c_getuid, c_geteuid, c_getegid, c_getpwnam, c_getpwuid, c_strerror
    public :: c_fork, c_dup2, c_chdir, c_exit, c_pipe2, c_waitpid, c_readlink, c_execv, &
              c_strsignal, c_mkostemp, c_opendir, c_readdir, c_closedir, c_symlink, c_localtime_r
    public :: c_statvfs
    public :: c_pthread_create, c_pthread_join, c_pthread_self, c_pthread_mutex_lock, &
              c_pthread_mutex_unlock, c_pthread_sigmask, c_kill, c_setsid, c_pidfd_open, c_tcgetpgrp, &
              c_ioctl_int, c_fcntl_int, c_execve, c_sched_getaffinity, c_time, c_environ
    public :: errno, c_string_to_fortran

    !> The environment (char **environ).
    type(c_ptr), bind(C, name="environ") :: c_environ

    interface
        integer(c_int) function c_tcgetattr(fd, t) bind(C, name="tcgetattr")
            import :: c_int, termios_type
            implicit none(type, external)
            integer(c_int), value :: fd
            type(termios_type), intent(out) :: t
        end function c_tcgetattr

        !> tcgetpgrp(fd): the foreground process group of a terminal.
        integer(c_int) function c_tcgetpgrp(fd) bind(C, name="tcgetpgrp")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: fd
        end function c_tcgetpgrp

        integer(c_int) function c_tcsetattr(fd, action, t) bind(C, name="tcsetattr")
            import :: c_int, termios_type
            implicit none(type, external)
            integer(c_int), value :: fd, action
            type(termios_type), intent(in) :: t
        end function c_tcsetattr

        integer(c_long) function c_read(fd, buf, count) bind(C, name="read")
            import :: c_int, c_long, c_size_t, c_char
            implicit none(type, external)
            integer(c_int), value :: fd
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_size_t), value :: count
        end function c_read

        integer(c_long) function c_write(fd, buf, count) bind(C, name="write")
            import :: c_int, c_long, c_size_t, c_char
            implicit none(type, external)
            integer(c_int), value :: fd
            character(kind=c_char), intent(in) :: buf(*)
            integer(c_size_t), value :: count
        end function c_write

        integer(c_int) function c_close(fd) bind(C, name="close")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: fd
        end function c_close

        integer(c_int) function c_isatty(fd) bind(C, name="isatty")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: fd
        end function c_isatty

        integer(c_int) function c_poll(fds, nfds, timeout) bind(C, name="poll")
            import :: c_int, c_long, pollfd_type
            implicit none(type, external)
            type(pollfd_type), intent(inout) :: fds(*)
            integer(c_long), value :: nfds
            integer(c_int), value :: timeout
        end function c_poll

        !> ioctl(fd, request, struct winsize *): see the module note.
        integer(c_int) function c_ioctl_winsize(fd, request, ws) bind(C, name="ioctl")
            import :: c_int, c_long, winsize_type
            implicit none(type, external)
            integer(c_int), value :: fd
            integer(c_long), value :: request
            type(winsize_type), intent(inout) :: ws
        end function c_ioctl_winsize

        !> signal(2) from glibc: BSD semantics (SA_RESTART, handler stays).
        type(c_funptr) function c_signal(signum, handler) bind(C, name="signal")
            import :: c_int, c_funptr
            implicit none(type, external)
            integer(c_int), value :: signum
            type(c_funptr), value :: handler
        end function c_signal

        integer(c_int) function c_raise(signum) bind(C, name="raise")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: signum
        end function c_raise

        integer(c_int) function c_posix_openpt(flags) bind(C, name="posix_openpt")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: flags
        end function c_posix_openpt

        integer(c_int) function c_grantpt(fd) bind(C, name="grantpt")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: fd
        end function c_grantpt

        integer(c_int) function c_unlockpt(fd) bind(C, name="unlockpt")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: fd
        end function c_unlockpt

        type(c_ptr) function c_ptsname(fd) bind(C, name="ptsname")
            import :: c_int, c_ptr
            implicit none(type, external)
            integer(c_int), value :: fd
        end function c_ptsname

        !> open(path, flags) without a mode: see the module note.
        integer(c_int) function c_open(path, flags) bind(C, name="open")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: flags
        end function c_open

        !> creat(path, mode): open(path, O_CREAT | O_WRONLY | O_TRUNC, mode)
        !> without a variadic call.
        integer(c_int) function c_creat(path, mode) bind(C, name="creat")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_creat

        integer(c_int) function c_access(path, mode) bind(C, name="access")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_access

        integer(c_int) function c_rename(old, new) bind(C, name="rename")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: old(*), new(*)
        end function c_rename

        integer(c_int) function c_unlink(path) bind(C, name="unlink")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
        end function c_unlink

        integer(c_int) function c_mkdir(path, mode) bind(C, name="mkdir")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_mkdir

        integer(c_int) function c_rmdir(path) bind(C, name="rmdir")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
        end function c_rmdir

        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name="setenv")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv

        integer(c_int) function c_unsetenv(name) bind(C, name="unsetenv")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: name(*)
        end function c_unsetenv

        integer(c_int) function c_chmod(path, mode) bind(C, name="chmod")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_chmod

        integer(c_int) function c_statx(dirfd, path, flags, mask, buf) bind(C, name="statx")
            import :: c_int, c_char, statx_type
            implicit none(type, external)
            integer(c_int), value :: dirfd
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: flags, mask
            type(statx_type), intent(out) :: buf
        end function c_statx

        type(c_ptr) function c_getcwd(buf, size) bind(C, name="getcwd")
            import :: c_ptr, c_char, c_size_t
            implicit none(type, external)
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_size_t), value :: size
        end function c_getcwd

        integer(c_int32_t) function c_getuid() bind(C, name="getuid")
            import :: c_int32_t
            implicit none(type, external)
        end function c_getuid

        integer(c_int32_t) function c_geteuid() bind(C, name="geteuid")
            import :: c_int32_t
            implicit none(type, external)
        end function c_geteuid

        integer(c_int32_t) function c_getegid() bind(C, name="getegid")
            import :: c_int32_t
            implicit none(type, external)
        end function c_getegid

        type(c_ptr) function c_getpwnam(name) bind(C, name="getpwnam")
            import :: c_ptr, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: name(*)
        end function c_getpwnam

        type(c_ptr) function c_getpwuid(uid) bind(C, name="getpwuid")
            import :: c_ptr, c_int32_t
            implicit none(type, external)
            integer(c_int32_t), value :: uid
        end function c_getpwuid

        type(c_ptr) function c_strerror(errnum) bind(C, name="strerror")
            import :: c_ptr, c_int
            implicit none(type, external)
            integer(c_int), value :: errnum
        end function c_strerror

        type(c_ptr) function c_opendir(path) bind(C, name="opendir")
            import :: c_ptr, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
        end function c_opendir

        !> The next struct dirent, or null at the end (or on error, with
        !> errno set).
        type(c_ptr) function c_readdir(dirp) bind(C, name="readdir")
            import :: c_ptr
            implicit none(type, external)
            type(c_ptr), value :: dirp
        end function c_readdir

        integer(c_int) function c_closedir(dirp) bind(C, name="closedir")
            import :: c_int, c_ptr
            implicit none(type, external)
            type(c_ptr), value :: dirp
        end function c_closedir

        integer(c_int) function c_symlink(target, linkpath) bind(C, name="symlink")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: target(*), linkpath(*)
        end function c_symlink

        type(c_ptr) function c_localtime_r(time, result) bind(C, name="localtime_r")
            import :: c_ptr, c_int64_t, tm_type
            implicit none(type, external)
            integer(c_int64_t), intent(in) :: time
            type(tm_type), intent(out) :: result
        end function c_localtime_r

        integer(c_int) function c_statvfs(path, buf) bind(C, name="statvfs")
            import :: c_int, c_char, statvfs_type
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
            type(statvfs_type), intent(out) :: buf
        end function c_statvfs

        integer(c_int32_t) function c_fork() bind(C, name="fork")
            import :: c_int32_t
            implicit none(type, external)
        end function c_fork

        integer(c_int) function c_dup2(oldfd, newfd) bind(C, name="dup2")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: oldfd, newfd
        end function c_dup2

        integer(c_int) function c_chdir(path) bind(C, name="chdir")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
        end function c_chdir

        !> _exit(2): leave a forked child without running exit handlers.
        subroutine c_exit(status) bind(C, name="_exit")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: status
        end subroutine c_exit

        integer(c_int) function c_pipe2(fds, flags) bind(C, name="pipe2")
            import :: c_int
            implicit none(type, external)
            integer(c_int), intent(out) :: fds(2)
            integer(c_int), value :: flags
        end function c_pipe2

        integer(c_int32_t) function c_waitpid(pid, wstatus, options) bind(C, name="waitpid")
            import :: c_int, c_int32_t
            implicit none(type, external)
            integer(c_int32_t), value :: pid
            integer(c_int), intent(out) :: wstatus
            integer(c_int), value :: options
        end function c_waitpid

        integer(c_long) function c_readlink(path, buf, bufsiz) bind(C, name="readlink")
            import :: c_long, c_size_t, c_char
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_size_t), value :: bufsiz
        end function c_readlink

        integer(c_int) function c_execv(path, argv) bind(C, name="execv")
            import :: c_int, c_char, c_ptr
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
            type(c_ptr), intent(in) :: argv(*)
        end function c_execv

        type(c_ptr) function c_strsignal(sig) bind(C, name="strsignal")
            import :: c_ptr, c_int
            implicit none(type, external)
            integer(c_int), value :: sig
        end function c_strsignal

        integer(c_int) function c_mkostemp(template, flags) bind(C, name="mkostemp")
            import :: c_int, c_char
            implicit none(type, external)
            character(kind=c_char), intent(inout) :: template(*)
            integer(c_int), value :: flags
        end function c_mkostemp

        !> pthread_create(thread, NULL, start, arg).
        integer(c_int) function c_pthread_create(thread, attr, start, arg) bind(C, name="pthread_create")
            import :: c_int, c_long, c_ptr, c_funptr
            implicit none(type, external)
            integer(c_long), intent(out) :: thread
            type(c_ptr), value :: attr
            type(c_funptr), value :: start
            type(c_ptr), value :: arg
        end function c_pthread_create

        !> pthread_join(thread, NULL).
        integer(c_int) function c_pthread_join(thread, retval) bind(C, name="pthread_join")
            import :: c_int, c_long, c_ptr
            implicit none(type, external)
            integer(c_long), value :: thread
            type(c_ptr), value :: retval
        end function c_pthread_join

        integer(c_long) function c_pthread_self() bind(C, name="pthread_self")
            import :: c_long
            implicit none(type, external)
        end function c_pthread_self

        integer(c_int) function c_pthread_mutex_lock(mutex) bind(C, name="pthread_mutex_lock")
            import :: c_int, c_int64_t
            implicit none(type, external)
            integer(c_int64_t), intent(inout) :: mutex(*)
        end function c_pthread_mutex_lock

        integer(c_int) function c_pthread_mutex_unlock(mutex) bind(C, name="pthread_mutex_unlock")
            import :: c_int, c_int64_t
            implicit none(type, external)
            integer(c_int64_t), intent(inout) :: mutex(*)
        end function c_pthread_mutex_unlock

        !> pthread_sigmask(how, set, oldset) with 128-byte sigset_t arrays.
        integer(c_int) function c_pthread_sigmask(how, set, oldset) bind(C, name="pthread_sigmask")
            import :: c_int, c_int64_t
            implicit none(type, external)
            integer(c_int), value :: how
            integer(c_int64_t), intent(in) :: set(16)
            integer(c_int64_t), intent(out) :: oldset(16)
        end function c_pthread_sigmask

        integer(c_int) function c_kill(pid, sig) bind(C, name="kill")
            import :: c_int, c_int32_t
            implicit none(type, external)
            integer(c_int32_t), value :: pid
            integer(c_int), value :: sig
        end function c_kill

        integer(c_int32_t) function c_setsid() bind(C, name="setsid")
            import :: c_int32_t
            implicit none(type, external)
        end function c_setsid

        !> pidfd_open(pid, flags): a descriptor that becomes readable when
        !> child `pid' terminates.
        integer(c_int) function c_pidfd_open(pid, flags) bind(C, name="pidfd_open")
            import :: c_int, c_int32_t
            implicit none(type, external)
            integer(c_int32_t), value :: pid
            integer(c_int), value :: flags
        end function c_pidfd_open

        !> ioctl(fd, request, int): see the module note.
        integer(c_int) function c_ioctl_int(fd, request, arg) bind(C, name="ioctl")
            import :: c_int, c_long
            implicit none(type, external)
            integer(c_int), value :: fd
            integer(c_long), value :: request
            integer(c_int), value :: arg
        end function c_ioctl_int

        integer(c_int) function c_execve(path, argv, envp) bind(C, name="execve")
            import :: c_int, c_char, c_ptr
            implicit none(type, external)
            character(kind=c_char), intent(in) :: path(*)
            type(c_ptr), intent(in) :: argv(*), envp(*)
        end function c_execve

        !> sched_getaffinity(pid, 128, mask): the CPUs the process may run
        !> on, as a 1024-bit cpu_set_t.
        integer(c_int) function c_sched_getaffinity(pid, size, mask) bind(C, name="sched_getaffinity")
            import :: c_int, c_int32_t, c_size_t, c_int64_t
            implicit none(type, external)
            integer(c_int32_t), value :: pid
            integer(c_size_t), value :: size
            integer(c_int64_t), intent(out) :: mask(16)
        end function c_sched_getaffinity

        !> time(NULL).
        integer(c_int64_t) function c_time(tloc) bind(C, name="time")
            import :: c_int64_t, c_ptr
            implicit none(type, external)
            type(c_ptr), value :: tloc
        end function c_time

        !> fcntl(fd, cmd, int): variadic, bound like ioctl (module note).
        integer(c_int) function c_fcntl_int(fd, cmd, arg) bind(C, name="fcntl")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: fd, cmd, arg
        end function c_fcntl_int

        type(c_ptr) function c_errno_location() bind(C, name="__errno_location")
            import :: c_ptr
            implicit none(type, external)
        end function c_errno_location
    end interface

contains

    !> Current value of errno.
    integer(c_int) function errno()
        integer(c_int), pointer :: p
        call c_f_pointer(c_errno_location(), p)
        errno = p
    end function errno

    !> Copy of the NUL-terminated C string at `p' ("" for a null pointer).
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
end module sys_linux_fm
