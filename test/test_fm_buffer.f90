! test_fm_buffer.f90 -- tests for fm_buffer.
! Includes cases ported from GNU Emacs 31.1 test/src/buffer-tests.el.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

!> Buffers are global state; every test creates uniquely named buffers
!> and kills them before returning.
module test_fm_buffer
   use, intrinsic :: iso_fortran_env, only: int64
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use fm_error, only: status_type, FM_INVALID_HANDLE, FM_BUFFER_READ_ONLY, &
                       FM_GENERIC_ERROR
   use fm_handle, only: handle_type
   use fm_buffer
   use fm_insdel, only: insert
   implicit none(type, external)
   private

   public :: collect_fm_buffer

contains

   subroutine collect_fm_buffer(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("get-buffer-create", test_create), &
                  new_unittest("empty-name-rejected", test_empty_name), &
                  new_unittest("generate-new-buffer-name", test_generate_name), &
                  new_unittest("test-generate-new-buffer-name-bug27966", test_bug27966), &
                  new_unittest("rename-buffer", test_rename), &
                  new_unittest("kill-buffer-stale-handle", test_kill), &
                  new_unittest("kill-current-buffer", test_kill_current), &
                  new_unittest("buffer-modified-p", test_modified), &
                  new_unittest("test-buffer-chars-modified-ticks", test_chars_ticks), &
                  new_unittest("modiff-incr", test_modiff_incr), &
                  new_unittest("barf-if-buffer-read-only", test_read_only) &
                  ]
   end subroutine collect_fm_buffer

   subroutine make(name, h, error)
      character(len=*), intent(in) :: name
      type(handle_type), intent(out) :: h
      type(error_type), allocatable, intent(inout) :: error
      type(status_type) :: st

      call get_buffer_create(name, h, st)
      call check(error, st%ok(), "get_buffer_create failed: "//name)
   end subroutine make

   subroutine discard(h)
      type(handle_type), intent(in) :: h
      type(status_type) :: st

      call kill_buffer(h, st)
   end subroutine discard

   subroutine test_create(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: a, again
      character(len=:), allocatable :: name
      type(status_type) :: st
      integer(int64) :: i

      call make("tb-create", a, error)
      if (allocated(error)) return
      call make("tb-create", again, error)
      if (allocated(error)) return
      call check(error, again%index == a%index .and. again%generation == a%generation, &
                 "second get-buffer-create made a new buffer")
      if (allocated(error)) return
      call buffer_name(a, name, st)
      call check(error, name, "tb-create")
      if (allocated(error)) return
      call resolve_buffer(a, i, st)
      call check(error, buffers(i)%text%z, BEG)
      if (allocated(error)) return
      call check(error, buffers(i)%text%gap_size, 20_int64)
      if (allocated(error)) return
      call check(error, buffers(i)%multibyte, "new buffer not multibyte")
      call discard(a)
   end subroutine test_create

   subroutine test_empty_name(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h
      type(status_type) :: st

      call get_buffer_create("", h, st)
      call check(error, st%code, FM_GENERIC_ERROR)
   end subroutine test_empty_name

   subroutine test_generate_name(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: a, b

      call check(error, generate_new_buffer_name("tb-gen"), "tb-gen")
      if (allocated(error)) return
      call make("tb-gen", a, error)
      if (allocated(error)) return
      call check(error, generate_new_buffer_name("tb-gen"), "tb-gen<2>")
      if (allocated(error)) return
      call make("tb-gen<2>", b, error)
      if (allocated(error)) return
      call check(error, generate_new_buffer_name("tb-gen"), "tb-gen<3>")
      if (allocated(error)) return
      ! IGNORE names a taken name that may be returned anyway.
      call check(error, generate_new_buffer_name("tb-gen", "tb-gen<2>"), "tb-gen<2>")
      if (allocated(error)) return
      call check(error, generate_new_buffer_name("tb-gen", "tb-gen"), "tb-gen")
      call discard(a)
      call discard(b)
   end subroutine test_generate_name

   !> buffer-tests.el: test-generate-new-buffer-name-bug27966.
   subroutine test_bug27966(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h

      call make("nil", h, error)
      if (allocated(error)) return
      call check(error, generate_new_buffer_name("nil") /= "nil", "returned a taken name")
      call discard(h)
   end subroutine test_bug27966

   subroutine test_rename(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: a, b, old, found
      character(len=:), allocatable :: actual, last
      type(status_type) :: st

      old = current_buffer()
      call make("tb-ren-a", a, error)
      if (allocated(error)) return
      call make("tb-ren-b", b, error)
      if (allocated(error)) return
      call set_buffer(a, st)

      call rename_buffer("tb-ren-b", .false., actual, st)
      call check(error, st%code, FM_GENERIC_ERROR)
      if (allocated(error)) return
      call check(error, st%detail, "Buffer name `tb-ren-b' is in use")
      if (allocated(error)) return

      call rename_buffer("tb-ren-b", .true., actual, st)
      call check(error, actual, "tb-ren-b<2>")
      if (allocated(error)) return
      found = get_buffer("tb-ren-b<2>")
      call check(error, found%index, a%index)
      if (allocated(error)) return
      call buffer_last_name(a, last, st)
      call check(error, last, "tb-ren-a")
      if (allocated(error)) return

      ! Renaming to its own name is a no-op.
      call rename_buffer("tb-ren-b<2>", .false., actual, st)
      call check(error, st%ok() .and. actual == "tb-ren-b<2>", "self-rename failed")

      call set_buffer(old, st)
      call discard(a)
      call discard(b)
   end subroutine test_rename

   subroutine test_kill(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, again, found
      character(len=:), allocatable :: name
      type(status_type) :: st

      call make("tb-kill", h, error)
      if (allocated(error)) return
      call kill_buffer(h, st)
      call check(error, st%ok(), "kill failed")
      if (allocated(error)) return
      call check(error,.not. buffer_live_p(h), "killed buffer still live")
      if (allocated(error)) return
      call buffer_name(h, name, st)
      call check(error, st%code, FM_INVALID_HANDLE)
      if (allocated(error)) return
      found = get_buffer("tb-kill")
      call check(error, found%index, 0_int64)
      if (allocated(error)) return
      ! A new buffer may reuse the slot; the old handle stays dead.
      call make("tb-kill", again, error)
      if (allocated(error)) return
      call check(error,.not. buffer_live_p(h), "stale handle revived")
      call discard(again)
   end subroutine test_kill

   subroutine test_kill_current(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: a, b, hidden, old
      type(status_type) :: st

      old = current_buffer()
      call make("tb-kc-a", a, error)
      if (allocated(error)) return
      call make(" tb-kc-hidden", hidden, error)
      if (allocated(error)) return
      call make("tb-kc-b", b, error)
      if (allocated(error)) return
      call set_buffer(b, st)
      call kill_buffer(b, st)
      ! The newest other non-hidden buffer becomes current.
      call check(error, cur /= 0, "no current buffer after kill")
      if (allocated(error)) return
      call check(error, buffers(cur)%name(1:1) /= " ", "hidden buffer became current")
      if (allocated(error)) return
      call check(error, buffer_live_p(current_buffer()), "current buffer not live")
      if (allocated(error)) return
      call set_buffer(b, st)
      call check(error, st%code, FM_INVALID_HANDLE)
      if (allocated(error)) return
      call check(error, st%detail, "Selecting deleted buffer")
      if (buffer_live_p(old)) call set_buffer(old, st)
      call discard(a)
      call discard(hidden)
   end subroutine test_kill_current

   !> The file-independent part of buffer-tests.el: test-buffer-modifications
   !> and test-restore-buffer-modified-p.
   subroutine test_modified(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st
      logical :: m

      old = current_buffer()
      call make("tb-mod", h, error)
      if (allocated(error)) return
      call set_buffer(h, st)
      call buffer_modified_p(h, m, st)
      call check(error,.not. m, "new buffer modified")
      if (allocated(error)) return
      call insert("foo", st)
      call buffer_modified_p(h, m, st)
      call check(error, m, "insert did not modify")
      if (allocated(error)) return
      call restore_buffer_modified_p(.false.)
      call buffer_modified_p(h, m, st)
      call check(error,.not. m, "restore nil did not clear")
      if (allocated(error)) return
      call restore_buffer_modified_p(.true.)
      call buffer_modified_p(h, m, st)
      call check(error, m, "restore t did not set")
      if (buffer_live_p(old)) call set_buffer(old, st)
      call discard(h)
   end subroutine test_modified

   !> The file-independent part of buffer-tests.el:
   !> test-buffer-chars-modified-ticks.
   subroutine test_chars_ticks(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st
      integer(int64) :: ticks, modt

      old = current_buffer()
      call make("tb-ticks", h, error)
      if (allocated(error)) return
      call set_buffer(h, st)
      call buffer_chars_modified_tick(h, ticks, st)
      call buffer_modified_tick(h, modt, st)
      call check(error, ticks, 1_int64)
      if (allocated(error)) return
      call check(error, ticks, modt)
      if (allocated(error)) return
      call insert("foobar", st)
      call buffer_chars_modified_tick(h, ticks, st)
      call buffer_modified_tick(h, modt, st)
      call check(error, ticks, modt)
      if (allocated(error)) return
      call check(error, ticks > 1, "chars tick did not advance")
      if (buffer_live_p(old)) call set_buffer(old, st)
      call discard(h)
   end subroutine test_chars_ticks

   subroutine test_modiff_incr(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: a, old

      a = 10
      call modiff_incr(a, 0_int64, old)
      call check(error, old == 10 .and. a == 11, "len 0 adds 1")
      if (allocated(error)) return
      a = 10
      call modiff_incr(a, 1_int64)
      call check(error, a, 11_int64)      ! elogb(1) + 1 = 1
      if (allocated(error)) return
      a = 10
      call modiff_incr(a, 1000_int64)
      call check(error, a, 20_int64)      ! elogb(1000) + 1 = 10
   end subroutine test_modiff_incr

   subroutine test_read_only(error)
      type(error_type), allocatable, intent(out) :: error
      type(handle_type) :: h, old
      type(status_type) :: st

      old = current_buffer()
      call make("tb-ro", h, error)
      if (allocated(error)) return
      call set_buffer(h, st)
      call barf_if_buffer_read_only(st)
      call check(error, st%ok(), "writable buffer barfed")
      if (allocated(error)) return
      call set_buffer_read_only(h, .true., st)
      call barf_if_buffer_read_only(st)
      call check(error, st%code, FM_BUFFER_READ_ONLY)
      if (allocated(error)) return
      call check(error, st%detail, "tb-ro")
      if (buffer_live_p(old)) call set_buffer(old, st)
      call discard(h)
   end subroutine test_read_only

end module test_fm_buffer
