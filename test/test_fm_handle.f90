! test_fm_handle.f90 -- tests for fm_handle.
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

module test_fm_handle
   use, intrinsic :: iso_fortran_env, only: int64
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use fm_error, only: status_type, FM_INVALID_HANDLE
   use fm_handle, only: handle_type, slot_table_type, null_handle
   implicit none(type, external)
   private

   public :: collect_fm_handle

contains

   subroutine collect_fm_handle(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("acquire-resolve", test_acquire_resolve), &
                  new_unittest("null-never-resolves", test_null), &
                  new_unittest("stale-after-release", test_stale), &
                  new_unittest("stale-after-reuse", test_stale_after_reuse), &
                  new_unittest("double-release", test_double_release), &
                  new_unittest("growth", test_growth), &
                  new_unittest("foreign-handle", test_foreign), &
                  new_unittest("handle-of-slot", test_handle_of) &
                  ]
   end subroutine collect_fm_handle

   subroutine test_acquire_resolve(error)
      type(error_type), allocatable, intent(out) :: error
      type(slot_table_type) :: t
      type(handle_type) :: a, b
      type(status_type) :: st
      integer(int64) :: i

      call t%acquire(a)
      call t%acquire(b)
      call check(error, a%index /= b%index, "two live handles share a slot")
      if (allocated(error)) return
      call t%resolve(b, i, st)
      call check(error, st%ok(), "live handle failed to resolve")
      if (allocated(error)) return
      call check(error, i, b%index)
      if (allocated(error)) return
      call check(error, t%count(), 2_int64)
      if (allocated(error)) return
      call check(error, t%capacity() >= 2_int64, "capacity below live count")
   end subroutine test_acquire_resolve

   subroutine test_null(error)
      type(error_type), allocatable, intent(out) :: error
      type(slot_table_type) :: t
      type(handle_type) :: h, d
      type(status_type) :: st
      integer(int64) :: i

      call t%acquire(h)
      call t%resolve(null_handle(), i, st)
      call check(error, st%code, FM_INVALID_HANDLE)
      if (allocated(error)) return
      ! A default-initialised handle is the null handle too.
      call t%resolve(d, i, st)
      call check(error, st%code, FM_INVALID_HANDLE)
      if (allocated(error)) return
      call check(error, i, 0_int64)
   end subroutine test_null

   subroutine test_stale(error)
      type(error_type), allocatable, intent(out) :: error
      type(slot_table_type) :: t
      type(handle_type) :: h, copy
      type(status_type) :: st

      call t%acquire(h)
      copy = h
      call t%release(h, st)
      call check(error, st%ok(), "release of live handle failed")
      if (allocated(error)) return
      call check(error,.not. t%is_live(copy), "copy still live after release")
      if (allocated(error)) return
      call check(error, t%count(), 0_int64)
   end subroutine test_stale

   subroutine test_stale_after_reuse(error)
      type(error_type), allocatable, intent(out) :: error
      type(slot_table_type) :: t
      type(handle_type) :: old, new
      type(status_type) :: st
      integer(int64) :: i

      call t%acquire(old)
      call t%release(old, st)
      call t%acquire(new)
      call check(error, new%index, old%index)  ! slot reused...
      if (allocated(error)) return
      call check(error, new%generation /= old%generation, "generation not bumped")
      if (allocated(error)) return
      call t%resolve(old, i, st)                ! ...but the old handle is stale
      call check(error, st%code, FM_INVALID_HANDLE)
      if (allocated(error)) return
      call check(error, t%is_live(new), "new handle not live")
   end subroutine test_stale_after_reuse

   subroutine test_double_release(error)
      type(error_type), allocatable, intent(out) :: error
      type(slot_table_type) :: t
      type(handle_type) :: h
      type(status_type) :: st

      call t%acquire(h)
      call t%release(h, st)
      call t%release(h, st)
      call check(error, st%code, FM_INVALID_HANDLE)
      if (allocated(error)) return
      call check(error, t%count(), 0_int64)
   end subroutine test_double_release

   subroutine test_growth(error)
      type(error_type), allocatable, intent(out) :: error
      type(slot_table_type) :: t
      type(handle_type) :: hs(100)
      type(status_type) :: st
      integer(int64) :: k, i

      do k = 1, size(hs, kind=int64)
         call t%acquire(hs(k))
      end do
      call check(error, t%count(), 100_int64)
      if (allocated(error)) return
      call check(error, t%capacity() >= 100_int64, "capacity did not grow")
      if (allocated(error)) return
      do k = 1, size(hs, kind=int64)
         call t%resolve(hs(k), i, st)
         call check(error, st%ok(), "handle lost after growth")
         if (allocated(error)) return
      end do
      ! Releasing every other handle and reacquiring reuses slots.
      do k = 1, size(hs, kind=int64), 2
         call t%release(hs(k), st)
      end do
      do k = 1, size(hs, kind=int64), 2
         call t%acquire(hs(k))
      end do
      call check(error, t%count(), 100_int64)
      if (allocated(error)) return
      call check(error, t%capacity() < 200_int64, "freed slots not reused")
   end subroutine test_growth

   ! A handle from one table must not resolve in an empty one.
   subroutine test_foreign(error)
      type(error_type), allocatable, intent(out) :: error
      type(slot_table_type) :: a, b
      type(handle_type) :: h

      call a%acquire(h)
      call check(error,.not. b%is_live(h), "empty table resolved a handle")
   end subroutine test_foreign

   subroutine test_handle_of(error)
      type(error_type), allocatable, intent(out) :: error
      type(slot_table_type) :: t
      type(handle_type) :: h, g
      type(status_type) :: st

      call t%acquire(h)
      g = t%handle_of(h%index)
      call check(error, g%index == h%index .and. g%generation == h%generation, "wrong handle")
      if (allocated(error)) return
      call t%release(h, st)
      g = t%handle_of(h%index)
      call check(error, g%index, 0_int64)
      if (allocated(error)) return
      g = t%handle_of(99_int64)
      call check(error, g%index, 0_int64)
   end subroutine test_handle_of

end module test_fm_handle
