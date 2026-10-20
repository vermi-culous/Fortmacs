! fm_handle.f90 -- generational handles and slot tables.
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

!> Handles for objects with explicit lifetimes (buffers, markers, windows).
!>
!> Not a port: upstream refers to these objects by pointer and relies on the
!> garbage collector.  Fortmacs keeps each kind of object in a registry
!> module that owns a `slot_table_type` plus a parallel array of payloads.
!> A handle is (index, generation).  Releasing a slot bumps its generation,
!> so handles taken before the release are detected as stale even after the
!> slot is reused.
!>
!> Typical owner pattern:
!>
!>     call table%acquire(h)             ! new slot; h%index is the payload index
!>     call grow_payload(table%capacity())
!>     ...
!>     call table%resolve(h, i, status)  ! i = payload index, or status error
!>     ...
!>     call table%release(h, status)     ! h and all its copies become stale
module fm_handle
   use, intrinsic :: iso_fortran_env, only: int64
   use fm_error, only: status_type, set_error, FM_INVALID_HANDLE
   implicit none(type, external)
   private

   public :: handle_type, slot_table_type, null_handle

   !> Reference to a slot.  Index 0 is the null handle.
   type :: handle_type
      integer(int64) :: index = 0
      integer(int64) :: generation = 0
   end type handle_type

   !> Allocation state of one kind of object.
   type :: slot_table_type
      private
      !> Current generation of each slot.  Starts at 1, so the default
      !> handle (generation 0) can never resolve.
      integer(int64), allocatable :: generations(:)
      logical, allocatable :: live(:)
      !> Stack of released slot indices, reused before growing.
      integer(int64), allocatable :: free_list(:)
      integer(int64) :: free_count = 0
      integer(int64) :: used = 0
      integer(int64) :: live_count = 0
   contains
      procedure :: acquire => table_acquire
      procedure :: release => table_release
      procedure :: resolve => table_resolve
      procedure :: is_live => table_is_live
      procedure :: capacity => table_capacity
      procedure :: count => table_count
   end type slot_table_type

   integer(int64), parameter :: initial_capacity = 16

contains

   !> The null handle, which never resolves.
   pure function null_handle() result(h)
      type(handle_type) :: h

      h = handle_type(0, 0)
   end function null_handle

   !> Allocate a slot and return a fresh handle to it.
   subroutine table_acquire(self, h)
      class(slot_table_type), intent(inout) :: self
      type(handle_type), intent(out) :: h
      integer(int64) :: i

      if (.not. allocated(self%generations)) call grow(self, initial_capacity)

      if (self%free_count > 0) then
         i = self%free_list(self%free_count)
         self%free_count = self%free_count - 1
      else
         if (self%used == size(self%generations, kind=int64)) then
            call grow(self, 2*size(self%generations, kind=int64))
         end if
         self%used = self%used + 1
         i = self%used
      end if

      self%live(i) = .true.
      self%live_count = self%live_count + 1
      h = handle_type(i, self%generations(i))
   end subroutine table_acquire

   !> Free the slot `h` refers to.  Fails on a null or stale handle,
   !> including a second release of the same handle.
   subroutine table_release(self, h, status)
      class(slot_table_type), intent(inout) :: self
      type(handle_type), intent(in) :: h
      type(status_type), intent(out) :: status
      integer(int64) :: i

      call self%resolve(h, i, status)
      if (.not. status%ok()) return

      self%live(i) = .false.
      self%generations(i) = self%generations(i) + 1
      self%live_count = self%live_count - 1
      self%free_count = self%free_count + 1
      self%free_list(self%free_count) = i
   end subroutine table_release

   !> Translate `h` into its payload index, or fail with invalid-handle.
   pure subroutine table_resolve(self, h, i, status)
      class(slot_table_type), intent(in) :: self
      type(handle_type), intent(in) :: h
      integer(int64), intent(out) :: i
      type(status_type), intent(out) :: status

      i = 0
      if (.not. self%is_live(h)) then
         call set_error(status, FM_INVALID_HANDLE)
         return
      end if
      i = h%index
   end subroutine table_resolve

   !> True when `h` refers to a live slot of this table.
   elemental logical function table_is_live(self, h)
      class(slot_table_type), intent(in) :: self
      type(handle_type), intent(in) :: h

      table_is_live = .false.
      if (.not. allocated(self%generations)) return
      if (h%index < 1 .or. h%index > self%used) return
      if (.not. self%live(h%index)) return
      table_is_live = self%generations(h%index) == h%generation
   end function table_is_live

   !> Number of payload slots the owner must provide (highest index in use
   !> can never exceed this).
   pure integer(int64) function table_capacity(self)
      class(slot_table_type), intent(in) :: self

      table_capacity = 0
      if (allocated(self%generations)) then
         table_capacity = size(self%generations, kind=int64)
      end if
   end function table_capacity

   !> Number of live slots.
   pure integer(int64) function table_count(self)
      class(slot_table_type), intent(in) :: self

      table_count = self%live_count
   end function table_count

   subroutine grow(self, new_size)
      type(slot_table_type), intent(inout) :: self
      integer(int64), intent(in) :: new_size
      integer(int64), allocatable :: gens(:), frees(:)
      logical, allocatable :: live(:)
      integer(int64) :: old

      old = 0
      if (allocated(self%generations)) old = size(self%generations, kind=int64)

      allocate (gens(new_size), live(new_size), frees(new_size))
      gens = 1
      live = .false.
      if (old > 0) then
         gens(1:old) = self%generations
         live(1:old) = self%live
         frees(1:self%free_count) = self%free_list(1:self%free_count)
      end if
      call move_alloc(gens, self%generations)
      call move_alloc(live, self%live)
      call move_alloc(frees, self%free_list)
   end subroutine grow

end module fm_handle
