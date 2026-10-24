! ring_fm.f90 -- handle rings of items.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/ring.el.
!
! Copyright (C) 1992, 2001-2026 Free Software Foundation, Inc.
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

!> Rings of strings, as the input history uses them: a `ring_type' holds
!> at most its size of items, the newest at index 0:
!>
!>     type(ring_type) :: ring
!>     call ring%make(500)
!>     call ring%insert("ls")
!>     call ring%ref(0, item, status)
!>
!> Upstream's rings hold any objects; the items here are strings (an
!> unallocated item stands for nil).  Not ported (Deferred): `ring_p' on
!> arbitrary objects, `ring_copy' (assignment copies a ring),
!> `ring_next', `ring_previous', `ring_elements', `ring_insert+extend',
!> `ring_remove+insert+extend', `ring_convert_sequence_to_ring'.
module ring_fm
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    implicit none(type, external)
    private

    !> One slot of a ring's vector.
    type :: ring_item_type
        character(len=:), allocatable :: s
    end type ring_item_type

    !> A ring: HEAD is the index (0-based) of the oldest item in VEC,
    !> LENGTH the number of items.  Port of ring.el's (HEAD LENGTH . VECTOR).
    type, public :: ring_type
        integer :: head = 0, length = 0
        type(ring_item_type), allocatable :: vec(:)
    contains
        !> Make the ring empty, able to contain SIZE elements.
        procedure :: make => ring_make
        !> The number of elements in the ring.
        procedure :: count => ring_length
        !> The size of the ring, the maximum number of elements it can contain.
        procedure :: size => ring_size
        !> Whether the ring is empty.
        procedure :: empty_p => ring_empty_p
        !> Insert ITEM as the newest (last) item, dumping the oldest item if
        !> the ring is full.
        procedure :: insert => ring_insert
        !> Add ITEM at the front, as the oldest item.
        procedure :: insert_at_beginning => ring_insert_at_beginning
        !> Remove an item (the oldest, or the one indexed).
        procedure :: remove => ring_remove
        !> The element at INDEX (0 the newest).
        procedure :: ref => ring_ref
        !> The index of ITEM in the ring, or -1.
        procedure :: member => ring_member
        !> Increase the size of the ring by X.
        procedure :: extend => ring_extend
        !> Set the size of the ring.
        procedure :: resize => ring_resize
    end type ring_type

    public :: ring_index

contains

    !> Make a ring that can contain SIZE elements.  Port of
    !> ring.el:make-ring.
    subroutine ring_make(self, size)
        class(ring_type), intent(inout) :: self
        integer, intent(in) :: size
        self%head = 0
        self%length = 0
        if (allocated(self%vec)) deallocate (self%vec)
        allocate (self%vec(0:max(size, 0) - 1))
    end subroutine ring_make

    !> Return INDEX+1, with wraparound.  Port of ring.el:ring-plus1.
    pure integer function ring_plus1(index, veclen) result(new_index)
        integer, intent(in) :: index, veclen
        new_index = index + 1
        if (new_index == veclen) new_index = 0
    end function ring_plus1

    !> Return INDEX-1, with wraparound.  Port of ring.el:ring-minus1.
    pure integer function ring_minus1(index, veclen) result(new_index)
        integer, intent(in) :: index, veclen
        if (index == 0) then
            new_index = veclen - 1
        else
            new_index = index - 1
        end if
    end function ring_minus1

    !> Return the number of elements in the RING.  Port of
    !> ring.el:ring-length.
    pure integer function ring_length(self)
        class(ring_type), intent(in) :: self
        ring_length = self%length
    end function ring_length

    !> Convert nominal ring index INDEX to an internal index.
    !> The internal index refers to the items ordered from newest to oldest.
    !> HEAD is the index of the oldest element in the ring.
    !> RINGLEN is the number of elements currently in the ring.
    !> VECLEN is the size of the vector in the ring.  Port of
    !> ring.el:ring-index.
    pure integer function ring_index(index, head, ringlen, veclen) result(k)
        integer, intent(in) :: index, head, ringlen, veclen
        k = modulo(index, ringlen)
        k = modulo(head + (ringlen - k) - 1, veclen)
    end function ring_index

    !> Return true if RING is empty; false otherwise.  Port of
    !> ring.el:ring-empty-p.
    pure logical function ring_empty_p(self)
        class(ring_type), intent(in) :: self
        ring_empty_p = self%length == 0
    end function ring_empty_p

    !> Return the size of RING, the maximum number of elements it can
    !> contain.  Port of ring.el:ring-size.
    pure integer function ring_size(self)
        class(ring_type), intent(in) :: self
        ring_size = 0
        if (allocated(self%vec)) ring_size = size(self%vec)
    end function ring_size

    !> Add to RING the item ITEM, at the front, as the oldest item.  Port
    !> of ring.el:ring-insert-at-beginning.
    subroutine ring_insert_at_beginning(self, item)
        class(ring_type), intent(inout) :: self
        character(len=*), intent(in) :: item
        integer :: veclen
        veclen = self%size()
        if (veclen == 0) return
        self%length = min(veclen, self%length + 1)
        self%head = ring_minus1(self%head, veclen)
        self%vec(self%head)%s = item
    end subroutine ring_insert_at_beginning

    !> Insert onto ring RING the item ITEM, as the newest (last) item.
    !> If the ring is full, dump the oldest item to make room.  Port of
    !> ring.el:ring-insert.
    subroutine ring_insert(self, item)
        class(ring_type), intent(inout) :: self
        character(len=*), intent(in) :: item
        integer :: veclen
        veclen = self%size()
        ! (A ring of size 0 holds nothing; upstream divides by zero.)
        if (veclen == 0) return
        self%vec(modulo(self%head + self%length, veclen))%s = item
        if (self%length == veclen) then
            self%head = ring_plus1(self%head, veclen)
        else
            self%length = self%length + 1
        end if
    end subroutine ring_insert

    !> Remove an item from the RING.  ITEM is the removed item.
    !> If optional INDEX is absent, remove the oldest item.  If it's
    !> present, remove the element indexed.  Port of ring.el:ring-remove.
    subroutine ring_remove(self, status, index, item)
        class(ring_type), intent(inout) :: self
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: index
        character(len=:), allocatable, intent(out), optional :: item
        integer :: veclen, tl, k
        if (self%empty_p()) then
            call set_error(status, FM_GENERIC_ERROR, "Ring empty")
            return
        end if
        veclen = self%size()
        tl = modulo(self%head + self%length - 1, veclen)
        if (present(index)) then
            k = index
        else
            k = self%length - 1
        end if
        k = ring_index(k, self%head, self%length, veclen)
        if (present(item)) then
            if (allocated(self%vec(k)%s)) item = self%vec(k)%s
        end if
        do while (k /= tl)
            if (allocated(self%vec(ring_plus1(k, veclen))%s)) then
                self%vec(k)%s = self%vec(ring_plus1(k, veclen))%s
            else if (allocated(self%vec(k)%s)) then
                deallocate (self%vec(k)%s)
            end if
            k = ring_plus1(k, veclen)
        end do
        if (allocated(self%vec(tl)%s)) deallocate (self%vec(tl)%s)
        self%length = self%length - 1
    end subroutine ring_remove

    !> Return RING's INDEX element.
    !> INDEX = 0 is the most recently inserted; higher indices
    !> correspond to older elements.
    !> INDEX need not be <= the ring length; the appropriate modulo
    !> operation will be performed.  Port of ring.el:ring-ref (a nil item
    !> is "").
    subroutine ring_ref(self, index, item, status)
        class(ring_type), intent(in) :: self
        integer, intent(in) :: index
        character(len=:), allocatable, intent(out) :: item
        type(status_type), intent(out) :: status
        integer :: k
        if (self%empty_p()) then
            call set_error(status, FM_GENERIC_ERROR, "Accessing an empty ring")
            item = ""
            return
        end if
        k = ring_index(index, self%head, self%length, self%size())
        if (allocated(self%vec(k)%s)) then
            item = self%vec(k)%s
        else
            item = ""
        end if
    end subroutine ring_ref

    !> Return index of ITEM if on RING, else -1.
    !> Comparison is done via `equal'.  The index is 0-based.  Port of
    !> ring.el:ring-member.
    integer function ring_member(self, item) result(ind)
        class(ring_type), intent(in) :: self
        character(len=*), intent(in) :: item
        integer :: k
        do ind = 0, self%length - 1
            k = ring_index(ind, self%head, self%length, self%size())
            if (allocated(self%vec(k)%s)) then
                if (len(self%vec(k)%s) == len(item)) then
                    if (self%vec(k)%s == item) return
                end if
            end if
        end do
        ind = -1
    end function ring_member

    !> Increase the size of RING by X.  Port of ring.el:ring-extend.
    subroutine ring_extend(self, x)
        class(ring_type), intent(inout) :: self
        integer, intent(in) :: x
        if (x > 0) call self%resize(x + self%size())
    end subroutine ring_extend

    !> Set the size of RING to SIZE.
    !> If the new size is smaller, then the oldest items in the ring are
    !> discarded.  Port of ring.el:ring-resize.
    subroutine ring_resize(self, size)
        class(ring_type), intent(inout) :: self
        integer, intent(in) :: size
        type(ring_item_type), allocatable :: new_vec(:)
        integer :: copy_length, copy_hd, old_size, j, k
        allocate (new_vec(0:max(size, 0) - 1))
        if (self%length == 0) then
            self%length = 0
            call move_alloc(new_vec, self%vec)
            return
        end if
        old_size = self%size()
        copy_length = min(size, self%length)
        copy_hd = modulo(self%head + (self%length - copy_length), self%length)
        ! If the ring is wrapped, the existing elements must be written
        ! out in the right order.
        do j = 0, copy_length - 1
            k = modulo(copy_hd + j, old_size)
            if (allocated(self%vec(k)%s)) call move_alloc(self%vec(k)%s, new_vec(j)%s)
        end do
        call move_alloc(new_vec, self%vec)
        self%length = copy_length
        self%head = 0
    end subroutine ring_resize
end module ring_fm
