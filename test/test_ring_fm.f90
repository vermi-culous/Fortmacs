! test_ring_fm.f90 -- tests for ring_fm.
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

!> Ports of ring-tests.el (those of the ported functions; the elements are
!> strings).
module test_ring_fm
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use ring_fm, only: ring_type, ring_index
    implicit none(type, external)
    private

    public :: collect_ring_fm

contains

    subroutine collect_ring_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("ring-tests-insert-at-beginning", test_insert_at_beginning), &
                    new_unittest("ring-tests-length", test_length), &
                    new_unittest("ring-tests-index", test_index), &
                    new_unittest("ring-tests-empty-p", test_empty_p), &
                    new_unittest("ring-tests-size", test_size), &
                    new_unittest("ring-tests-insert", test_insert), &
                    new_unittest("ring-tests-remove", test_remove), &
                    new_unittest("ring-tests-ref", test_ref), &
                    new_unittest("ring-tests-member", test_member), &
                    new_unittest("ring-tests-extend", test_extend), &
                    new_unittest("ring-resize/grow", test_grow), &
                    new_unittest("ring-resize/grow-empty", test_grow_empty), &
                    new_unittest("ring-resize/grow-wrapped-ring", test_grow_wrapped), &
                    new_unittest("ring-resize/shrink", test_shrink) &
                    ]
    end subroutine collect_ring_fm

    !> The items of RING, newest first, separated by spaces (ring-elements).
    function elements(ring) result(s)
        type(ring_type), intent(in) :: ring
        character(len=:), allocatable :: s, item
        type(status_type) :: st
        integer :: k
        s = ""
        do k = 0, ring%count() - 1
            call ring%ref(k, item, st)
            if (k > 0) s = s//" "
            s = s//item
        end do
    end function elements

    subroutine test_insert_at_beginning(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        call ring%make(5)
        call ring%insert_at_beginning("foo")
        call ring%insert_at_beginning("bar")
        call check(error, elements(ring), "foo bar")
    end subroutine test_insert_at_beginning

    subroutine test_length(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        call ring%make(2)
        call check(error, ring%count(), 0)
        if (allocated(error)) return
        call ring%insert("a")
        call check(error, ring%count(), 1)
        if (allocated(error)) return
        call ring%insert("b")
        call check(error, ring%count(), 2)
        if (allocated(error)) return
        call ring%insert("c")
        call check(error, ring%count(), 2)
    end subroutine test_length

    subroutine test_index(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, ring_index(0, 0, 3, 3), 2)
        if (.not. allocated(error)) call check(error, ring_index(0, 0, 3, 5), 2)
        if (.not. allocated(error)) call check(error, ring_index(0, 2, 3, 3), 1)
        if (.not. allocated(error)) call check(error, ring_index(1, 2, 3, 3), 0)
        if (.not. allocated(error)) call check(error, ring_index(2, 2, 3, 3), 2)
    end subroutine test_index

    subroutine test_empty_p(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        type(status_type) :: st
        call ring%make(5)
        call check(error, ring%empty_p())
        if (allocated(error)) return
        call ring%insert("1")
        call check(error,.not. ring%empty_p())
        if (allocated(error)) return
        call ring%remove(st)
        call check(error, ring%empty_p())
    end subroutine test_empty_p

    subroutine test_size(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        call ring%make(2)
        call check(error, ring%size(), 2)
        if (allocated(error)) return
        call ring%insert("a")
        call ring%insert("b")
        call ring%insert("c")
        call check(error, ring%size(), 2)
        if (allocated(error)) return
        call ring%extend(3)
        call check(error, ring%size(), 5)
    end subroutine test_size

    subroutine test_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        call ring%make(2)
        call ring%insert(":a")
        call check(error, elements(ring), ":a")
        if (allocated(error)) return
        call ring%insert(":b")
        call check(error, elements(ring), ":b :a")
        if (allocated(error)) return
        call ring%insert(":c")
        call check(error, elements(ring), ":c :b")
    end subroutine test_insert

    subroutine test_remove(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        type(status_type) :: st
        character(len=:), allocatable :: item
        call ring%make(2)
        call ring%remove(st)
        call check(error,.not. st%ok(), "should-error")
        if (allocated(error)) return
        call ring%insert("foo")
        call ring%insert("bar")
        call ring%remove(st, item=item)
        call check(error, item, "foo")
        if (.not. allocated(error)) call check(error, elements(ring), "bar")
        if (allocated(error)) return
        call ring%insert("baz")
        call ring%remove(st, 0, item)
        call check(error, item, "baz")
        if (.not. allocated(error)) call check(error, elements(ring), "bar")
    end subroutine test_remove

    subroutine test_ref(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        type(status_type) :: st
        character(len=:), allocatable :: item
        call ring%make(2)
        call ring%ref(0, item, st)
        call check(error,.not. st%ok(), "should-error")
        if (allocated(error)) return
        call ring%insert(":a")
        call ring%ref(0, item, st)
        call check(error, item, ":a")
        if (allocated(error)) return
        call ring%insert(":b")
        call ring%ref(0, item, st)
        call check(error, item, ":b")
        if (allocated(error)) return
        call ring%ref(1, item, st)
        call check(error, item, ":a")
        if (allocated(error)) return
        call ring%ref(2, item, st)
        call check(error, item, ":b")
    end subroutine test_ref

    subroutine test_member(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        call ring%make(3)
        call ring%insert("foo")
        call ring%insert("bar")
        call check(error, ring%member("foo"), 1)
        if (.not. allocated(error)) call check(error, ring%member("bar"), 0)
        if (.not. allocated(error)) call check(error, ring%member("baz"), -1)
    end subroutine test_member

    subroutine test_extend(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        call ring%make(2)
        call ring%insert("1")
        call ring%insert("2")
        call check(error, ring%size(), 2)
        if (.not. allocated(error)) call check(error, ring%count(), 2)
        if (allocated(error)) return
        call ring%extend(3)
        call ring%insert("3")
        call check(error, ring%size(), 5)
        if (.not. allocated(error)) call check(error, elements(ring), "3 2 1")
    end subroutine test_extend

    subroutine test_grow(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        call ring%make(3)
        call ring%insert("1")
        call ring%insert("2")
        call ring%insert("3")
        call ring%resize(5)
        call check(error, ring%size(), 5)
        if (.not. allocated(error)) call check(error, elements(ring), "3 2 1")
    end subroutine test_grow

    subroutine test_grow_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        call ring%make(3)
        call ring%resize(5)
        call check(error, ring%size(), 5)
        if (.not. allocated(error)) call check(error, elements(ring), "")
    end subroutine test_grow_empty

    subroutine test_grow_wrapped(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        integer :: k
        character(len=1) :: c
        call ring%make(3)
        do k = 1, 5
            write (c, "(i1)") k
            call ring%insert(c)
        end do
        call ring%resize(5)
        call check(error, ring%size(), 5)
        if (.not. allocated(error)) call check(error, elements(ring), "5 4 3")
    end subroutine test_grow_wrapped

    subroutine test_shrink(error)
        type(error_type), allocatable, intent(out) :: error
        type(ring_type) :: ring
        integer :: k
        character(len=1) :: c
        call ring%make(5)
        do k = 1, 5
            write (c, "(i1)") k
            call ring%insert(c)
        end do
        call ring%resize(3)
        call check(error, ring%size(), 3)
        if (.not. allocated(error)) call check(error, elements(ring), "5 4 3")
    end subroutine test_shrink
end module test_ring_fm
