! buffer_ops_fm.f90 -- buffer.c commands that need markers or insdel.
! Ported from GNU Emacs 31.1 src/buffer.c.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> Part of buffer_fm.  A submodule can use marker_fm and insdel_fm even
!> though both use buffer_fm, which a module cannot.
submodule(buffer_fm) buffer_ops_fm
    use marker_fm, only: unchain_all_markers, clear_charpos_cache, free_marker
    use insdel_fm, only: del_range
    implicit none(type, external)

contains

    !> If it was current, the most recent other live, non-hidden buffer
    !> becomes current, creating *scratch* if there is none (as
    !> buffer.c:other_buffer_safely).  Hooks, processes, windows, overlays
    !> and indirect buffers are not handled yet.
    module subroutine kill_buffer(h, status)
        type(handle_type), intent(in) :: h
        type(status_type), intent(out) :: status
        type(handle_type) :: other
        type(status_type) :: ignore
        integer(int64) :: i, k
        logical :: was_current
        call resolve_buffer(h, i, status)
        if (.not. status%ok()) return
        ! Make this buffer not be displayed.
        if (associated(replace_buffer_in_windows_hook)) call replace_buffer_in_windows_hook(h)
        was_current = i == cur
        ! Its local variables go with it: make it not current first.
        if (was_current) call set_buffer_internal(0_int64)
        buffers(i)%last_name = buffers(i)%name
        call alist_remove(h)
        call frames_discard_buffer(h)
        ! Unchain all markers of this buffer and leave them pointing nowhere.
        call unchain_all_markers(i)
        call clear_charpos_cache(i)
        ! The mark and the mark ring belong to the buffer (upstream they are
        ! left to GC).
        if (buffers(i)%mark%index /= 0) call free_marker(buffers(i)%mark, ignore)
        if (allocated(buffers(i)%mark_ring)) then
            do k = 1, size(buffers(i)%mark_ring)
                call free_marker(buffers(i)%mark_ring(k), ignore)
            end do
        end if
        call table%release(h, status)
        if (.not. status%ok()) return
        if (allocated(buffers(i)%text%beg)) deallocate (buffers(i)%text%beg)
        buffers(i) = buffer_type()
        if (.not. was_current) return
        do k = alist_count, 1, -1
            if (buffers(alist(k)%index)%name(1:1) /= " ") then
                call set_buffer_internal(alist(k)%index)
                return
            end if
        end do
        call get_buffer_create("*scratch*", other, status)
        if (status%ok()) call set_buffer_internal(other%index)
    end subroutine kill_buffer

    module subroutine erase_buffer(status)
        type(status_type), intent(out) :: status
        if (cur == 0) return
        ! Fwiden (editfns.c), inline until editfns_fm exists.
        associate (b => buffers(cur))
            b%begv = BEG
            b%begv_byte = BEG_BYTE
            b%zv = b%text%z
            b%zv_byte = b%text%z_byte
        end associate
        call del_range(BEG, buffers(cur)%text%z, status)
    end subroutine erase_buffer

    !> Drop `h' from the live-buffer list.  Lives here, its only user:
    !> gfortran drops private parent procedures that only a submodule uses.
    subroutine alist_remove(h)
        type(handle_type), intent(in) :: h
        integer(int64) :: k
        do k = 1, alist_count
            if (alist(k)%index == h%index .and. alist(k)%generation == h%generation) then
                alist(k:alist_count - 1) = alist(k + 1:alist_count)
                alist_count = alist_count - 1
                return
            end if
        end do
    end subroutine alist_remove
end submodule buffer_ops_fm
