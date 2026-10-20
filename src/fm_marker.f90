! fm_marker.f90 -- markers and character/byte position conversion.
! Ported from GNU Emacs 31.1 src/marker.c.
!
! Copyright (C) 1985, 1997-1998, 2001-2026 Free Software Foundation,
! Inc.
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

!> Markers, and converting between character and byte positions.
!>
!> A marker records a buffer and a (charpos, bytepos) pair that fm_insdel
!> keeps up to date as text is inserted and deleted.  Each buffer's markers
!> form a singly linked chain (upstream `next`), headed by
!> `buffers(b)%text%markers`; links are marker registry slots.
!>
!> Markers are referred to by handle.  Upstream frees unreachable markers
!> in GC; here `free_marker` must be called (it detaches the marker first).
!> Killing a buffer leaves its markers pointing nowhere.
!>
!> Position conversion finds the known (charpos, bytepos) pair closest to
!> the target -- BEG, BEGV, PT, GPT, ZV, Z, a one-entry cache, and the
!> buffer's markers -- and scans from there.  Upstream also creates a
!> marker after a long scan "until the next GC"; without GC that would
!> leak, so only the cache records it.
module fm_marker
   use, intrinsic :: iso_fortran_env, only: int64
   use fm_error, only: status_type, set_error, FM_GENERIC_ERROR
   use fm_handle, only: handle_type, slot_table_type, null_handle
   use fm_buffer, only: buffers, cur, BEG, BEG_BYTE, buf_next_char_len, &
                        buf_prev_char_len, resolve_buffer, buffer_handle
   implicit none(type, external)
   private

   public :: buf_charpos_to_bytepos, buf_bytepos_to_charpos
   public :: char_to_byte, byte_to_char, clear_charpos_cache
   public :: make_marker, free_marker, marker_buffer, marker_position, &
             marker_byte_position, marker_last_position, set_marker, &
             set_marker_to, set_marker_restricted, set_marker_both, &
             set_marker_restricted_both, detach_marker, copy_marker, &
             copy_marker_at, marker_insertion_type, &
             set_marker_insertion_type, resolve_marker
   public :: unchain_all_markers, count_markers

   !> Port of lisp.h:struct Lisp_Marker.
   type, public :: marker_type
      !> Buffer slot, or 0 when the marker points nowhere.
      integer(int64) :: buffer = 0
      integer(int64) :: charpos = 0, bytepos = 0
      !> True: the marker advances past text inserted at it.
      logical :: insertion_type = .false.
      !> Next marker slot in the buffer's chain, 0 at the end.
      integer(int64) :: next = 0
   end type marker_type

   !> Registry storage; fm_insdel walks the chains directly.
   type(marker_type), allocatable, public :: markers(:)
   type(slot_table_type) :: table

   !> marker.c: BYTECHAR_DISTANCE_INITIAL / _INCREMENT.
   integer(int64), parameter :: distance_initial = 50, distance_increment = 50

   !> The one-entry cache of marker.c (cached_charpos et al.).  The buffer
   !> is identified by serial, since registry slots are reused.
   integer(int64) :: cached_charpos = 0, cached_bytepos = 0
   integer(int64) :: cached_buffer = 0, cached_modiff = -1

contains

   !> Forget the cached position for buffer slot `b`.
   !> Port of marker.c:clear_charpos_cache.
   subroutine clear_charpos_cache(b)
      integer(int64), intent(in) :: b

      if (cached_buffer == buffers(b)%serial) cached_buffer = 0
   end subroutine clear_charpos_cache

   !> Byte position of `charpos` in buffer slot `b`.
   !> Port of marker.c:buf_charpos_to_bytepos.
   integer(int64) function buf_charpos_to_bytepos(b, charpos) result(bytepos)
      integer(int64), intent(in) :: b, charpos
      integer(int64) :: best_above, best_above_byte, best_below, best_below_byte
      integer(int64) :: distance, m
      logical :: done

      associate (buf => buffers(b))
         best_above = buf%text%z
         best_above_byte = buf%text%z_byte
         ! As many chars as bytes: every char is one byte.
         if (best_above == best_above_byte) then
            bytepos = charpos
            return
         end if
         best_below = BEG
         best_below_byte = BEG_BYTE

         done = .false.
         call consider(buf%pt, buf%pt_byte)
         if (done) return
         call consider(buf%text%gpt, buf%text%gpt_byte)
         if (done) return
         call consider(buf%begv, buf%begv_byte)
         if (done) return
         call consider(buf%zv, buf%zv_byte)
         if (done) return
         if (buf%serial == cached_buffer .and. buf%text%modiff == cached_modiff) then
            call consider(cached_charpos, cached_bytepos)
            if (done) return
         end if

         ! Markers, until the bracket is narrow enough to just scan; the
         ! further down the chain, the less demanding (see marker.c).
         distance = distance_initial
         m = buf%text%markers
         do while (m /= 0)
            if (best_above - charpos < distance .or. charpos - best_below < distance) exit
            call consider(markers(m)%charpos, markers(m)%bytepos)
            if (done) return
            m = markers(m)%next
            distance = distance + distance_increment
         end do

         ! Scan from whichever known point is closer.
         if (charpos - best_below < best_above - charpos) then
            do while (best_below < charpos)
               best_below = best_below + 1
               best_below_byte = best_below_byte + buf_next_char_len(buf, best_below_byte)
            end do
            call remember(best_below, best_below_byte)
            bytepos = best_below_byte
         else
            do while (best_above > charpos)
               best_above = best_above - 1
               best_above_byte = best_above_byte - buf_prev_char_len(buf, best_above_byte)
            end do
            call remember(best_above, best_above_byte)
            bytepos = best_above_byte
         end if
      end associate

   contains

      !> The CONSIDER macro of buf_charpos_to_bytepos.
      subroutine consider(this_charpos, this_bytepos)
         integer(int64), intent(in) :: this_charpos, this_bytepos
         logical :: changed

         changed = .false.
         if (this_charpos == charpos) then
            bytepos = this_bytepos
            done = .true.
            return
         else if (this_charpos > charpos) then
            if (this_charpos < best_above) then
               best_above = this_charpos
               best_above_byte = this_bytepos
               changed = .true.
            end if
         else if (this_charpos > best_below) then
            best_below = this_charpos
            best_below_byte = this_bytepos
            changed = .true.
         end if
         ! An all-ASCII stretch between the two bounds: interpolate.
         if (changed .and. best_above - best_below == best_above_byte - best_below_byte) then
            bytepos = best_below_byte + (charpos - best_below)
            done = .true.
         end if
      end subroutine consider

      subroutine remember(c, bpos)
         integer(int64), intent(in) :: c, bpos

         cached_buffer = buffers(b)%serial
         cached_modiff = buffers(b)%text%modiff
         cached_charpos = c
         cached_bytepos = bpos
      end subroutine remember

   end function buf_charpos_to_bytepos

   !> Character position of `bytepos` (a character boundary) in buffer
   !> slot `b`.  Port of marker.c:buf_bytepos_to_charpos.
   integer(int64) function buf_bytepos_to_charpos(b, bytepos) result(charpos)
      integer(int64), intent(in) :: b, bytepos
      integer(int64) :: best_above, best_above_byte, best_below, best_below_byte
      integer(int64) :: distance, m
      logical :: done

      associate (buf => buffers(b))
         best_above = buf%text%z
         best_above_byte = buf%text%z_byte
         if (best_above == best_above_byte) then
            charpos = bytepos
            return
         end if
         best_below = BEG
         best_below_byte = BEG_BYTE

         done = .false.
         call consider(buf%pt_byte, buf%pt)
         if (done) return
         call consider(buf%text%gpt_byte, buf%text%gpt)
         if (done) return
         call consider(buf%begv_byte, buf%begv)
         if (done) return
         call consider(buf%zv_byte, buf%zv)
         if (done) return
         if (buf%serial == cached_buffer .and. buf%text%modiff == cached_modiff) then
            call consider(cached_bytepos, cached_charpos)
            if (done) return
         end if

         distance = distance_initial
         m = buf%text%markers
         do while (m /= 0)
            if (best_above_byte - bytepos < distance .or. bytepos - best_below_byte < distance) exit
            call consider(markers(m)%bytepos, markers(m)%charpos)
            if (done) return
            m = markers(m)%next
            distance = distance + distance_increment
         end do

         if (bytepos - best_below_byte < best_above_byte - bytepos) then
            do while (best_below_byte < bytepos)
               best_below = best_below + 1
               best_below_byte = best_below_byte + buf_next_char_len(buf, best_below_byte)
            end do
            call remember(best_below, best_below_byte)
            charpos = best_below
         else
            do while (best_above_byte > bytepos)
               best_above = best_above - 1
               best_above_byte = best_above_byte - buf_prev_char_len(buf, best_above_byte)
            end do
            call remember(best_above, best_above_byte)
            charpos = best_above
         end if
      end associate

   contains

      !> The CONSIDER macro of buf_bytepos_to_charpos.
      subroutine consider(this_bytepos, this_charpos)
         integer(int64), intent(in) :: this_bytepos, this_charpos
         logical :: changed

         changed = .false.
         if (this_bytepos == bytepos) then
            charpos = this_charpos
            done = .true.
            return
         else if (this_bytepos > bytepos) then
            if (this_bytepos < best_above_byte) then
               best_above = this_charpos
               best_above_byte = this_bytepos
               changed = .true.
            end if
         else if (this_bytepos > best_below_byte) then
            best_below = this_charpos
            best_below_byte = this_bytepos
            changed = .true.
         end if
         if (changed .and. best_above - best_below == best_above_byte - best_below_byte) then
            charpos = best_below + (bytepos - best_below_byte)
            done = .true.
         end if
      end subroutine consider

      subroutine remember(c, bpos)
         integer(int64), intent(in) :: c, bpos

         cached_buffer = buffers(b)%serial
         cached_modiff = buffers(b)%text%modiff
         cached_charpos = c
         cached_bytepos = bpos
      end subroutine remember

   end function buf_bytepos_to_charpos

   !> Port of buffer.h:CHAR_TO_BYTE (current buffer).
   integer(int64) function char_to_byte(charpos)
      integer(int64), intent(in) :: charpos

      char_to_byte = buf_charpos_to_bytepos(cur, charpos)
   end function char_to_byte

   !> Port of buffer.h:BYTE_TO_CHAR (current buffer).
   integer(int64) function byte_to_char(bytepos)
      integer(int64), intent(in) :: bytepos

      byte_to_char = buf_bytepos_to_charpos(cur, bytepos)
   end function byte_to_char

   ! ------------------------------------------------------------------
   ! Marker objects
   ! ------------------------------------------------------------------

   !> Slot of marker `m`, or invalid-handle.
   subroutine resolve_marker(m, i, status)
      type(handle_type), intent(in) :: m
      integer(int64), intent(out) :: i
      type(status_type), intent(out) :: status

      call table%resolve(m, i, status)
   end subroutine resolve_marker

   !> A new marker that points nowhere.  Port of alloc.c:Fmake_marker.
   subroutine make_marker(m)
      type(handle_type), intent(out) :: m
      type(marker_type), allocatable :: grown(:)

      call table%acquire(m)
      if (.not. allocated(markers)) then
         allocate (markers(table%capacity()))
      else if (size(markers, kind=int64) < table%capacity()) then
         allocate (grown(table%capacity()))
         grown(1:size(markers)) = markers
         call move_alloc(grown, markers)
      end if
      markers(m%index) = marker_type()
   end subroutine make_marker

   !> Detach and free marker `m`; its handle becomes stale.  Not a port:
   !> upstream relies on GC.
   subroutine free_marker(m, status)
      type(handle_type), intent(in) :: m
      type(status_type), intent(out) :: status
      integer(int64) :: i

      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      call unchain_marker(i)
      call table%release(m, status)
   end subroutine free_marker

   !> Buffer `m` points into, or the null handle.
   !> Port of marker.c:Fmarker_buffer.
   subroutine marker_buffer(m, b, status)
      type(handle_type), intent(in) :: m
      type(handle_type), intent(out) :: b
      type(status_type), intent(out) :: status
      integer(int64) :: i

      b = null_handle()
      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      if (markers(i)%buffer /= 0) b = buffer_handle(markers(i)%buffer)
   end subroutine marker_buffer

   !> Character position of `m`; an error if it points nowhere.
   !> Port of marker.c:marker_position (Fmarker_position returns nil
   !> where this fails).
   subroutine marker_position(m, pos, status)
      type(handle_type), intent(in) :: m
      integer(int64), intent(out) :: pos
      type(status_type), intent(out) :: status
      integer(int64) :: i

      pos = 0
      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      if (markers(i)%buffer == 0) then
         call set_error(status, FM_GENERIC_ERROR, "Marker does not point anywhere")
         return
      end if
      pos = markers(i)%charpos
   end subroutine marker_position

   !> Byte position of `m`.  Port of marker.c:marker_byte_position.
   subroutine marker_byte_position(m, pos, status)
      type(handle_type), intent(in) :: m
      integer(int64), intent(out) :: pos
      type(status_type), intent(out) :: status
      integer(int64) :: i

      pos = 0
      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      if (markers(i)%buffer == 0) then
         call set_error(status, FM_GENERIC_ERROR, "Marker does not point anywhere")
         return
      end if
      pos = markers(i)%bytepos
   end subroutine marker_byte_position

   !> Last character position of `m`, even after its buffer was killed.
   !> Port of marker.c:Fmarker_last_position.
   subroutine marker_last_position(m, pos, status)
      type(handle_type), intent(in) :: m
      integer(int64), intent(out) :: pos
      type(status_type), intent(out) :: status
      integer(int64) :: i

      pos = 0
      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      pos = markers(i)%charpos
   end subroutine marker_last_position

   !> Point marker slot `i` at buffer slot `b`, moving it to `b`'s chain.
   !> Port of marker.c:attach_marker.
   subroutine attach_marker(i, b, charpos, bytepos)
      integer(int64), intent(in) :: i, b, charpos, bytepos

      markers(i)%charpos = charpos
      markers(i)%bytepos = bytepos
      if (markers(i)%buffer /= b) then
         call unchain_marker(i)
         markers(i)%buffer = b
         markers(i)%next = buffers(b)%text%markers
         buffers(b)%text%markers = i
      end if
   end subroutine attach_marker

   !> Remove marker slot `i` from its buffer's chain; it points nowhere.
   !> Port of marker.c:unchain_marker.
   subroutine unchain_marker(i)
      integer(int64), intent(in) :: i
      integer(int64) :: b, prev, tail

      b = markers(i)%buffer
      if (b == 0) return
      markers(i)%buffer = 0
      prev = 0
      tail = buffers(b)%text%markers
      do while (tail /= 0)
         if (tail == i) then
            if (prev == 0) then
               buffers(b)%text%markers = markers(i)%next
            else
               markers(prev)%next = markers(i)%next
            end if
            exit
         end if
         prev = tail
         tail = markers(tail)%next
      end do
      markers(i)%next = 0
   end subroutine unchain_marker

   !> Make every marker of buffer slot `b` point nowhere.  Port of the
   !> marker loop in buffer.c:Fkill_buffer.
   subroutine unchain_all_markers(b)
      integer(int64), intent(in) :: b
      integer(int64) :: m, next

      m = buffers(b)%text%markers
      do while (m /= 0)
         next = markers(m)%next
         markers(m)%buffer = 0
         markers(m)%next = 0
         m = next
      end do
      buffers(b)%text%markers = 0
   end subroutine unchain_all_markers

   !> Number of markers in buffer slot `b`'s chain.
   !> Port of marker.c:count_markers.
   integer(int64) function count_markers(b)
      integer(int64), intent(in) :: b
      integer(int64) :: m

      count_markers = 0
      m = buffers(b)%text%markers
      do while (m /= 0)
         count_markers = count_markers + 1
         m = markers(m)%next
      end do
   end function count_markers

   !> Shared core of set-marker for an integer position.
   !> Port of marker.c:set_marker_internal (integer POSITION).
   subroutine set_marker_pos(m, charpos_in, buffer, restricted, status)
      type(handle_type), intent(in) :: m
      integer(int64), intent(in) :: charpos_in
      type(handle_type), intent(in), optional :: buffer
      logical, intent(in) :: restricted
      type(status_type), intent(out) :: status
      integer(int64) :: i, b, charpos, bytepos

      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      call live_buffer(buffer, b)
      if (b == 0) then
         call unchain_marker(i)
         return
      end if
      associate (buf => buffers(b))
         if (restricted) then
            charpos = clip(buf%begv, charpos_in, buf%zv)
         else
            charpos = clip(BEG, charpos_in, buf%text%z)
         end if
      end associate
      bytepos = buf_charpos_to_bytepos(b, charpos)
      call attach_marker(i, b, charpos, bytepos)
   end subroutine set_marker_pos

   !> Position `m` before character `charpos` of `buffer` (default: the
   !> current buffer), clipped to the buffer.  A dead buffer makes `m`
   !> point nowhere.  Port of marker.c:Fset_marker (integer POSITION).
   subroutine set_marker(m, charpos, status, buffer)
      type(handle_type), intent(in) :: m
      integer(int64), intent(in) :: charpos
      type(status_type), intent(out) :: status
      type(handle_type), intent(in), optional :: buffer

      call set_marker_pos(m, charpos, buffer, .false., status)
   end subroutine set_marker

   !> Like `set_marker`, but clipped to the accessible region.
   !> Port of marker.c:set_marker_restricted.
   subroutine set_marker_restricted(m, charpos, status, buffer)
      type(handle_type), intent(in) :: m
      integer(int64), intent(in) :: charpos
      type(status_type), intent(out) :: status
      type(handle_type), intent(in), optional :: buffer

      call set_marker_pos(m, charpos, buffer, .true., status)
   end subroutine set_marker_restricted

   !> Position `m` where marker `position` is, in `buffer` (default: the
   !> current buffer).  If `position` points nowhere, so does `m`.  The
   !> byte position is only reused when both are in the same buffer
   !> (Bug#24368).  Port of marker.c:Fset_marker (marker POSITION).
   subroutine set_marker_to(m, position, status, buffer)
      type(handle_type), intent(in) :: m, position
      type(status_type), intent(out) :: status
      type(handle_type), intent(in), optional :: buffer
      integer(int64) :: i, p, b, charpos, bytepos

      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      call resolve_marker(position, p, status)
      if (.not. status%ok()) return
      call live_buffer(buffer, b)

      if (markers(p)%buffer == 0 .or. b == 0) then
         call unchain_marker(i)
      else if (b == markers(p)%buffer .and. b == markers(i)%buffer) then
         markers(i)%bytepos = markers(p)%bytepos
         markers(i)%charpos = markers(p)%charpos
      else
         charpos = clip(BEG, markers(p)%charpos, buffers(b)%text%z)
         if (markers(p)%buffer /= b) then
            bytepos = buf_charpos_to_bytepos(b, charpos)
         else
            bytepos = clip(BEG_BYTE, markers(p)%bytepos, buffers(b)%text%z_byte)
         end if
         call attach_marker(i, b, charpos, bytepos)
      end if
   end subroutine set_marker_to

   !> Set `m` from both positions, unchecked.  A dead `buffer` makes it
   !> point nowhere.  Port of marker.c:set_marker_both.
   subroutine set_marker_both(m, buffer, charpos, bytepos, status)
      type(handle_type), intent(in) :: m, buffer
      integer(int64), intent(in) :: charpos, bytepos
      type(status_type), intent(out) :: status
      integer(int64) :: i, b

      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      call live_buffer(buffer, b)
      if (b /= 0) then
         call attach_marker(i, b, charpos, bytepos)
      else
         call unchain_marker(i)
      end if
   end subroutine set_marker_both

   !> Like `set_marker_both`, clipped to the accessible region.
   !> Port of marker.c:set_marker_restricted_both.
   subroutine set_marker_restricted_both(m, buffer, charpos, bytepos, status)
      type(handle_type), intent(in) :: m, buffer
      integer(int64), intent(in) :: charpos, bytepos
      type(status_type), intent(out) :: status
      integer(int64) :: i, b

      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      call live_buffer(buffer, b)
      if (b /= 0) then
         call attach_marker(i, b, clip(buffers(b)%begv, charpos, buffers(b)%zv), &
                            clip(buffers(b)%begv_byte, bytepos, buffers(b)%zv_byte))
      else
         call unchain_marker(i)
      end if
   end subroutine set_marker_restricted_both

   !> Make `m` point nowhere.  Port of marker.c:detach_marker.
   subroutine detach_marker(m, status)
      type(handle_type), intent(in) :: m
      type(status_type), intent(out) :: status
      integer(int64) :: i

      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      call unchain_marker(i)
   end subroutine detach_marker

   !> A new marker where `source` is (or nowhere), with insertion type
   !> `insertion_type`.  Port of marker.c:Fcopy_marker (marker argument).
   subroutine copy_marker(source, new, insertion_type, status)
      type(handle_type), intent(in) :: source
      type(handle_type), intent(out) :: new
      logical, intent(in) :: insertion_type
      type(status_type), intent(out) :: status
      type(handle_type) :: b

      call marker_buffer(source, b, status)
      if (.not. status%ok()) return
      call make_marker(new)
      if (b%index /= 0) call set_marker_to(new, source, status, b)
      markers(new%index)%insertion_type = insertion_type
   end subroutine copy_marker

   !> A new marker at `charpos` in the current buffer.
   !> Port of marker.c:Fcopy_marker (integer argument).
   subroutine copy_marker_at(charpos, new, insertion_type, status)
      integer(int64), intent(in) :: charpos
      type(handle_type), intent(out) :: new
      logical, intent(in) :: insertion_type
      type(status_type), intent(out) :: status

      call make_marker(new)
      call set_marker(new, charpos, status)
      markers(new%index)%insertion_type = insertion_type
   end subroutine copy_marker_at

   !> Port of marker.c:Fmarker_insertion_type.
   subroutine marker_insertion_type(m, itype, status)
      type(handle_type), intent(in) :: m
      logical, intent(out) :: itype
      type(status_type), intent(out) :: status
      integer(int64) :: i

      itype = .false.
      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      itype = markers(i)%insertion_type
   end subroutine marker_insertion_type

   !> Port of marker.c:Fset_marker_insertion_type.
   subroutine set_marker_insertion_type(m, itype, status)
      type(handle_type), intent(in) :: m
      logical, intent(in) :: itype
      type(status_type), intent(out) :: status
      integer(int64) :: i

      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      markers(i)%insertion_type = itype
   end subroutine set_marker_insertion_type

   !> Slot of `buffer` (the current buffer if absent), or 0 if it is dead.
   !> Port of marker.c:live_buffer.
   subroutine live_buffer(buffer, b)
      type(handle_type), intent(in), optional :: buffer
      integer(int64), intent(out) :: b
      type(status_type) :: st

      if (.not. present(buffer)) then
         b = cur
         return
      end if
      call resolve_buffer(buffer, b, st)
      if (.not. st%ok()) b = 0
   end subroutine live_buffer

   !> Port of lisp.h:clip_to_bounds.
   pure integer(int64) function clip(lower, num, upper)
      integer(int64), intent(in) :: lower, num, upper

      clip = max(lower, min(num, upper))
   end function clip

end module fm_marker
