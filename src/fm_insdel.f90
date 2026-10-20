! fm_insdel.f90 -- buffer insertion, deletion and gap motion.
! Ported from GNU Emacs 31.1 src/insdel.c.
!
! Copyright (C) 1985-1986, 1993-1995, 1997-2026 Free Software
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

!> Inserting and deleting text in the current buffer.
!>
!> All operations act on the current buffer, as upstream.  Text to insert
!> is given in the buffer's own representation (multibyte text for a
!> multibyte buffer); `insert_char` encodes a single character.
!>
!> Deferred until their subsystems exist: interval/text-property offsets,
!> overlay adjustment, undo recording, change hooks and
!> `inhibit-modification-hooks`, compositions, tree-sitter, quitting
!> in the middle of a gap move, `copy_text` conversions, `replace_range`
!> and `insert_from_buffer`.
module fm_insdel
   use, intrinsic :: iso_fortran_env, only: int32, int64
   use fm_error, only: status_type, set_error, FM_GENERIC_ERROR, FM_MEMORY_FULL
   use fm_character, only: char_string, multibyte_chars_in_text, &
                           MAX_MULTIBYTE_LENGTH
   use fm_buffer, only: buffers, cur, BEG, BUF_BYTES_MAX, GAP_BYTES_DFL, &
                        GAP_BYTES_MIN, modiff_incr, buf_compute_unchanged, &
                        buf_bytes, barf_if_buffer_read_only
   use fm_marker, only: char_to_byte, byte_to_char, markers
   implicit none(type, external)
   private

   public :: move_gap_both, make_gap
   public :: insert, insert_char, insert_before_markers, insert_1_both
   public :: del_range, del_range_1, del_range_byte, del_range_both, &
             del_range_2
   public :: modify_text, prepare_to_modify_buffer

contains

   ! ------------------------------------------------------------------
   ! The gap
   ! ------------------------------------------------------------------

   !> Move the gap to `bytepos` (also char position `charpos`).
   !> Port of insdel.c:move_gap_both.
   subroutine move_gap_both(charpos, bytepos)
      integer(int64), intent(in) :: charpos, bytepos

      if (bytepos < buffers(cur)%text%gpt_byte) then
         call gap_left(charpos, bytepos, .false.)
      else if (bytepos > buffers(cur)%text%gpt_byte) then
         call gap_right(charpos, bytepos)
      end if
   end subroutine move_gap_both

   !> Move the gap down to `bytepos`.  With `newgap`, leave the unchanged
   !> bookkeeping alone.  Port of insdel.c:gap_left.
   subroutine gap_left(charpos, bytepos, newgap)
      integer(int64), intent(in) :: charpos, bytepos
      logical, intent(in) :: newgap
      integer(int64) :: n

      if (.not. newgap) call buf_compute_unchanged(buffers(cur), charpos, buffers(cur)%text%gpt)
      associate (t => buffers(cur)%text)
         ! Bytes [bytepos, gpt_byte) move up to the end of the gap.
         n = t%gpt_byte - bytepos
         if (n > 0) then
            t%beg(bytepos + t%gap_size:t%gpt_byte - 1 + t%gap_size) = t%beg(bytepos:t%gpt_byte - 1)
         end if
         t%gpt_byte = bytepos
         t%gpt = charpos
      end associate
   end subroutine gap_left

   !> Move the gap up to `bytepos`.  Port of insdel.c:gap_right.
   subroutine gap_right(charpos, bytepos)
      integer(int64), intent(in) :: charpos, bytepos
      integer(int64) :: n

      call buf_compute_unchanged(buffers(cur), charpos, buffers(cur)%text%gpt)
      associate (t => buffers(cur)%text)
         ! Bytes [gpt_byte, bytepos) move down from after the gap.
         n = bytepos - t%gpt_byte
         if (n > 0) then
            t%beg(t%gpt_byte:bytepos - 1) = t%beg(t%gpt_byte + t%gap_size:bytepos - 1 + t%gap_size)
         end if
         t%gpt = charpos
         t%gpt_byte = bytepos
      end associate
   end subroutine gap_right

   !> Grow the gap by `nbytes_added` (plus slack), or shrink it by
   !> `-nbytes_added` (never below GAP_BYTES_MIN).
   !> Port of insdel.c:make_gap, make_gap_larger and make_gap_smaller.
   subroutine make_gap(nbytes_added, status)
      integer(int64), intent(in) :: nbytes_added
      type(status_type), intent(out) :: status

      if (nbytes_added >= 0) then
         ! Grow by at least 1/64 of the buffer to avoid O(N^2) growth.
         call make_gap_larger(max(nbytes_added, (buffers(cur)%text%z - BEG)/64), status)
      else
         call make_gap_smaller(-nbytes_added, status)
      end if
   end subroutine make_gap

   !> Port of insdel.c:make_gap_larger.  Upstream enlarges the block and
   !> slides the gap; here the text is copied into a larger string with the
   !> gap already in place, which gives the same layout.
   subroutine make_gap_larger(nbytes_requested, status)
      integer(int64), intent(in) :: nbytes_requested
      type(status_type), intent(out) :: status
      integer(int64) :: current_size, nbytes_added, after, new_gap
      character(len=:), allocatable :: grown
      integer :: astat

      associate (t => buffers(cur)%text)
         current_size = t%z_byte - BEG + t%gap_size
         if (BUF_BYTES_MAX - current_size < nbytes_requested) then
            call set_error(status, FM_GENERIC_ERROR, "Maximum buffer size exceeded")
            return
         end if
         ! Get enough to last a while.
         nbytes_added = min(nbytes_requested + GAP_BYTES_DFL, BUF_BYTES_MAX - current_size)

         allocate (character(len=current_size + nbytes_added) :: grown, stat=astat)
         if (astat /= 0) then
            call set_error(status, FM_MEMORY_FULL)
            return
         end if
         new_gap = t%gap_size + nbytes_added
         after = t%z_byte - t%gpt_byte
         if (t%gpt_byte > 1) grown(1:t%gpt_byte - 1) = t%beg(1:t%gpt_byte - 1)
         if (after > 0) then
            grown(t%gpt_byte + new_gap:t%gpt_byte + new_gap + after - 1) = &
               t%beg(t%gpt_byte + t%gap_size:t%gpt_byte + t%gap_size + after - 1)
         end if
         call move_alloc(grown, t%beg)
         t%gap_size = new_gap
      end associate
   end subroutine make_gap_larger

   !> Port of insdel.c:make_gap_smaller.
   subroutine make_gap_smaller(nbytes_requested, status)
      integer(int64), intent(in) :: nbytes_requested
      type(status_type), intent(out) :: status
      integer(int64) :: nbytes_removed, after, new_gap
      character(len=:), allocatable :: shrunk
      integer :: astat

      associate (t => buffers(cur)%text)
         nbytes_removed = nbytes_requested
         if (t%gap_size - nbytes_removed < GAP_BYTES_MIN) nbytes_removed = t%gap_size - GAP_BYTES_MIN
         if (nbytes_removed <= 0) return

         new_gap = t%gap_size - nbytes_removed
         allocate (character(len=len(t%beg, kind=int64) - nbytes_removed) :: shrunk, stat=astat)
         if (astat /= 0) then
            call set_error(status, FM_MEMORY_FULL)
            return
         end if
         after = t%z_byte - t%gpt_byte
         if (t%gpt_byte > 1) shrunk(1:t%gpt_byte - 1) = t%beg(1:t%gpt_byte - 1)
         if (after > 0) then
            shrunk(t%gpt_byte + new_gap:t%gpt_byte + new_gap + after - 1) = &
               t%beg(t%gpt_byte + t%gap_size:t%gpt_byte + t%gap_size + after - 1)
         end if
         call move_alloc(shrunk, t%beg)
         t%gap_size = new_gap
      end associate
   end subroutine make_gap_smaller

   ! ------------------------------------------------------------------
   ! Insertion
   ! ------------------------------------------------------------------

   !> Insert `string` (in the buffer's representation) before point.
   !> Port of insdel.c:insert.
   subroutine insert(string, status)
      character(len=*), intent(in) :: string
      type(status_type), intent(out) :: status

      call insert_text(string, .false., status)
   end subroutine insert

   !> Like `insert`, but markers at point end up after the text.
   !> Port of insdel.c:insert_before_markers.
   subroutine insert_before_markers(string, status)
      character(len=*), intent(in) :: string
      type(status_type), intent(out) :: status

      call insert_text(string, .true., status)
   end subroutine insert_before_markers

   subroutine insert_text(string, before_markers, status)
      character(len=*), intent(in) :: string
      logical, intent(in) :: before_markers
      type(status_type), intent(out) :: status
      integer(int64) :: nchars

      if (len(string) == 0) return
      call chars_in_text(string, nchars, status)
      if (.not. status%ok()) return
      call insert_1_both(string, nchars, len(string, kind=int64), .false., .true., &
                         before_markers, status)
   end subroutine insert_text

   !> Insert character `c` before point.  Port of insdel.c:insert_char.
   subroutine insert_char(c, status)
      integer(int32), intent(in) :: c
      type(status_type), intent(out) :: status
      character(len=MAX_MULTIBYTE_LENGTH) :: str
      integer(int32) :: n

      if (buffers(cur)%multibyte) then
         call char_string(c, str, n, status)
         if (.not. status%ok()) return
      else
         n = 1
         str(1:1) = char(iand(c, 255_int32))
      end if
      call insert(str(1:n), status)
   end subroutine insert_char

   !> Characters in `string` as the current buffer would count them.
   !> Port of character.c:chars_in_text.
   subroutine chars_in_text(string, nchars, status)
      character(len=*), intent(in) :: string
      integer(int64), intent(out) :: nchars
      type(status_type), intent(out) :: status

      if (.not. buffers(cur)%multibyte) then
         nchars = len(string, kind=int64)
      else
         call multibyte_chars_in_text(string, nchars, status)
      end if
   end subroutine chars_in_text

   !> Insert `nchars` characters occupying the `nbytes` bytes of `string`
   !> before point.  `prepare` runs the read-only check.  `inherit` (text
   !> properties) and `before_markers` take effect once intervals and
   !> markers exist.  Port of insdel.c:insert_1_both.
   subroutine insert_1_both(string, nchars_in, nbytes, inherit, prepare, before_markers, status)
      character(len=*), intent(in) :: string
      integer(int64), intent(in) :: nchars_in, nbytes
      logical, intent(in) :: inherit, prepare, before_markers
      type(status_type), intent(out) :: status
      integer(int64) :: nchars

      if (nchars_in == 0) return
      nchars = nchars_in
      if (.not. buffers(cur)%multibyte) nchars = nbytes

      if (prepare) then
         call prepare_to_modify_buffer(buffers(cur)%pt, buffers(cur)%pt, status)
         if (.not. status%ok()) return
      end if

      if (buffers(cur)%pt /= buffers(cur)%text%gpt) then
         call move_gap_both(buffers(cur)%pt, buffers(cur)%pt_byte)
      end if
      if (buffers(cur)%text%gap_size < nbytes) then
         call make_gap(nbytes - buffers(cur)%text%gap_size, status)
         if (.not. status%ok()) return
      end if

      associate (b => buffers(cur), t => buffers(cur)%text)
         call modiff_incr(t%modiff, nchars)
         t%chars_modiff = t%modiff

         t%beg(t%gpt_byte:t%gpt_byte + nbytes - 1) = string(1:nbytes)

         t%gap_size = t%gap_size - nbytes
         t%gpt = t%gpt + nchars
         b%zv = b%zv + nchars
         t%z = t%z + nchars
         t%gpt_byte = t%gpt_byte + nbytes
         b%zv_byte = b%zv_byte + nbytes
         t%z_byte = t%z_byte + nbytes

         ! The insert may have been in the unchanged region.
         if (t%z - t%gpt < t%end_unchanged) t%end_unchanged = t%z - t%gpt
      end associate

      call adjust_markers_for_insert(buffers(cur)%pt, buffers(cur)%pt_byte, &
                                     buffers(cur)%pt + nchars, buffers(cur)%pt_byte + nbytes, &
                                     before_markers)

      ! offset_intervals / set_text_properties (which use `inherit`) are
      ! pending; the no-op reference keeps the interface without a warning.
      if (inherit) continue

      call adjust_point(nchars, nbytes)
   end subroutine insert_1_both

   !> Relocate the current buffer's markers for an insertion spanning
   !> `from`..`to`.  Markers at the insertion point advance only if their
   !> insertion type is t or `before_markers` is set.
   !> Port of insdel.c:adjust_markers_for_insert.
   subroutine adjust_markers_for_insert(from, from_byte, to, to_byte, before_markers)
      integer(int64), intent(in) :: from, from_byte, to, to_byte
      logical, intent(in) :: before_markers
      integer(int64) :: m, nchars, nbytes

      nchars = to - from
      nbytes = to_byte - from_byte
      m = buffers(cur)%text%markers
      do while (m /= 0)
         associate (mk => markers(m))
            if (mk%bytepos == from_byte) then
               if (mk%insertion_type .or. before_markers) then
                  mk%bytepos = to_byte
                  mk%charpos = to
               end if
            else if (mk%bytepos > from_byte) then
               mk%bytepos = mk%bytepos + nbytes
               mk%charpos = mk%charpos + nchars
            end if
            m = mk%next
         end associate
      end do
   end subroutine adjust_markers_for_insert

   !> Relocate the current buffer's markers for a deletion of
   !> `from`..`to`: markers inside move to `from`, markers after shift.
   !> Port of insdel.c:adjust_markers_for_delete.
   subroutine adjust_markers_for_delete(from, from_byte, to, to_byte)
      integer(int64), intent(in) :: from, from_byte, to, to_byte
      integer(int64) :: m

      m = buffers(cur)%text%markers
      do while (m /= 0)
         associate (mk => markers(m))
            if (mk%charpos > to) then
               mk%charpos = mk%charpos - (to - from)
               mk%bytepos = mk%bytepos - (to_byte - from_byte)
            else if (mk%charpos > from) then
               mk%charpos = from
               mk%bytepos = from_byte
            end if
            m = mk%next
         end associate
      end do
   end subroutine adjust_markers_for_delete

   !> Move point by `nchars`/`nbytes`.  Port of insdel.c:adjust_point.
   subroutine adjust_point(nchars, nbytes)
      integer(int64), intent(in) :: nchars, nbytes

      buffers(cur)%pt = buffers(cur)%pt + nchars
      buffers(cur)%pt_byte = buffers(cur)%pt_byte + nbytes
   end subroutine adjust_point

   ! ------------------------------------------------------------------
   ! Deletion
   ! ------------------------------------------------------------------

   !> Delete characters `from` up to `to`, clamped to the accessible
   !> region; nothing if `to <= from`.  Port of insdel.c:del_range.
   subroutine del_range(from, to, status)
      integer(int64), intent(in) :: from, to
      type(status_type), intent(out) :: status

      call del_range_1(from, to, .true., status)
   end subroutine del_range

   !> Like `del_range`; `prepare` runs the read-only check.  With
   !> `deleted` present, it receives the deleted text.
   !> Port of insdel.c:del_range_1.
   subroutine del_range_1(from_in, to_in, prepare, status, deleted)
      integer(int64), intent(in) :: from_in, to_in
      logical, intent(in) :: prepare
      type(status_type), intent(out) :: status
      character(len=:), allocatable, intent(out), optional :: deleted
      integer(int64) :: from, to, from_byte, to_byte

      from = max(from_in, buffers(cur)%begv)
      to = min(to_in, buffers(cur)%zv)
      if (to <= from) then
         if (present(deleted)) deleted = ""
         return
      end if

      if (prepare) then
         call prepare_to_modify_buffer(from, to, status)
         if (.not. status%ok()) return
      end if

      from_byte = char_to_byte(from)
      to_byte = char_to_byte(to)
      call del_range_2(from, from_byte, to, to_byte, deleted)
   end subroutine del_range_1

   !> Like `del_range_1`, with byte positions.  Port of insdel.c:del_range_byte.
   subroutine del_range_byte(from_byte_in, to_byte_in, status)
      integer(int64), intent(in) :: from_byte_in, to_byte_in
      type(status_type), intent(out) :: status
      integer(int64) :: from, to, from_byte, to_byte

      from_byte = max(from_byte_in, buffers(cur)%begv_byte)
      to_byte = min(to_byte_in, buffers(cur)%zv_byte)
      if (to_byte <= from_byte) return

      from = byte_to_char(from_byte)
      to = byte_to_char(to_byte)
      call prepare_to_modify_buffer(from, to, status)
      if (.not. status%ok()) return
      call del_range_2(from, from_byte, to, to_byte)
   end subroutine del_range_byte

   !> Like `del_range_1`, with both kinds of position given.
   !> Port of insdel.c:del_range_both.
   subroutine del_range_both(from_in, from_byte_in, to_in, to_byte_in, prepare, status)
      integer(int64), intent(in) :: from_in, from_byte_in, to_in, to_byte_in
      logical, intent(in) :: prepare
      type(status_type), intent(out) :: status
      integer(int64) :: from, to, from_byte, to_byte

      from_byte = max(from_byte_in, buffers(cur)%begv_byte)
      to_byte = min(to_byte_in, buffers(cur)%zv_byte)
      if (to_byte <= from_byte) return
      from = max(from_in, buffers(cur)%begv)
      to = min(to_in, buffers(cur)%zv)

      if (prepare) then
         call prepare_to_modify_buffer(from, to, status)
         if (.not. status%ok()) return
      end if
      call del_range_2(from, from_byte, to, to_byte)
   end subroutine del_range_both

   !> Delete the text between two valid positions, given as both char and
   !> byte positions.  With `deleted` present, it receives the deleted text.
   !> Port of insdel.c:del_range_2.
   subroutine del_range_2(from, from_byte, to, to_byte, deleted)
      integer(int64), intent(in) :: from, from_byte, to, to_byte
      character(len=:), allocatable, intent(out), optional :: deleted
      integer(int64) :: nchars_del, nbytes_del

      nchars_del = to - from
      nbytes_del = to_byte - from_byte

      ! Make sure the gap is somewhere in or next to what we are deleting.
      if (from > buffers(cur)%text%gpt) call gap_right(from, from_byte)
      if (to < buffers(cur)%text%gpt) call gap_left(to, to_byte, .false.)

      if (present(deleted)) deleted = buf_bytes(buffers(cur), from_byte, to_byte)

      ! record_delete: pending (no undo yet).
      ! Relocate all markers pointing into the new, larger gap.
      call adjust_markers_for_delete(from, from_byte, to, to_byte)

      associate (b => buffers(cur), t => buffers(cur)%text)
         call modiff_incr(t%modiff, nchars_del)
         t%chars_modiff = t%modiff

         ! Relocate point as if it were a marker.
         if (from < b%pt) then
            b%pt = b%pt + (from - min(b%pt, to))
            b%pt_byte = b%pt_byte + (from_byte - min(b%pt_byte, to_byte))
         end if

         ! offset_intervals: pending.

         t%gap_size = t%gap_size + nbytes_del
         b%zv = b%zv - nchars_del
         t%z = t%z - nchars_del
         b%zv_byte = b%zv_byte - nbytes_del
         t%z_byte = t%z_byte - nbytes_del
         t%gpt = from
         t%gpt_byte = from_byte

         if (t%gpt - BEG < t%beg_unchanged) t%beg_unchanged = t%gpt - BEG
         if (t%z - t%gpt < t%end_unchanged) t%end_unchanged = t%z - t%gpt
      end associate
   end subroutine del_range_2

   ! ------------------------------------------------------------------
   ! Modification bookkeeping
   ! ------------------------------------------------------------------

   !> Announce a change to characters `start`..`end` that does not insert
   !> or delete (e.g. case changes).  Port of insdel.c:modify_text.
   subroutine modify_text(start, end, status)
      integer(int64), intent(in) :: start, end
      type(status_type), intent(out) :: status

      call prepare_to_modify_buffer(start, end, status)
      if (.not. status%ok()) return
      call buf_compute_unchanged(buffers(cur), start - 1, end)
      associate (t => buffers(cur)%text)
         call modiff_incr(t%modiff, end - start)
         t%chars_modiff = t%modiff
      end associate
   end subroutine modify_text

   !> Check that the region may be modified.  Upstream also verifies
   !> read-only text properties, locks the visited file, saves the region
   !> for `select-active-regions' and runs before-change hooks; only the
   !> read-only check exists so far.
   !> Port of insdel.c:prepare_to_modify_buffer(_1).
   subroutine prepare_to_modify_buffer(start, end, status)
      integer(int64), intent(in) :: start, end
      type(status_type), intent(out) :: status

      call barf_if_buffer_read_only(status)
      if (.not. status%ok()) return
      buffers(cur)%text%redisplay = .true.
      ! start/end feed verify_interval_modification once intervals exist.
      if (start > end) continue
   end subroutine prepare_to_modify_buffer

end module fm_insdel
