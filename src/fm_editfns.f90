! fm_editfns.f90 -- point, narrowing, and reading and editing buffer text.
! Ported from GNU Emacs 31.1 src/editfns.c.
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

!> Editing primitives on the current buffer.
!>
!> Lisp functions that return nil for "no such position" take a `found`
!> (or similar) logical here.  Text comes back as byte strings in the
!> buffer's representation.
!>
!> Point is set with `temp_set_point_both`: upstream's SET_PT also runs
!> text-property hooks (`point-entered` etc.), which arrive with
!> intervals.  Labeled restrictions (`with-restriction`) are deferred, as
!> are `format`, fields, `transpose-regions`, `compare-buffer-substrings`
!> and `subst-char-in-region` (they need case tables, intervals or undo).
module fm_editfns
   use, intrinsic :: iso_fortran_env, only: int32, int64
   use fm_error, only: status_type, set_error, FM_ARGS_OUT_OF_RANGE, &
                       FM_WRONG_TYPE_ARGUMENT, FM_GENERIC_ERROR
   use fm_handle, only: handle_type
   use fm_character, only: char_head_p, char_valid_p, char_string, &
                           MAX_MULTIBYTE_LENGTH
   use fm_buffer, only: buffers, cur, BEG, BEG_BYTE, BUF_BYTES_MAX, &
                        buf_fetch_byte, buf_fetch_char, buf_prev_char_len, &
                        buf_bytes, temp_set_point_both, current_buffer, &
                        resolve_buffer
   use fm_marker, only: char_to_byte, byte_to_char, buf_charpos_to_bytepos, &
                        markers, resolve_marker, make_marker, set_marker_both
   use fm_insdel, only: insert, del_range, del_range_1
   use fm_search, only: scan_newline_from_point, find_before_next_newline
   implicit none(type, external)
   private

   public :: point, point_min, point_max, buffer_size, buffer_size_of, &
             gap_position, gap_size
   public :: goto_char, goto_char_marker, set_point_from_marker
   public :: point_marker, point_min_marker, point_max_marker
   public :: position_bytes, byte_to_position
   public :: following_char, preceding_char, bobp, eobp, bolp, eolp, &
             char_after, char_before
   public :: insert_char_n, narrow_to_region, widen, validate_region
   public :: buffer_substring, buffer_string, delete_region, &
             delete_and_extract_region
   public :: pos_bol, pos_eol, line_beginning_position, line_end_position

contains

   ! ------------------------------------------------------------------
   ! Positions
   ! ------------------------------------------------------------------

   !> Port of editfns.c:Fpoint.
   integer(int64) function point()
      point = buffers(cur)%pt
   end function point

   !> Port of editfns.c:Fpoint_min.
   integer(int64) function point_min()
      point_min = buffers(cur)%begv
   end function point_min

   !> Port of editfns.c:Fpoint_max.
   integer(int64) function point_max()
      point_max = buffers(cur)%zv
   end function point_max

   !> Characters in the current buffer, ignoring narrowing.
   !> Port of editfns.c:Fbuffer_size.
   integer(int64) function buffer_size()
      buffer_size = buffers(cur)%text%z - BEG
   end function buffer_size

   !> Like `buffer_size`, for buffer `h`.  Port of editfns.c:Fbuffer_size.
   subroutine buffer_size_of(h, size, status)
      type(handle_type), intent(in) :: h
      integer(int64), intent(out) :: size
      type(status_type), intent(out) :: status
      integer(int64) :: i

      size = 0
      call resolve_buffer(h, i, status)
      if (.not. status%ok()) return
      size = buffers(i)%text%z - BEG
   end subroutine buffer_size_of

   !> Port of editfns.c:Fgap_position.
   integer(int64) function gap_position()
      gap_position = buffers(cur)%text%gpt
   end function gap_position

   !> Port of editfns.c:Fgap_size.
   integer(int64) function gap_size()
      gap_size = buffers(cur)%text%gap_size
   end function gap_size

   !> Set point to `position`, clipped to the accessible region.
   !> Port of editfns.c:Fgoto_char (integer POSITION).
   subroutine goto_char(position)
      integer(int64), intent(in) :: position
      integer(int64) :: pos

      pos = max(buffers(cur)%begv, min(position, buffers(cur)%zv))
      call temp_set_point_both(buffers(cur), pos, char_to_byte(pos))
   end subroutine goto_char

   !> Set point to marker `m`.  Port of editfns.c:Fgoto_char (marker
   !> POSITION), i.e. intervals.c:set_point_from_marker.
   subroutine goto_char_marker(m, status)
      type(handle_type), intent(in) :: m
      type(status_type), intent(out) :: status

      call set_point_from_marker(m, status)
   end subroutine goto_char_marker

   !> Port of intervals.c:set_point_from_marker.  The marker's byte
   !> position is only trusted if it is in the current buffer.
   subroutine set_point_from_marker(m, status)
      type(handle_type), intent(in) :: m
      type(status_type), intent(out) :: status
      integer(int64) :: i, charpos, bytepos

      call resolve_marker(m, i, status)
      if (.not. status%ok()) return
      if (markers(i)%buffer == 0) then
         call set_error(status, FM_GENERIC_ERROR, "Marker does not point anywhere")
         return
      end if
      associate (b => buffers(cur))
         charpos = max(b%begv, min(markers(i)%charpos, b%zv))
         if (markers(i)%buffer /= cur) then
            bytepos = buf_charpos_to_bytepos(cur, charpos)
         else
            bytepos = max(b%begv_byte, min(markers(i)%bytepos, b%zv_byte))
         end if
         call temp_set_point_both(b, charpos, bytepos)
      end associate
   end subroutine set_point_from_marker

   !> A new marker at point; the caller frees it.
   !> Port of editfns.c:Fpoint_marker.
   subroutine point_marker(m)
      type(handle_type), intent(out) :: m

      call new_marker_at(buffers(cur)%pt, buffers(cur)%pt_byte, m)
   end subroutine point_marker

   !> Port of editfns.c:Fpoint_min_marker.
   subroutine point_min_marker(m)
      type(handle_type), intent(out) :: m

      call new_marker_at(buffers(cur)%begv, buffers(cur)%begv_byte, m)
   end subroutine point_min_marker

   !> Port of editfns.c:Fpoint_max_marker.
   subroutine point_max_marker(m)
      type(handle_type), intent(out) :: m

      call new_marker_at(buffers(cur)%zv, buffers(cur)%zv_byte, m)
   end subroutine point_max_marker

   !> Port of alloc.c:build_marker (current buffer).
   subroutine new_marker_at(charpos, bytepos, m)
      integer(int64), intent(in) :: charpos, bytepos
      type(handle_type), intent(out) :: m
      type(status_type) :: st

      call make_marker(m)
      call set_marker_both(m, current_buffer(), charpos, bytepos, st)
   end subroutine new_marker_at

   !> Byte position of character `position`; `found` is false when it is
   !> outside the buffer.  Port of editfns.c:Fposition_bytes.
   subroutine position_bytes(position, bytepos, found)
      integer(int64), intent(in) :: position
      integer(int64), intent(out) :: bytepos
      logical, intent(out) :: found

      bytepos = 0
      found = BEG <= position .and. position <= buffers(cur)%text%z
      if (found) bytepos = char_to_byte(position)
   end subroutine position_bytes

   !> Character position containing byte `bytepos`; `found` is false when
   !> it is outside the buffer.  Port of editfns.c:Fbyte_to_position.
   subroutine byte_to_position(bytepos, position, found)
      integer(int64), intent(in) :: bytepos
      integer(int64), intent(out) :: position
      logical, intent(out) :: found
      integer(int64) :: pos_byte

      position = 0
      found = BEG_BYTE <= bytepos .and. bytepos <= buffers(cur)%text%z_byte
      if (.not. found) return
      pos_byte = bytepos
      if (buffers(cur)%text%z /= buffers(cur)%text%z_byte) then
         ! Back up to the start of the character containing BYTEPOS.
         do while (pos_byte < buffers(cur)%text%z_byte)
            if (char_head_p(buf_fetch_byte(buffers(cur), pos_byte))) exit
            pos_byte = pos_byte - 1
         end do
      end if
      position = byte_to_char(pos_byte)
   end subroutine byte_to_position

   ! ------------------------------------------------------------------
   ! Characters around point
   ! ------------------------------------------------------------------

   !> Character after point, or 0 at the end of the accessible region.
   !> Port of editfns.c:Ffollowing_char.
   integer(int32) function following_char()
      following_char = 0
      if (buffers(cur)%pt < buffers(cur)%zv) &
         following_char = buf_fetch_char(buffers(cur), buffers(cur)%pt_byte)
   end function following_char

   !> Character before point, or 0 at the start of the accessible region.
   !> Port of editfns.c:Fprevious_char.
   integer(int32) function preceding_char()
      integer(int64) :: pos

      preceding_char = 0
      associate (b => buffers(cur))
         if (b%pt <= b%begv) return
         if (b%multibyte) then
            pos = b%pt_byte - buf_prev_char_len(b, b%pt_byte)
            preceding_char = buf_fetch_char(b, pos)
         else
            preceding_char = buf_fetch_byte(b, b%pt_byte - 1)
         end if
      end associate
   end function preceding_char

   !> Port of editfns.c:Fbobp.
   logical function bobp()
      bobp = buffers(cur)%pt == buffers(cur)%begv
   end function bobp

   !> Port of editfns.c:Feobp.
   logical function eobp()
      eobp = buffers(cur)%pt == buffers(cur)%zv
   end function eobp

   !> Port of editfns.c:Fbolp.
   logical function bolp()
      associate (b => buffers(cur))
         bolp = b%pt == b%begv
         if (.not. bolp) bolp = buf_fetch_byte(b, b%pt_byte - 1) == 10
      end associate
   end function bolp

   !> Port of editfns.c:Feolp.
   logical function eolp()
      associate (b => buffers(cur))
         eolp = b%pt == b%zv
         if (.not. eolp) eolp = buf_fetch_byte(b, b%pt_byte) == 10
      end associate
   end function eolp

   !> Character at `pos` (default: point); `found` is false outside the
   !> accessible region.  Port of editfns.c:Fchar_after (integer POS).
   subroutine char_after(c, found, pos)
      integer(int32), intent(out) :: c
      logical, intent(out) :: found
      integer(int64), intent(in), optional :: pos
      integer(int64) :: pos_byte

      c = 0
      associate (b => buffers(cur))
         if (.not. present(pos)) then
            pos_byte = b%pt_byte
            found = b%begv_byte <= pos_byte .and. pos_byte < b%zv_byte
         else
            found = b%begv <= pos .and. pos < b%zv
            if (found) pos_byte = char_to_byte(pos)
         end if
         if (found) c = buf_fetch_char(b, pos_byte)
      end associate
   end subroutine char_after

   !> Character before `pos` (default: point); `found` is false outside
   !> the accessible region.  Port of editfns.c:Fchar_before (integer POS).
   subroutine char_before(c, found, pos)
      integer(int32), intent(out) :: c
      logical, intent(out) :: found
      integer(int64), intent(in), optional :: pos
      integer(int64) :: p, pos_byte

      c = 0
      associate (b => buffers(cur))
         p = b%pt
         if (present(pos)) p = pos
         found = b%begv < p .and. p <= b%zv
         if (.not. found) return
         pos_byte = char_to_byte(p)
         if (b%multibyte) then
            pos_byte = pos_byte - buf_prev_char_len(b, pos_byte)
            c = buf_fetch_char(b, pos_byte)
         else
            c = buf_fetch_byte(b, pos_byte - 1)
         end if
      end associate
   end subroutine char_before

   ! ------------------------------------------------------------------
   ! Inserting, narrowing, extracting, deleting
   ! ------------------------------------------------------------------

   !> Insert `count` copies of character `c` (nothing if count <= 0).
   !> Port of editfns.c:Finsert_char (without INHERIT).
   subroutine insert_char_n(c, count, status)
      integer(int32), intent(in) :: c
      integer(int64), intent(in) :: count
      type(status_type), intent(out) :: status
      character(len=MAX_MULTIBYTE_LENGTH) :: str
      character(len=:), allocatable :: block
      integer(int32) :: clen
      integer(int64) :: n, chunk, reps

      if (.not. char_valid_p(c)) then
         call set_error(status, FM_WRONG_TYPE_ARGUMENT, "characterp")
         return
      end if
      if (buffers(cur)%multibyte) then
         call char_string(c, str, clen, status)
         if (.not. status%ok()) return
      else
         str(1:1) = char(iand(c, 255_int32))
         clen = 1
      end if
      if (count <= 0) return
      if (BUF_BYTES_MAX/clen < count) then
         call set_error(status, FM_GENERIC_ERROR, "Maximum buffer size exceeded")
         return
      end if

      ! Insert in chunks of at most ~4000 bytes, as upstream.
      reps = min(count, 4000_int64/clen)
      block = repeat(str(1:clen), reps)
      n = count
      do while (n > 0)
         chunk = min(n, reps)
         call insert(block(1:chunk*clen), status)
         if (.not. status%ok()) return
         n = n - chunk
      end do
   end subroutine insert_char_n

   !> Restrict editing to `start`..`end` (in either order); point moves
   !> into the region.  Port of editfns.c:Fnarrow_to_region.
   subroutine narrow_to_region(start, end, status)
      integer(int64), intent(in) :: start, end
      type(status_type), intent(out) :: status
      integer(int64) :: s, e

      s = min(start, end)
      e = max(start, end)
      associate (b => buffers(cur))
         if (.not. (BEG <= s .and. e <= b%text%z)) then
            call range_error(status, start, end)
            return
         end if
         b%begv = s
         b%begv_byte = char_to_byte(s)
         b%zv = e
         b%zv_byte = char_to_byte(e)
         if (b%pt < s) call temp_set_point_both(b, s, b%begv_byte)
         if (e < b%pt) call temp_set_point_both(b, e, b%zv_byte)
      end associate
   end subroutine narrow_to_region

   !> Remove narrowing.  Port of editfns.c:Fwiden.
   subroutine widen()
      associate (b => buffers(cur))
         b%begv = BEG
         b%begv_byte = BEG_BYTE
         b%zv = b%text%z
         b%zv_byte = b%text%z_byte
      end associate
   end subroutine widen

   !> Order `start`, `end` and check they lie in the accessible region.
   !> Port of buffer.c:validate_region.
   subroutine validate_region(start, end, status)
      integer(int64), intent(inout) :: start, end
      type(status_type), intent(out) :: status
      integer(int64) :: s, e

      s = min(start, end)
      e = max(start, end)
      if (.not. (buffers(cur)%begv <= s .and. e <= buffers(cur)%zv)) then
         call range_error(status, start, end)
         return
      end if
      start = s
      end = e
   end subroutine validate_region

   subroutine range_error(status, a, b)
      type(status_type), intent(inout) :: status
      integer(int64), intent(in) :: a, b
      character(len=24) :: sa, sb

      write (sa, '(i0)') a
      write (sb, '(i0)') b
      call set_error(status, FM_ARGS_OUT_OF_RANGE, trim(sa)//", "//trim(sb))
   end subroutine range_error

   !> Text between `start` and `end` (either order).
   !> Port of editfns.c:Fbuffer_substring (text properties pending).
   subroutine buffer_substring(start, end, text, status)
      integer(int64), intent(in) :: start, end
      character(len=:), allocatable, intent(out) :: text
      type(status_type), intent(out) :: status
      integer(int64) :: s, e

      s = start
      e = end
      call validate_region(s, e, status)
      if (.not. status%ok()) return
      text = buf_bytes(buffers(cur), char_to_byte(s), char_to_byte(e))
   end subroutine buffer_substring

   !> The accessible text.  Port of editfns.c:Fbuffer_string.
   function buffer_string() result(text)
      character(len=:), allocatable :: text

      text = buf_bytes(buffers(cur), buffers(cur)%begv_byte, buffers(cur)%zv_byte)
   end function buffer_string

   !> Delete the text between `start` and `end` (either order).
   !> Port of editfns.c:Fdelete_region.
   subroutine delete_region(start, end, status)
      integer(int64), intent(in) :: start, end
      type(status_type), intent(out) :: status
      integer(int64) :: s, e

      s = start
      e = end
      call validate_region(s, e, status)
      if (.not. status%ok()) return
      call del_range(s, e, status)
   end subroutine delete_region

   !> Delete the text between `start` and `end` and return it.
   !> Port of editfns.c:Fdelete_and_extract_region.
   subroutine delete_and_extract_region(start, end, text, status)
      integer(int64), intent(in) :: start, end
      character(len=:), allocatable, intent(out) :: text
      type(status_type), intent(out) :: status
      integer(int64) :: s, e

      text = ""
      s = start
      e = end
      call validate_region(s, e, status)
      if (.not. status%ok()) return
      if (s == e) return
      call del_range_1(s, e, .true., status, text)
   end subroutine delete_and_extract_region

   ! ------------------------------------------------------------------
   ! Line positions
   ! ------------------------------------------------------------------

   !> Start of the current line, after moving `n` - 1 lines (default 1);
   !> `count` receives the number of lines scanned (n - 1).
   !> Port of editfns.c:bol.
   integer(int64) function bol(n, count)
      integer(int64), intent(in), optional :: n
      integer(int64), intent(out), optional :: count
      integer(int64) :: cnt, charpos, bytepos, counted

      cnt = 0
      if (present(n)) cnt = max(-BUF_BYTES_MAX, min(n - 1, BUF_BYTES_MAX))
      if (present(count)) count = cnt
      counted = scan_newline_from_point(cnt, charpos, bytepos)
      bol = charpos
   end function bol

   !> Position of the first character on the current line (after moving
   !> `n` - 1 lines first).  Does not move point.  Port of editfns.c:Fpos_bol.
   integer(int64) function pos_bol(n)
      integer(int64), intent(in), optional :: n

      pos_bol = bol(n)
   end function pos_bol

   !> Like `pos_bol`; upstream also constrains the result to the current
   !> field, which needs text properties (pending).
   !> Port of editfns.c:Fline_beginning_position.
   integer(int64) function line_beginning_position(n)
      integer(int64), intent(in), optional :: n

      line_beginning_position = bol(n)
   end function line_beginning_position

   !> Port of editfns.c:eol.
   integer(int64) function eol(n)
      integer(int64), intent(in), optional :: n
      integer(int64) :: cnt

      cnt = 1
      if (present(n)) cnt = max(-BUF_BYTES_MAX, min(n, BUF_BYTES_MAX))
      if (cnt <= 0) cnt = cnt - 1
      eol = find_before_next_newline(buffers(cur)%pt, cnt)
   end function eol

   !> Position of the last character on the current line (after moving
   !> `n` - 1 lines first).  Does not move point.  Port of editfns.c:Fpos_eol.
   integer(int64) function pos_eol(n)
      integer(int64), intent(in), optional :: n

      pos_eol = eol(n)
   end function pos_eol

   !> Like `pos_eol`, without the field constraint (pending).
   !> Port of editfns.c:Fline_end_position.
   integer(int64) function line_end_position(n)
      integer(int64), intent(in), optional :: n

      line_end_position = eol(n)
   end function line_end_position

end module fm_editfns
