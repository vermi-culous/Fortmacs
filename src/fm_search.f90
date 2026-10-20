! fm_search.f90 -- newline scanning (the start of the search.c port).
! Ported from GNU Emacs 31.1 src/search.c.
!
! Copyright (C) 1985-1987, 1993-1994, 1997-1999, 2001-2026 Free Software
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

!> Finding newlines in the current buffer.
!>
!> Upstream's find_newline consults a per-buffer newline cache
!> (region-cache.c) and scans contiguous runs with memchr/memrchr.  Here
!> each run on either side of the gap is scanned with INDEX; the cache is
!> deferred until profiling shows a need.  Byte positions are always
!> known, so the "-1 = unknown" convention is replaced by optional
!> arguments.  String/regexp searching comes later.
module fm_search
   use, intrinsic :: iso_fortran_env, only: int64
   use fm_buffer, only: buffers, cur, buf_prev_char_len, temp_set_point_both
   use fm_marker, only: char_to_byte, byte_to_char
   implicit none(type, external)
   private

   public :: find_newline, scan_newline, scan_newline_from_point, &
             find_newline_no_quit, find_before_next_newline

   character(len=*), parameter :: newline = achar(10)

contains

   !> Byte position of the first newline in [from_byte, to_byte) of the
   !> current buffer, or 0.
   integer(int64) function next_newline_byte(from_byte, to_byte) result(found)
      integer(int64), intent(in) :: from_byte, to_byte
      integer(int64) :: a, b, k

      found = 0
      associate (t => buffers(cur)%text)
         ! Before the gap: string index = byte position.
         a = from_byte
         b = min(to_byte, t%gpt_byte) - 1
         if (a <= b) then
            k = index(t%beg(a:b), newline, kind=int64)
            if (k > 0) then
               found = a + k - 1
               return
            end if
         end if
         ! After the gap: string index = byte position + gap_size.
         a = max(from_byte, t%gpt_byte)
         b = to_byte - 1
         if (a <= b) then
            k = index(t%beg(a + t%gap_size:b + t%gap_size), newline, kind=int64)
            if (k > 0) found = a + k - 1
         end if
      end associate
   end function next_newline_byte

   !> Byte position of the last newline in [from_byte, to_byte), or 0.
   integer(int64) function prev_newline_byte(from_byte, to_byte) result(found)
      integer(int64), intent(in) :: from_byte, to_byte
      integer(int64) :: a, b, k

      found = 0
      associate (t => buffers(cur)%text)
         a = max(from_byte, t%gpt_byte)
         b = to_byte - 1
         if (a <= b) then
            k = index(t%beg(a + t%gap_size:b + t%gap_size), newline, back=.true., kind=int64)
            if (k > 0) then
               found = a + k - 1
               return
            end if
         end if
         a = from_byte
         b = min(to_byte, t%gpt_byte) - 1
         if (a <= b) then
            k = index(t%beg(a:b), newline, back=.true., kind=int64)
            if (k > 0) found = a + k - 1
         end if
      end associate
   end function prev_newline_byte

   !> Search for `count` newlines from `start` toward `end` (forward if
   !> count > 0, backward if count < 0; `end` defaults to ZV or BEGV).
   !> Returns the position just after the count'th newline found -- also
   !> when searching backward -- or `end` if there are fewer.  `counted`
   !> receives the number found (negated when backward); `bytepos` the
   !> byte position of the result.  Port of search.c:find_newline.
   integer(int64) function find_newline(start, end, count, counted, bytepos, start_byte, end_byte) &
      result(pos)
      integer(int64), intent(in) :: start, count
      integer(int64), intent(in), optional :: end, start_byte, end_byte
      integer(int64), intent(out), optional :: counted, bytepos
      integer(int64) :: lim, lim_byte, pos_byte, nl, left

      associate (b => buffers(cur))
         if (present(end)) then
            lim = end
            if (present(end_byte)) then
               lim_byte = end_byte
            else
               lim_byte = char_to_byte(end)
            end if
         else if (count > 0) then
            lim = b%zv
            lim_byte = b%zv_byte
         else
            lim = b%begv
            lim_byte = b%begv_byte
         end if
      end associate
      if (present(start_byte)) then
         pos_byte = start_byte
      else
         pos_byte = char_to_byte(start)
      end if

      left = count
      if (count > 0) then
         do while (pos_byte < lim_byte)
            nl = next_newline_byte(pos_byte, lim_byte)
            if (nl == 0) exit
            pos_byte = nl + 1
            left = left - 1
            if (left == 0) then
               call finish(byte_to_char(pos_byte), pos_byte)
               return
            end if
         end do
      else if (count < 0) then
         do while (pos_byte > lim_byte)
            nl = prev_newline_byte(lim_byte, pos_byte)
            if (nl == 0) exit
            left = left + 1
            if (left >= 0) then
               call finish(byte_to_char(nl + 1), nl + 1)
               return
            end if
            pos_byte = nl
         end do
      else
         ! count == 0: "do anything you please"; stay put.
         lim = start
         lim_byte = pos_byte
      end if

      ! Fewer than COUNT newlines: stop at the limit.
      call finish(lim, lim_byte)

   contains

      subroutine finish(p, pbyte)
         integer(int64), intent(in) :: p, pbyte

         pos = p
         if (present(bytepos)) bytepos = pbyte
         if (present(counted)) counted = count - left
      end subroutine finish

   end function find_newline

   !> Move point past `count` line boundaries from `start`, or to `limit`
   !> if there are fewer.  Port of search.c:scan_newline.
   subroutine scan_newline(start, start_byte, limit, limit_byte, count)
      integer(int64), intent(in) :: start, start_byte, limit, limit_byte, count
      integer(int64) :: charpos, bytepos, counted

      charpos = find_newline(start, limit, count, counted, bytepos, start_byte, limit_byte)
      if (counted /= count) then
         call temp_set_point_both(buffers(cur), limit, limit_byte)
      else
         call temp_set_point_both(buffers(cur), charpos, bytepos)
      end if
   end subroutine scan_newline

   !> Scan `count` lines from point (count <= 0: to the start of the line
   !> |count| lines back) without moving point.  Returns how many newlines
   !> were found.  Port of search.c:scan_newline_from_point.
   integer(int64) function scan_newline_from_point(count, charpos, bytepos) result(counted)
      integer(int64), intent(in) :: count
      integer(int64), intent(out) :: charpos, bytepos

      associate (b => buffers(cur))
         if (count <= 0) then
            charpos = find_newline(b%pt, b%begv, count - 1, counted, bytepos, b%pt_byte, b%begv_byte)
         else
            charpos = find_newline(b%pt, b%zv, count, counted, bytepos, b%pt_byte, b%zv_byte)
         end if
      end associate
   end function scan_newline_from_point

   !> Port of search.c:find_newline_no_quit.
   integer(int64) function find_newline_no_quit(from, frombyte, cnt, bytepos)
      integer(int64), intent(in) :: from, frombyte, cnt
      integer(int64), intent(out), optional :: bytepos

      find_newline_no_quit = find_newline(from, count=cnt, bytepos=bytepos, start_byte=frombyte)
   end function find_newline_no_quit

   !> Like `find_newline`, but returns the position before the newline,
   !> searching no further than `to` (default: ZV/BEGV).
   !> Port of search.c:find_before_next_newline.
   integer(int64) function find_before_next_newline(from, cnt, bytepos, to) result(pos)
      integer(int64), intent(in) :: from, cnt
      integer(int64), intent(out), optional :: bytepos
      integer(int64), intent(in), optional :: to
      integer(int64) :: counted, pbyte

      pos = find_newline(from, to, cnt, counted, pbyte)
      if (counted == cnt) then
         pos = pos - 1
         pbyte = pbyte - buf_prev_char_len(buffers(cur), pbyte)
      end if
      if (present(bytepos)) bytepos = pbyte
   end function find_before_next_newline

end module fm_search
