! fm_cmds.f90 -- simple built-in editing commands.
! Ported from GNU Emacs 31.1 src/cmds.c.
!
! Copyright (C) 1985, 1993-1998, 2001-2026 Free Software Foundation, Inc.
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

!> Motion and deletion commands on the current buffer.
!>
!> Deferred: `self-insert-command` (needs the command loop's last event,
!> abbrevs, auto-fill and overwrite mode), the kill-ring path of
!> `delete-char`, undo amalgamation, and intangible/invisible text.
module fm_cmds
   use, intrinsic :: iso_fortran_env, only: int64
   use fm_error, only: status_type, set_error, FM_BEGINNING_OF_BUFFER, &
                       FM_END_OF_BUFFER
   use fm_buffer, only: buffers, cur, BUF_BYTES_MAX, buf_fetch_byte, &
                        temp_set_point_both
   use fm_search, only: scan_newline_from_point
   use fm_insdel, only: del_range
   use fm_editfns, only: line_beginning_position, line_end_position, goto_char
   implicit none(type, external)
   private

   public :: forward_char, backward_char, forward_line, beginning_of_line, &
             end_of_line, delete_char

contains

   !> Move point `n` characters (backward if not `forward`), stopping at
   !> the accessible region's edge with beginning-/end-of-buffer.
   !> Port of cmds.c:move_point.
   subroutine move_point(n, forward, status)
      integer(int64), intent(in) :: n
      logical, intent(in) :: forward
      type(status_type), intent(out) :: status
      integer(int64) :: new_point

      if (forward) then
         new_point = buffers(cur)%pt + n
      else
         new_point = buffers(cur)%pt - n
      end if
      if (new_point < buffers(cur)%begv) then
         call goto_char(buffers(cur)%begv)
         call set_error(status, FM_BEGINNING_OF_BUFFER)
      else if (new_point > buffers(cur)%zv) then
         call goto_char(buffers(cur)%zv)
         call set_error(status, FM_END_OF_BUFFER)
      else
         call goto_char(new_point)
      end if
   end subroutine move_point

   !> Port of cmds.c:Fforward_char (N defaults to 1).
   subroutine forward_char(status, n)
      type(status_type), intent(out) :: status
      integer(int64), intent(in), optional :: n

      if (present(n)) then
         call move_point(n, .true., status)
      else
         call move_point(1_int64, .true., status)
      end if
   end subroutine forward_char

   !> Port of cmds.c:Fbackward_char (N defaults to 1).
   subroutine backward_char(status, n)
      type(status_type), intent(out) :: status
      integer(int64), intent(in), optional :: n

      if (present(n)) then
         call move_point(n, .false., status)
      else
         call move_point(1_int64, .false., status)
      end if
   end subroutine backward_char

   !> Move to the start of the line `n` lines away (default 1) and return
   !> the count of lines left to move (see the forward-line docstring:
   !> a non-empty last line counts as moved).  Port of cmds.c:Fforward_line.
   integer(int64) function forward_line(n) result(shortage)
      integer(int64), intent(in), optional :: n
      integer(int64) :: count, opoint, pos, pos_byte, counted
      logical :: excessive

      count = 1
      excessive = .false.
      if (present(n)) then
         count = max(-BUF_BYTES_MAX, min(n, BUF_BYTES_MAX))
         excessive = count /= n
      end if
      opoint = buffers(cur)%pt
      counted = scan_newline_from_point(count, pos, pos_byte)
      call temp_set_point_both(buffers(cur), pos, pos_byte)
      shortage = count - counted
      if (count <= 0) shortage = shortage - 1
      if (shortage /= 0) then
         if (count <= 0) then
            shortage = shortage + 1
         else
            associate (b => buffers(cur))
               if (b%begv < b%zv .and. b%pt /= opoint) then
                  if (buf_fetch_byte(b, b%pt_byte - 1) /= 10) shortage = shortage - 1
               end if
            end associate
         end if
      end if
      ! A count beyond any buffer size reports the shortage relative to N.
      if (excessive) shortage = shortage - count + n
   end function forward_line

   !> Move to the start of the line (after moving `n` - 1 lines).
   !> Port of cmds.c:Fbeginning_of_line (fields pending).
   subroutine beginning_of_line(n)
      integer(int64), intent(in), optional :: n

      call goto_char(line_beginning_position(n))
   end subroutine beginning_of_line

   !> Move to the end of the line (after moving `n` - 1 lines).
   !> Port of cmds.c:Fend_of_line; the intangible-text retry loop is moot
   !> until text properties exist.
   subroutine end_of_line(n)
      integer(int64), intent(in), optional :: n

      call goto_char(line_end_position(n))
   end subroutine end_of_line

   !> Delete `n` characters after point (before, if negative); signals
   !> beginning-/end-of-buffer instead of deleting fewer.
   !> Port of cmds.c:Fdelete_char (without KILLFLAG).
   subroutine delete_char(n, status)
      integer(int64), intent(in) :: n
      type(status_type), intent(out) :: status
      integer(int64) :: pos

      pos = buffers(cur)%pt + n
      if (n < 0) then
         if (pos < buffers(cur)%begv) then
            call set_error(status, FM_BEGINNING_OF_BUFFER)
         else
            call del_range(pos, buffers(cur)%pt, status)
         end if
      else
         if (pos > buffers(cur)%zv) then
            call set_error(status, FM_END_OF_BUFFER)
         else
            call del_range(buffers(cur)%pt, pos, status)
         end if
      end if
   end subroutine delete_char

end module fm_cmds
