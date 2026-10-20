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

!> Converting between character and byte positions.
!>
!> Upstream finds the known (charpos, bytepos) pair closest to the target
!> -- BEG, BEGV, PT, GPT, ZV, Z, a one-entry cache, and every marker --
!> and scans from there.  Marker objects themselves arrive in Phase 1
!> step 4; until then only the fixed points and the cache are consulted.
!> Buffers are addressed by registry slot (see fm_buffer).
module fm_marker
   use, intrinsic :: iso_fortran_env, only: int64
   use fm_buffer, only: buffers, cur, BEG, BEG_BYTE, buf_next_char_len, &
                        buf_prev_char_len
   implicit none(type, external)
   private

   public :: buf_charpos_to_bytepos, buf_bytepos_to_charpos
   public :: char_to_byte, byte_to_char, clear_charpos_cache

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
   !> Port of marker.c:buf_charpos_to_bytepos (marker scan pending).
   integer(int64) function buf_charpos_to_bytepos(b, charpos) result(bytepos)
      integer(int64), intent(in) :: b, charpos
      integer(int64) :: best_above, best_above_byte, best_below, best_below_byte
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
   !> slot `b`.  Port of marker.c:buf_bytepos_to_charpos (marker scan
   !> pending).
   integer(int64) function buf_bytepos_to_charpos(b, bytepos) result(charpos)
      integer(int64), intent(in) :: b, bytepos
      integer(int64) :: best_above, best_above_byte, best_below, best_below_byte
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

end module fm_marker
