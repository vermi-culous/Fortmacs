! fm_character.f90 -- multibyte character encoding.
! Ported from GNU Emacs 31.1 src/character.c and src/character.h.
!
! Copyright (C) 2001-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 1997, 1998, 2001 Electrotechnical Laboratory, JAPAN.
!   Licensed to the Free Software Foundation.
! Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H13PRO009
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

!> Emacs's internal multibyte encoding: UTF-8 extended to 22-bit codes,
!> plus two-byte forms for raw 8-bit bytes.  From character.h:
!>
!>     character code   1st byte   byte sequence
!>     --------------   --------   -------------
!>          0-7F        00..7F     0xxxxxxx
!>         80-7FF       C2..DF     110yyyyx 10xxxxxx
!>        800-FFFF      E0..EF     1110yyyy 10yxxxxx 10xxxxxx
!>      10000-1FFFFF    F0..F7     11110yyy 10yyxxxx 10xxxxxx 10xxxxxx
!>     200000-3FFF7F    F8         11111000 1000yxxx 10xxxxxx 10xxxxxx 10xxxxxx
!>     3FFF80-3FFFFF    C0..C1     1100000x 10xxxxxx  (eight-bit-char)
!>     400000-...                  invalid
!>
!> Valid Unicode text is therefore plain UTF-8.  Byte strings are Fortran
!> default-kind `character` (each element one byte) and byte positions are
!> 1-based `int64`.  Character codes are `int32` (they include modifier
!> bits up to CHAR_META).
!>
!> Where upstream takes a `check` flag and an end pointer, these procedures
!> always bound-check against `len(str)`; the unchecked fast paths are an
!> optimisation C needs and Fortran (with -fcheck=all) does not.
module fm_character
   use, intrinsic :: iso_fortran_env, only: int32, int64
   use fm_error, only: status_type, set_error, FM_GENERIC_ERROR
   implicit none(type, external)
   private

   ! character.h
   integer(int32), parameter, public :: MAX_CHAR = int(z'3FFFFF', int32)
   integer(int32), parameter, public :: MAX_UNICODE_CHAR = int(z'10FFFF', int32)
   integer(int32), parameter, public :: MAX_1_BYTE_CHAR = int(z'7F', int32)
   integer(int32), parameter, public :: MAX_2_BYTE_CHAR = int(z'7FF', int32)
   integer(int32), parameter, public :: MAX_3_BYTE_CHAR = int(z'FFFF', int32)
   integer(int32), parameter, public :: MAX_4_BYTE_CHAR = int(z'1FFFFF', int32)
   integer(int32), parameter, public :: MAX_5_BYTE_CHAR = int(z'3FFF7F', int32)
   integer(int32), parameter, public :: MIN_MULTIBYTE_LEADING_CODE = int(z'C0', int32)
   integer(int32), parameter, public :: MAX_MULTIBYTE_LEADING_CODE = int(z'F8', int32)
   integer, parameter, public :: MAX_MULTIBYTE_LENGTH = 5

   ! lisp.h: modifier bits of character events.
   integer(int32), parameter, public :: CHAR_ALT = int(z'0400000', int32)
   integer(int32), parameter, public :: CHAR_SUPER = int(z'0800000', int32)
   integer(int32), parameter, public :: CHAR_HYPER = int(z'1000000', int32)
   integer(int32), parameter, public :: CHAR_SHIFT = int(z'2000000', int32)
   integer(int32), parameter, public :: CHAR_CTL = int(z'4000000', int32)
   integer(int32), parameter, public :: CHAR_META = int(z'8000000', int32)
   integer(int32), parameter, public :: CHAR_MODIFIER_MASK = &
                                        ior(ior(ior(CHAR_ALT, CHAR_SUPER), ior(CHAR_HYPER, CHAR_SHIFT)), &
                                            ior(CHAR_CTL, CHAR_META))

   integer(int32), parameter :: byte8_offset = int(z'3FFF00', int32)

   public :: ascii_char_p, single_byte_char_p, char_valid_p
   public :: char_byte8_p, byte8_to_char, unibyte_to_char, char_to_byte8, &
             char_to_byte_safe, char_byte8_head_p
   public :: char_bytes, char_leading_code, char_string
   public :: leading_code_p, trailing_code_p, char_head_p, bytes_by_char_head
   public :: multibyte_length, raw_prev_char_len, string_char_and_length, &
             string_char
   public :: multibyte_chars_in_text, parse_str_as_multibyte, &
             str_as_multibyte, count_size_as_multibyte, str_to_multibyte, &
             str_as_unibyte
   public :: char_resolve_modifier_mask
   public :: char_variation_selector_p, char_surrogate_p, char_hexdigit

contains

   !> Byte value (0..255) at position `i` of `str`.
   pure integer(int32) function byte_at(str, i)
      character(len=*), intent(in) :: str
      integer(int64), intent(in) :: i

      byte_at = ichar(str(i:i), kind=int32)
   end function byte_at

   !> Port of lisp.h:ASCII_CHAR_P.
   elemental logical function ascii_char_p(c)
      integer(int32), intent(in) :: c

      ascii_char_p = 0 <= c .and. c < 128
   end function ascii_char_p

   !> Port of character.h:SINGLE_BYTE_CHAR_P.
   elemental logical function single_byte_char_p(c)
      integer(int32), intent(in) :: c

      single_byte_char_p = 0 <= c .and. c < 256
   end function single_byte_char_p

   !> Port of character.h:CHAR_VALID_P.
   elemental logical function char_valid_p(c)
      integer(int32), intent(in) :: c

      char_valid_p = 0 <= c .and. c <= MAX_CHAR
   end function char_valid_p

   !> True if `c` stands for a raw 8-bit byte.
   !> Port of character.h:CHAR_BYTE8_P.
   elemental logical function char_byte8_p(c)
      integer(int32), intent(in) :: c

      char_byte8_p = MAX_5_BYTE_CHAR < c
   end function char_byte8_p

   !> Character code for raw byte `byte`.
   !> Port of character.h:BYTE8_TO_CHAR.
   elemental integer(int32) function byte8_to_char(byte)
      integer(int32), intent(in) :: byte

      byte8_to_char = byte + byte8_offset
   end function byte8_to_char

   !> Port of character.h:UNIBYTE_TO_CHAR.
   elemental integer(int32) function unibyte_to_char(byte)
      integer(int32), intent(in) :: byte

      if (ascii_char_p(byte)) then
         unibyte_to_char = byte
      else
         unibyte_to_char = byte8_to_char(byte)
      end if
   end function unibyte_to_char

   !> Raw byte for character `c`.  Port of character.h:CHAR_TO_BYTE8.
   elemental integer(int32) function char_to_byte8(c)
      integer(int32), intent(in) :: c

      if (char_byte8_p(c)) then
         char_to_byte8 = c - byte8_offset
      else
         char_to_byte8 = iand(c, 255_int32)
      end if
   end function char_to_byte8

   !> Raw byte for `c`, or -1 if it is not one.
   !> Port of character.h:CHAR_TO_BYTE_SAFE.
   elemental integer(int32) function char_to_byte_safe(c)
      integer(int32), intent(in) :: c

      if (ascii_char_p(c)) then
         char_to_byte_safe = c
      else if (char_byte8_p(c)) then
         char_to_byte_safe = c - byte8_offset
      else
         char_to_byte_safe = -1
      end if
   end function char_to_byte_safe

   !> True if `byte` leads the two-byte form of a raw byte.
   !> Port of character.h:CHAR_BYTE8_HEAD_P.
   elemental logical function char_byte8_head_p(byte)
      integer(int32), intent(in) :: byte

      char_byte8_head_p = byte == int(z'C0', int32) .or. byte == int(z'C1', int32)
   end function char_byte8_head_p

   !> Length of the multibyte form of `c`.  Port of character.h:CHAR_BYTES.
   elemental integer(int32) function char_bytes(c)
      integer(int32), intent(in) :: c

      if (c <= MAX_1_BYTE_CHAR) then
         char_bytes = 1
      else if (c <= MAX_2_BYTE_CHAR) then
         char_bytes = 2
      else if (c <= MAX_3_BYTE_CHAR) then
         char_bytes = 3
      else if (c <= MAX_4_BYTE_CHAR) then
         char_bytes = 4
      else if (c <= MAX_5_BYTE_CHAR) then
         char_bytes = 5
      else
         char_bytes = 2
      end if
   end function char_bytes

   !> First byte of the multibyte form of `c`.
   !> Port of character.h:CHAR_LEADING_CODE.
   elemental integer(int32) function char_leading_code(c)
      integer(int32), intent(in) :: c

      if (c <= MAX_1_BYTE_CHAR) then
         char_leading_code = c
      else if (c <= MAX_2_BYTE_CHAR) then
         char_leading_code = ior(int(z'C0', int32), ishft(c, -6))
      else if (c <= MAX_3_BYTE_CHAR) then
         char_leading_code = ior(int(z'E0', int32), ishft(c, -12))
      else if (c <= MAX_4_BYTE_CHAR) then
         char_leading_code = ior(int(z'F0', int32), ishft(c, -18))
      else if (c <= MAX_5_BYTE_CHAR) then
         char_leading_code = int(z'F8', int32)
      else
         char_leading_code = ior(int(z'C0', int32), iand(ishft(c, -6), 1_int32))
      end if
   end function char_leading_code

   !> Store the multibyte form of `c` in `buf(1:nbytes)`.  Modifier bits
   !> are folded in where possible and otherwise dropped.  Fails for codes
   !> outside 0..MAX_CHAR.
   !> Port of character.c:char_string and character.h:CHAR_STRING,
   !> BYTE8_STRING.
   pure subroutine char_string(c, buf, nbytes, status)
      integer(int32), intent(in) :: c
      character(len=MAX_MULTIBYTE_LENGTH), intent(out) :: buf
      integer(int32), intent(out) :: nbytes
      type(status_type), intent(out) :: status
      integer(int32) :: k
      character(len=8) :: hex

      buf = ""
      nbytes = 0
      k = c
      if (iand(k, CHAR_MODIFIER_MASK) /= 0 .and. k > 0) then
         k = char_resolve_modifier_mask(k)
         k = iand(k, not(CHAR_MODIFIER_MASK))
      end if

      if (k < 0 .or. k > MAX_CHAR) then
         write (hex, '(z0)') c
         call set_error(status, FM_GENERIC_ERROR, "Invalid character: "//trim(hex))
         return
      end if

      if (k <= MAX_1_BYTE_CHAR) then
         buf(1:1) = char(k)
         nbytes = 1
      else if (k <= MAX_2_BYTE_CHAR) then
         buf(1:1) = char(ior(int(z'C0', int32), ishft(k, -6)))
         buf(2:2) = char(trail(k))
         nbytes = 2
      else if (k <= MAX_3_BYTE_CHAR) then
         buf(1:1) = char(ior(int(z'E0', int32), ishft(k, -12)))
         buf(2:2) = char(trail(ishft(k, -6)))
         buf(3:3) = char(trail(k))
         nbytes = 3
      else if (k <= MAX_4_BYTE_CHAR) then
         buf(1:1) = char(ior(int(z'F0', int32), ishft(k, -18)))
         buf(2:2) = char(trail(ishft(k, -12)))
         buf(3:3) = char(trail(ishft(k, -6)))
         buf(4:4) = char(trail(k))
         nbytes = 4
      else if (k <= MAX_5_BYTE_CHAR) then
         buf(1:1) = char(int(z'F8', int32))
         buf(2:2) = char(ior(int(z'80', int32), iand(ishft(k, -18), int(z'0F', int32))))
         buf(3:3) = char(trail(ishft(k, -12)))
         buf(4:4) = char(trail(ishft(k, -6)))
         buf(5:5) = char(trail(k))
         nbytes = 5
      else
         ! BYTE8_STRING
         k = char_to_byte8(k)
         buf(1:1) = char(ior(int(z'C0', int32), iand(ishft(k, -6), 1_int32)))
         buf(2:2) = char(trail(k))
         nbytes = 2
      end if

   contains

      !> 10xxxxxx from the low six bits of `x`.
      pure integer(int32) function trail(x)
         integer(int32), intent(in) :: x

         trail = ior(int(z'80', int32), iand(x, int(z'3F', int32)))
      end function trail

   end subroutine char_string

   !> Port of character.h:LEADING_CODE_P.
   elemental logical function leading_code_p(byte)
      integer(int32), intent(in) :: byte

      leading_code_p = iand(byte, int(z'C0', int32)) == int(z'C0', int32)
   end function leading_code_p

   !> Port of character.h:TRAILING_CODE_P.
   elemental logical function trailing_code_p(byte)
      integer(int32), intent(in) :: byte

      trailing_code_p = iand(byte, int(z'C0', int32)) == int(z'80', int32)
   end function trailing_code_p

   !> True if `byte` starts a character.  Port of character.h:CHAR_HEAD_P.
   elemental logical function char_head_p(byte)
      integer(int32), intent(in) :: byte

      char_head_p = iand(byte, int(z'C0', int32)) /= int(z'80', int32)
   end function char_head_p

   !> Length of a character judged by its first byte alone (no validation).
   !> Port of character.h:BYTES_BY_CHAR_HEAD.
   elemental integer(int32) function bytes_by_char_head(byte)
      integer(int32), intent(in) :: byte

      if (iand(byte, int(z'80', int32)) == 0) then
         bytes_by_char_head = 1
      else if (iand(byte, int(z'20', int32)) == 0) then
         bytes_by_char_head = 2
      else if (iand(byte, int(z'10', int32)) == 0) then
         bytes_by_char_head = 3
      else if (iand(byte, int(z'08', int32)) == 0) then
         bytes_by_char_head = 4
      else
         bytes_by_char_head = 5
      end if
   end function bytes_by_char_head

   !> Byte length of the valid multibyte form starting at `str(pos:)`, or 0
   !> if there is none.  With `allow_8bit`, raw-byte forms (C0/C1) count.
   !> Port of character.h:multibyte_length (always with check = true).
   pure integer(int32) function multibyte_length(str, pos, allow_8bit)
      character(len=*), intent(in) :: str
      integer(int64), intent(in) :: pos
      logical, intent(in) :: allow_8bit
      integer(int64) :: pend
      integer(int32) :: c, d, e, f, w, w1, w2, lo2
      integer(int64) :: lw, w3

      multibyte_length = 0
      pend = len(str, kind=int64) + 1
      if (pos >= pend) return
      c = byte_at(str, pos)
      if (c < int(z'80', int32)) then
         multibyte_length = 1
         return
      end if
      if (pos + 1 >= pend) return
      d = byte_at(str, pos + 1)
      w = ishft(iand(d, int(z'C0', int32)), 2) + c
      lo2 = int(z'2C2', int32)
      if (allow_8bit) lo2 = int(z'2C0', int32)
      if (lo2 <= w .and. w <= int(z'2DF', int32)) then
         multibyte_length = 2
         return
      end if
      if (pos + 2 >= pend) return
      e = byte_at(str, pos + 2)
      w = w + ishft(iand(e, int(z'C0', int32)), 4)
      w1 = ior(w, ishft(iand(d, int(z'20', int32)), -2))
      if (int(z'AE1', int32) <= w1 .and. w1 <= int(z'AEF', int32)) then
         multibyte_length = 3
         return
      end if
      if (pos + 3 >= pend) return
      f = byte_at(str, pos + 3)
      w = w + ishft(iand(f, int(z'C0', int32)), 6)
      w2 = ior(w, ishft(iand(d, int(z'30', int32)), -3))
      if (int(z'2AF1', int32) <= w2 .and. w2 <= int(z'2AF7', int32)) then
         multibyte_length = 4
         return
      end if
      if (pos + 4 >= pend) return
      lw = int(w, int64) + ishft(int(iand(byte_at(str, pos + 4), int(z'C0', int32)), int64), 8)
      w3 = ishft(lw, 24) + ishft(int(d, int64), 16) + ishft(int(e, int64), 8) + int(f, int64)
      if (int(z'AAF8888080', int64) <= w3 .and. w3 <= int(z'AAF88FBFBD', int64)) then
         multibyte_length = 5
      end if
   end function multibyte_length

   !> Byte length of the character ending just before byte `pos`.  Assumes
   !> `pos` is a character boundary after the start of a valid text.
   !> Port of character.h:raw_prev_char_len.
   pure integer(int32) function raw_prev_char_len(str, pos)
      character(len=*), intent(in) :: str
      integer(int64), intent(in) :: pos
      integer(int32) :: n

      ! Upstream loops without bound; stopping at the text start and at
      ! MAX_MULTIBYTE_LENGTH keeps malformed input from reading out of range.
      raw_prev_char_len = 1
      do n = 1, MAX_MULTIBYTE_LENGTH
         if (pos - n < 1) exit
         raw_prev_char_len = n
         if (char_head_p(byte_at(str, pos - n))) return
      end do
   end function raw_prev_char_len

   !> Decode the character whose valid multibyte form starts at `str(pos:)`
   !> and return its byte length in `length`.
   !> Port of character.h:string_char_and_length.
   pure subroutine string_char_and_length(str, pos, ch, length)
      character(len=*), intent(in) :: str
      integer(int64), intent(in) :: pos
      integer(int32), intent(out) :: ch, length
      integer(int32) :: c, d

      c = byte_at(str, pos)
      if (iand(c, int(z'80', int32)) == 0) then
         length = 1
         ch = c
         return
      end if

      d = ishft(c, 6) + byte_at(str, pos + 1) - (ishft(int(z'C0', int32), 6) + int(z'80', int32))
      if (iand(c, int(z'20', int32)) == 0) then
         length = 2
         ch = d
         if (c < int(z'C2', int32)) ch = d + int(z'3FFF80', int32)
         return
      end if

      d = ishft(d, 6) + byte_at(str, pos + 2) - (ishft(int(z'20', int32), 12) + int(z'80', int32))
      if (iand(c, int(z'10', int32)) == 0) then
         length = 3
         ch = d
         return
      end if

      d = ishft(d, 6) + byte_at(str, pos + 3) - (ishft(int(z'10', int32), 18) + int(z'80', int32))
      if (iand(c, int(z'08', int32)) == 0) then
         length = 4
         ch = d
         return
      end if

      d = ishft(d, 6) + byte_at(str, pos + 4) - (ishft(int(z'08', int32), 24) + int(z'80', int32))
      length = 5
      ch = d
   end subroutine string_char_and_length

   !> Decode the character at `str(pos:)`.  Port of character.h:STRING_CHAR.
   pure integer(int32) function string_char(str, pos)
      character(len=*), intent(in) :: str
      integer(int64), intent(in) :: pos
      integer(int32) :: length

      call string_char_and_length(str, pos, string_char, length)
   end function string_char

   !> Number of characters in multibyte text `str`, which must contain only
   !> valid multibyte forms (raw-byte forms included).  Upstream aborts on
   !> an invalid sequence; here that is an error.
   !> Port of character.c:multibyte_chars_in_text.
   pure subroutine multibyte_chars_in_text(str, nchars, status)
      character(len=*), intent(in) :: str
      integer(int64), intent(out) :: nchars
      type(status_type), intent(out) :: status
      integer(int64) :: p
      integer(int32) :: n

      nchars = 0
      p = 1
      do while (p <= len(str, kind=int64))
         n = multibyte_length(str, p, .true.)
         if (n == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid multibyte sequence")
            return
         end if
         p = p + n
         nchars = nchars + 1
      end do
   end subroutine multibyte_chars_in_text

   !> Count the characters and bytes unibyte text `str` would have as
   !> multibyte text: bytes that start no valid sequence become 2-byte
   !> raw-byte forms.  Port of character.c:parse_str_as_multibyte.
   pure subroutine parse_str_as_multibyte(str, nchars, nbytes)
      character(len=*), intent(in) :: str
      integer(int64), intent(out) :: nchars, nbytes
      integer(int64) :: p
      integer(int32) :: n

      nchars = 0
      nbytes = 0
      p = 1
      do while (p <= len(str, kind=int64))
         n = multibyte_length(str, p, .false.)
         if (n > 0) then
            p = p + n
            nbytes = nbytes + n
         else
            p = p + 1
            nbytes = nbytes + 2
         end if
         nchars = nchars + 1
      end do
   end subroutine parse_str_as_multibyte

   !> Unibyte text `str` rearranged as multibyte text: valid sequences are
   !> kept, other 8-bit bytes become raw-byte forms.  `nchars` receives the
   !> character count.  Port of character.c:str_as_multibyte (which works
   !> in place; this produces a new string).
   pure subroutine str_as_multibyte(str, out, nchars)
      character(len=*), intent(in) :: str
      character(len=:), allocatable, intent(out) :: out
      integer(int64), intent(out), optional :: nchars
      integer(int64) :: p, q, chars, nbytes
      integer(int32) :: n, b

      call parse_str_as_multibyte(str, chars, nbytes)
      if (present(nchars)) nchars = chars
      allocate (character(len=nbytes) :: out)
      p = 1
      q = 1
      do while (p <= len(str, kind=int64))
         n = multibyte_length(str, p, .false.)
         if (n > 0) then
            out(q:q + n - 1) = str(p:p + n - 1)
            p = p + n
            q = q + n
         else
            b = byte_at(str, p)
            out(q:q) = char(ior(int(z'C0', int32), iand(ishft(b, -6), 1_int32)))
            out(q + 1:q + 1) = char(ior(int(z'80', int32), iand(b, int(z'3F', int32))))
            p = p + 1
            q = q + 2
         end if
      end do
   end subroutine str_as_multibyte

   !> Bytes `str` occupies once every non-ASCII byte becomes a raw-byte
   !> form.  Port of character.c:count_size_as_multibyte.
   pure integer(int64) function count_size_as_multibyte(str)
      character(len=*), intent(in) :: str
      integer(int64) :: i

      count_size_as_multibyte = len(str, kind=int64)
      do i = 1, len(str, kind=int64)
         if (byte_at(str, i) > 127) count_size_as_multibyte = count_size_as_multibyte + 1
      end do
   end function count_size_as_multibyte

   !> Unibyte `src` as multibyte text holding the same single-byte
   !> characters (every non-ASCII byte becomes a raw-byte form).
   !> Port of character.c:str_to_multibyte.
   pure function str_to_multibyte(src) result(dst)
      character(len=*), intent(in) :: src
      character(len=:), allocatable :: dst
      integer(int64) :: i, q
      integer(int32) :: b

      allocate (character(len=count_size_as_multibyte(src)) :: dst)
      q = 1
      do i = 1, len(src, kind=int64)
         b = byte_at(src, i)
         if (b <= 127) then
            dst(q:q) = src(i:i)
            q = q + 1
         else
            dst(q:q) = char(int(z'C0', int32) + iand(ishft(b, -6), 1_int32))
            dst(q + 1:q + 1) = char(int(z'80', int32) + iand(b, int(z'3F', int32)))
            q = q + 2
         end if
      end do
   end function str_to_multibyte

   !> Multibyte `str` with raw-byte forms turned back into single bytes.
   !> Port of character.c:str_as_unibyte (which works in place).
   pure function str_as_unibyte(str) result(out)
      character(len=*), intent(in) :: str
      character(len=:), allocatable :: out
      character(len=len(str)) :: work
      integer(int64) :: p, q
      integer(int32) :: c, n

      p = 1
      q = 0
      do while (p <= len(str, kind=int64))
         c = byte_at(str, p)
         if (char_byte8_head_p(c)) then
            call string_char_and_length(str, p, c, n)
            q = q + 1
            work(q:q) = char(char_to_byte8(c))
         else
            n = min(bytes_by_char_head(c), int(len(str, kind=int64) - p + 1, int32))
            work(q + 1:q + n) = str(p:p + n - 1)
            q = q + n
         end if
         p = p + n
      end do
      out = work(1:q)
   end function str_as_unibyte

   !> Fold modifier bits of `c` into the code where ASCII allows it (e.g.
   !> C-a -> 1, S-a -> A).  Port of character.c:char_resolve_modifier_mask.
   elemental integer(int32) function char_resolve_modifier_mask(c) result(k)
      integer(int32), intent(in) :: c
      integer(int32) :: low

      k = c
      ! A non-ASCII character can't reflect modifier bits to the code.
      if (.not. ascii_char_p(iand(k, not(CHAR_MODIFIER_MASK)))) return

      if (iand(k, CHAR_SHIFT) /= 0) then
         low = iand(k, 255_int32)
         if (low >= iachar('A') .and. low <= iachar('Z')) then
            k = iand(k, not(CHAR_SHIFT))
         else if (low >= iachar('a') .and. low <= iachar('z')) then
            k = iand(k, not(CHAR_SHIFT)) - (iachar('a') - iachar('A'))
         else if (iand(k, not(CHAR_MODIFIER_MASK)) <= 32) then
            ! Shift is ignored for control characters and SPC.
            k = iand(k, not(CHAR_SHIFT))
         end if
      end if

      if (iand(k, CHAR_CTL) /= 0) then
         low = iand(k, 255_int32)
         if (low == iachar(' ')) then
            ! C-SPC -> NUL
            k = iand(k, not(ior(127_int32, CHAR_CTL)))
         else if (low == iachar('?')) then
            ! C-? -> DEL
            k = ior(127_int32, iand(k, not(ior(127_int32, CHAR_CTL))))
         else if (iand(k, 95_int32) >= 65 .and. iand(k, 95_int32) <= 90) then
            ! Letters, either case.
            k = iand(k, ior(31_int32, iand(not(127_int32), not(CHAR_CTL))))
         else if (iand(k, 127_int32) >= 64 .and. iand(k, 127_int32) <= 95) then
            ! The non-letters in 0100..0137 (@ [ \ ] ^ _).
            k = iand(k, ior(31_int32, iand(not(127_int32), not(CHAR_CTL))))
         end if
      end if
   end function char_resolve_modifier_mask

   !> Index (1..256) of variation selector `c`, or 0.
   !> Port of character.h:CHAR_VARIATION_SELECTOR_P.
   elemental integer(int32) function char_variation_selector_p(c)
      integer(int32), intent(in) :: c

      if (c < int(z'FE00', int32)) then
         char_variation_selector_p = 0
      else if (c <= int(z'FE0F', int32)) then
         char_variation_selector_p = c - int(z'FE00', int32) + 1
      else if (c < int(z'E0100', int32)) then
         char_variation_selector_p = 0
      else if (c <= int(z'E01EF', int32)) then
         char_variation_selector_p = c - int(z'E0100', int32) + 17
      else
         char_variation_selector_p = 0
      end if
   end function char_variation_selector_p

   !> Port of character.h:char_surrogate_p.
   elemental logical function char_surrogate_p(c)
      integer(int32), intent(in) :: c

      char_surrogate_p = int(z'D800', int32) <= c .and. c <= int(z'DFFF', int32)
   end function char_surrogate_p

   !> Value 0..15 of hex digit `c`, or -1.
   !> Port of character.h:char_hexdigit.
   elemental integer(int32) function char_hexdigit(c)
      integer(int32), intent(in) :: c

      if (c >= iachar('0') .and. c <= iachar('9')) then
         char_hexdigit = c - iachar('0')
      else if (c >= iachar('a') .and. c <= iachar('f')) then
         char_hexdigit = c - iachar('a') + 10
      else if (c >= iachar('A') .and. c <= iachar('F')) then
         char_hexdigit = c - iachar('A') + 10
      else
         char_hexdigit = -1
      end if
   end function char_hexdigit

end module fm_character
