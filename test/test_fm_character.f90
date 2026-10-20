! test_fm_character.f90 -- tests for fm_character.
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

!> Upstream test/src/character-tests.el only covers `string-width`, which
!> needs char-width tables (deferred to the display phase).  These tests
!> exercise the encoding table in character.h directly.
module test_fm_character
   use, intrinsic :: iso_fortran_env, only: int32, int64
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use fm_error, only: status_type, FM_GENERIC_ERROR
   use fm_character
   implicit none(type, external)
   private

   public :: collect_fm_character

contains

   subroutine collect_fm_character(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("utf8-known-encodings", test_known), &
                  new_unittest("roundtrip-boundaries", test_roundtrip), &
                  new_unittest("raw-bytes", test_raw_bytes), &
                  new_unittest("invalid-sequences", test_invalid), &
                  new_unittest("invalid-char-code", test_invalid_code), &
                  new_unittest("chars-in-text", test_chars_in_text), &
                  new_unittest("unibyte-multibyte-conversion", test_conversion), &
                  new_unittest("raw-prev-char-len", test_prev_len), &
                  new_unittest("modifier-resolution", test_modifiers), &
                  new_unittest("small-predicates", test_predicates) &
                  ]
   end subroutine collect_fm_character

   !> String made of the given byte values.
   pure function bytes(b) result(s)
      integer, intent(in) :: b(:)
      character(len=size(b)) :: s
      integer :: i

      do i = 1, size(b)
         s(i:i) = char(b(i))
      end do
   end function bytes

   !> Encode `c`; empty string on error.
   function enc(c) result(s)
      integer(int32), intent(in) :: c
      character(len=:), allocatable :: s
      character(len=MAX_MULTIBYTE_LENGTH) :: buf
      integer(int32) :: n
      type(status_type) :: st

      call char_string(c, buf, n, st)
      s = buf(1:n)
   end function enc

   subroutine test_known(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, enc(int(z'E9', int32)) == bytes([195, 169]), "U+00E9")
      if (allocated(error)) return
      call check(error, enc(int(z'20AC', int32)) == bytes([226, 130, 172]), "U+20AC")
      if (allocated(error)) return
      call check(error, enc(int(z'1F600', int32)) == bytes([240, 159, 152, 128]), "U+1F600")
      if (allocated(error)) return
      call check(error, string_char(bytes([226, 130, 172]), 1_int64), int(z'20AC', int32))
   end subroutine test_known

   subroutine test_roundtrip(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: codes(*) = [ &
                                   0, 65, int(z'7F'), int(z'80'), int(z'7FF'), int(z'800'), &
                                   int(z'FFFF'), int(z'10000'), int(z'10FFFF'), int(z'1FFFFF'), &
                                   int(z'200000'), int(z'3FFF7F'), int(z'3FFF80'), int(z'3FFFFF')]
      integer(int32), parameter :: lengths(*) = [1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 2, 2]
      character(len=:), allocatable :: s
      integer(int32) :: i, n, c

      do i = 1, size(codes)
         s = enc(codes(i))
         call check(error, len(s), lengths(i))
         if (allocated(error)) return
         call check(error, char_bytes(codes(i)), lengths(i))
         if (allocated(error)) return
         call check(error, ichar(s(1:1), int32), char_leading_code(codes(i)))
         if (allocated(error)) return
         call check(error, multibyte_length(s, 1_int64, .true.), lengths(i))
         if (allocated(error)) return
         call check(error, bytes_by_char_head(ichar(s(1:1), int32)), lengths(i))
         if (allocated(error)) return
         call string_char_and_length(s, 1_int64, c, n)
         call check(error, c, codes(i))
         if (allocated(error)) return
         call check(error, n, lengths(i))
         if (allocated(error)) return
      end do
   end subroutine test_roundtrip

   subroutine test_raw_bytes(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, byte8_to_char(int(z'FF', int32)), int(z'3FFFFF', int32))
      if (allocated(error)) return
      call check(error, char_to_byte8(int(z'3FFF80', int32)), int(z'80', int32))
      if (allocated(error)) return
      call check(error, unibyte_to_char(65_int32), 65_int32)
      if (allocated(error)) return
      call check(error, char_to_byte_safe(int(z'E9', int32)), -1_int32)
      if (allocated(error)) return
      call check(error, enc(byte8_to_char(int(z'FF', int32))) == bytes([193, 191]), "raw FF form")
      if (allocated(error)) return
      ! Raw-byte forms are only valid when allow_8bit is set.
      call check(error, multibyte_length(bytes([193, 191]), 1_int64, .false.), 0_int32)
      if (allocated(error)) return
      call check(error, char_byte8_head_p(193_int32), "C1 not a byte8 head")
   end subroutine test_raw_bytes

   subroutine test_invalid(error)
      type(error_type), allocatable, intent(out) :: error

      ! Lone trailing byte.
      call check(error, multibyte_length(bytes([128]), 1_int64, .true.), 0_int32)
      if (allocated(error)) return
      ! Overlong 3-byte form of NUL.
      call check(error, multibyte_length(bytes([224, 128, 128]), 1_int64, .true.), 0_int32)
      if (allocated(error)) return
      ! Truncated 3-byte form.
      call check(error, multibyte_length(bytes([226, 130]), 1_int64, .true.), 0_int32)
      if (allocated(error)) return
      ! F9 never leads.
      call check(error, multibyte_length(bytes([249, 128, 128, 128, 128]), 1_int64, .true.), 0_int32)
      if (allocated(error)) return
      ! Past the end.
      call check(error, multibyte_length("a", 2_int64, .true.), 0_int32)
   end subroutine test_invalid

   subroutine test_invalid_code(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=MAX_MULTIBYTE_LENGTH) :: buf
      integer(int32) :: n
      type(status_type) :: st

      ! Like upstream, MAX_CHAR + 1 is the CHAR_ALT bit, which is dropped.
      call char_string(MAX_CHAR + 1, buf, n, st)
      call check(error, st%ok() .and. n == 1 .and. buf(1:1) == char(0), "alt bit not dropped")
      if (allocated(error)) return
      ! Bits above the modifier mask cannot be dropped: invalid.
      call char_string(int(z'10000000', int32), buf, n, st)
      call check(error, st%code, FM_GENERIC_ERROR)
      if (allocated(error)) return
      call check(error, st%detail, "Invalid character: 10000000")
      if (allocated(error)) return
      call char_string(-1_int32, buf, n, st)
      call check(error,.not. st%ok(), "negative code accepted")
   end subroutine test_invalid_code

   subroutine test_chars_in_text(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: nchars, nbytes
      type(status_type) :: st

      call multibyte_chars_in_text("a"//bytes([195, 169, 226, 130, 172, 193, 191]), nchars, st)
      call check(error, st%ok(), "valid text rejected")
      if (allocated(error)) return
      call check(error, nchars, 4_int64)
      if (allocated(error)) return
      call multibyte_chars_in_text("a"//bytes([128]), nchars, st)
      call check(error,.not. st%ok(), "invalid text accepted")
      if (allocated(error)) return

      ! a, raw FF (2 bytes once converted), é
      call parse_str_as_multibyte("a"//bytes([255, 195, 169]), nchars, nbytes)
      call check(error, nchars, 3_int64)
      if (allocated(error)) return
      call check(error, nbytes, 5_int64)
   end subroutine test_chars_in_text

   subroutine test_conversion(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: nchars
      character(len=:), allocatable :: s

      call str_as_multibyte("a"//bytes([255, 195, 169]), s, nchars)
      call check(error, s == "a"//bytes([193, 191, 195, 169]), "str_as_multibyte")
      if (allocated(error)) return
      call check(error, nchars, 3_int64)
      if (allocated(error)) return
      call check(error, count_size_as_multibyte("a"//bytes([233])), 3_int64)
      if (allocated(error)) return
      call check(error, str_to_multibyte("a"//bytes([233])) == "a"//bytes([193, 169]), &
                 "str_to_multibyte")
      if (allocated(error)) return
      call check(error, str_as_unibyte(bytes([193, 169])//"a"//bytes([195, 169])) == &
                 bytes([233])//"a"//bytes([195, 169]), "str_as_unibyte")
   end subroutine test_conversion

   subroutine test_prev_len(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: s

      s = "a"//bytes([226, 130, 172])//"b"
      call check(error, raw_prev_char_len(s, 5_int64), 3_int32)
      if (allocated(error)) return
      call check(error, raw_prev_char_len(s, 6_int64), 1_int32)
      if (allocated(error)) return
      call check(error, raw_prev_char_len(s, 2_int64), 1_int32)
   end subroutine test_prev_len

   subroutine test_modifiers(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, char_resolve_modifier_mask(CHAR_CTL + iachar('a')), 1_int32)
      if (allocated(error)) return
      call check(error, char_resolve_modifier_mask(CHAR_CTL + iachar('A')), 1_int32)
      if (allocated(error)) return
      call check(error, char_resolve_modifier_mask(CHAR_CTL + iachar('[')), 27_int32)
      if (allocated(error)) return
      call check(error, char_resolve_modifier_mask(CHAR_CTL + iachar(' ')), 0_int32)
      if (allocated(error)) return
      call check(error, char_resolve_modifier_mask(CHAR_CTL + iachar('?')), 127_int32)
      if (allocated(error)) return
      call check(error, char_resolve_modifier_mask(CHAR_SHIFT + iachar('a')), int(iachar('A'), int32))
      if (allocated(error)) return
      ! C-% has no ASCII control equivalent: the modifier stays.
      call check(error, char_resolve_modifier_mask(CHAR_CTL + iachar('%')), CHAR_CTL + iachar('%'))
      if (allocated(error)) return
      ! M-C-a keeps meta, resolves control.
      call check(error, char_resolve_modifier_mask(CHAR_META + CHAR_CTL + iachar('a')), CHAR_META + 1_int32)
      if (allocated(error)) return
      ! Non-ASCII base: untouched.
      call check(error, char_resolve_modifier_mask(CHAR_CTL + 233_int32), CHAR_CTL + 233_int32)
      if (allocated(error)) return
      ! char_string drops what cannot be folded.
      call check(error, enc(CHAR_META + iachar('a')) == "a", "M-a not stored as a")
   end subroutine test_modifiers

   subroutine test_predicates(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, char_hexdigit(int(iachar('f'), int32)), 15_int32)
      if (allocated(error)) return
      call check(error, char_hexdigit(int(iachar('G'), int32)), -1_int32)
      if (allocated(error)) return
      call check(error, char_surrogate_p(int(z'DABC', int32)), "surrogate")
      if (allocated(error)) return
      call check(error, char_variation_selector_p(int(z'FE0F', int32)), 16_int32)
      if (allocated(error)) return
      call check(error, char_variation_selector_p(int(z'E0100', int32)), 17_int32)
      if (allocated(error)) return
      call check(error, char_valid_p(MAX_CHAR) .and. .not. char_valid_p(MAX_CHAR + 1), "char_valid_p")
      if (allocated(error)) return
      call check(error, leading_code_p(195_int32) .and. trailing_code_p(169_int32), "lead/trail")
   end subroutine test_predicates

end module test_fm_character
