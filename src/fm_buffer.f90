! fm_buffer.f90 -- buffer objects, the buffer registry and the current buffer.
! Ported from GNU Emacs 31.1 src/buffer.c and src/buffer.h.
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

!> Buffers.
!>
!> Each buffer's text is a gap buffer (`buffer_text_type`, after
!> buffer.h:struct buffer_text) held in one allocatable string.  Positions
!> are 1-based as upstream (BEG = BEG_BYTE = 1).  Because BEG_BYTE is 1,
!> the string index of byte position `n` is simply
!> `n + (n >= gpt_byte ? gap_size : 0)` (buffer.h:BUF_BYTE_ADDRESS).
!>
!> Buffers live in a registry and are referred to by `handle_type`.
!> Lower layers (fm_marker, fm_insdel) work on the registry slot directly:
!> `buffers(i)` with `i` from `resolve_buffer`, and `cur` is the slot of
!> the current buffer.  Everything else should go through the procedures.
!>
!> Deferred: indirect buffers, buffer-local variables, overlays, undo,
!> file visiting and locking, hooks, `erase-buffer` and the marker part
!> of `kill-buffer` (they need fm_insdel/fm_marker; they will live in a
!> submodule to avoid a module cycle).
module fm_buffer
   use, intrinsic :: iso_fortran_env, only: int32, int64
   use fm_error, only: status_type, set_error, FM_GENERIC_ERROR, &
                       FM_BUFFER_READ_ONLY, FM_INVALID_HANDLE
   use fm_handle, only: handle_type, slot_table_type, null_handle
   use fm_character, only: bytes_by_char_head, raw_prev_char_len, &
                           string_char_and_length
   implicit none(type, external)
   private

   !> buffer.h: BEG, BEG_BYTE.
   integer(int64), parameter, public :: BEG = 1, BEG_BYTE = 1
   !> buffer.h: extra gap to allocate when growing, and the minimum gap.
   integer(int64), parameter, public :: GAP_BYTES_DFL = 2000, GAP_BYTES_MIN = 20
   !> buffer.h:BUF_BYTES_MAX on a 64-bit host (MOST_POSITIVE_FIXNUM - 1).
   integer(int64), parameter, public :: BUF_BYTES_MAX = 2_int64**61 - 2

   !> Port of buffer.h:struct buffer_text.  The anchor NUL bytes upstream
   !> keeps at GPT and Z (for C string searches) are not kept.
   type, public :: buffer_text_type
      !> Text and gap; len(beg) = (z_byte - BEG_BYTE) + gap_size.
      character(len=:), allocatable :: beg
      integer(int64) :: gpt = BEG, z = BEG
      integer(int64) :: gpt_byte = BEG_BYTE, z_byte = BEG_BYTE
      integer(int64) :: gap_size = 0
      integer(int64) :: modiff = 1, chars_modiff = 1, save_modiff = 1
      integer(int64) :: overlay_modiff = 1, compact = 1
      integer(int64) :: beg_unchanged = 0, end_unchanged = 0
      integer(int64) :: unchanged_modified = 1, overlay_unchanged_modified = 1
      logical :: inhibit_shrinking = .false.
      logical :: redisplay = .false.
   end type buffer_text_type

   !> Port of the parts of buffer.h:struct buffer used so far.
   type, public :: buffer_type
      character(len=:), allocatable :: name, last_name
      type(buffer_text_type) :: text
      integer(int64) :: pt = BEG, pt_byte = BEG_BYTE
      integer(int64) :: begv = BEG, begv_byte = BEG_BYTE
      integer(int64) :: zv = BEG, zv_byte = BEG_BYTE
      !> BVAR enable_multibyte_characters.
      logical :: multibyte = .true.
      !> BVAR read_only.
      logical :: read_only = .false.
      !> Upstream sets undo_list to t (disabled) for names starting with SPC.
      logical :: undo_enabled = .true.
      logical :: inhibit_buffer_hooks = .false.
      !> Unique per buffer ever created, so caches keyed by slot (which is
      !> reused after kill-buffer) can tell buffers apart.
      integer(int64) :: serial = 0
   end type buffer_type

   !> Registry storage; slot `i` is valid while its handle is live.
   type(buffer_type), allocatable, public :: buffers(:)
   !> Slot of the current buffer, 0 before any buffer exists.
   integer(int64), protected, public :: cur = 0

   type(slot_table_type) :: table
   !> Live buffers in creation order (upstream Vbuffer_alist).
   type(handle_type), allocatable :: alist(:)
   integer(int64) :: alist_count = 0
   integer(int64) :: last_serial = 0

   public :: resolve_buffer, buffer_live_p, current_buffer
   public :: get_buffer, get_buffer_create, generate_new_buffer_name
   public :: buffer_name, buffer_last_name, rename_buffer, buffer_list
   public :: set_buffer, kill_buffer
   public :: buffer_modified_p, restore_buffer_modified_p, &
             buffer_modified_tick, buffer_chars_modified_tick
   public :: barf_if_buffer_read_only, set_buffer_read_only
   public :: modiff_incr, buf_compute_unchanged
   public :: buf_byte_index, buf_fetch_byte, buf_fetch_char, &
             buf_next_char_len, buf_prev_char_len, buf_bytes
   public :: temp_set_point_both

contains

   ! ------------------------------------------------------------------
   ! Registry
   ! ------------------------------------------------------------------

   !> Slot index of buffer `h`, or invalid-handle if it has been killed.
   subroutine resolve_buffer(h, i, status)
      type(handle_type), intent(in) :: h
      integer(int64), intent(out) :: i
      type(status_type), intent(out) :: status

      call table%resolve(h, i, status)
   end subroutine resolve_buffer

   !> Port of buffer.c:Fbuffer_live_p.
   logical function buffer_live_p(h)
      type(handle_type), intent(in) :: h

      buffer_live_p = table%is_live(h)
   end function buffer_live_p

   !> Port of buffer.c:Fcurrent_buffer.  Null before any buffer exists.
   function current_buffer() result(h)
      type(handle_type) :: h
      integer(int64) :: k

      h = null_handle()
      do k = 1, alist_count
         if (alist(k)%index == cur) then
            h = alist(k)
            return
         end if
      end do
   end function current_buffer

   !> Live buffer named `name`, or the null handle.
   !> Port of buffer.c:Fget_buffer (string argument).
   function get_buffer(name) result(h)
      character(len=*), intent(in) :: name
      type(handle_type) :: h
      integer(int64) :: k

      h = null_handle()
      do k = 1, alist_count
         if (buffers(alist(k)%index)%name == name .and. &
             len(buffers(alist(k)%index)%name) == len(name)) then
            h = alist(k)
            return
         end if
      end do
   end function get_buffer

   !> True if no live buffer is named `name`.
   logical function name_free(name)
      character(len=*), intent(in) :: name
      type(handle_type) :: h

      h = get_buffer(name)
      name_free = h%index == 0
   end function name_free

   !> Live buffer named `name`, created (empty, multibyte) if needed.
   !> The first buffer created becomes current.
   !> Port of buffer.c:Fget_buffer_create.
   subroutine get_buffer_create(name, h, status, inhibit_buffer_hooks)
      character(len=*), intent(in) :: name
      type(handle_type), intent(out) :: h
      type(status_type), intent(out) :: status
      logical, intent(in), optional :: inhibit_buffer_hooks
      integer(int64) :: i
      integer :: astat
      type(buffer_type), allocatable :: grown(:)

      h = get_buffer(name)
      if (h%index /= 0) return

      if (len(name) == 0) then
         call set_error(status, FM_GENERIC_ERROR, "Empty string for buffer name is not allowed")
         return
      end if

      call table%acquire(h)
      i = h%index
      if (.not. allocated(buffers)) then
         allocate (buffers(table%capacity()))
      else if (size(buffers, kind=int64) < table%capacity()) then
         allocate (grown(table%capacity()))
         grown(1:size(buffers)) = buffers
         call move_alloc(grown, buffers)
      end if

      ! A fresh slot value resets every field to its default.
      buffers(i) = buffer_type()
      buffers(i)%text%gap_size = 20
      allocate (character(len=buffers(i)%text%gap_size) :: buffers(i)%text%beg, stat=astat)
      if (astat /= 0) then
         call set_error(status, FM_GENERIC_ERROR, "Memory exhausted")
         return
      end if
      last_serial = last_serial + 1
      buffers(i)%serial = last_serial
      buffers(i)%name = name
      buffers(i)%last_name = name
      buffers(i)%undo_enabled = name(1:1) /= " "
      if (present(inhibit_buffer_hooks)) buffers(i)%inhibit_buffer_hooks = inhibit_buffer_hooks

      call alist_append(h)
      if (cur == 0) cur = i
   end subroutine get_buffer_create

   !> A name, based on `name`, that no live buffer has.  `ignore` is a name
   !> that may be returned even if taken.
   !> Port of buffer.c:Fgenerate_new_buffer_name.
   function generate_new_buffer_name(name, ignore) result(newname)
      character(len=*), intent(in) :: name
      character(len=*), intent(in), optional :: ignore
      character(len=:), allocatable :: newname
      character(len=:), allocatable :: genbase
      character(len=24) :: num
      integer(int64) :: count
      real :: r

      if (present(ignore)) then
         if (same(name, ignore)) then
            newname = name
            return
         end if
      end if
      if (name_free(name)) then
         newname = name
         return
      end if

      if (len(name) == 0 .or. name(1:1) /= " ") then
         genbase = name
      else
         ! Hidden buffers get a random suffix first, to find a free name fast.
         call random_number(r)
         write (num, '(i0)') int(r*1000000)
         genbase = name//"-"//trim(num)
         if (name_free(genbase)) then
            newname = genbase
            return
         end if
      end if

      count = 2
      do
         write (num, '(i0)') count
         newname = genbase//"<"//trim(num)//">"
         if (present(ignore)) then
            if (same(newname, ignore)) return
         end if
         if (name_free(newname)) return
         count = count + 1
      end do

   contains

      pure logical function same(a, b)
         character(len=*), intent(in) :: a, b

         same = len(a) == len(b) .and. a == b
      end function same

   end function generate_new_buffer_name

   !> Port of buffer.c:Fbuffer_name.
   subroutine buffer_name(h, name, status)
      type(handle_type), intent(in) :: h
      character(len=:), allocatable, intent(out) :: name
      type(status_type), intent(out) :: status
      integer(int64) :: i

      call resolve_buffer(h, i, status)
      if (.not. status%ok()) return
      name = buffers(i)%name
   end subroutine buffer_name

   !> Port of buffer.c:Fbuffer_last_name.
   subroutine buffer_last_name(h, name, status)
      type(handle_type), intent(in) :: h
      character(len=:), allocatable, intent(out) :: name
      type(status_type), intent(out) :: status
      integer(int64) :: i

      call resolve_buffer(h, i, status)
      if (.not. status%ok()) return
      name = buffers(i)%last_name
   end subroutine buffer_last_name

   !> Rename the current buffer to `newname`; with `unique`, pick a free
   !> variant instead of failing.  `actual` receives the name given.
   !> Port of buffer.c:Frename_buffer.
   subroutine rename_buffer(newname, unique, actual, status)
      character(len=*), intent(in) :: newname
      logical, intent(in) :: unique
      character(len=:), allocatable, intent(out) :: actual
      type(status_type), intent(out) :: status
      type(handle_type) :: tem
      character(len=:), allocatable :: oldname

      if (cur == 0) then
         call set_error(status, FM_INVALID_HANDLE)
         return
      end if
      if (len(newname) == 0) then
         call set_error(status, FM_GENERIC_ERROR, "Empty string is invalid as a buffer name")
         return
      end if

      oldname = buffers(cur)%name
      actual = newname
      tem = get_buffer(newname)
      if (tem%index /= 0) then
         if (.not. unique .and. tem%index == cur) then
            actual = oldname
            return
         end if
         if (unique) then
            actual = generate_new_buffer_name(newname, oldname)
         else
            call set_error(status, FM_GENERIC_ERROR, "Buffer name `"//newname//"' is in use")
            return
         end if
      end if
      buffers(cur)%last_name = oldname
      buffers(cur)%name = actual
   end subroutine rename_buffer

   !> Live buffers in creation order.  Port of buffer.c:Fbuffer_list
   !> (without frame-local ordering).
   function buffer_list() result(list)
      type(handle_type), allocatable :: list(:)

      if (alist_count == 0) then
         allocate (list(0))
      else
         list = alist(1:alist_count)
      end if
   end function buffer_list

   !> Make `h` the current buffer.  Port of buffer.c:Fset_buffer.
   subroutine set_buffer(h, status)
      type(handle_type), intent(in) :: h
      type(status_type), intent(out) :: status
      integer(int64) :: i

      call resolve_buffer(h, i, status)
      if (.not. status%ok()) then
         status%detail = "Selecting deleted buffer"
         return
      end if
      cur = i
   end subroutine set_buffer

   !> Kill buffer `h` and free its text.  If it was current, the most
   !> recent other live, non-hidden buffer becomes current (creating
   !> *scratch* if there is none), as buffer.c:other_buffer_safely does.
   !> Port of the core of buffer.c:Fkill_buffer; hooks, processes,
   !> windows, markers and indirect buffers are not handled yet.
   subroutine kill_buffer(h, status)
      type(handle_type), intent(in) :: h
      type(status_type), intent(out) :: status
      type(handle_type) :: other
      integer(int64) :: i, k
      logical :: was_current

      call resolve_buffer(h, i, status)
      if (.not. status%ok()) return
      was_current = i == cur

      buffers(i)%last_name = buffers(i)%name
      call alist_remove(h)
      call table%release(h, status)
      if (.not. status%ok()) return
      if (allocated(buffers(i)%text%beg)) deallocate (buffers(i)%text%beg)
      buffers(i) = buffer_type()

      if (.not. was_current) return
      cur = 0
      do k = alist_count, 1, -1
         if (buffers(alist(k)%index)%name(1:1) /= " ") then
            cur = alist(k)%index
            return
         end if
      end do
      call get_buffer_create("*scratch*", other, status)
      if (status%ok()) cur = other%index
   end subroutine kill_buffer

   subroutine alist_append(h)
      type(handle_type), intent(in) :: h
      type(handle_type), allocatable :: grown(:)

      if (.not. allocated(alist)) allocate (alist(16))
      if (alist_count == size(alist, kind=int64)) then
         allocate (grown(2*size(alist)))
         grown(1:alist_count) = alist(1:alist_count)
         call move_alloc(grown, alist)
      end if
      alist_count = alist_count + 1
      alist(alist_count) = h
   end subroutine alist_append

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

   ! ------------------------------------------------------------------
   ! Modification state
   ! ------------------------------------------------------------------

   !> Advance modification counter `a` logarithmically in `len`; `old`
   !> receives its previous value.  Port of lisp.h:modiff_incr.
   pure subroutine modiff_incr(a, len, old)
      integer(int64), intent(inout) :: a
      integer(int64), intent(in) :: len
      integer(int64), intent(out), optional :: old

      if (present(old)) old = a
      if (len == 0) then
         a = a + 1
      else
         ! elogb (len) + 1, i.e. the bit length of len.
         a = a + (bit_size(len) - leadz(len))
      end if
   end subroutine modiff_incr

   !> Port of buffer.h:BUF_COMPUTE_UNCHANGED.
   pure subroutine buf_compute_unchanged(b, start, end)
      type(buffer_type), intent(inout) :: b
      integer(int64), intent(in) :: start, end

      associate (t => b%text)
         if (t%unchanged_modified == t%modiff .and. &
             t%overlay_unchanged_modified == t%overlay_modiff) then
            t%beg_unchanged = start - BEG
            t%end_unchanged = t%z - end
         else
            if (t%z - end < t%end_unchanged) t%end_unchanged = t%z - end
            if (start - BEG < t%beg_unchanged) t%beg_unchanged = start - BEG
         end if
      end associate
   end subroutine buf_compute_unchanged

   !> Port of buffer.c:Fbuffer_modified_p (the `autosaved' state is not
   !> tracked yet).
   subroutine buffer_modified_p(h, modified, status)
      type(handle_type), intent(in) :: h
      logical, intent(out) :: modified
      type(status_type), intent(out) :: status
      integer(int64) :: i

      modified = .false.
      call resolve_buffer(h, i, status)
      if (.not. status%ok()) return
      modified = buffers(i)%text%save_modiff < buffers(i)%text%modiff
   end subroutine buffer_modified_p

   !> Mark the current buffer modified or unmodified.
   !> Port of buffer.c:Frestore_buffer_modified_p (also the behaviour of
   !> Fset_buffer_modified_p, minus mode-line updates and file locks).
   subroutine restore_buffer_modified_p(flag)
      logical, intent(in) :: flag

      if (cur == 0) return
      associate (t => buffers(cur)%text)
         if (.not. flag) then
            t%save_modiff = t%modiff
         else if (t%save_modiff >= t%modiff) then
            call modiff_incr(t%modiff, 1_int64, t%save_modiff)
         end if
      end associate
   end subroutine restore_buffer_modified_p

   !> Port of buffer.c:Fbuffer_modified_tick.
   subroutine buffer_modified_tick(h, tick, status)
      type(handle_type), intent(in) :: h
      integer(int64), intent(out) :: tick
      type(status_type), intent(out) :: status
      integer(int64) :: i

      tick = 0
      call resolve_buffer(h, i, status)
      if (.not. status%ok()) return
      tick = buffers(i)%text%modiff
   end subroutine buffer_modified_tick

   !> Port of buffer.c:Fbuffer_chars_modified_tick.
   subroutine buffer_chars_modified_tick(h, tick, status)
      type(handle_type), intent(in) :: h
      integer(int64), intent(out) :: tick
      type(status_type), intent(out) :: status
      integer(int64) :: i

      tick = 0
      call resolve_buffer(h, i, status)
      if (.not. status%ok()) return
      tick = buffers(i)%text%chars_modiff
   end subroutine buffer_chars_modified_tick

   !> Fail with buffer-read-only if the current buffer is read-only.
   !> Port of buffer.c:Fbarf_if_buffer_read_only (`inhibit-read-only',
   !> variable and text property, not supported yet).
   subroutine barf_if_buffer_read_only(status)
      type(status_type), intent(out) :: status

      if (cur == 0) return
      if (buffers(cur)%read_only) then
         call set_error(status, FM_BUFFER_READ_ONLY, buffers(cur)%name)
      end if
   end subroutine barf_if_buffer_read_only

   !> Set BVAR read_only of buffer `h` (the `buffer-read-only' variable).
   subroutine set_buffer_read_only(h, flag, status)
      type(handle_type), intent(in) :: h
      logical, intent(in) :: flag
      type(status_type), intent(out) :: status
      integer(int64) :: i

      call resolve_buffer(h, i, status)
      if (.not. status%ok()) return
      buffers(i)%read_only = flag
   end subroutine set_buffer_read_only

   ! ------------------------------------------------------------------
   ! Text access (buffer.h macros)
   ! ------------------------------------------------------------------

   !> String index of byte position `bytepos`.
   !> Port of buffer.h:BUF_BYTE_ADDRESS.
   pure integer(int64) function buf_byte_index(b, bytepos)
      type(buffer_type), intent(in) :: b
      integer(int64), intent(in) :: bytepos

      buf_byte_index = bytepos
      if (bytepos >= b%text%gpt_byte) buf_byte_index = bytepos + b%text%gap_size
   end function buf_byte_index

   !> Byte (0..255) at `bytepos`.  Port of buffer.h:BUF_FETCH_BYTE.
   pure integer(int32) function buf_fetch_byte(b, bytepos)
      type(buffer_type), intent(in) :: b
      integer(int64), intent(in) :: bytepos
      integer(int64) :: k

      k = buf_byte_index(b, bytepos)
      buf_fetch_byte = ichar(b%text%beg(k:k), kind=int32)
   end function buf_fetch_byte

   !> Character at `bytepos`.  Port of buffer.h:BUF_FETCH_CHAR.
   pure integer(int32) function buf_fetch_char(b, bytepos)
      type(buffer_type), intent(in) :: b
      integer(int64), intent(in) :: bytepos
      integer(int32) :: n

      if (b%multibyte) then
         call string_char_and_length(b%text%beg, buf_byte_index(b, bytepos), buf_fetch_char, n)
      else
         buf_fetch_char = buf_fetch_byte(b, bytepos)
      end if
   end function buf_fetch_char

   !> Byte length of the character at `bytepos`.
   !> Port of buffer.h:buf_next_char_len.
   pure integer(int32) function buf_next_char_len(b, bytepos)
      type(buffer_type), intent(in) :: b
      integer(int64), intent(in) :: bytepos

      buf_next_char_len = bytes_by_char_head(buf_fetch_byte(b, bytepos))
   end function buf_next_char_len

   !> Byte length of the character before `bytepos`.
   !> Port of buffer.h:buf_prev_char_len.
   pure integer(int32) function buf_prev_char_len(b, bytepos)
      type(buffer_type), intent(in) :: b
      integer(int64), intent(in) :: bytepos
      integer(int64) :: k

      k = bytepos
      if (bytepos > b%text%gpt_byte) k = bytepos + b%text%gap_size
      buf_prev_char_len = raw_prev_char_len(b%text%beg, k)
   end function buf_prev_char_len

   !> The bytes from `from_byte` up to (not including) `to_byte`, joined
   !> across the gap.  Not a port; the byte-copy core of
   !> editfns.c:make_buffer_string_both.
   pure function buf_bytes(b, from_byte, to_byte) result(s)
      type(buffer_type), intent(in) :: b
      integer(int64), intent(in) :: from_byte, to_byte
      character(len=:), allocatable :: s
      integer(int64) :: g, n

      associate (t => b%text)
         g = t%gpt_byte
         n = max(0_int64, to_byte - from_byte)
         allocate (character(len=n) :: s)
         if (n == 0) return
         if (to_byte <= g) then
            s = t%beg(from_byte:to_byte - 1)
         else if (from_byte >= g) then
            s = t%beg(from_byte + t%gap_size:to_byte - 1 + t%gap_size)
         else
            s = t%beg(from_byte:g - 1)//t%beg(g + t%gap_size:to_byte - 1 + t%gap_size)
         end if
      end associate
   end function buf_bytes

   !> Set point of `b` without any text-property processing.
   !> Port of intervals.c:temp_set_point_both.
   pure subroutine temp_set_point_both(b, charpos, bytepos)
      type(buffer_type), intent(inout) :: b
      integer(int64), intent(in) :: charpos, bytepos

      b%pt = charpos
      b%pt_byte = bytepos
   end subroutine temp_set_point_both

end module fm_buffer
