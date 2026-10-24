! itree_fm.f90 -- overlays: the intervals they cover, and buffer.c's overlay functions.
! Ported from GNU Emacs 31.1 src/itree.c and src/buffer.c.
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

!> Overlays.  An overlay is a registry object referenced by a handle: the
!> buffer it is in (the null handle once deleted), the interval it
!> covers (itree.c's struct itree_node: begin, end, front_advance,
!> rear_advance) and its property list.  Upstream keeps a buffer's
!> overlays in an interval tree (itree.c); here they are one array of
!> nodes, scanned in order of their start, which gives the same results
!> for the few overlays a buffer has.  insdel_fm moves them with the text
!> (`adjust_overlays_for_insert' and `adjust_overlays_for_delete', with
!> itree.c's insert_gap and delete_gap rules), kill-buffer deletes them,
!> and redisplay merges their `face' properties (xfaces_fm).
!>
!> No garbage collector frees overlays: `free_overlay' deletes an overlay
!> and releases its handle.
!>
!> Deferred: indirect buffers, overlay modification hooks
!> (`modification_hooks', `insert_in_front_hooks', `insert_behind_hooks'),
!> the display-related properties redisplay would use (`invisible',
!> `display', `before_string', `after_string', `window', `mouse_face',
!> `intangible'), priorities given as (PRIMARY . SECONDARY) pairs (no
!> dotted pairs), `overlay_lists', `overlay_recenter', `remove_overlays'
!> (subr.el), `copy_overlay' and the `get_char_property' family.
module itree_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type, slot_table_type, null_handle
    use lisp_fm, only: value_type, nil, nilp, fixnump, xfixnum, make_list, list_elements
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use buffer_fm, only: buffers, cur, buffer_live_p, buffer_handle, resolve_buffer, modiff_incr, &
                         buf_compute_unchanged, BUF_BEG => BEG
    use intervals_fm, only: textget
    implicit none(type, external)
    private

    public :: make_overlay, move_overlay, delete_overlay, free_overlay, delete_all_overlays, overlayp, &
              overlay_start, overlay_end, overlay_buffer, overlay_get, overlay_put, overlay_properties, &
              overlays_at, overlays_in, next_overlay_change, previous_overlay_change, sort_overlays, &
              adjust_overlays_for_insert, adjust_overlays_for_delete, buffer_has_overlays, overlay_priority, &
              compare_overlays

    !> An overlay: struct Lisp_Overlay with its itree_node.
    type :: overlay_type
        !> The buffer the overlay is in; the null handle if deleted.
        type(handle_type) :: buffer
        integer(int64) :: begin = 0, end = 0
        logical :: front_advance = .false., rear_advance = .false.
        !> The property list.
        type(value_type) :: plist
    end type overlay_type

    type(slot_table_type) :: table
    type(overlay_type), allocatable :: overlays(:)

contains

    ! ------------------------------------------------------------------
    ! Objects
    ! ------------------------------------------------------------------

    !> A new overlay, in no buffer (buffer.c:build_overlay).
    subroutine build_overlay(ov, front_advance, rear_advance)
        type(handle_type), intent(out) :: ov
        logical, intent(in) :: front_advance, rear_advance
        type(overlay_type), allocatable :: grown(:)
        call table%acquire(ov)
        if (.not. allocated(overlays)) then
            allocate (overlays(table%capacity()))
        else if (size(overlays, kind=int64) < table%capacity()) then
            allocate (grown(table%capacity()))
            grown(1:size(overlays)) = overlays
            call move_alloc(grown, overlays)
        end if
        overlays(ov%index) = overlay_type()
        overlays(ov%index)%front_advance = front_advance
        overlays(ov%index)%rear_advance = rear_advance
    end subroutine build_overlay

    !> Return true if OBJECT is an overlay.  Port of buffer.c:Foverlayp
    !> (a live overlay handle).
    logical function overlayp(object)
        type(handle_type), intent(in) :: object
        overlayp = table%is_live(object)
    end function overlayp

    subroutine check_overlay(ov, status)
        type(handle_type), intent(in) :: ov
        type(status_type), intent(out) :: status
        if (.not. overlayp(ov)) call set_error(status, FM_WRONG_TYPE_ARGUMENT, "overlayp")
    end subroutine check_overlay

    !> Whether overlay slot I is in buffer slot B.
    logical function in_buffer(i, b)
        integer(int64), intent(in) :: i, b
        in_buffer = .false.
        if (b <= 0) return
        if (overlays(i)%buffer%index /= b) return
        in_buffer = buffer_live_p(overlays(i)%buffer)
    end function in_buffer

    !> The live overlay slots of buffer slot B, sorted by start (the order
    !> of an ascending ITREE_FOREACH).
    subroutine buffer_nodes(b, idx)
        integer(int64), intent(in) :: b
        integer(int64), allocatable, intent(out) :: idx(:)
        integer(int64) :: i, n, k, tmp
        if (.not. allocated(overlays)) then
            allocate (idx(0))
            return
        end if
        n = 0
        allocate (idx(size(overlays)))
        do i = 1, size(overlays, kind=int64)
            if (.not. table%is_live(table%handle_of(i))) cycle
            if (.not. in_buffer(i, b)) cycle
            n = n + 1
            idx(n) = i
        end do
        idx = idx(1:n)
        ! Insertion sort by begin (stable).
        do i = 2, n
            tmp = idx(i)
            k = i - 1
            do while (k >= 1)
                if (overlays(idx(k))%begin <= overlays(tmp)%begin) exit
                idx(k + 1) = idx(k)
                k = k - 1
            end do
            idx(k + 1) = tmp
        end do
    end subroutine buffer_nodes

    !> Whether the current buffer has overlays (buffer.c:buffer_has_overlays).
    logical function buffer_has_overlays()
        integer(int64) :: i
        buffer_has_overlays = .false.
        if (.not. allocated(overlays) .or. cur == 0) return
        do i = 1, size(overlays, kind=int64)
            if (.not. table%is_live(table%handle_of(i))) cycle
            if (in_buffer(i, cur)) then
                buffer_has_overlays = .true.
                return
            end if
        end do
    end function buffer_has_overlays

    !> Mark a section of buffer slot B as needing redisplay because of
    !> overlays changes.  Port of buffer.c:modify_overlay.
    subroutine modify_overlay(b, start_in, end_in)
        integer(int64), intent(in) :: b, start_in, end_in
        integer(int64) :: start, end
        start = min(start_in, end_in)
        end = max(start_in, end_in)
        call buf_compute_unchanged(buffers(b), start, end)
        buffers(b)%text%redisplay = .true.
        call modiff_incr(buffers(b)%text%overlay_modiff, 1_int64)
    end subroutine modify_overlay

    !> Create a new overlay with range BEG to END in BUFFER and return it.
    !> If omitted, BUFFER defaults to the current buffer.
    !> BEG and END may be integers or markers.
    !> The fourth arg FRONT_ADVANCE, if true, makes the marker
    !> for the front of the overlay advance when text is inserted there
    !> (which means the text *is not* included in the overlay).
    !> The fifth arg REAR_ADVANCE, if true, makes the marker
    !> for the rear of the overlay advance when text is inserted there
    !> (which means the text *is* included in the overlay).  Port of
    !> buffer.c:Fmake_overlay (BEG and END are positions).
    subroutine make_overlay(ov, beg, end, status, buffer, front_advance, rear_advance)
        type(handle_type), intent(out) :: ov
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        logical, intent(in), optional :: front_advance, rear_advance
        integer(int64) :: b, obeg, oend, lo, hi
        logical :: fa, ra
        ov = null_handle()
        b = cur
        if (present(buffer)) then
            if (buffer%index /= 0) then
                if (.not. buffer_live_p(buffer)) then
                    call set_error(status, FM_GENERIC_ERROR, "Attempt to create overlay in a dead buffer")
                    return
                end if
                b = buffer%index
            end if
        end if
        if (b == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to create overlay in a dead buffer")
            return
        end if
        fa = .false.
        if (present(front_advance)) fa = front_advance
        ra = .false.
        if (present(rear_advance)) ra = rear_advance
        lo = min(beg, end)
        hi = max(beg, end)
        obeg = clip(BUF_BEG, lo, buffers(b)%text%z)
        oend = clip(obeg, hi, buffers(b)%text%z)
        call build_overlay(ov, fa, ra)
        overlays(ov%index)%buffer = buffer_handle(b)
        overlays(ov%index)%begin = obeg
        overlays(ov%index)%end = oend
        ! We don't need to redisplay the region covered by the overlay,
        ! because the overlay has no properties at the moment.
    end subroutine make_overlay

    pure integer(int64) function clip(lo, x, hi)
        integer(int64), intent(in) :: lo, x, hi
        clip = max(lo, min(x, hi))
    end function clip

    !> Set the endpoints of OVERLAY to BEG and END in BUFFER.
    !> If BUFFER is omitted, leave OVERLAY in the same buffer it inhabits now.
    !> If BUFFER is omitted, and OVERLAY is in no buffer, put it in the current
    !> buffer.  Port of buffer.c:Fmove_overlay.
    subroutine move_overlay(overlay, beg, end, status, buffer)
        type(handle_type), intent(in) :: overlay
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        integer(int64) :: b, ob, n_beg, n_end, o_beg, o_end, lo, hi, i
        call check_overlay(overlay, status)
        if (.not. status%ok()) return
        i = overlay%index
        b = 0
        if (present(buffer)) b = buffer%index
        if (b == 0) then
            if (buffer_live_p(overlays(i)%buffer)) b = overlays(i)%buffer%index
        end if
        if (b == 0) b = cur
        if (b == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Attempt to move overlay to a dead buffer")
            return
        end if
        if (present(buffer)) then
            if (buffer%index /= 0 .and. .not. buffer_live_p(buffer)) then
                call set_error(status, FM_GENERIC_ERROR, "Attempt to move overlay to a dead buffer")
                return
            end if
        end if
        lo = min(beg, end)
        hi = max(beg, end)
        ob = 0
        if (buffer_live_p(overlays(i)%buffer)) ob = overlays(i)%buffer%index
        n_beg = clip(BUF_BEG, lo, buffers(b)%text%z)
        n_end = clip(n_beg, hi, buffers(b)%text%z)
        o_beg = 0
        o_end = 0
        if (ob /= 0) then
            o_beg = overlays(i)%begin
            o_end = overlays(i)%end
        end if
        overlays(i)%buffer = buffer_handle(b)
        overlays(i)%begin = n_beg
        overlays(i)%end = n_end
        if (b /= ob) then
            ! If the overlay has changed buffers, do a thorough redisplay.
            ! Redisplay where the overlay was.
            if (ob /= 0) call modify_overlay(ob, o_beg, o_end)
            ! Redisplay where the overlay is going to be.
            call modify_overlay(b, n_beg, n_end)
        else
            ! Redisplay the area the overlay has just left, or just
            ! enclosed.
            if (o_beg == n_beg) then
                call modify_overlay(b, o_end, n_end)
            else if (o_end == n_end) then
                call modify_overlay(b, o_beg, n_beg)
            else
                call modify_overlay(b, min(o_beg, n_beg), max(o_end, n_end))
            end if
        end if
        ! Delete the overlay if it is empty after clipping and has the
        ! evaporate property.
        if (n_beg == n_end) then
            if (.not. nilp(overlay_get(overlay, "evaporate"))) call drop_overlay(i)
        end if
    end subroutine move_overlay

    !> Remove overlay slot I from its buffer (buffer.c:drop_overlay).
    subroutine drop_overlay(i)
        integer(int64), intent(in) :: i
        if (buffer_live_p(overlays(i)%buffer)) then
            call modify_overlay(overlays(i)%buffer%index, overlays(i)%begin, overlays(i)%end)
        end if
        overlays(i)%buffer = null_handle()
    end subroutine drop_overlay

    !> Delete the overlay OVERLAY from its buffer.  Port of
    !> buffer.c:Fdelete_overlay.
    subroutine delete_overlay(overlay)
        type(handle_type), intent(in) :: overlay
        if (.not. overlayp(overlay)) return
        if (.not. buffer_live_p(overlays(overlay%index)%buffer)) return
        call drop_overlay(overlay%index)
    end subroutine delete_overlay

    !> Delete OVERLAY and release its handle, which becomes stale.  Not a
    !> port: upstream relies on GC.
    subroutine free_overlay(overlay)
        type(handle_type), intent(in) :: overlay
        type(status_type) :: st
        if (.not. overlayp(overlay)) return
        call delete_overlay(overlay)
        overlays(overlay%index) = overlay_type()
        call table%release(overlay, st)
    end subroutine free_overlay

    !> Delete all overlays of BUFFER.
    !> BUFFER omitted means delete all overlays of the current
    !> buffer.  Port of buffer.c:Fdelete_all_overlays (with
    !> delete_all_overlays).
    subroutine delete_all_overlays(buffer)
        type(handle_type), intent(in), optional :: buffer
        integer(int64), allocatable :: idx(:)
        integer(int64) :: b
        integer :: k
        b = cur
        if (present(buffer)) then
            b = 0
            if (buffer_live_p(buffer)) b = buffer%index
        end if
        if (b == 0) return
        call buffer_nodes(b, idx)
        do k = 1, size(idx)
            call drop_overlay(idx(k))
        end do
    end subroutine delete_all_overlays

    !> Return the position at which OVERLAY starts (0 if it is deleted,
    !> upstream's nil).  Port of buffer.c:Foverlay_start.
    integer(int64) function overlay_start(overlay)
        type(handle_type), intent(in) :: overlay
        overlay_start = 0
        if (.not. overlayp(overlay)) return
        if (buffer_live_p(overlays(overlay%index)%buffer)) overlay_start = overlays(overlay%index)%begin
    end function overlay_start

    !> Return the position at which OVERLAY ends (0 if it is deleted,
    !> upstream's nil).  Port of buffer.c:Foverlay_end.
    integer(int64) function overlay_end(overlay)
        type(handle_type), intent(in) :: overlay
        overlay_end = 0
        if (.not. overlayp(overlay)) return
        if (buffer_live_p(overlays(overlay%index)%buffer)) overlay_end = overlays(overlay%index)%end
    end function overlay_end

    !> Return the buffer OVERLAY belongs to.
    !> Return the null handle if OVERLAY has been deleted.  Port of
    !> buffer.c:Foverlay_buffer.
    function overlay_buffer(overlay) result(b)
        type(handle_type), intent(in) :: overlay
        type(handle_type) :: b
        b = null_handle()
        if (.not. overlayp(overlay)) return
        if (buffer_live_p(overlays(overlay%index)%buffer)) b = overlays(overlay%index)%buffer
    end function overlay_buffer

    !> Return a list of the properties on OVERLAY.
    !> This is a copy of OVERLAY's plist; modifying its conses has no effect on
    !> OVERLAY.  Port of buffer.c:Foverlay_properties.
    function overlay_properties(overlay) result(plist)
        type(handle_type), intent(in) :: overlay
        type(value_type) :: plist
        if (overlayp(overlay)) plist = overlays(overlay%index)%plist
    end function overlay_properties

    !> Get the property of overlay OVERLAY with property name PROP.  Port
    !> of buffer.c:Foverlay_get.
    function overlay_get(overlay, prop) result(val)
        type(handle_type), intent(in) :: overlay
        character(len=*), intent(in) :: prop
        type(value_type) :: val
        if (.not. overlayp(overlay)) return
        val = textget(overlays(overlay%index)%plist, sym(prop))
    end function overlay_get

    !> Set one property of overlay OVERLAY: give property PROP value VALUE.
    !> Port of buffer.c:Foverlay_put.
    subroutine overlay_put(overlay, prop, value, status)
        type(handle_type), intent(in) :: overlay
        character(len=*), intent(in) :: prop
        type(value_type), intent(in) :: value
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: elems(:), grown(:)
        type(value_type) :: key
        integer :: k, n
        logical :: changed, found
        integer(int64) :: i
        call check_overlay(overlay, status)
        if (.not. status%ok()) return
        i = overlay%index
        key = sym(prop)
        call list_elements(overlays(i)%plist, elems)
        found = .false.
        changed = .false.
        do k = 1, size(elems) - 1, 2
            if (value_eq(elems(k), key)) then
                changed = .not. value_eq(elems(k + 1), value)
                elems(k + 1) = value
                found = .true.
                exit
            end if
        end do
        if (.not. found) then
            ! It wasn't in the list, so add it to the front.
            changed = .not. nilp(value)
            n = size(elems)
            allocate (grown(n + 2))
            grown(1) = key
            grown(2) = value
            grown(3:n + 2) = elems
            call move_alloc(grown, elems)
        end if
        overlays(i)%plist = make_list(elems)
        if (buffer_live_p(overlays(i)%buffer)) then
            if (changed) call modify_overlay(overlays(i)%buffer%index, overlays(i)%begin, overlays(i)%end)
            if (prop == "evaporate" .and. .not. nilp(value) .and. overlays(i)%begin == overlays(i)%end) then
                call delete_overlay(overlay)
            end if
        end if
    end subroutine overlay_put

    ! ------------------------------------------------------------------
    ! Finding overlays
    ! ------------------------------------------------------------------

    !> Find all the overlays in the current buffer that overlap the range
    !> BEG-END, or are empty at BEG, or are empty at END provided END
    !> denotes the position at the end of the accessible part of the buffer.
    !>
    !> Return them in VEC, and in NEXT the next position after END where
    !> an overlay starts, or Z if there are no more overlays after.
    !> EMPTY: include empty overlays.  Port of buffer.c:overlays_in.
    subroutine overlays_in_1(beg, end, empty, trailing, vec, next)
        integer(int64), intent(in) :: beg, end
        logical, intent(in) :: empty, trailing
        type(handle_type), allocatable, intent(out) :: vec(:)
        integer(int64), intent(out) :: next
        integer(int64), allocatable :: idx(:)
        integer(int64) :: z, search_end, nb, ne
        integer :: k, n
        z = buffers(cur)%text%z
        next = z
        call buffer_nodes(cur, idx)
        ! Extend the search range if overlays beginning at Z are wanted.
        search_end = z
        if (end >= z .and. (empty .or. trailing)) search_end = search_end + 1
        allocate (vec(size(idx)))
        n = 0
        do k = 1, size(idx)
            nb = overlays(idx(k))%begin
            ne = overlays(idx(k))%end
            ! ITREE_FOREACH (BEG, SEARCH_END): nodes intersecting it.
            if (.not. intersects(nb, ne, beg, search_end)) cycle
            if (nb > end) then
                next = min(next, nb)
                exit
            else if (nb == end) then
                next = nb
                if ((.not. empty .or. end < z) .and. beg < end) exit
                if (empty .and. nb /= ne) cycle
            end if
            if (.not. empty .and. nb == ne) cycle
            n = n + 1
            vec(n) = table%handle_of(idx(k))
        end do
        vec = vec(1:n)
    end subroutine overlays_in_1

    !> Whether node [NB, NE] intersects the search range [B, E]
    !> (itree.c:itree_node_intersects).
    pure logical function intersects(nb, ne, b, e)
        integer(int64), intent(in) :: nb, ne, b, e
        intersects = (b < ne .and. nb < e) .or. (nb == ne .and. b == nb)
    end function intersects

    !> Return a list of the overlays that contain the character at POS.
    !> If SORTED is true, then sort them by decreasing priority.
    !>
    !> Zero-length (a.k.a. "empty") overlays that start and stop at POS are not
    !> included in the return value.  Use `overlays_in' if empty overlays are of
    !> interest.
    !>
    !> This function can return overlays outside of the current narrowing of
    !> the buffer if POS is outside of the narrowing.  Port of
    !> buffer.c:Foverlays_at (with overlays_at; NEXT, if present, is the
    !> next position after POS where an overlay starts).
    subroutine overlays_at(pos, vec, sorted, next)
        integer(int64), intent(in) :: pos
        type(handle_type), allocatable, intent(out) :: vec(:)
        logical, intent(in), optional :: sorted
        integer(int64), intent(out), optional :: next
        integer(int64) :: nxt
        integer :: k, n
        type(handle_type) :: tmp
        if (cur == 0) then
            allocate (vec(0))
            if (present(next)) next = 0
            return
        end if
        call overlays_in_1(pos, pos + 1, .false., .true., vec, nxt)
        if (present(next)) next = nxt
        if (.not. present(sorted)) return
        if (.not. sorted) return
        call sort_overlays(vec)
        ! The list should be in decreasing order of priority.
        n = size(vec)
        do k = 1, n/2
            tmp = vec(k)
            vec(k) = vec(n + 1 - k)
            vec(n + 1 - k) = tmp
        end do
    end subroutine overlays_at

    !> Return a list of the overlays that overlap the region BEG ... END.
    !> Overlap means that at least one character between BEG and END is contained
    !> within the overlay.  Note that this excludes the character at (i.e., after)
    !> END.
    !>
    !> Empty overlays do not contain any characters, so they are included in the
    !> result if they are located at BEG, between BEG and END, or at END provided
    !> END denotes the position at the end of the buffer.  (If BEG and END are the
    !> same buffer position, an empty overlay at BEG will also be at END, but it
    !> still qualifies because it's at BEG.)
    !>
    !> The resulting list of overlays is in an arbitrary unpredictable order.
    !>
    !> This function can return overlays outside of the current narrowing of
    !> the buffer if BEG and/or END are outside of the narrowing.  Port of
    !> buffer.c:Foverlays_in.
    subroutine overlays_in(beg, end, vec)
        integer(int64), intent(in) :: beg, end
        type(handle_type), allocatable, intent(out) :: vec(:)
        integer(int64) :: next
        if (cur == 0) then
            allocate (vec(0))
            return
        end if
        call overlays_in_1(beg, end, .true., .false., vec, next)
    end subroutine overlays_in

    !> Return the next position after POS where an overlay starts or ends.
    !> If there are no overlay boundaries from POS to (point_max),
    !> the value is (point_max).  Port of buffer.c:Fnext_overlay_change
    !> (with next_overlay_change).
    integer(int64) function next_overlay_change(pos) result(next)
        integer(int64), intent(in) :: pos
        integer(int64), allocatable :: idx(:)
        integer(int64) :: nb, ne
        integer :: k
        next = 0
        if (cur == 0) return
        next = buffers(cur)%zv
        call buffer_nodes(cur, idx)
        do k = 1, size(idx)
            nb = overlays(idx(k))%begin
            ne = overlays(idx(k))%end
            if (.not. intersects(nb, ne, pos, next)) cycle
            if (nb > pos) then
                ! NB must be the least upper bound of POS, because the
                ! search is limited to [POS, NEXT).
                if (nb < next) next = nb
                exit
            else if (nb < ne .and. ne < next) then
                next = ne
            end if
        end do
    end function next_overlay_change

    !> Return the previous position before POS where an overlay starts or ends.
    !> If there are no overlay boundaries from (point_min) to POS,
    !> the value is (point_min).  Port of buffer.c:Fprevious_overlay_change
    !> (with previous_overlay_change).
    integer(int64) function previous_overlay_change(pos) result(prev)
        integer(int64), intent(in) :: pos
        integer(int64), allocatable :: idx(:)
        integer(int64) :: nb, ne
        integer :: k
        prev = 0
        if (cur == 0) return
        prev = buffers(cur)%begv
        call buffer_nodes(cur, idx)
        do k = size(idx), 1, -1
            nb = overlays(idx(k))%begin
            ne = overlays(idx(k))%end
            if (.not. intersects(nb, ne, prev, pos)) cycle
            if (ne < pos) then
                prev = max(prev, ne)
            else
                prev = max(prev, nb)
            end if
        end do
    end function previous_overlay_change

    !> The priority of OVERLAY in PRIORITY and its secondary priority in
    !> SPRIORITY: its `priority' property if a fixnum, else 0
    !> (buffer.c:make_sortvec_item).
    subroutine overlay_priority(overlay, priority, spriority)
        type(handle_type), intent(in) :: overlay
        integer(int64), intent(out) :: priority, spriority
        type(value_type) :: tem
        priority = 0
        spriority = 0
        tem = overlay_get(overlay, "priority")
        if (fixnump(tem)) priority = xfixnum(tem)
    end subroutine overlay_priority

    !> Sort an array of overlays by priority, increasing.  Invalid
    !> (deleted) overlays are dropped.  Port of buffer.c:sort_overlays
    !> with compare_overlays (no window).
    subroutine sort_overlays(vec)
        type(handle_type), allocatable, intent(inout) :: vec(:)
        integer(int64), allocatable :: prio(:), sprio(:)
        type(handle_type) :: tmp
        integer(int64) :: tp, ts
        integer :: i, k, n
        ! Put the valid and relevant overlays first.
        n = 0
        do i = 1, size(vec)
            if (overlay_start(vec(i)) > 0 .and. overlay_end(vec(i)) > 0) then
                n = n + 1
                vec(n) = vec(i)
            end if
        end do
        vec = vec(1:n)
        allocate (prio(n), sprio(n))
        do i = 1, n
            call overlay_priority(vec(i), prio(i), sprio(i))
        end do
        ! Sort the overlays into the proper order: increasing priority.
        do i = 2, n
            tmp = vec(i)
            tp = prio(i)
            ts = sprio(i)
            k = i - 1
            do while (k >= 1)
                if (compare_overlays(prio(k), sprio(k), overlay_start(vec(k)), overlay_end(vec(k)), vec(k)%index, &
                                     tp, ts, overlay_start(tmp), overlay_end(tmp), tmp%index) <= 0) exit
                vec(k + 1) = vec(k)
                prio(k + 1) = prio(k)
                sprio(k + 1) = sprio(k)
                k = k - 1
            end do
            vec(k + 1) = tmp
            prio(k + 1) = tp
            sprio(k + 1) = ts
        end do
    end subroutine sort_overlays

    !> 1 if the first overlay should take precedence, -1 if the second
    !> should, 0 if they are the same: overlays with priorities P1 and
    !> P2, secondary priorities S1 and S2, bounds B1-E1 and B2-E2 and
    !> tie-breaking ids K1 and K2 (the same id for the same overlay).
    !> Port of buffer.c:compare_overlays (on struct sortvec items).
    pure integer function compare_overlays(p1, s1, b1, e1, k1, p2, s2, b2, e2, k2) result(c)
        integer(int64), intent(in) :: p1, s1, b1, e1, k1, p2, s2, b2, e2, k2
        if (p1 /= p2) then
            c = merge(-1, 1, p1 < p2)
        else if (b1 < b2) then
            ! If the priority is equal, give precedence to the one not
            ! covered by the other.  If neither covers the other, obey
            ! spriority.
            c = merge(1, -1, e1 < e2 .and. s1 > s2)
        else if (b1 > b2) then
            c = merge(-1, 1, e1 > e2 .and. s1 < s2)
        else if (e1 /= e2) then
            c = merge(-1, 1, e2 < e1)
        else if (s1 /= s2) then
            c = merge(-1, 1, s1 < s2)
        else if (k1 == k2) then
            c = 0
        else
            ! Avoid the non-determinism of qsort by choosing an arbitrary
            ! ordering between "equal" overlays.
            c = merge(-1, 1, k1 < k2)
        end if
    end function compare_overlays

    ! ------------------------------------------------------------------
    ! Moving with the text
    ! ------------------------------------------------------------------

    !> Insert a gap at POS of length LENGTH expanding all intervals
    !> intersecting it, while respecting their rear_advance and
    !> front_advance setting.
    !>
    !> If BEFORE_MARKERS is true, all overlays beginning/ending at POS
    !> are treated as if their front_advance/rear_advance was true.  Port
    !> of buffer.c:adjust_overlays_for_insert with itree.c:itree_insert_gap
    !> (the current buffer).
    subroutine adjust_overlays_for_insert(pos, length, before_markers)
        integer(int64), intent(in) :: pos, length
        logical, intent(in) :: before_markers
        integer(int64) :: i
        if (length <= 0 .or. cur == 0 .or. .not. allocated(overlays)) return
        do i = 1, size(overlays, kind=int64)
            if (.not. table%is_live(table%handle_of(i))) cycle
            if (.not. in_buffer(i, cur)) cycle
            associate (node => overlays(i))
                if (.not. before_markers .and. node%begin == pos .and. node%front_advance .and. &
                    (node%begin /= node%end .or. node%rear_advance)) then
                    ! Nodes with front_advance starting at POS move as a
                    ! whole (removed and reinserted upstream).  If we have
                    ! front_advance and not rear_advance and the overlay is
                    ! empty, we don't move begin past end by pretending
                    ! it's not front_advance.
                    node%begin = node%begin + length
                    node%end = node%end + length
                else
                    if (before_markers) then
                        if (node%begin >= pos) node%begin = node%begin + length
                    else if (node%begin > pos) then
                        node%begin = node%begin + length
                    end if
                    if (node%end > pos) then
                        node%end = node%end + length
                    else if (node%end == pos .and. (before_markers .or. node%rear_advance)) then
                        node%end = node%end + length
                    end if
                end if
            end associate
        end do
    end subroutine adjust_overlays_for_insert

    !> Delete a gap at POS of length LENGTH, contracting all intervals
    !> intersecting it, then delete the empty overlays at POS that have
    !> the `evaporate' property.  Port of
    !> buffer.c:adjust_overlays_for_delete (adjust_overlays_for_delete_in_buffer)
    !> with itree.c:itree_delete_gap (the current buffer).
    subroutine adjust_overlays_for_delete(pos, length)
        integer(int64), intent(in) :: pos, length
        type(handle_type), allocatable :: hit_list(:)
        integer(int64) :: i
        integer :: n, k
        if (length <= 0 .or. cur == 0 .or. .not. allocated(overlays)) return
        do i = 1, size(overlays, kind=int64)
            if (.not. table%is_live(table%handle_of(i))) cycle
            if (.not. in_buffer(i, cur)) cycle
            associate (node => overlays(i))
                if (pos < node%begin) node%begin = max(pos, node%begin - length)
                if (node%end > pos) node%end = max(pos, node%end - length)
            end associate
        end do
        ! Delete any zero-sized overlays at position POS, if the
        ! `evaporate' property is set.
        allocate (hit_list(size(overlays)))
        n = 0
        do i = 1, size(overlays, kind=int64)
            if (.not. table%is_live(table%handle_of(i))) cycle
            if (.not. in_buffer(i, cur)) cycle
            if (overlays(i)%begin /= pos .or. overlays(i)%end /= pos) cycle
            if (nilp(overlay_get(table%handle_of(i), "evaporate"))) cycle
            n = n + 1
            hit_list(n) = table%handle_of(i)
        end do
        do k = 1, n
            call delete_overlay(hit_list(k))
        end do
    end subroutine adjust_overlays_for_delete
end module itree_fm
