! cmds_fm.f90 -- simple built-in editing commands.
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

!> Motion, deletion and insertion commands on the current buffer, and
!> their registration as commands (`syms_of_cmds').
!>
!> Deferred: overwrite mode and abbrevs in `self_insert_command',
!> `translation_table_for_input', and intangible/invisible text.
module cmds_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, &
                        FM_BEGINNING_OF_BUFFER, &
                        FM_END_OF_BUFFER
    use buffer_fm, only: buffers, cur, BUF_BYTES_MAX, buf_fetch_byte, &
                         temp_set_point_both, auto_fill_function
    use search_fm, only: scan_newline_from_point
    use insdel_fm, only: del_range
    use editfns_fm, only: line_beginning_position, line_end_position, goto_char, &
                          insert_char_n
    use lisp_fm, only: value_type, fixnump, xfixnum, nilp
    use character_fm, only: MAX_CHAR
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: last_command_event, this_command, last_command
    use lisp_fm, only: make_fixnum
    use eval_fm, only: hook_type
    use data_fm, only: value_eq
    use undo_fm, only: undo_auto_amalgamate, this_command_amalgamating
    use dispnew_fm, only: ding
    implicit none(type, external)
    private

    public :: forward_char, backward_char, forward_line, beginning_of_line, &
              end_of_line, delete_char, self_insert_command, syms_of_cmds

    !> A range of characters FROM..TO, both included.
    type, public :: char_range_type
        integer :: from = 0
        integer :: to = -1
    end type char_range_type

    !> The characters which invoke auto-filling, as ranges of characters.
    !> Port of character.c
    !> auto-fill-chars: here the ranges whose value is t (unallocated is
    !> not a char-table: space and newline).  `syms_of_cmds' gives it
    !> space and newline (character.c) and characters.el's CJK ranges.
    type(char_range_type), allocatable, public :: auto_fill_chars(:)

    abstract interface
        !> simple.el:internal-auto-fill: `filled' is its value (true
        !> when the auto-fill function did any work).
        subroutine internal_auto_fill_proc(status, filled)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(out) :: filled
        end subroutine internal_auto_fill_proc

        !> simple.el:kill-forward-chars: kill `n' characters after point
        !> (before, if negative).
        subroutine kill_chars_proc(n, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: n
            type(status_type), intent(out) :: status
        end subroutine kill_chars_proc
    end interface

    !> simple.el:internal-auto-fill, which `self_insert_command' calls to
    !> perform auto-filling; set by simple_fm (null: no auto-filling).
    procedure(internal_auto_fill_proc), pointer, public :: internal_auto_fill_hook => null()

    !> simple.el:kill-forward-chars, which `delete_char' calls with
    !> KILLFLAG; set by simple_fm (null: KILLFLAG deletes).
    procedure(kill_chars_proc), pointer, public :: kill_forward_chars_hook => null()

    !> Hook run at the end of `self_insert_command'.
    !> This is run after inserting a character.
    !> The hook can access the inserted character via `last_command_event'.
    !> Port of cmds.c post-self-insert-hook.
    type(hook_type), target, public :: post_self_insert_hook = hook_type(name="post_self_insert_hook")

contains

    !> Move point `n' characters (backward if not `forward'), stopping at
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

    !> Move to the start of the line `n' lines away (default 1) and return
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

    !> Move to the start of the line (after moving `n' - 1 lines).
    !> Port of cmds.c:Fbeginning_of_line (fields pending).
    subroutine beginning_of_line(n)
        integer(int64), intent(in), optional :: n
        call goto_char(line_beginning_position(n))
    end subroutine beginning_of_line

    !> Move to the end of the line (after moving `n' - 1 lines).
    !> Port of cmds.c:Fend_of_line; the intangible-text retry loop is moot
    !> until text properties exist.
    subroutine end_of_line(n)
        integer(int64), intent(in), optional :: n
        call goto_char(line_end_position(n))
    end subroutine end_of_line

    !> Delete `n' characters after point (before, if negative); signals
    !> beginning-/end-of-buffer instead of deleting fewer.  Optional
    !> KILLFLAG true means kill instead (save in kill ring).
    !> Port of cmds.c:Fdelete_char.
    subroutine delete_char(n, status, killflag)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: killflag
        integer(int64) :: pos
        logical :: kill
        if (abs(n) < 2) call undo_auto_amalgamate(value_eq(this_command, last_command))
        pos = buffers(cur)%pt + n
        kill = .false.
        if (present(killflag)) kill = killflag
        if (kill .and. associated(kill_forward_chars_hook)) then
            call kill_forward_chars_hook(n, status)
        else if (n < 0) then
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

    !> Insert character `c' `n' times, then run `post_self_insert_hook';
    !> `c' becomes `last_command_event'.  Port of
    !> cmds.c:Fself_insert_command with internal_self_insert reduced to
    !> the insertion, auto-fill and the hook (no overwrite or abbrevs).  A
    !> non-character `c' (-1: `last_command_event' is not a character)
    !> rings the bell.
    recursive subroutine self_insert_command(n, c, status)
        integer(int64), intent(in) :: n, c
        type(status_type), intent(out) :: status
        character(len=24) :: buf
        integer :: hairy
        status = status_type()
        ! Backward compatibility.
        if (c >= 0) last_command_event = make_fixnum(c)
        if (n < 0) then
            write (buf, "(i0)") n
            call set_error(status, FM_GENERIC_ERROR, &
                           "Negative repetition argument "//trim(buf))
            return
        end if
        if (n < 2) call undo_auto_amalgamate(value_eq(this_command, last_command))
        ! Barf if the key that invoked this was not a character.
        if (c < 0 .or. c > MAX_CHAR) then
            call ding()
            return
        end if
        call internal_self_insert(int(c, int32), n, hairy, status)
        if (hairy == 2) this_command_amalgamating = .false.
    end subroutine self_insert_command

    !> Insert character `c' `n' times, auto-fill and run
    !> `post_self_insert_hook'; `hairy' is 2 when auto-filling did
    !> something (the undo amalgamation is then reset).  Port of
    !> cmds.c:internal_self_insert (no overwrite mode, abbrevs or change
    !> functions).
    recursive subroutine internal_self_insert(c, n, hairy, status)
        integer(int32), intent(in) :: c
        integer(int64), intent(in) :: n
        integer, intent(out) :: hairy
        type(status_type), intent(out) :: status
        logical :: filled
        hairy = 0
        call insert_char_n(c, n, status)
        if (.not. status%ok()) return
        if (auto_fill_char_p(c) .and. associated(auto_fill_function%value) .and. associated(internal_auto_fill_hook)) then
            ! After inserting a newline, move to previous line and fill
            ! that.  Must have the newline in place already so filling and
            ! justification, if any, know where the end is going to be.
            if (c == 10) call goto_char(buffers(cur)%pt - 1)
            call internal_auto_fill_hook(status, filled)
            ! Test PT < ZV in case the auto-fill-function is strange.
            if (c == 10 .and. buffers(cur)%pt < buffers(cur)%zv) call goto_char(buffers(cur)%pt + 1)
            if (filled) hairy = 2
        end if
        ! Run hooks for electric keys.
        if (.not. status%ok()) return
        call post_self_insert_hook%run(status)
    end subroutine internal_self_insert

    !> Whether `auto_fill_chars' contains `c' (space and
    !> newline when it is not a char-table).
    logical function auto_fill_char_p(c)
        integer(int32), intent(in) :: c
        integer :: k
        if (.not. allocated(auto_fill_chars)) then
            auto_fill_char_p = c == 32 .or. c == 10
            return
        end if
        auto_fill_char_p = .false.
        do k = 1, size(auto_fill_chars)
            if (c >= auto_fill_chars(k)%from .and. c <= auto_fill_chars(k)%to) then
                auto_fill_char_p = .true.
                return
            end if
        end do
    end function auto_fill_char_p

    ! ------------------------------------------------------------------
    ! Commands (the interactive side of the DEFUNs)
    ! ------------------------------------------------------------------

    !> Interactive "p": the numeric prefix argument.
    integer(int64) function arg_p()
        arg_p = prefix_numeric_value(current_prefix_arg)
    end function arg_p

    !> Move point N characters forward (backward if N is negative).
    !> On reaching end or beginning of buffer, stop and signal error.
    !> Interactively, N is the numeric prefix argument.
    !> If N is omitted, move point 1 character forward.
    !>
    !> Depending on the bidirectional context, the movement may be to the
    !> right or to the left on the screen.  This is in contrast with
    !> \[right_char], which see.  Port of cmds.c:Fforward_char (the command).
    subroutine cmd_forward_char(status)
        type(status_type), intent(out) :: status
        call forward_char(status, arg_p())
    end subroutine cmd_forward_char

    !> Move point N characters backward (forward if N is negative).
    !> On attempt to pass beginning or end of buffer, stop and signal error.
    !> Interactively, N is the numeric prefix argument.
    !> If N is omitted, move point 1 character backward.
    !>
    !> Depending on the bidirectional context, the movement may be to the
    !> right or to the left on the screen.  This is in contrast with
    !> \[left_char], which see.  Port of cmds.c:Fbackward_char (the command).
    subroutine cmd_backward_char(status)
        type(status_type), intent(out) :: status
        call backward_char(status, arg_p())
    end subroutine cmd_backward_char

    !> Move N lines forward (backward if N is negative).
    !> Precisely, if point is on line I, move to the start of line I + N
    !> ("start of line" in the logical order).
    !> If there isn't room, go as far as possible (no error).
    !> Interactively, N is the numeric prefix argument and defaults to 1.
    !>
    !> Returns the count of lines left to move.  If moving forward,
    !> that is N minus number of lines moved; if backward, N plus number
    !> moved.
    !>
    !> Exception: With positive N, a non-empty line at the end counts as one
    !> line successfully moved (for the return value).  This means in that case
    !> it returns one less than the number of lines left to move.
    !>
    !> The command C-n (\[next_line]) can also be used to move to the next
    !> line.  Port of cmds.c:Fforward_line (the command).
    subroutine cmd_forward_line(status)
        type(status_type), intent(out) :: status
        integer(int64) :: shortage
        status = status_type()
        shortage = forward_line(arg_p())
    end subroutine cmd_forward_line

    !> Move point to beginning of current line (in the logical order).
    !> With argument N not 1, move forward N - 1 lines first.
    !> If point reaches the beginning or end of buffer, it stops there.
    !>
    !> This function constrains point to the current field unless this moves
    !> point to a different line from the original, unconstrained result.
    !> If N is 1, and a front-sticky field starts at point, the point
    !> does not move.  To ignore field boundaries set
    !> `inhibit_field_text_motion' to true, or use the `forward_line' function
    !> instead.  Port of cmds.c:Fbeginning_of_line (the command).
    subroutine cmd_beginning_of_line(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call beginning_of_line(arg_p())
    end subroutine cmd_beginning_of_line

    !> Move point to end of current line (in the logical order).
    !> With argument N not 1, move forward N - 1 lines first.
    !> If point reaches the beginning or end of buffer, it stops there.
    !>
    !> This function constrains point to the current field unless this moves
    !> point to a different line from the original, unconstrained result.  If
    !> N is 1, and a rear-sticky field ends at point, the point does
    !> not move.  To ignore field boundaries set `inhibit_field_text_motion'
    !> to true.  Port of cmds.c:Fend_of_line (the command).
    subroutine cmd_end_of_line(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call end_of_line(arg_p())
    end subroutine cmd_end_of_line

    !> Delete the following N characters (previous if N is negative).
    !> Interactively, N is the prefix arg.
    !>
    !> Optional second arg KILLFLAG true means kill instead (save in kill ring).
    !> Interactively, N is the prefix arg, and KILLFLAG is set if
    !> N was explicitly specified.
    !>
    !> The command `delete_forward_char' is preferable for interactive use, e.g.
    !> because it respects values of `delete_active_region' and `overwrite_mode'.
    !> Port of cmds.c:Fdelete_char (the command).
    subroutine cmd_delete_char(status)
        type(status_type), intent(out) :: status
        call delete_char(arg_p(), status,.not. nilp(current_prefix_arg))
    end subroutine cmd_delete_char

    !> Insert the character you type.
    !> Whichever character C you type to run this command is inserted.
    !> The numeric prefix argument N says how many times to repeat the insertion.
    !> After insertion, `internal_auto_fill' is called if
    !> `auto_fill_function' is true and if the `auto_fill_chars' table has
    !> a true value for the inserted character.  At the end, it runs
    !> `post_self_insert_hook'.  Port of cmds.c:Fself_insert_command (the
    !> command; abbrevs are not ported).
    recursive subroutine cmd_self_insert_command(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (fixnump(last_command_event)) then
            call self_insert_command(arg_p(), xfixnum(last_command_event), status)
        else
            ! Not a character: self_insert_command barfs.
            call self_insert_command(arg_p(), -1_int64, status)
        end if
    end subroutine cmd_self_insert_command

    !> Register this module's commands.  Port of the defsubr part of
    !> cmds.c:syms_of_cmds.
    subroutine syms_of_cmds()
        commands%forward_char%run => cmd_forward_char
        commands%backward_char%run => cmd_backward_char
        commands%forward_line%run => cmd_forward_line
        commands%beginning_of_line%run => cmd_beginning_of_line
        commands%end_of_line%run => cmd_end_of_line
        commands%delete_char%run => cmd_delete_char
        commands%self_insert_command%run => cmd_self_insert_command
        ! character.c: space and newline; characters.el: the charsets that
        ! should invoke auto-filling.
        auto_fill_chars = [char_range_type(32, 32), char_range_type(10, 10), &
                           char_range_type(int(z'3041'), int(z'30FF')), char_range_type(int(z'3400'), int(z'4DB5')), &
                           char_range_type(int(z'4E00'), int(z'9FBB')), char_range_type(int(z'F900'), int(z'FAFF')), &
                           char_range_type(int(z'FF00'), int(z'FF9F')), char_range_type(int(z'20000'), int(z'2FFFF'))]
    end subroutine syms_of_cmds
end module cmds_fm
