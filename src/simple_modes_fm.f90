! simple_modes_fm.f90 -- Fundamental and Special modes, a submodule of
! simple_fm.
! Ported from GNU Emacs 31.1 lisp/simple.el.
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

!> `fundamental_mode' and `special_mode': a submodule because they use
!> derived_fm.
submodule(simple_fm) simple_modes_fm
    use commands_fm, only: commands
    use data_fm, only: kill_all_local_variables
    use subr_fm, only: run_mode_hooks
    use keymap_fm, only: keymap_set, define_key
    use modes_fm, only: major_modes, minor_modes
    use buffer_fm, only: get_buffer, get_buffer_create, set_buffer, buffer_live_p, current_buffer
    use xdisp_fm, only: messages_buffer_mode_function, message_log_echo, MESSAGES_BUFFER_NAME
    use keyboard_fm, only: message_log_hook
    implicit none(type, external)

contains

    module procedure fundamental_mode
    call kill_all_local_variables()
    call run_mode_hooks(status)
    end procedure fundamental_mode

    module procedure special_mode
    logical :: saved
    call major_modes%special_mode%begin(saved, status)
    if (.not. status%ok()) return
    buffers(cur)%read_only = .true.
    call major_modes%special_mode%finish(saved, status)
    end procedure special_mode

    module procedure messages_buffer_mode
    logical :: saved
    call major_modes%messages_buffer_mode%begin(saved, status)
    if (.not. status%ok()) return
    call major_modes%messages_buffer_mode%finish(saved, status)
    end procedure messages_buffer_mode

    module procedure messages_buffer
    type(handle_type) :: old
    type(status_type) :: st
    buffer = get_buffer(MESSAGES_BUFFER_NAME)
    status = status_type()
    if (buffer_live_p(buffer)) return
    old = current_buffer()
    call get_buffer_create(MESSAGES_BUFFER_NAME, buffer, status)
    if (.not. status%ok()) return
    call set_buffer(buffer, status)
    if (.not. status%ok()) return
    call messages_buffer_mode(status)
    if (buffer_live_p(old)) call set_buffer(old, st)
    end procedure messages_buffer

    !> Toggle Transient Mark mode.
    !>
    !> Transient Mark mode is a global minor mode.  When enabled, the
    !> region is highlighted with the `region' face whenever the mark
    !> is active.  The mark is "deactivated" after certain non-motion
    !> commands, including those that change the text in the buffer, and
    !> during shift or mouse selection by any unshifted cursor motion
    !> command.
    !>
    !> Many commands change their behavior when Transient Mark mode is
    !> in effect and the mark is active, by acting on the region instead
    !> of their usual default part of the buffer's text.  Port of
    !> simple.el:transient-mark-mode.
    subroutine cmd_transient_mark_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%transient_mark_mode%command(status)
    end subroutine cmd_transient_mark_mode

    !> Toggle whether indentation can insert TAB characters.  Port of
    !> simple.el:indent-tabs-mode.
    subroutine cmd_indent_tabs_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%indent_tabs_mode%command(status)
    end subroutine cmd_indent_tabs_mode

    module procedure syms_of_simple_modes
    type(status_type) :: st
    integer :: d
    character(len=1) :: digit
    type(handle_type) :: map
    type(value_type) :: remap_key
    type(value_type), allocatable :: remap_events(:)
    commands%fundamental_mode%run => fundamental_mode
    commands%special_mode%run => special_mode
    ! Fundamental mode is not derived; it is defined so that other modes
    ! can name it as their parent.
    call major_modes%fundamental_mode%define("fundamental_mode", "Fundamental", fundamental_mode)
    call major_modes%special_mode%define("special_mode", "Special", special_mode, special_mode_hook)
    special_mode_map = major_modes%special_mode%map
    major_modes%special_mode%special = .true.
    call major_modes%messages_buffer_mode%define("messages_buffer_mode", "Messages", messages_buffer_mode, &
                                                 messages_buffer_mode_hook, parent=major_modes%special_mode)
    messages_buffer_mode_map = major_modes%messages_buffer_mode%map
    ! :parent special-mode-map "g" nil: nothing to revert.
    call keymap_set(messages_buffer_mode_map, "g", nil(), st)
    commands%messages_buffer_mode%run => messages_buffer_mode
    messages_buffer_mode_function => messages_buffer_mode
    message_log_hook => message_log_echo
    call minor_modes%transient_mark_mode%define("transient_mark_mode", global=.true., &
                                                variable="transient_mark_mode", hook=transient_mark_mode_hook)
    commands%transient_mark_mode%run => cmd_transient_mark_mode
    call minor_modes%indent_tabs_mode%define("indent_tabs_mode", variable="indent_tabs_mode", &
                                             hook=indent_tabs_mode_hook)
    commands%indent_tabs_mode%run => cmd_indent_tabs_mode
    ! special-mode-map: (suppress-keymap map) and the viewing keys.
    map = major_modes%special_mode%map
    ! subr.el:suppress-keymap: (define-key map [remap self-insert-command] #'undefined).
    remap_key = list(sym("remap"), sym("self_insert_command"))
    call list_elements(remap_key, remap_events)
    call define_key(map, remap_events, sym("undefined"), st)
    call keymap_set(map, "q", "quit_window", st)
    call keymap_set(map, "SPC", "scroll_up_command", st)
    call keymap_set(map, "S-SPC", "scroll_down_command", st)
    call keymap_set(map, "DEL", "scroll_down_command", st)
    call keymap_set(map, "<", "beginning_of_buffer", st)
    call keymap_set(map, ">", "end_of_buffer", st)
    call keymap_set(map, "-", "negative_argument", st)
    do d = 0, 9
        write (digit, "(i1)") d
        call keymap_set(map, digit, "digit_argument", st)
    end do
    end procedure syms_of_simple_modes
end submodule simple_modes_fm
