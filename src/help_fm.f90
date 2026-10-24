! help_fm.f90 -- help commands: the help map, C-h k, C-h c, C-h w, C-h b, C-h l.
! Ported from GNU Emacs 31.1 lisp/help.el, with lisp/help-macro.el
! (make-help-screen) and src/keymap.c (describe-buffer-bindings).
!
! Copyright (C) 1985-1986, 1993-1994, 1998-2026 Free Software Foundation, Inc.
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

!> The help commands of help.el: the help map on C-h, <help> and <f1>,
!> `describe_key' (C-h k), `describe_key_briefly' (C-h c), `where_is'
!> (C-h w), `describe_bindings' (C-h b, with keymap.c's
!> describe-buffer-bindings and help.el's help--describe-map-tree and
!> describe-map), `view_lossage' (C-h l), `view_echo_area_messages'
!> (C-h e), `help_for_help' (C-h C-h, C-h ?, help-macro.el's help screen)
!> and `help_quit'; `substitute_command_keys', which turns a docstring's
!> \[COMMAND], \<MAP>, \{MAP}, \`KEY', \= and quotes into what the reader
!> sees; and `with_help_window', which shows text in *Help*.
!>
!> `describe_function', `describe_variable', `describe_symbol',
!> `describe_command' and `describe_mode' are in help_fns_fm, which
!> installs the text `describe_key' shows through
!> `describe_function_1_hook' (help-fns.el is loaded after help.el); the
!> apropos commands are in apropos_fm.
!>
!> Deferred: the help-map commands that are not ported (Info and the
!> manuals, the NEWS, FAQ, tutorial and other files, describe-syntax,
!> describe-coding-system and the input methods, display-local-help,
!> help-quick, finder, describe-package, help-find-source), whose
!> keys in the help map stay unbound (the help-for-help screen lists
!> them as upstream does); `three_step_help'; the
!> `help_key_binding' and `separator_line' faces; Outline minor mode in
!> *Help* (`describe_bindings_outline'); `describe_bindings''s PREFIX and
!> BUFFER, keyboard-translate-table, text property keymaps, menus and
!> the character names of translations; `view_lossage''s AUTO-REFRESH and
!> `lossage_size' changes; key translations in `describe_key''s report,
!> mouse and menu events, the help-xref stack and buttons,
!> `function_called_at_point' defaults, and describing a key bound to a
!> keyboard macro (upstream names the macro by an address hash,
!> help-fns-function-name; here the key "runs a keyboard macro").
module help_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, symbolp, fixnump, keymap_ref_p, xhandle, list_elements, make_keymap_ref
    use lread_fm, only: sym
    use data_fm, only: value_eq, symbol_name
    use keymap_fm, only: make_sparse_keymap, keymap_live_p, free_keymap, keymap_set, define_key, key_parse, &
                         key_valid_p, key_description, where_is_internal, lookup_key, current_active_maps, &
                         overriding_terminal_local_map, active_minor_mode_of_map, current_global_map
    use derived_fm, only: major_mode_type, major_mode_named
    use keyboard_fm, only: read_key_sequence
    use callint_fm, only: commandp
    use buffer_fm, only: current_buffer, get_buffer_create, set_buffer, set_buffer_read_only, erase_buffer, &
                         restore_buffer_modified_p, buffers, cur, buffer_live_p
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, widen
    use marker_fm, only: set_marker
    use xdisp_fm, only: message
    use minibuf_fm, only: completion_obarray_table, minibuffer_local_map
    use minibuffer_fm, only: completing_read, REQUIRE_MATCH_T
    use help_mode_fm, only: help_mode, help_mode_map, init_help_mode
    use frame_fm, only: selected_frame, selected_window
    use window_fm, only: windows, window_live_p, same_window, window_leaves, temp_buffer_window_show, &
                         pos_visible_in_window_p, QR_WINDOW, QR_FRAME, QR_OTHER, display_buffer, &
                         minibuffer_scroll_window
    use derived_fm, only: derived_mode_p
    use lisp_fm, only: t_value, make_fixnum, make_list, stringp, xstring, vtag, V_LIST, list_elt, xfixnum
    use keymap_fm, only: accessible_keymaps, map_keymap_items, current_minor_maps, key_list_type, current_local_map, &
                         set_keymap_parent, single_key_description
    use keyboard_fm, only: recent_keys, lossage_size, bitch_at_user, key_translation_map, local_function_key_map, &
                           input_decode_map
    use xdisp_fm, only: message_nolog, glyph_columns, cursor_in_echo_area
    use simple_fm, only: messages_buffer
    use window_fm, only: set_window_point, pop_to_buffer, scroll_up, scroll_down, window_configuration_type, &
                         current_window_configuration, set_window_configuration, free_window_configuration, &
                         window_text_cols
    use callint_fm, only: command_defined_p, call_interactively
    use data_fm, only: default_value
    use fns_fm, only: value_equal
    use print_fm, only: fortran_repr
    use error_fm, only: FM_QUIT
    use faces_fm, only: display_supports_face_attributes_p, face_attrs
    implicit none(type, external)
    private

    public :: substitute_command_keys, register_keymap_name, keymap_name, with_help_window, with_output_to_temp_buffer, &
              help_buffer_name, describe_key, describe_key_briefly, where_is, help_quit, syms_of_help, &
              view_echo_area_messages, view_lossage, help_for_help, describe_bindings, describe_buffer_bindings, &
              describe_map_tree, lossage_comment_indent, make_separator_line

    !> Keymap for characters following the Help key.  Port of help.el
    !> help-map (f, v, k, c, w and q so far).
    type(handle_type), public :: help_map

    abstract interface
        !> The text describing function `name', after `lead'.
        subroutine describe_function_1_proc(name, text, lead)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            character(len=:), allocatable, intent(out) :: text
            character(len=*), intent(in), optional :: lead
        end subroutine describe_function_1_proc

        !> The raw docstring of function `name' (`found' false if none).
        subroutine documentation_proc(name, doc, found)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            character(len=:), allocatable, intent(out) :: doc
            logical, intent(out) :: found
        end subroutine documentation_proc
    end interface

    !> help-fns.el:describe-function-1, set by help_fns_fm.
    procedure(describe_function_1_proc), pointer, public :: describe_function_1_hook => null()

    !> doc.c:Fdocumentation (RAW), set by help_fns_fm, which holds the
    !> compiled-in docstrings.
    procedure(documentation_proc), pointer, public :: documentation_hook => null()

    !> Name of the help buffer.  help.el (help-buffer).
    character(len=*), parameter :: help_buffer_name = "*Help*"

    !> A keymap that \<NAME> can name in a docstring (a Lisp variable
    !> holding a keymap upstream).
    type :: named_keymap_type
        character(len=:), allocatable :: name
        type(handle_type) :: map
    end type named_keymap_type

    type(named_keymap_type), allocatable :: named_keymaps(:)

    !> One entry of a describe-map section: the key text, the
    !> definition, and what follows the definition on its line and on
    !> lines of its own.
    type :: entry_line_type
        character(len=:), allocatable :: key, def, extra
    end type entry_line_type

    character(len=*), parameter :: LF = achar(10), TAB = achar(9), FF = achar(12)

    !> newcomment.el comment-inline-offset, and the `comment_column'
    !> view-lossage sets.
    integer, parameter :: COMMENT_INLINE_OFFSET = 1, COMMENT_COLUMN = 24

    !> M-v and S-SPC.
    integer(int64), parameter :: M_V = 134217728_int64 + 118_int64, S_SPC = 33554432_int64 + 32_int64

    !> Left and right single quotes (text-quoting-style `curve').
    character(len=*), parameter :: LQUOTE = char(226)//char(128)//char(152), &
                                   RQUOTE = char(226)//char(128)//char(153)

contains

    !> Let \<`name'> and "found in `name'" refer to keymap `map'.
    subroutine register_keymap_name(name, map)
        character(len=*), intent(in) :: name
        type(handle_type), intent(in) :: map
        type(named_keymap_type), allocatable :: grown(:)
        integer :: k
        if (.not. allocated(named_keymaps)) allocate (named_keymaps(0))
        do k = 1, size(named_keymaps)
            if (named_keymaps(k)%name == name) then
                named_keymaps(k)%map = map
                return
            end if
        end do
        allocate (grown(size(named_keymaps) + 1))
        grown(1:size(named_keymaps)) = named_keymaps
        grown(size(grown))%name = name
        grown(size(grown))%map = map
        call move_alloc(grown, named_keymaps)
    end subroutine register_keymap_name

    !> The registered name of keymap `map', or "".
    function keymap_name(map) result(name)
        type(handle_type), intent(in) :: map
        character(len=:), allocatable :: name
        integer :: k
        name = ""
        if (.not. allocated(named_keymaps)) return
        do k = 1, size(named_keymaps)
            if (named_keymaps(k)%map%index == map%index .and. &
                named_keymaps(k)%map%generation == map%generation) then
                name = named_keymaps(k)%name
                return
            end if
        end do
    end function keymap_name

    !> The keymap registered as `name'; `found' false if there is none.
    subroutine named_keymap(name, map, found)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: map
        logical, intent(out) :: found
        integer :: k
        found = .false.
        if (.not. allocated(named_keymaps)) return
        do k = 1, size(named_keymaps)
            if (named_keymaps(k)%name == name) then
                map = named_keymaps(k)%map
                found = keymap_live_p(map)
                return
            end if
        end do
    end subroutine named_keymap

    !> The keys that run command `name', as "C-x C-f", searching `map' and
    !> the global map if `have_map', else the active maps; "M-x NAME" if
    !> none.
    function command_keys(name, map, have_map) result(s)
        character(len=*), intent(in) :: name
        type(handle_type), intent(in) :: map
        logical, intent(in) :: have_map
        character(len=:), allocatable :: s
        type(value_type) :: keys
        type(value_type), allocatable :: events(:)
        if (have_map) then
            call where_is_internal(sym(name), keys, firstonly=.true., keymap=map)
        else
            call where_is_internal(sym(name), keys, firstonly=.true.)
        end if
        if (nilp(keys)) then
            s = "M-x "//name
        else
            call list_elements(keys, events)
            s = key_description(events)
        end if
    end function command_keys

    !> Substitute key descriptions for command names in STRING.
    !> Each substring of the form \[COMMAND] is replaced by either a
    !> keystroke sequence that invokes COMMAND, or "M-x COMMAND" if COMMAND
    !> is not on any keys.
    !>
    !> Each substring of the form \`KEYBINDING' will be replaced by
    !> KEYBINDING.
    !>
    !> Each substring of the form \<MAPVAR> specifies the use of MAPVAR
    !> as the keymap for future \[COMMAND] substrings.
    !>
    !> Each grave accent ` is replaced by left quote, and each apostrophe '
    !> is replaced by right quote.  Left and right quote characters are
    !> specified by `text_quoting_style'.
    !>
    !> Each substring of the form \{MAPVAR} is replaced by a summary of
    !> the value of MAPVAR as a keymap.  This summary is similar to the one
    !> produced by `describe_bindings'.  The summary ends in two newlines
    !> (used by the helper function `help_make_xrefs' to find the end of the
    !> summary).
    !>
    !> \= quotes the following character and is discarded; thus, \=\= puts \=
    !> into the output, \=\[ puts \[ into the output, and \=` puts ' into the
    !> output.  Port of help.el:substitute-command-keys (no faces or menus;
    !> text-quoting-style is `curve').
    function substitute_command_keys(string) result(out)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: out
        type(handle_type) :: map
        integer :: i, j, n, clen
        logical :: have_map, found
        character :: c, nx
        out = ""
        have_map = .false.
        n = len(string)
        i = 1
        do while (i <= n)
            c = string(i:i)
            if (c == "\" .and. i < n) then
                nx = string(i + 1:i + 1)
                select case (nx)
                case ("=")
                    if (i + 2 <= n) then
                        clen = utf8_length(string(i + 2:i + 2))
                        out = out//string(i + 2:min(n, i + 1 + clen))
                        i = i + 2 + clen
                    else
                        ! \= at the end of the string stays.
                        out = out//"\="
                        i = i + 2
                    end if
                    cycle
                case ("[")
                    j = index(string(i + 2:), "]")
                    if (j > 0) then
                        out = out//command_keys(string(i + 2:i + j), map, have_map)
                        i = i + 2 + j
                        cycle
                    end if
                case ("<")
                    j = index(string(i + 2:), ">")
                    if (j > 0) then
                        call named_keymap(string(i + 2:i + j), map, found)
                        have_map = found
                        i = i + 2 + j
                        cycle
                    end if
                case ("{")
                    j = index(string(i + 2:), "}")
                    if (j > 0) then
                        out = out//keymap_summary(string(i + 2:i + j))
                        i = i + 2 + j
                        cycle
                    end if
                case ("`")
                    j = index(string(i + 2:), "'")
                    if (j > 0) then
                        if (literal_key_p(string(i + 2:i + j))) then
                            out = out//string(i + 2:i + j)
                            i = i + 2 + j
                            cycle
                        end if
                    end if
                case default
                end select
                out = out//c
                i = i + 1
            else if (c == "`") then
                out = out//LQUOTE
                i = i + 1
            else if (c == "'") then
                out = out//RQUOTE
                i = i + 1
            else
                out = out//c
                i = i + 1
            end if
        end do
    end function substitute_command_keys

    !> What \{NAME} in a docstring becomes: the summary of the keymap
    !> registered as NAME, shadowed by the active maps that take precedence
    !> over it, or a line saying it is not defined.  The \{MAPVAR} case of
    !> help.el:substitute-command-keys.
    function keymap_summary(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        type(handle_type) :: map
        type(value_type) :: active
        type(value_type), allocatable :: maps(:), earlier(:)
        logical :: found
        integer :: k, n
        call named_keymap(name, map, found)
        if (.not. found) then
            s = LF//"Uses keymap "//LQUOTE//name//RQUOTE//", which is not currently defined."//LF
            return
        end if
        ! The active keymaps that precede this one; none if it's not
        ! active.
        active = current_active_maps()
        if (keymap_ref_p(active)) then
            allocate (maps(1))
            maps(1) = active
        else
            call list_elements(active, maps)
        end if
        n = 0
        do k = 1, size(maps)
            if (.not. keymap_ref_p(maps(k))) cycle
            if (same_map(xhandle(maps(k)), map)) then
                n = k - 1
                exit
            end if
        end do
        allocate (earlier(n))
        if (n > 0) earlier = maps(1:n)
        s = ""
        call describe_map_tree(map, .true., earlier, "", .false., s, mention_shadow=.true.)
    end function keymap_summary

    !> Whether \`KEY' in a docstring shows KEY: a valid key sequence, a
    !> mouse button or "M-x COMMAND".
    logical function literal_key_p(key)
        character(len=*), intent(in) :: key
        literal_key_p = .false.
        if (len(key) == 0) return
        literal_key_p = key_valid_p(key)
        if (len(key) >= 7) literal_key_p = literal_key_p .or. (key(1:6) == "mouse-" .and. verify(key(7:7), "123456789") == 0)
        if (len(key) > 4) literal_key_p = literal_key_p .or. key(1:4) == "M-x "
    end function literal_key_p

    !> Length of the UTF-8 sequence that starts with byte `b'.
    integer function utf8_length(b)
        character, intent(in) :: b
        integer :: code
        code = iachar(b)
        if (code >= 240) then
            utf8_length = 4
        else if (code >= 224) then
            utf8_length = 3
        else if (code >= 192) then
            utf8_length = 2
        else
            utf8_length = 1
        end if
    end function utf8_length

    !> Show `text' in the help buffer, in Help mode, in a window chosen by
    !> `temp_buffer_window_show', and say how to quit it; the text ends
    !> with one newline (help-mode.el:help-make-xrefs).  Port of
    !> help.el:with-help-window and help--window-setup (no xrefs;
    !> `help_window_point_marker' is deferred).
    subroutine with_help_window(text, status)
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, previous, window
        type(status_type) :: st
        integer :: n
        previous = current_buffer()
        call get_buffer_create(help_buffer_name, buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        if (.not. derived_mode_p(["help_mode"])) then
            call help_mode(status)
            if (.not. status%ok()) return
        end if
        buffers(cur)%undo_enabled = .false.
        call set_buffer_read_only(buf, .false., status)
        call erase_buffer(status)
        if (.not. status%ok()) return
        ! help-make-xrefs: delete extraneous newlines at the end of the
        ! text, then end it with one.
        n = len(text)
        do while (n > 0)
            if (text(n:n) /= LF) exit
            n = n - 1
        end do
        call insert(text(1:n)//LF, status)
        if (.not. status%ok()) return
        call set_buffer_read_only(buf, .true., status)
        call temp_buffer_window_show(buf, window, status)
        if (.not. status%ok()) return
        call help_window_setup(window)
        if (buffer_live_p(previous)) call set_buffer(previous, st)
    end subroutine with_help_window

    !> Show TEXT in buffer BUFNAME: the buffer is erased and put in Help
    !> mode, TEXT is inserted (it is what the body prints to
    !> `standard_output'), and the buffer is displayed with
    !> `display_buffer' at its beginning, unmodified and read-only.  Port
    !> of subr.el:with-output-to-temp-buffer with
    !> window.c:temp_output_buffer_show and help.el's functions on
    !> `temp_buffer_setup_hook' and `temp_buffer_show_hook'
    !> (help-mode-setup, help-mode-finish; no xrefs, no
    !> `temp_buffer_show_function').
    subroutine with_output_to_temp_buffer(bufname, text, status)
        character(len=*), intent(in) :: bufname, text
        type(status_type), intent(out) :: status
        type(handle_type) :: previous, buf, window
        type(status_type) :: st
        character(len=:), allocatable :: old_dir
        previous = current_buffer()
        if (allocated(buffers(cur)%directory)) old_dir = buffers(cur)%directory
        call get_buffer_create(bufname, buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        ! (kill-all-local-variables) and the variables the macro sets; Help
        ! mode (help-mode-setup) kills the local variables again.
        if (allocated(old_dir)) buffers(cur)%directory = old_dir
        call set_buffer_read_only(buf, .false., status)
        if (allocated(buffers(cur)%filename)) deallocate (buffers(cur)%filename)
        buffers(cur)%undo_enabled = .false.
        call erase_buffer(status)
        if (.not. status%ok()) return
        call help_mode(status)
        if (.not. status%ok()) return
        call set_buffer_read_only(buf, .false., status)
        call insert(text, status)
        if (.not. status%ok()) return
        ! temp_output_buffer_show
        if (allocated(old_dir)) buffers(cur)%directory = old_dir
        call restore_buffer_modified_p(.false.)
        call widen()
        call goto_char(buffers(cur)%begv)
        if (buffer_live_p(previous)) call set_buffer(previous, st)
        call display_buffer(buf, window, status)
        if (.not. status%ok()) return
        if (window_live_p(window)) then
            minibuffer_scroll_window = window
            windows(window%index)%hscroll = 0
            if (windows(window%index)%start%index /= 0) call set_marker(windows(window%index)%start, 1_int64, st, buf)
            if (windows(window%index)%pointm%index /= 0) call set_marker(windows(window%index)%pointm, 1_int64, st, buf)
        end if
        ! help-mode-finish
        call set_buffer_read_only(buf, .true., st)
    end subroutine with_output_to_temp_buffer

    !> Display message telling how to quit and scroll help window.
    !> QUIT_PART is a string telling how to quit the help window WINDOW.
    !> SCROLL is 0 (don't tell how to scroll), 1 (tell how to scroll
    !> WINDOW) or 2 (tell how to scroll the "other" window).  Port of
    !> help.el:help-window-display-message.
    subroutine help_window_display_message(quit_part, window, scroll)
        character(len=*), intent(in) :: quit_part
        type(handle_type), intent(in) :: window
        integer, intent(in) :: scroll
        character(len=:), allocatable :: scroll_part
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: zv
        ! If we don't have QUIT_PART we probably reuse a window showing the
        ! same buffer so we don't show any message.
        if (len(quit_part) == 0) then
            call message("")
            return
        end if
        old = current_buffer()
        call set_buffer(windows(window%index)%buffer, st)
        zv = buffers(cur)%zv
        if (buffer_live_p(old)) call set_buffer(old, st)
        if (pos_visible_in_window_p(zv, window)) then
            ! Buffer end is at least partially visible, no need to talk
            ! about scrolling.
            scroll_part = "."
        else if (scroll == 2) then
            scroll_part = ", \[scroll_other_window] to scroll help."
        else if (scroll == 1) then
            scroll_part = ", \[scroll_up_command] to scroll help."
        else
            scroll_part = ""
        end if
        call message(substitute_command_keys(quit_part//scroll_part))
    end subroutine help_window_display_message

    !> Set up help window WINDOW for `with_help_window'.  WINDOW is the
    !> window used for displaying the help buffer.  Port of
    !> help.el:help-window-setup (`help_window_select' nil, one frame).
    subroutine help_window_setup(window)
        type(handle_type), intent(in) :: window
        integer :: help_setup, nwindows
        type(handle_type), allocatable :: leaves(:)
        if (.not. window_live_p(window)) return
        help_setup = windows(window%index)%quit_restore%kind
        call window_leaves(selected_frame%root_window, leaves)
        nwindows = size(leaves)
        if (same_window(window, selected_window())) then
            ! The help window is or gets selected ...
            select case (help_setup)
            case (QR_WINDOW)
                ! ... and is new, ...
                call help_window_display_message("Type \<help_map>\[help_quit] to delete help window", window, 1)
            case (QR_FRAME)
                ! ... on a new frame, ...
                call help_window_display_message("Type \<help_map>\[help_quit] to quit the help frame", window, 1)
            case (QR_OTHER)
                ! ... or displayed some other buffer before.
                call help_window_display_message("Type \<help_map>\[help_quit] to restore previous buffer", &
                                                 window, 1)
            case default
                call help_window_display_message("", window, 1)
            end select
        else if (nwindows == 2) then
            ! There are two windows on the help window's frame and the
            ! other one is the selected one.
            select case (help_setup)
            case (QR_WINDOW)
                call help_window_display_message("Type \[delete_other_windows] to delete the help window", &
                                                 window, 2)
            case (QR_OTHER)
                call help_window_display_message( &
                    "Type \<help_map>\[help_quit] in help window to restore its previous buffer", window, 2)
            case default
                call help_window_display_message("", window, 2)
            end select
        else
            ! The help window is not selected ...
            select case (help_setup)
            case (QR_WINDOW)
                ! ... and is new, ...
                call help_window_display_message("Type \<help_map>\[help_quit] in help window to delete it", &
                                                 window, 0)
            case (QR_OTHER)
                ! ... or displayed some other buffer before.
                call help_window_display_message( &
                    "Type \<help_map>\[help_quit] in help window to restore previous buffer", window, 0)
            case default
                call help_window_display_message("", window, 0)
            end select
        end if
    end subroutine help_window_setup

    !> " (found in MAP)" for the keymap variable holding the active map
    !> in which `keys' is bound; "" if none holds it.  The locus of
    !> help.el:describe-key.
    function found_in(keys) result(s)
        type(value_type), intent(in) :: keys(:)
        character(len=:), allocatable :: s
        s = binding_locus(keys)
        if (len(s) > 0) s = " (found in "//s//")"
    end function found_in

    !> Describe in which keymap KEY is defined.
    !> Return the name of a variable pointing to that keymap if one exists;
    !> otherwise return "".  Port of help.el:help--binding-locus (with
    !> help--key-binding-keymap; no remapping; the variables are those
    !> `register_keymap_name' knows, the minor mode maps and the major
    !> mode's map).
    function binding_locus(keys) result(name)
        type(value_type), intent(in) :: keys(:)
        character(len=:), allocatable :: name
        type(value_type) :: active, val
        type(value_type), allocatable :: maps(:)
        type(status_type) :: status
        type(handle_type) :: map
        type(major_mode_type), pointer :: mode
        logical :: found
        integer :: k
        name = ""
        ! help--key-binding-keymap: the first active map binding KEY.
        active = current_active_maps()
        if (keymap_ref_p(active)) then
            allocate (maps(1))
            maps(1) = active
        else
            call list_elements(active, maps)
        end if
        found = .false.
        do k = 1, size(maps)
            if (.not. keymap_ref_p(maps(k))) cycle
            call lookup_key(maps(k), keys, val, status, accept_default=.true.)
            if (.not. status%ok()) cycle
            ! A number: the first events of KEY were found but not all.
            if (nilp(val) .or. fixnump(val)) cycle
            map = xhandle(maps(k))
            found = .true.
            exit
        end do
        if (.not. found) return
        ! Look into these advertised symbols first.
        if (keymap_live_p(overriding_terminal_local_map)) then
            if (same_map(map, overriding_terminal_local_map)) then
                name = "overriding_terminal_local_map"
                return
            end if
        end if
        name = active_minor_mode_of_map(map)
        if (len(name) > 0) then
            name = name//"_map"
            return
        end if
        if (same_map(map, current_global_map())) then
            name = "global_map"
            return
        end if
        if (cur /= 0) then
            if (allocated(buffers(cur)%major_mode)) then
                mode => major_mode_named(buffers(cur)%major_mode)
                if (associated(mode)) then
                    if (same_map(map, mode%map)) then
                        name = trim(mode%name)//"_map"
                        return
                    end if
                end if
            end if
        end if
        ! Only look in other symbols otherwise.
        name = keymap_name(map)
    end function binding_locus

    logical function same_map(a, b)
        type(handle_type), intent(in) :: a, b
        same_map = a%index == b%index .and. a%generation == b%generation
    end function same_map

    !> Read a key sequence from the user; `ok' false at end of input.
    !> Port of help.el:help--read-key-sequence (one key sequence).
    subroutine read_help_key(keys, binding, ok, status)
        type(value_type), allocatable, intent(out) :: keys(:)
        type(value_type), intent(out) :: binding
        logical, intent(out) :: ok
        type(status_type), intent(out) :: status
        logical :: eof
        call message_nolog("Describe the following key, mouse click, or menu item: ")
        call read_key_sequence(keys, binding, eof, status)
        call message("")
        ok = status%ok() .and. .not. eof
    end subroutine read_help_key

    !> Whether `defn' leaves a key undefined: nil, a number (a key too
    !> long) or `undefined'.  Port of help.el:help--binding-undefined-p.
    logical function binding_undefined_p(defn)
        type(value_type), intent(in) :: defn
        binding_undefined_p = nilp(defn) .or. fixnump(defn)
        if (.not. binding_undefined_p .and. symbolp(defn)) binding_undefined_p = value_eq(defn, sym("undefined"))
    end function binding_undefined_p

    !> Display documentation of the function invoked by KEY_LIST.
    !> Interactively, this command can't describe prefix commands, but
    !> will always wait for the user to type the complete key sequence.
    !> Port of help.el:describe-key (one key sequence, no translations;
    !> a keyboard macro binding is deferred: upstream names it by an
    !> address hash, help-fns-function-name).
    subroutine describe_key(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        character(len=:), allocatable :: desc, name, text, lead
        logical :: ok
        call read_help_key(keys, binding, ok, status)
        if (.not. ok) return
        desc = key_description(keys)
        if (binding_undefined_p(binding)) then
            call message(desc//" is undefined")
            return
        end if
        if (.not. symbolp(binding)) then
            call message(desc//" runs a keyboard macro")
            return
        end if
        call symbol_name(binding, name, status)
        if (.not. status%ok()) return
        lead = desc//" runs the command "//name//found_in(keys)//", which is "
        text = lead
        if (associated(describe_function_1_hook)) call describe_function_1_hook(name, text, lead)
        call with_help_window(text, status)
    end subroutine describe_key

    !> Print the name of the functions KEY_LIST invokes.
    !> Port of help.el:describe-key-briefly (no INSERT; a keyboard macro
    !> binding is deferred, as in `describe_key').
    subroutine describe_key_briefly(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        character(len=:), allocatable :: desc, name
        logical :: ok
        call read_help_key(keys, binding, ok, status)
        if (.not. ok) return
        desc = key_description(keys)
        if (binding_undefined_p(binding)) then
            call message(desc//" is undefined")
        else if (symbolp(binding)) then
            call symbol_name(binding, name, status)
            if (.not. status%ok()) return
            call message(desc//" runs the command "//name)
        else
            call message(desc//" runs a keyboard macro")
        end if
    end subroutine describe_key_briefly

    !> Print message listing key sequences that invoke the command DEFINITION.
    !> Port of help.el:where-is (no INSERT, aliases or remapping).
    recursive subroutine where_is(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name, keys_text
        type(value_type) :: all
        type(value_type), allocatable :: seqs(:), events(:)
        integer :: k
        call completing_read("Where is command: ", completion_obarray_table(), name, status, commandp, &
                             require_match=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        if (len(name) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No command")
            return
        end if
        call where_is_internal(sym(name), all)
        if (nilp(all)) then
            call message(name//" is not on any key")
            return
        end if
        call list_elements(all, seqs)
        keys_text = ""
        do k = 1, size(seqs)
            call list_elements(seqs(k), events)
            if (k > 1) keys_text = keys_text//", "
            keys_text = keys_text//key_description(events)
        end do
        call message(name//" is on "//keys_text)
    end subroutine where_is

    !> Just exit from the Help command's command loop.
    !> Port of help.el:help-quit.
    subroutine help_quit(status)
        type(status_type), intent(out) :: status
        status = status_type()
    end subroutine help_quit

    ! ------------------------------------------------------------------
    ! Messages and lossage
    ! ------------------------------------------------------------------

    !> View the log of recent echo-area messages: the `*Messages*' buffer.
    !> The number of messages retained in that buffer is specified by
    !> the variable `message_log_max'.  Port of
    !> help.el:view-echo-area-messages.
    subroutine view_echo_area_messages(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, old, win
        type(status_type) :: st
        integer(int64) :: pt
        call messages_buffer(buf, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        call goto_char(buffers(cur)%zv)
        pt = buffers(cur)%pt
        call display_buffer(buf, win, status)
        ! If the buffer is already displayed, we need to forcibly set the
        ! window point to scroll to the end of the buffer.
        if (status%ok() .and. window_live_p(win)) call set_window_point(windows(win%index), pt, st)
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine view_echo_area_messages

    !> Display last few input keystrokes and the commands run.
    !> For convenience this uses the same format as
    !> `edit_last_kbd_macro'.
    !> See `lossage_size' to update the number of recorded keystrokes.
    !>
    !> To record all your input, use `open_dribble_file'.  Port of
    !> help.el:view-lossage (no AUTO-REFRESH, no buttons; the commands'
    !> comments are aligned as newcomment.el's comment-indent aligns them,
    !> with `comment_column' 24).
    subroutine view_lossage(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        text = " "//lossage_make_recent_keys()
        call lossage_comment_indent(text)
        call with_help_window(text, status)
    end subroutine view_lossage

    !> A string containing all the recent keys and their commands, the
    !> commands as ";; NAME" comments ending their lines.  Port of
    !> help.el:help--lossage-make-recent-keys (no MOST-RECENT; a key
    !> sequence that ran no command is shown as null()).
    function lossage_make_recent_keys() result(s)
        character(len=:), allocatable :: s
        type(value_type), allocatable :: keys(:)
        logical, allocatable :: is_command(:)
        character(len=:), allocatable :: name
        type(status_type) :: st
        integer :: k
        call recent_keys(.true., keys, is_command)
        s = ""
        do k = 1, size(keys)
            if (k > 1) s = s//" "
            if (is_command(k)) then
                if (nilp(keys(k))) then
                    s = s//";; null()"//LF
                else if (symbolp(keys(k))) then
                    call symbol_name(keys(k), name, st)
                    s = s//";; "//name//LF
                else
                    s = s//";; anonymous-command"//LF
                end if
            else
                s = s//single_key_description(keys(k))
            end if
        end do
    end function lossage_make_recent_keys

    !> Indent the comment of each line of `text' as view-lossage does,
    !> with newcomment.el's comment-indent (`comment_start' ";; ",
    !> `comment_start_skip' ";; ", `comment_use_syntax' false,
    !> `comment_column' 24), in a buffer whose `fill_column' and
    !> `indent_tabs_mode' are the defaults.  Port of the loop of
    !> help.el:view-lossage with newcomment.el:comment-indent,
    !> comment-indent-default and comment-choose-indent.
    subroutine lossage_comment_indent(text)
        character(len=:), allocatable, intent(inout) :: text
        character(len=:), allocatable :: lines(:)
        character(len=:), allocatable :: out, line
        integer, allocatable :: lens(:)
        integer :: n, k, begpos, codecol, indent, lo, hi, other, j, ocol, omax, omin, fill, tabw
        logical :: trailing_newline, tabs
        call default_settings(fill, tabw, tabs)
        trailing_newline = .false.
        if (len(text) > 0) trailing_newline = text(len(text):len(text)) == LF
        call split_lines(text, lines, lens, n)
        do k = 1, n
            line = lines(k) (1:lens(k))
            begpos = index(line, ";; ")
            if (begpos == 0) then
                ! No comment: insert one at comment-column.
                line = line//indent_string(text_columns(line), COMMENT_COLUMN, tabw, tabs)
                if (len(line) > 0) then
                    if (line(len(line):len(line)) /= " " .and. line(len(line):len(line)) /= TAB) line = line//" "
                end if
                begpos = len(line) + 1
                line = line//";; "
            end if
            codecol = text_columns(line(1:begpos - 1))
            j = len_trim_ws(line(1:begpos - 1))
            if (j == 0) then
                ! No code before the comment: comment-indent-default gives
                ! nil, and indent-according-to-mode indents the line like
                ! the previous one (indent-relative's ad-hoc replacement).
                indent = 0
                if (k > 1) indent = line_indentation(lines(k - 1) (1:lens(k - 1)))
                line = indent_string(0, indent, tabw, tabs)//line(verify(line//"x", " "//TAB):)
                lines(k) = pad_to(line, len(lines(k)))
                lens(k) = len(line)
                cycle
            end if
            ! comment-choose-indent with INDENT comment-column.
            indent = COMMENT_COLUMN
            hi = codecol + (fill - text_columns(line))
            lo = COMMENT_INLINE_OFFSET + text_columns(line(1:j))
            if (hi < lo) hi = lo
            if (hi <= indent) indent = hi
            ! Let's try to align to a comment on the previous line.
            other = -1
            if (k > 1) then
                ocol = index(lines(k - 1) (1:lens(k - 1)), ";; ")
                if (ocol > 0) other = text_columns(lines(k - 1) (1:ocol - 1))
            end if
            if (other >= 0 .and. other <= hi .and. other >= lo) then
                indent = other
            else
                ! Try and choose an indentation point with which the comments
                ! on the following lines will be able to align themselves.
                do j = k + 1, n
                    ocol = index(lines(j) (1:lens(j)), ";; ")
                    if (ocol == 0) exit
                    omax = text_columns(lines(j) (1:ocol - 1)) + (fill - text_columns(lines(j) (1:lens(j))))
                    omin = text_columns(lines(j) (1:len_trim_ws(lines(j) (1:ocol - 1)))) + 1
                    if (omax >= lo .and. omin <= hi) then
                        lo = max(omin, lo)
                        hi = min(omax, hi)
                    else
                        exit
                    end if
                end do
                indent = max(lo, min(hi, indent))
            end if
            if (codecol /= indent) then
                j = len_trim_ws(line(1:begpos - 1))
                line = line(1:j)//indent_string(text_columns(line(1:j)), indent, tabw, tabs)//line(begpos:)
            end if
            lines(k) = pad_to(line, max(len(lines(k)), len(line)))
            lens(k) = len(line)
        end do
        out = ""
        do k = 1, n
            out = out//lines(k) (1:lens(k))
            if (k < n .or. trailing_newline) out = out//LF
        end do
        text = out
    end subroutine lossage_comment_indent

    !> `line' padded with blanks to `n' characters.
    function pad_to(line, n) result(s)
        character(len=*), intent(in) :: line
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        s = line//repeat(" ", max(0, n - len(line)))
    end function pad_to

    !> The lines of `text' (without their newlines; no last empty line
    !> after a final newline) in `lines', their lengths in `lens'.
    subroutine split_lines(text, lines, lens, n)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: lines(:)
        integer, allocatable, intent(out) :: lens(:)
        integer, intent(out) :: n
        integer :: i, start, maxlen, k
        n = 0
        maxlen = 0
        start = 1
        do i = 1, len(text)
            if (text(i:i) == LF) then
                n = n + 1
                maxlen = max(maxlen, i - start)
                start = i + 1
            end if
        end do
        if (start <= len(text)) then
            n = n + 1
            maxlen = max(maxlen, len(text) - start + 1)
        end if
        ! Room for the indentation the lines may get.
        allocate (character(len=maxlen + 64) :: lines(n))
        allocate (lens(n))
        k = 0
        start = 1
        do i = 1, len(text)
            if (text(i:i) == LF) then
                k = k + 1
                lines(k) = text(start:i - 1)
                lens(k) = i - start
                start = i + 1
            end if
        end do
        if (start <= len(text)) then
            k = k + 1
            lines(k) = text(start:)
            lens(k) = len(text) - start + 1
        end if
    end subroutine split_lines

    !> The length of `s' without trailing blanks and tabs.
    integer function len_trim_ws(s) result(n)
        character(len=*), intent(in) :: s
        n = len(s)
        do while (n > 0)
            if (s(n:n) /= " " .and. s(n:n) /= TAB) exit
            n = n - 1
        end do
    end function len_trim_ws

    !> The column of the first non-blank character of `line' (as
    !> current-indentation).
    integer function line_indentation(line) result(col)
        character(len=*), intent(in) :: line
        integer :: k
        k = verify(line, " "//TAB)
        if (k == 0) k = len(line) + 1
        col = text_columns(line(1:k - 1))
    end function line_indentation

    !> The whitespace indent.c:Findent_to inserts to go from column
    !> `fromcol' to `mincol': tabs if `tabs' (indent-tabs-mode), then
    !> spaces.
    function indent_string(fromcol, mincol, tabw, tabs) result(s)
        integer, intent(in) :: fromcol, mincol, tabw
        logical, intent(in) :: tabs
        character(len=:), allocatable :: s
        integer :: n, col
        s = ""
        col = fromcol
        if (mincol <= col) return
        if (tabs .and. tabw > 0) then
            n = mincol/tabw - col/tabw
            if (n > 0) then
                s = repeat(TAB, n)
                col = (mincol/tabw)*tabw
            end if
        end if
        s = s//repeat(" ", mincol - col)
    end function indent_string

    !> The display columns `text' (one line) takes, tabs included.
    integer function text_columns(text) result(col)
        character(len=*), intent(in) :: text
        integer :: i, clen, tabw
        integer(int32) :: c
        integer :: dummy
        logical :: tabs
        call default_settings(dummy, tabw, tabs)
        col = 0
        i = 1
        do while (i <= len(text))
            clen = utf8_length(text(i:i))
            if (text(i:i) == TAB) then
                col = (col/tabw + 1)*tabw
            else
                c = utf8_code(text(i:min(len(text), i + clen - 1)))
                col = col + glyph_columns(c, col)
            end if
            i = i + clen
        end do
    end function text_columns

    !> The code point of the UTF-8 sequence `s'.
    integer(int32) function utf8_code(s) result(c)
        character(len=*), intent(in) :: s
        integer :: k, n
        n = len(s)
        c = iachar(s(1:1))
        if (n == 1) return
        c = iand(c, int(z"FF", int32)/(2**(n + 1)))
        do k = 2, n
            c = c*64 + iand(iachar(s(k:k)), 63)
        end do
    end function utf8_code

    !> The default values of `fill_column', `tab_width' and
    !> `indent_tabs_mode' (those of a buffer such as *Help*).
    subroutine default_settings(fill, tabw, tabs)
        integer, intent(out) :: fill, tabw
        logical, intent(out) :: tabs
        type(value_type) :: v
        type(status_type) :: st
        fill = 70
        tabw = 8
        tabs = .true.
        call default_value(sym("fill_column"), v, st)
        if (st%ok() .and. fixnump(v)) fill = int(xfixnum(v))
        call default_value(sym("tab_width"), v, st)
        if (st%ok() .and. fixnump(v)) tabw = int(xfixnum(v))
        if (tabw <= 0 .or. tabw > 1000) tabw = 8
        call default_value(sym("indent_tabs_mode"), v, st)
        if (st%ok()) tabs = .not. nilp(v)
    end subroutine default_settings

    ! ------------------------------------------------------------------
    ! Help for help
    ! ------------------------------------------------------------------

    !> The text of the help-for-help screen, as help.el's help-for-help
    !> makes it.  A command that is not ported yet is listed under the
    !> key upstream's `help_map' gives it (help.el:help-map), which stays
    !> unbound here.
    function help_for_help_text() result(text)
        character(len=:), allocatable :: text
        text = "(Type "//"<PageDown>"//" or "//"<PageUp>"//" to scroll, "//"C-s"// &
               " to search, or \<help_map>\[help_quit] to exit.)"
        call section("Commands, Keys and Functions", &
                     [character(len=40) :: "describe_mode", "describe_bindings", "describe_key", &
                      "describe_key_briefly", "where_is", "", "apropos_command", "apropos_documentation", &
                      "describe_command", "describe_function", "help_find_source", "describe_variable", &
                      "describe_symbol"], &
                     [character(len=4) :: "m", "b", "k", "c", "w", "", "a", "d", "x", "f", "4 s", "v", "o"], &
                     [character(len=72) :: "Show help for current major and minor modes and their commands", &
                      "Show all key bindings", "Show help for key", "Show help for key briefly", &
                      "Show which key runs a specific command", "", &
                      "Search for commands (see also \[apropos])", &
                      "Search documentation of functions, variables, and other items", &
                      "Show help for command", "Show help for function", &
                      "Show the source for what's being described in *Help*", "Show help for variable", &
                      "Show help for function or variable"])
        call section("Manuals", &
                     [character(len=40) :: "info_emacs_manual", "Info_goto_emacs_command_node", &
                      "Info_goto_emacs_key_command_node", "info", "info_display_manual", "info_lookup_symbol"], &
                     [character(len=4) :: "r", "F", "K", "i", "R", "S"], &
                     [character(len=72) :: "Show Emacs manual", "Show Emacs manual section for command", &
                      "Show Emacs manual section for a key sequence", "Show all installed manuals", &
                      "Show a specific manual", "Show description of symbol in pertinent manual"])
        call section("Other Help Commands", &
                     [character(len=40) :: "view_external_packages", "finder_by_keyword", "describe_package", "", &
                      "help_with_tutorial", "help_quick_toggle", "view_echo_area_messages", "view_lossage", &
                      "display_local_help"], &
                     [character(len=4) :: "C-e", "p", "P", "", "t", "C-q", "e", "l", "."], &
                     [character(len=72) :: "Extending Emacs with external packages", &
                      "Search for Emacs packages (see also \[list_packages])", "Describe a specific Emacs package", &
                      "", "Start the Emacs tutorial", "Display the quick help buffer.", &
                      "Show recent messages (from echo area)", "Show last "//int_text(lossage_size())// &
                      " input keystrokes (lossage)", "Show local help at point"])
        call section("Miscellaneous", &
                     [character(len=40) :: "about_emacs", "view_emacs_FAQ", "C-n", &
                      "view_emacs_problems", "view_emacs_debugging", "", "describe_gnu_project", &
                      "describe_copying", "describe_distribution", "C-m", "view_emacs_todo", &
                      "describe_no_warranty"], &
                     [character(len=4) :: "C-a", "C-f", "", "C-p", "C-d", "", "g", "C-c", "C-o", "", "C-t", "C-w"], &
                     [character(len=72) :: "About Emacs", "Emacs FAQ", "News of recent changes", "Known problems", &
                      "Debugging Emacs", "", "About the GNU project", &
                      "Emacs copying permission (GNU General Public License)", &
                      "Emacs ordering and distribution information", "Order printed manuals", "Emacs TODO", &
                      "Information on absence of warranty"])
        call section("Internationalization and Coding Systems", &
                     [character(len=40) :: "describe_input_method", "describe_coding_system", &
                      "describe_language_environment", "describe_syntax", "view_hello_file"], &
                     [character(len=4) :: "I", "C", "L", "s", "h"], &
                     [character(len=72) :: "Describe input method", "Describe coding system", &
                      "Describe language environment", "Show current syntax table", &
                      "Display the HELLO file illustrating various scripts"])
        text = text//LF
    contains

        !> Append a section: its title, then a line per command (an empty
        !> name gives an empty line).  A name such as C-n is shown as it
        !> is; a ported command by its key (\[NAME]), one not ported yet
        !> by its key in upstream's help map, `key'.  Port of
        !> help.el:help--for-help-make-sections and
        !> help--for-help-make-commands.
        subroutine section(title, names, keys, descs)
            character(len=*), intent(in) :: title, names(:), keys(:), descs(:)
            character(len=:), allocatable :: name
            integer :: k
            text = text//LF//LF//title//LF//LF
            do k = 1, size(names)
                if (k > 1) text = text//LF
                name = trim(names(k))
                if (len(name) == 0) cycle
                text = text//"   "
                if (len_trim(keys(k)) == 0) then
                    ! (string-match "\`C-\w\'" name)
                    text = text//name
                else if (command_defined_p(sym(name))) then
                    text = text//"\["//name//"]"
                else
                    text = text//trim(keys(k))
                end if
                text = text//" "//TAB//trim(descs(k))
            end do
        end subroutine section
    end function help_for_help_text

    !> Help command.  Show the help text and read a help option, then run
    !> the help command bound to it in `help_map'.  Port of
    !> help.el:help-for-help (with help-macro.el:make-help-screen and
    !> help--help-screen; `three_step_help' false).
    recursive subroutine help_for_help(status)
        type(status_type), intent(out) :: status
        call help_screen(help_for_help_text(), help_map, " *Metahelp*", status)
    end subroutine help_for_help

    !> Display `help_text' in buffer `buffer_name' and read keys there,
    !> scrolling with the scrolling keys, until one that is bound in
    !> `helped_map', whose command is then run with the previous window
    !> configuration.  Port of help-macro.el:help--help-screen
    !> (`three_step_help' false; no frames or mouse events).
    recursive subroutine help_screen(help_text, helped_map, buffer_name, status)
        character(len=*), intent(in) :: help_text, buffer_name
        type(handle_type), intent(in) :: helped_map
        type(status_type), intent(out) :: status
        type(handle_type) :: local_map, buf, saved_override
        type(window_configuration_type) :: config
        type(value_type), allocatable :: keys(:), tkey(:)
        type(value_type) :: binding, defn, ch
        type(status_type) :: st
        character(len=:), allocatable :: prompt, name
        logical :: eof, have_config
        call make_sparse_keymap(local_map)
        call set_keymap_parent(local_map, helped_map, st)
        allocate (tkey(1))
        tkey(1) = t_value()
        call define_key(local_map, tkey, sym("undefined"), st)
        call current_window_configuration(config)
        have_config = .true.
        call get_buffer_create(buffer_name, buf, status)
        if (.not. status%ok()) return
        call pop_to_buffer(buf, status, norecord=.true.)
        if (.not. status%ok()) return
        call set_buffer(buf, st)
        call help_mode(st)
        buffers(cur)%undo_enabled = .false.
        call set_buffer_read_only(buf, .false., st)
        call erase_buffer(st)
        call insert(substitute_command_keys(help_text), st)
        call set_buffer_read_only(buf, .true., st)
        call goto_char(buffers(cur)%begv)
        ch = make_fixnum(iachar("?", int64))
        saved_override = overriding_terminal_local_map
        do while (help_screen_key_p(ch, keys))
            st = status_type()
            if (event_is(ch, 22_int64) .or. event_is(ch, 32_int64) .or. event_named(ch, "next") .or. &
                event_named(ch, "end")) then
                call scroll_up(nil(), st)
            else if (event_is(ch, 127_int64) .or. event_is(ch, M_V) .or. event_is(ch, S_SPC) .or. &
                     event_named(ch, "deletechar") .or. event_named(ch, "backspace") .or. &
                     event_named(ch, "prior") .or. event_named(ch, "home")) then
                call scroll_down(nil(), st)
            else if (event_named(ch, "down")) then
                call scroll_up(make_fixnum(1_int64), st)
            else if (event_named(ch, "up")) then
                call scroll_down(make_fixnum(1_int64), st)
            end if
            if (pos_visible_in_window_p(buffers(cur)%zv, selected_window())) then
                prompt = "Type one of listed options: "
            else
                prompt = "Type one of listed options, or <PageDown>/<PageUp>/SPC/DEL to scroll: "
            end if
            overriding_terminal_local_map = local_map
            cursor_in_echo_area = .true.
            call message_nolog(prompt)
            call read_key_sequence(keys, binding, eof, status)
            cursor_in_echo_area = .false.
            overriding_terminal_local_map = saved_override
            if (.not. status%ok() .or. eof) then
                if (status%ok()) call set_error(status, FM_QUIT)
                exit
            end if
            ch = keys(1)
        end do
        ! We don't need the prompt any more.
        call message("")
        if (status%ok()) then
            call lookup_key(make_keymap_ref(local_map), keys, defn, st)
            if (fixnump(defn)) defn = nil()
            if (.not. nilp(defn)) then
                call set_window_configuration(config)
                have_config = .false.
                if (symbolp(defn)) then
                    call call_interactively(defn, status)
                end if
            else
                name = key_description(keys)
                if (name /= "C-g") then
                    call message(substitute_command_keys("No help command is bound to `\`"//name//"''"))
                    call bitch_at_user(st)
                end if
            end if
        end if
        if (have_config) call set_window_configuration(config)
        call free_window_configuration(config)
        call free_keymap(local_map, st)
    end subroutine help_screen

    !> Whether event `ch' keeps the help screen reading keys: the help
    !> character, the `help_event_list' events (<help>, <f1>, ?) and the
    !> scrolling keys.
    logical function help_screen_key_p(ch, keys) result(p)
        type(value_type), intent(in) :: ch
        type(value_type), allocatable, intent(in) :: keys(:)
        integer :: k
        character(len=19), parameter :: names(12) = [character(len=19) :: "help", "f1", "deletechar", "backspace", &
                                                                      "vertical-scroll-bar", "home", "end", "next", "prior", "up", &
                                                                                                      "down", "vertical_scroll_bar"]
        p = event_is(ch, 63_int64) .or. event_is(ch, 8_int64) .or. event_is(ch, 22_int64) .or. &
            event_is(ch, 32_int64) .or. event_is(ch, 127_int64) .or. event_is(ch, M_V) .or. event_is(ch, S_SPC)
        do k = 1, size(names)
            if (event_named(ch, trim(names(k)))) p = .true.
        end do
        if (allocated(keys)) then
            ! (equal key "\M-v")
            if (size(keys) == 2) then
                if (event_is(keys(1), 27_int64) .and. event_is(keys(2), iachar("v", int64))) p = .true.
            end if
        end if
    end function help_screen_key_p

    logical function event_is(ev, c)
        type(value_type), intent(in) :: ev
        integer(int64), intent(in) :: c
        event_is = .false.
        if (fixnump(ev)) event_is = xfixnum(ev) == c
    end function event_is

    logical function event_named(ev, name)
        type(value_type), intent(in) :: ev
        character(len=*), intent(in) :: name
        event_named = .false.
        if (symbolp(ev)) event_named = value_eq(ev, sym(name))
    end function event_named

    !> `n' as decimal digits.
    function int_text(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=12) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function int_text

    ! ------------------------------------------------------------------
    ! Key bindings
    ! ------------------------------------------------------------------

    !> Display a buffer showing a list of all defined keys, and their
    !> definitions.  The keys are displayed in order of precedence.
    !> Port of help.el:describe-bindings (no PREFIX or BUFFER; Outline
    !> minor mode, which `describe_bindings_outline' turns on upstream,
    !> is not ported).
    subroutine describe_bindings(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        call describe_buffer_bindings(text)
        call with_help_window(text, status)
    end subroutine describe_bindings

    !> The list of all defined keys and their definitions, looked up in
    !> the current buffer: key translations, overriding bindings, the
    !> minor mode maps, the major mode map, the global map, and the
    !> function key and input decoding translations.  Port of
    !> keymap.c:Fdescribe_buffer_bindings (no PREFIX, keyboard-translate-
    !> table, text property maps or menus).
    subroutine describe_buffer_bindings(text)
        character(len=:), allocatable, intent(out) :: text
        type(value_type), allocatable :: shadow(:), none(:)
        character(len=48), allocatable :: modes(:)
        type(handle_type), allocatable :: maps(:)
        type(handle_type) :: local
        character(len=:), allocatable :: major
        integer :: i
        text = ""
        allocate (shadow(0), none(0))
        if (keymap_live_p(key_translation_map)) then
            call describe_map_tree(key_translation_map, .false., none, "Key translations", .true., text)
        end if
        if (keymap_live_p(overriding_terminal_local_map)) then
            call describe_map_tree(overriding_terminal_local_map, .true., shadow, FF//LF//"Overriding Bindings", &
                                   .false., text)
            call push_shadow(shadow, overriding_terminal_local_map)
        else
            ! Print the minor mode and major mode keymaps.
            call current_minor_maps(modes, maps)
            do i = 1, size(maps)
                call describe_map_tree(maps(i), .true., shadow, FF//LF//LQUOTE//trim(modes(i))//RQUOTE// &
                                       " Minor Mode Bindings", .false., text)
                call push_shadow(shadow, maps(i))
            end do
            local = current_local_map()
            if (keymap_live_p(local)) then
                major = "fundamental_mode"
                if (allocated(buffers(cur)%major_mode)) major = buffers(cur)%major_mode
                call describe_map_tree(local, .true., shadow, FF//LF//LQUOTE//major//RQUOTE//" Major Mode Bindings", &
                                       .false., text)
                call push_shadow(shadow, local)
            end if
        end if
        call describe_map_tree(current_global_map(), .true., shadow, FF//LF//"Global Bindings", .false., text, &
                               mention_shadow=.true.)
        ! Print the function-key-map translations under this prefix.
        if (keymap_live_p(local_function_key_map)) then
            call describe_map_tree(local_function_key_map, .false., none, FF//LF//"Function key map translations", &
                                   .true., text)
        end if
        ! Print the input-decode-map translations under this prefix.
        if (keymap_live_p(input_decode_map)) then
            call describe_map_tree(input_decode_map, .false., none, FF//LF//"Input decoding map translations", &
                                   .true., text)
        end if
    end subroutine describe_buffer_bindings

    !> Add keymap `map' to the front of the shadowing maps.
    subroutine push_shadow(shadow, map)
        type(value_type), allocatable, intent(inout) :: shadow(:)
        type(handle_type), intent(in) :: map
        type(value_type), allocatable :: grown(:)
        integer :: n
        n = size(shadow)
        allocate (grown(n + 1))
        grown(1) = make_keymap_ref(map)
        if (n > 0) grown(2:) = shadow
        call move_alloc(grown, shadow)
    end subroutine push_shadow

    !> Append to `text' a description of the key bindings in STARTMAP,
    !> followed by the key bindings of all maps reachable through
    !> STARTMAP.  If PARTIAL, omit certain uninteresting commands (such as
    !> `undefined').  SHADOW is a list of maps; don't mention keys which
    !> would be shadowed by any of them.  TITLE (if not empty) is put at
    !> the beginning, with a colon, if there is anything to describe.  If TRANSL, the definitions
    !> are actually key translations.  If MENTION_SHADOW, keys shadowed by
    !> SHADOW are mentioned, saying they are shadowed.  Port of
    !> help.el:help--describe-map-tree (no PREFIX, menus or ALWAYS-TITLE, which
    !> matters only when there is nothing to describe).
    subroutine describe_map_tree(startmap, partial, shadow, title, transl, text, mention_shadow)
        type(handle_type), intent(in) :: startmap
        logical, intent(in) :: partial, transl
        type(value_type), intent(in) :: shadow(:)
        character(len=*), intent(in) :: title
        character(len=:), allocatable, intent(inout) :: text
        logical, intent(in), optional :: mention_shadow
        type(key_list_type), allocatable :: maps(:)
        type(value_type), allocatable :: sub_shadows(:)
        type(value_type) :: tem
        type(status_type) :: st
        character(len=:), allocatable :: body
        logical :: mention, describe
        integer :: k, j
        mention = .false.
        if (present(mention_shadow)) mention = mention_shadow
        call accessible_keymaps(startmap, maps)
        body = ""
        do k = 1, size(maps)
            ! sub-shadows: the binding of the prefix in SHADOW.
            allocate (sub_shadows(0))
            describe = .true.
            if (size(shadow) > 0) then
                if (size(maps(k)%keys) == 0) then
                    sub_shadows = shadow
                else
                    call lookup_key(make_list(shadow), maps(k)%keys, tem, st, accept_default=.true.)
                    if (fixnump(tem)) then
                        ! A number: the prefix is too long; no shadows.
                        continue
                    else if (keymap_ref_p(tem)) then
                        call append_shadow(sub_shadows, tem)
                    else if (.not. nilp(tem)) then
                        ! Describe this map iff the prefix is bound to a
                        ! keymap, since otherwise it completely shadows it.
                        describe = .false.
                    end if
                end if
            end if
            if (describe) then
                ! Maps we have already listed in this loop shadow this map.
                do j = 1, k - 1
                    if (same_keys(maps(j)%keys, maps(k)%keys)) call append_shadow(sub_shadows, make_keymap_ref(maps(j)%map))
                end do
                call describe_map(maps(k)%map, maps(k)%keys, transl, partial, sub_shadows, mention, body)
            end if
            deallocate (sub_shadows)
        end do
        ! Print title unless the keymap was empty.
        if (len(body) == 0) return
        ! The first map's leading newline goes.
        if (body(1:1) == LF) body = body(2:)
        if (len(title) > 0) text = text//title//":"//LF
        text = text//LF//"Key             Binding"//LF//make_separator_line()//body
    end subroutine describe_map_tree

    subroutine append_shadow(list, v)
        type(value_type), allocatable, intent(inout) :: list(:)
        type(value_type), intent(in) :: v
        type(value_type), allocatable :: grown(:)
        integer :: n
        n = size(list)
        allocate (grown(n + 1))
        if (n > 0) grown(1:n) = list
        grown(n + 1) = v
        call move_alloc(grown, list)
    end subroutine append_shadow

    logical function same_keys(a, b)
        type(value_type), intent(in) :: a(:), b(:)
        integer :: i
        same_keys = size(a) == size(b)
        if (.not. same_keys) return
        do i = 1, size(a)
            if (.not. value_eq(a(i), b(i))) then
                same_keys = .false.
                return
            end if
        end do
    end function same_keys

    !> Append to `text' the description of the contents of keymap MAP,
    !> reached by the sequence of prefix keys PREFIX: the runs of
    !> characters of its char-table (`describe_vector'), then its other
    !> bindings, sorted, each section aligned.  Port of help.el:describe-map
    !> with keymap-canonicalize (subr.el), help--describe-map-compare,
    !> help--shadow-lookup and help--describe-command.
    subroutine describe_map(map, prefix, transl, partial, shadow, mention_shadow, text)
        type(handle_type), intent(in) :: map
        type(value_type), intent(in) :: prefix(:), shadow(:)
        logical, intent(in) :: transl, partial, mention_shadow
        character(len=:), allocatable, intent(inout) :: text
        type(value_type), allocatable :: events(:), bindings(:), ev(:), def(:), remapped(:), one(:), key(:), seqs(:)
        logical, allocatable :: shadowed(:), seen(:)
        integer(int64), allocatable :: rfrom(:), rto(:)
        type(value_type), allocatable :: rdef(:)
        type(value_type) :: tem, all_keys
        character(len=:), allocatable :: keytext, block
        type(entry_line_type), allocatable :: lines(:)
        integer :: i, j, n, nr, nl, start
        integer(int64) :: a, b
        logical :: keep, is_remap
        call map_keymap_items(map, events, bindings)
        ! keymap-canonicalize: char ranges of more than two characters go
        ! to a char-table, the rest are bindings; the first binding of an
        ! event (the map's own, before its parents') wins.
        allocate (ev(size(events)), def(size(events)), rfrom(size(events)), rto(size(events)), rdef(size(events)))
        n = 0
        nr = 0
        do i = 1, size(events)
            if (vtag(events(i)) == V_LIST) then
                a = xfixnum(list_elt(events(i), 1_int64))
                b = xfixnum(list_elt(events(i), 2_int64))
                if (b == a + 1) then
                    call add_binding(make_fixnum(a), bindings(i))
                    call add_binding(make_fixnum(b), bindings(i))
                else
                    keep = .true.
                    do j = 1, nr
                        if (a >= rfrom(j) .and. a <= rto(j)) keep = .false.
                    end do
                    if (keep) then
                        nr = nr + 1
                        rfrom(nr) = a
                        rto(nr) = b
                        rdef(nr) = bindings(i)
                    end if
                end if
            else
                call add_binding(events(i), bindings(i))
            end if
        end do
        ! The char-table: help--describe-vector.
        if (nr > 0) call describe_vector(rfrom(1:nr), rto(1:nr), rdef(1:nr), prefix, transl, partial, shadow, &
                                         mention_shadow, text)
        ! The other bindings: which to show.
        allocate (shadowed(n), seen(n))
        seen = .false.
        do i = 1, n
            shadowed(i) = .false.
            if (.not. (symbolp(ev(i)) .or. fixnump(ev(i)))) cycle
            ! Don't show undefined commands or suppressed commands.
            if (nilp(def(i))) cycle
            if (partial .and. suppressed_p(def(i))) cycle
            ! Don't show a command that isn't really visible because a
            ! local definition of the same key shadows it.
            if (size(shadow) > 0) then
                allocate (one(1))
                one(1) = ev(i)
                call shadow_lookup(shadow, one, tem)
                deallocate (one)
                if (.not. nilp(tem)) then
                    ! If both bindings are keymaps, this key is a prefix
                    ! key, so don't say it is shadowed.
                    if (.not. (keymap_ref_p(def(i)) .and. keymap_ref_p(tem))) then
                        if (mention_shadow .and. .not. value_eq(tem, def(i))) then
                            shadowed(i) = .true.
                        else
                            cycle
                        end if
                    end if
                end if
            end if
            seen(i) = .true.
        end do
        ! Sort them (a stable insertion sort with help--describe-map-compare).
        call sort_entries(ev, def, shadowed, seen, n)
        allocate (lines(n))
        nl = 0
        i = 1
        do while (i <= n)
            if (.not. seen(i)) then
                i = i + 1
                cycle
            end if
            start = i
            ! Find consecutive chars that are identically defined.
            if (fixnump(ev(i))) then
                do while (i + 1 <= n)
                    if (.not. seen(i + 1)) exit
                    if (.not. fixnump(ev(i + 1))) exit
                    if (xfixnum(ev(i + 1)) /= xfixnum(ev(i)) + 1) exit
                    if (.not. value_equal(def(i + 1), def(i))) exit
                    if (shadowed(i + 1) .neqv. shadowed(i)) exit
                    i = i + 1
                end do
            end if
            if (start /= i .or. .not. keymap_ref_p(def(i))) then
                ! If we're in a <remap> section of the output, then also
                ! display the bindings of the keys that we've remapped from.
                is_remap = .false.
                if (size(prefix) == 1) then
                    if (value_eq(prefix(1), sym("remap")) .and. .not. value_eq(def(i), sym("self_insert_command"))) then
                        call where_is_internal(def(i), all_keys)
                        if (.not. nilp(all_keys)) then
                            call list_elements(all_keys, seqs)
                            call list_elements(seqs(1), remapped)
                            key = remapped(size(remapped):size(remapped))
                            keytext = key_description(key, remapped(1:size(remapped) - 1))
                            is_remap = .true.
                        end if
                    end if
                end if
                if (.not. is_remap) then
                    key = ev(start:start)
                    keytext = key_description(key, prefix)
                end if
                if (start /= i) then
                    key = ev(i:i)
                    keytext = keytext//" .. "//key_description(key, prefix)
                end if
                nl = nl + 1
                lines(nl)%key = keytext
                lines(nl)%def = describe_command(def(i), transl)
                lines(nl)%extra = ""
                if (shadowed(i)) lines(nl)%extra = lines(nl)%extra//LF//"  (this binding is currently shadowed)"
                if (is_remap) then
                    key = ev(start:start)
                    lines(nl)%extra = lines(nl)%extra//LF//"  (Remapped via "//key_description(key, prefix)//")"
                end if
            end if
            i = i + 1
        end do
        if (nl > 0) then
            call align_section(lines(1:nl), block)
            text = text//LF//block
        end if
    contains

        !> Add (event, binding) unless the event has a binding already.
        subroutine add_binding(e, d)
            type(value_type), intent(in) :: e, d
            integer :: m
            do m = 1, n
                if (value_eq(ev(m), e)) return
            end do
            n = n + 1
            ev(n) = e
            def(n) = d
        end subroutine add_binding
    end subroutine describe_map

    !> The binding of `key' in the shadowing maps (lookup-key with
    !> ACCEPT-DEFAULT); nil if the key sequence is too long.  Port of
    !> help.el:help--shadow-lookup (no REMAP).
    subroutine shadow_lookup(shadow, key, val)
        type(value_type), intent(in) :: shadow(:), key(:)
        type(value_type), intent(out) :: val
        type(status_type) :: st
        call lookup_key(make_list(shadow), key, val, st, accept_default=.true.)
        if (.not. st%ok()) val = nil()
        if (fixnump(val)) val = nil()
    end subroutine shadow_lookup

    !> Whether `def' has a `suppress_keymap' property: `undefined'
    !> (subr.el puts it there).
    logical function suppressed_p(def)
        type(value_type), intent(in) :: def
        suppressed_p = value_eq(def, sym("undefined"))
    end function suppressed_p

    !> The description of the definition DEFINITION, as describe-map
    !> shows it: a translation as its key description, a keyboard macro,
    !> "Prefix Command" for a keymap, else the command's name.  Port of
    !> help.el:help--describe-command (no character names after
    !> translations).
    function describe_command(definition, translation) result(s)
        type(value_type), intent(in) :: definition
        logical, intent(in) :: translation
        character(len=:), allocatable :: s
        type(value_type), allocatable :: events(:)
        type(status_type) :: st
        if (stringp(definition) .or. vtag(definition) == V_LIST) then
            if (translation) then
                if (stringp(definition)) then
                    s = xstring(definition)
                else
                    call list_elements(definition, events)
                    s = key_description(events)
                end if
            else
                ! These should be rare nowadays, replaced by `kmacro's.
                s = "Keyboard Macro"
            end if
        else if (keymap_ref_p(definition)) then
            s = "Prefix Command"
        else if (symbolp(definition)) then
            call symbol_name(definition, s, st)
        else
            s = fortran_repr(definition)
        end if
    end function describe_command

    !> Sort the bindings with help--describe-map-compare: characters in
    !> order first, then the other events by name in the "natural" way
    !> (string-version-lessp, so <f2> comes between <f1> and <f11>).
    subroutine sort_entries(ev, def, shadowed, seen, n)
        type(value_type), intent(inout) :: ev(:), def(:)
        logical, intent(inout) :: shadowed(:), seen(:)
        integer, intent(in) :: n
        type(value_type) :: e, d
        logical :: s, v
        integer :: i, j
        do i = 2, n
            e = ev(i)
            d = def(i)
            s = shadowed(i)
            v = seen(i)
            j = i - 1
            do while (j >= 1)
                if (.not. map_less(e, ev(j))) exit
                ev(j + 1) = ev(j)
                def(j + 1) = def(j)
                shadowed(j + 1) = shadowed(j)
                seen(j + 1) = seen(j)
                j = j - 1
            end do
            ev(j + 1) = e
            def(j + 1) = d
            shadowed(j + 1) = s
            seen(j + 1) = v
        end do
    end subroutine sort_entries

    !> Port of help.el:help--describe-map-compare.
    logical function map_less(a, b)
        type(value_type), intent(in) :: a, b
        character(len=:), allocatable :: na, nb
        type(status_type) :: st
        map_less = .false.
        if (fixnump(a) .and. fixnump(b)) then
            map_less = xfixnum(a) < xfixnum(b)
        else if (fixnump(a)) then
            map_less = .true.
        else if (symbolp(a) .and. symbolp(b)) then
            call symbol_name(a, na, st)
            call symbol_name(b, nb, st)
            map_less = string_version_lessp(na, nb)
        end if
    end function map_less

    !> Return true if S1 is less than S2, as version strings: sequences of
    !> digits compare as numbers.  Port of fns.c:Fstring_version_lessp
    !> (filevercmp's ordering of digit runs, by bytes otherwise).
    logical function string_version_lessp(s1, s2) result(less)
        character(len=*), intent(in) :: s1, s2
        integer :: i1, i2, e1, e2, cmp
        less = .false.
        i1 = 1
        i2 = 1
        do
            if (i1 > len(s1) .or. i2 > len(s2)) then
                less = i1 > len(s1) .and. i2 <= len(s2)
                return
            end if
            if (is_digit(s1(i1:i1)) .and. is_digit(s2(i2:i2))) then
                e1 = i1
                do while (e1 < len(s1))
                    if (.not. is_digit(s1(e1 + 1:e1 + 1))) exit
                    e1 = e1 + 1
                end do
                e2 = i2
                do while (e2 < len(s2))
                    if (.not. is_digit(s2(e2 + 1:e2 + 1))) exit
                    e2 = e2 + 1
                end do
                cmp = compare_digits(s1(i1:e1), s2(i2:e2))
                if (cmp /= 0) then
                    less = cmp < 0
                    return
                end if
                i1 = e1 + 1
                i2 = e2 + 1
            else
                if (s1(i1:i1) /= s2(i2:i2)) then
                    less = iachar(s1(i1:i1)) < iachar(s2(i2:i2))
                    return
                end if
                i1 = i1 + 1
                i2 = i2 + 1
            end if
        end do
    end function string_version_lessp

    logical function is_digit(c)
        character, intent(in) :: c
        is_digit = c >= "0" .and. c <= "9"
    end function is_digit

    !> Compare two runs of digits as numbers: -1, 0 or 1.
    integer function compare_digits(a, b) result(cmp)
        character(len=*), intent(in) :: a, b
        integer :: ka, kb
        ka = verify(a, "0")
        kb = verify(b, "0")
        if (ka == 0) ka = len(a) + 1
        if (kb == 0) kb = len(b) + 1
        cmp = 0
        if (len(a) - ka /= len(b) - kb) then
            cmp = merge(-1, 1, len(a) - ka < len(b) - kb)
        else if (a(ka:) /= b(kb:)) then
            cmp = merge(-1, 1, lge(b(kb:), a(ka:)))
        end if
    end function compare_digits

    !> Append to `text' the description of the runs FROM .. TO of
    !> characters bound alike to DEFS in a keymap's char-table, reached by
    !> PREFIX: a run of one character is described as the character, a
    !> longer one as "FIRST .. LAST"; a run that crosses the end of the
    !> 5-byte characters is split there.  Port of
    !> keymap.c:describe_vector (a char-table; KEYMAP_P true) with the
    !> help.el:describe-map describer.
    subroutine describe_vector(from, to, defs, prefix, transl, partial, shadow, mention_shadow, text)
        integer(int64), intent(in) :: from(:), to(:)
        type(value_type), intent(in) :: defs(:), prefix(:), shadow(:)
        logical, intent(in) :: transl, partial, mention_shadow
        character(len=:), allocatable, intent(inout) :: text
        integer(int64), parameter :: MAX_5_BYTE_CHAR = int(z"3FFF7F", int64)
        integer(int64), allocatable :: rf(:), rt(:)
        type(value_type), allocatable :: rd(:), kludge(:)
        integer, allocatable :: order(:)
        type(entry_line_type), allocatable :: lines(:)
        type(value_type) :: shadowed_by, definition
        character(len=:), allocatable :: block, name
        type(status_type) :: st
        integer :: k, m, nl
        logical :: this_shadowed
        ! Split at the end of the 5-byte characters, in character order.
        allocate (rf(2*size(from)), rt(2*size(from)), rd(2*size(from)))
        m = 0
        call order_by_start(from, order)
        do k = 1, size(order)
            if (from(order(k)) <= MAX_5_BYTE_CHAR .and. to(order(k)) > MAX_5_BYTE_CHAR) then
                m = m + 1
                rf(m) = from(order(k))
                rt(m) = MAX_5_BYTE_CHAR
                rd(m) = defs(order(k))
                m = m + 1
                rf(m) = MAX_5_BYTE_CHAR + 1
                rt(m) = to(order(k))
                rd(m) = defs(order(k))
            else
                m = m + 1
                rf(m) = from(order(k))
                rt(m) = to(order(k))
                rd(m) = defs(order(k))
            end if
        end do
        allocate (lines(m), kludge(1))
        nl = 0
        do k = 1, m
            definition = rd(k)
            if (nilp(definition)) cycle
            ! Don't mention suppressed commands.
            if (partial .and. suppressed_p(definition)) cycle
            kludge(1) = make_fixnum(rf(k))
            ! If this binding is shadowed by some other map, ignore it.
            this_shadowed = .false.
            shadowed_by = nil()
            if (size(shadow) > 0) then
                call shadow_lookup(shadow, kludge, shadowed_by)
                if (.not. nilp(shadowed_by) .and. .not. value_eq(shadowed_by, definition)) then
                    if (.not. mention_shadow) cycle
                    this_shadowed = .true.
                end if
            end if
            nl = nl + 1
            lines(nl)%key = key_description(kludge, prefix)
            if (rt(k) /= rf(k)) then
                kludge(1) = make_fixnum(rt(k))
                lines(nl)%key = lines(nl)%key//" .. "//key_description(kludge, prefix)
            end if
            lines(nl)%def = describe_command(definition, transl)
            lines(nl)%extra = ""
            if (this_shadowed) then
                if (symbolp(shadowed_by)) then
                    call symbol_name(shadowed_by, name, st)
                    lines(nl)%extra = "  (currently shadowed by `"//name//"')"
                else
                    lines(nl)%extra = "  (currently shadowed)"
                end if
            end if
        end do
        if (nl == 0) return
        call align_section(lines(1:nl), block)
        text = text//LF//block
    end subroutine describe_vector

    !> The indices of `from' in increasing order.
    subroutine order_by_start(from, order)
        integer(int64), intent(in) :: from(:)
        integer, allocatable, intent(out) :: order(:)
        integer :: i, j, x
        allocate (order(size(from)))
        do i = 1, size(from)
            order(i) = i
        end do
        do i = 2, size(from)
            x = order(i)
            j = i - 1
            do while (j >= 1)
                if (from(order(j)) <= from(x)) exit
                order(j + 1) = order(j)
                j = j - 1
            end do
            order(j + 1) = x
        end do
    end subroutine order_by_start

    !> The lines of one section of describe-map, the definitions aligned:
    !> to column 16, 24 or 32 if every key fits before it, else one column
    !> past the widest key if the definitions fit in the window, else each
    !> line on its own, continued on the next line when too long.  Port of
    !> help.el:describe-map--align-section and describe-map--fill-columns.
    subroutine align_section(lines, block)
        type(entry_line_type), intent(in) :: lines(:)
        character(len=:), allocatable, intent(out) :: block
        integer, allocatable :: cols(:)
        integer :: k, max_key, max_def, width, ww, fill, tabw
        logical :: tabs
        call default_settings(fill, tabw, tabs)
        allocate (cols(size(lines)))
        max_key = 0
        max_def = 0
        do k = 1, size(lines)
            cols(k) = text_columns(lines(k)%key)
            max_key = max(max_key, cols(k))
            max_def = max(max_def, len(lines(k)%def))
        end do
        block = ""
        width = 0
        if (max_key < 16) then
            width = 16
        else if (max_key < 24) then
            width = 24
        else if (max_key < 32) then
            width = 32
        else
            ww = window_body_cols()
            if (max_def + max(16, max_key) < ww) width = 1 + max(16, max_key)
        end if
        do k = 1, size(lines)
            block = block//lines(k)%key
            if (width > 0) then
                block = block//fill_string(cols(k), width, tabw)
            else if (cols(k) + len(lines(k)%def) < ww) then
                block = block//repeat(" ", 1 + max_key - cols(k))
            else
                ! Continuation.
                block = block//LF//TAB//TAB
            end if
            block = block//lines(k)%def//lines(k)%extra//LF
        end do
    end subroutine align_section

    !> The tabs and spaces describe-map--fill-columns inserts after a key
    !> ending at column `col' to reach column `width'.
    function fill_string(col, width, tabw) result(s)
        integer, intent(in) :: col, width, tabw
        character(len=:), allocatable :: s
        integer :: tabs
        tabs = width/tabw - col/tabw
        s = repeat(TAB, max(0, tabs))
        if (tabs == 0) then
            s = s//repeat(" ", max(0, width - col))
        else
            s = s//repeat(" ", mod(width, tabw))
        end if
    end function fill_string

    !> Make a string appropriate for usage as a visual separator line:
    !> a newline, which the terminal shows underlined, or, on a display
    !> that can't underline, a line of dashes as wide as the window less
    !> one.  Port of simple.el:make-separator-line (no LENGTH; the
    !> `separator_line' face is not ported).
    function make_separator_line() result(s)
        character(len=:), allocatable :: s
        logical :: underline
        underline = .false.
        if (associated(display_supports_face_attributes_p)) then
            underline = display_supports_face_attributes_p(face_attrs(underline=.true.))
        end if
        if (underline) then
            s = LF
        else
            s = repeat("-", window_body_cols() - 1)//LF
        end if
    end function make_separator_line

    !> The width of the selected window's text area (window-width).
    integer function window_body_cols() result(w)
        type(handle_type) :: sw
        w = 80
        sw = selected_window()
        if (window_live_p(sw)) w = window_text_cols(windows(sw%index))
    end function window_body_cols

    !> Make `help_map' and put it on C-h, <help> and <f1> in `global_map'; name
    !> the keymaps docstrings refer to; register the commands.
    subroutine syms_of_help(global_map)
        type(handle_type), intent(in) :: global_map
        type(status_type) :: status
        type(value_type), allocatable :: key(:)
        if (keymap_live_p(help_map)) call free_keymap(help_map, status)
        call make_sparse_keymap(help_map)
        call keymap_set(help_map, "C-h", "help_for_help", status)
        call keymap_set(help_map, "<help>", "help_for_help", status)
        call keymap_set(help_map, "<f1>", "help_for_help", status)
        call keymap_set(help_map, "?", "help_for_help", status)
        call keymap_set(help_map, "a", "apropos_command", status)
        call keymap_set(help_map, "b", "describe_bindings", status)
        call keymap_set(help_map, "c", "describe_key_briefly", status)
        call keymap_set(help_map, "d", "apropos_documentation", status)
        call keymap_set(help_map, "e", "view_echo_area_messages", status)
        call keymap_set(help_map, "f", "describe_function", status)
        call keymap_set(help_map, "u", "apropos_user_option", status)
        call keymap_set(help_map, "k", "describe_key", status)
        call keymap_set(help_map, "l", "view_lossage", status)
        call keymap_set(help_map, "m", "describe_mode", status)
        call keymap_set(help_map, "o", "describe_symbol", status)
        call keymap_set(help_map, "v", "describe_variable", status)
        call keymap_set(help_map, "w", "where_is", status)
        call keymap_set(help_map, "x", "describe_command", status)
        call keymap_set(help_map, "q", "help_quit", status)
        call key_parse("C-h", key, status)
        call define_key(global_map, key, make_keymap_ref(help_map), status)
        call key_parse("<help>", key, status)
        call define_key(global_map, key, make_keymap_ref(help_map), status)
        call key_parse("<f1>", key, status)
        call define_key(global_map, key, make_keymap_ref(help_map), status)
        call init_help_mode()
        call register_keymap_name("global_map", global_map)
        call register_keymap_name("help_map", help_map)
        call register_keymap_name("help_mode_map", help_mode_map)
        call register_keymap_name("minibuffer_local_map", minibuffer_local_map)
        commands%describe_key%run => cmd_describe_key
        commands%describe_key_briefly%run => cmd_describe_key_briefly
        commands%where_is%run => cmd_where_is
        commands%help_quit%run => help_quit
        commands%view_echo_area_messages%run => view_echo_area_messages
        commands%view_lossage%run => view_lossage
        commands%help_for_help%run => help_for_help
        commands%describe_bindings%run => describe_bindings
    end subroutine syms_of_help

    subroutine cmd_describe_key(status)
        type(status_type), intent(out) :: status
        call describe_key(status)
    end subroutine cmd_describe_key

    subroutine cmd_describe_key_briefly(status)
        type(status_type), intent(out) :: status
        call describe_key_briefly(status)
    end subroutine cmd_describe_key_briefly

    recursive subroutine cmd_where_is(status)
        type(status_type), intent(out) :: status
        call where_is(status)
    end subroutine cmd_where_is
end module help_fm
