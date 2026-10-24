! subr_read_fm.f90 -- read-number, a submodule of subr_fm.
! Ported from GNU Emacs 31.1 lisp/subr.el.
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

!> The minibuffer readers of subr.el (`read_number', `y_or_n_p',
!> `read_char_from_minibuffer', `read_char_choice'): a
!> submodule because minibuf_fm uses subr_fm.
submodule(subr_fm) subr_read_fm
    use commands_fm, only: commands
    use minibuf_fm, only: minibuffer_string_type
    use character_fm, only: string_char_and_length
    use, intrinsic :: iso_fortran_env, only: int32
    use minibuf_fm, only: read_from_minibuffer, minibuffer_local_map, enable_recursive_minibuffers, minibufferp
    use minibuffer_fm, only: delete_minibuffer_contents, exit_minibuffer, minibuffer_message
    use keyboard_fm, only: sit_for, this_command, real_this_command, abort_recursive_edit
    use keymap_fm, only: set_keymap_parent, define_key_range, free_keymap
    use data_fm, only: value_eq
    use character_fm, only: MAX_CHAR
    use lisp_fm, only: make_fixnum
    use insdel_fm, only: insert
    use dispnew_fm, only: ding
    use window_fm, only: recenter, scroll_up_command, scroll_down_command, window_configuration_type, &
                         current_window_configuration, set_window_configuration, free_window_configuration
    use character_fm, only: char_string, MAX_MULTIBYTE_LENGTH
    use keyboard_fm, only: last_command_event, last_command
    use keymap_fm, only: single_key_description
    use buffer_fm, only: get_buffer, buffer_live_p, kill_buffer
    use help_fm, only: with_output_to_temp_buffer, substitute_command_keys
    use rmc_fm, only: read_char_choice_use_read_key
    use error_fm, only: format_message, FM_GENERIC_ERROR
    implicit none(type, external)

    !> y-or-n-p-map, made by `init_y_or_n_p_map'.
    type(handle_type) :: y_or_n_p_map

    !> read-char-from-minibuffer-map, made by
    !> `init_read_char_from_minibuffer_map'.
    type(handle_type) :: read_char_from_minibuffer_map

    !> The `help_form' of the `read_char_from_minibuffer' in progress
    !> (unallocated for none).
    character(len=:), allocatable :: rcfm_help_form

    !> The character that shows the help form: `help_char''s standard value,
    !> C-h (the variable itself is not ported).
    integer, parameter :: HELP_CHAR = 8

contains

    module procedure read_number
    character(len=:), allocatable :: full, str, histvar, def1
    type(minibuffer_string_type), allocatable :: defs(:)
    integer :: k, ndefs
    logical :: have
    status = status_type()
    n = 0
    full = prompt
    histvar = "read_number_history"
    if (present(hist)) histvar = hist
    ndefs = 0
    if (present(defaults)) then
        ndefs = size(defaults)
    else if (present(default)) then
        ndefs = 1
    end if
    allocate (defs(ndefs))
    do k = 1, ndefs
        if (present(defaults)) then
            defs(k)%s = itoa(defaults(k))
        else
            defs(k)%s = itoa(default)
        end if
    end do
    if (ndefs > 0) then
        def1 = defs(1)%s
        ! (string-match "\\(\\):[ \t]*\\'" prompt): before the last colon
        ! followed only by whitespace, else in place of the trailing
        ! whitespace.
        k = len(full)
        do while (k >= 1)
            if (full(k:k) /= " " .and. full(k:k) /= achar(9)) exit
            k = k - 1
        end do
        if (k >= 1) then
            if (full(k:k) == ":") then
                full = full(1:k - 1)//" (default "//def1//")"//full(k:)
            else
                full = full(1:k)//" (default "//def1//")"
            end if
        else
            full = " (default "//def1//")"
        end if
    end if
    do
        call read_from_minibuffer(full, str, status, history=histvar, defaults=defs)
        if (.not. status%ok()) return
        if (len(str) == 0) then
            have = ndefs > 0
            if (have) then
                if (present(defaults)) then
                    n = defaults(1)
                else
                    n = default
                end if
            end if
        else
            call read_integer(str, n, have)
        end if
        if (have) exit
        call message("Please enter a number.")
        call sit_for(1000)
    end do
contains

    function itoa(v) result(s)
        integer(int64), intent(in) :: v
        character(len=:), allocatable :: s
        character(len=24) :: buf
        write (buf, "(i0)") v
        s = trim(buf)
    end function itoa
    end procedure read_number

    !> The integer the Lisp reader reads first from STR, `ok' false if it
    !> reads something else (or nothing, or signals).  The syntax of
    !> lread.c:read0 for integers: decimal with an optional sign and
    !> trailing dot, #x, #o, #b and #RADIXr prefixes, and ?C characters.
    !> Floats are not read (the result is an integer): Deferred.
    subroutine read_integer(str, n, ok)
        character(len=*), intent(in) :: str
        integer(int64), intent(out) :: n
        logical, intent(out) :: ok
        character(len=*), parameter :: DELIMS = " "//achar(9)//achar(10)//achar(12)//achar(13)//"()[]""';`,"
        integer :: i, j, radix
        integer(int32) :: c, clen
        n = 0
        ok = .false.
        i = 1
        ! Skip whitespace and comments.
        do while (i <= len(str))
            if (str(i:i) == ";") then
                j = index(str(i:), achar(10))
                if (j == 0) return
                i = i + j
            else if (index(" "//achar(9)//achar(10)//achar(12)//achar(13), str(i:i)) > 0) then
                i = i + 1
            else
                exit
            end if
        end do
        if (i > len(str)) return
        if (str(i:i) == "?") then
            ! A character.
            if (i + 1 > len(str)) return
            if (str(i + 1:i + 1) == "\") then
                if (i + 2 > len(str)) return
                select case (str(i + 2:i + 2))
                case ("a")
                    c = 7
                case ("b")
                    c = 8
                case ("t")
                    c = 9
                case ("n")
                    c = 10
                case ("v")
                    c = 11
                case ("f")
                    c = 12
                case ("r")
                    c = 13
                case ("e")
                    c = 27
                case ("s")
                    c = 32
                case ("d")
                    c = 127
                case default
                    call string_char_and_length(str, int(i + 2, int64), c, clen)
                    j = i + 2 + clen
                    if (str(i + 2:i + 2) == "C" .or. str(i + 2:i + 2) == "M" .or. str(i + 2:i + 2) == "S" .or. &
                        str(i + 2:i + 2) == "^" .or. str(i + 2:i + 2) == "x" .or. str(i + 2:i + 2) == "u" .or. &
                        (str(i + 2:i + 2) >= "0" .and. str(i + 2:i + 2) <= "7")) then
                        ! Modifier, hex, Unicode and octal escapes are not
                        ! ported (Deferred).
                        return
                    end if
                    call finish(j, int(c, int64))
                    return
                end select
                call finish(i + 3, int(c, int64))
            else
                call string_char_and_length(str, int(i + 1, int64), c, clen)
                call finish(i + 1 + clen, int(c, int64))
            end if
            return
        end if
        ! The token: up to a delimiter.
        j = i
        do while (j <= len(str))
            if (index(DELIMS, str(j:j)) > 0) exit
            j = j + 1
        end do
        if (str(i:i) == "#") then
            if (j - i < 3) return
            select case (str(i + 1:i + 1))
            case ("x", "X")
                radix = 16
                i = i + 2
            case ("o", "O")
                radix = 8
                i = i + 2
            case ("b", "B")
                radix = 2
                i = i + 2
            case default
                ! #RADIXrINTEGER
                radix = 0
                i = i + 1
                do while (i < j)
                    if (str(i:i) < "0" .or. str(i:i) > "9") exit
                    radix = radix*10 + (iachar(str(i:i)) - 48)
                    if (radix > 36) return
                    i = i + 1
                end do
                if (i >= j .or. radix < 2) return
                if (str(i:i) /= "r" .and. str(i:i) /= "R") return
                i = i + 1
            end select
            call parse_digits(str(i:j - 1), radix, .false., n, ok)
            return
        end if
        call parse_digits(str(i:j - 1), 10, .true., n, ok)
    contains

        ! A character read ends at a delimiter.
        subroutine finish(next, v)
            integer, intent(in) :: next
            integer(int64), intent(in) :: v
            if (next <= len(str)) then
                if (index(DELIMS, str(next:next)) == 0) return
            end if
            n = v
            ok = .true.
        end subroutine finish
    end subroutine read_integer

    !> TOKEN as an integer in RADIX with an optional sign (and, if
    !> `trailing_dot', an optional trailing "."): lread.c:string_to_number.
    subroutine parse_digits(token, radix, trailing_dot, n, ok)
        character(len=*), intent(in) :: token
        integer, intent(in) :: radix
        logical, intent(in) :: trailing_dot
        integer(int64), intent(out) :: n
        logical, intent(out) :: ok
        integer :: i, last, d
        logical :: negative
        n = 0
        ok = .false.
        last = len(token)
        if (trailing_dot .and. last >= 2) then
            if (token(last:last) == ".") last = last - 1
        end if
        i = 1
        negative = .false.
        if (last >= 1) then
            if (token(1:1) == "+" .or. token(1:1) == "-") then
                negative = token(1:1) == "-"
                i = 2
            end if
        end if
        if (i > last) return
        do while (i <= last)
            select case (token(i:i))
            case ("0":"9")
                d = iachar(token(i:i)) - 48
            case ("a":"z")
                d = iachar(token(i:i)) - 87
            case ("A":"Z")
                d = iachar(token(i:i)) - 55
            case default
                return
            end select
            if (d >= radix) return
            n = n*radix + d
            i = i + 1
        end do
        if (negative) n = -n
        ok = .true.
    end subroutine parse_digits
    module procedure y_or_n_p
    character(len=:), allocatable :: padded, str
    type(value_type) :: key, saved_this, saved_real_this
    type(status_type) :: st
    integer :: def
    logical :: saved_enable, again
    answer = .false.
    status = status_type()
    padded = prompt
    if (len(prompt) > 0) then
        if (prompt(len(prompt):len(prompt)) /= " ") padded = padded//" "
    end if
    padded = padded//"(y or n) "
    if (y_or_n_p_use_read_key) then
        def = ANSWER_RECENTER
        do
            if (def == ANSWER_RECENTER .or. def == ANSWER_SCROLL_UP .or. def == ANSWER_SCROLL_DOWN .or. &
                def == ANSWER_SCROLL_OTHER_WINDOW .or. def == ANSWER_SCROLL_OTHER_WINDOW_DOWN) then
                call read_key(padded, key, status)
            else
                call read_key("Please answer y or n.  "//padded, key, status)
            end if
            if (.not. status%ok()) return
            def = query_replace_answer(key)
            again = .true.
            select case (def)
            case (ANSWER_SKIP, ANSWER_ACT)
                again = .false.
            case (ANSWER_RECENTER)
                call recenter(nil(), .false., st)
            case (ANSWER_SCROLL_UP)
                call scroll_up_command(nil(), st)
            case (ANSWER_SCROLL_DOWN)
                call scroll_down_command(nil(), st)
            case (ANSWER_SCROLL_OTHER_WINDOW, ANSWER_SCROLL_OTHER_WINDOW_DOWN)
                ! scroll-other-window and scroll-other-window-down are not
                ! ported (Deferred).
                continue
            case (ANSWER_EXIT_PREFIX, ANSWER_QUIT)
                call set_error(status, FM_QUIT)
                return
            case default
                if (fixnump(key)) then
                    if (xfixnum(key) == 27) then
                        call set_error(status, FM_QUIT)
                        return
                    end if
                end if
            end select
            if (.not. again) exit
            call ding()
            call discard_input()
        end do
        answer = def == ANSWER_ACT
    else
        call init_y_or_n_p_map()
        saved_enable = enable_recursive_minibuffers
        enable_recursive_minibuffers = .true.
        ! Protect this-command, and preserve the actual command that
        ! eventually called `y_or_n_p'.
        saved_this = this_command
        saved_real_this = real_this_command
        if (allocated(y_or_n_p_history_variable)) then
            call read_from_minibuffer(padded, str, status, keymap=y_or_n_p_map, history=y_or_n_p_history_variable)
        else
            call read_from_minibuffer(padded, str, status, keymap=y_or_n_p_map, history="t")
        end if
        enable_recursive_minibuffers = saved_enable
        this_command = saved_this
        real_this_command = saved_real_this
        if (.not. status%ok()) return
        answer = str == "y" .or. str == "Y"
    end if
    if (answer) then
        call message(padded//"y")
    else
        call message(padded//"n")
    end if
    end procedure y_or_n_p

    !> Make `y_or_n_p_map': y-or-n-p-map composed with `query_replace_map'
    !> and its command remappings, flattened, as keymap_fm has neither
    !> composed keymaps nor command remapping: each key of
    !> query-replace-map that minibuffer-local-map (its parent) doesn't
    !> bind gets the command its answer is remapped to (or the answer
    !> itself when it isn't), and keys that would run
    !> `self_insert_command' run `y_or_n_p_insert_other'.  Port of
    !> subr.el y-or-n-p-map (minibuffer-scroll-other-window and
    !> minibuffer-scroll-other-window-down are not ported: Deferred).
    subroutine init_y_or_n_p_map()
        type(value_type) :: key1(1)
        type(value_type), allocatable :: seq(:)
        type(value_type) :: b
        type(status_type) :: st
        integer :: k
        integer(int64) :: c
        if (keymap_live_p(y_or_n_p_map)) return
        call init_query_replace_map()
        call make_sparse_keymap(y_or_n_p_map)
        call set_keymap_parent(y_or_n_p_map, minibuffer_local_map, st)
        do k = 1, size(QRM_KEYS)
            call key_parse(trim(QRM_KEYS(k)), seq, st)
            if (.not. st%ok()) cycle
            call lookup_key(make_keymap_ref(minibuffer_local_map), seq, b, st)
            if (.not. nilp(b)) cycle
            call define_key(y_or_n_p_map, seq, sym(remapped(trim(QRM_ANSWERS(k)))), st)
        end do
        ! [remap self-insert-command]
        do c = 0, 127
            key1(1) = make_fixnum(c)
            call lookup_key(make_keymap_ref(minibuffer_local_map), key1, b, st)
            if (nilp(b)) then
                call lookup_key(make_keymap_ref(query_replace_map), key1, b, st)
                if (.not. nilp(b)) cycle
                call lookup_key(make_keymap_ref(current_global_map()), key1, b, st)
            end if
            if (value_eq(b, sym("self_insert_command"))) then
                call define_key(y_or_n_p_map, key1, sym("y_or_n_p_insert_other"), st)
            end if
        end do
        key1(1) = make_fixnum(128_int64)
        call lookup_key(make_keymap_ref(current_global_map()), key1, b, st)
        if (value_eq(b, sym("self_insert_command"))) then
            call define_key_range(y_or_n_p_map, 128_int64, int(MAX_CHAR, int64), sym("y_or_n_p_insert_other"), st)
        end if
    contains

        function remapped(answer) result(cmd)
            character(len=*), intent(in) :: answer
            character(len=:), allocatable :: cmd
            select case (answer)
            case ("act", "act_and_show", "act_and_exit", "automatic")
                cmd = "y_or_n_p_insert_y"
            case ("skip")
                cmd = "y_or_n_p_insert_n"
            case ("backup", "undo", "undo_all", "edit", "edit_replacement", "delete_and_edit", "exit")
                cmd = "y_or_n_p_insert_other"
            case ("recenter")
                cmd = "minibuffer_recenter_top_bottom"
            case ("scroll_up")
                cmd = "minibuffer_scroll_up_command"
            case ("scroll_down")
                cmd = "minibuffer_scroll_down_command"
            case ("exit_prefix", "quit")
                cmd = "abort_recursive_edit"
            case default
                cmd = answer
            end select
        end function remapped
    end subroutine init_y_or_n_p_map

    !> Insert the answer "y" and exit the minibuffer of `y_or_n_p'.
    !> Discard all previous input before inserting and exiting the minibuffer.
    !> Port of subr.el:y-or-n-p-insert-y.
    subroutine y_or_n_p_insert_y(status)
        type(status_type), intent(out) :: status
        call insert_answer("y", status)
    end subroutine y_or_n_p_insert_y

    !> Insert the answer "n" and exit the minibuffer of `y_or_n_p'.
    !> Discard all previous input before inserting and exiting the minibuffer.
    !> Port of subr.el:y-or-n-p-insert-n.
    subroutine y_or_n_p_insert_n(status)
        type(status_type), intent(out) :: status
        call insert_answer("n", status)
    end subroutine y_or_n_p_insert_n

    subroutine insert_answer(text, status)
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. minibufferp()) return
        call delete_minibuffer_contents(status)
        if (.not. status%ok()) return
        call insert(text, status)
        if (.not. status%ok()) return
        call exit_minibuffer(status)
    end subroutine insert_answer

    !> Handle inserting of other answers in the minibuffer of `y_or_n_p'.
    !> Display an error on trying to insert a disallowed character.
    !> Also discard all previous input in the minibuffer.
    !> Port of subr.el:y-or-n-p-insert-other.
    subroutine y_or_n_p_insert_other(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. minibufferp()) return
        call delete_minibuffer_contents(status)
        if (.not. status%ok()) return
        call ding()
        call discard_input()
        call minibuffer_message("Please answer y or n")
        call sit_for(2000)
    end subroutine y_or_n_p_insert_other

    !> Abort the command that requested this recursive edit or minibuffer input.
    !> Port of keyboard.c:abort-recursive-edit.
    subroutine cmd_abort_recursive_edit(status)
        type(status_type), intent(out) :: status
        call abort_recursive_edit(status)
    end subroutine cmd_abort_recursive_edit

    module procedure syms_of_subr
    type(status_type) :: st
    commands%undefined%run => undefined
    commands%y_or_n_p_insert_y%run => y_or_n_p_insert_y
    commands%y_or_n_p_insert_n%run => y_or_n_p_insert_n
    commands%y_or_n_p_insert_other%run => y_or_n_p_insert_other
    commands%abort_recursive_edit%run => cmd_abort_recursive_edit
    ! The map is made again on first use (its parent and the global map
    ! may have been rebuilt).
    if (keymap_live_p(y_or_n_p_map)) call free_keymap(y_or_n_p_map, st)
    commands%read_char_from_minibuffer_insert_char%run => read_char_from_minibuffer_insert_char
    commands%read_char_from_minibuffer_insert_other%run => read_char_from_minibuffer_insert_other
    commands%help_form_show%run => help_form_show
    if (keymap_live_p(read_char_from_minibuffer_map)) call free_keymap(read_char_from_minibuffer_map, st)
    end procedure syms_of_subr

    !> Make `read_char_from_minibuffer_map': minibuffer-local-map's keys,
    !> with the minibuffer's own recentering and scrolling commands, and
    !> RET rejected.  Port of subr.el read-char-from-minibuffer-map
    !> (minibuffer-scroll-other-window and -down are not ported).
    subroutine init_read_char_from_minibuffer_map()
        type(status_type) :: st
        if (keymap_live_p(read_char_from_minibuffer_map)) return
        call make_sparse_keymap(read_char_from_minibuffer_map)
        call set_keymap_parent(read_char_from_minibuffer_map, minibuffer_local_map, st)
        call define_remap(read_char_from_minibuffer_map, "exit_minibuffer", "read_char_from_minibuffer_insert_other")
        call define_remap(read_char_from_minibuffer_map, "recenter_top_bottom", "minibuffer_recenter_top_bottom")
        call define_remap(read_char_from_minibuffer_map, "scroll_up_command", "minibuffer_scroll_up_command")
        call define_remap(read_char_from_minibuffer_map, "scroll_down_command", "minibuffer_scroll_down_command")
    end subroutine init_read_char_from_minibuffer_map

    !> (define-key MAP [remap FROM] TO).
    subroutine define_remap(map, from, to)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: from, to
        type(value_type) :: events(2)
        type(status_type) :: st
        events(1) = sym("remap")
        events(2) = sym(from)
        call define_key(map, events, sym(to), st)
    end subroutine define_remap

    module procedure read_char_from_minibuffer
    type(handle_type) :: map
    type(value_type) :: key1(1), saved_this
    type(status_type) :: st
    character(len=:), allocatable :: result, saved_help
    character(len=MAX_MULTIBYTE_LENGTH) :: buf
    integer(int32) :: ch, clen, nbytes
    integer :: k
    logical :: had_help
    c = 13
    call init_read_char_from_minibuffer_map()
    had_help = allocated(rcfm_help_form)
    if (had_help) saved_help = rcfm_help_form
    if (present(help_form)) rcfm_help_form = help_form
    if (present(chars)) then
        call make_sparse_keymap(map)
        call set_keymap_parent(map, read_char_from_minibuffer_map, st)
        ! If we have a `help_form' here, then the `C-h' (i.e.,
        ! `help_char') character should output that instead of being a
        ! command char.
        if (present(help_form)) then
            key1(1) = make_fixnum(int(HELP_CHAR, int64))
            call define_key(map, key1, sym("help_form_show"), st)
        end if
        do k = 1, size(chars)
            key1(1) = make_fixnum(int(chars(k), int64))
            call define_key(map, key1, sym("read_char_from_minibuffer_insert_char"), st)
        end do
        call define_remap(map, "self_insert_command", "read_char_from_minibuffer_insert_other")
    else
        map = read_char_from_minibuffer_map
    end if
    ! Protect this-command when called from pre-command-hook (bug#45029)
    saved_this = this_command
    if (present(history)) then
        call read_from_minibuffer(prompt, result, status, keymap=map, history=history)
    else
        call read_from_minibuffer(prompt, result, status, keymap=map, history="t")
    end if
    this_command = saved_this
    if (present(chars)) call free_keymap(map, st)
    if (had_help) then
        rcfm_help_form = saved_help
    else if (allocated(rcfm_help_form)) then
        deallocate (rcfm_help_form)
    end if
    if (.not. status%ok()) return
    if (len(result) > 0) then
        ! We have a string (with one character), so return the first one.
        call string_char_and_length(result, 1_int64, ch, clen)
        c = ch
    else
        ! The default value is RET.  (Pushing "\r" on HISTORY is not
        ! ported: a history is a minibuffer variable here.)
        c = 13
    end if
    ! Display the question with the answer.
    call char_string(int(c, int32), buf, nbytes, st)
    call message(prompt//buf(1:nbytes))
    end procedure read_char_from_minibuffer

    !> Insert the character you type into the minibuffer and exit minibuffer.
    !> Discard all previous input before inserting and exiting the minibuffer.
    !> Port of subr.el:read-char-from-minibuffer-insert-char.
    subroutine read_char_from_minibuffer_insert_char(status)
        type(status_type), intent(out) :: status
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        type(status_type) :: st
        integer(int32) :: nbytes
        status = status_type()
        if (.not. minibufferp()) return
        call delete_minibuffer_contents(status)
        if (.not. status%ok()) return
        if (fixnump(last_command_event)) then
            call char_string(int(xfixnum(last_command_event), int32), buf, nbytes, st)
            if (st%ok()) call insert(buf(1:nbytes), status)
            if (.not. status%ok()) return
        end if
        call exit_minibuffer(status)
    end subroutine read_char_from_minibuffer_insert_char

    !> Reject a disallowed character typed into the minibuffer.
    !> This command is intended to be bound to keys that users are not
    !> allowed to type into the minibuffer.  When the user types any
    !> such key, this command discard all minibuffer input and displays
    !> an error message.  Port of
    !> subr.el:read-char-from-minibuffer-insert-other.
    subroutine read_char_from_minibuffer_insert_other(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. minibufferp()) return
        call delete_minibuffer_contents(status)
        if (.not. status%ok()) return
        call ding()
        call discard_input()
        call minibuffer_message("Wrong answer")
        call sit_for(2000)
    end subroutine read_char_from_minibuffer_insert_other

    !> Display the output of a `help_form' (the text bound by the
    !> caller).  Port of help.el:help-form-show.
    subroutine help_form_show(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. allocated(rcfm_help_form)) return
        call with_output_to_temp_buffer(" *Char Help*", rcfm_help_form, status)
    end subroutine help_form_show

    module procedure read_char_choice
    type(value_type) :: saved_last
    if (.not. read_char_choice_use_read_key) then
        ! We are about to enter recursive-edit, which sets
        ! `last_command'.  If the callers of this function have some
        ! logic based on `last_command''s value (example: `kill_region'),
        ! that could interfere with their logic.  So we let-bind
        ! `last_command' here to prevent that.
        saved_last = last_command
        call read_char_from_minibuffer(prompt, c, status, chars=chars, help_form=help_form)
        last_command = saved_last
    else
        call read_char_choice_with_read_key(prompt, chars, c, status, inhibit_keyboard_quit, help_form)
    end if
    end procedure read_char_choice

    !> Read and return one of the characters in CHARS, prompting with PROMPT.
    !> CHARS should be a list of single characters.
    !> Any input that is not one of CHARS is ignored.
    !>
    !> If optional argument INHIBIT_KEYBOARD_QUIT is true, ignore
    !> `keyboard_quit' events while waiting for valid input.
    !>
    !> If HELP_FORM is present, then pressing `help_char' displays it.
    !> Port of subr.el:read-char-choice-with-read-key (the prompt is not
    !> given the `minibuffer_prompt' face; C-g is read as a character, not
    !> as a quit).
    recursive subroutine read_char_choice_with_read_key(prompt, chars, c, status, inhibit_keyboard_quit, help_form)
        character(len=*), intent(in) :: prompt
        integer, intent(in) :: chars(:)
        integer, intent(out) :: c
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: inhibit_keyboard_quit
        character(len=*), intent(in), optional :: help_form
        type(window_configuration_type) :: config
        type(value_type) :: key
        type(handle_type) :: helpbuf
        character(len=:), allocatable :: please
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        type(status_type) :: st
        integer(int32) :: nbytes
        logical :: done, show_help, esc_flag, inhibit, saved_cursor
        integer :: k
        status = status_type()
        c = 0
        inhibit = .false.
        if (present(inhibit_keyboard_quit)) inhibit = inhibit_keyboard_quit
        if (size(chars) == 0) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Called `read_char_choice' without valid char choices"))
            return
        end if
        done = .false.
        show_help = .false.
        esc_flag = .false.
        saved_cursor = cursor_in_echo_area
        cursor_in_echo_area = .true.
        ! in case we call help-form-show
        call current_window_configuration(config)
        do while (.not. done)
            call read_key(prompt, key, status)
            if (.not. status%ok()) exit
            if (show_help) then
                helpbuf = get_buffer(" *Char Help*")
                if (buffer_live_p(helpbuf)) call kill_buffer(helpbuf, st)
            end if
            if (.not. fixnump(key)) cycle
            c = int(xfixnum(key))
            ! If caller has set help-form, that's enough.
            ! They don't explicitly have to add help-char to chars.
            if (present(help_form) .and. c == HELP_CHAR) then
                show_help = .true.
                call with_output_to_temp_buffer(" *Char Help*", help_form, st)
                cycle
            end if
            if (any(chars == c)) then
                done = .true.
            else if (.not. inhibit) then
                if (.not. esc_flag .and. c == 27) then
                    esc_flag = .true.
                else if (c == 7 .or. c == 27) then
                    call set_error(status, FM_QUIT)
                    exit
                end if
            else
                call ding()
                please = ""
                do k = 1, size(chars)
                    if (k > 1) please = please//", "
                    please = please//"\`"//single_key_description(make_fixnum(int(chars(k), int64)))//"'"
                end do
                call message("Please type "//substitute_command_keys(please))
                call sit_for(3000)
            end if
        end do
        call set_window_configuration(config)
        call free_window_configuration(config)
        cursor_in_echo_area = saved_cursor
        if (.not. status%ok()) return
        ! Display the question with the answer.  But without
        ! cursor-in-echo-area.
        call char_string(int(c, int32), buf, nbytes, st)
        call message(prompt//buf(1:nbytes))
    end subroutine read_char_choice_with_read_key
end submodule subr_read_fm
