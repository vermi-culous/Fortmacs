! help_fns_fm.f90 -- C-h f, C-h v, C-h o, C-h x and C-h m.
! Ported from GNU Emacs 31.1 lisp/help-fns.el.
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

!> `describe_function', `describe_variable', `describe_symbol' (C-h o),
!> `describe_command' (C-h x) and `describe_mode' (C-h m), from the
!> documentation docstrings_fm collects.  The functions are the commands
!> and the procedures of the config API (module `fortmacs'), the
!> variables its settings.  A function's report names the Fortran file
!> defining it and shows how Fortran calls it where upstream shows its
!> argument list; a variable's shows its Fortran name and its value as
!> Fortran (.true./.false., integers, string literals, named constants,
!> procedure names or null(), "unallocated"), then whether it becomes
!> buffer-local when set and whether it is a user option ("You can
!> customize this variable.").
!>
!> Deferred: the default from the symbol at point, cross-reference
!> buttons, "Probably introduced at" lines, obsolescence, aliases,
!> buffer-local values and the other help-fns-describe-variable-functions
!> (safe and risky local variables, file-local values, watchpoints), the
!> values of hooks, keymaps, syntax tables and other derived-type
!> variables (the value line is left out), faces and types in
!> `describe_symbol', `describe_mode''s BUFFER and Outline minor mode
!> (`describe_mode_outline': the layout is that of its default, true),
!> and the Customize interface the customize line refers to.
module help_fns_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, format_message, FM_USER_ERROR
    use lisp_fm, only: value_type, nilp, make_string, make_list, list_elements, V_LIST, vtag, list_elt, fixnump, &
                       xfixnum, symbolp
    use handle_fm, only: handle_type
    use character_fm, only: CHAR_ALT, CHAR_SUPER, CHAR_HYPER, CHAR_SHIFT, CHAR_CTL, CHAR_META
    use buffer_fm, only: buffers, cur, current_buffer, get_buffer_create, set_buffer, kill_buffer, &
                         generate_new_buffer_name
    use editfns_fm, only: point_min, point_max, buffer_string
    use insdel_fm, only: insert
    use fill_fm, only: fill_region_as_paragraph
    use lread_fm, only: sym
    use data_fm, only: symbol_name
    use callint_fm, only: command_defined_p, fboundp
    use keymap_fm, only: key_description, where_is_internal, parse_modifiers
    use minibuf_fm, only: completion_obarray_table, completion_list_table
    use minibuffer_fm, only: completing_read, REQUIRE_MATCH_T
    use docstrings_fm, only: doc_entry_type, function_doc, variable_doc, function_names, variable_names
    use help_fm, only: substitute_command_keys, with_help_window, describe_function_1_hook, documentation_hook, &
                       make_separator_line
    use data_fm, only: automatically_local_p
    use easy_mmode_fm, only: minor_mode_type, minor_mode_count, minor_mode_at
    use casefiddle_fm, only: capitalize_string
    use callint_fm, only: commandp
    implicit none(type, external)
    private

    public :: describe_function, describe_function_1, describe_variable, describe_variable_text, &
              syms_of_help_fns, describe_mode, describe_mode_text, describe_symbol, describe_symbol_text, &
              function_known_p, variable_known_p

    character(len=*), parameter :: LF = achar(10)

    !> The modifier bits of a character event, and shift.
    integer(int64), parameter :: M_SHIFT = int(CHAR_SHIFT, int64), &
                                 M_MODS = int(CHAR_ALT, int64) + int(CHAR_SUPER, int64) + int(CHAR_HYPER, int64) + &
                                 int(CHAR_SHIFT, int64) + int(CHAR_CTL, int64) + int(CHAR_META, int64)

    !> Left and right single quotes.
    character(len=*), parameter :: LQUOTE = char(226)//char(128)//char(152), &
                                   RQUOTE = char(226)//char(128)//char(153)

contains

    !> Whether `name' is a function help knows: a documented one or a
    !> command.
    logical function function_known_p(name)
        character(len=*), intent(in) :: name
        integer :: k
        function_known_p = .true.
        do k = 1, size(function_names)
            if (trim(function_names(k)) == name) return
        end do
        function_known_p = fboundp(sym(name))
    end function function_known_p

    !> Completion predicate for `describe_function'.
    logical function function_symbol_p(elt)
        type(value_type), intent(in) :: elt
        character(len=:), allocatable :: name
        type(status_type) :: status
        call symbol_name(elt, name, status)
        function_symbol_p = status%ok()
        if (function_symbol_p) function_symbol_p = function_known_p(name)
    end function function_symbol_p

    !> Whether event `ev' has no modifiers but shift: its
    !> `event_modifiers' are nil or (shift).
    logical function unmodified_event_p(ev)
        type(value_type), intent(in) :: ev
        type(value_type) :: head
        character(len=:), allocatable :: base
        integer(int64) :: bits, c
        head = ev
        ! A list (a character interval here): its car.
        if (vtag(ev) == V_LIST) head = list_elt(ev, 1_int64)
        unmodified_event_p = .false.
        if (fixnump(head)) then
            ! subr.el:event-modifiers for a character.
            bits = iand(xfixnum(head), M_MODS)
            c = iand(xfixnum(head), not(M_MODS))
            if (iand(bits, not(M_SHIFT)) /= 0) return
            if (c < 32) return
            unmodified_event_p = .true.
        else if (symbolp(head)) then
            call parse_modifiers(head, bits, base)
            unmodified_event_p = bits == 0 .or. bits == M_SHIFT
        end if
    end function unmodified_event_p

    !> The key sequences in `keys' joined as "A, B and C".  Port of
    !> help-fns.el:help-fns--insert-bindings.
    function bindings_text(keys) result(s)
        type(value_type), intent(in) :: keys(:)
        character(len=:), allocatable :: s
        type(value_type), allocatable :: events(:)
        integer :: k
        s = ""
        do k = 1, size(keys)
            if (k == 1) then
                s = ""
            else if (k == size(keys)) then
                s = s//" and "
            else
                s = s//", "
            end if
            call list_elements(keys(k), events)
            s = s//key_description(events)
        end do
    end function bindings_text

    !> What help-fns--key-bindings prints for command `name', before it is
    !> filled: its keys, "many ordinary text characters" standing for
    !> ten or more unmodified ones.  Port of the text of
    !> help-fns.el:help-fns--key-bindings (no remapping or menus).
    function key_bindings_text(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        type(value_type) :: all
        type(value_type), allocatable :: keys(:), events(:), rest(:)
        logical, allocatable :: unmodified(:)
        integer :: k, n, nunmod
        s = ""
        call where_is_internal(sym(name), all)
        call list_elements(all, keys)
        if (name == "self_insert_command" .and. size(keys) > 0) then
            call list_elements(keys(1), events)
            if (size(events) > 0) then
                if (vtag(events(1)) == V_LIST) then
                    s = "It is bound to many ordinary text characters."//LF
                    return
                end if
            end if
        end if
        ! Which non-control non-meta keys run this command?
        allocate (unmodified(size(keys)))
        do k = 1, size(keys)
            call list_elements(keys(k), events)
            unmodified(k) = .false.
            if (size(events) > 0) unmodified(k) = unmodified_event_p(events(1))
        end do
        nunmod = count(unmodified)
        if (size(keys) > 0) then
            s = "It is bound to "
            ! If lots of ordinary text characters run this command, don't
            ! mention them one by one.
            if (nunmod < 10) then
                s = s//bindings_text(keys)
            else
                allocate (rest(size(keys) - nunmod))
                n = 0
                do k = 1, size(keys)
                    if (unmodified(k)) cycle
                    n = n + 1
                    rest(n) = keys(k)
                end do
                if (n > 0) then
                    s = s//bindings_text(rest)//", and many ordinary text characters"
                else
                    s = s//"many ordinary text characters."
                end if
            end if
        end if
        if (size(keys) > 0 .or. nunmod > 0) s = s//"."//LF
    end function key_bindings_text

    !> `text' filled as a paragraph (help-fns.el fills its lines in the
    !> help buffer with fill-region-as-paragraph); `nosqueeze' keeps its
    !> spaces.
    function fill_text(text, nosqueeze) result(filled)
        character(len=*), intent(in) :: text
        logical, intent(in) :: nosqueeze
        character(len=:), allocatable :: filled
        type(handle_type) :: previous, temp
        type(status_type) :: st
        filled = text
        if (len(text) == 0) return
        previous = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), temp, st)
        if (.not. st%ok()) return
        call set_buffer(temp, st)
        buffers(cur)%undo_enabled = .false.
        call insert(text, st)
        call fill_region_as_paragraph(point_min(), point_max(), st, nosqueeze=nosqueeze)
        filled = buffer_string()
        call set_buffer(previous, st)
        call kill_buffer(temp, st)
    end function fill_text

    !> What describe-function says about function `name' after `lead'
    !> ("NAME is "): its kind and file, its keys, how Fortran calls it and
    !> its documentation; `text' starts with `lead'.  Port of
    !> help-fns.el:describe-function-1 (with
    !> help-fns-function-description-header, help-fns--key-bindings and
    !> help-fns--signature).
    subroutine describe_function_1(name, text, lead)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: text
        character(len=*), intent(in), optional :: lead
        type(doc_entry_type) :: entry
        character(len=:), allocatable :: keys, doc
        logical :: found, interactive, subroutine_p
        call function_doc(name, entry, found)
        interactive = command_defined_p(sym(name))
        if (found) interactive = interactive .or. entry%interactive
        subroutine_p = .true.
        if (found) subroutine_p = len(entry%fortran) == 0 .or. index(entry%fortran, "call ") == 1
        text = ""
        if (present(lead)) text = lead
        if (interactive) then
            text = text//"an interactive "
        else
            text = text//"a "
        end if
        if (subroutine_p) then
            text = text//"Fortran subroutine"
        else
            text = text//"Fortran function"
        end if
        if (found) text = text//" in "//LQUOTE//entry%file//RQUOTE
        text = fill_text(text//".", .true.)//LF//LF
        if (interactive) then
            keys = fill_text(key_bindings_text(name), .false.)
            if (len(keys) > 0) then
                if (keys(len(keys):) /= LF) keys = keys//LF
                text = text//keys//LF
            end if
        end if
        if (found) then
            if (len(entry%fortran) > 0) text = text//entry%fortran//LF//LF
            doc = entry%doc
        else
            doc = ""
        end if
        if (len(doc) == 0) doc = "Not documented."
        text = text//substitute_command_keys(doc)//LF
    end subroutine describe_function_1

    !> Display the full documentation of FUNCTION (a name).  Port of
    !> help-fns.el:describe-function (FUNCTION is a name).
    subroutine describe_function(function, status)
        character(len=*), intent(in) :: function
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        call describe_function_1(function, text, function//" is ")
        call with_help_window(text, status)
    end subroutine describe_function

    !> The report describe-variable shows for variable `name'; `found'
    !> false if help doesn't know it.  Port of the text help-fns.el:
    !> describe-variable inserts.
    subroutine describe_variable_text(name, text, found)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found
        type(doc_entry_type) :: entry
        character(len=:), allocatable :: doc, extra
        call variable_doc(name, entry, found)
        if (.not. found) then
            text = name//" is void as a variable."//LF
            return
        end if
        text = name//" is a variable defined in "//LQUOTE//entry%file//RQUOTE//"."//LF//LF
        ! No value line for a value without a printed form (unallocated
        ! here, or empty for a procedure without a name).
        if (allocated(entry%value)) then
            if (len(entry%value) > 0) then
                ! A value too long for the line starts on the next.
                if (len(entry%value) + len("Its value is") < 68) then
                    text = text//"Its value is "//entry%value//LF
                else
                    text = text//"Its value is"//LF//entry%value//LF
                end if
                if (len(entry%original) > 0 .and. entry%original /= entry%value) then
                    ! A short value stays on the line.
                    if (len(entry%original) < 20) then
                        text = text//"Original value was "//entry%original//LF
                    else
                        text = text//"Original value was "//LF//entry%original//LF
                    end if
                end if
            end if
        end if
        text = text//"Its Fortran name is "//LQUOTE//entry%fortran//RQUOTE//"."//LF//LF
        doc = entry%doc
        if (len(doc) == 0) doc = "Not documented as a variable."
        text = text//substitute_command_keys(doc)//LF
        ! The indented administrative bits
        ! (help-fns-describe-variable-functions), after an empty line.
        extra = ""
        ! help-fns--var-bufferlocal
        if (automatically_local_p(sym(name))) extra = extra//"  Automatically becomes buffer-local when set."//LF
        ! help-fns--customize-variable
        if (entry%user_option) extra = extra//"  You can customize this variable."//LF
        if (len(extra) > 0) text = text//LF//extra
    end subroutine describe_variable_text

    !> Display the full documentation of VARIABLE (a name).  Port of
    !> help-fns.el:describe-variable (VARIABLE is a name; global values
    !> only).
    subroutine describe_variable(variable, status)
        character(len=*), intent(in) :: variable
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        logical :: found
        call describe_variable_text(variable, text, found)
        call with_help_window(text, status)
    end subroutine describe_variable

    !> Display the full documentation of FUNCTION (a name).
    !> Port of help-fns.el:describe-function (the command; no default).
    recursive subroutine cmd_describe_function(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        call completing_read("Describe function: ", completion_obarray_table(), name, status, &
                             function_symbol_p, require_match=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        ! help-fns--describe-function-or-command-prompt (no function at
        ! point yet)
        if (len(name) == 0) then
            call set_error(status, FM_USER_ERROR, format_message("You didn't specify a function symbol"))
            return
        end if
        call describe_function(name, status)
    end subroutine cmd_describe_function

    !> Display the full documentation of VARIABLE (a name).
    !> Port of help-fns.el:describe-variable (the command; no default).
    recursive subroutine cmd_describe_variable(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(value_type), allocatable :: names(:)
        integer :: k
        allocate (names(size(variable_names)))
        do k = 1, size(variable_names)
            names(k) = make_string(trim(variable_names(k)))
        end do
        call completing_read("Describe variable: ", completion_list_table(make_list(names)), name, status, &
                             require_match=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        ! (No variable at point yet.)
        if (len(name) == 0) then
            call set_error(status, FM_USER_ERROR, format_message("You didn't specify a variable"))
            return
        end if
        call describe_variable(name, status)
    end subroutine cmd_describe_variable

    !> Whether `name' is a variable help knows (a documented one).
    logical function variable_known_p(name)
        character(len=*), intent(in) :: name
        integer :: k
        variable_known_p = .true.
        do k = 1, size(variable_names)
            if (trim(variable_names(k)) == name) return
        end do
        variable_known_p = .false.
    end function variable_known_p

    !> Display documentation of current major mode and minor modes.
    !> The major mode description comes first, followed by the local
    !> minor modes and the global minor modes, each under a heading.
    !>
    !> For this to work correctly for a minor mode, the mode's indicator
    !> variable (listed in `minor_mode_alist') must also be a function
    !> whose documentation describes the minor mode.  Port of
    !> help-fns.el:describe-mode (no BUFFER; the layout of
    !> `describe_mode_outline' true, without Outline minor mode itself).
    subroutine describe_mode(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        call describe_mode_text(text)
        call with_help_window(text, status)
    end subroutine describe_mode

    !> The text `describe_mode' shows for the current buffer.  Port of the
    !> text help-fns.el:describe-mode and describe-mode--minor-modes insert
    !> (with ensure-empty-lines; help-fns--list-local-commands finds no
    !> command: commands don't name the modes they are for).
    subroutine describe_mode_text(text)
        character(len=:), allocatable, intent(out) :: text
        character(len=:), allocatable :: major, mname
        character(len=64), allocatable :: local_modes(:), global_modes(:)
        type(doc_entry_type) :: entry
        type(minor_mode_type), pointer :: m
        logical :: found
        integer :: k, nl, ng
        major = "fundamental_mode"
        if (allocated(buffers(cur)%major_mode)) major = buffers(cur)%major_mode
        mname = "Fundamental"
        if (allocated(buffers(cur)%mode_name)) mname = buffers(cur)%mode_name
        ! The enabled minor modes, local and global.
        allocate (local_modes(minor_mode_count()), global_modes(minor_mode_count()))
        nl = 0
        ng = 0
        do k = 1, minor_mode_count()
            m => minor_mode_at(k)
            if (.not. m%enabled()) cycle
            if (m%global) then
                ng = ng + 1
                global_modes(ng) = m%name
            else
                nl = nl + 1
                local_modes(nl) = m%name
            end if
        end do
        ! Document the major mode.
        text = "Major mode "//major//LF//LF//"The major mode is "//mname//" mode"
        call function_doc(major, entry, found)
        if (found) text = text//" defined in "//entry%file
        text = text//":"//LF//LF
        if (found) text = text//substitute_command_keys(entry%doc)
        ! (ensure-empty-lines 1)
        do while (len(text) > 0)
            if (text(len(text):len(text)) /= LF) exit
            text = text(1:len(text) - 1)
        end do
        text = text//LF//LF
        call minor_modes(local_modes(1:nl), "Local")
        call minor_modes(global_modes(1:ng), "Global")
    contains

        !> Append the description of the minor modes `modes', sorted.
        !> Port of help-fns.el:describe-mode--minor-modes.
        subroutine minor_modes(modes, kind)
            character(len=64), intent(in) :: modes(:)
            character(len=*), intent(in) :: kind
            character(len=64), allocatable :: sorted(:)
            character(len=64) :: x
            type(minor_mode_type), pointer :: mm
            character(len=:), allocatable :: name, pretty, indicator, doc
            integer :: i, j
            type(doc_entry_type) :: e
            logical :: have
            sorted = modes
            do i = 2, size(sorted)
                x = sorted(i)
                j = i - 1
                do while (j >= 1)
                    if (llt(sorted(j), x) .or. sorted(j) == x) exit
                    sorted(j + 1) = sorted(j)
                    j = j - 1
                end do
                sorted(j + 1) = x
            end do
            do i = 1, size(sorted)
                name = trim(sorted(i))
                pretty = pretty_minor_mode(name)
                text = text//kind//" minor mode "//name//LF//LF
                mm => minor_mode_at(minor_mode_index_of(name))
                indicator = ""
                if (allocated(mm%lighter)) indicator = mm%lighter
                if (len(indicator) == 0) then
                    indicator = "no indicator"
                else
                    indicator = "indicator"//indicator
                end if
                text = text//pretty//" minor mode ("//indicator//"):"//LF
                call function_doc(name, e, have)
                doc = ""
                if (have) doc = substitute_command_keys(e%doc)
                if (len(doc) == 0) doc = "No docstring"
                text = text//doc//LF//LF
            end do
        end subroutine minor_modes
    end subroutine describe_mode_text

    !> The index of the minor mode named `name'.
    integer function minor_mode_index_of(name) result(k)
        character(len=*), intent(in) :: name
        type(minor_mode_type), pointer :: m
        do k = 1, minor_mode_count()
            m => minor_mode_at(k)
            if (trim(m%name) == name) return
        end do
        k = 0
    end function minor_mode_index_of

    !> The name of minor mode `name' as describe-mode shows it: without
    !> "_mode" or "_minor_mode" at the end, capitalized.
    function pretty_minor_mode(name) result(pretty)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: pretty
        pretty = name
        if (len(pretty) >= 11) then
            if (pretty(len(pretty) - 10:) == "_minor_mode") pretty = pretty(1:len(pretty) - 11)
        end if
        if (len(pretty) >= 5) then
            if (pretty(len(pretty) - 4:) == "_mode") pretty = pretty(1:len(pretty) - 5)
        end if
        pretty = capitalize_string(pretty)
    end function pretty_minor_mode

    !> The text `describe_symbol' shows about `name': its documentation as
    !> a function, then as a variable; `found' false if it is neither.
    !> Port of the text help-fns.el:describe-symbol makes (the function
    !> and variable backends).
    subroutine describe_symbol_text(name, text, found)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found
        character(len=:), allocatable :: ftext, vtext
        logical :: is_function, is_variable, vfound
        is_function = function_known_p(name)
        is_variable = variable_known_p(name)
        found = is_function .or. is_variable
        text = ""
        if (is_function) call describe_function_1(name, ftext, name//" is ")
        if (is_variable) call describe_variable_text(name, vtext, vfound)
        if (is_function .and. is_variable) then
            ! The function's documentation, then a separator line.
            do while (len(ftext) > 0)
                if (verify(ftext(len(ftext):len(ftext)), " "//achar(9)//LF) /= 0) exit
                ftext = ftext(1:len(ftext) - 1)
            end do
            text = ftext//LF//LF//make_separator_line()//LF//vtext
        else if (is_function) then
            text = ftext
        else if (is_variable) then
            text = vtext
        end if
    end subroutine describe_symbol_text

    !> Display the full documentation of SYMBOL (a name).
    !> Will show the info of SYMBOL as a function, variable, and/or face.
    !> Port of help-fns.el:describe-symbol (no faces or types; no BUFFER or
    !> FRAME).
    subroutine describe_symbol(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        logical :: found
        call describe_symbol_text(name, text, found)
        if (.not. found) then
            call set_error(status, FM_USER_ERROR, "Unknown symbol: "//name)
            return
        end if
        call with_help_window(text, status)
    end subroutine describe_symbol

    !> Completion predicate for `describe_symbol': a function or a
    !> variable.
    logical function symbol_known_p(elt)
        type(value_type), intent(in) :: elt
        character(len=:), allocatable :: name
        type(status_type) :: st
        call symbol_name(elt, name, st)
        symbol_known_p = st%ok()
        if (symbol_known_p) symbol_known_p = function_known_p(name) .or. variable_known_p(name)
    end function symbol_known_p

    !> Display the full documentation of SYMBOL (a name).
    !> Will show the info of SYMBOL as a function, variable, and/or face.
    !> Port of help-fns.el:describe-symbol (the command; no default from
    !> point).
    recursive subroutine cmd_describe_symbol(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        call completing_read("Describe symbol: ", completion_obarray_table(), name, status, symbol_known_p, &
                             require_match=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        if (len(name) == 0) then
            call set_error(status, FM_USER_ERROR, format_message("You didn't specify a function or variable"))
            return
        end if
        call describe_symbol(name, status)
    end subroutine cmd_describe_symbol

    !> Display the full documentation of COMMAND (a name).  Port of
    !> help-fns.el:describe-command (with
    !> help-fns--describe-function-or-command-prompt; no default from the
    !> command history).
    recursive subroutine cmd_describe_command(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        call completing_read("Describe command: ", completion_obarray_table(), name, status, commandp, &
                             require_match=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        if (len(name) == 0) then
            call set_error(status, FM_USER_ERROR, format_message("You didn't specify a command's symbol"))
            return
        end if
        call describe_function(name, status)
    end subroutine cmd_describe_command

    !> Display documentation of current major mode and minor modes.
    !> A brief summary of the minor modes comes first, followed by the
    !> major mode description.  This is followed by detailed
    !> descriptions of the minor modes, each on a separate page.
    !>
    !> For this to work correctly for a minor mode, the mode's indicator
    !> variable (listed in `minor_mode_alist') must also be a function
    !> whose documentation describes the minor mode.  Port of
    !> help-fns.el:describe-mode (the command; its layout is that of
    !> `describe_mode_outline' true).
    subroutine cmd_describe_mode(status)
        type(status_type), intent(out) :: status
        call describe_mode(status)
    end subroutine cmd_describe_mode

    !> Register the commands, intern the documented function and variable
    !> names (for completion and apropos) and install `describe_function_1' for describe-key.
    subroutine syms_of_help_fns()
        type(value_type) :: s
        integer :: k
        do k = 1, size(function_names)
            s = sym(trim(function_names(k)))
        end do
        ! The documented variables too (upstream interns every defvar).
        do k = 1, size(variable_names)
            s = sym(trim(variable_names(k)))
        end do
        describe_function_1_hook => describe_function_1
        documentation_hook => documentation
        commands%describe_function%run => cmd_describe_function
        commands%describe_variable%run => cmd_describe_variable
        commands%describe_mode%run => cmd_describe_mode
        commands%describe_symbol%run => cmd_describe_symbol
        commands%describe_command%run => cmd_describe_command
    end subroutine syms_of_help_fns

    !> The raw docstring of function NAME.  Port of doc.c:Fdocumentation
    !> (RAW t; the compiled-in docstrings).
    subroutine documentation(name, doc, found)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: doc
        logical, intent(out) :: found
        type(doc_entry_type) :: entry
        doc = ""
        call function_doc(name, entry, found)
        if (found .and. allocated(entry%doc)) doc = entry%doc
    end subroutine documentation
end module help_fns_fm
