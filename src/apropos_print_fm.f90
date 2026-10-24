! apropos_print_fm.f90 -- the apropos commands' searches and *Apropos*.
! Ported from GNU Emacs 31.1 lisp/apropos.el.
!
! Copyright (C) 1989-2026 Free Software Foundation, Inc.
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

!> The searches of `apropos_command' and `apropos_documentation', the
!> *Apropos* buffer they fill, and `apropos_follow': a submodule because
!> they use the documentation (docstrings_fm), which reads apropos_fm's
!> variables.
submodule(apropos_fm) apropos_print_fm
    use docstrings_fm, only: doc_entry_type, function_doc, variable_doc
    use help_fns_fm, only: describe_function, describe_variable, function_known_p, variable_known_p
    implicit none(type, external)

contains

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    module procedure apropos_command
    type(apropos_item_type), allocatable :: items(:)
    type(doc_entry_type) :: entry
    character(len=:), allocatable :: name, msg
    type(status_type) :: st
    logical :: found, ok
    integer :: k, n
    status = status_type()
    call apropos_parse_pattern(pattern, words, .false.)
    msg = return_message()
    allocate (items(nsymbols))
    n = 0
    do k = 1, int(nsymbols)
        name = symbols(k)%name
        if (string_match(apropos_regexp, name, st, inhibit_modify=.true.) == 0) cycle
        if (.not. st%ok()) then
            status = st
            return
        end if
        select case (var_kind)
        case (VAR_USER_OPTION)
            call variable_doc(name, entry, found)
            ok = found
            if (ok) ok = entry%user_option
        case (VAR_ANY)
            ok = variable_known_p(name)
        case default
            if (do_all) then
                ok = function_known_p(name)
            else
                ok = commandp(sym(name))
            end if
        end select
        if (.not. ok) cycle
        if (.not. apropos_true_hit(name, apropos_words)) cycle
        n = n + 1
        items(n)%name = name
        items(n)%score = apropos_score_symbol(name, 3)
        if (var_kind == VAR_NONE) then
            if (function_known_p(name)) then
                call function_doc(name, entry, found)
                items(n)%fn_doc = "(not documented)"
                if (found) then
                    if (len(entry%doc) > 0) then
                        items(n)%score = items(n)%score + apropos_score_doc(entry%doc)
                        items(n)%fn_doc = first_line(entry%doc)
                    end if
                end if
            end if
        else
            call variable_doc(name, entry, found)
            if (found) then
                if (len(entry%doc) > 0) then
                    items(n)%score = items(n)%score + apropos_score_doc(entry%doc)
                    items(n)%var_doc = first_line(entry%doc)
                end if
            end if
        end if
    end do
    call apropos_print(items(1:n), .true., "", .true., do_all, .false., status)
    if (.not. status%ok()) return
    if (n > 0 .and. len(msg) > 0) call message(msg)
    end procedure apropos_command

    module procedure apropos_documentation
    type(apropos_item_type), allocatable :: items(:)
    type(doc_entry_type) :: entry
    character(len=:), allocatable :: name, fdoc, vdoc
    logical :: found, has_f, has_v
    integer :: k, n
    status = status_type()
    call apropos_parse_pattern(pattern, words, .true.)
    allocate (items(nsymbols))
    n = 0
    do k = 1, int(nsymbols)
        name = symbols(k)%name
        has_f = .false.
        has_v = .false.
        if (function_known_p(name)) then
            call function_doc(name, entry, found)
            if (found) then
                fdoc = entry%doc
                has_f = documentation_internal(fdoc)
            end if
        end if
        if (variable_known_p(name)) then
            call variable_doc(name, entry, found)
            if (found) then
                vdoc = entry%doc
                has_v = documentation_internal(vdoc)
            end if
        end if
        if (.not. (has_f .or. has_v)) cycle
        n = n + 1
        items(n)%name = name
        items(n)%score = apropos_score_symbol(name, 2)
        if (has_f) then
            items(n)%fn_doc = fdoc
            items(n)%score = items(n)%score + apropos_score_doc(fdoc)
        end if
        if (has_v) then
            items(n)%var_doc = vdoc
            items(n)%score = items(n)%score + apropos_score_doc(vdoc)
        end if
    end do
    call apropos_print(items(1:n), .false., LF//"----------------"//LF, .true., .true., .true., status)
    end procedure apropos_documentation

    !> Whether DOC is really matched by the current pattern; it becomes
    !> its text after `substitute_command_keys'.  Port of
    !> apropos.el:apropos-documentation-internal (no match face).
    logical function documentation_internal(doc) result(hit)
        character(len=:), allocatable, intent(inout) :: doc
        type(status_type) :: st
        hit = .false.
        if (len(doc) == 0) return
        if (string_match(apropos_all_words_regexp, doc, st, inhibit_modify=.true.) == 0) return
        if (.not. apropos_true_hit(doc, apropos_all_words)) return
        doc = substitute_command_keys(doc)
        hit = .true.
    end function documentation_internal

    !> The first line of `doc'.
    function first_line(doc) result(s)
        character(len=*), intent(in) :: doc
        character(len=:), allocatable :: s
        integer :: k
        k = index(doc, LF)
        if (k == 0) then
            s = doc
        else
            s = doc(1:k - 1)
        end if
    end function first_line

    !> The message saying how to restore windows after help, as
    !> `apropos_command' shows it after listing: computed before *Apropos*
    !> is displayed.  Port of help.el:help-print-return-message with
    !> FUNCTION `identity' (`pop_up_windows' true; no frames).
    function return_message() result(msg)
        use window_fm, only: pop_up_windows
        character(len=:), allocatable :: msg
        type(handle_type), allocatable :: leaves(:)
        type(handle_type) :: buf
        integer :: k
        msg = ""
        ! Not when the buffer is shown already.
        buf = get_buffer("*Apropos*")
        call window_leaves(selected_frame%root_window, leaves)
        if (buffer_live_p(buf)) then
            do k = 1, size(leaves)
                if (shows(leaves(k), buf)) return
            end do
        end if
        if (size(leaves) > 1) then
            msg = substitute_command_keys("Type \[display_buffer] RET to restore the other window.")
        else if (pop_up_windows) then
            msg = substitute_command_keys("Type \[delete_other_windows] to remove help window.")
        else
            msg = substitute_command_keys("Type \[switch_to_buffer] RET to remove help window.")
        end if
        msg = msg//"  "
        if (.not. pop_up_windows) msg = msg//substitute_command_keys("\[scroll_other_window] to scroll the help.")
    contains

        logical function shows(w, b)
            use window_fm, only: windows
            type(handle_type), intent(in) :: w, b
            shows = windows(w%index)%buffer%index == b%index .and. windows(w%index)%buffer%generation == b%generation
        end function shows
    end function return_message

    ! ------------------------------------------------------------------
    ! Output
    ! ------------------------------------------------------------------

    !> Output result of apropos searching into buffer `*Apropos*'.
    !> ITEMS is the list of items to output, sorted alphabetically by
    !> name (by score, if `by_score').  If DO_KEYS, output the key
    !> bindings.  If SPACING is not empty, separate items with that
    !> string.  NOSUBST leaves docstrings alone.  MULTI_TYPE labels each
    !> documentation with its type.  Port of apropos.el:apropos-print
    !> (with apropos-print-doc and apropos--preamble; no TEXT).
    subroutine apropos_print(items_in, do_keys, spacing, nosubst, multi_type, by_score, status)
        type(apropos_item_type), intent(in) :: items_in(:)
        logical, intent(in) :: do_keys, nosubst, multi_type, by_score
        character(len=*), intent(in) :: spacing
        type(status_type), intent(out) :: status
        type(apropos_item_type), allocatable :: items(:)
        type(apropos_item_type) :: x
        character(len=:), allocatable :: text, keys, label_type
        type(value_type) :: all
        type(value_type), allocatable :: seqs(:), events(:)
        type(doc_entry_type) :: entry
        integer(int64), allocatable :: name_start(:), name_end(:), lab_start(:), lab_end(:)
        type(apropos_word_type), allocatable :: lab_type(:), lab_name(:)
        integer :: i, j, k, nlab, button_end
        logical :: found
        status = status_type()
        if (size(items_in) == 0) then
            call message(format_message("No apropos matches for `"//apropos_pattern//"'"))
            return
        end if
        items = items_in
        do i = 2, size(items)
            x = items(i)
            j = i - 1
            do while (j >= 1)
                if (.not. item_less(x, items(j), by_score)) exit
                items(j + 1) = items(j)
                j = j - 1
            end do
            items(j + 1) = x
        end do
        allocate (name_start(size(items)), name_end(size(items)), lab_start(2*size(items)), lab_end(2*size(items)))
        allocate (lab_type(2*size(items)), lab_name(2*size(items)))
        nlab = 0
        text = substitute_command_keys("Type \<apropos_mode_map>\[apropos_follow] on ")
        if (multi_type) then
            text = text//"a type label"
        else
            text = text//"an entry"
        end if
        text = text//" to view its full documentation."//LF//LF
        button_end = -1
        do i = 1, size(items)
            if (len(spacing) > 0 .and. i > 1) text = text//spacing
            if (nchars(text) == button_end) text = text//LF
            name_start(i) = nchars(text) + 1
            text = text//items(i)%name
            name_end(i) = nchars(text) + 1
            button_end = nchars(text)
            ! Calculate key-bindings if we want them.
            if (do_keys .and. commandp(sym(items(i)%name)) .and. items(i)%name /= "self_insert_command") then
                text = text//indent_to(current_column(text), 30, 1)
                call where_is_internal(sym(items(i)%name), all)
                if (nilp(all)) then
                    text = text//"M-x ... RET"
                else
                    call list_elements(all, seqs)
                    keys = ""
                    do k = 1, size(seqs)
                        call list_elements(seqs(k), events)
                        if (k > 1) keys = keys//", "
                        keys = keys//key_description(events)
                    end do
                    text = text//keys
                end if
            end if
            text = text//LF
            if (allocated(items(i)%fn_doc)) then
                if (commandp(sym(items(i)%name))) then
                    label_type = "apropos_command"
                else
                    label_type = "apropos_function"
                end if
                call print_doc(items(i)%fn_doc, label_type)
            end if
            if (allocated(items(i)%var_doc)) then
                call variable_doc(items(i)%name, entry, found)
                label_type = "apropos_variable"
                if (found) then
                    if (entry%user_option) label_type = "apropos_user_option"
                end if
                call print_doc(items(i)%var_doc, label_type)
            end if
            ! Insert an excerpt of variable values.
            if (variable_known_p(items(i)%name)) then
                call variable_doc(items(i)%name, entry, found)
                if (found .and. allocated(entry%value)) then
                    if (len(entry%value) > 0) text = text//"  Value: "//truncated(entry%value)//LF
                end if
            end if
        end do
        call with_output_to_temp_buffer("*Apropos*", text, status)
        if (.not. status%ok()) return
        call mark_entries()
    contains

        !> Append the documentation DOC of the current item, labeled with
        !> its type.  Port of apropos.el:apropos-print-doc.
        subroutine print_doc(doc_in, type)
            character(len=*), intent(in) :: doc_in, type
            character(len=:), allocatable :: doc, line, filled
            integer :: ocol, k
            doc = doc_in
            text = text//" "
            if (multi_type) then
                text = text//" "
                nlab = nlab + 1
                lab_start(nlab) = nchars(text) + 1
                text = text//type_label(type)
                lab_end(nlab) = nchars(text) + 1
                text = text//": "
            else
                ! A blank button with the entry's name.
                nlab = nlab + 1
                lab_start(nlab) = nchars(text) + 1
                text = text//" "
                lab_end(nlab) = nchars(text) + 1
            end if
            lab_type(nlab)%s = type
            lab_name(nlab)%s = items(i)%name
            ocol = current_column(text)
            if (len(doc) == 0) then
                doc = "(not documented)"
            else if (.not. nosubst) then
                doc = substitute_command_keys(doc)
            end if
            ! The labeling buttons might make the line too long, so fill it
            ! if necessary.
            k = index(text, LF, back=.true.)
            line = text(k + 1:)
            filled = fill_doc(line, doc, ocol)
            text = text(1:k)//filled
            if (text(len(text):len(text)) /= LF) text = text//LF
        end subroutine print_doc

        !> Put the properties marking names and labels on *Apropos*.
        subroutine mark_entries()
            type(handle_type) :: old, buf
            type(status_type) :: st
            integer :: m
            old = current_buffer()
            buf = get_buffer("*Apropos*")
            call set_buffer(buf, st)
            call apropos_mode(st)
            buffers(cur)%read_only = .false.
            do m = 1, size(items)
                call put_text_property(name_start(m), name_end(m), sym(P_BUTTON), sym("apropos_symbol"), st)
                call put_text_property(name_start(m), name_end(m), sym(P_NAME), make_string(items(m)%name), st)
            end do
            do m = 1, nlab
                call put_text_property(lab_start(m), lab_end(m), sym(P_BUTTON), sym(lab_type(m)%s), st)
                call put_text_property(lab_start(m), lab_end(m), sym(P_NAME), make_string(lab_name(m)%s), st)
            end do
            buffers(cur)%read_only = .true.
            buffers(cur)%text%save_modiff = buffers(cur)%text%modiff
            if (buffer_live_p(old)) call set_buffer(old, st)
        end subroutine mark_entries
    end subroutine apropos_print

    !> The label of an apropos button type.
    function type_label(type) result(label)
        character(len=*), intent(in) :: type
        character(len=:), allocatable :: label
        select case (type)
        case ("apropos_command")
            label = "Command"
        case ("apropos_function")
            label = "Function"
        case ("apropos_user_option")
            label = "User option"
        case default
            label = "Variable"
        end select
    end function type_label

    !> Whether `a' sorts before `b': by name (string_lessp), or by score
    !> first when sorting by scores (only `apropos_documentation').
    logical function item_less(a, b, by_score)
        type(apropos_item_type), intent(in) :: a, b
        logical, intent(in) :: by_score
        if (by_score) then
            item_less = a%score > b%score
            if (a%score == b%score) item_less = llt(a%name, b%name)
        else
            item_less = llt(a%name, b%name)
        end if
    end function item_less

    !> The value excerpt: VALUE truncated to the window width less 20,
    !> with an ellipsis.  Port of the truncate-string-to-width call in
    !> apropos.el:apropos-print.
    function truncated(value) result(s)
        use window_fm, only: windows, window_text_cols
        use frame_fm, only: selected_window
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: s
        character(len=*), parameter :: ELLIPSIS = char(226)//char(128)//char(166)
        type(handle_type) :: sw
        integer :: width, k, n
        width = 60
        sw = selected_window()
        if (sw%index > 0) width = window_text_cols(windows(sw%index)) - 20
        s = value
        if (nchars(value) <= width) return
        ! Characters up to width - 1, then the ellipsis.
        n = 0
        do k = 1, len(value)
            if (iand(ichar(value(k:k)), 192) /= 128) n = n + 1
            if (n > width - 1) exit
        end do
        s = value(1:k - 1)//ELLIPSIS
    end function truncated

    !> The number of characters in UTF-8 `s'.
    integer function nchars(s) result(n)
        character(len=*), intent(in) :: s
        integer :: k
        n = 0
        do k = 1, len(s)
            if (iand(ichar(s(k:k)), 192) /= 128) n = n + 1
        end do
    end function nchars

    !> The column at the end of `text' (characters since its last newline,
    !> tabs to multiples of 8).
    integer function current_column(text) result(col)
        character(len=*), intent(in) :: text
        integer :: k
        col = 0
        do k = index(text, LF, back=.true.) + 1, len(text)
            if (text(k:k) == TAB) then
                col = (col/8 + 1)*8
            else if (iand(ichar(text(k:k)), 192) /= 128) then
                col = col + 1
            end if
        end do
    end function current_column

    !> What indent-to inserts at column `from' to reach `column', at least
    !> `minimum' spaces, with tabs (indent-tabs-mode).
    function indent_to(from, column, minimum) result(s)
        integer, intent(in) :: from, column, minimum
        character(len=:), allocatable :: s
        integer :: mincol, n, col
        mincol = max(column, from + minimum)
        s = ""
        col = from
        n = mincol/8 - col/8
        if (n > 0) then
            s = repeat(TAB, n)
            col = (mincol/8)*8
        end if
        s = s//repeat(" ", mincol - col)
    end function indent_to

    !> LINE followed by DOC, the doc filled with a fill column of 70 and a
    !> fill prefix of OCOL spaces, as apropos-print-doc fills it
    !> (fill-region with NOSQUEEZE).
    function fill_doc(line, doc, ocol) result(out)
        character(len=*), intent(in) :: line, doc
        integer, intent(in) :: ocol
        character(len=:), allocatable :: out
        type(handle_type) :: old, temp
        type(status_type) :: st
        integer(int64) :: opoint
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), temp, st)
        call set_buffer(temp, st)
        buffers(cur)%undo_enabled = .false.
        call insert(line, st)
        opoint = buffers(cur)%pt
        call insert(doc, st)
        call fill_column%set_local(70_int64)
        call fill_prefix%set_local(repeat(" ", ocol))
        call fill_region(opoint, buffers(cur)%pt, st, nosqueeze=.true.)
        out = buffer_string()
        call set_buffer(old, st)
        call kill_buffer(temp, st)
    end function fill_doc

    !> Return the next apropos label button after POS, or 0 if there's
    !> none.  Will also return 0 if more than one `apropos_symbol' button
    !> is encountered before finding a label.  Port of
    !> apropos.el:apropos-next-label-button.
    integer(int64) function apropos_next_label_button(pos) result(btn)
        integer(int64), intent(in) :: pos
        character(len=:), allocatable :: type, name
        type(status_type) :: st
        integer(int64) :: p, nxt
        logical :: found, already_hit_symbol
        btn = 0
        already_hit_symbol = .false.
        p = pos
        ! (next-button pos t): a button at POS counts.
        call button_at(p, type, name)
        if (len(type) == 0) then
            call next_single_property_change(p, sym(P_BUTTON), nxt, found, st)
            if (.not. found) return
            p = nxt
            call button_at(p, type, name)
        end if
        do while (len(type) > 0)
            if (type /= "apropos_symbol") then
                btn = p
                return
            end if
            if (already_hit_symbol) return
            already_hit_symbol = .true.
            ! The next button after this one.
            call next_single_property_change(p, sym(P_BUTTON), nxt, found, st)
            if (.not. found) return
            p = nxt
            call button_at(p, type, name)
            if (len(type) == 0) then
                call next_single_property_change(p, sym(P_BUTTON), nxt, found, st)
                if (.not. found) return
                p = nxt
                call button_at(p, type, name)
            end if
        end do
    end function apropos_next_label_button

    module procedure apropos_follow
    character(len=:), allocatable :: type, name
    integer(int64) :: btn
    btn = apropos_next_label_button(line_beginning_position())
    if (btn == 0) then
        call set_error(status, FM_GENERIC_ERROR, "There is nothing to follow here")
        return
    end if
    call button_at(btn, type, name)
    select case (type)
    case ("apropos_command", "apropos_function")
        call describe_function(name, status)
    case default
        call describe_variable(name, status)
    end select
    end procedure apropos_follow
end submodule apropos_print_fm
