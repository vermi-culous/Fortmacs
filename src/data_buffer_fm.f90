! data_buffer_fm.f90 -- buffer-local variables, a submodule of data_fm.
! Ported from GNU Emacs 31.1 src/data.c and src/buffer.c.
!
! Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2026 Free Software
! Foundation, Inc.
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

!> Buffer-local variables (data.c's SYMBOL_LOCALIZED and
!> buffer.c's local_var_alist) and the variables forwarded to Fortran
!> settings (data.c's SYMBOL_FORWARDED).
!>
!> The value of a variable is always the current buffer's.  When a
!> buffer has its own value for a variable, the default is kept in the
!> symbol (`default_val') while that buffer is current, and the local
!> value in the buffer (`local_values') while it is not; the swap
!> happens in set_buffer_internal.  Variables forwarded to procedure
!> pointers (auto-fill-function, indent-line-function) keep the pointers
!> themselves in this submodule, so a procedure without a name survives
!> a swap; as values they are known by name.
!>
!> A submodule because the Fortran settings live in modules that use
!> data_fm.
submodule(data_fm) data_buffer_fm
    use lisp_fm, only: t_value, nilp, make_fixnum, make_string, make_symbol_ref, make_list, xfixnum, xstring
    use lread_fm, only: intern, sym
    use handle_fm, only: null_handle
    use buffer_fm, only: buffers, cur, swap_buffer_locals_hook, fill_column, left_margin, &
                         auto_fill_function, auto_fill_proc, buffer_live_p, buffer_handle, transient_mark_mode
    use xdisp_fm, only: tab_width
    use indent_fm, only: indent_tabs_mode, standard_indent, indent_line_function, indent_line_proc, &
                         indent_line_setting, indent_line_function_name, indent_line_function_named
    use callint_fm, only: find_command, command_type, funcall, command_name_of
    use lisp_fm, only: command_proc
    use search_fm, only: case_fold_search
    use minibuf_fm, only: completion_ignore_case
    use paragraphs_fm, only: paragraph_start, paragraph_separate, sentence_end, &
                             sentence_end_double_space, init_paragraph_variables
    use fill_fm, only: adaptive_fill_mode
    use simple_fm, only: fill_prefix, auto_fill_inhibit_regexp, do_auto_fill
    use fns_fm, only: get
    use eval_fm, only: change_major_mode_hook, kill_local_hooks
    use subr_fm, only: kill_delayed_mode_hooks
    use easy_mmode_fm, only: minor_mode_type, minor_mode_index, minor_mode_at
    use text_mode_fm, only: text_mode_variant
    use syntax_fm, only: parse_sexp_ignore_comments, comment_end_can_be_escaped
    use casefiddle_fm, only: case_symbols_as_words
    use insdel_fm, only: inhibit_modification_hooks
    use electric_fm, only: electric_indent_inhibit
    use files_fm, only: require_final_newline, mode_require_final_newline, FINAL_NEWLINE_NEVER, &
                        FINAL_NEWLINE_SAVE, FINAL_NEWLINE_VISIT, FINAL_NEWLINE_VISIT_SAVE, FINAL_NEWLINE_ASK
    implicit none(type, external)

    !> Kinds of forwarded variable.
    integer, parameter :: K_INT64 = 1, K_INT = 2, K_BOOL = 3, K_STRING = 4, K_AUTO_FILL = 5, &
                          K_INDENT_LINE = 6, K_SLOT = 7

    !> The variables forwarded to Fortran settings; slot variables are
    !> per-buffer fields of buffer_type (always local, like buffer.c's
    !> slots with a local flag of -1).
    integer, parameter :: NFWD = 28
    character(len=*), parameter :: FWD_NAMES(NFWD) = [character(len=32) :: &
                                                "fill_column", "left_margin", "tab_width", "indent_tabs_mode", "case_fold_search", &
                                                      "fill_prefix", "paragraph_start", "paragraph_separate", "sentence_end", &
                                                    "sentence_end_double_space", "adaptive_fill_mode", "auto_fill_inhibit_regexp", &
                                           "standard_indent", "auto_fill_function", "indent_line_function", "transient_mark_mode", &
                                                      "major_mode", "mode_name", "buffer_read_only", "text_mode_variant", &
                                                      "parse_sexp_ignore_comments", "require_final_newline", &
                                                      "mode_require_final_newline", "case_symbols_as_words", &
                                                      "completion_ignore_case", "comment_end_can_be_escaped", &
                                                      "inhibit_modification_hooks", &
                                                      "electric_indent_inhibit"]
    integer, parameter :: FWD_KIND(NFWD) = [K_INT64, K_INT64, K_INT, K_BOOL, K_BOOL, &
                                            K_STRING, K_STRING, K_STRING, K_STRING, &
                                            K_BOOL, K_BOOL, K_STRING, &
                                            K_INT64, K_AUTO_FILL, K_INDENT_LINE, K_BOOL, &
                                            K_SLOT, K_SLOT, K_SLOT, K_BOOL, K_BOOL, K_INT, K_INT, K_BOOL, K_BOOL, K_BOOL, K_BOOL, &
                                            K_BOOL]
    !> DEFVAR_PER_BUFFER, `make_variable_buffer_local' or `:local t':
    !> local when set.
    logical, parameter :: FWD_PER_BUFFER(NFWD) = [.true., .true., .true., .false., .true., &
                                                  .true., .false., .false., .false., &
                                                  .false., .false., .false., &
                                                  .false., .true., .false., .false., &
                                                  .true., .true., .true., .false., .false., .false., .false., .true., .false., &
                                                  .true., .false., .true.]

    !> Saved procedure pointers: the default while the current buffer
    !> has a local value, or a buffer's local value while it is not
    !> current.
    type :: procs_type
        type(function_setting) :: auto_fill
        type(indent_line_setting) :: indent_line
    end type procs_type

    type(procs_type) :: default_procs
    !> Indexed by buffer slot.
    type(procs_type), allocatable :: saved_procs(:)

contains

    ! ------------------------------------------------------------------
    ! Forwarding
    ! ------------------------------------------------------------------

    !> The forwarding table index of symbol ID, or -1.
    integer function resolve(id) result(k)
        integer(int64), intent(in) :: id
        integer :: i
        if (symbols(id)%fwd == 0) then
            symbols(id)%fwd = -1
            do i = 1, NFWD
                if (symbols(id)%name == trim(FWD_NAMES(i))) then
                    symbols(id)%fwd = i
                    if (FWD_PER_BUFFER(i)) symbols(id)%local_if_set = .true.
                    exit
                end if
            end do
            if (symbols(id)%fwd < 0) call resolve_minor_mode(id)
        end if
        k = symbols(id)%fwd
    end function resolve

    !> Forward symbol ID to the state of the minor mode of that name, if
    !> the mode keeps its own state: its variable is buffer-local when
    !> set unless the mode is global.  Forwarded minor modes come after
    !> the table (NFWD + the mode's index).
    subroutine resolve_minor_mode(id)
        integer(int64), intent(in) :: id
        type(minor_mode_type), pointer :: m
        integer :: i
        i = minor_mode_index(symbols(id)%name)
        if (i == 0) return
        m => minor_mode_at(i)
        if (len_trim(m%variable) > 0) return
        ! A mode whose state is an expression has no variable.
        if (associated(m%getter)) return
        symbols(id)%fwd = NFWD + i
        if (.not. m%global) symbols(id)%local_if_set = .true.
    end subroutine resolve_minor_mode

    !> The kind of symbol ID: 0 for a plain variable.
    integer function kind_of(id) result(kind)
        integer(int64), intent(in) :: id
        integer :: k
        kind = 0
        k = resolve(id)
        if (k > NFWD) then
            kind = K_BOOL
        else if (k > 0) then
            kind = FWD_KIND(k)
        end if
    end function kind_of

    !> The value of an auto-fill-function as a value: its name (nil for
    !> none; t for a procedure without a name).
    function auto_fill_value(p) result(v)
        procedure(command_proc), pointer, intent(in) :: p
        type(value_type) :: v
        character(len=:), allocatable :: name
        if (.not. associated(p)) then
            v = nil()
            return
        end if
        name = command_name_of(p)
        if (len(name) > 0) then
            v = sym(name)
        else
            v = t_value()
        end if
    end function auto_fill_value

    !> The value of an indent-line-function: its name (t for a procedure
    !> without a name).
    function indent_line_value(p) result(v)
        procedure(indent_line_proc), pointer, intent(in) :: p
        type(value_type) :: v
        character(len=:), allocatable :: name
        name = indent_line_function_name(p)
        if (len(name) > 0) then
            v = sym(name)
        else
            v = t_value()
        end if
    end function indent_line_value

    !> auto-fill-function set to a function that has no Fortran
    !> procedure of this kind: call it by name.
    subroutine auto_fill_by_name(status)
        type(status_type), intent(out) :: status
        call funcall(auto_fill_function%exact, status)
    end subroutine auto_fill_by_name

    function bool_value(b) result(v)
        logical, intent(in) :: b
        type(value_type) :: v
        if (b) then
            v = t_value()
        else
            v = nil()
        end if
    end function bool_value

    !> The value of setting S given its Fortran value as FORTRAN: the exact
    !> value if one is kept, unbound if void.
    subroutine setting_value(s, fortran, v, bound)
        class(setting_type), intent(in) :: s
        type(value_type), intent(in) :: fortran
        type(value_type), intent(out) :: v
        logical, intent(out) :: bound
        bound = .not. s%void
        if (s%has_exact) then
            v = s%exact
        else
            v = fortran
        end if
    end subroutine setting_value

    !> Keep V as the exact value of S (its Fortran value can't hold it).
    subroutine keep_exact(s, v)
        class(setting_type), intent(inout) :: s
        type(value_type), intent(in) :: v
        s%exact = v
        s%has_exact = .true.
        s%void = .false.
    end subroutine keep_exact

    !> Set integer setting S to V.  With INTEGERP (a DEFVAR_PER_BUFFER with
    !> predicate `integerp'), a non-nil V must be an integer; otherwise any
    !> value is kept.
    subroutine set_integer(s, v, integerp, status)
        type(integer_setting), intent(inout) :: s
        type(value_type), intent(in) :: v
        logical, intent(in) :: integerp
        type(status_type), intent(out) :: status
        status = status_type()
        if (fixnump(v)) then
            s = xfixnum(v)
        else if (integerp .and. .not. nilp(v)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "integerp")
        else
            call keep_exact(s, v)
        end if
    end subroutine set_integer

    !> Set logical setting S to V (any value; the Fortran value is whether
    !> it is non-nil).
    subroutine set_logical(s, v)
        type(logical_setting), intent(inout) :: s
        type(value_type), intent(in) :: v
        s = .not. nilp(v)
        if (.not. (nilp(v) .or. value_eq(v, t_value()))) call keep_exact(s, v)
    end subroutine set_logical

    !> Set string setting S to V (any value; the Fortran value is nil
    !> unless it is a string).
    subroutine set_string(s, v)
        type(string_setting), intent(inout) :: s
        type(value_type), intent(in) :: v
        if (stringp(v)) then
            s = xstring(v)
        else
            call s%clear()
            if (.not. nilp(v)) call keep_exact(s, v)
        end if
    end subroutine set_string

    !> The value of forwarded variable K in buffer B (B matters only for
    !> slot variables; the others read the current value); BOUND is false
    !> when it is void.
    subroutine fwd_get(k, b, v, bound)
        integer, intent(in) :: k
        integer(int64), intent(in) :: b
        type(value_type), intent(out) :: v
        logical, intent(out) :: bound
        type(minor_mode_type), pointer :: m
        bound = .true.
        if (k > NFWD) then
            m => minor_mode_at(k - NFWD)
            call setting_value(m%state, bool_value(m%state%value), v, bound)
            return
        end if
        select case (trim(FWD_NAMES(k)))
        case ("fill_column")
            call setting_value(fill_column, make_fixnum(fill_column%value), v, bound)
        case ("left_margin")
            call setting_value(left_margin, make_fixnum(left_margin%value), v, bound)
        case ("tab_width")
            call setting_value(tab_width, make_fixnum(tab_width%value), v, bound)
        case ("indent_tabs_mode")
            ! A DEFVAR_BOOL: always t or nil.
            v = bool_value(indent_tabs_mode%value)
        case ("completion_ignore_case")
            v = bool_value(completion_ignore_case%value)
        case ("case_fold_search")
            call setting_value(case_fold_search, bool_value(case_fold_search%value), v, bound)
        case ("fill_prefix")
            call setting_value(fill_prefix, string_or_nil(fill_prefix%value), v, bound)
        case ("paragraph_start")
            call init_paragraph_variables()
            call setting_value(paragraph_start, string_or_nil(paragraph_start%value), v, bound)
        case ("paragraph_separate")
            call init_paragraph_variables()
            call setting_value(paragraph_separate, string_or_nil(paragraph_separate%value), v, bound)
        case ("sentence_end")
            call setting_value(sentence_end, string_or_nil(sentence_end%value), v, bound)
        case ("sentence_end_double_space")
            call setting_value(sentence_end_double_space, bool_value(sentence_end_double_space%value), v, bound)
        case ("adaptive_fill_mode")
            call setting_value(adaptive_fill_mode, bool_value(adaptive_fill_mode%value), v, bound)
        case ("auto_fill_inhibit_regexp")
            call setting_value(auto_fill_inhibit_regexp, string_or_nil(auto_fill_inhibit_regexp%value), v, bound)
        case ("standard_indent")
            call setting_value(standard_indent, make_fixnum(standard_indent%value), v, bound)
        case ("auto_fill_function")
            call setting_value(auto_fill_function, auto_fill_value(auto_fill_function%value), v, bound)
        case ("indent_line_function")
            call setting_value(indent_line_function, indent_line_value(indent_line_function%value), v, bound)
        case ("transient_mark_mode")
            call setting_value(transient_mark_mode, bool_value(transient_mark_mode%value), v, bound)
        case ("electric_indent_inhibit")
            call setting_value(electric_indent_inhibit, bool_value(electric_indent_inhibit%value), v, bound)
        case ("major_mode")
            if (b > 0 .and. allocated(buffers(b)%major_mode)) then
                v = sym(buffers(b)%major_mode)
            else
                v = sym("fundamental_mode")
            end if
        case ("mode_name")
            if (b > 0 .and. allocated(buffers(b)%mode_name)) then
                v = make_string(buffers(b)%mode_name)
            else
                v = make_string("Fundamental")
            end if
        case ("buffer_read_only")
            v = nil()
            if (b > 0) v = bool_value(buffers(b)%read_only)
        case ("text_mode_variant")
            call setting_value(text_mode_variant, bool_value(text_mode_variant%value), v, bound)
        case ("parse_sexp_ignore_comments")
            ! A DEFVAR_BOOL: always t or nil.
            v = bool_value(parse_sexp_ignore_comments%value)
        case ("case_symbols_as_words")
            ! A DEFVAR_BOOL: always t or nil.
            v = bool_value(case_symbols_as_words%value)
        case ("comment_end_can_be_escaped")
            ! A DEFVAR_BOOL: always t or nil.
            v = bool_value(comment_end_can_be_escaped%value)
        case ("inhibit_modification_hooks")
            ! A DEFVAR_BOOL: always t or nil.
            v = bool_value(inhibit_modification_hooks%value)
        case ("require_final_newline")
            call setting_value(require_final_newline, final_newline_value(require_final_newline%value), v, bound)
        case ("mode_require_final_newline")
            call setting_value(mode_require_final_newline, final_newline_value(mode_require_final_newline%value), &
                               v, bound)
        case default
            v = nil()
        end select
    end subroutine fwd_get

    !> A string or nil.
    function string_or_nil(s) result(v)
        character(len=:), allocatable, intent(in) :: s
        type(value_type) :: v
        if (allocated(s)) then
            v = make_string(s)
        else
            v = nil()
        end if
    end function string_or_nil

    !> Set forwarded variable K (of the current buffer, for slots).
    !> Port of data.c:store_symval_forwarding.
    subroutine fwd_set(k, v, status)
        integer, intent(in) :: k
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status
        type(minor_mode_type), pointer :: m
        status = status_type()
        if (k > NFWD) then
            m => minor_mode_at(k - NFWD)
            call set_logical(m%state, v)
            return
        end if
        select case (trim(FWD_NAMES(k)))
        case ("fill_column")
            call set_integer(fill_column, v, .true., status)
        case ("left_margin")
            call set_integer(left_margin, v, .true., status)
        case ("tab_width")
            call set_integer(tab_width, v, .true., status)
        case ("indent_tabs_mode")
            indent_tabs_mode = .not. nilp(v)
        case ("completion_ignore_case")
            completion_ignore_case = .not. nilp(v)
        case ("case_fold_search")
            call set_logical(case_fold_search, v)
        case ("fill_prefix")
            call set_string(fill_prefix, v)
        case ("paragraph_start")
            call set_string(paragraph_start, v)
        case ("paragraph_separate")
            call set_string(paragraph_separate, v)
        case ("sentence_end")
            call set_string(sentence_end, v)
        case ("sentence_end_double_space")
            call set_logical(sentence_end_double_space, v)
        case ("adaptive_fill_mode")
            call set_logical(adaptive_fill_mode, v)
        case ("auto_fill_inhibit_regexp")
            call set_string(auto_fill_inhibit_regexp, v)
        case ("standard_indent")
            call set_integer(standard_indent, v, .false., status)
        case ("auto_fill_function")
            call set_auto_fill(auto_fill_function, v)
        case ("indent_line_function")
            call set_indent_line(indent_line_function, v)
        case ("transient_mark_mode")
            call set_logical(transient_mark_mode, v)
        case ("electric_indent_inhibit")
            call set_logical(electric_indent_inhibit, v)
        case ("major_mode")
            if (cur == 0) return
            if (.not. symbolp(v)) then
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "symbolp")
            else if (nilp(v)) then
                buffers(cur)%major_mode = "nil"
            else
                buffers(cur)%major_mode = symbols(xsymbol(v))%name
            end if
        case ("mode_name")
            if (cur == 0) return
            if (stringp(v)) then
                buffers(cur)%mode_name = xstring(v)
            else
                call set_error(status, FM_WRONG_TYPE_ARGUMENT, "stringp")
            end if
        case ("buffer_read_only")
            if (cur > 0) buffers(cur)%read_only = .not. nilp(v)
        case ("text_mode_variant")
            call set_logical(text_mode_variant, v)
        case ("parse_sexp_ignore_comments")
            parse_sexp_ignore_comments = .not. nilp(v)
        case ("case_symbols_as_words")
            case_symbols_as_words = .not. nilp(v)
        case ("comment_end_can_be_escaped")
            comment_end_can_be_escaped = .not. nilp(v)
        case ("inhibit_modification_hooks")
            inhibit_modification_hooks = .not. nilp(v)
        case ("require_final_newline")
            call set_final_newline(require_final_newline, v)
        case ("mode_require_final_newline")
            call set_final_newline(mode_require_final_newline, v)
        case default
            ! Not a forwarded variable (resolve gives no such K).
        end select
    end subroutine fwd_set

    !> auto-fill-function := V: the procedure of the function V names, or
    !> a call by name.
    subroutine set_auto_fill(s, v)
        type(function_setting), intent(inout) :: s
        type(value_type), intent(in) :: v
        type(command_type), pointer :: c
        if (nilp(v)) then
            s%value => null()
            call settle(s)
            return
        end if
        c => null()
        if (symbolp(v)) c => find_command(symbols(xsymbol(v))%name)
        if (associated(c)) then
            s%value => c%run
            call settle(s)
        else
            s%value => auto_fill_by_name
            call keep_exact(s, v)
        end if
    end subroutine set_auto_fill

    !> indent-line-function := V.
    subroutine set_indent_line(s, v)
        type(indent_line_setting), intent(inout) :: s
        type(value_type), intent(in) :: v
        if (symbolp(v) .and. .not. nilp(v)) then
            call indent_line_function_named(symbols(xsymbol(v))%name, s%value)
            if (len(indent_line_function_name(s%value)) > 0) then
                call settle(s)
                return
            end if
        else
            call indent_line_function_named("", s%value)
        end if
        call keep_exact(s, v)
    end subroutine set_indent_line

    !> The value of a require-final-newline setting N: nil, t, `visit',
    !> `visit_save' or (for any other non-nil value) `ask'.
    function final_newline_value(n) result(v)
        integer(int64), intent(in) :: n
        type(value_type) :: v
        select case (n)
        case (FINAL_NEWLINE_NEVER)
            v = nil()
        case (FINAL_NEWLINE_SAVE)
            v = t_value()
        case (FINAL_NEWLINE_VISIT)
            v = sym("visit")
        case (FINAL_NEWLINE_VISIT_SAVE)
            v = sym("visit_save")
        case default
            v = sym("ask")
        end select
    end function final_newline_value

    !> Set require-final-newline setting S to V.
    subroutine set_final_newline(s, v)
        type(integer_setting), intent(inout) :: s
        type(value_type), intent(in) :: v
        if (nilp(v)) then
            s = FINAL_NEWLINE_NEVER
        else if (value_eq(v, t_value())) then
            s = FINAL_NEWLINE_SAVE
        else if (value_eq(v, sym("visit"))) then
            s = FINAL_NEWLINE_VISIT
        else if (value_eq(v, sym("visit_save"))) then
            s = FINAL_NEWLINE_VISIT_SAVE
        else
            s = FINAL_NEWLINE_ASK
            call keep_exact(s, v)
        end if
    end subroutine set_final_newline

    !> Forget S's exact value and voidness.
    subroutine settle(s)
        class(setting_type), intent(inout) :: s
        s%has_exact = .false.
        s%void = .false.
        s%exact = nil()
    end subroutine settle

    !> makunbound on forwarded variable K: Fset with Qunbound, which
    !> store_symval_forwarding checks against the variable's kind.
    subroutine fwd_makunbound(k, status)
        integer, intent(in) :: k
        type(status_type), intent(out) :: status
        type(minor_mode_type), pointer :: m
        status = status_type()
        if (k > NFWD) then
            m => minor_mode_at(k - NFWD)
            m%state%void = .true.
            return
        end if
        select case (trim(FWD_NAMES(k)))
        case ("fill_column", "left_margin", "tab_width")
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "integerp")
        case ("indent_tabs_mode")
            ! A DEFVAR_BOOL stores !NILP (Qunbound): t.
            indent_tabs_mode = .true.
        case ("completion_ignore_case")
            completion_ignore_case = .true.
        case ("major_mode")
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "symbolp")
        case ("mode_name", "buffer_read_only")
            continue
        case ("case_fold_search")
            case_fold_search%void = .true.
        case ("fill_prefix")
            fill_prefix%void = .true.
        case ("paragraph_start")
            paragraph_start%void = .true.
        case ("paragraph_separate")
            paragraph_separate%void = .true.
        case ("sentence_end")
            sentence_end%void = .true.
        case ("sentence_end_double_space")
            sentence_end_double_space%void = .true.
        case ("adaptive_fill_mode")
            adaptive_fill_mode%void = .true.
        case ("auto_fill_inhibit_regexp")
            auto_fill_inhibit_regexp%void = .true.
        case ("standard_indent")
            standard_indent%void = .true.
        case ("auto_fill_function")
            auto_fill_function%void = .true.
        case ("indent_line_function")
            indent_line_function%void = .true.
        case ("transient_mark_mode")
            transient_mark_mode%void = .true.
        case ("electric_indent_inhibit")
            electric_indent_inhibit%void = .true.
        case ("text_mode_variant")
            text_mode_variant%void = .true.
        case ("parse_sexp_ignore_comments")
            ! A DEFVAR_BOOL stores !NILP (Qunbound): t.
            parse_sexp_ignore_comments = .true.
        case ("case_symbols_as_words")
            ! A DEFVAR_BOOL stores !NILP (Qunbound): t.
            case_symbols_as_words = .true.
        case ("comment_end_can_be_escaped")
            ! A DEFVAR_BOOL stores !NILP (Qunbound): t.
            comment_end_can_be_escaped = .true.
        case ("inhibit_modification_hooks")
            ! A DEFVAR_BOOL stores !NILP (Qunbound): t.
            inhibit_modification_hooks = .true.
        case ("require_final_newline")
            require_final_newline%void = .true.
        case ("mode_require_final_newline")
            mode_require_final_newline%void = .true.
        case default
            continue
        end select
    end subroutine fwd_makunbound

    module procedure forwarded_get
    integer :: k
    logical :: is_bound
    k = resolve(id)
    found = k > 0
    is_bound = .false.
    if (found) then
        call fwd_get(k, cur, v, is_bound)
    else
        v = nil()
    end if
    if (present(bound)) bound = is_bound
    end procedure forwarded_get

    module procedure forwarded_makunbound
    integer :: k
    status = status_type()
    k = resolve(id)
    found = k > 0
    if (found) call fwd_makunbound(k, status)
    end procedure forwarded_makunbound

    module procedure forwarded_set
    integer :: k
    status = status_type()
    k = resolve(id)
    found = k > 0
    if (found) call fwd_set(k, v, status)
    end procedure forwarded_set

    module procedure auto_local
    integer :: k
    k = resolve(id)
    auto_local = symbols(id)%local_if_set
    end procedure auto_local

    ! ------------------------------------------------------------------
    ! Current values and the swap
    ! ------------------------------------------------------------------

    !> The current value of symbol ID.
    subroutine get_current(id, v, bound)
        integer(int64), intent(in) :: id
        type(value_type), intent(out) :: v
        logical, intent(out) :: bound
        integer :: k
        k = resolve(id)
        if (k > 0) then
            call fwd_get(k, cur, v, bound)
        else
            v = symbols(id)%val
            bound = symbols(id)%bound
        end if
    end subroutine get_current

    !> Set the current value of symbol ID (not procedure variables).
    subroutine put_current(id, v, bound)
        integer(int64), intent(in) :: id
        type(value_type), intent(in) :: v
        logical, intent(in) :: bound
        integer :: k
        type(status_type) :: st
        k = resolve(id)
        if (k > 0) then
            ! The value came from the same variable, so it is valid.
            if (bound) then
                call fwd_set(k, v, st)
            else
                call fwd_makunbound(k, st)
            end if
        else
            symbols(id)%val = v
            symbols(id)%bound = bound
        end if
    end subroutine put_current

    !> The index of symbol ID in buffer B's local variables, or 0.
    integer function local_index(b, id) result(k)
        integer(int64), intent(in) :: b, id
        integer :: i
        k = 0
        if (b <= 0) return
        if (.not. allocated(buffers(b)%local_ids)) return
        do i = 1, buffers(b)%nlocal
            if (buffers(b)%local_ids(i) == id) then
                k = i
                return
            end if
        end do
    end function local_index

    subroutine ensure_saved_procs(b)
        integer(int64), intent(in) :: b
        type(procs_type), allocatable :: tmp(:)
        integer(int64) :: i
        if (.not. allocated(saved_procs)) allocate (saved_procs(max(b, 16_int64)))
        if (size(saved_procs, kind=int64) < b) then
            allocate (tmp(max(b, 2*size(saved_procs, kind=int64))))
            do i = 1, size(saved_procs, kind=int64)
                tmp(i) = saved_procs(i)
            end do
            call move_alloc(tmp, saved_procs)
        end if
    end subroutine ensure_saved_procs

    !> Swap out the local values of buffer OLD and swap in those of NEW.
    !> Port of data.c:swap_in_symval_forwarding, for all of a buffer's
    !> local variables at once.
    subroutine swap_locals(old, new)
        integer(int64), intent(in) :: old, new
        integer :: i
        integer(int64) :: id
        if (old > 0) then
            do i = 1, buffers(old)%nlocal
                id = buffers(old)%local_ids(i)
                select case (kind_of(id))
                case (K_AUTO_FILL)
                    saved_procs(old)%auto_fill = auto_fill_function
                    auto_fill_function = default_procs%auto_fill
                case (K_INDENT_LINE)
                    saved_procs(old)%indent_line = indent_line_function
                    indent_line_function = default_procs%indent_line
                case default
                    call get_current(id, buffers(old)%local_values(i), buffers(old)%local_bound(i))
                    call put_current(id, symbols(id)%default_val, symbols(id)%default_bound)
                end select
            end do
        end if
        if (new > 0) then
            do i = 1, buffers(new)%nlocal
                id = buffers(new)%local_ids(i)
                select case (kind_of(id))
                case (K_AUTO_FILL)
                    default_procs%auto_fill = auto_fill_function
                    auto_fill_function = saved_procs(new)%auto_fill
                case (K_INDENT_LINE)
                    default_procs%indent_line = indent_line_function
                    indent_line_function = saved_procs(new)%indent_line
                case default
                    call get_current(id, symbols(id)%default_val, symbols(id)%default_bound)
                    call put_current(id, buffers(new)%local_values(i), buffers(new)%local_bound(i))
                end select
            end do
        end if
    end subroutine swap_locals

    !> Restore the default value of symbol ID, local in the current
    !> buffer at index I, and forget the local value.
    subroutine drop_local(i)
        integer, intent(in) :: i
        integer(int64) :: id
        integer :: n
        id = buffers(cur)%local_ids(i)
        select case (kind_of(id))
        case (K_AUTO_FILL)
            auto_fill_function = default_procs%auto_fill
        case (K_INDENT_LINE)
            indent_line_function = default_procs%indent_line
        case default
            call put_current(id, symbols(id)%default_val, symbols(id)%default_bound)
        end select
        n = buffers(cur)%nlocal
        buffers(cur)%local_ids(i:n - 1) = buffers(cur)%local_ids(i + 1:n)
        buffers(cur)%local_values(i:n - 1) = buffers(cur)%local_values(i + 1:n)
        buffers(cur)%local_bound(i:n - 1) = buffers(cur)%local_bound(i + 1:n)
        buffers(cur)%nlocal = n - 1
    end subroutine drop_local

    !> Check that VARIABLE is a symbol that can be made local.
    subroutine check_localizable(variable, id, status)
        type(value_type), intent(in) :: variable
        integer(int64), intent(out) :: id
        type(status_type), intent(out) :: status
        id = 0
        call ensure_obarray()
        call check_symbol(variable, status)
        if (.not. status%ok()) return
        id = xsymbol(variable)
        if (symbols(id)%constant) call set_error(status, FM_SETTING_CONSTANT, symbols(id)%name)
    end subroutine check_localizable

    ! ------------------------------------------------------------------
    ! The Lisp-level functions
    ! ------------------------------------------------------------------

    module procedure local_in_current
    if (kind_of(id) == K_SLOT) then
        local_in_current = .true.
    else
        local_in_current = local_index(cur, id) > 0
    end if
    end procedure local_in_current

    module procedure make_local_variable
    integer(int64) :: id
    integer :: n
    type(value_type), allocatable :: values(:)
    integer(int64), allocatable :: ids(:)
    logical, allocatable :: bound(:)
    call check_localizable(variable, id, status)
    if (.not. status%ok()) return
    if (cur == 0) return
    if (local_in_current(id)) return
    swap_buffer_locals_hook => swap_locals
    n = buffers(cur)%nlocal
    if (.not. allocated(buffers(cur)%local_ids)) then
        allocate (buffers(cur)%local_ids(8), buffers(cur)%local_values(8), buffers(cur)%local_bound(8))
    else if (n == size(buffers(cur)%local_ids)) then
        allocate (ids(2*n), values(2*n), bound(2*n))
        ids(1:n) = buffers(cur)%local_ids(1:n)
        values(1:n) = buffers(cur)%local_values(1:n)
        bound(1:n) = buffers(cur)%local_bound(1:n)
        call move_alloc(ids, buffers(cur)%local_ids)
        call move_alloc(values, buffers(cur)%local_values)
        call move_alloc(bound, buffers(cur)%local_bound)
    end if
    n = n + 1
    buffers(cur)%nlocal = n
    buffers(cur)%local_ids(n) = id
    ! The local value starts out as the current value, which becomes the
    ! default; the local value lives in the variable while CUR is current.
    select case (kind_of(id))
    case (K_AUTO_FILL)
        call ensure_saved_procs(cur)
        default_procs%auto_fill = auto_fill_function
    case (K_INDENT_LINE)
        call ensure_saved_procs(cur)
        default_procs%indent_line = indent_line_function
    case default
        call get_current(id, symbols(id)%default_val, symbols(id)%default_bound)
    end select
    end procedure make_local_variable

    module procedure make_variable_buffer_local
    integer(int64) :: id
    call check_localizable(variable, id, status)
    if (.not. status%ok()) return
    symbols(id)%local_if_set = .true.
    ! A void variable gets the default value nil.
    if (kind_of(id) == 0 .and. .not. local_in_current(id)) then
        if (.not. symbols(id)%bound) then
            symbols(id)%val = nil()
            symbols(id)%bound = .true.
        end if
    end if
    end procedure make_variable_buffer_local

    module procedure kill_local_variable
    integer(int64) :: id
    integer :: i
    call check_localizable(variable, id, status)
    if (.not. status%ok()) return
    i = local_index(cur, id)
    if (i > 0) call drop_local(i)
    end procedure kill_local_variable

    module procedure kill_all_local_variables
    logical :: all
    integer :: i
    type(value_type) :: prop
    type(status_type) :: st
    if (cur == 0) return
    ! The first thing this function does is run the normal hook
    ! `change_major_mode_hook'.
    call change_major_mode_hook%run(st)
    all = .false.
    if (present(kill_permanent)) all = kill_permanent
    call kill_local_hooks(all)
    call kill_delayed_mode_hooks()
    buffers(cur)%syntax_table = null_handle()
    buffers(cur)%major_mode = "fundamental_mode"
    buffers(cur)%mode_name = "Fundamental"
    buffers(cur)%keymap = null_handle()
    do i = buffers(cur)%nlocal, 1, -1
        if (.not. all) then
            call get(make_symbol_ref(buffers(cur)%local_ids(i)), sym("permanent_local"), prop, st)
            if (.not. nilp(prop)) cycle
        end if
        call drop_local(i)
    end do
    ! The buffer-local variables kept in Fortran objects (none of them
    ! permanent).
    if (allocated(killers)) then
        do i = 1, size(killers)
            call killers(i)%fn()
        end do
    end if
    end procedure kill_all_local_variables

    module procedure local_variable_p
    integer(int64) :: b, id
    local_variable_p = .false.
    call ensure_obarray()
    if (.not. symbolp(variable)) return
    id = xsymbol(variable)
    b = cur
    if (present(buffer)) b = buffer
    if (kind_of(id) == K_SLOT) then
        local_variable_p = .true.
    else
        local_variable_p = local_index(b, id) > 0
    end if
    end procedure local_variable_p

    module procedure local_variable_if_set_p
    local_variable_if_set_p = local_variable_p(variable, buffer)
    if (.not. local_variable_if_set_p .and. symbolp(variable)) local_variable_if_set_p = auto_local(xsymbol(variable))
    end procedure local_variable_if_set_p

    !> The default value of symbol ID.
    subroutine get_default(id, v, bound)
        integer(int64), intent(in) :: id
        type(value_type), intent(out) :: v
        logical, intent(out) :: bound
        bound = .true.
        select case (kind_of(id))
        case (K_SLOT)
            call fwd_get(resolve(id), 0_int64, v, bound)
        case (K_AUTO_FILL)
            if (local_in_current(id)) then
                call setting_value(default_procs%auto_fill, auto_fill_value(default_procs%auto_fill%value), v, bound)
            else
                call get_current(id, v, bound)
            end if
        case (K_INDENT_LINE)
            if (local_in_current(id)) then
                call setting_value(default_procs%indent_line, indent_line_value(default_procs%indent_line%value), &
                                   v, bound)
            else
                call get_current(id, v, bound)
            end if
        case default
            if (local_in_current(id)) then
                v = symbols(id)%default_val
                bound = symbols(id)%default_bound
            else
                call get_current(id, v, bound)
            end if
        end select
    end subroutine get_default

    module procedure default_value
    integer(int64) :: id
    logical :: bound
    call ensure_obarray()
    call check_symbol(symbol, status)
    if (.not. status%ok()) return
    id = xsymbol(symbol)
    call get_default(id, v, bound)
    if (.not. bound) call set_error(status, FM_VOID_VARIABLE, symbols(id)%name)
    end procedure default_value

    module procedure default_boundp
    type(value_type) :: v
    default_boundp = .false.
    call ensure_obarray()
    if (.not. symbolp(symbol)) return
    call get_default(xsymbol(symbol), v, default_boundp)
    end procedure default_boundp

    module procedure set_default
    integer(int64) :: id
    integer :: k
    logical :: found
    call check_localizable(symbol, id, status)
    if (.not. status%ok()) return
    k = resolve(id)
    if (.not. local_in_current(id)) then
        call forwarded_set(id, v, status, found)
        if (found) return
        symbols(id)%val = v
        symbols(id)%bound = .true.
        return
    end if
    select case (kind_of(id))
    case (K_SLOT)
        ! The defaults of per-buffer slots are fixed.
    case (K_AUTO_FILL)
        call set_auto_fill(default_procs%auto_fill, v)
    case (K_INDENT_LINE)
        call set_indent_line(default_procs%indent_line, v)
    case (0)
        symbols(id)%default_val = v
        symbols(id)%default_bound = .true.
    case default
        ! Check the type by storing into the local value and back.
        block
            type(value_type) :: here
            logical :: bound
            call get_current(id, here, bound)
            call fwd_set(k, v, status)
            if (status%ok()) then
                symbols(id)%default_val = v
                symbols(id)%default_bound = .true.
            end if
            call put_current(id, here, bound)
        end block
    end select
    end procedure set_default

    module procedure buffer_local_value
    integer(int64) :: id
    integer :: i
    logical :: bound
    call ensure_obarray()
    call check_symbol(variable, status)
    if (.not. status%ok()) return
    id = xsymbol(variable)
    if (buffer <= 0 .or. buffer > size(buffers, kind=int64)) then
        call set_error(status, FM_WRONG_TYPE_ARGUMENT, "bufferp")
        return
    end if
    if (.not. buffer_live_p(buffer_handle(buffer))) then
        call set_error(status, FM_WRONG_TYPE_ARGUMENT, "bufferp")
        return
    end if
    bound = .true.
    if (buffer == cur) then
        call get_current(id, v, bound)
    else if (kind_of(id) == K_SLOT) then
        call fwd_get(resolve(id), buffer, v, bound)
    else
        i = local_index(buffer, id)
        if (i == 0) then
            call get_default(id, v, bound)
        else
            select case (kind_of(id))
            case (K_AUTO_FILL)
                call setting_value(saved_procs(buffer)%auto_fill, auto_fill_value(saved_procs(buffer)%auto_fill%value), &
                                   v, bound)
            case (K_INDENT_LINE)
                call setting_value(saved_procs(buffer)%indent_line, &
                                   indent_line_value(saved_procs(buffer)%indent_line%value), v, bound)
            case default
                v = buffers(buffer)%local_values(i)
                bound = buffers(buffer)%local_bound(i)
            end select
        end if
    end if
    if (.not. bound) call set_error(status, FM_VOID_VARIABLE, symbols(id)%name)
    end procedure buffer_local_value

    module procedure buffer_local_variables
    integer(int64) :: b, id
    integer :: i, n
    type(value_type), allocatable :: elts(:)
    type(value_type) :: v
    type(status_type) :: st
    character(len=*), parameter :: SLOTS(3) = [character(len=16) :: "buffer_read_only", "major_mode", "mode_name"]
    b = cur
    if (present(buffer)) b = buffer
    alist = nil()
    if (b <= 0) return
    allocate (elts(buffers(b)%nlocal + size(SLOTS)))
    n = 0
    do i = 1, buffers(b)%nlocal
        id = buffers(b)%local_ids(i)
        call buffer_local_value(make_symbol_ref(id), b, v, st)
        n = n + 1
        if (st%ok()) then
            elts(n) = list(make_symbol_ref(id), v)
        else
            elts(n) = make_symbol_ref(id)
        end if
    end do
    do i = 1, size(SLOTS)
        call buffer_local_value(sym(trim(SLOTS(i))), b, v, st)
        n = n + 1
        elts(n) = list(sym(trim(SLOTS(i))), v)
    end do
    alist = make_list(elts(1:n))
    end procedure buffer_local_variables

    module procedure setq_local
    type(value_type) :: s
    s = sym(variable)
    call make_local_variable(s, status)
    if (status%ok()) call set(s, v, status)
    end procedure setq_local
end submodule data_buffer_fm
