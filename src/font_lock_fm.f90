! font_lock_fm.f90 -- Font Lock: fontification of buffer text.
! Ported from GNU Emacs 31.1 lisp/font-lock.el.
!
! Copyright (C) 1992-2026 Free Software Foundation, Inc.
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

!> Font Lock fontifies buffer text: syntactically (strings and comments,
!> from the syntax table) and by keywords (regexps or functions, each
!> with the faces to give the parts of its matches), as the major mode's
!> `font_lock_defaults' says.  With the default support mode
!> (Just-in-time Lock, jit_lock_fm) text is fontified as redisplay is
!> about to show it; otherwise the whole buffer at once, and the changed
!> lines after each change.  This module also defines the faces of
!> font-lock.el.
!>
!> The keywords of a buffer (`font_lock_keywords') are
!> `font_lock_keyword_type' values (font_core_fm), built with
!> `font_lock_keyword', `font_lock_highlight' and `font_lock_anchored'.
!>
!> Deferred: font-lock-syntactic-keywords and syntax-propertize,
!> font-lock-multiline, comment delimiters in
!> `font_lock_comment_delimiter_face' (needs newcomment.el's
!> comment-start-skip), the OTHER-VARS of `font_lock_defaults' and the
!> function variables a mode would set with them
!> (font-lock-fontify-region-function and its companions, which are always
!> the defaults here, font-lock-extend-region-functions,
!> font-lock-extend-after-change-region-function,
!> font-lock-mark-block-function), font-lock-remove-keywords,
!> font-lock-ignore, keywords given as forms to evaluate, faces given as
!> (face FACE PROP VAL...), the mode alist forms of
!> `font_lock_maximum_decoration' and `font_lock_verbose', the
!> toplevel-defun warning keyword (syntax-begin-function), the obsolete
!> face variables (font-lock-comment-face as a variable: not applicable).
module font_lock_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, nilp, t_value, list, consp, symbolp, symbols, xsymbol, list_elements, make_list
    use lread_fm, only: sym
    use data_fm, only: symbol_value, set, add_local_variables_killer, value_eq
    use buffer_fm, only: buffers, cur, current_buffer, buffer_modified_p, restore_buffer_modified_p
    use marker_fm, only: make_marker, free_marker, set_marker, marker_position
    use editfns_fm, only: point, goto_char, point_min, point_max, widen, narrow_to_region, line_beginning_position, &
                          line_end_position
    use cmds_fm, only: forward_line, forward_char
    use search_fm, only: re_search_forward, match_beginning, match_end, match_data, set_match_data, case_fold_search, &
                         SEARCH_NOERROR
    use textprop_fm, only: put_text_property, get_text_property, next_single_property_change, &
                           remove_list_of_text_properties, text_property_any, text_property_not_all
    use syntax_fm, only: syntax_table, set_syntax_table, copy_syntax_table, modify_syntax_entry
    use derived_fm, only: derived_mode_p
    use faces_fm, only: init_faces, named_face_type, display_when, face_attrs, FACE_CLASS_COLOR, FACE_CLASS_GRAYSCALE, &
                        FACE_BACKGROUND_LIGHT, FACE_BACKGROUND_DARK
    use font_core_fm, only: font_lock_defaults, font_lock_defaults_type, font_lock_keyword_type, &
                            font_lock_highlight_type, font_lock_match_highlight_type, font_lock_keywords_type, &
                            font_lock_mode_internal_proc, font_lock_specified_p_proc, font_lock_initial_fontify_proc, &
                            font_lock_mode_enabled, FONT_LOCK_OVERRIDE_NIL, FONT_LOCK_OVERRIDE_T, &
                            FONT_LOCK_OVERRIDE_KEEP, FONT_LOCK_OVERRIDE_PREPEND, FONT_LOCK_OVERRIDE_APPEND, &
                            font_lock_face_proc, font_lock_matcher_proc, font_lock_form_proc, major_mode_of_current
    use syntax_fm, only: parse_state_type, parse_partial_sexp, syntax_ppss, COMMENTSTOP_SYNTAX_TABLE
    use subr_fm, only: silent_modifications_type, silent_modifications_begin, silent_modifications_end
    use insdel_fm, only: after_change_functions
    use jit_lock_fm, only: jit_lock_register, jit_lock_unregister, jit_lock_refontify, jit_lock_fontify_now, &
                           jit_lock_after_change_extend_region_functions, jit_lock_start, jit_lock_end, jit_lock_mode_p
    use textprop_fm, only: previous_single_property_change
    use error_fm, only: format_error
    use modes_fm, only: minor_modes
    use commands_fm, only: commands
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use xdisp_fm, only: message
    implicit none(type, external)
    private
    public :: syms_of_font_lock, font_lock_specified_p, font_lock_mode_internal, font_lock_set_defaults, &
              font_lock_fontify_buffer, font_lock_unfontify_buffer, font_lock_fontify_region, font_lock_unfontify_region, &
              font_lock_flush, font_lock_ensure, font_lock_update, font_lock_default_fontify_region, &
              font_lock_default_unfontify_region, font_lock_fontify_keywords_region, font_lock_after_change_function, &
              font_lock_add_keywords, font_lock_refresh_defaults, font_lock_prepend_text_property, &
              font_lock_append_text_property, font_lock_fillin_text_property, &
              font_lock_remove_face_from_text_property, font_lock_keyword, font_lock_highlight, &
              font_lock_anchored, font_lock_fontified, font_lock_keywords_of_buffer

    !> `font_lock_maximum_decoration' for the maximum decoration available
    !> (upstream's t).
    integer, parameter, public :: FONT_LOCK_DECORATION_MAX = huge(0)

    !> Maximum decoration level for fontification.
    !> If 0, use the default decoration (typically the minimum available).
    !> If FONT_LOCK_DECORATION_MAX (the default), use the maximum decoration
    !> available.
    !> If a number, use that level of decoration (or if not available the
    !> maximum).  The higher the number, the more decoration is done.
    !> Port of font-lock.el:font-lock-maximum-decoration (the alist form,
    !> by major mode, is deferred).
    integer, public :: font_lock_maximum_decoration = FONT_LOCK_DECORATION_MAX

    !> If true, means show status messages for buffer fontification.  Port
    !> of font-lock.el:font-lock-verbose (the size threshold form is
    !> deferred).
    logical, public :: font_lock_verbose = .false.

    !> If true, font-lock will work on the non-widened buffer.
    !> Useful for things like RMAIL and Info where the whole buffer is not
    !> a very meaningful entity to highlight.  Port of
    !> font-lock.el:font-lock-dont-widen.
    logical, public :: font_lock_dont_widen = .false.

    !> Additional text properties managed by font-lock.
    !> This is used by `font_lock_default_unfontify_region' to decide
    !> what properties to clear before refontifying a region.  Port of
    !> font-lock.el:font-lock-extra-managed-props.
    character(len=64), allocatable, public :: font_lock_extra_managed_props(:)

    !> Support modes for Font Lock mode (`font_lock_support_mode'): none, or
    !> Just-in-time Lock mode.
    integer, parameter, public :: FONT_LOCK_SUPPORT_NONE = 0, FONT_LOCK_SUPPORT_JIT_LOCK = 1

    !> Support mode for Font Lock mode.
    !> If FONT_LOCK_SUPPORT_NONE, means support for Font Lock mode is never
    !> performed.  This can be useful for debugging.
    !>
    !> The value of this variable is used when Font Lock mode is turned on.
    !> Port of font-lock.el:font-lock-support-mode.
    integer, public :: font_lock_support_mode = FONT_LOCK_SUPPORT_JIT_LOCK

    !> Keywords a major mode gets in addition to (or instead of) its own
    !> (`font_lock_add_keywords').
    type :: added_keywords_type
        character(len=64) :: mode = ""
        type(font_lock_keyword_type), allocatable :: keywords(:)
        !> HOW: 0 at the beginning, 1 at the end, 2 replacing (`set').
        integer :: how = 0
    end type added_keywords_type

    !> Where `font_lock_add_keywords' puts keywords: at the beginning, at
    !> the end, or instead of the existing ones.
    integer, parameter, public :: FONT_LOCK_ADD_BEGINNING = 0, FONT_LOCK_ADD_END = 1, FONT_LOCK_ADD_SET = 2

    !> Alist of additional `font_lock_keywords' elements for major modes.
    !>
    !> Each element has the form (MODE KEYWORDS . HOW).
    !> Function `font_lock_set_defaults' adds the elements in the list
    !> KEYWORDS to `font_lock_keywords' when Font Lock is turned on in
    !> major mode MODE.
    !>
    !> This is normally set via `font_lock_add_keywords'.  Port of
    !> font-lock.el:font-lock-keywords-alist.
    type(added_keywords_type), allocatable :: font_lock_keywords_alist(:)

    !> The buffer-local variables of font-lock.el, by buffer.
    type :: locals_type
        integer(int64) :: serial = 0
        !> `font_lock_keywords' (set for this buffer).
        logical :: keywords_set = .false.
        type(font_lock_keyword_type), allocatable :: keywords(:)
        !> `font_lock_keywords_only'.
        logical :: keywords_only = .false.
        !> `font_lock_keywords_case_fold_search'.
        logical :: keywords_case_fold_search = .false.
        !> `font_lock_syntax_table' (the null handle for nil) and
        !> `font_lock__syntax_table_affects_ppss'.
        type(handle_type) :: syntax_table
        logical :: syntax_table_affects_ppss = .false.
        !> `font_lock_set_defaults'.
        logical :: set_defaults = .false.
        !> `font_lock_major_mode'.
        character(len=:), allocatable :: major_mode
        !> `font_lock_fontified'.
        logical :: fontified = .false.
        !> Jit-lock's local values of `font_lock_flush_function',
        !> `font_lock_ensure_function' and `font_lock_fontify_buffer_function'
        !> (font-lock-turn-on-thing-lock).
        logical :: jit_lock = .false.
    end type locals_type

    type(locals_type), allocatable :: locals(:)

    abstract interface
        !> `font_lock_syntactic_face_function': FACE gets the name of the
        !> face to give the string or comment whose parse state (at its
        !> start) is STATE ("" for none).
        subroutine syntactic_face_iface(state, face)
            import :: parse_state_type
            implicit none(type, external)
            type(parse_state_type), intent(in) :: state
            character(len=:), allocatable, intent(out) :: face
        end subroutine syntactic_face_iface
    end interface

    !> Function to determine which face to use when fontifying
    !> syntactically.  The function is called with a single parameter (the
    !> state as returned by `parse_partial_sexp' at the beginning of the
    !> region to highlight) and should give the name of a face.  This is
    !> normally set via `font_lock_defaults'.  Port of
    !> font-lock.el:font-lock-syntactic-face-function.
    procedure(syntactic_face_iface), pointer, public :: font_lock_syntactic_face_function => null()

    !> Font Lock mode face used to highlight comments.  Port of
    !> font-lock.el:font-lock-comment-face.
    type(named_face_type), target, public :: font_lock_comment_face
    !> Font Lock mode face used to highlight comment delimiters.  Port of
    !> font-lock.el:font-lock-comment-delimiter-face.
    type(named_face_type), target, public :: font_lock_comment_delimiter_face
    !> Font Lock mode face used to highlight strings.  Port of
    !> font-lock.el:font-lock-string-face.
    type(named_face_type), target, public :: font_lock_string_face
    !> Font Lock mode face used to highlight documentation embedded in
    !> program code.  It is typically used for special documentation
    !> comments or strings.  Port of font-lock.el:font-lock-doc-face.
    type(named_face_type), target, public :: font_lock_doc_face
    !> Font Lock mode face used to highlight embedded documentation
    !> mark-up.  It is meant for mark-up elements in text that uses
    !> `font_lock_doc_face', such as the constructs of Haddock, Javadoc and
    !> similar systems.  Port of font-lock.el:font-lock-doc-markup-face.
    type(named_face_type), target, public :: font_lock_doc_markup_face
    !> Font Lock mode face used to highlight keywords.  Port of
    !> font-lock.el:font-lock-keyword-face.
    type(named_face_type), target, public :: font_lock_keyword_face
    !> Font Lock mode face used to highlight builtins.  Port of
    !> font-lock.el:font-lock-builtin-face.
    type(named_face_type), target, public :: font_lock_builtin_face
    !> Font Lock mode face used to highlight function names.  Port of
    !> font-lock.el:font-lock-function-name-face.
    type(named_face_type), target, public :: font_lock_function_name_face
    !> Font Lock mode face used to highlight function calls.  Port of
    !> font-lock.el:font-lock-function-call-face.
    type(named_face_type), target, public :: font_lock_function_call_face
    !> Font Lock mode face used to highlight variable names.  Port of
    !> font-lock.el:font-lock-variable-name-face.
    type(named_face_type), target, public :: font_lock_variable_name_face
    !> Font Lock mode face used to highlight variable references.  Port
    !> of font-lock.el:font-lock-variable-use-face.
    type(named_face_type), target, public :: font_lock_variable_use_face
    !> Font Lock mode face used to highlight type and class names.  Port
    !> of font-lock.el:font-lock-type-face.
    type(named_face_type), target, public :: font_lock_type_face
    !> Font Lock mode face used to highlight constants and labels.  Port
    !> of font-lock.el:font-lock-constant-face.
    type(named_face_type), target, public :: font_lock_constant_face
    !> Font Lock mode face used to highlight warnings.  Port of
    !> font-lock.el:font-lock-warning-face.
    type(named_face_type), target, public :: font_lock_warning_face
    !> Font Lock mode face used to highlight easy to overlook negation.
    !> Port of font-lock.el:font-lock-negation-char-face.
    type(named_face_type), target, public :: font_lock_negation_char_face
    !> Font Lock mode face used to highlight preprocessor directives.
    !> Port of font-lock.el:font-lock-preprocessor-face.
    type(named_face_type), target, public :: font_lock_preprocessor_face
    !> Font Lock mode face used to highlight regexp literals.  Port of
    !> font-lock.el:font-lock-regexp-face.
    type(named_face_type), target, public :: font_lock_regexp_face
    !> Font Lock mode face for backslashes in regexp grouping constructs.
    !> Port of font-lock.el:font-lock-regexp-grouping-backslash.
    type(named_face_type), target, public :: font_lock_regexp_grouping_backslash
    !> Font Lock mode face used to highlight grouping constructs in
    !> regexps.  Port of font-lock.el:font-lock-regexp-grouping-construct.
    type(named_face_type), target, public :: font_lock_regexp_grouping_construct
    !> Font Lock mode face used to highlight escape sequences in strings.
    !> Port of font-lock.el:font-lock-escape-face.
    type(named_face_type), target, public :: font_lock_escape_face
    !> Font Lock mode face used to highlight numbers.  Port of
    !> font-lock.el:font-lock-number-face.
    type(named_face_type), target, public :: font_lock_number_face
    !> Font Lock mode face used to highlight operators.  Port of
    !> font-lock.el:font-lock-operator-face.
    type(named_face_type), target, public :: font_lock_operator_face
    !> Font Lock mode face used to highlight properties of an object.
    !> For example, the declaration of fields in a struct.  Port of
    !> font-lock.el:font-lock-property-name-face.
    type(named_face_type), target, public :: font_lock_property_name_face
    !> Font Lock mode face used to highlight property references.
    !> For example, property lookup of fields in a struct.  Port of
    !> font-lock.el:font-lock-property-use-face.
    type(named_face_type), target, public :: font_lock_property_use_face
    !> Font Lock mode face used to highlight punctuation characters.  Port
    !> of font-lock.el:font-lock-punctuation-face.
    type(named_face_type), target, public :: font_lock_punctuation_face
    !> Font Lock mode face used to highlight brackets, braces, and parens.
    !> Port of font-lock.el:font-lock-bracket-face.
    type(named_face_type), target, public :: font_lock_bracket_face
    !> Font Lock mode face used to highlight delimiters.
    !> What exactly is a delimiter depends on the major mode, but usually
    !> these are characters like comma, colon, and semi-colon.  Port of
    !> font-lock.el:font-lock-delimiter-face.
    type(named_face_type), target, public :: font_lock_delimiter_face
    !> Font Lock mode face used to highlight miscellaneous punctuation.
    !> Port of font-lock.el:font-lock-misc-punctuation-face.
    type(named_face_type), target, public :: font_lock_misc_punctuation_face

contains

    ! ------------------------------------------------------------------
    ! Building keywords
    ! ------------------------------------------------------------------

    !> A MATCH-HIGHLIGHT: give subexpression SUBEXP of the match the face
    !> FACE (or the face FACE_FUNCTION names), combined with existing
    !> fontification as OVERRIDE says (FONT_LOCK_OVERRIDE_NIL by default),
    !> LAXMATCH if SUBEXP need not match.
    function font_lock_highlight(subexp, face, override, laxmatch, face_function, true_properties) result(h)
        integer, intent(in) :: subexp
        character(len=*), intent(in), optional :: face
        integer, intent(in), optional :: override
        logical, intent(in), optional :: laxmatch
        procedure(font_lock_face_proc), optional :: face_function
        !> Text properties to set to true along with the face.
        character(len=*), intent(in), optional :: true_properties(:)
        type(font_lock_highlight_type) :: h
        integer :: k
        h%match%subexp = subexp
        if (present(true_properties)) then
            allocate (h%match%true_properties(size(true_properties)))
            do k = 1, size(true_properties)
                h%match%true_properties(k) = true_properties(k)
            end do
        end if
        if (present(face)) h%match%face = face
        if (present(override)) h%match%override = override
        if (present(laxmatch)) h%match%laxmatch = laxmatch
        if (present(face_function)) h%match%face_function => face_function
    end function font_lock_highlight

    !> A MATCH-ANCHORED highlight: REGEXP (or MATCHER) searched for after
    !> each match of the keyword, each match highlighted as HIGHLIGHTS say
    !> (their `match' parts).
    function font_lock_anchored(highlights, regexp, matcher, pre_match_form, post_match_form) result(h)
        type(font_lock_highlight_type), intent(in) :: highlights(:)
        character(len=*), intent(in), optional :: regexp
        procedure(font_lock_matcher_proc), optional :: matcher
        procedure(font_lock_form_proc), optional :: pre_match_form, post_match_form
        type(font_lock_highlight_type) :: h
        integer :: k
        h%anchored = .true.
        if (present(regexp)) h%regexp = regexp
        if (present(matcher)) h%matcher => matcher
        if (present(pre_match_form)) h%pre_match_form => pre_match_form
        if (present(post_match_form)) h%post_match_form => post_match_form
        allocate (h%highlights(size(highlights)))
        do k = 1, size(highlights)
            h%highlights(k) = highlights(k)%match
        end do
    end function font_lock_anchored

    !> A keyword: REGEXP (or the function MATCHER), with HIGHLIGHTS; with
    !> only FACE, subexpression 0 (or SUBEXP) in FACE; with neither,
    !> subexpression 0 (or SUBEXP) in `font_lock_keyword_face'
    !> (font-lock-compile-keyword's forms MATCHER, (MATCHER . SUBEXP),
    !> (MATCHER . FACENAME)).
    function font_lock_keyword(regexp, highlights, face, subexp, matcher) result(k)
        character(len=*), intent(in), optional :: regexp
        type(font_lock_highlight_type), intent(in), optional :: highlights(:)
        character(len=*), intent(in), optional :: face
        integer, intent(in), optional :: subexp
        procedure(font_lock_matcher_proc), optional :: matcher
        type(font_lock_keyword_type) :: k
        integer :: n
        if (present(regexp)) k%regexp = regexp
        if (present(matcher)) k%matcher => matcher
        if (present(highlights)) then
            ! (Allocated first: gfortran's reallocating assignment reads
            ! uninitialized memory here.)
            allocate (k%highlights(size(highlights)))
            k%highlights(:) = highlights(:)
        else
            n = 0
            if (present(subexp)) n = subexp
            allocate (k%highlights(1))
            k%highlights(1)%match%subexp = n
            if (present(face)) then
                k%highlights(1)%match%face = face
            else
                k%highlights(1)%match%face = "font_lock_keyword_face"
            end if
        end if
    end function font_lock_keyword

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    !> The index of the current buffer's entry in `locals', made if MAKE.
    integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(locals_type), allocatable :: grown(:)
        integer :: i, n
        k = 0
        if (cur == 0) return
        if (.not. allocated(locals)) allocate (locals(0))
        do i = 1, size(locals)
            if (locals(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do
        if (.not. make) return
        do i = 1, size(locals)
            if (locals(i)%serial == 0) then
                locals(i)%serial = buffers(cur)%serial
                k = i
                return
            end if
        end do
        n = size(locals)
        allocate (grown(n + 1))
        grown(1:n) = locals
        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, locals)
        k = n + 1
    end function local_index

    !> kill-all-local-variables: forget the current buffer's font-lock.el
    !> variables.
    subroutine kill_font_lock_locals()
        integer :: k
        k = local_index(.false.)
        if (k == 0) return
        locals(k) = locals_type()
    end subroutine kill_font_lock_locals

    !> Whether the current buffer's `font_lock_fontified' is true.
    logical function font_lock_fontified()
        integer :: k
        k = local_index(.false.)
        font_lock_fontified = .false.
        if (k > 0) font_lock_fontified = locals(k)%fontified
    end function font_lock_fontified

    !> The current buffer's `font_lock_keywords' (empty if not set).
    function font_lock_keywords_of_buffer() result(keywords)
        type(font_lock_keyword_type), allocatable :: keywords(:)
        integer :: k
        k = local_index(.false.)
        allocate (keywords(0))
        if (k == 0) return
        if (locals(k)%keywords_set) keywords = locals(k)%keywords
    end function font_lock_keywords_of_buffer

    ! ------------------------------------------------------------------
    ! Turning Font Lock on and off
    ! ------------------------------------------------------------------

    !> Return true if the current buffer is ready for fontification.
    !> The MODE argument, if true, means Font Lock mode is about to
    !> be enabled.  Port of font-lock.el:font-lock-specified-p.
    logical function font_lock_specified_p(mode) result(p)
        logical, intent(in) :: mode
        integer :: k
        p = font_lock_defaults%present_p()
        if (p) return
        k = local_index(.false.)
        if (k == 0) return
        if (locals(k)%keywords_set) then
            p = size(locals(k)%keywords) > 0
            if (p) return
        end if
        if (mode .and. locals(k)%set_defaults .and. allocated(locals(k)%major_mode)) then
            if (len(locals(k)%major_mode) > 0) p = locals(k)%major_mode /= major_mode_of_current()
        end if
    end function font_lock_specified_p

    !> The first fontification after turning the mode on.  This must only
    !> be called after the mode hooks have been run.  Port of
    !> font-lock.el:font-lock-initial-fontify.
    subroutine font_lock_initial_fontify(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. font_lock_mode_enabled()) return
        if (.not. font_lock_specified_p(.true.)) return
        if (font_lock_fontified()) return
        call font_lock_fontify_buffer(status)
    end subroutine font_lock_initial_fontify

    !> Turn the fontification of Font Lock on (ARG) or off in the current
    !> buffer.  Port of font-lock.el:font-lock-mode-internal.
    subroutine font_lock_mode_internal(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        status = status_type()
        ! Turn on Font Lock mode.
        if (arg) then
            call add_after_change_function()
            call font_lock_set_defaults(status)
            if (.not. status%ok()) return
            call font_lock_turn_on_thing_lock(status)
            if (.not. status%ok()) return
        end if
        ! Turn off Font Lock mode.
        if (.not. font_lock_mode_enabled()) then
            call remove_after_change_function()
            call font_lock_unfontify_buffer(status)
            if (.not. status%ok()) return
            call font_lock_turn_off_thing_lock(status)
        end if
    end subroutine font_lock_mode_internal

    !> Arrange for the current buffer to be refontified after changes
    !> (font-lock-mode-internal's after-change-functions part).
    subroutine add_after_change_function()
        call after_change_functions%add(font_lock_after_change_function, depth=90, local=.true.)
    end subroutine add_after_change_function

    !> Stop refontifying the current buffer after changes.
    subroutine remove_after_change_function()
        call after_change_functions%remove(font_lock_after_change_function, local=.true.)
    end subroutine remove_after_change_function

    !> Turn on the support mode (`font_lock_support_mode'): with
    !> Just-in-time Lock, fontification happens as redisplay needs it, and
    !> the flush, ensure and fontify-buffer functions are Just-in-time
    !> Lock's.  Port of font-lock.el:font-lock-turn-on-thing-lock.
    subroutine font_lock_turn_on_thing_lock(status)
        type(status_type), intent(out) :: status
        integer :: k
        status = status_type()
        if (font_lock_support_mode /= FONT_LOCK_SUPPORT_JIT_LOCK) return
        ! Prepare for jit-lock.
        call remove_after_change_function()
        k = local_index(.true.)
        locals(k)%jit_lock = .true.
        ! Don't fontify eagerly (and don't abort if the buffer is large).
        locals(k)%fontified = .true.
        ! Use jit-lock.
        call jit_lock_register(font_lock_fontify_region_function, status)
        if (.not. status%ok()) return
        ! Tell jit-lock how we extend the region to refontify.
        call jit_lock_after_change_extend_region_functions%add(font_lock_extend_jit_lock_region_after_change, &
                                                               local=.true.)
    end subroutine font_lock_turn_on_thing_lock

    !> Turn off the support mode.  Port of
    !> font-lock.el:font-lock-turn-off-thing-lock.
    subroutine font_lock_turn_off_thing_lock(status)
        type(status_type), intent(out) :: status
        integer :: k
        status = status_type()
        if (.not. jit_lock_mode_p()) return
        call jit_lock_unregister(font_lock_fontify_region_function, status)
        ! Reset local vars to the non-jit-lock case.
        k = local_index(.false.)
        if (k > 0) locals(k)%jit_lock = .false.
    end subroutine font_lock_turn_off_thing_lock

    !> `font_lock_fontify_region' as a function of `jit_lock_functions':
    !> DONE_BEG and DONE_END give the region actually fontified
    !> (jit-lock-bounds).
    subroutine font_lock_fontify_region_function(beg, end, done_beg, done_end, status)
        integer(int64), intent(in) :: beg, end
        integer(int64), intent(out) :: done_beg, done_end
        type(status_type), intent(out) :: status
        call font_lock_fontify_region(beg, end, status, done_beg=done_beg, done_end=done_end)
    end subroutine font_lock_fontify_region_function

    !> Function meant for `jit_lock_after_change_extend_region_functions'.
    !> This function does 2 things:
    !> - extend the region so that it not only includes the part that was
    !>   modified but also the surrounding text whose highlighting may
    !>   change as a consequence.
    !> - anticipate (part of) the region extension that will happen later
    !>   in `font_lock_default_fontify_region', in order to avoid the need
    !>   for double-redisplay in `jit_lock_fontify_now'.  Port of
    !> font-lock.el:font-lock-extend-jit-lock-region-after-change.
    subroutine font_lock_extend_jit_lock_region_after_change(beg_in, end_in, old_len, status)
        integer(int64), intent(in) :: beg_in, end_in, old_len
        type(status_type), intent(out) :: status
        type(value_type) :: v
        integer(int64) :: beg, end, pt, pos
        logical :: found
        status = status_type()
        if (old_len < 0) return
        beg = beg_in
        end = end_in
        pt = point()
        ! Extend the region obeying font-lock-multiline properties,
        ! indicating which part of the buffer needs to be refontified.
        if (beg > point_min()) then
            call get_text_property(beg - 1, sym("font_lock_multiline"), v, status)
            if (.not. nilp(v)) then
                call previous_single_property_change(beg, sym("font_lock_multiline"), pos, found, status)
                beg = point_min()
                if (found) beg = pos
            end if
        end if
        if (end < point_max()) then
            call get_text_property(end, sym("font_lock_multiline"), v, status)
            if (.not. nilp(v)) then
                call text_property_any(end, point_max(), sym("font_lock_multiline"), nil(), pos, found, status)
                end = point_max()
                if (found) end = pos
            else
                ! Rounding up to a whole number of lines should include the
                ! line right after END.
                end = end + 1
            end if
        end if
        ! Finally, pre-enlarge the region to a whole number of lines.
        call goto_char(beg)
        beg = min(jit_lock_start, line_beginning_position())
        call goto_char(end)
        if (point() == line_beginning_position()) then
            end = max(jit_lock_end, point())
        else
            end = max(jit_lock_end, line_beginning_position(2_int64))
        end if
        jit_lock_start = beg
        jit_lock_end = end
        call goto_char(pt)
    end subroutine font_lock_extend_jit_lock_region_after_change

    !> Restart fontification in current buffer after recomputing from
    !> defaults.  Recompute fontification variables using
    !> `font_lock_defaults' and `font_lock_maximum_decoration'.  Then
    !> restart fontification.
    !>
    !> Use this function when you have changed any of the above
    !> variables directly.
    !>
    !> Note: This function will erase modifications done by
    !> `font_lock_add_keywords'.  Port of
    !> font-lock.el:font-lock-refresh-defaults.
    subroutine font_lock_refresh_defaults(status)
        type(status_type), intent(out) :: status
        integer :: k
        call minor_modes%font_lock_mode%disable(status)
        if (.not. status%ok()) return
        k = local_index(.false.)
        if (k > 0) locals(k)%set_defaults = .false.
        call minor_modes%font_lock_mode%enable(status)
    end subroutine font_lock_refresh_defaults

    !> Set fontification defaults appropriately for this mode.
    !> Sets various variables using `font_lock_defaults' and
    !> `font_lock_maximum_decoration'.  Port of
    !> font-lock.el:font-lock-set-defaults.
    subroutine font_lock_set_defaults(status)
        type(status_type), intent(out) :: status
        type(font_lock_defaults_type) :: defaults
        type(font_lock_keyword_type), allocatable :: keywords(:)
        integer :: k, i, j, level
        status = status_type()
        k = local_index(.true.)
        if (k == 0) return
        ! Set fontification defaults if not previously set for correct
        ! major mode.
        if (locals(k)%set_defaults .and. allocated(locals(k)%major_mode)) then
            if (len(locals(k)%major_mode) > 0) then
                ! (derived-mode-p is true for the mode itself.)
                if (locals(k)%major_mode == major_mode_of_current()) return
                if (derived_mode_p(locals(k)%major_mode)) return
            end if
        end if
        locals(k)%major_mode = major_mode_of_current()
        locals(k)%set_defaults = .true.
        allocate (keywords(0))
        if (font_lock_defaults%present_p()) then
            defaults = font_lock_defaults%get()
            ! font-lock-choose-keywords.
            if (allocated(defaults%levels)) then
                if (size(defaults%levels) > 0) then
                    level = min(font_lock_maximum_decoration, size(defaults%levels) - 1)
                    level = max(level, 0)
                    if (allocated(defaults%levels(level + 1)%keywords)) keywords = defaults%levels(level + 1)%keywords
                end if
            end if
            ! Syntactic fontification?
            locals(k)%keywords_only = defaults%keywords_only
            ! Case fold during regexp fontification?
            locals(k)%keywords_case_fold_search = defaults%case_fold
            ! Syntax table for regexp and syntactic fontification?
            locals(k)%syntax_table_affects_ppss = .false.
            locals(k)%syntax_table = null_handle()
            if (allocated(defaults%syntax_alist)) then
                if (size(defaults%syntax_alist) > 0) then
                    call copy_syntax_table(locals(k)%syntax_table, status, syntax_table())
                    if (.not. status%ok()) return
                    do i = 1, size(defaults%syntax_alist)
                        associate (selem => defaults%syntax_alist(i))
                            do j = 1, len(selem%chars)
                                call modify_syntax_entry(ichar(selem%chars(j:j)), ichar(selem%chars(j:j)), selem%syntax, &
                                                         status, locals(k)%syntax_table)
                                if (.not. status%ok()) return
                            end do
                        end associate
                    end do
                    ! (Whether the changes affect syntax-ppss is not
                    ! analysed: they are taken to.)
                    locals(k)%syntax_table_affects_ppss = .true.
                end if
            end if
        else
            locals(k)%keywords_only = .false.
            locals(k)%keywords_case_fold_search = .false.
            locals(k)%syntax_table = null_handle()
        end if
        ! Set up `font_lock_keywords' last because its value might depend
        ! on other settings.
        locals(k)%keywords = keywords
        locals(k)%keywords_set = .true.
        ! Local fontification?
        if (allocated(font_lock_keywords_alist)) then
            do i = 1, size(font_lock_keywords_alist)
                if (trim(font_lock_keywords_alist(i)%mode) /= major_mode_of_current()) cycle
                call add_keywords_here(font_lock_keywords_alist(i)%keywords, font_lock_keywords_alist(i)%how)
            end do
        end if
        call font_lock_flush(status)
    end subroutine font_lock_set_defaults

    !> Add KEYWORDS to the current buffer's `font_lock_keywords' as HOW
    !> says (the buffer part of font-lock-add-keywords).
    subroutine add_keywords_here(keywords, how)
        type(font_lock_keyword_type), intent(in) :: keywords(:)
        integer, intent(in) :: how
        type(font_lock_keyword_type), allocatable :: old(:)
        integer :: k
        k = local_index(.true.)
        if (k == 0) return
        if (how == FONT_LOCK_ADD_SET) then
            locals(k)%keywords = keywords
        else
            allocate (old(0))
            if (allocated(locals(k)%keywords)) call move_alloc(locals(k)%keywords, old)
            if (how == FONT_LOCK_ADD_END) then
                locals(k)%keywords = [old, keywords]
            else
                locals(k)%keywords = [keywords, old]
            end if
        end if
        locals(k)%keywords_set = .true.
    end subroutine add_keywords_here

    !> Add highlighting KEYWORDS for MODE.
    !>
    !> MODE should be the major mode command name, such as "c_mode", or
    !> empty.  If empty, highlighting keywords are added for the current
    !> buffer.  By default they are added at the beginning of the current
    !> highlighting list.  If HOW is FONT_LOCK_ADD_SET, they are used to
    !> replace the current highlighting list.  If HOW is FONT_LOCK_ADD_END,
    !> they are added at the end of the current highlighting list.
    !>
    !> For example:
    !>
    !>  type(font_lock_highlight_type) :: fixme(1)
    !>  type(font_lock_keyword_type) :: keywords(2)
    !>  fixme(1) = font_lock_highlight(1, "font_lock_warning_face", FONT_LOCK_OVERRIDE_PREPEND)
    !>  keywords(1) = font_lock_keyword("\\<\\(FIXME\\):", fixme)
    !>  keywords(2) = font_lock_keyword("\\<\\(and\\|or\\|not\\)\\>", face="font_lock_keyword_face")
    !>  call font_lock_add_keywords("c_mode", keywords, status)
    !>
    !> adds two fontification patterns for C mode, to fontify `FIXME:'
    !> words, even in comments, and to fontify `and', `or' and `not' words
    !> as keywords.
    !>
    !> The above procedure will only add the keywords for C mode, not
    !> for modes derived from C mode.  To add them for derived modes too,
    !> pass an empty MODE and add the call to `c_mode_hook'.  Port of
    !> font-lock.el:font-lock-add-keywords (without removing duplicates of
    !> the keywords added; font-lock-remove-keywords is deferred).
    subroutine font_lock_add_keywords(mode, keywords, status, how)
        character(len=*), intent(in) :: mode
        type(font_lock_keyword_type), intent(in) :: keywords(:)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: how
        type(added_keywords_type), allocatable :: grown(:)
        integer :: h, i, n
        status = status_type()
        h = FONT_LOCK_ADD_BEGINNING
        if (present(how)) h = how
        if (len(mode) > 0) then
            ! Add the KEYWORDS and HOW spec to `font_lock_keywords_alist'
            ! so `font_lock_set_defaults' uses them.
            if (.not. allocated(font_lock_keywords_alist)) allocate (font_lock_keywords_alist(0))
            if (h == FONT_LOCK_ADD_SET) then
                ! Replace the mode's earlier specs.
                n = 0
                allocate (grown(0))
                do i = 1, size(font_lock_keywords_alist)
                    if (trim(font_lock_keywords_alist(i)%mode) == mode) cycle
                    grown = [grown, font_lock_keywords_alist(i)]
                end do
                call move_alloc(grown, font_lock_keywords_alist)
            end if
            n = size(font_lock_keywords_alist)
            allocate (grown(n + 1))
            grown(1:n) = font_lock_keywords_alist
            grown(n + 1)%mode = mode
            grown(n + 1)%keywords = keywords
            grown(n + 1)%how = h
            call move_alloc(grown, font_lock_keywords_alist)
            return
        end if
        if (font_lock_mode_enabled() .and. .not. font_lock_specified_p(.false.)) then
            ! The major mode has not set any keywords, so when we enabled
            ! font-lock-mode it only enabled the font-core.el part, not the
            ! font-lock-mode-internal.  Try again.
            call minor_modes%font_lock_mode%disable(status)
            if (.not. status%ok()) return
            call font_lock_defaults%set_local(keywords_only=.true.)
            call minor_modes%font_lock_mode%enable(status)
            if (.not. status%ok()) return
        end if
        ! Otherwise set or add the keywords now.  This is a no-op if it has
        ! been done already in this buffer for the correct major mode.
        call font_lock_set_defaults(status)
        if (.not. status%ok()) return
        call add_keywords_here(keywords, h)
    end subroutine font_lock_add_keywords

    ! ------------------------------------------------------------------
    ! Fontification functions
    ! ------------------------------------------------------------------

    !> Fontify the current buffer the way the function `font_lock_mode'
    !> would.  Port of font-lock.el:font-lock-fontify-buffer.
    subroutine font_lock_fontify_buffer(status)
        type(status_type), intent(out) :: status
        integer :: k
        call font_lock_set_defaults(status)
        if (.not. status%ok()) return
        k = local_index(.false.)
        if (k > 0) then
            if (locals(k)%jit_lock) then
                call jit_lock_refontify(status, point_min(), point_max())
                return
            end if
        end if
        call font_lock_default_fontify_buffer(status)
    end subroutine font_lock_fontify_buffer

    !> The interactive `font_lock_fontify_buffer'.
    subroutine cmd_font_lock_fontify_buffer(status)
        type(status_type), intent(out) :: status
        logical :: saved
        saved = font_lock_verbose
        font_lock_verbose = .true.
        call font_lock_fontify_buffer(status)
        font_lock_verbose = saved
    end subroutine cmd_font_lock_fontify_buffer

    !> Unfontify the whole buffer (font-lock-unfontify-buffer-function).
    !> Port of font-lock.el:font-lock-unfontify-buffer.
    subroutine font_lock_unfontify_buffer(status)
        type(status_type), intent(out) :: status
        call font_lock_default_unfontify_buffer(status)
    end subroutine font_lock_unfontify_buffer

    !> Fontify the text between BEG and END.
    !> If LOUDLY is true, print status messages while fontifying.
    !> This works by calling `font_lock_fontify_region_function'.  Port of
    !> font-lock.el:font-lock-fontify-region.
    subroutine font_lock_fontify_region(beg, end, status, loudly, done_beg, done_end)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: loudly
        !> The region actually fontified (jit-lock-bounds).
        integer(int64), intent(out), optional :: done_beg, done_end
        integer(int64) :: begv, zv, b, e
        logical :: loud
        loud = .false.
        if (present(loudly)) loud = loudly
        call font_lock_set_defaults(status)
        if (.not. status%ok()) return
        begv = buffers(cur)%begv
        zv = buffers(cur)%zv
        if (.not. font_lock_dont_widen) call widen()
        call font_lock_default_fontify_region(beg, end, loud, status, b, e)
        call restore_restriction(begv, zv)
        if (present(done_beg)) done_beg = b
        if (present(done_end)) done_end = e
    end subroutine font_lock_fontify_region

    !> Unfontify the text between BEG and END.
    !> This works by calling `font_lock_unfontify_region_function'.  Port
    !> of font-lock.el:font-lock-unfontify-region.
    subroutine font_lock_unfontify_region(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        type(silent_modifications_type) :: silent
        call silent_modifications_begin(silent)
        call font_lock_default_unfontify_region(beg, end, status)
        call silent_modifications_end(silent)
    end subroutine font_lock_unfontify_region

    !> Declare the region BEG...END's fontification as out-of-date.
    !> If the region is not specified, it defaults to the entire
    !> accessible portion of the current buffer.  Port of
    !> font-lock.el:font-lock-flush.
    subroutine font_lock_flush(status, beg, end)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beg, end
        integer(int64) :: b, e
        integer :: k
        status = status_type()
        if (.not. font_lock_mode_enabled()) return
        if (.not. font_lock_fontified()) return
        b = point_min()
        if (present(beg)) b = beg
        e = point_max()
        if (present(end)) e = end
        k = local_index(.false.)
        if (locals(k)%jit_lock) then
            call jit_lock_refontify(status, b, e)
        else
            call font_lock_after_change_function(b, e, 0_int64, status)
        end if
    end subroutine font_lock_flush

    !> Make sure the region BEG...END has been fontified.
    !> If the region is not specified, it defaults to the entire accessible
    !> portion of the buffer.  Port of font-lock.el:font-lock-ensure.
    subroutine font_lock_ensure(status, beg, end)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beg, end
        integer(int64) :: b, e, pt
        integer :: k
        status = status_type()
        if (.not. font_lock_specified_p(.true.)) return
        call font_lock_set_defaults(status)
        if (.not. status%ok()) return
        b = point_min()
        if (present(beg)) b = beg
        e = point_max()
        if (present(end)) e = end
        k = local_index(.false.)
        if (locals(k)%jit_lock) then
            call jit_lock_fontify_now(status, b, e)
        else if (.not. locals(k)%fontified) then
            pt = point()
            call font_lock_fontify_region(b, e, status)
            call goto_char(pt)
        end if
    end subroutine font_lock_ensure

    !> Update the syntax highlighting in this buffer.
    !> Refontify the accessible portion of this buffer, or enable Font Lock
    !> mode in this buffer if it is currently disabled.  With prefix ARG,
    !> toggle Font Lock mode.  Port of font-lock.el:font-lock-update.
    subroutine font_lock_update(status)
        type(status_type), intent(out) :: status
        integer(int64) :: pt
        pt = point()
        if (nilp(current_prefix_arg) .and. font_lock_mode_enabled()) then
            call font_lock_fontify_region(point_min(), point_max(), status)
        else
            call font_lock_unfontify_region(point_min(), point_max(), status)
            if (status%ok()) call minor_modes%font_lock_mode%toggle(status)
        end if
        call goto_char(pt)
    end subroutine font_lock_update

    !> Fontify the whole buffer using `font_lock_fontify_region_function'.
    !> Port of font-lock.el:font-lock-default-fontify-buffer.
    subroutine font_lock_default_fontify_buffer(status)
        type(status_type), intent(out) :: status
        integer(int64) :: begv, zv, pt
        integer(int64), allocatable :: saved_match(:)
        integer :: k
        if (font_lock_verbose) call message("Fontifying "//buffers(cur)%name//"...")
        ! Make sure we fontify etc. in the whole buffer.
        begv = buffers(cur)%begv
        zv = buffers(cur)%zv
        if (.not. font_lock_dont_widen) call widen()
        pt = point()
        saved_match = match_data()
        call font_lock_fontify_region(point_min(), point_max(), status, font_lock_verbose)
        call set_match_data(saved_match)
        call goto_char(pt)
        if (status%ok()) then
            k = local_index(.true.)
            locals(k)%fontified = .true.
        end if
        call restore_restriction(begv, zv)
        if (font_lock_verbose) call message("")
    end subroutine font_lock_default_fontify_buffer

    !> Unfontify the whole buffer using
    !> `font_lock_unfontify_region_function'.  Port of
    !> font-lock.el:font-lock-default-unfontify-buffer.
    subroutine font_lock_default_unfontify_buffer(status)
        type(status_type), intent(out) :: status
        integer(int64) :: begv, zv
        integer :: k
        ! Make sure we unfontify etc. in the whole buffer.
        begv = buffers(cur)%begv
        zv = buffers(cur)%zv
        call widen()
        call font_lock_unfontify_region(point_min(), point_max(), status)
        k = local_index(.false.)
        if (k > 0) locals(k)%fontified = .false.
        call restore_restriction(begv, zv)
    end subroutine font_lock_default_unfontify_buffer

    !> Narrow back to BEGV...ZV (the end of a save-restriction).
    subroutine restore_restriction(begv, zv)
        integer(int64), intent(in) :: begv, zv
        type(status_type) :: st
        call widen()
        call narrow_to_region(begv, zv, st)
    end subroutine restore_restriction

    !> Move fontification boundaries BEG and END to beginning of lines;
    !> CHANGED tells whether they moved.  Port of
    !> font-lock.el:font-lock-extend-region-wholelines (with
    !> syntax.el:syntax-propertize-wholelines).
    subroutine font_lock_extend_region_wholelines(beg, end, changed)
        integer(int64), intent(inout) :: beg, end
        logical, intent(out) :: changed
        integer(int64) :: pt, new_beg, new_end
        integer(int64) :: shortage
        pt = point()
        call goto_char(beg)
        new_beg = line_beginning_position()
        call goto_char(end)
        new_end = end
        if (point() /= line_beginning_position()) then
            shortage = forward_line(1_int64)
            new_end = point()
        end if
        call goto_char(pt)
        changed = .false.
        if (new_beg /= beg .or. new_end /= end) then
            beg = new_beg
            end = new_end
            changed = .true.
        end if
    end subroutine font_lock_extend_region_wholelines

    !> Move fontification boundaries BEG and END away from any
    !> `font_lock_multiline' property; CHANGED tells whether they moved.
    !> Port of font-lock.el:font-lock-extend-region-multiline.
    subroutine font_lock_extend_region_multiline(beg, end, changed)
        integer(int64), intent(inout) :: beg, end
        logical, intent(out) :: changed
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: before_end, new_end, pos
        logical :: found
        changed = .false.
        if (beg > point_min()) then
            call get_text_property(beg - 1, sym("font_lock_multiline"), v, st)
            if (.not. nilp(v)) then
                changed = .true.
                call previous_change(beg, sym("font_lock_multiline"), pos, found)
                beg = point_min()
                if (found) beg = pos
            end if
        end if
        ! If `font_lock_multiline' starts at END, do not extend the region.
        before_end = max(point_min(), end - 1)
        call get_text_property(before_end, sym("font_lock_multiline"), v, st)
        if (.not. nilp(v)) then
            call text_property_any(before_end, point_max(), sym("font_lock_multiline"), nil(), pos, found, st)
            new_end = point_max()
            if (found) new_end = pos
            if (new_end /= end) then
                changed = .true.
                end = new_end
            end if
        end if
    contains
        subroutine previous_change(position, prop, result, found)
            integer(int64), intent(in) :: position
            type(value_type), intent(in) :: prop
            integer(int64), intent(out) :: result
            logical, intent(out) :: found
            type(status_type) :: st
            call previous_single_property_change(position, prop, result, found, st)
        end subroutine previous_change
    end subroutine font_lock_extend_region_multiline

    !> Fontify the text between BEG and END.
    !> If LOUDLY is true, print status messages while fontifying.
    !> This function is the default `font_lock_fontify_region_function'.
    !> Port of font-lock.el:font-lock-default-fontify-region (the
    !> extend-region functions are font-lock-extend-region-wholelines and
    !> font-lock-extend-region-multiline; font-lock-syntactic-keywords are
    !> deferred).
    subroutine font_lock_default_fontify_region(beg_in, end_in, loudly, status, done_beg, done_end)
        integer(int64), intent(in) :: beg_in, end_in
        logical, intent(in) :: loudly
        type(status_type), intent(out) :: status
        !> The region fontified, BEG...END extended (jit-lock-bounds).
        integer(int64), intent(out) :: done_beg, done_end
        type(silent_modifications_type) :: silent
        type(handle_type) :: saved_table
        integer(int64) :: beg, end
        logical :: changed, any_changed
        integer :: k
        status = status_type()
        beg = beg_in
        end = min(end_in, point_max())
        call silent_modifications_begin(silent)
        ! Use the fontification syntax table, if any.
        k = local_index(.true.)
        saved_table = buffers(cur)%syntax_table
        if (locals(k)%syntax_table%index /= 0) call set_syntax_table(locals(k)%syntax_table, status)
        ! Extend the region to fontify so that it starts and ends at safe
        ! places: run the functions in turn repeatedly until they all
        ! return false.
        do
            any_changed = .false.
            call font_lock_extend_region_wholelines(beg, end, changed)
            ! (A change by the first function doesn't restart the list.)
            call font_lock_extend_region_multiline(beg, end, changed)
            if (changed) any_changed = .true.
            if (.not. any_changed) exit
        end do
        ! Now do the fontification.
        call font_lock_unfontify_region(beg, end, status)
        if (status%ok() .and. .not. locals(k)%keywords_only) then
            call font_lock_fontify_syntactically_region(beg, end, loudly, status)
        end if
        if (status%ok()) call font_lock_fontify_keywords_region(beg, end, loudly, status)
        buffers(cur)%syntax_table = saved_table
        call silent_modifications_end(silent)
        done_beg = beg
        done_end = end
    end subroutine font_lock_default_fontify_region

    !> Unfontify the text between BEG and END.
    !> This function is the default `font_lock_unfontify_region_function'.
    !> Port of font-lock.el:font-lock-default-unfontify-region.
    subroutine font_lock_default_unfontify_region(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        type(value_type) :: props
        type(value_type), allocatable :: names(:)
        logical :: changed
        integer :: k, n
        n = 2
        if (allocated(font_lock_extra_managed_props)) n = n + size(font_lock_extra_managed_props)
        allocate (names(n))
        if (allocated(font_lock_extra_managed_props)) then
            do k = 1, size(font_lock_extra_managed_props)
                names(k) = sym(trim(font_lock_extra_managed_props(k)))
            end do
        end if
        names(n - 1) = sym("face")
        names(n) = sym("font_lock_multiline")
        props = make_list(names)
        call remove_list_of_text_properties(beg, end, props, changed, status)
    end subroutine font_lock_default_unfontify_region

    !> Called when any modification is made to buffer text: refontify the
    !> whole lines BEG...END (and the line after END unless at the end).
    !> Port of font-lock.el:font-lock-after-change-function
    !> (font-lock-extend-after-change-region-function is deferred).
    subroutine font_lock_after_change_function(beg, end, old_len, status)
        integer(int64), intent(in) :: beg, end, old_len
        type(status_type), intent(out) :: status
        integer(int64) :: pt, e
        integer(int64), allocatable :: saved_match(:)
        pt = point()
        saved_match = match_data()
        e = end
        ! Rounding up to a whole number of lines should include the line
        ! right after `end'.
        if (e /= point_max()) e = e + 1
        call font_lock_fontify_region(beg, e, status)
        call set_match_data(saved_match)
        call goto_char(pt)
        if (.false.) print *, old_len
    end subroutine font_lock_after_change_function

    ! ------------------------------------------------------------------
    ! Additional text property functions
    ! ------------------------------------------------------------------

    !> Add an element to a property of the text from START to END.
    !> Arguments PROP and VALUE specify the property and value to add to
    !> the value already in place.  The resulting property values are
    !> always lists.  If argument APPEND is true, VALUE will be appended,
    !> otherwise it will be prepended.  Port of
    !> font-lock.el:font-lock--add-text-property.
    subroutine add_text_property(start_in, end, prop, value, append, status)
        integer(int64), intent(in) :: start_in, end
        type(value_type), intent(in) :: prop, value
        logical, intent(in) :: append
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: val(:), prev_items(:), all(:)
        type(value_type) :: prev, new_value
        integer(int64) :: start, next
        logical :: found
        integer :: i
        status = status_type()
        ! A single face (a plist of face attributes, or a name) or a list
        ! of faces.
        if (consp(value) .and. .not. keyword_head(value)) then
            call list_elements(value, val)
        else
            allocate (val(1))
            val(1) = value
        end if
        start = start_in
        do while (start /= end)
            call next_single_property_change(start, prop, next, found, status, end)
            if (.not. status%ok()) return
            if (.not. found) next = end
            call get_text_property(start, prop, prev, status)
            if (.not. status%ok()) return
            ! Canonicalize old forms of face property.
            if (consp(prev) .and. keyword_head(prev)) prev = list(prev)
            if (nilp(prev)) then
                allocate (prev_items(0))
            else if (consp(prev)) then
                call list_elements(prev, prev_items)
            else
                allocate (prev_items(1))
                prev_items(1) = prev
            end if
            if (append) then
                all = [prev_items, val]
            else
                all = [val, prev_items]
            end if
            new_value = make_list(all)
            call put_text_property(start, next, prop, new_value, status)
            if (.not. status%ok()) return
            deallocate (prev_items)
            start = next
        end do
    end subroutine add_text_property

    !> Whether V is a list whose first element is a keyword.
    logical function keyword_head(v)
        type(value_type), intent(in) :: v
        type(value_type), allocatable :: elts(:)
        keyword_head = .false.
        if (.not. consp(v)) return
        call list_elements(v, elts)
        if (.not. symbolp(elts(1))) return
        associate (name => symbols(xsymbol(elts(1)))%name)
            if (len(name) > 0) keyword_head = name(1:1) == ":"
        end associate
    end function keyword_head

    !> Prepend to one property of the text from START to END.
    !> Arguments PROP and VALUE specify the property and value to prepend
    !> to the value already in place.  The resulting property values are
    !> always lists.  Port of font-lock.el:font-lock-prepend-text-property.
    subroutine font_lock_prepend_text_property(start, end, prop, value, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: prop, value
        type(status_type), intent(out) :: status
        call add_text_property(start, end, prop, value, .false., status)
    end subroutine font_lock_prepend_text_property

    !> Append to one property of the text from START to END.
    !> Arguments PROP and VALUE specify the property and value to append
    !> to the value already in place.  The resulting property values are
    !> always lists.  Port of font-lock.el:font-lock-append-text-property.
    subroutine font_lock_append_text_property(start, end, prop, value, status)
        integer(int64), intent(in) :: start, end
        type(value_type), intent(in) :: prop, value
        type(status_type), intent(out) :: status
        call add_text_property(start, end, prop, value, .true., status)
    end subroutine font_lock_append_text_property

    !> Fill in one property of the text from START to END.
    !> Arguments PROP and VALUE specify the property and value to put where
    !> none are already in place.  Therefore existing property values are
    !> not overwritten.  Port of font-lock.el:font-lock-fillin-text-property.
    subroutine font_lock_fillin_text_property(start_in, end, prop, value, status)
        integer(int64), intent(in) :: start_in, end
        type(value_type), intent(in) :: prop, value
        type(status_type), intent(out) :: status
        integer(int64) :: start, next
        logical :: found, found_next
        call text_property_any(start_in, end, prop, nil(), start, found, status)
        do while (found .and. status%ok())
            call next_single_property_change(start, prop, next, found_next, status, end)
            if (.not. found_next) next = end
            call put_text_property(start, next, prop, value, status)
            if (.not. status%ok()) return
            call text_property_any(next, end, prop, nil(), start, found, status)
        end do
    end subroutine font_lock_fillin_text_property

    !> Remove a specific property value from text from START to END.
    !> Arguments PROP and VALUE specify the property and value to remove.
    !> The resulting property values are not `eq' to VALUE nor arrays
    !> containing VALUE.  Port of
    !> font-lock.el:font-lock--remove-face-from-text-property (in the
    !> current buffer).
    subroutine font_lock_remove_face_from_text_property(start_in, end, prop, value, status)
        use textprop_fm, only: text_property_not_all, remove_list_of_text_properties
        integer(int64), intent(in) :: start_in, end
        type(value_type), intent(in) :: prop, value
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: elts(:), kept(:)
        type(value_type) :: prev
        integer(int64) :: start, next
        logical :: found, changed, member, atom
        integer :: i, n
        call text_property_not_all(start_in, end, prop, nil(), start, found, status)
        do while (found .and. status%ok())
            call next_single_property_change(start, prop, next, found, status, end)
            if (.not. status%ok()) return
            if (.not. found) next = end
            call get_text_property(start, prop, prev, status)
            atom = .not. consp(prev)
            if (.not. atom) atom = keyword_head(prev) .or. color_head(prev)
            if (atom) then
                if (value_eq(value, prev)) call remove_list_of_text_properties(start, next, list(prop), changed, status)
            else
                call list_elements(prev, elts)
                member = .false.
                n = 0
                allocate (kept(size(elts)))
                do i = 1, size(elts)
                    if (value_eq(elts(i), value)) then
                        member = .true.
                    else
                        n = n + 1
                        kept(n) = elts(i)
                    end if
                end do
                if (member) then
                    if (n == 0) then
                        call remove_list_of_text_properties(start, next, list(prop), changed, status)
                    else if (n == 1) then
                        call put_text_property(start, next, prop, kept(1), status)
                    else
                        call put_text_property(start, next, prop, make_list(kept(1:n)), status)
                    end if
                end if
                deallocate (kept)
            end if
            if (.not. status%ok()) return
            call text_property_not_all(next, end, prop, nil(), start, found, status)
        end do
    contains

        !> Whether V starts with `foreground_color' or `background_color'.
        logical function color_head(v)
            type(value_type), intent(in) :: v
            type(value_type), allocatable :: e(:)
            color_head = .false.
            call list_elements(v, e)
            if (size(e) == 0) return
            if (.not. symbolp(e(1))) return
            color_head = symbols(xsymbol(e(1)))%name == "foreground_color" .or. &
                         symbols(xsymbol(e(1)))%name == "background_color"
        end function color_head
    end subroutine font_lock_remove_face_from_text_property

    ! ------------------------------------------------------------------
    ! Keyword regexp fontification functions
    ! ------------------------------------------------------------------

    !> The value of a highlight's FACE: the name of its face, "" for none.
    function highlight_face(h) result(face)
        type(font_lock_match_highlight_type), intent(in) :: h
        character(len=:), allocatable :: face
        face = ""
        if (associated(h%face_function)) then
            call h%face_function(face)
        else if (allocated(h%face)) then
            face = h%face
        end if
    end function highlight_face

    !> Apply HIGHLIGHT following a match.  Port of
    !> font-lock.el:font-lock-apply-highlight.
    subroutine font_lock_apply_highlight(highlight, status)
        type(font_lock_match_highlight_type), intent(in) :: highlight
        type(status_type), intent(out) :: status
        type(status_type) :: st
        type(value_type) :: val
        character(len=:), allocatable :: face
        character(len=12) :: buf
        integer(int64) :: start, end, pos
        integer :: k
        logical :: found
        status = status_type()
        start = match_beginning(highlight%subexp, st)
        end = match_end(highlight%subexp, st)
        if (start < 0 .or. .not. st%ok()) then
            ! No match but we might not signal an error.
            if (.not. highlight%laxmatch) then
                write (buf, "(i0)") highlight%subexp
                call set_error(status, FM_GENERIC_ERROR, "No match "//trim(buf)//" in highlight")
            end if
            return
        end if
        ! (face FACE PROP VAL ...): add the properties.
        if (allocated(highlight%true_properties)) then
            do k = 1, size(highlight%true_properties)
                call put_text_property(start, end, sym(trim(highlight%true_properties(k))), t_value(), status)
                if (.not. status%ok()) return
            end do
        end if
        face = highlight_face(highlight)
        val = nil()
        if (len(face) > 0) val = sym(face)
        if (nilp(val) .and. highlight%override /= FONT_LOCK_OVERRIDE_T) then
            ! If `val' is nil, don't do anything.
            return
        end if
        select case (highlight%override)
        case (FONT_LOCK_OVERRIDE_NIL)
            ! Cannot override existing fontification.
            call text_property_not_all(start, end, sym("face"), nil(), pos, found, status)
            if (.not. found .and. status%ok()) call put_text_property(start, end, sym("face"), val, status)
        case (FONT_LOCK_OVERRIDE_T)
            ! Override existing fontification.
            call put_text_property(start, end, sym("face"), val, status)
        case (FONT_LOCK_OVERRIDE_PREPEND)
            ! Prepend to existing fontification.
            call font_lock_prepend_text_property(start, end, sym("face"), val, status)
        case (FONT_LOCK_OVERRIDE_APPEND)
            ! Append to existing fontification.
            call font_lock_append_text_property(start, end, sym("face"), val, status)
        case (FONT_LOCK_OVERRIDE_KEEP)
            ! Keep existing fontification.
            call font_lock_fillin_text_property(start, end, sym("face"), val, status)
        case default
            continue
        end select
    end subroutine font_lock_apply_highlight

    !> Search forward from point for MATCHER (REGEXP or the function
    !> MATCHER), not beyond LIMIT; FOUND tells whether it matched.
    subroutine search_matcher(regexp, matcher, limit, found, status)
        character(len=:), allocatable, intent(in) :: regexp
        procedure(font_lock_matcher_proc), pointer, intent(in) :: matcher
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: pos
        logical :: saved
        found = .false.
        if (associated(matcher)) then
            call matcher(limit, found, status)
        else if (allocated(regexp)) then
            saved = case_fold_search%value
            case_fold_search%value = keywords_case_fold()
            call re_search_forward(regexp, pos, status, bound=limit, noerror=SEARCH_NOERROR)
            case_fold_search%value = saved
            found = status%ok() .and. pos > 0
        else
            status = status_type()
        end if
    end subroutine search_matcher

    !> `font_lock_keywords_case_fold_search' in the current buffer.
    logical function keywords_case_fold()
        integer :: k
        keywords_case_fold = .false.
        k = local_index(.false.)
        if (k > 0) keywords_case_fold = locals(k)%keywords_case_fold_search
    end function keywords_case_fold

    !> Fontify according to KEYWORDS until LIMIT.
    !> KEYWORDS should be of the form MATCH-ANCHORED, see
    !> `font_lock_keywords'; LIMIT can be modified by the value of its
    !> PRE-MATCH-FORM.  Port of
    !> font-lock.el:font-lock-fontify-anchored-keywords
    !> (font-lock-multiline is deferred).
    subroutine font_lock_fontify_anchored_keywords(keywords, limit_in, status)
        type(font_lock_highlight_type), intent(in) :: keywords
        integer(int64), intent(in) :: limit_in
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: saved_match(:)
        integer(int64) :: limit, pre_match_value, ignored
        logical :: found
        integer :: i
        status = status_type()
        ! Evaluate PRE-MATCH-FORM.
        pre_match_value = 0
        if (associated(keywords%pre_match_form)) call keywords%pre_match_form(pre_match_value)
        ! Set LIMIT to value of PRE-MATCH-FORM or the end of line.
        if (pre_match_value > point()) then
            limit = pre_match_value
        else
            limit = line_end_position()
        end if
        if (.false.) limit = limit_in
        saved_match = match_data()
        ! Find an occurrence of `matcher' before `limit'.
        do while (point() < limit)
            call search_matcher(keywords%regexp, keywords%matcher, limit, found, status)
            if (.not. status%ok() .or. .not. found) exit
            ! Apply each highlight to this instance of `matcher'.
            do i = 1, size(keywords%highlights)
                call font_lock_apply_highlight(keywords%highlights(i), status)
                if (.not. status%ok()) exit
            end do
            if (.not. status%ok()) exit
        end do
        call set_match_data(saved_match)
        ! Evaluate POST-MATCH-FORM.
        if (status%ok() .and. associated(keywords%post_match_form)) call keywords%post_match_form(ignored)
    end subroutine font_lock_fontify_anchored_keywords

    !> Fontify according to `font_lock_keywords' between START and END.
    !> START should be at the beginning of a line.
    !> LOUDLY, if true, allows progress-meter bar.  Port of
    !> font-lock.el:font-lock-fontify-keywords-region (font-lock-multiline
    !> is deferred).
    subroutine font_lock_fontify_keywords_region(start, end, loudly, status)
        integer(int64), intent(in) :: start, end
        logical, intent(in) :: loudly
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        integer(int64) :: pos
        type(status_type) :: st
        logical :: found
        integer :: i, j, k
        status = status_type()
        k = local_index(.false.)
        if (k == 0) return
        if (.not. locals(k)%keywords_set) return
        if (.not. allocated(locals(k)%keywords)) return
        keywords = locals(k)%keywords
        ! Fontify each item in `font_lock_keywords' from START to END.
        do i = 1, size(keywords)
            if (loudly) call message("Fontifying "//buffers(cur)%name//"... (regexps.."//repeat(".", i)//")")
            ! Find an occurrence of `matcher' from START to END.
            call goto_char(start)
            do while (point() < end)
                call search_matcher(keywords(i)%regexp, keywords(i)%matcher, end, found, status)
                if (.not. status%ok()) return
                if (.not. found) exit
                ! Beware empty string matches since they will loop
                ! indefinitely.
                if (point() <= match_beginning(0, st)) call forward_char(st)
                ! Apply each highlight to this instance of `matcher', which
                ! may be specific highlights or more keywords anchored to
                ! `matcher'.
                if (.not. allocated(keywords(i)%highlights)) cycle
                do j = 1, size(keywords(i)%highlights)
                    if (.not. keywords(i)%highlights(j)%anchored) then
                        call font_lock_apply_highlight(keywords(i)%highlights(j)%match, status)
                    else
                        pos = point()
                        call font_lock_fontify_anchored_keywords(keywords(i)%highlights(j), end, status)
                        ! Ensure forward progress.
                        if (point() < pos) call goto_char(pos)
                    end if
                    if (.not. status%ok()) return
                end do
            end do
        end do
    end subroutine font_lock_fontify_keywords_region

    ! ------------------------------------------------------------------
    ! Syntactic fontification
    ! ------------------------------------------------------------------

    !> The default `font_lock_syntactic_face_function': strings in
    !> `font_lock_string_face', comments in `font_lock_comment_face'.
    subroutine default_syntactic_face(state, face)
        type(parse_state_type), intent(in) :: state
        character(len=:), allocatable, intent(out) :: face
        if (state%instring /= -1) then
            face = "font_lock_string_face"
        else
            face = "font_lock_comment_face"
        end if
    end subroutine default_syntactic_face

    !> Syntactically fontify the text between BEG and END.
    !> If LOUDLY is true, print status messages while fontifying.
    !> This works by calling `font_lock_fontify_syntactically_function'.
    !> Port of font-lock.el:font-lock-fontify-syntactically-region.
    subroutine font_lock_fontify_syntactically_region(beg, end, loudly, status)
        integer(int64), intent(in) :: beg, end
        logical, intent(in) :: loudly
        type(status_type), intent(out) :: status
        call font_lock_default_fontify_syntactically(beg, end, loudly, status)
    end subroutine font_lock_fontify_syntactically_region

    !> Put proper face on each string and comment between START and END.
    !> START should be at the beginning of a line.  Port of
    !> font-lock.el:font-lock-default-fontify-syntactically
    !> (syntax-propertize, comment-start-skip and comment-end-skip with
    !> font-lock-comment-delimiter-face are deferred).
    subroutine font_lock_default_fontify_syntactically(start, end, loudly, status)
        integer(int64), intent(in) :: start, end
        logical, intent(in) :: loudly
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state, next
        character(len=:), allocatable :: face
        integer(int64) :: beg
        integer :: k
        status = status_type()
        k = local_index(.true.)
        ! Find the `start' state.
        if (locals(k)%syntax_table_affects_ppss) then
            ! syntax-ppss would use the fontification syntax table and
            ! pollute its cache: parse from the beginning instead.
            call parse_partial_sexp(point_min(), start, state, status)
            if (.not. status%ok()) return
        else
            call syntax_ppss(state, start)
        end if
        if (loudly) call message("Fontifying "//buffers(cur)%name//"... (syntactically...)")
        ! Find each interesting place between here and `end'.
        do
            if (state%instring /= -1 .or. state%incomment /= 0) then
                call font_lock_syntactic_face_function(state, face)
                beg = max(state%comstr_start, start)
                call parse_partial_sexp(point(), end, next, status, oldstate=state, &
                                        commentstop=COMMENTSTOP_SYNTAX_TABLE)
                if (.not. status%ok()) return
                state = next
                if (len(face) > 0) call put_text_property(beg, point(), sym("face"), sym(face), status)
                if (.not. status%ok()) return
            end if
            if (point() >= end) exit
            call parse_partial_sexp(point(), end, next, status, oldstate=state, commentstop=COMMENTSTOP_SYNTAX_TABLE)
            if (.not. status%ok()) return
            state = next
        end do
    end subroutine font_lock_default_fontify_syntactically

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> Define the faces, commands and functions of font-lock.el (as
    !> loading it does).
    subroutine syms_of_font_lock()
        ! (faces.el is loaded before font-lock.el.)
        call init_faces()
        call init_font_lock_faces()
        font_lock_mode_internal_proc => font_lock_mode_internal
        font_lock_specified_p_proc => font_lock_specified_p
        font_lock_initial_fontify_proc => font_lock_initial_fontify
        if (.not. associated(font_lock_syntactic_face_function)) then
            font_lock_syntactic_face_function => default_syntactic_face
        end if
        call add_local_variables_killer(kill_font_lock_locals)
        commands%font_lock_update%run => font_lock_update
        commands%font_lock_fontify_buffer%run => cmd_font_lock_fontify_buffer
        commands%font_lock_fontify_block%run => font_lock_fontify_block
        commands%font_lock_debug_fontify%run => font_lock_debug_fontify
        commands%font_lock_refresh_defaults%run => font_lock_refresh_defaults
        commands%font_lock_refresh_defaults%interactive = .false.
        commands%font_lock_change_mode%interactive = .false.
    end subroutine syms_of_font_lock

    !> Fontify some lines the way `font_lock_fontify_buffer' would.
    !> The lines could be a function or paragraph, or a specified number
    !> of lines.  If ARG is given, fontify that many lines before and after
    !> point, or 16 lines if no ARG is given.  Port of
    !> font-lock.el:font-lock-fontify-block (font-lock-mark-block-function is
    !> deferred).
    subroutine font_lock_fontify_block(status)
        type(status_type), intent(out) :: status
        integer(int64) :: lines, pt, b, e, shortage
        integer(int64), allocatable :: saved_match(:)
        ! Make sure we have the right `font_lock_keywords' etc.
        if (.not. font_lock_mode_enabled()) then
            call font_lock_set_defaults(status)
            if (.not. status%ok()) return
        end if
        lines = 16
        if (.not. nilp(current_prefix_arg)) lines = prefix_numeric_value(current_prefix_arg)
        pt = point()
        saved_match = match_data()
        shortage = forward_line(-lines)
        b = point()
        call goto_char(pt)
        shortage = forward_line(lines)
        e = point()
        call goto_char(pt)
        call font_lock_fontify_region(b, e, status)
        if (.not. status%ok()) then
            call message("Fontifying block..."//format_error(status))
            status = status_type()
        end if
        call set_match_data(saved_match)
        call goto_char(pt)
    end subroutine font_lock_fontify_block

    !> Reinitialize the font-lock machinery and (re-)fontify the buffer.
    !> This functions is a convenience functions when developing font
    !> locking for a mode, and is not meant to be called from Fortran code.
    !> Port of font-lock.el:font-lock-debug-fontify.
    subroutine font_lock_debug_fontify(status)
        type(status_type), intent(out) :: status
        integer(int64) :: pt
        integer :: k
        ! Make font-lock recalculate all the mode-specific data.
        k = local_index(.true.)
        if (k > 0) locals(k)%major_mode = ""
        call font_lock_set_defaults(status)
        if (.not. status%ok()) return
        pt = point()
        call font_lock_fontify_region(point_min(), point_max(), status)
        call goto_char(pt)
    end subroutine font_lock_debug_fontify

    !> Define the faces of font-lock.el with their specs (as loading
    !> font-lock.el does).
    subroutine init_font_lock_faces()
        integer, parameter :: C = FACE_CLASS_COLOR, G = FACE_CLASS_GRAYSCALE
        integer, parameter :: LIGHT = FACE_BACKGROUND_LIGHT, DARK = FACE_BACKGROUND_DARK
        character(len=*), parameter :: LF = achar(10)
        type(named_face_type), pointer :: f
        f => font_lock_comment_face
        call f%define("font_lock_comment_face", "Font Lock mode face used to highlight comments.")
        call f%spec_entry(display_when(classes=[G], backgrounds=[LIGHT]), &
                          face_attrs(foreground="DimGray", weight="bold", slant="italic"))
        call f%spec_entry(display_when(classes=[G], backgrounds=[DARK]), &
                          face_attrs(foreground="LightGray", weight="bold", slant="italic"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), face_attrs(foreground="Firebrick"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), face_attrs(foreground="chocolate1"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), face_attrs(foreground="red"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), face_attrs(foreground="red1"))
        call f%spec_entry(display_when(classes=[C], min_colors=8, backgrounds=[LIGHT]), face_attrs(foreground="red"))
        call f%spec_entry(display_when(classes=[C], min_colors=8, backgrounds=[DARK]), face_attrs(foreground="yellow"))
        call f%spec_entry(display_when(), face_attrs(weight="bold", slant="italic"))

        f => font_lock_comment_delimiter_face
        call f%define("font_lock_comment_delimiter_face", "Font Lock mode face used to highlight comment delimiters.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="font_lock_comment_face"))

        f => font_lock_string_face
        call f%define("font_lock_string_face", "Font Lock mode face used to highlight strings.")
        call f%spec_entry(display_when(classes=[G], backgrounds=[LIGHT]), face_attrs(foreground="DimGray", slant="italic"))
        call f%spec_entry(display_when(classes=[G], backgrounds=[DARK]), face_attrs(foreground="LightGray", slant="italic"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), face_attrs(foreground="VioletRed4"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), face_attrs(foreground="LightSalmon"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), face_attrs(foreground="RosyBrown"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), face_attrs(foreground="LightSalmon"))
        call f%spec_entry(display_when(classes=[C], min_colors=8), face_attrs(foreground="green"))
        call f%spec_entry(display_when(), face_attrs(slant="italic"))

        f => font_lock_doc_face
        call f%define("font_lock_doc_face", "Font Lock mode face used to highlight documentation embedded in program code."// &
                      LF//"It is typically used for special documentation comments or strings.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_string_face"))

        f => font_lock_doc_markup_face
        call f%define("font_lock_doc_markup_face", "Font Lock mode face used to highlight embedded documentation mark-up."// &
                      LF//"It is meant for mark-up elements in text that uses `font_lock_doc_face', such"// &
                      LF//"as the constructs of Haddock, Javadoc and similar systems.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_constant_face"))

        f => font_lock_keyword_face
        call f%define("font_lock_keyword_face", "Font Lock mode face used to highlight keywords.")
        call f%spec_entry(display_when(classes=[G], backgrounds=[LIGHT]), face_attrs(foreground="LightGray", weight="bold"))
        call f%spec_entry(display_when(classes=[G], backgrounds=[DARK]), face_attrs(foreground="DimGray", weight="bold"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), face_attrs(foreground="Purple"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), face_attrs(foreground="Cyan1"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), face_attrs(foreground="Purple"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), face_attrs(foreground="Cyan"))
        call f%spec_entry(display_when(classes=[C], min_colors=8), face_attrs(foreground="cyan", weight="bold"))
        call f%spec_entry(display_when(), face_attrs(weight="bold"))

        f => font_lock_builtin_face
        call f%define("font_lock_builtin_face", "Font Lock mode face used to highlight builtins.")
        call f%spec_entry(display_when(classes=[G], backgrounds=[LIGHT]), face_attrs(foreground="LightGray", weight="bold"))
        call f%spec_entry(display_when(classes=[G], backgrounds=[DARK]), face_attrs(foreground="DimGray", weight="bold"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), &
                          face_attrs(foreground="dark slate blue"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), face_attrs(foreground="LightSteelBlue"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), face_attrs(foreground="Orchid"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), face_attrs(foreground="LightSteelBlue"))
        call f%spec_entry(display_when(classes=[C], min_colors=8), face_attrs(foreground="blue", weight="bold"))
        call f%spec_entry(display_when(), face_attrs(weight="bold"))

        f => font_lock_function_name_face
        call f%define("font_lock_function_name_face", "Font Lock mode face used to highlight function names.")
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), face_attrs(foreground="Blue1"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), face_attrs(foreground="LightSkyBlue"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), face_attrs(foreground="Blue"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), face_attrs(foreground="LightSkyBlue"))
        call f%spec_entry(display_when(classes=[C], min_colors=8), face_attrs(foreground="blue", weight="bold"))
        call f%spec_entry(display_when(), face_attrs(inverse_video=.true., weight="bold"))

        f => font_lock_function_call_face
        call f%define("font_lock_function_call_face", "Font Lock mode face used to highlight function calls.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_function_name_face"))

        f => font_lock_variable_name_face
        call f%define("font_lock_variable_name_face", "Font Lock mode face used to highlight variable names.")
        call f%spec_entry(display_when(classes=[G], backgrounds=[LIGHT]), &
                          face_attrs(foreground="Gray90", weight="bold", slant="italic"))
        call f%spec_entry(display_when(classes=[G], backgrounds=[DARK]), &
                          face_attrs(foreground="DimGray", weight="bold", slant="italic"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), face_attrs(foreground="sienna"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), face_attrs(foreground="LightGoldenrod"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), face_attrs(foreground="DarkGoldenrod"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), face_attrs(foreground="LightGoldenrod"))
        call f%spec_entry(display_when(classes=[C], min_colors=8), face_attrs(foreground="yellow", weight="light"))
        call f%spec_entry(display_when(), face_attrs(weight="bold", slant="italic"))

        f => font_lock_variable_use_face
        call f%define("font_lock_variable_use_face", "Font Lock mode face used to highlight variable references.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_variable_name_face"))

        f => font_lock_type_face
        call f%define("font_lock_type_face", "Font Lock mode face used to highlight type and class names.")
        call f%spec_entry(display_when(classes=[G], backgrounds=[LIGHT]), face_attrs(foreground="Gray90", weight="bold"))
        call f%spec_entry(display_when(classes=[G], backgrounds=[DARK]), face_attrs(foreground="DimGray", weight="bold"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), face_attrs(foreground="ForestGreen"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), face_attrs(foreground="PaleGreen"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), face_attrs(foreground="ForestGreen"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), face_attrs(foreground="PaleGreen"))
        call f%spec_entry(display_when(classes=[C], min_colors=8), face_attrs(foreground="green"))
        call f%spec_entry(display_when(), face_attrs(weight="bold", underline=.true.))

        f => font_lock_constant_face
        call f%define("font_lock_constant_face", "Font Lock mode face used to highlight constants and labels.")
        call f%spec_entry(display_when(classes=[G], backgrounds=[LIGHT]), &
                          face_attrs(foreground="LightGray", weight="bold", underline=.true.))
        call f%spec_entry(display_when(classes=[G], backgrounds=[DARK]), &
                          face_attrs(foreground="Gray50", weight="bold", underline=.true.))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[LIGHT]), face_attrs(foreground="dark cyan"))
        call f%spec_entry(display_when(classes=[C], min_colors=88, backgrounds=[DARK]), face_attrs(foreground="Aquamarine"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[LIGHT]), face_attrs(foreground="CadetBlue"))
        call f%spec_entry(display_when(classes=[C], min_colors=16, backgrounds=[DARK]), face_attrs(foreground="Aquamarine"))
        call f%spec_entry(display_when(classes=[C], min_colors=8), face_attrs(foreground="magenta"))
        call f%spec_entry(display_when(), face_attrs(weight="bold", underline=.true.))

        f => font_lock_warning_face
        call f%define("font_lock_warning_face", "Font Lock mode face used to highlight warnings.")
        call f%spec_entry(display_when(), face_attrs(inherit="error"))

        f => font_lock_negation_char_face
        call f%define("font_lock_negation_char_face", "Font Lock mode face used to highlight easy to overlook negation.")
        call f%spec_entry(display_when(), face_attrs())

        f => font_lock_preprocessor_face
        call f%define("font_lock_preprocessor_face", "Font Lock mode face used to highlight preprocessor directives.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_builtin_face"))

        f => font_lock_regexp_face
        call f%define("font_lock_regexp_face", "Font Lock mode face used to highlight regexp literals.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_string_face"))

        f => font_lock_regexp_grouping_backslash
        call f%define("font_lock_regexp_grouping_backslash", &
                      "Font Lock mode face for backslashes in regexp grouping constructs.")
        call f%spec_entry(display_when(), face_attrs(inherit="bold"))

        f => font_lock_regexp_grouping_construct
        call f%define("font_lock_regexp_grouping_construct", &
                      "Font Lock mode face used to highlight grouping constructs in regexps.")
        call f%spec_entry(display_when(), face_attrs(inherit="bold"))

        f => font_lock_escape_face
        call f%define("font_lock_escape_face", "Font Lock mode face used to highlight escape sequences in strings.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_regexp_grouping_backslash"))

        f => font_lock_number_face
        call f%define("font_lock_number_face", "Font Lock mode face used to highlight numbers.")
        call f%spec_entry(display_when(), face_attrs())

        f => font_lock_operator_face
        call f%define("font_lock_operator_face", "Font Lock mode face used to highlight operators.")
        call f%spec_entry(display_when(), face_attrs())

        f => font_lock_property_name_face
        call f%define("font_lock_property_name_face", "Font Lock mode face used to highlight properties of an object."// &
                      LF//"For example, the declaration of fields in a struct.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_variable_name_face"))

        f => font_lock_property_use_face
        call f%define("font_lock_property_use_face", "Font Lock mode face used to highlight property references."// &
                      LF//"For example, property lookup of fields in a struct.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_property_name_face"))

        f => font_lock_punctuation_face
        call f%define("font_lock_punctuation_face", "Font Lock mode face used to highlight punctuation characters.")
        call f%spec_entry(display_when(), face_attrs())

        f => font_lock_bracket_face
        call f%define("font_lock_bracket_face", "Font Lock mode face used to highlight brackets, braces, and parens.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_punctuation_face"))

        f => font_lock_delimiter_face
        call f%define("font_lock_delimiter_face", "Font Lock mode face used to highlight delimiters."// &
                      LF//"What exactly is a delimiter depends on the major mode, but usually"// &
                      LF//"these are characters like comma, colon, and semi-colon.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_punctuation_face"))

        f => font_lock_misc_punctuation_face
        call f%define("font_lock_misc_punctuation_face", "Font Lock mode face used to highlight miscellaneous punctuation.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_punctuation_face"))
    end subroutine init_font_lock_faces
end module font_lock_fm
