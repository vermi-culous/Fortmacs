! test_elec_pair_fm.f90 -- tests for Electric Pair mode (elec_pair_fm).
! Includes cases ported from GNU Emacs 31.1 test/lisp/electric-tests.el.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> electric-tests.el's `define-electric-pair-test' cases, generated the
!> same way (`pair_test': one run per character of INPUT that isn't
!> `-', in code and in strings, the expected text computed from
!> SKIP-PAIR-STRING as `electric-pair-define-test-form' does), each
!> unit test reporting the name of the ERT test that failed.  Upstream
!> runs them in ruby-mode, js-mode, python-mode and c-mode, which are not
!> ported; here they run in Fundamental mode, whose standard syntax table
!> has the same parens and string quote, and no comments (so the
!> "-in-comments" variants and the mode-specific cases are not ported).
!> electric-quote and electric-layout cases wait for those modes.
module test_elec_pair_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: nil, make_fixnum
    use lread_fm, only: sym
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, transient_mark_mode
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point, point_min, point_max, buffer_string
    use callint_fm, only: call_interactively, current_prefix_arg
    use keyboard_fm, only: last_command_event
    use cmds_fm, only: delete_char, forward_char
    use simple_fm, only: fundamental_mode, blink_paren_function, newline, undo, deactivate_mark
    use text_mode_fm, only: text_mode
    use lisp_cmds_fm, only: mark_sexp
    use undo_fm, only: undo_boundary
    use modes_fm, only: minor_modes
    use bindings_fm, only: init_bindings
    use elec_pair_fm, only: electric_pair_syntax_info, electric_pair_info_type, electric_pair_delete_pair, &
                            electric_pair_skip_whitespace, electric_pair_pairs, electric_pair_type, &
                            ELECTRIC_PAIR_SKIP_WHITESPACE_T, ELECTRIC_PAIR_SKIP_WHITESPACE_CHOMP
    use lisp_fm, only: command_proc
    use test_support, only: show_alone
    implicit none(type, external)
    private

    public :: collect_elec_pair_fm

    abstract interface
        !> Set (BIND true) or restore the variables a test binds.
        subroutine bindings_proc(bind)
            implicit none(type, external)
            logical, intent(in) :: bind
        end subroutine bindings_proc

        !> A test's FIXTURE-FN.
        subroutine fixture_proc()
            implicit none(type, external)
        end subroutine fixture_proc
    end interface

    type(electric_pair_type), allocatable :: saved_pairs(:)
    integer :: saved_skip_whitespace

contains

    subroutine collect_elec_pair_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("electric-pair-balanced-situation", test_balanced), &
                    new_unittest("electric-pair-too-many", test_too_many), &
                    new_unittest("electric-pair-autoskipping", test_autoskipping), &
                    new_unittest("electric-pair-mixed-paren", test_mixed_paren), &
                    new_unittest("electric-pair-different-paren-types", test_different_types), &
                    new_unittest("electric-pair-quotes", test_quotes), &
                    new_unittest("electric-pair-whitespace", test_whitespace), &
                    new_unittest("electric-pair-angle-brackets-everywhere", test_angle_brackets), &
                    new_unittest("electric-pair-autowrapping", test_autowrapping), &
                    new_unittest("electric-pair-strings-pairs", test_strings_pairs), &
                    new_unittest("electric-pair-strings-pairs-with-space", test_strings_pairs_with_space), &
                    new_unittest("electric-pair-backspace-1", test_backspace_1), &
                    new_unittest("electric-pair-backspace-2", test_backspace_2), &
                    new_unittest("electric-pair-undo-unrelated-state", test_undo_unrelated_state), &
                    new_unittest("electric-pair-open-extra-newline", test_open_extra_newline), &
                    new_unittest("electric-pair-local-mode", test_local_mode) &
                    ]
    end subroutine collect_elec_pair_fm

    subroutine no_bindings(bind)
        logical, intent(in) :: bind
        if (bind) continue
    end subroutine no_bindings

    !> The default FIXTURE-FN: #'electric-pair-mode.
    subroutine enable_pair_mode()
        type(status_type) :: st
        call minor_modes%electric_pair_mode%enable(st)
    end subroutine enable_pair_mode

    !> A fresh Fundamental (or, with TEXT, Text) mode buffer holding FIXTURE.
    subroutine temp_buffer(h, fixture, text)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: fixture
        logical, intent(in), optional :: text
        type(status_type) :: st
        call get_buffer_create("*elec-pair-test*", h, st)
        call set_buffer(h, st)
        call fundamental_mode(st)
        if (present(text)) then
            if (text) call text_mode(st)
        end if
        call insert(fixture, st)
        call show_alone()
    end subroutine temp_buffer

    !> Port of electric-tests.el:electric-pair-test-for: insert FIXTURE,
    !> put point at WHERE, run FIXTURE_FN and with BINDINGS self-insert CHAR
    !> (with `save_electric_modes': Electric Pair, Indent and Layout modes
    !> off, no `blink_paren_function').  OK says whether the text and
    !> point are as expected.
    subroutine pair_test_for(fixture, where, char, expected_string, expected_point, bindings, fixture_fn, ok, &
                             text, prefix)
        character(len=*), intent(in) :: fixture, expected_string
        integer(int64), intent(in) :: where, expected_point
        integer, intent(in) :: char
        procedure(bindings_proc) :: bindings
        procedure(fixture_proc) :: fixture_fn
        logical, intent(out) :: ok
        logical, intent(in), optional :: text
        integer(int64), intent(in), optional :: prefix
        type(handle_type) :: h
        type(status_type) :: st
        procedure(command_proc), pointer :: saved_blink
        logical :: saved_indent
        call temp_buffer(h, fixture, text)
        ! save-electric-modes
        saved_blink => blink_paren_function
        blink_paren_function => null()
        saved_indent = minor_modes%electric_indent_mode%enabled()
        call minor_modes%electric_pair_mode%disable(st)
        call minor_modes%electric_indent_mode%disable(st)
        last_command_event = make_fixnum(int(char, int64))
        transient_mark_mode = .true.
        call goto_char(where)
        call fixture_fn()
        call bindings(.true.)
        if (present(prefix)) current_prefix_arg = make_fixnum(prefix)
        call call_interactively(sym("self_insert_command"), st)
        current_prefix_arg = nil()
        call bindings(.false.)
        call minor_modes%electric_pair_mode%disable(st)
        if (saved_indent) call minor_modes%electric_indent_mode%enable(st)
        blink_paren_function => saved_blink
        ok = buffer_string() == expected_string .and. point() == expected_point
        call deactivate_mark(.true.)
        call kill_buffer(h, st)
    end subroutine pair_test_for

    !> Port of electric-tests.el:define-electric-pair-test for the given
    !> places: for each character of INPUT that isn't `-', at its
    !> position, in code and (unless IN_STRINGS is false) in strings.
    !> With SKIP_PAIR_STRING, the expected text is FIXTURE with the
    !> character inserted, then paired (`p'), skipped (`s') or left alone
    !> (`-').  Otherwise it is EXPECTED_STRING with point at
    !> EXPECTED_POINT.
    subroutine pair_test(error, name, fixture, input, skip_pair_string, expected_string, expected_point, &
                         bindings, fixture_fn, in_strings, in_code, text)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: name, fixture, input
        character(len=*), intent(in), optional :: skip_pair_string, expected_string
        integer(int64), intent(in), optional :: expected_point
        procedure(bindings_proc), optional :: bindings
        procedure(fixture_proc), optional :: fixture_fn
        logical, intent(in), optional :: in_strings, in_code, text
        character(len=:), allocatable :: expected, prefix, suffix, desc, mode
        integer(int64) :: point_expected, pos
        integer :: k, variant
        logical :: ok, strings, code
        character(len=16) :: buf
        strings = .true.
        if (present(in_strings)) strings = in_strings
        code = .true.
        if (present(in_code)) code = in_code
        mode = "fundamental-mode"
        if (present(text)) then
            if (text) mode = "text-mode"
        end if
        do variant = 1, 2
            if (variant == 1) then
                if (.not. strings) cycle
                prefix = '"'
                suffix = '"'
                desc = "-in-strings"
            else
                if (.not. code) cycle
                prefix = ""
                suffix = ""
                desc = ""
            end if
            do k = 1, len(input)
                if (input(k:k) == "-") cycle
                pos = k - 1
                if (present(skip_pair_string)) then
                    call expected_from_skip_pair_string(fixture, input(k:k), pos, skip_pair_string(k:k), &
                                                        expected, point_expected, bindings, text)
                else
                    expected = expected_string
                    point_expected = expected_point
                end if
                if (present(bindings) .and. present(fixture_fn)) then
                    call pair_test_for(prefix//fixture//suffix, len(prefix) + pos + 1, iachar(input(k:k)), &
                                       prefix//expected//suffix, len(prefix) + point_expected, bindings, &
                                       fixture_fn, ok, text)
                else if (present(bindings)) then
                    call pair_test_for(prefix//fixture//suffix, len(prefix) + pos + 1, iachar(input(k:k)), &
                                       prefix//expected//suffix, len(prefix) + point_expected, bindings, &
                                       enable_pair_mode, ok, text)
                else if (present(fixture_fn)) then
                    call pair_test_for(prefix//fixture//suffix, len(prefix) + pos + 1, iachar(input(k:k)), &
                                       prefix//expected//suffix, len(prefix) + point_expected, no_bindings, &
                                       fixture_fn, ok, text)
                else
                    call pair_test_for(prefix//fixture//suffix, len(prefix) + pos + 1, iachar(input(k:k)), &
                                       prefix//expected//suffix, len(prefix) + point_expected, no_bindings, &
                                       enable_pair_mode, ok, text)
                end if
                if (.not. ok) then
                    write (buf, "(i0)") len(prefix) + pos + 1
                    call test_failed(error, "electric-pair-"//name//"-at-point-"//trim(buf)//"-in-"//mode//desc)
                    return
                end if
            end do
        end do
    end subroutine pair_test

    !> The expected text and point for a SKIP_PAIR_STRING character
    !> (electric-pair-define-test-form).
    subroutine expected_from_skip_pair_string(fixture, char, pos, what, expected, point_expected, bindings, text)
        character(len=*), intent(in) :: fixture, char, what
        integer(int64), intent(in) :: pos
        character(len=:), allocatable, intent(out) :: expected
        integer(int64), intent(out) :: point_expected
        procedure(bindings_proc), optional :: bindings
        logical, intent(in), optional :: text
        type(handle_type) :: h
        type(status_type) :: st
        type(electric_pair_info_type) :: info
        if (present(bindings)) call bindings(.true.)
        call temp_buffer(h, fixture, text)
        call goto_char(pos + 1)
        call insert(char, st)
        if (what == "p") then
            info = electric_pair_syntax_info(iachar(char))
            call insert(achar(info%pair), st)
            call goto_char(point() - 1)
        else if (what == "s") then
            call delete_char(-1_int64, st)
            call forward_char(st, 1_int64)
        end if
        expected = buffer_string()
        point_expected = point()
        call kill_buffer(h, st)
        if (present(bindings)) call bindings(.false.)
    end subroutine expected_from_skip_pair_string

    ! ------------------------------------------------------------------
    ! Basic pairs and skips
    ! ------------------------------------------------------------------

    subroutine test_balanced(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call pair_test(error, "balanced-situation", " (())  ", "(((((((", skip_pair_string="ppppppp")
    end subroutine test_balanced

    subroutine test_too_many(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call pair_test(error, "too-many-openings", " ((()) ", "(((((((", skip_pair_string="ppppppp")
        if (allocated(error)) return
        call pair_test(error, "too-many-closings", " (())) ", "(((((((", skip_pair_string="------p")
        if (allocated(error)) return
        call pair_test(error, "too-many-closings-2", "()   ) ", "---(---", skip_pair_string="-------")
        if (allocated(error)) return
        call pair_test(error, "too-many-closings-3", ")()    ", "(------", skip_pair_string="-------")
    end subroutine test_too_many

    subroutine test_autoskipping(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call pair_test(error, "balanced-autoskipping", " (())  ", "---))--", skip_pair_string="---ss--")
        if (allocated(error)) return
        call pair_test(error, "too-many-openings-autoskipping", " ((()) ", "----))-", skip_pair_string="-------")
        if (allocated(error)) return
        call pair_test(error, "too-many-closings-autoskipping", " (())) ", "---)))-", skip_pair_string="---sss-")
    end subroutine test_autoskipping

    subroutine test_mixed_paren(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call pair_test(error, "mixed-paren-1", "  ()]  ", "-(-(---", skip_pair_string="-p-p---")
        if (allocated(error)) return
        call pair_test(error, "mixed-paren-2", "  [()  ", "-(-()--", skip_pair_string="-p-ps--")
        if (allocated(error)) return
        call pair_test(error, "mixed-paren-3", "  (])  ", "-(-()--", skip_pair_string="---ps--")
        if (allocated(error)) return
        call pair_test(error, "mixed-paren-4", "  ()]  ", "---)]--", skip_pair_string="---ss--")
        if (allocated(error)) return
        call pair_test(error, "mixed-paren-5", "  [()  ", "----(--", skip_pair_string="----p--")
    end subroutine test_mixed_paren

    subroutine test_different_types(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call pair_test(error, "find-matching-different-paren-type", "  ()]  ", "-[-----", skip_pair_string="-------")
        if (allocated(error)) return
        call pair_test(error, "find-matching-different-paren-type-inside-list", "( ()]) ", "-[-----", &
                       skip_pair_string="-------")
        if (allocated(error)) return
        call pair_test(error, "ignore-different-nonmatching-paren-type", "( ()]) ", "-(-----", &
                       skip_pair_string="-p-----")
        if (allocated(error)) return
        call pair_test(error, "autopair-keep-least-amount-of-mixed-unbalance", "( ()]  ", "-(-----", &
                       skip_pair_string="-p-----")
        if (allocated(error)) return
        call pair_test(error, "dont-autopair-to-resolve-mixed-unbalance", "( ()]  ", "-[-----", &
                       skip_pair_string="-------")
        if (allocated(error)) return
        call pair_test(error, "autopair-so-as-not-to-worsen-unbalance-situation", "( (])  ", "-[-----", &
                       skip_pair_string="-p-----")
        if (allocated(error)) return
        call pair_test(error, "skip-over-partially-balanced", " [([])   ", "-----)---", skip_pair_string="-----s---")
        if (allocated(error)) return
        call pair_test(error, "only-skip-over-at-least-partially-balanced-stuff", " [([())  ", "-----))--", &
                       skip_pair_string="-----s---")
    end subroutine test_different_types

    ! ------------------------------------------------------------------
    ! Quotes
    ! ------------------------------------------------------------------

    subroutine test_quotes(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        ! (electric-pair-text-syntax-table is prog-mode-syntax-table, the
        ! default here.)
        call pair_test(error, "pair-some-quotes-skip-others", ' ""      ', '-""-"---', skip_pair_string="-ps-p----", &
                       in_strings=.false.)
        if (allocated(error)) return
        call pair_test(error, "inhibit-if-strings-mismatched", '"foo""bar', '"', expected_string='""foo""bar', &
                       expected_point=2_int64, in_strings=.false.)
        if (allocated(error)) return
        call pair_test(error, "pairing-skipping-quotes-in-code", ' ""      ', '-""-----', skip_pair_string="-ps------", &
                       in_strings=.false.)
    end subroutine test_quotes

    ! ------------------------------------------------------------------
    ! Skipping over whitespace
    ! ------------------------------------------------------------------

    subroutine skip_whitespace_t(bind)
        logical, intent(in) :: bind
        if (bind) then
            saved_skip_whitespace = electric_pair_skip_whitespace
            electric_pair_skip_whitespace = ELECTRIC_PAIR_SKIP_WHITESPACE_T
        else
            electric_pair_skip_whitespace = saved_skip_whitespace
        end if
    end subroutine skip_whitespace_t

    subroutine skip_whitespace_chomp(bind)
        logical, intent(in) :: bind
        if (bind) then
            saved_skip_whitespace = electric_pair_skip_whitespace
            electric_pair_skip_whitespace = ELECTRIC_PAIR_SKIP_WHITESPACE_CHOMP
        else
            electric_pair_skip_whitespace = saved_skip_whitespace
        end if
    end subroutine skip_whitespace_chomp

    subroutine test_whitespace(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call pair_test(error, "whitespace-jumping", " (    )  ", "--))))---", expected_string=" (    )  ", &
                       expected_point=8_int64, bindings=skip_whitespace_t)
        if (allocated(error)) return
        call pair_test(error, "whitespace-chomping", " (    )  ", "--)------", expected_string=" ()  ", &
                       expected_point=4_int64, bindings=skip_whitespace_chomp)
        if (allocated(error)) return
        call pair_test(error, "whitespace-chomping-2", " ( "//achar(10)//achar(9)//achar(9)//achar(10)//"  )  ", &
                       "--)------", expected_string=" ()  ", expected_point=4_int64, bindings=skip_whitespace_chomp)
        if (allocated(error)) return
        call pair_test(error, "whitespace-skipping-for-quotes-not-outside", '  "  "', '"-----', &
                       expected_string='""  "  "', expected_point=2_int64, bindings=skip_whitespace_chomp, &
                       in_strings=.false.)
        if (allocated(error)) return
        call pair_test(error, "whitespace-skipping-for-quotes-only-inside", '  "  "', '---"--', &
                       expected_string='  ""', expected_point=5_int64, bindings=skip_whitespace_chomp, &
                       in_strings=.false.)
        if (allocated(error)) return
        call pair_test(error, "whitespace-skipping-quotes-not-without-proper-syntax", '  "  "', '---"--', &
                       expected_string='  """  "', expected_point=5_int64, bindings=skip_whitespace_chomp, &
                       in_strings=.false., text=.true.)
    end subroutine test_whitespace

    ! ------------------------------------------------------------------
    ! Pairing arbitrary characters
    ! ------------------------------------------------------------------

    subroutine angle_pairs(bind)
        logical, intent(in) :: bind
        if (bind) then
            saved_pairs = electric_pair_pairs
            deallocate (electric_pair_pairs)
            allocate (electric_pair_pairs(1))
            electric_pair_pairs(1)%open = iachar("<")
            electric_pair_pairs(1)%close = iachar(">")
        else
            call move_alloc(saved_pairs, electric_pair_pairs)
        end if
    end subroutine angle_pairs

    subroutine test_angle_brackets(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call pair_test(error, "angle-brackets-everywhere", "<>", "<>", skip_pair_string="ps", bindings=angle_pairs)
        if (allocated(error)) return
        call pair_test(error, "angle-brackets-everywhere-2", "(<>", "-<>", skip_pair_string="-ps", bindings=angle_pairs)
    end subroutine test_angle_brackets

    ! ------------------------------------------------------------------
    ! Autowrapping
    ! ------------------------------------------------------------------

    !> (electric-pair-mode 1) (mark-sexp 1)
    subroutine mark_forward()
        type(status_type) :: st
        call minor_modes%electric_pair_mode%enable(st)
        call mark_sexp(1_int64, .true., .false., st)
    end subroutine mark_forward

    !> (electric-pair-mode 1) (goto-char (point-max)) (skip-chars-backward
    !> "\"") (mark-sexp -1)
    subroutine mark_backward()
        type(status_type) :: st
        call minor_modes%electric_pair_mode%enable(st)
        call goto_char(point_max())
        if (point() > point_min()) then
            if (buffer_string_tail() == '"') call goto_char(point() - 1)
        end if
        call mark_sexp(-1_int64, .true., .false., st)
    end subroutine mark_backward

    !> The last character of the buffer.
    function buffer_string_tail() result(c)
        character :: c
        character(len=:), allocatable :: s
        s = buffer_string()
        c = s(len(s):len(s))
    end function buffer_string_tail

    subroutine test_autowrapping(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: ok
        call init_bindings()
        call pair_test(error, "autowrapping-1", "foo", "(", expected_string="(foo)", expected_point=2_int64, &
                       fixture_fn=mark_forward)
        if (allocated(error)) return
        call pair_test(error, "autowrapping-2", "foo", ")", expected_string="(foo)", expected_point=6_int64, &
                       fixture_fn=mark_forward)
        if (allocated(error)) return
        call pair_test(error, "autowrapping-3", "foo", ")", expected_string="(foo)", expected_point=6_int64, &
                       fixture_fn=mark_backward)
        if (allocated(error)) return
        call pair_test(error, "autowrapping-4", "foo", "(", expected_string="(foo)", expected_point=2_int64, &
                       fixture_fn=mark_backward)
        if (allocated(error)) return
        call pair_test(error, "autowrapping-5", "foo", '"', expected_string='"foo"', expected_point=2_int64, &
                       fixture_fn=mark_forward)
        if (allocated(error)) return
        call pair_test(error, "autowrapping-6", "foo", '"', expected_string='"foo"', expected_point=6_int64, &
                       fixture_fn=mark_backward)
        if (allocated(error)) return
        ! autowrapping-multi-1 and -2 (in code; current-prefix-arg 5).
        call pair_test_for("foo", 1_int64, iachar("("), "(((((foo)))))", 6_int64, no_bindings, mark_forward, ok, &
                           prefix=5_int64)
        call check(error, ok, "electric-pair-autowrapping-multi-1-at-point-1-in-fundamental-mode")
        if (allocated(error)) return
        call pair_test_for("foo", 1_int64, iachar(")"), "(((((foo)))))", 14_int64, no_bindings, mark_backward, ok, &
                           prefix=5_int64)
        call check(error, ok, "electric-pair-autowrapping-multi-2-at-point-1-in-fundamental-mode")
    end subroutine test_autowrapping

    ! ------------------------------------------------------------------
    ! String pairs
    ! ------------------------------------------------------------------

    subroutine test_strings_pairs(error)
        type(error_type), allocatable, intent(out) :: error
        call strings_pairs(error, .false., "/**/")
    end subroutine test_strings_pairs

    subroutine test_strings_pairs_with_space(error)
        type(error_type), allocatable, intent(out) :: error
        call strings_pairs(error, .true., "/*  */")
    end subroutine test_strings_pairs_with_space

    !> electric-pair-strings-pairs(-with-space): with the pair ("/\\*" .
    !> "*/") or ("/\\*" " */" t), typing * after / inserts the closer.
    subroutine strings_pairs(error, space, expected)
        type(error_type), allocatable, intent(out) :: error
        logical, intent(in) :: space
        character(len=*), intent(in) :: expected
        type(handle_type) :: h
        type(status_type) :: st
        type(electric_pair_type), allocatable :: saved(:)
        call init_bindings()
        call temp_buffer(h, "")
        call minor_modes%electric_indent_mode%disable(st)
        saved = electric_pair_pairs
        deallocate (electric_pair_pairs)
        allocate (electric_pair_pairs(1))
        electric_pair_pairs(1)%open_string = "/\*"
        if (space) then
            electric_pair_pairs(1)%close_string = " */"
            electric_pair_pairs(1)%space = .true.
        else
            electric_pair_pairs(1)%close_string = "*/"
        end if
        call minor_modes%electric_pair_local_mode%enable(st)
        call insert("/", st)
        last_command_event = make_fixnum(int(iachar("*"), int64))
        call call_interactively(sym("self_insert_command"), st)
        call check(error, buffer_string(), expected)
        call move_alloc(saved, electric_pair_pairs)
        call minor_modes%electric_pair_local_mode%disable(st)
        call minor_modes%electric_indent_mode%enable(st)
        call kill_buffer(h, st)
    end subroutine strings_pairs

    ! ------------------------------------------------------------------
    ! Backspacing, undoing and newlines
    ! ------------------------------------------------------------------

    subroutine test_backspace_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call temp_buffer(h, "()")
        call goto_char(2_int64)
        call electric_pair_delete_pair(1_int64, st)
        call check(error, buffer_string(), "")
        call kill_buffer(h, st)
    end subroutine test_backspace_1

    subroutine test_backspace_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call temp_buffer(h, "((()))")
        call goto_char(4_int64)
        call electric_pair_delete_pair(2_int64, st)
        call check(error, buffer_string(), "()")
        call kill_buffer(h, st)
    end subroutine test_backspace_2

    !> Make sure `electric_pair_mode' does not confuse `undo' (bug#39680).
    subroutine test_undo_unrelated_state(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call temp_buffer(h, "")
        call minor_modes%electric_pair_local_mode%enable(st)
        last_command_event = make_fixnum(int(iachar("("), int64))
        call call_interactively(sym("self_insert_command"), st)
        call undo_boundary()
        last_command_event = make_fixnum(int(iachar("a"), int64))
        call call_interactively(sym("self_insert_command"), st)
        call undo_boundary()
        call undo(nil(), st)
        last_command_event = make_fixnum(int(iachar("("), int64))
        call call_interactively(sym("self_insert_command"), st)
        call check(error, buffer_string(), "(())")
        call minor_modes%electric_pair_local_mode%disable(st)
        call kill_buffer(h, st)
    end subroutine test_undo_unrelated_state

    !> electric-pair-open-extra-newline, in Fundamental mode (upstream:
    !> c-mode, whose indentation is not ported; Fundamental mode indents
    !> the new lines like the previous one).
    subroutine test_open_extra_newline(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call init_bindings()
        call temp_buffer(h, "")
        call minor_modes%electric_pair_mode%enable(st)
        call minor_modes%electric_indent_mode%enable(st)
        call insert("int main {}", st)
        call goto_char(point() - 1)
        call newline(st, 1_int64, interactive=.true.)
        call check(error, buffer_string(), "int main {"//achar(10)//achar(10)//"}")
        if (allocated(error)) return
        call check(error, point() == point_max() - 2, "between the braces")
        call minor_modes%electric_pair_mode%disable(st)
        call kill_buffer(h, st)
    end subroutine test_open_extra_newline

    !> Electric Pair Local mode pairs in one buffer only.
    subroutine test_local_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        call init_bindings()
        call get_buffer_create("*elec-pair-a*", a, st)
        call get_buffer_create("*elec-pair-b*", b, st)
        call set_buffer(a, st)
        call minor_modes%electric_pair_local_mode%enable(st)
        call check(error, minor_modes%electric_pair_local_mode%enabled() .and. &
                   .not. minor_modes%electric_pair_mode%enabled(), "locally on, globally off")
        if (allocated(error)) return
        last_command_event = make_fixnum(int(iachar("("), int64))
        call call_interactively(sym("self_insert_command"), st)
        call check(error, buffer_string(), "()")
        if (allocated(error)) return
        call set_buffer(b, st)
        call call_interactively(sym("self_insert_command"), st)
        call check(error, buffer_string(), "(")
        if (allocated(error)) return
        call set_buffer(a, st)
        call minor_modes%electric_pair_local_mode%disable(st)
        call check(error,.not. minor_modes%electric_pair_local_mode%enabled(), "off again")
        call kill_buffer(a, st)
        call kill_buffer(b, st)
    end subroutine test_local_mode
end module test_elec_pair_fm
