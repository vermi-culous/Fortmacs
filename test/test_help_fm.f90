! test_help_fm.f90 -- tests for help_fm, help_fns_fm and help_mode_fm.
!
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

!> Ports the help-tests.el substitute-command-keys tests that need no
!> faces, keymap summaries or other quoting styles; the rest are own
!> tests, through the command loop.
module test_help_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, t_value, make_fixnum, make_string, make_list
    use lread_fm, only: sym
    use print_fm, only: fortran_repr
    use callint_fm, only: current_prefix_arg
    use files_fm, only: require_final_newline, FINAL_NEWLINE_VISIT, FINAL_NEWLINE_NEVER
    use buffer_fm, only: buffers, cur, get_buffer_create, get_buffer, set_buffer, current_buffer, buffer_modified_p, &
                         auto_fill_function, buffer_live_p
    use frame_fm, only: selected_frame, ensure_frame_windows
    use window_fm, only: windows, window_leaves, delete_other_windows, switch_to_buffer, resize_window_tree
    use editfns_fm, only: buffer_string
    use keymap_fm, only: key_parse, keymap_global_set, keymap_set
    use text_mode_fm, only: text_mode, text_mode_map
    use keyboard_fm, only: unread_command_events, discard_input, last_message, command_loop
    use bindings_fm, only: init_bindings
    use xdisp_fm, only: tab_width
    use isearch_fm, only: search_upper_case, UPPER_CASE_T, UPPER_CASE_NOT_YANKS
    use simple_fm, only: fill_prefix, do_auto_fill
    use help_fm, only: substitute_command_keys, with_help_window, describe_map_tree, lossage_comment_indent, &
                       make_separator_line, register_keymap_name
    use xdisp_fm, only: message, message_nolog
    use keymap_fm, only: make_sparse_keymap, free_keymap
    use lisp_fm, only: make_keymap_ref
    use help_fns_fm, only: describe_function_1, describe_variable_text
    implicit none(type, external)
    private

    public :: collect_help_fm

    character(len=*), parameter :: LQ = char(226)//char(128)//char(152), RQ = char(226)//char(128)//char(153), &
                                   LF = achar(10), TAB = achar(9), FF = achar(12)

contains

    subroutine collect_help_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("help-tests-substitute-command-keys/no-change", test_no_change), &
                    new_unittest("help-tests-substitute-command-keys/commands", test_commands), &
                    new_unittest("help-tests-substitute-command-keys/literal-key-sequence", test_literal_keys), &
                    new_unittest("help-tests-substitute-command-keys/literal-key-sequence-ignore-invalid", &
                                 test_literal_keys_invalid), &
                    new_unittest("help-tests-substitute-command-keys/quotes", test_quotes), &
                    new_unittest("help-tests-substitute-command-keys/literals", test_literals), &
                    new_unittest("help-tests-substitute-command-keys/no-change-2", test_no_change_2), &
                    new_unittest("substitute-command-keys-keymap", test_keymap), &
                    new_unittest("describe-function-command", test_describe_command), &
                    new_unittest("describe-function-api", test_describe_api), &
                    new_unittest("describe-variable", test_describe_variable), &
                    new_unittest("describe-variable-fortran-values", test_variable_values), &
                    new_unittest("describe-function-fortran-file", test_function_file), &
                    new_unittest("fortran-repr", test_fortran_repr), &
                    new_unittest("help-empty-input", test_empty_input), &
                    new_unittest("help-window-and-quit", test_help_window), &
                    new_unittest("describe-key", test_describe_key), &
                    new_unittest("describe-key-undefined", test_describe_key_undefined), &
                    new_unittest("describe-key-binding-locus", test_binding_locus), &
                    new_unittest("describe-key-briefly", test_describe_key_briefly), &
                    new_unittest("where-is", test_where_is), &
                    new_unittest("advertised-binding", test_advertised_binding), &
                    new_unittest("describe-function-keys", test_describe_function_keys), &
                    new_unittest("describe-variable-keys", test_describe_variable_keys), &
                    new_unittest("help-tests-substitute-command-keys/keymaps", test_sck_keymaps), &
                    new_unittest("help-tests-substitute-command-keys/undefined-map", test_sck_undefined_map), &
                    new_unittest("help-tests-describe-map-tree/mention-shadow-t", test_map_tree_shadow_t), &
                    new_unittest("help-tests-describe-map-tree/mention-shadow-nil", test_map_tree_shadow_nil), &
                    new_unittest("help-tests-describe-map-tree/partial-t", test_map_tree_partial_t), &
                    new_unittest("help-tests-describe-map-tree/partial-nil", test_map_tree_partial_nil), &
                    new_unittest("describe-map-tree-ranges-and-prefixes", test_map_tree_ranges), &
                    new_unittest("describe-bindings", test_describe_bindings), &
                    new_unittest("describe-mode", test_describe_mode), &
                    new_unittest("help-fns-test-describe-symbol", test_describe_symbol), &
                    new_unittest("describe-command", test_describe_command_x), &
                    new_unittest("describe-variable-customize", test_describe_variable_customize), &
                    new_unittest("view-lossage", test_view_lossage), &
                    new_unittest("lossage-comment-indent", test_lossage_indent), &
                    new_unittest("view-echo-area-messages", test_view_messages), &
                    new_unittest("messages-log-duplicates", test_messages_duplicates), &
                    new_unittest("help-for-help", test_help_for_help), &
                    new_unittest("help-for-help-unbound-key", test_help_for_help_unbound) &
                    ]
    end subroutine collect_help_fm

    !> A fresh *scratch* with the default bindings.
    subroutine fresh_scratch()
        type(handle_type) :: buf
        type(status_type) :: st
        call init_bindings()
        call discard_input()
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        ! One window, showing *scratch*.
        call ensure_frame_windows()
        call delete_other_windows(selected_frame%selected_window, st)
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call switch_to_buffer("*scratch*", st)
        last_message = ""
    end subroutine fresh_scratch

    !> The text of *Help*.
    function help_text() result(text)
        character(len=:), allocatable :: text
        type(handle_type) :: old
        type(status_type) :: st
        old = current_buffer()
        call set_buffer(get_buffer("*Help*"), st)
        text = buffer_string()
        call set_buffer(old, st)
    end function help_text

    !> The number of windows on the frame.
    integer function window_count()
        type(handle_type), allocatable :: leaves(:)
        call window_leaves(selected_frame%root_window, leaves)
        window_count = size(leaves)
    end function window_count

    !> The name of the buffer shown in the selected window.
    function selected_buffer_name() result(name)
        character(len=:), allocatable :: name
        name = buffers(windows(selected_frame%selected_window%index)%buffer%index)%name
    end function selected_buffer_name

    !> Queue `keys` (a key_valid_p string) as input.
    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        call key_parse(keys, evs, st)
        call unread_command_events(evs)
    end subroutine type_keys

    !> Check that substitute-command-keys turns `input` into `want`.
    subroutine test_sck(error, input, want)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: input, want
        character(len=:), allocatable :: got
        got = substitute_command_keys(input)
        call check(error, got == want .and. len(got) == len(want), input//" gave "//got//", want "//want)
    end subroutine test_sck

    subroutine test_no_change(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "foo", "foo")
        if (allocated(error)) return
        call test_sck(error, "\invalid-escape", "\invalid-escape")
    end subroutine test_no_change

    !> As upstream, but with forward-char for goto-char (on M-g c there).
    subroutine test_commands(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "foo \[forward_char]", "foo C-f")
        if (allocated(error)) return
        call test_sck(error, "\[next_line]", "C-n")
        if (allocated(error)) return
        call test_sck(error, "\[next_line]"//LF//"\[next_line]", "C-n"//LF//"C-n")
        if (allocated(error)) return
        call test_sck(error, "\[next_line]\[previous_line]", "C-nC-p")
        if (allocated(error)) return
        call test_sck(error, "\[next_line]\=\[previous_line]", "C-n\[previous_line]")
        if (allocated(error)) return
        call test_sck(error, "\[next_line]`foo'", "C-n"//LQ//"foo"//RQ)
        if (allocated(error)) return
        call test_sck(error, "\[emacs-version]", "M-x emacs-version")
        if (allocated(error)) return
        call test_sck(error, "\[emacs-version]\[next_line]", "M-x emacs-versionC-n")
        if (allocated(error)) return
        call test_sck(error, "\[emacs-version]`foo'", "M-x emacs-version"//LQ//"foo"//RQ)
    end subroutine test_commands

    subroutine test_literal_keys(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "\`C-m'", "C-m")
        if (allocated(error)) return
        call test_sck(error, "\`C-m'\`C-j'", "C-mC-j")
        if (allocated(error)) return
        call test_sck(error, "foo\`C-m'bar\`C-j'baz", "fooC-mbarC-jbaz")
        if (allocated(error)) return
        call test_sck(error, "\`M-x next-line'", "M-x next-line")
        if (allocated(error)) return
        call test_sck(error, "\`mouse-1'", "mouse-1")
    end subroutine test_literal_keys

    subroutine test_literal_keys_invalid(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "ab\`'cd", "ab\"//LQ//RQ//"cd")
        if (allocated(error)) return
        call test_sck(error, "\`c-c'", "\"//LQ//"c-c"//RQ)
        if (allocated(error)) return
        call test_sck(error, "\`<foo bar baz>'", "\"//LQ//"<foo bar baz>"//RQ)
    end subroutine test_literal_keys_invalid

    !> The text-quoting-style `curve` cases.
    subroutine test_quotes(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "quotes "//LQ//"like this"//RQ, "quotes "//LQ//"like this"//RQ)
        if (allocated(error)) return
        call test_sck(error, "`x'", LQ//"x"//RQ)
        if (allocated(error)) return
        call test_sck(error, "`", LQ)
        if (allocated(error)) return
        call test_sck(error, "'", RQ)
        if (allocated(error)) return
        call test_sck(error, "\`", "\"//LQ)
    end subroutine test_quotes

    subroutine test_literals(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "foo \=\[goto_char]", "foo \[goto_char]")
        if (allocated(error)) return
        call test_sck(error, "foo \=\=", "foo \=")
        if (allocated(error)) return
        call test_sck(error, "\=\=", "\=")
        if (allocated(error)) return
        call test_sck(error, "\=\[", "\[")
        if (allocated(error)) return
        call test_sck(error, "\=`x\='", "`x'")
    end subroutine test_literals

    subroutine test_no_change_2(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "\[foobar", "\[foobar")
        if (allocated(error)) return
        call test_sck(error, "\=", "\=")
    end subroutine test_no_change_2

    !> \<MAP> makes \[COMMAND] look in MAP (and the global map).
    subroutine test_keymap(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "\[next_history_element]", "M-x next_history_element")
        if (allocated(error)) return
        call test_sck(error, "\<minibuffer_local_map>\[next_history_element]", "M-n")
        if (allocated(error)) return
        call test_sck(error, "\<help_mode_map>\[quit_window]", "q")
        if (allocated(error)) return
        call test_sck(error, "\<no-such-map>\[forward_char]", "C-f")
    end subroutine test_keymap

    subroutine test_describe_command(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        call fresh_scratch()
        call describe_function_1("forward_char", text)
        call check(error, index(text, "an interactive Fortran subroutine in "//LQ//"cmds_fm.f90"//RQ//"."//LF//LF) == 1, &
                   text)
        if (allocated(error)) return
        call check(error, index(text, "It is bound to C-f."//LF) > 0, text)
        if (allocated(error)) return
        call check(error, index(text, "Move point N characters forward (backward if N is negative).") > 0, text)
        if (allocated(error)) return
        ! \[right_char] became a key, and the citation is gone.
        call check(error, index(text, "This is in contrast with"//LF//"<right>, which see.") > 0 .and. &
                   index(text, "Port of") == 0, text)
        if (allocated(error)) return
        ! An apostrophe becomes a right quote.
        ! Ordinary text characters are not listed (help-fns--key-bindings).
        call describe_function_1("self_insert_command", text)
        call check(error, index(text, LF//LF//"It is bound to many ordinary text characters."//LF//LF) > 0, text)
        if (allocated(error)) return
        ! The last key follows " and ".
        call describe_function_1("kill_ring_save", text)
        call check(error, index(text, LF//LF//"It is bound to <copy>, C-<insertchar>, C-<insert> and M-w."//LF//LF) > 0, &
                   text)
        if (allocated(error)) return
        call describe_function_1("help_quit", text)
        call check(error, index(text, "Just exit from the Help command's") == 0 .and. &
                   index(text, "Just exit from the Help command"//RQ//"s command loop.") > 0, text)
    end subroutine test_describe_command

    subroutine test_describe_api(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        call fresh_scratch()
        call describe_function_1("insert", text)
        call check(error, index(text, "a Fortran subroutine in "//LQ//"insdel_fm.f90"//RQ//"."//LF//LF// &
                                "call insert(") == 1, text)
        if (allocated(error)) return
        call describe_function_1("point", text)
        call check(error, index(text, "a Fortran function in "//LQ//"editfns_fm.f90"//RQ) == 1 .and. &
                   index(text, LF//"point()"//LF) > 0, text)
        if (allocated(error)) return
        call describe_function_1("no-such-function", text)
        call check(error, index(text, "Not documented.") > 0, text)
    end subroutine test_describe_api

    subroutine test_describe_variable(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        logical :: found
        integer :: saved
        call fresh_scratch()
        if (allocated(fill_prefix%value)) deallocate (fill_prefix%value)
        saved = tab_width%value
        tab_width = 4
        call describe_variable_text("tab_width", text, found)
        tab_width = saved
        call check(error, found .and. index(text, "tab_width is a variable defined in "//LQ//"xdisp_fm.f90"//RQ// &
                                            "."//LF//LF//"Its value is 4"//LF//"Original value was 8"//LF// &
                                            "Its Fortran name is "//LQ//"tab_width"//RQ//"."//LF//LF// &
                                            "Distance between tab stops") == 1, text)
        if (allocated(error)) return
        call describe_variable_text("tab_width", text, found)
        call check(error, index(text, "Original value") == 0, text)
        if (allocated(error)) return
        call describe_variable_text("search_upper_case", text, found)
        call check(error, index(text, "Its value is UPPER_CASE_NOT_YANKS"//LF) > 0, text)
        if (allocated(error)) return
        call describe_variable_text("completion_ignored_extensions", text, found)
        call check(error, index(text, "Its value is"//LF//"["".o"", ""~"", ") > 0, text)
        if (allocated(error)) return
        call describe_variable_text("current_prefix_arg", text, found)
        call check(error, index(text, "Its value is .false."//LF) > 0, text)
        if (allocated(error)) return
        call describe_variable_text("no-such-variable", text, found)
        call check(error,.not. found)
        if (allocated(error)) return
        ! An original value of 20 characters or more starts on the next
        ! line.
        search_upper_case = UPPER_CASE_T
        call describe_variable_text("search_upper_case", text, found)
        search_upper_case = UPPER_CASE_NOT_YANKS
        call check(error, index(text, "Its value is UPPER_CASE_T"//LF// &
                                "Original value was "//LF//"UPPER_CASE_NOT_YANKS"//LF) > 0, text)
        if (allocated(error)) return
        ! A value fits on the "Its value is" line when it is shorter than
        ! 56 characters.
        ! (fill_prefix is unset: unallocated.)
        fill_prefix = repeat("x", 53)
        call describe_variable_text("fill_prefix", text, found)
        call check(error, index(text, "Its value is """//repeat("x", 53)//""""//LF) > 0, text)
        if (allocated(error)) return
        fill_prefix = repeat("x", 54)
        call describe_variable_text("fill_prefix", text, found)
        deallocate (fill_prefix%value)
        call check(error, index(text, "Its value is"//LF//""""//repeat("x", 54)//""""//LF) > 0, text)
    end subroutine test_describe_variable

    !> C-h v shows every kind of setting as Fortran: logicals, named
    !> constants of enumerated integer settings, unallocated strings,
    !> procedure settings by name or null(), and no value line for a
    !> hook (no printed form yet).
    subroutine test_variable_values(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        logical :: found
        type(value_type) :: elems(1)
        call fresh_scratch()
        call describe_variable_text("case_fold_search", text, found)
        call check(error, index(text, "Its value is .true."//LF) > 0, text)
        if (allocated(error)) return
        call describe_variable_text("kill_read_only_ok", text, found)
        call check(error, index(text, "Its value is .false."//LF) > 0, text)
        if (allocated(error)) return
        ! An enumerated integer setting: its named constant, and the one
        ! it started with.
        require_final_newline = FINAL_NEWLINE_VISIT
        call describe_variable_text("require_final_newline", text, found)
        require_final_newline = FINAL_NEWLINE_NEVER
        call check(error, index(text, "Its value is FINAL_NEWLINE_VISIT"//LF// &
                                "Original value was FINAL_NEWLINE_NEVER"//LF) > 0, text)
        if (allocated(error)) return
        call describe_variable_text("fill_prefix", text, found)
        call check(error, index(text, "Its value is unallocated"//LF) > 0, text)
        if (allocated(error)) return
        auto_fill_function%value => null()
        call describe_variable_text("auto_fill_function", text, found)
        call check(error, index(text, "Its value is null()"//LF) > 0, text)
        if (allocated(error)) return
        auto_fill_function%value => do_auto_fill
        call describe_variable_text("auto_fill_function", text, found)
        auto_fill_function%value => null()
        call check(error, index(text, "Its value is do_auto_fill"//LF) > 0, text)
        if (allocated(error)) return
        call describe_variable_text("indent_line_function", text, found)
        call check(error, index(text, "Its value is indent_relative"//LF) > 0, text)
        if (allocated(error)) return
        ! A C-u prefix argument.
        elems(1) = make_fixnum(4_int64)
        current_prefix_arg = make_list(elems)
        call describe_variable_text("current_prefix_arg", text, found)
        current_prefix_arg = nil()
        call check(error, index(text, "Its value is [4]"//LF) > 0, text)
        if (allocated(error)) return
        ! A hook has no printed form yet: the documentation follows the
        ! Fortran name.
        ! A deferred-length string variable is documented too.
        call describe_variable_text("search_whitespace_regexp", text, found)
        call check(error, found .and. index(text, "Its Fortran name is "//LQ//"search_whitespace_regexp"//RQ) > 0, text)
        if (allocated(error)) return
        call describe_variable_text("text_mode_hook", text, found)
        call check(error, found .and. index(text, "Its value") == 0 .and. index(text, "Original value") == 0 .and. &
                   index(text, "Its Fortran name is "//LQ//"text_mode_hook"//RQ//"."//LF//LF//"Normal hook run") > 0, text)
    end subroutine test_variable_values

    !> C-h f names the Fortran source file, as upstream names the Lisp
    !> or C one ("kill-line is an interactive byte-compiled Lisp function
    !> in `simple.el'.").
    subroutine test_function_file(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        call fresh_scratch()
        call describe_function_1("kill_line", text, "kill_line is ")
        call check(error, index(text, "kill_line is an interactive Fortran subroutine in "//LQ//"simple_fm.f90"//RQ// &
                                "."//LF//LF) == 1, text)
        if (allocated(error)) return
        call describe_function_1("fill_paragraph", text, "fill_paragraph is ")
        call check(error, index(text, "fill_paragraph is an interactive Fortran subroutine in "//LQ//"fill_fm.f90"// &
                                RQ//".") == 1, text)
        if (allocated(error)) return
        ! A function (not a subroutine) of the config API.
        call describe_function_1("looking_at", text, "looking_at is ")
        call check(error, index(text, "looking_at is a Fortran function in "//LQ//"search_fm.f90"//RQ//".") == 1, text)
        if (allocated(error)) return
        ! Each command shows its own docstring (normal_mode once showed
        ! save_buffer's), an alias that of the command it runs, and
        ! procedures documented in a submodule theirs.
        call describe_function_1("normal_mode", text)
        call check(error, index(text, "Choose the major mode for this buffer automatically.") > 0 .and. &
                   index(text, "Save current buffer") == 0, text)
        if (allocated(error)) return
        call describe_function_1("join_line", text)
        call check(error, index(text, "Join this line to previous") > 0, text)
        if (allocated(error)) return
        call describe_function_1("indent_relative", text)
        call check(error, index(text, "Space out to under next indent point") > 0, text)
        if (allocated(error)) return
        call describe_function_1("point", text)
        call check(error, index(text, "Return value of point, as an integer.") > 0, text)
    end subroutine test_function_file

    !> Values written as Fortran.
    subroutine test_fortran_repr(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: elems(3)
        call check(error, fortran_repr(nil()) == ".false." .and. fortran_repr(t_value()) == ".true.")
        if (allocated(error)) return
        call check(error, fortran_repr(make_fixnum(-12_int64)) == "-12", fortran_repr(make_fixnum(-12_int64)))
        if (allocated(error)) return
        call check(error, fortran_repr(make_string('say "hi"')) == '"say ""hi"""', fortran_repr(make_string('say "hi"')))
        if (allocated(error)) return
        call check(error, fortran_repr(sym("UPPER_CASE_T")) == "UPPER_CASE_T")
        if (allocated(error)) return
        elems(1) = make_fixnum(1_int64)
        elems(2) = make_string("a")
        elems(3) = nil()
        call check(error, fortran_repr(make_list(elems)) == '[1, "a", .false.]', fortran_repr(make_list(elems)))
    end subroutine test_fortran_repr

    !> Empty input to C-h f, C-h v and C-h w is an error.
    subroutine test_empty_input(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h f RET")
        call command_loop(st)
        call check(error, last_message, "You didn"//RQ//"t specify a function symbol")
        if (allocated(error)) return
        call type_keys("C-h v RET")
        call command_loop(st)
        call check(error, last_message, "You didn"//RQ//"t specify a variable")
        if (allocated(error)) return
        call type_keys("C-h w RET")
        call command_loop(st)
        call check(error, last_message, "No command")
    end subroutine test_empty_input

    !> *Help* is read-only, in Help mode; q goes back.
    subroutine test_help_window(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: help
        logical :: modified
        call fresh_scratch()
        call with_help_window("Some help."//LF, st)
        ! The help buffer pops up in a new window below; the selected
        ! window and the current buffer stay.
        call check(error, st%ok() .and. buffers(cur)%name == "*scratch*" .and. window_count() == 2 .and. &
                   selected_buffer_name() == "*scratch*", buffers(cur)%name)
        if (allocated(error)) return
        help = get_buffer("*Help*")
        call check(error, buffers(help%index)%read_only .and. help_text() == "Some help."//LF, help_text())
        if (allocated(error)) return
        call check(error, last_message == "Type C-x 1 to delete the help window.", last_message)
        if (allocated(error)) return
        call buffer_modified_p(help, modified, st)
        call check(error,.not. modified, "*Help* is unmodified")
        if (allocated(error)) return
        ! q in the help window deletes it.
        call type_keys("C-x o")
        call command_loop(st)
        call check(error, selected_buffer_name() == "*Help*", selected_buffer_name())
        if (allocated(error)) return
        call type_keys("q")
        call command_loop(st)
        call check(error, buffers(cur)%name == "*scratch*" .and. window_count() == 1, buffers(cur)%name)
    end subroutine test_help_window

    subroutine test_describe_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h k C-f")
        call command_loop(st)
        call check(error, buffers(cur)%name == "*scratch*" .and. window_count() == 2, buffers(cur)%name)
        if (allocated(error)) return
        ! The line is filled (describe-function-1).
        call check(error, index(help_text(), "C-f runs the command forward_char (found in global_map), which is "// &
                                "an"//LF//"interactive Fortran subroutine in "//LQ//"cmds_fm.f90"//RQ//"."//LF//LF) == 1, &
                   help_text())
        if (allocated(error)) return
        call type_keys("<f1> k C-x C-f")
        call command_loop(st)
        call check(error, index(help_text(), "C-x C-f runs the command find_file (found in global_map)") == 1, &
                   help_text())
        if (allocated(error)) return
        ! The help window shows *Help* again: no message.
        call check(error, window_count() == 2, "two windows")
        if (allocated(error)) return
        call type_keys("C-x o q")
        call command_loop(st)
        call check(error, buffers(cur)%name == "*scratch*", buffers(cur)%name)
    end subroutine test_describe_key

    subroutine test_describe_key_undefined(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h k C-x z")
        call command_loop(st)
        call check(error, last_message == "C-x z is undefined" .and. buffers(cur)%name == "*scratch*", last_message)
        if (allocated(error)) return
        ! A key bound to `undefined' is undefined too
        ! (help--binding-undefined-p).
        call keymap_global_set("C-c z", "undefined", st)
        call type_keys("C-h k C-c z")
        call command_loop(st)
        call check(error, last_message == "C-c z is undefined" .and. buffers(cur)%name == "*scratch*", last_message)
        if (allocated(error)) return
        call type_keys("C-h c C-c z")
        call command_loop(st)
        call check(error, last_message == "C-c z is undefined", last_message)
    end subroutine test_describe_key_undefined

    !> help--binding-locus names the major mode's map.
    subroutine test_binding_locus(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call switch_to_buffer("*locus*", st)
        call text_mode(st)
        call keymap_set(text_mode_map, "C-c t", "forward_char", st)
        call type_keys("C-h k C-c t")
        call command_loop(st)
        call check(error, index(help_text(), "C-c t runs the command forward_char (found in text_mode_map)") == 1, &
                   help_text())
        call keymap_set(text_mode_map, "C-c t", nil(), st)
        call fresh_scratch()
    end subroutine test_binding_locus

    !> bindings.el's :advertised-binding of undo and set-mark-command.
    subroutine test_advertised_binding(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "\[undo]", "C-x u")
        if (allocated(error)) return
        call test_sck(error, "\[set_mark_command]", "C-SPC")
        if (allocated(error)) return
        call test_sck(error, "\[undo_redo]", "C-M-_")
    end subroutine test_advertised_binding

    subroutine test_describe_key_briefly(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h c C-n")
        call command_loop(st)
        call check(error, last_message == "C-n runs the command next_line", last_message)
    end subroutine test_describe_key_briefly

    subroutine test_where_is(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h w f o r w a r d _ c h a r RET")
        call command_loop(st)
        call check(error, last_message == "forward_char is on C-f", last_message)
        if (allocated(error)) return
        call type_keys("C-h w h e l p _ q u i t RET")
        call command_loop(st)
        call check(error, last_message == "help_quit is on C-h q, <f1> q, <help> q", last_message)
    end subroutine test_where_is

    subroutine test_describe_function_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h f i n s e r t RET")
        call command_loop(st)
        call check(error, window_count() == 2 .and. &
                   index(help_text(), "insert is a Fortran subroutine in "//LQ//"insdel_fm.f90"//RQ) == 1, &
                   help_text())
        ! Back to one window.
        call fresh_scratch()
    end subroutine test_describe_function_keys

    subroutine test_describe_variable_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h v t a b _ w i d t h RET")
        call command_loop(st)
        call check(error, window_count() == 2 .and. &
                   index(help_text(), "tab_width is a variable defined in "//LQ//"xdisp_fm.f90"//RQ) == 1, &
                   help_text())
        ! Back to one window.
        call fresh_scratch()
    end subroutine test_describe_variable_keys
    subroutine test_sck_keymaps(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(status_type) :: st
        character(len=24), parameter :: keys(21) = [character(len=24) :: "C-g", "TAB", "C-j", "RET", "SPC", "?", &
                                                                  "C-<tab>", "<XF86Back>", "<XF86Forward>", "<backtab>", "<down>", &
                                                                   "<next>", "<prior>", "<up>", "M-v", "M-<", "M-n", "M-p", "M-r", &
                                                                                                                   "M-s", "M-g M-c"]
        character(len=40), parameter :: defs(21) = [character(len=40) :: "abort_minibuffers", "minibuffer_complete", &
                                                                   "minibuffer_complete_and_exit", "minibuffer_complete_and_exit", &
                                                                         "minibuffer_complete_word", "minibuffer_completion_help", &
                                                                     "file_cache_minibuffer_complete", "previous_history_element", &
                                                                                    "next_history_element", "minibuffer_complete", &
                                                                           "next_line_or_history_element", "next_history_element", &
                                                                      "switch_to_completions", "previous_line_or_history_element", &
                                                                        "switch_to_completions", "minibuffer_beginning_of_buffer", &
                                                                               "next_history_element", "previous_history_element", &
                                                                                              "previous_matching_history_element", &
                                                                           "next_matching_history_element", "switch_to_completions"]
        integer :: k
        call fresh_scratch()
        call make_sparse_keymap(map)
        do k = 1, size(keys)
            call keymap_set(map, trim(keys(k)), sym(trim(defs(k))), st)
        end do
        call register_keymap_name("help_tests_test_keymap", map)
        call test_sck(error, "\{help_tests_test_keymap}", LF//"Key             Binding"//LF//sep()// &
                      "C-g"//TAB//TAB//"abort_minibuffers"//LF//"TAB"//TAB//TAB//"minibuffer_complete"//LF// &
                      "C-j"//TAB//TAB//"minibuffer_complete_and_exit"//LF//"RET"//TAB//TAB//"minibuffer_complete_and_exit"// &
                      LF//"SPC"//TAB//TAB//"minibuffer_complete_word"//LF//"?"//TAB//TAB//"minibuffer_completion_help"//LF// &
                      "C-<tab>"//TAB//TAB//"file_cache_minibuffer_complete"//LF// &
                      "<XF86Back>"//TAB//"previous_history_element"//LF//"<XF86Forward>"//TAB//"next_history_element"//LF// &
                      "<backtab>"//TAB//"minibuffer_complete"//LF//"<down>"//TAB//TAB//"next_line_or_history_element"//LF// &
                      "<next>"//TAB//TAB//"next_history_element"//LF//"<prior>"//TAB//TAB//"switch_to_completions"//LF// &
                      "<up>"//TAB//TAB//"previous_line_or_history_element"//LF//LF// &
                      "M-<"//TAB//TAB//"minibuffer_beginning_of_buffer"//LF//"M-n"//TAB//TAB//"next_history_element"//LF// &
                      "M-p"//TAB//TAB//"previous_history_element"//LF//"M-r"//TAB//TAB//"previous_matching_history_element"// &
                      LF//"M-s"//TAB//TAB//"next_matching_history_element"//LF//"M-v"//TAB//TAB//"switch_to_completions"// &
                      LF//LF//"M-g M-c"//TAB//TAB//"switch_to_completions"//LF)
        call free_keymap(map, st)
    end subroutine test_sck_keymaps

    subroutine test_sck_undefined_map(error)
        type(error_type), allocatable, intent(out) :: error
        call test_sck(error, "\{foobar_map}", LF//"Uses keymap "//LQ//"foobar_map"//RQ//", which is not currently defined."//LF)
    end subroutine test_sck_undefined_map

    !> The text of help--describe-map-tree for `map' (title "" as nil).
    function map_tree_text(map, partial, shadow, mention) result(text)
        type(handle_type), intent(in) :: map
        logical, intent(in) :: partial, mention
        type(value_type), intent(in) :: shadow(:)
        character(len=:), allocatable :: text
        text = ""
        call describe_map_tree(map, partial, shadow, "", .false., text, mention_shadow=mention)
    end function map_tree_text

    !> The separator line: dashes without a display that underlines (as
    !> in tests run alone), else a newline.
    function sep() result(s)
        character(len=:), allocatable :: s
        s = make_separator_line()
    end function sep

    subroutine shadow_maps(map, shadow)
        type(handle_type), intent(out) :: map
        type(value_type), allocatable, intent(out) :: shadow(:)
        type(handle_type) :: s
        type(status_type) :: st
        call make_sparse_keymap(map)
        call keymap_set(map, "C-a", sym("foo"), st)
        call keymap_set(map, "C-b", sym("bar"), st)
        call make_sparse_keymap(s)
        call keymap_set(s, "C-a", sym("baz"), st)
        allocate (shadow(1))
        shadow(1) = make_keymap_ref(s)
    end subroutine shadow_maps

    subroutine test_map_tree_shadow_t(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(value_type), allocatable :: shadow(:)
        character(len=:), allocatable :: text
        type(status_type) :: st
        call fresh_scratch()
        call shadow_maps(map, shadow)
        text = map_tree_text(map, .true., shadow, .true.)
        call check(error, text, LF//"Key             Binding"//LF//sep()//"C-a"//TAB//TAB//"foo"//LF// &
                   "  (this binding is currently shadowed)"//LF//"C-b"//TAB//TAB//"bar"//LF)
        call free_keymap(map, st)
    end subroutine test_map_tree_shadow_t

    subroutine test_map_tree_shadow_nil(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(value_type), allocatable :: shadow(:)
        character(len=:), allocatable :: text
        type(status_type) :: st
        call fresh_scratch()
        call shadow_maps(map, shadow)
        text = map_tree_text(map, .true., shadow, .false.)
        call check(error, text, LF//"Key             Binding"//LF//sep()//"C-b"//TAB//TAB//"bar"//LF)
        call free_keymap(map, st)
    end subroutine test_map_tree_shadow_nil

    subroutine test_map_tree_partial_t(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(value_type), allocatable :: none(:)
        type(status_type) :: st
        call fresh_scratch()
        allocate (none(0))
        call make_sparse_keymap(map)
        call keymap_set(map, "C-a", sym("foo"), st)
        call keymap_set(map, "C-b", sym("undefined"), st)
        call check(error, map_tree_text(map, .true., none, .false.), &
                   LF//"Key             Binding"//LF//sep()//"C-a"//TAB//TAB//"foo"//LF)
        call free_keymap(map, st)
    end subroutine test_map_tree_partial_t

    subroutine test_map_tree_partial_nil(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(value_type), allocatable :: none(:)
        type(status_type) :: st
        call fresh_scratch()
        allocate (none(0))
        call make_sparse_keymap(map)
        call keymap_set(map, "C-a", sym("foo"), st)
        call keymap_set(map, "C-b", sym("undefined"), st)
        call check(error, map_tree_text(map, .false., none, .false.), &
                   LF//"Key             Binding"//LF//sep()//"C-a"//TAB//TAB//"foo"//LF//"C-b"//TAB//TAB//"undefined"//LF)
        call free_keymap(map, st)
    end subroutine test_map_tree_partial_nil

    !> Runs of characters bound alike, prefix maps after the map itself
    !> (accessible-keymaps), keys too wide for column 16, the natural
    !> order of function keys.
    subroutine test_map_tree_ranges(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: map
        type(value_type), allocatable :: none(:)
        type(status_type) :: st
        call fresh_scratch()
        allocate (none(0))
        call make_sparse_keymap(map)
        call keymap_set(map, "a", sym("self_insert_command"), st)
        call keymap_set(map, "b", sym("self_insert_command"), st)
        call keymap_set(map, "c", sym("self_insert_command"), st)
        call keymap_set(map, "<f11>", sym("eleven"), st)
        call keymap_set(map, "<f2>", sym("two"), st)
        call keymap_set(map, "C-c C-x", sym("ccx"), st)
        call keymap_set(map, "C-c <some-long-key-name>", sym("long"), st)
        call check(error, map_tree_text(map, .true., none, .false.), &
                   LF//"Key             Binding"//LF//sep()//"a .. c"//TAB//TAB//"self_insert_command"//LF// &
                   "<f2>"//TAB//TAB//"two"//LF//"<f11>"//TAB//TAB//"eleven"//LF//LF// &
                   "C-c C-x"//repeat(TAB, 4)//"ccx"//LF//"C-c <some-long-key-name>"//TAB//"long"//LF)
        call free_keymap(map, st)
    end subroutine test_map_tree_ranges

    !> C-h b: the major mode's map, then the global map, its bindings
    !> shadowed by the major mode's map mentioned.
    subroutine test_describe_bindings(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer :: k1, k2
        call fresh_scratch()
        call switch_to_buffer("*bindings*", st)
        call text_mode(st)
        call keymap_set(text_mode_map, "C-f", "backward_char", st)
        call type_keys("C-h b")
        call command_loop(st)
        call keymap_set(text_mode_map, "C-f", nil(), st)
        text = help_text()
        k1 = index(text, FF//LF//LQ//"text_mode"//RQ//" Major Mode Bindings:"//LF//LF//"Key             Binding"//LF// &
                   sep())
        k2 = index(text, FF//LF//"Global Bindings:"//LF//LF//"Key             Binding"//LF//sep())
        call check(error, k1 == 1 .and. k2 > k1, text)
        if (allocated(error)) return
        call check(error, index(text, LF//"C-f"//TAB//TAB//"backward_char"//LF) > k1 .and. &
                   index(text, LF//"C-f"//TAB//TAB//"forward_char"//LF//"  (this binding is currently shadowed)"//LF) > k2 &
                   .and. index(text, LF//"C-x C-f"//TAB//TAB//"find_file"//LF) > k2, text)
        if (allocated(error)) return
        call check(error, index(text, LF//"SPC .. ~"//TAB//"self_insert_command"//LF) > k2 .and. &
                   index(text, LF//"C-h b"//TAB//TAB//"describe_bindings"//LF) > k2, text)
        call fresh_scratch()
    end subroutine test_describe_bindings

    !> C-h m: the major mode, then the local and global minor modes, each
    !> under its heading.
    subroutine test_describe_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text
        call fresh_scratch()
        call switch_to_buffer("*mode*", st)
        call text_mode(st)
        call type_keys("M-x a u t o _ f i l l _ m o d e RET C-h m")
        call command_loop(st)
        text = help_text()
        call check(error, index(text, "Major mode text_mode"//LF//LF//"The major mode is Text mode defined in "// &
                                "text_mode_fm.f90:"//LF//LF//"Major mode for editing text written for humans to read.") == 1, &
                   text)
        if (allocated(error)) return
        call check(error, index(text, "Turning on Text mode runs the normal hook "//LQ//"text_mode_hook"//RQ//"."//LF//LF// &
                                "Local minor mode auto_fill_mode"//LF//LF//"Auto_Fill minor mode (indicator Fill):"//LF// &
                                "Toggle automatic line breaking (Auto Fill mode).") > 0, text)
        if (allocated(error)) return
        call check(error, index(text, LF//LF//"Global minor mode transient_mark_mode"//LF//LF// &
                                "Transient_Mark minor mode (no indicator):"//LF//"Toggle Transient Mark mode.") > 0 .and. &
                   index(text, "Local minor mode indent_tabs_mode") > index(text, "Local minor mode auto_fill_mode"), text)
        if (allocated(error)) return
        ! The text ends with one newline.
        call check(error, text(len(text) - 1:) /= LF//LF, text)
        call fresh_scratch()
    end subroutine test_describe_mode

    !> describe-symbol shows a function and a variable of that name, with
    !> a separator line between them.
    subroutine test_describe_symbol(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text
        call fresh_scratch()
        call type_keys("C-h o t r a n s i e n t _ m a r k _ m o d e RET")
        call command_loop(st)
        text = help_text()
        call check(error, index(text, "transient_mark_mode is an interactive Fortran subroutine") == 1 .and. &
                   index(text, LF//LF//sep()//LF//"transient_mark_mode is a variable defined in ") > 0, text)
        if (allocated(error)) return
        call type_keys("C-h o f i l l _ c o l u m n RET")
        call command_loop(st)
        call check(error, index(help_text(), "fill_column is a variable defined in ") == 1, help_text())
        if (allocated(error)) return
        call type_keys("C-h o RET")
        call command_loop(st)
        call check(error, last_message, "You didn"//RQ//"t specify a function or variable")
        call fresh_scratch()
    end subroutine test_describe_symbol

    subroutine test_describe_command_x(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h x f o r w a r d _ c h a r RET")
        call command_loop(st)
        call check(error, index(help_text(), "forward_char is an interactive Fortran subroutine") == 1, help_text())
        if (allocated(error)) return
        ! Functions that are not commands don't complete.
        call type_keys("C-h x RET")
        call command_loop(st)
        call check(error, last_message, "You didn"//RQ//"t specify a command"//RQ//"s symbol")
        call fresh_scratch()
    end subroutine test_describe_command_x

    !> C-h v: the indented lines after the docstring
    !> (help-fns-describe-variable-functions).
    subroutine test_describe_variable_customize(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        logical :: found
        call fresh_scratch()
        call describe_variable_text("fill_column", text, found)
        call check(error, index(text, "C-x f."//LF//LF//"  Automatically becomes buffer-local when set."//LF// &
                                "  You can customize this variable."//LF) > 0, text)
        if (allocated(error)) return
        call describe_variable_text("case_replace", text, found)
        call check(error, index(text, LF//LF//"  You can customize this variable."//LF) > 0 .and. &
                   index(text, "buffer-local") == 0, text)
        if (allocated(error)) return
        ! Not a user option.
        call describe_variable_text("inhibit_modification_hooks", text, found)
        call check(error, index(text, "customize") == 0, text)
    end subroutine test_describe_variable_customize

    !> C-h l: the recent keys, each command after its keys as a comment
    !> at column 24.
    subroutine test_view_lossage(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text
        call fresh_scratch()
        call type_keys("a C-b C-f C-x h C-h l")
        call command_loop(st)
        text = help_text()
        call check(error, index(text, LF//" a"//TAB//TAB//TAB//";; self_insert_command"//LF// &
                                " C-b"//TAB//TAB//TAB//";; backward_char"//LF// &
                                " C-f"//TAB//TAB//TAB//";; forward_char"//LF// &
                                " C-x h"//TAB//TAB//TAB//";; mark_whole_buffer"//LF// &
                                " C-h l"//TAB//TAB//TAB//";; view_lossage"//LF) > 0, text)
        if (allocated(error)) return
        ! The text ends with the last command.
        call check(error, text(len(text) - len(";; view_lossage"//LF) + 1:) == ";; view_lossage"//LF, text)
        call fresh_scratch()
    end subroutine test_view_lossage

    !> newcomment.el's comment-indent as view-lossage uses it: code past
    !> the comment column puts the comment one space after it, and the
    !> next line aligns with it (comment-choose-indent).
    subroutine test_lossage_indent(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        text = " a b c d e f g h i j k l m n ;; x"//LF//" C-f ;; y"//LF
        call lossage_comment_indent(text)
        call check(error, text, " a b c d e f g h i j k l m n ;; x"//LF//" C-f"//TAB//TAB//TAB//"     ;; y"//LF)
    end subroutine test_lossage_indent

    !> C-h e: the messages logged in *Messages*, at its end.
    subroutine test_view_messages(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text
        call fresh_scratch()
        call message("First message")
        call message("Second message")
        call message_nolog("Not logged")
        call type_keys("C-h e")
        call command_loop(st)
        text = buf_text("*Messages*")
        call check(error, index(text, "First message"//LF//"Second message"//LF) > 0 .and. &
                   index(text, "Not logged") == 0, text)
        if (allocated(error)) return
        call check(error, window_count() == 2, "*Messages* is displayed")
        if (allocated(error)) return
        ! Errors are logged too.
        call type_keys("C-x z")
        call command_loop(st)
        call message("After")
        text = buf_text("*Messages*")
        call check(error, index(text, LF//"C-x z is undefined"//LF//"After"//LF) > 0, text)
        call fresh_scratch()
    end subroutine test_view_messages

    !> A repeated message becomes "MESSAGE [N times]" (xdisp.c's
    !> message_log_check_duplicate); progress messages ending in "..."
    !> replace each other.
    subroutine test_messages_duplicates(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        call fresh_scratch()
        call message("Unique start")
        call message("Again")
        call message("Again")
        call message("Again")
        call message("Working...")
        call message("Working...done")
        text = buf_text("*Messages*")
        call check(error, index(text, "Unique start"//LF//"Again [3 times]"//LF//"Working...done"//LF) > 0, text)
    end subroutine test_messages_duplicates

    !> C-h C-h shows the help-for-help screen, and a key runs its command.
    subroutine test_help_for_help(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text
        call fresh_scratch()
        call type_keys("C-h C-h SPC f")
        call unread_command_events([sym("x")])
        call discard_input()
        call type_keys("C-h C-h k C-f")
        call command_loop(st)
        text = buf_text(" *Metahelp*")
        call check(error, index(text, "(Type <PageDown> or <PageUp> to scroll, C-s to search, or q to exit.)"//LF//LF// &
                                "Commands, Keys and Functions"//LF//LF//"   m "//TAB// &
                                "Show help for current major and minor modes and their commands"//LF) == 1, text)
        if (allocated(error)) return
        call check(error, index(text, "   l "//TAB//"Show last 300 input keystrokes (lossage)") > 0, text)
        if (allocated(error)) return
        ! Commands not ported yet are listed under their upstream keys.
        call check(error, index(text, LF//LF//"Manuals"//LF//LF//"   r "//TAB//"Show Emacs manual"//LF) > 0 .and. &
                   index(text, LF//"   C-n "//TAB//"News of recent changes"//LF) > 0 .and. &
                   index(text, "   4 s "//TAB//"Show the source for what"//RQ//"s being described in *Help*") > 0 .and. &
                   index(text, "   h "//TAB//"Display the HELLO file illustrating various scripts"//LF) > 0, text)
        if (allocated(error)) return
        ! k ran describe-key, in the previous window configuration.
        call check(error, index(help_text(), "C-f runs the command forward_char") == 1, help_text())
        call fresh_scratch()
    end subroutine test_help_for_help

    subroutine test_help_for_help_unbound(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h ? z")
        call command_loop(st)
        call check(error, last_message, "No help command is bound to "//LQ//"z"//RQ)
        if (allocated(error)) return
        call check(error, window_count() == 1, "the window configuration is restored")
    end subroutine test_help_for_help_unbound

    function buf_text(name) result(text)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: text
        type(handle_type) :: old
        type(status_type) :: st
        old = current_buffer()
        text = "(none)"
        if (.not. buffer_live_p(get_buffer(name))) return
        call set_buffer(get_buffer(name), st)
        text = buffer_string()
        call set_buffer(old, st)
    end function buf_text
end module test_help_fm
