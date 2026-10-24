! shell_fm.f90 -- specialized comint.el for running the shell.
! Ported from GNU Emacs 31.1 lisp/shell.el.
!
! Copyright (C) 1988, 1993-1997, 2000-2026 Free Software Foundation, Inc.
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

!> M-x shell: an inferior shell in *shell*, in Shell mode (Comint mode
!> with the shell's prompt, its history file, its font-lock keywords and
!> directory tracking of cd, pushd and popd), and Shell Command mode for
!> the output of asynchronous shell commands.
!>
!> Not ported yet (Deferred): completion (`shell_completion_vars' beyond
!> the delimiters, `shell_dynamic_complete_*', TAB and M-? in
!> `shell_mode_map', `shell_command_completion'), `shell_resync_dirs'
!> (M-RET, `dirs'), `shell_copy_environment_variable' and
!> `shell_snarf_envar', the input history expansion of
!> `shell_input_autoexpand', ANSI colors (`shell_apply_ansi_color',
!> `shell_reapply_ansi_color'), `comint_fontify_input_mode' and
!> `shell_fontify_input_enable' (the :after-hook), `shell_highlight_undef_mode',
!> `shell_indirect_setup_hook', the indentation functions of the input
!> (`comint_indent_input_line_default'), `scroll_conservatively',
!> bookmarks, remote directories (`shell' asks for nothing more with a
!> prefix argument than the buffer), the value t of
!> `shell_history_file_name' (no history file), the `explicit_SHELL_args'
!> variables of shells other than bash and csh (they get "-i"),
!> `file_truename' (history files are compared by expanded name), the
!> repeat map and menus.
module shell_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nilp, consp, stringp, xstring
    use lread_fm, only: sym
    use data_fm, only: string_setting, logical_setting, forward_setting, add_local_variables_killer
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, buffer_live_p, kill_buffer, &
                         generate_new_buffer_name
    use editfns_fm, only: point, point_min, point_max, goto_char, buffer_substring, line_end_position, &
                          line_beginning_position, narrow_to_region, field_beginning, field_end, field_string, eobp
    use textprop_fm, only: get_text_property
    use insdel_fm, only: insert
    use search_fm, only: string_match, re_search_forward, re_search_backward, match_beginning, match_end, &
                         looking_at, SEARCH_NOERROR, SEARCH_MOVE
    use syntax_fm, only: skip_chars_forward, skip_syntax_backward, skip_syntax_forward
    use env_fm, only: getenv
    use fileio_fm, only: file_exists_p, expand_file_name, file_name_nondirectory, file_name_absolute_p, &
                         directory_file_name, insert_file_contents
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes, minor_modes
    use keymap_fm, only: keymap_set
    use paragraphs_fm, only: paragraph_start, paragraph_separate
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight
    use window_fm, only: pop_to_buffer_same_window
    use callproc_fm, only: shell_file_name, init_callproc
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use minibuffer_fm, only: read_buffer
    use derived_fm, only: derived_mode_p
    use xdisp_fm, only: message
    use dispnew_fm, only: ding
    use commands_fm, only: commands
    use process_fm, only: process_arg_type, get_buffer_process, process_filter, process_sentinel, set_process_filter, &
                          set_process_sentinel, process_send_string, accept_process_output, process_live_p, &
                          process_buffer, process_command, process_filter_proc, process_sentinel_proc, process_name
    use comint_fm, only: comint_mode, comint_check_proc, make_comint_in_buffer, comint_prompt_regexp, &
                         comint_delimiter_argument_list, comint_input_ring_size, comint_input_ring_file_name, &
                         comint_input_ring_file_prefix, comint_preoutput_filter_functions, comint_input_filter_functions, &
                         comint_use_prompt_regexp, comint_read_input_ring, comint_write_input_ring, comint_bol, &
                         comint_arguments, comint_substitute_in_file_name, comint_input_ring_length, &
                         set_comint_get_old_input, comint_next_prompt, comint_previous_prompt, syms_of_comint
    use compile_fm, only: cd
    implicit none(type, external)
    private

    public :: shell, shell_mode, shell_command_mode, syms_of_shell, shell_get_old_input, shell_directory_tracker, &
              shell_unquote_argument, shell_process_cd, shell_process_pushd, shell_process_popd, shell_cd, &
              shell_dirstack_message, shell_forward_command, shell_backward_command, shell_narrow_to_prompt, &
              shell_eval_command, shell_filter_ctrl_a_ctrl_b, shell_filter_ring_bell, shell_write_history_on_exit, &
              split_string_shell_command, shell_dirtrack_mode, shell_dirstack

    !> Regexp to match shells that don't save their command history, and
    !> don't handle the backslash as a quote character.  For shells that
    !> match this regexp, Emacs will write out the command history when the
    !> shell finishes, and won't remove backslashes when it unquotes shell
    !> arguments.  Port of shell.el shell-dumb-shell-regexp.
    type(string_setting), public :: shell_dumb_shell_regexp = string_setting(name="shell_dumb_shell_regexp")

    !> Regexp to match prompts in the inferior shell.
    !> Defaults to "^[^#$%>\n]*[#$%>] *", which works pretty well.
    !> This variable is used to initialize `comint_prompt_regexp' in the
    !> shell buffer.
    !>
    !> If `comint_use_prompt_regexp' is false, then this variable is only
    !> used to determine paragraph boundaries.  See Info node `Shell
    !> Prompts' for how Shell mode treats paragraphs.
    !>
    !> The pattern should probably not match more than one line.  If it
    !> does, Shell mode may become confused trying to distinguish prompt
    !> from input on lines which don't start with a prompt.  Port of
    !> shell.el shell-prompt-pattern.
    type(string_setting), public :: shell_prompt_pattern = string_setting(name="shell_prompt_pattern")

    !> The characters to recognize as separate arguments in input.
    !> This variable is used to initialize `comint_delimiter_argument_list'
    !> in the shell buffer.  The value may depend on the operating system
    !> or shell.  Port of shell.el shell-delimiter-argument-list (the
    !> characters as a string).
    type(string_setting), public :: shell_delimiter_argument_list = &
                                    string_setting(name="shell_delimiter_argument_list")

    !> Regexp to match a single command within a pipeline.
    !> This is used for directory tracking and does not do a perfect job.
    !> Port of shell.el shell-command-regexp.
    type(string_setting), public :: shell_command_regexp = string_setting(name="shell_command_regexp")

    !> Regexp to match a single command within a pipeline.
    !> This is used for directory tracking and does not do a perfect job.
    !> Port of shell.el shell-command-separator-regexp.
    type(string_setting), public :: shell_command_separator_regexp = &
                                    string_setting(name="shell_command_separator_regexp")

    !> Regexp to match subshell commands equivalent to popd.  Port of
    !> shell.el shell-popd-regexp.
    type(string_setting), public :: shell_popd_regexp = string_setting(name="shell_popd_regexp")

    !> Regexp to match subshell commands equivalent to pushd.  Port of
    !> shell.el shell-pushd-regexp.
    type(string_setting), public :: shell_pushd_regexp = string_setting(name="shell_pushd_regexp")

    !> If true, make pushd with no arg behave as "pushd ~" (like cd).
    !> This mirrors the optional behavior of tcsh.  Port of shell.el
    !> shell-pushd-tohome.
    type(logical_setting), public :: shell_pushd_tohome = logical_setting(name="shell_pushd_tohome", value=.false.)

    !> If true, make "pushd +n" pop the nth dir to the stack top.
    !> This mirrors the optional behavior of tcsh.  Port of shell.el
    !> shell-pushd-dextract.
    type(logical_setting), public :: shell_pushd_dextract = logical_setting(name="shell_pushd_dextract", value=.false.)

    !> If true, make pushd only add unique directories to the stack.
    !> This mirrors the optional behavior of tcsh.  Port of shell.el
    !> shell-pushd-dunique.
    type(logical_setting), public :: shell_pushd_dunique = logical_setting(name="shell_pushd_dunique", value=.false.)

    !> Regexp to match subshell commands equivalent to cd.  Port of
    !> shell.el shell-cd-regexp.
    type(string_setting), public :: shell_cd_regexp = string_setting(name="shell_cd_regexp")

    !> If true, show the directory stack following directory change.
    !> This is effective only if directory tracking is enabled.  Port of
    !> shell.el shell-dirtrack-verbose.
    type(logical_setting), public :: shell_dirtrack_verbose = &
                                     logical_setting(name="shell_dirtrack_verbose", value=.true.)

    !> If allocated, the file name to use for explicitly requested inferior
    !> shells.  When unallocated, such interactive shell sessions fall back
    !> to using the shell specified in either the environment variable
    !> "ESHELL" or `shell_file_name'.  Port of shell.el
    !> explicit-shell-file-name.
    type(string_setting), target, public :: explicit_shell_file_name = string_setting(name="explicit_shell_file_name")

    !> Args passed to inferior shell by \[shell], if the shell is csh.
    !> Value is an array of strings, which may be empty.  Port of shell.el
    !> explicit-csh-args.
    type(process_arg_type), allocatable, public :: explicit_csh_args(:)

    !> Args passed to inferior shell by \[shell], if the shell is bash.
    !> Value is an array of strings, which may be empty.  Port of shell.el
    !> explicit-bash-args.
    type(process_arg_type), allocatable, public :: explicit_bash_args(:)

    !> If true, `shell_mode' handles implicit "cd" commands.
    !> Implicit "cd" is changing the directory if the command is a
    !> directory.  You can make this variable buffer-local to change it,
    !> per shell-mode instance.  Useful for shells like zsh that has this
    !> feature.  Port of shell.el shell-has-auto-cd.
    type(logical_setting), target, public :: shell_has_auto_cd = logical_setting(name="shell_has_auto_cd", value=.false.)

    !> Whether `shell_get_old_input' includes "\" lines.  Port of shell.el
    !> shell-get-old-input-include-continuation-lines.
    type(logical_setting), public :: shell_get_old_input_include_continuation_lines = &
                                     logical_setting(name="shell_get_old_input_include_continuation_lines", &
                                                     value=.false.)

    !> Kill a shell buffer after the shell process terminates.  Port of
    !> shell.el shell-kill-buffer-on-exit.
    type(logical_setting), public :: shell_kill_buffer_on_exit = &
                                     logical_setting(name="shell_kill_buffer_on_exit", value=.false.)

    !> The history file name used in `shell_mode'.
    !> When it is a string, this file name will be used.
    !> When it is unallocated, the environment variable HISTFILE is used.
    !>
    !> If no history file name can be determined, a default according to
    !> the shell type is used.  Port of shell.el shell-history-file-name
    !> (the value t is Deferred).
    type(string_setting), target, public :: shell_history_file_name = string_setting(name="shell_history_file_name")

    !> Command used by `shell_resync_dirs' to query the shell.  Port of
    !> shell.el shell-dirstack-query.
    character(len=:), allocatable, public :: shell_dirstack_query

    !> Hook for customizing Shell mode.  Port of shell.el shell-mode-hook.
    type(hook_type), target, public :: shell_mode_hook = hook_type(name="shell_mode_hook")

    !> Hook run after entering Shell Command mode.  Port of what
    !> define-derived-mode defines for shell.el:shell-command-mode.
    type(hook_type), target, public :: shell_command_mode_hook = hook_type(name="shell_command_mode_hook")

    !> Hook run after entering or leaving `shell_dirtrack_mode'.  Port of
    !> what define-minor-mode defines for shell.el:shell-dirtrack-mode.
    type(hook_type), target, public :: shell_dirtrack_mode_hook = hook_type(name="shell_dirtrack_mode_hook")

    !> Keymap for `shell_mode'.  Port of shell.el shell-mode-map (the keys
    !> of ported commands).
    type(handle_type), public :: shell_mode_map

    !> Keymap for `shell_command_mode'.
    type(handle_type), public :: shell_command_mode_map

    !> The directory a buffer's list shows for it (its shell's directory
    !> for a shell buffer).  Port of menu-bar.el list-buffers-directory.
    type(string_setting), target, public :: list_buffers_directory = string_setting(name="list_buffers_directory")

    !> A shell buffer's own state: shell.el's buffer-local shell-dirstack
    !> and shell-last-dir, shell--start-prog (permanent), and the sentinel
    !> wrapped for `shell_kill_buffer_on_exit'.
    type :: shell_buffer_type
        integer(int64) :: serial = 0
        type(handle_type) :: buffer
        character(len=:), allocatable :: start_prog
        type(process_arg_type), allocatable :: dirstack(:)
        character(len=:), allocatable :: last_dir
        procedure(process_sentinel_proc), pointer, nopass :: kill_sentinel => null()
        logical :: kill_on_exit = .false.
    end type shell_buffer_type

    type(shell_buffer_type), allocatable :: shell_buffers(:)

    !> What `shell_eval_command' collects.
    character(len=:), allocatable :: eval_result

contains

    ! ------------------------------------------------------------------
    ! The buffers' own state
    ! ------------------------------------------------------------------

    integer function entry_of(bi) result(k)
        integer(int64), intent(in) :: bi
        if (allocated(shell_buffers) .and. bi > 0) then
            do k = 1, size(shell_buffers)
                if (shell_buffers(k)%serial == buffers(bi)%serial) return
            end do
        end if
        k = 0
    end function entry_of

    !> The entry of the current buffer, made if needed (entries of dead
    !> buffers dropped then).
    integer function current_entry() result(k)
        type(shell_buffer_type), allocatable :: kept(:)
        integer :: i, n
        k = entry_of(cur)
        if (k /= 0) return
        if (.not. allocated(shell_buffers)) allocate (shell_buffers(0))
        allocate (kept(size(shell_buffers) + 1))
        n = 0
        do i = 1, size(shell_buffers)
            if (.not. buffer_live_p(shell_buffers(i)%buffer)) cycle
            n = n + 1
            kept(n) = shell_buffers(i)
        end do
        n = n + 1
        kept(n)%serial = buffers(cur)%serial
        kept(n)%buffer = current_buffer()
        allocate (kept(n)%dirstack(0))
        shell_buffers = kept(1:n)
        k = n
    end function current_entry

    !> Forget the buffer-local shell-dirstack and shell-last-dir.
    subroutine kill_shell_locals()
        integer :: k
        k = entry_of(cur)
        if (k == 0) return
        if (allocated(shell_buffers(k)%dirstack)) deallocate (shell_buffers(k)%dirstack)
        allocate (shell_buffers(k)%dirstack(0))
        if (allocated(shell_buffers(k)%last_dir)) deallocate (shell_buffers(k)%last_dir)
    end subroutine kill_shell_locals

    !> The current buffer's directory stack (shell-dirstack): the
    !> directories saved by pushd, the most recent first.
    !>
    !> List of directories saved by pushd in this buffer's shell.
    !> Thus, this does not include the shell's current directory.  Port of
    !> shell.el shell-dirstack.
    subroutine shell_dirstack(stack)
        type(process_arg_type), allocatable, intent(out) :: stack(:)
        integer :: k
        k = entry_of(cur)
        if (k == 0) then
            allocate (stack(0))
        else
            stack = shell_buffers(k)%dirstack
        end if
    end subroutine shell_dirstack

    !> The current buffer's default directory.
    function default_directory() result(dir)
        character(len=:), allocatable :: dir
        dir = ""
        if (cur > 0) then
            if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
        end if
    end function default_directory

    ! ------------------------------------------------------------------
    ! Old input and arguments
    ! ------------------------------------------------------------------

    !> Default for `comint_get_old_input' in `shell_mode'.
    !> If `comint_use_prompt_regexp' is false, then either return the
    !> current input field (if point is on an input field), or the current
    !> line (if point is on an output field).  If `comint_use_prompt_regexp'
    !> is true, then return the current line, with any initial string
    !> matching the regexp `comint_prompt_regexp' removed.  In either case,
    !> if `shell_get_old_input_include_continuation_lines' is true and the
    !> current line ends with a backslash, the next line is also included
    !> and examined for a backslash, ending with a final line without a
    !> backslash.  Port of shell.el:shell-get-old-input.
    recursive subroutine shell_get_old_input(input, status)
        character(len=:), allocatable, intent(out) :: input
        type(status_type), intent(out) :: status
        type(value_type) :: field_prop
        integer(int64) :: bof, start, n
        logical :: output
        status = status_type()
        output = .false.
        if (.not. comint_use_prompt_regexp%value) then
            ! Make sure we're in an input rather than output field.
            bof = field_beginning(point(), .false.)
            call get_text_property(bof, sym("field"), field_prop, status)
            if (nilp(field_prop)) then
                input = field_string(bof)
                return
            end if
            output = symbol_is(field_prop, "output")
        end if
        call comint_bol(.false.)
        start = point()
        if (comint_use_prompt_regexp%value .or. output) then
            call goto_char(line_end_position())
            if (shell_get_old_input_include_continuation_lines%value) then
                ! Include continuation lines as long as the current line
                ! ends with a backslash.
                do while (.not. eobp())
                    if (char_before_point() /= iachar("\")) exit
                    n = 2
                    call goto_char(line_end_position(n))
                end do
            end if
        else
            call goto_char(field_end(point(), .false.))
        end if
        call buffer_substring(start, point(), input, status)
    end subroutine shell_get_old_input

    integer function char_before_point()
        use editfns_fm, only: char_before
        integer(int32) :: c
        logical :: found
        call char_before(c, found)
        char_before_point = c
    end function char_before_point

    logical function symbol_is(v, name)
        use lisp_fm, only: symbolp, symbols, xsymbol
        type(value_type), intent(in) :: v
        character(len=*), intent(in) :: name
        symbol_is = .false.
        if (.not. symbolp(v)) return
        if (nilp(v)) return
        symbol_is = symbols(xsymbol(v))%name == name
    end function symbol_is

    !> Parse whitespace separated arguments from the start of the current
    !> command to point: ARGS, and the positions where they begin.  Port of
    !> shell.el:shell--parse-pcomplete-arguments (a non-empty
    !> `comint_file_name_quote_list', as on GNU/Linux).
    subroutine shell_parse_pcomplete_arguments(args, begins)
        use subr_fm, only: match_string
        type(process_arg_type), allocatable, intent(out) :: args(:)
        integer(int64), allocatable, intent(out) :: begins(:)
        character(len=*), parameter :: ARG_RE = "\(?:[^ "//achar(9)//achar(10)//"\""';]+\|'\([^']*\)'?\|"// &
                                       """\(\(?:[^""\]\|\\.\)*\)""?\|\\\(\(?:.\|"//achar(10)//"\)?\)\)"
        type(status_type) :: st
        character(len=:), allocatable :: arg, part, text
        integer(int64) :: begin, end, opoint, moved
        opoint = point()
        call shell_backward_command(1_int64, st)
        begin = point()
        call goto_char(opoint)
        end = point()
        allocate (args(0), begins(0))
        call goto_char(begin)
        do while (point() < end)
            moved = skip_chars_forward(" "//achar(9)//achar(10)//";", st)
            begins = [begins, point()]
            arg = ""
            do while (looking_at(ARG_RE, st))
                call goto_char(match_end(0, st))
                if (match_beginning(3, st) > 0) then
                    ! Backslash escape.
                    if (match_beginning(3, st) == match_end(3, st)) then
                        part = "\"
                    else
                        call buffer_substring(match_beginning(3, st), match_end(3, st), part, st)
                    end if
                else if (match_beginning(2, st) > 0) then
                    ! Double quote.
                    call buffer_substring(match_beginning(2, st), match_end(2, st), text, st)
                    part = unbackslash(text)
                else if (match_beginning(1, st) > 0) then
                    ! Single quote.
                    call buffer_substring(match_beginning(1, st), match_end(1, st), part, st)
                else
                    call buffer_substring(match_beginning(0, st), match_end(0, st), part, st)
                end if
                arg = arg//part
            end do
            call append_arg(args, arg)
        end do
        call goto_char(opoint)
    end subroutine shell_parse_pcomplete_arguments

    !> TEXT with each backslash-escaped character replaced by the character
    !> (replace-regexp-in-string "\\\\\\(.\\)" "\\1").
    function unbackslash(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: k
        out = ""
        k = 1
        do while (k <= len(text))
            if (text(k:k) == "\" .and. k < len(text)) then
                if (text(k + 1:k + 1) /= achar(10)) k = k + 1
            end if
            out = out//text(k:k)
            k = k + 1
        end do
    end function unbackslash

    !> Split STRING (a shell command) into an array of strings.
    !> General shell syntax, like single and double quoting, as well as
    !> backslash quoting, is respected.  Port of
    !> shell.el:split-string-shell-command.
    subroutine split_string_shell_command(string, args)
        type(process_arg_type), allocatable, intent(out) :: args(:)
        character(len=*), intent(in) :: string
        type(handle_type) :: old, tmp
        type(status_type) :: st
        integer(int64), allocatable :: begins(:)
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, st)
        call set_buffer(tmp, st)
        call insert(string, st)
        call shell_parse_pcomplete_arguments(args, begins)
        call set_buffer(old, st)
        call kill_buffer(tmp, st)
    end subroutine split_string_shell_command

    ! ------------------------------------------------------------------
    ! The modes
    ! ------------------------------------------------------------------

    !> Major mode for interacting with an inferior shell.
    !> \<shell_mode_map>
    !> \[comint_send_input] after the end of the process' output sends the
    !>     text from the end of process to the end of the current line.
    !> \[comint_send_input] before end of process output copies the current
    !>     line minus the prompt to the end of the buffer and sends it
    !>     (\[comint_copy_old_input] just copies the current line).
    !>
    !> If you want to make multiple shell buffers, rename the "*shell*"
    !> buffer using \[rename_buffer] and start a new shell.
    !>
    !> If you want to make shell buffers limited in length, add the
    !> function `comint_truncate_buffer' to `comint_output_filter_functions'.
    !>
    !> `cd', `pushd' and `popd' commands given to the shell are watched by
    !> Emacs to keep this buffer's default directory the same as the
    !> shell's working directory.
    !> \[shell_dirtrack_mode] turns directory tracking on and off.
    !>
    !> Customization: Entry to this mode runs the hooks on
    !> `comint_mode_hook' and `shell_mode_hook' (in that order).  Before
    !> each input, the hooks on `comint_input_filter_functions' are run.
    !> After each shell output, the hooks on
    !> `comint_output_filter_functions' are run.
    !>
    !> Variables `shell_cd_regexp', `shell_pushd_regexp' and
    !> `shell_popd_regexp' are used to match their respective commands,
    !> while `shell_pushd_tohome', `shell_pushd_dextract' and
    !> `shell_pushd_dunique' control the behavior of the relevant command.
    !>
    !> Variables `comint_input_ring_file_name' and `comint_input_ring_size'
    !> control the initialization of the input ring history.
    !>
    !> Variables `comint_output_filter_functions', a hook, and
    !> `comint_move_point_for_output' control whether input and output
    !> cause the window to scroll to the end of the buffer.
    !>
    !> By default, shell mode does nothing special when it receives a
    !> "bell" character (C-g or ^G).  If you add the function
    !> `shell_filter_ring_bell' to `comint_output_filter_functions' locally
    !> from `shell_mode_hook', Emacs will call the `ding' function whenever
    !> it receives the bell character in output from a command.  Port of
    !> shell.el:shell-mode.
    recursive subroutine shell_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        if (.not. associated(major_modes%shell_mode%run)) call syms_of_shell()
        call major_modes%shell_mode%begin(saved, status)
        if (.not. status%ok()) return
        call shell_mode_body(status)
        if (.not. status%ok()) return
        call major_modes%shell_mode%finish(saved, status)
    end subroutine shell_mode

    !> The body of shell-mode.
    recursive subroutine shell_mode_body(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type) :: keywords(3)
        type(handle_type) :: proc
        character(len=:), allocatable :: shell, hfile, hsize, expanded
        type(status_type) :: st
        logical :: have_hfile, found
        integer :: k, n, ios
        comint_prompt_regexp = shell_prompt_pattern%value
        call shell_completion_vars()
        call paragraph_separate%set_local("\'")
        call paragraph_start%set_local(comint_prompt_regexp%value)
        call shell_font_lock_keywords(keywords)
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        k = current_entry()
        call kill_shell_locals()
        call set_comint_get_old_input(shell_get_old_input)
        call minor_modes%shell_dirtrack_mode%enable(status)
        if (.not. status%ok()) return
        ! This is not really correct, since the shell buffer does not
        ! really edit this directory.  But it is useful in the buffer list
        ! and menus.
        call expand_file_name(default_directory(), expanded, st)
        call list_buffers_directory%set_local(expanded)
        ! shell-dependent assignments.
        if (comint_input_ring_length() == 0) then
            k = current_entry()
            shell = ""
            if (allocated(shell_buffers(k)%start_prog)) shell = shell_buffers(k)%start_prog
            have_hfile = allocated(shell_history_file_name%value)
            if (have_hfile) then
                hfile = shell_history_file_name%value
            else
                call getenv("HISTFILE", hfile, have_hfile)
            end if
            if (have_hfile) have_hfile = len(hfile) > 0
            if (.not. have_hfile) then
                select case (shell)
                case ("bash")
                    hfile = "~/.bash_history"
                case ("ksh")
                    hfile = "~/.sh_history"
                case ("zsh")
                    hfile = "~/.zsh_history"
                case default
                    hfile = "~/.history"
                end select
            end if
            call getenv("HISTSIZE", hsize, found)
            if (found) then
                read (hsize, *, iostat=ios) n
                if (ios == 0 .and. n > 0) call comint_input_ring_size%set_local(n)
            end if
            comint_input_ring_file_name = hfile
            if (len(hfile) == 0) then
                call comint_input_ring_file_name%clear()
            else
                call expand_file_name(hfile, expanded, st)
                if (expanded == "/dev/null") call comint_input_ring_file_name%clear()
            end if
            ! Arrange to write out the input ring on exit, if the shell
            ! doesn't do this itself.
            if (allocated(comint_input_ring_file_name%value)) then
                if (string_match(shell_dumb_shell_regexp%value, shell, st) /= 0) then
                    proc = get_buffer_process(current_buffer())
                    call set_process_sentinel(proc, st, shell_write_history_on_exit)
                end if
            end if
            select case (shell)
            case ("sh")
                shell_dirstack_query = "pwd"
            case ("ksh")
                shell_dirstack_query = "echo $PWD ~-"
            case ("bash")
                ! Bypass any aliases.
                shell_dirstack_query = "command dirs"
            case ("zsh")
                shell_dirstack_query = "dirs -l"
            case default
                shell_dirstack_query = "dirs"
            end select
            ! Bypass a bug in certain versions of bash.
            if (shell == "bash") call comint_preoutput_filter_functions%add(shell_filter_ctrl_a_ctrl_b, local=.true.)
            ! Skip extended history for zsh.
            if (shell == "zsh") call comint_input_ring_file_prefix%set_local(": [[:digit:]]+:[[:digit:]]+;")
            call comint_read_input_ring(.true.)
        end if
    end subroutine shell_mode_body

    !> Setup completion vars for `shell_mode' and `read_shell_command'.
    !> Port of shell.el:shell-completion-vars (the delimiters; completion
    !> is Deferred).
    subroutine shell_completion_vars()
        call comint_delimiter_argument_list%set_local(shell_delimiter_argument_list%value)
    end subroutine shell_completion_vars

    !> shell.el shell-font-lock-keywords: additional expressions to
    !> highlight in Shell mode.
    subroutine shell_font_lock_keywords(keywords)
        use font_core_fm, only: font_lock_highlight_type
        type(font_lock_keyword_type), intent(out) :: keywords(3)
        type(font_lock_highlight_type) :: h(1)
        h(1) = font_lock_highlight(1, "font_lock_comment_face")
        keywords(1) = font_lock_keyword("[ "//achar(9)//"]\([+-][^ "//achar(9)//achar(10)//"]+\)", h)
        keywords(2) = font_lock_keyword("^[^ "//achar(9)//achar(10)//"]+:.*", face="font_lock_string_face")
        keywords(3) = font_lock_keyword("^\[[1-9][0-9]*\]", face="font_lock_string_face")
    end subroutine shell_font_lock_keywords

    !> Major mode for the output of asynchronous `shell_command'.  Port of
    !> shell.el:shell-command-mode.
    recursive subroutine shell_command_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type) :: keywords(3)
        character(len=:), allocatable :: expanded
        type(status_type) :: st
        logical :: saved
        if (.not. associated(major_modes%shell_command_mode%run)) call syms_of_shell()
        call major_modes%shell_command_mode%begin(saved, status)
        if (.not. status%ok()) return
        call shell_font_lock_keywords(keywords)
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        call expand_file_name(default_directory(), expanded, st)
        call list_buffers_directory%set_local(expanded)
        call major_modes%shell_command_mode%finish(saved, status)
    end subroutine shell_command_mode

    ! ------------------------------------------------------------------
    ! M-x shell
    ! ------------------------------------------------------------------

    !> Run an inferior shell, with I/O through BUFFER (which defaults to
    !> `*shell*').  Interactively, a prefix arg means to prompt for BUFFER.
    !>
    !> If BUFFER exists but shell process is not running, make new shell.
    !> If BUFFER exists and shell process is running, just switch to
    !> BUFFER.  Program used comes from variable `explicit_shell_file_name',
    !>  or (if that is unallocated) from the ESHELL environment variable,
    !>  or (if that is unset) from `shell_file_name'.
    !> Non-interactively, it can also be specified via the FILE_NAME arg.
    !>
    !> If a file `~/.emacs_SHELLNAME' exists, or
    !> `~/.emacs.d/init_SHELLNAME.sh', it is given as initial input (but
    !> this may be lost, due to a timing error, if the shell discards input
    !> when it starts up).  The buffer is put in Shell mode, giving commands
    !> for sending input and controlling the subjobs of the shell.  See
    !> `shell_mode'.  See also the variable `shell_prompt_pattern'.
    !>
    !> The shell file name (sans directories) is used to choose the
    !> arguments: `explicit_bash_args' for bash, `explicit_csh_args' for
    !> csh.  Otherwise, one argument `-i' is passed to the shell.
    !>
    !> Make the shell buffer the current buffer, and return it in RESULT.
    !> Port of shell.el:shell.
    recursive subroutine shell(result, status, buffer_name, file_name)
        type(handle_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: buffer_name, file_name
        type(handle_type) :: buf, proc
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st
        character(len=:), allocatable :: prog, name, startfile, contents, out
        integer(int64) :: start_point
        logical :: found, got
        integer :: k
        result = null_handle()
        if (present(buffer_name) .or. .not. derived_mode_p("shell_mode") .or. comint_check_proc(current_buffer())) then
            if (present(buffer_name)) then
                call get_buffer_create(buffer_name, buf, status)
            else
                call get_buffer_create("*shell*", buf, status)
            end if
            if (.not. status%ok()) return
        else
            ! If the current buffer is a dead shell buffer, use it.
            buf = current_buffer()
        end if
        ! The buffer's window must be correctly set when we call comint (so
        ! that comint sets the COLUMNS env var properly).
        call pop_to_buffer_same_window(buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        if (present(file_name)) call explicit_shell_file_name%set_local(file_name)
        ! Rain or shine, BUFFER must be current by now.
        if (.not. comint_check_proc(buf)) then
            if (allocated(explicit_shell_file_name%value)) then
                prog = explicit_shell_file_name%value
            else
                call getenv("ESHELL", prog, found)
                if (.not. found) then
                    if (.not. allocated(shell_file_name%value)) call init_callproc()
                    prog = shell_file_name%value
                end if
            end if
            name = file_name_nondirectory(prog)
            call expand_file_name("~/.emacs_"//name, startfile, st)
            start_point = point()
            if (.not. file_exists_p(startfile)) call expand_file_name("~/.emacs.d/init_"//name//".sh", startfile, st)
            k = current_entry()
            shell_buffers(k)%start_prog = file_name_nondirectory(prog)
            select case (name)
            case ("bash")
                args = explicit_bash_args
            case ("csh")
                args = explicit_csh_args
            case default
                allocate (args(1))
                args(1)%s = "-i"
            end select
            call make_comint_in_buffer("shell", prog, buf, status, buffer_name=buffers(buf%index)%name, switches=args)
            if (.not. status%ok()) return
            call set_buffer(buf, st)
            call shell_mode(status)
            if (.not. status%ok()) return
            if (file_exists_p(startfile)) then
                ! Wait until the prompt has appeared.
                proc = get_buffer_process(buf)
                do while (start_point == point() .and. process_live_p(proc))
                    call accept_process_output(got, st, proc, 0.1)
                end do
                call file_contents(startfile, contents, st)
                call shell_eval_command(contents, out, status)
                if (.not. status%ok()) return
            end if
        end if
        if (shell_kill_buffer_on_exit%value) then
            proc = get_buffer_process(buf)
            k = current_entry()
            call process_sentinel(proc, shell_buffers(k)%kill_sentinel)
            shell_buffers(k)%kill_on_exit = .true.
            call set_process_sentinel(proc, st, kill_buffer_sentinel)
        end if
        result = buf
    end subroutine shell

    !> The contents of FILE.
    subroutine file_contents(file, text, status)
        use editfns_fm, only: buffer_string
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: old, tmp
        type(status_type) :: st
        integer(int64) :: inserted
        text = ""
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, status)
        if (.not. status%ok()) return
        call set_buffer(tmp, st)
        call insert_file_contents(file, .false., inserted, status)
        if (status%ok()) text = buffer_string()
        call set_buffer(old, st)
        call kill_buffer(tmp, st)
    end subroutine file_contents

    !> The sentinel `shell' gives the process when
    !> `shell_kill_buffer_on_exit': the previous sentinel, then kill the
    !> buffer once the process is no longer live.
    recursive subroutine kill_buffer_sentinel(proc, event, status)
        use process_fm, only: internal_default_process_sentinel
        type(handle_type), intent(in) :: proc
        character(len=*), intent(in) :: event
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, old
        type(status_type) :: st
        integer :: k
        status = status_type()
        buf = process_buffer(proc)
        if (.not. buffer_live_p(buf)) then
            call internal_default_process_sentinel(proc, event, status)
            return
        end if
        k = entry_of(buf%index)
        if (k /= 0) then
            if (associated(shell_buffers(k)%kill_sentinel)) then
                call shell_buffers(k)%kill_sentinel(proc, event, status)
            else
                call internal_default_process_sentinel(proc, event, status)
            end if
        end if
        if (.not. process_live_p(proc)) then
            old = current_buffer()
            call kill_buffer(buf, st)
            if (buffer_live_p(old)) call set_buffer(old, st)
        end if
    end subroutine kill_buffer_sentinel

    !> The command: with a prefix argument, read the buffer name.  The
    !> interactive form of shell.el:shell.
    !>
    !> Run an inferior shell, with I/O through BUFFER (which defaults to
    !> `*shell*').
    recursive subroutine cmd_shell(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, proc
        character(len=:), allocatable :: name, default
        if (nilp(current_prefix_arg)) then
            call shell(buf, status)
            return
        end if
        ! If the current buffer is an inactive shell buffer, use it as the
        ! default.
        proc = get_buffer_process(current_buffer())
        if (buffers(cur)%major_mode == "shell_mode" .and. proc%index == 0) then
            default = buffers(cur)%name
        else
            default = generate_new_buffer_name("*shell*")
        end if
        call read_buffer("Shell buffer: ", name, status, def=default)
        if (.not. status%ok()) return
        call shell(buf, status, buffer_name=name)
    end subroutine cmd_shell

    !> Eval COMMAND in the current shell process and return the result in
    !> RESULT.  Port of shell.el:shell-eval-command.
    recursive subroutine shell_eval_command(command, result, status)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        procedure(process_filter_proc), pointer :: old_filter
        type(status_type) :: st
        character(len=:), allocatable :: last, prev
        logical :: have_prev, got, done
        integer :: k
        result = ""
        proc = get_buffer_process(current_buffer())
        call process_filter(proc, old_filter)
        eval_result = ""
        call set_process_filter(proc, st, collect_output)
        call process_send_string(command, status, proc)
        have_prev = .false.
        ! Wait until we get a prompt (which will be a line without a
        ! newline).  This is far from fool-proof -- if something outputs
        ! incomplete data and then sleeps, we'll think we've received the
        ! prompt.
        do while (status%ok())
            done = .false.
            if (len(eval_result) > 0) then
                k = index(eval_result, achar(10), back=.true.)
                last = eval_result(k + 1:)
                if (len(last) > 0) then
                    if (.not. have_prev) then
                        done = .true.
                    else
                        done = .not. (last == prev .and. len(last) == len(prev))
                    end if
                    if (done) then
                        prev = last
                        have_prev = .true.
                    end if
                end if
            end if
            if (done .or. .not. process_live_p(proc)) exit
            call accept_process_output(got, st, proc, 0.1)
        end do
        ! Restore old filter.
        if (associated(old_filter)) then
            call set_process_filter(proc, st, old_filter)
        else
            call set_process_filter(proc, st)
        end if
        ! Remove the prompt.
        k = index(eval_result, achar(10), back=.true.)
        if (k > 0) then
            result = eval_result(1:k)
        else
            result = ""
        end if
    end subroutine shell_eval_command

    !> The filter of `shell_eval_command': keep the output.
    recursive subroutine collect_output(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        status = status_type()
        ! (PROCESS is not used.)
        if (process%index < 0) return
        eval_result = eval_result//string
    end subroutine collect_output

    !> Remove `^A' and `^B' characters from comint output.
    !>
    !> Bash uses these characters as internal quoting characters in its
    !> prompt.  Due to a bug in some bash versions (including 2.03, 2.04,
    !> and 2.05b), they may erroneously show up when bash is started with
    !> the `--noediting' option and Select Graphic Rendition (SGR) control
    !> sequences (formerly known as ANSI escape sequences) are used to
    !> color the prompt.
    !>
    !> This function can be put on `comint_preoutput_filter_functions'.
    !> Port of shell.el:shell-filter-ctrl-a-ctrl-b.
    recursive subroutine shell_filter_ctrl_a_ctrl_b(string, result, status)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        integer :: k
        status = status_type()
        result = ""
        do k = 1, len(string)
            if (string(k:k) /= achar(1) .and. string(k:k) /= achar(2)) result = result//string(k:k)
        end do
    end subroutine shell_filter_ctrl_a_ctrl_b

    !> Call `ding' if STRING contains a "^G" character.
    !> This function can be put on `comint_output_filter_functions'.  Port
    !> of shell.el:shell-filter-ring-bell.
    recursive subroutine shell_filter_ring_bell(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        status = status_type()
        if (index(string, achar(7)) > 0) call ding(status=status)
    end subroutine shell_filter_ring_bell

    !> Called when the shell process is stopped.
    !>
    !> Writes the input history to a history file
    !> `comint_input_ring_file_name' using `comint_write_input_ring' and
    !> inserts a short message in the shell buffer.
    !>
    !> This function is a sentinel watching the shell interpreter process.
    !> Sentinels will always get the two parameters PROCESS and EVENT.
    !> Port of shell.el:shell-write-history-on-exit.
    recursive subroutine shell_write_history_on_exit(process, event, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: event
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, old
        type(status_type) :: st
        status = status_type()
        ! Write history.
        call comint_write_input_ring()
        buf = process_buffer(process)
        if (buffer_live_p(buf)) then
            old = current_buffer()
            call set_buffer(buf, st)
            call insert(achar(10)//"Process "//process_name(process)//" "//event//achar(10), status)
            call set_buffer(old, st)
        end if
    end subroutine shell_write_history_on_exit

    ! ------------------------------------------------------------------
    ! Directory tracking
    ! ------------------------------------------------------------------

    !> Toggle directory tracking in this shell buffer (Shell Dirtrack mode).
    !> This assigns a buffer-local true value to `shell_dirtrack_mode'.
    !> Port of shell.el:shell-dirtrack-mode.
    recursive subroutine shell_dirtrack_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%shell_dirtrack_mode%command(status)
    end subroutine shell_dirtrack_mode

    !> The body of Shell Dirtrack mode.
    subroutine shell_dirtrack_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        status = status_type()
        if (enable) then
            call list_buffers_directory%set_local(default_directory())
            call comint_input_filter_functions%add(shell_directory_tracker, local=.true.)
        else
            call list_buffers_directory%make_local()
            call list_buffers_directory%clear()
            call comint_input_filter_functions%remove(shell_directory_tracker, local=.true.)
        end if
    end subroutine shell_dirtrack_body

    !> Tracks cd, pushd and popd commands issued to the shell.
    !> This function is called on each input passed to the shell.
    !> It watches for cd, pushd and popd commands and sets the buffer's
    !> default directory to track these commands.
    !>
    !> You may toggle this tracking on and off with \[shell_dirtrack_mode].
    !>
    !> See variables `shell_cd_regexp', `shell_pushd_regexp', and
    !> `shell_popd_regexp', while `shell_pushd_tohome',
    !> `shell_pushd_dextract', and `shell_pushd_dunique' control the
    !> behavior of the relevant command.
    !>
    !> Environment variables are expanded, see function
    !> `substitute_in_file_name'.  Port of shell.el:shell-directory-tracker.
    recursive subroutine shell_directory_tracker(str, status)
        use search_fm, only: case_fold_search
        character(len=*), intent(in) :: str
        type(status_type), intent(out) :: status
        type(status_type) :: st
        character(len=:), allocatable :: cmd, arg1, part, cmd_subst_fn
        integer(int64) :: start, end, idx
        logical :: saved_fold
        status = status_type()
        if (.not. minor_modes%shell_dirtrack_mode%enabled()) return
        ! We fail gracefully if we think the command will fail in the
        ! shell.  This fails so often that it seems better to just ignore
        ! errors.
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        ! Skip whitespace.
        idx = string_match("^"//shell_command_separator_regexp%value, str, st)
        start = match_end(0, st)
        do
            if (.not. st%ok()) exit
            idx = string_match(shell_command_regexp%value, str, st, start=start)
            if (idx == 0 .or. .not. st%ok()) exit
            end = match_end(0, st)
            part = str(start:end - 1)
            cmd = comint_arguments(part, 0, 0)
            arg1 = comint_arguments(part, 1, 1)
            if (len(arg1) > 0) arg1 = shell_unquote_argument(arg1)
            if (shell_has_auto_cd%value) cmd_subst_fn = comint_substitute_in_file_name(cmd)
            if (string_match("\`\("//shell_popd_regexp%value//"\)\($\|[ "//achar(9)//"]\)", cmd, st) /= 0) then
                call shell_process_popd(comint_substitute_in_file_name(arg1), st)
            else if (string_match("\`\("//shell_pushd_regexp%value//"\)\($\|[ "//achar(9)//"]\)", cmd, st) /= 0) then
                call shell_process_pushd(comint_substitute_in_file_name(arg1), st)
            else if (string_match("\`\("//shell_cd_regexp%value//"\)\($\|[ "//achar(9)//"]\)", cmd, st) /= 0) then
                call shell_process_cd(comint_substitute_in_file_name(arg1), st)
            else if (shell_has_auto_cd%value) then
                block
                    use fileio_fm, only: file_directory_p
                    if (file_directory_p(cmd_subst_fn)) call shell_process_cd(cmd_subst_fn, st)
                end block
            end if
            ! Skip again.
            idx = string_match(shell_command_separator_regexp%value, str, st, start=end)
            start = match_end(0, st)
        end do
        case_fold_search%value = saved_fold
    end subroutine shell_directory_tracker

    !> Remove all kinds of shell quoting from STRING.  Port of
    !> shell.el:shell-unquote-argument.
    function shell_unquote_argument(string) result(s)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: s
        type(process_arg_type), allocatable :: command(:)
        type(status_type) :: st
        character(len=:), allocatable :: quote_chars
        character :: inside
        logical :: in_quotes
        integer :: next
        quote_chars = "\'`"""
        call process_command(get_buffer_process(current_buffer()), command)
        if (size(command) > 0) then
            if (string_match(shell_dumb_shell_regexp%value, file_name_nondirectory(command(1)%s), st) /= 0) then
                quote_chars = "'`"""
            end if
        end if
        s = string
        in_quotes = .false.
        inside = " "
        next = 1
        do while (next <= len(s))
            if (index(quote_chars, s(next:next)) == 0) then
                next = next + 1
                cycle
            end if
            if (s(next:next) == "\") then
                s = s(1:next - 1)//s(next + 1:)
                next = next + 1
            else if (in_quotes .and. s(next:next) == inside) then
                s = s(1:next - 1)//s(next + 1:)
                in_quotes = .false.
            else if (in_quotes) then
                next = next + 1
            else
                inside = s(next:next)
                in_quotes = .true.
                s = s(1:next - 1)//s(next + 1:)
            end if
        end do
    end function shell_unquote_argument

    !> popd [+n].  Port of shell.el:shell-process-popd.
    recursive subroutine shell_process_popd(arg, status)
        character(len=*), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer :: num, k
        status = status_type()
        num = shell_extract_num(arg)
        if (num < 0) num = 0
        k = current_entry()
        if (num == 0 .and. size(shell_buffers(k)%dirstack) > 0) then
            call shell_cd(shell_buffers(k)%dirstack(1)%s, status)
            if (.not. status%ok()) return
            k = current_entry()
            shell_buffers(k)%dirstack = shell_buffers(k)%dirstack(2:)
            call shell_dirstack_message()
        else if (num > 0 .and. num <= size(shell_buffers(k)%dirstack)) then
            shell_buffers(k)%dirstack = [shell_buffers(k)%dirstack(1:num - 1), shell_buffers(k)%dirstack(num + 1:)]
            call shell_dirstack_message()
        else
            call set_error(status, FM_GENERIC_ERROR, "Couldn't popd")
        end if
    end subroutine shell_process_popd

    !> cd [dir].  Port of shell.el:shell-process-cd.
    recursive subroutine shell_process_cd(arg, status)
        character(len=*), intent(in) :: arg
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: new_dir
        integer :: k
        k = current_entry()
        if (len(arg) == 0) then
            new_dir = "~"
        else if (arg == "-") then
            new_dir = ""
            if (allocated(shell_buffers(k)%last_dir)) new_dir = shell_buffers(k)%last_dir
        else
            new_dir = arg
        end if
        shell_buffers(k)%last_dir = default_directory()
        call shell_cd(new_dir, status)
        if (.not. status%ok()) return
        call shell_dirstack_message()
    end subroutine shell_process_cd

    !> pushd [+n | dir].  Port of shell.el:shell-process-pushd.
    recursive subroutine shell_process_pushd(arg, status)
        character(len=*), intent(in) :: arg
        type(status_type), intent(out) :: status
        type(process_arg_type), allocatable :: ds(:), new_ds(:)
        character(len=:), allocatable :: old, dir
        integer :: num, k, dslen
        status = status_type()
        num = shell_extract_num(arg)
        k = current_entry()
        if (len(arg) == 0) then
            ! no arg -- swap pwd and car of stack unless shell-pushd-tohome.
            if (shell_pushd_tohome%value) then
                call shell_process_pushd("~", status)
            else if (size(shell_buffers(k)%dirstack) > 0) then
                old = default_directory()
                call shell_cd(shell_buffers(k)%dirstack(1)%s, status)
                if (.not. status%ok()) return
                k = current_entry()
                shell_buffers(k)%dirstack(1)%s = old
                call shell_dirstack_message()
            else
                call message("Directory stack empty.")
            end if
        else if (num >= 0) then
            ! pushd +n.
            if (num > size(shell_buffers(k)%dirstack)) then
                call message("Directory stack not that deep.")
            else if (num == 0) then
                call set_error(status, FM_GENERIC_ERROR, "Couldn't cd")
            else if (shell_pushd_dextract%value) then
                dir = shell_buffers(k)%dirstack(num)%s
                call shell_process_popd(arg, status)
                if (.not. status%ok()) return
                call shell_process_pushd(default_directory(), status)
                if (.not. status%ok()) return
                call shell_cd(dir, status)
                if (.not. status%ok()) return
                call shell_dirstack_message()
            else
                ds = shell_buffers(k)%dirstack
                call prepend_arg(ds, default_directory())
                dslen = size(ds)
                ! (append (nthcdr num ds) (first num of ds))
                new_ds = [ds(num + 1:dslen), ds(1:num)]
                call shell_cd(new_ds(1)%s, status)
                if (.not. status%ok()) return
                k = current_entry()
                shell_buffers(k)%dirstack = new_ds(2:)
                call shell_dirstack_message()
            end if
        else
            ! pushd <dir>.
            old = default_directory()
            call shell_cd(arg, status)
            if (.not. status%ok()) return
            k = current_entry()
            if (.not. shell_pushd_dunique%value .or. .not. member(old, shell_buffers(k)%dirstack)) then
                call prepend_arg(shell_buffers(k)%dirstack, old)
            end if
            call shell_dirstack_message()
        end if
    end subroutine shell_process_pushd

    logical function member(s, list)
        character(len=*), intent(in) :: s
        type(process_arg_type), intent(in) :: list(:)
        integer :: i
        member = .false.
        do i = 1, size(list)
            if (list(i)%s == s .and. len(list(i)%s) == len(s)) member = .true.
        end do
    end function member

    !> If STR is of the form +n, for n>0, return n.  Otherwise, -1.  Port
    !> of shell.el:shell-extract-num.
    integer function shell_extract_num(str) result(n)
        character(len=*), intent(in) :: str
        type(status_type) :: st
        integer :: ios
        n = -1
        if (string_match("^\+[1-9][0-9]*$", str, st) == 0) return
        read (str(2:), *, iostat=ios) n
        if (ios /= 0) n = -1
    end function shell_extract_num

    !> Do normal `cd' to DIR, and set `list_buffers_directory'.  Port of
    !> shell.el:shell-cd.
    recursive subroutine shell_cd(dir, status)
        character(len=*), intent(in) :: dir
        type(status_type), intent(out) :: status
        call cd(dir, status)
        if (.not. status%ok()) return
        if (minor_modes%shell_dirtrack_mode%enabled()) call list_buffers_directory%set_local(default_directory())
    end subroutine shell_cd

    !> Show the current dirstack on the message line.
    !> Pretty up dirs a bit by changing "/usr/jqr/foo" to "~/foo".  Port of
    !> shell.el:shell-dirstack-message.
    subroutine shell_dirstack_message()
        type(process_arg_type), allocatable :: ds(:)
        type(status_type) :: st
        character(len=:), allocatable :: msg, home, dir
        integer :: k, i, homelen
        if (.not. shell_dirtrack_verbose%value) return
        k = current_entry()
        ds = shell_buffers(k)%dirstack
        call prepend_arg(ds, default_directory())
        call expand_file_name("~/", home, st)
        homelen = len(home)
        msg = ""
        do i = 1, size(ds)
            dir = ds(i)%s
            if (len(dir) >= homelen) then
                if (dir(1:homelen) == home) dir = "~/"//dir(homelen + 1:)
            end if
            msg = msg//directory_file_name(dir)//" "
        end do
        call message(msg)
    end subroutine shell_dirstack_message

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Move forward across ARG shell command(s).  Does not cross lines.
    !> See `shell_command_regexp'.  Port of shell.el:shell-forward-command.
    subroutine shell_forward_command(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: limit, pt, found, moved
        limit = line_end_position()
        pt = point()
        call re_search_forward(shell_command_regexp%value//"\([;&|]["//achar(9)//" ]*\)+", found, status, &
                               bound=limit, noerror=SEARCH_MOVE, count=arg)
        if (.not. status%ok()) return
        if (pt /= point()) moved = skip_syntax_backward(" ", pt)
    end subroutine shell_forward_command

    !> Move backward across ARG shell command(s).  Does not cross lines.
    !> See `shell_command_regexp'.  Port of
    !> shell.el:shell-backward-command.
    subroutine shell_backward_command(arg, status)
        use syntax_fm, only: skip_chars_forward
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: limit, pt, opoint, found, moved
        opoint = point()
        call comint_bol(.false.)
        limit = point()
        call goto_char(opoint)
        if (limit > point()) limit = line_beginning_position()
        moved = skip_syntax_backward(" ", limit)
        pt = point()
        call re_search_backward("[;&|]+["//achar(9)//" ]*\("//shell_command_regexp%value//"\)", found, status, &
                                bound=limit, noerror=SEARCH_MOVE, count=arg)
        if (.not. status%ok()) return
        if (found /= 0) then
            call goto_char(match_beginning(1, status))
            moved = skip_chars_forward(";&|", status)
        end if
        if (pt /= point()) moved = skip_syntax_forward(" ", pt)
    end subroutine shell_backward_command

    !> Narrow buffer to the command line (and any following command output)
    !> at point.  Port of shell.el:shell-narrow-to-prompt.
    subroutine shell_narrow_to_prompt(status)
        type(status_type), intent(out) :: status
        integer(int64) :: begin, opoint, finish
        begin = prompt_begin_position()
        opoint = point()
        call goto_char(prompt_end_position())
        call comint_next_prompt(1)
        if (begin == prompt_begin_position()) then
            finish = point_max()
        else
            finish = prompt_begin_position()
        end if
        call goto_char(opoint)
        call narrow_to_region(begin, finish, status)
    end subroutine shell_narrow_to_prompt

    !> The start of the prompt of the command at point.  Port of
    !> shell.el:shell--prompt-begin-position.
    integer(int64) function prompt_begin_position() result(pos)
        type(status_type) :: st
        integer(int64) :: old_point, a, b, c, found
        old_point = point()
        ! Right result if not on prompt.
        call comint_previous_prompt(1)
        call re_search_backward(comint_prompt_regexp%value, found, st, noerror=SEARCH_NOERROR)
        a = point()
        call goto_char(old_point)
        ! Right result if on first char after prompt.
        call re_search_backward(comint_prompt_regexp%value, found, st, noerror=SEARCH_NOERROR)
        b = point()
        call goto_char(old_point)
        ! Right result if on prompt.
        call comint_next_prompt(1)
        call re_search_backward(comint_prompt_regexp%value, found, st, noerror=SEARCH_NOERROR)
        if (point() <= old_point) then
            c = point()
        else
            c = point_min()
        end if
        call goto_char(old_point)
        pos = max(a, b, c)
    end function prompt_begin_position

    !> The end of the prompt of the command at point.  Port of
    !> shell.el:shell--prompt-end-position.
    integer(int64) function prompt_end_position() result(pos)
        integer(int64) :: opoint
        opoint = point()
        call goto_char(prompt_begin_position())
        call comint_next_prompt(1)
        pos = point()
        call goto_char(opoint)
    end function prompt_end_position

    !> Move forward across ARG shell command(s).
    subroutine cmd_shell_forward_command(status)
        type(status_type), intent(out) :: status
        call shell_forward_command(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_shell_forward_command

    !> Move backward across ARG shell command(s).
    subroutine cmd_shell_backward_command(status)
        type(status_type), intent(out) :: status
        call shell_backward_command(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_shell_backward_command

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Define Shell mode, Shell Command mode, Shell Dirtrack mode and the
    !> settings, and register the commands.
    subroutine syms_of_shell()
        type(status_type) :: st
        if (associated(major_modes%shell_mode%run)) return
        if (.not. associated(major_modes%comint_mode%run)) call syms_of_comint()
        if (.not. allocated(shell_dumb_shell_regexp%value)) shell_dumb_shell_regexp = "cmd\(proxy\)?\.exe"
        if (.not. allocated(shell_prompt_pattern%value)) shell_prompt_pattern = "^[^#$%>"//achar(10)//"]*[#$%>] *"
        if (.not. allocated(shell_delimiter_argument_list%value)) shell_delimiter_argument_list = "|&<>();"
        if (.not. allocated(shell_command_regexp%value)) shell_command_regexp = "[^;&|"//achar(10)//"]+"
        if (.not. allocated(shell_command_separator_regexp%value)) then
            shell_command_separator_regexp = "[;&|"//achar(10)//" "//achar(9)//"]*"
        end if
        if (.not. allocated(shell_popd_regexp%value)) shell_popd_regexp = "popd"
        if (.not. allocated(shell_pushd_regexp%value)) shell_pushd_regexp = "pushd"
        if (.not. allocated(shell_cd_regexp%value)) shell_cd_regexp = "cd"
        if (.not. allocated(shell_dirstack_query)) shell_dirstack_query = "dirs"
        if (.not. allocated(explicit_csh_args)) then
            allocate (explicit_csh_args(1))
            explicit_csh_args(1)%s = "-i"
        end if
        if (.not. allocated(explicit_bash_args)) then
            ! Tell bash not to use readline.
            allocate (explicit_bash_args(2))
            explicit_bash_args(1)%s = "--noediting"
            explicit_bash_args(2)%s = "-i"
        end if
        call forward_setting(explicit_shell_file_name)
        call forward_setting(shell_has_auto_cd)
        call forward_setting(shell_history_file_name)
        call forward_setting(list_buffers_directory, local_if_set=.true.)
        call major_modes%shell_mode%define("shell_mode", "Shell", shell_mode, shell_mode_hook, &
                                           parent=major_modes%comint_mode)
        major_modes%shell_mode%special = .true.
        shell_mode_map = major_modes%shell_mode%map
        call major_modes%shell_command_mode%define("shell_command_mode", "Shell", shell_command_mode, &
                                                   shell_command_mode_hook, parent=major_modes%comint_mode)
        shell_command_mode_map = major_modes%shell_command_mode%map
        call minor_modes%shell_dirtrack_mode%define("shell_dirtrack_mode", hook=shell_dirtrack_mode_hook, &
                                                    body=shell_dirtrack_body)
        call add_local_variables_killer(kill_shell_locals)
        call keymap_set(shell_mode_map, "C-c C-f", "shell_forward_command", st)
        call keymap_set(shell_mode_map, "C-c C-b", "shell_backward_command", st)
        call keymap_set(shell_mode_map, "C-x n d", "shell_narrow_to_prompt", st)
        commands%shell%run => cmd_shell
        commands%shell_mode%run => shell_mode
        commands%shell_mode%interactive = .false.
        commands%shell_command_mode%run => shell_command_mode
        commands%shell_dirtrack_mode%run => shell_dirtrack_mode
        commands%shell_forward_command%run => cmd_shell_forward_command
        commands%shell_backward_command%run => cmd_shell_backward_command
        commands%shell_narrow_to_prompt%run => shell_narrow_to_prompt
    end subroutine syms_of_shell

    !> Add S at the end of LIST.
    subroutine append_arg(list, s)
        type(process_arg_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(process_arg_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))
        do k = 1, n
            if (allocated(list(k)%s)) call move_alloc(list(k)%s, grown(k)%s)
        end do
        grown(n + 1)%s = s
        call move_alloc(grown, list)
    end subroutine append_arg

    !> Add S at the start of LIST.
    subroutine prepend_arg(list, s)
        type(process_arg_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(process_arg_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))
        grown(1)%s = s
        do k = 1, n
            if (allocated(list(k)%s)) call move_alloc(list(k)%s, grown(k + 1)%s)
        end do
        call move_alloc(grown, list)
    end subroutine prepend_arg
end module shell_fm
