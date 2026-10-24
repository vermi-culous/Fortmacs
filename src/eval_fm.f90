! eval_fm.f90 -- hooks and calling functions.
! Ported from GNU Emacs 31.1 src/eval.c and lisp/subr.el (add-hook,
! remove-hook).
!
! Copyright (C) 1985-1987, 1993-1995, 1999-2026 Free Software Foundation,
! Inc.
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

!> Hooks.  A hook is an object holding procedures, run in order:
!>
!>     call text_mode_hook%add(turn_on_auto_fill)
!>     call after_change_functions%add(my_after_change, local=.true.)
!>
!> Upstream a hook is a variable whose value is a list of functions; a
!> buffer-local value holds `t' where the default value's functions run.
!> Here a buffer's own functions are kept in the hook by buffer, with a
!> `t' entry in the same place.
!>
!> The kinds of hook differ only in the arguments their functions take:
!> `hook_type' (normal hooks: no arguments, `command_proc'),
!> `change_hook_type' (`change_function': the bounds of a change) and
!> `after_change_hook_type' (`after_change_function': the bounds and the
!> old length).  They extend `hook_base_type', which keeps the functions,
!> buffers' own values and depths (subr.el's add-hook and remove-hook)
!> once for all kinds; an entry holds a procedure pointer of its hook's
!> kind, and each kind adds the type-bound procedures taking that kind of
!> procedure and running it with its arguments (eval.c's
!> run_hook_with_args).
!>
!> Deferred: the evaluator, and `run-hook-with-args-until-success' and
!> friends (no hook uses them yet).
module eval_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use lisp_fm, only: value_type, nil, command_proc
    use callint_fm, only: funcall, current_prefix_arg
    use buffer_fm, only: buffers, cur
    implicit none(type, external)
    private

    public :: funcall_noargs, kill_local_hooks
    public :: change_function, after_change_function, position_function, region_function, region_entry_type
    public :: string_function, string_filter_function

    abstract interface
        !> A function of a `change_hook_type' hook such as
        !> `before_change_functions': BEG and END are the bounds of the
        !> text about to change.
        subroutine change_function(beg, end, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: beg, end
            type(status_type), intent(out) :: status
        end subroutine change_function

        !> A function of an `after_change_hook_type' hook such as
        !> `after_change_functions': BEG and END are the bounds of the
        !> changed text, OLD_LEN the length of the text it replaced.
        subroutine after_change_function(beg, end, old_len, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: beg, end, old_len
            type(status_type), intent(out) :: status
        end subroutine after_change_function

        !> A function of a `position_hook_type' hook such as
        !> `fontification_functions': POS is the position to work from.
        subroutine position_function(pos, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: pos
            type(status_type), intent(out) :: status
        end subroutine position_function

        !> A function of a `region_hook_type' hook such as
        !> `jit_lock_functions': do something to the text from BEG to END,
        !> giving in DONE_BEG and DONE_END the part it actually did
        !> (upstream's return value (jit-lock-bounds BEG . END); by default
        !> BEG and END).
        subroutine region_function(beg, end, done_beg, done_end, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: beg, end
            integer(int64), intent(out) :: done_beg, done_end
            type(status_type), intent(out) :: status
        end subroutine region_function

        !> A function of a `string_hook_type' hook such as
        !> `comint_output_filter_functions': STRING is the text it is
        !> about.
        subroutine string_function(string, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: string
            type(status_type), intent(out) :: status
        end subroutine string_function

        !> A function of a `string_filter_hook_type' hook such as
        !> `comint_preoutput_filter_functions': RESULT is STRING
        !> transformed (perhaps STRING itself).
        subroutine string_filter_function(string, result, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: string
            character(len=:), allocatable, intent(out) :: result
            type(status_type), intent(out) :: status
        end subroutine string_filter_function
    end interface

    !> A function of a `region_hook_type' hook, as `region_hook_functions'
    !> lists them.
    type :: region_entry_type
        procedure(region_function), pointer, nopass :: fn => null()
    end type region_entry_type

    !> One function of a hook, or the `t' marker of a buffer's own value.
    !> The pointer of the hook's kind is the one set.  In a hook's depths
    !> (subr.el's `hook--depth-alist'), the depth a function was added
    !> with.
    type :: hook_entry_type
        procedure(command_proc), pointer, nopass :: fn => null()
        procedure(change_function), pointer, nopass :: change_fn => null()
        procedure(after_change_function), pointer, nopass :: after_change_fn => null()
        procedure(position_function), pointer, nopass :: position_fn => null()
        procedure(region_function), pointer, nopass :: region_fn => null()
        procedure(string_function), pointer, nopass :: string_fn => null()
        procedure(string_filter_function), pointer, nopass :: string_filter_fn => null()
        integer :: depth = 0
        logical :: is_t = .false.
    end type hook_entry_type

    !> One buffer's own depths (the depth alist made buffer-local).
    type :: local_depths_type
        integer(int64) :: serial = 0
        type(hook_entry_type), allocatable :: depths(:)
    end type local_depths_type

    !> The functions of one buffer's own (buffer-local) value.
    type :: local_hook_type
        !> The buffer's serial number (buffer slots are reused).
        integer(int64) :: serial = 0
        type(hook_entry_type), allocatable :: entries(:)
    end type local_hook_type

    !> What every hook has: its functions in order, buffers' own
    !> functions and the depths they were added with.  The kinds of hook
    !> extend it.
    type, abstract, public :: hook_base_type
        !> The hook's name, as the help commands show it.
        character(len=64) :: name = ""
        !> The default (global) functions.
        type(hook_entry_type), allocatable :: entries(:)
        !> Buffers' own functions.
        type(local_hook_type), allocatable :: locals(:)
        !> The depths functions were added with, and buffers' own.
        type(hook_entry_type), allocatable :: depths(:)
        type(local_depths_type), allocatable :: local_depths(:)
        !> Keep buffers' own functions when the major mode changes (the
        !> `permanent_local' property).
        logical :: permanent_local = .false.
    contains
        !> Forget the current buffer's own functions.
        procedure :: kill_local => hook_kill_local
        !> Whether the current buffer has its own functions.
        procedure :: local_p => hook_local_p
        !> Whether the hook's value in the current buffer is empty.
        procedure :: empty => hook_empty
        !> Make the hook's value in the current buffer empty.
        procedure :: clear => hook_clear
        procedure, private :: add_entry => hook_add_entry
        procedure, private :: remove_entry => hook_remove_entry
        procedure, private :: member_entry => hook_member_entry
        procedure, private :: run_entries => hook_run_entries
    end type hook_base_type

    !> A pointer to a hook.
    type :: hook_ref_type
        class(hook_base_type), pointer :: p => null()
    end type hook_ref_type

    !> Hooks that have been given buffers' own functions.
    type(hook_ref_type), allocatable :: local_hooks(:)

    !> A normal hook: functions of no arguments (`command_proc').
    type, extends(hook_base_type), public :: hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: add => hook_add
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => hook_remove
        !> Run the hook's functions (run-hooks).
        procedure :: run => hook_run
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => hook_member
    end type hook_type

    !> A hook of functions taking the bounds of a change
    !> (`change_function'), like `before_change_functions'.
    type, extends(hook_base_type), public :: change_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: add => change_hook_add
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => change_hook_remove
        !> Run the hook's functions with the bounds of a change
        !> (run-hook-with-args).
        procedure :: run => change_hook_run
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => change_hook_member
    end type change_hook_type

    !> A hook of functions taking the bounds of a change and the length
    !> of the text it replaced (`after_change_function'), like
    !> `after_change_functions'.
    type, extends(hook_base_type), public :: after_change_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: add => after_change_hook_add
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => after_change_hook_remove
        !> Run the hook's functions with the bounds of a change and the
        !> old length (run-hook-with-args).
        procedure :: run => after_change_hook_run
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => after_change_hook_member
    end type after_change_hook_type

    !> A hook of functions taking a position (`position_function'), like
    !> `fontification_functions'.
    type, extends(hook_base_type), public :: position_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: add => position_hook_add
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => position_hook_remove
        !> Run the hook's functions with a position (run-hook-with-args).
        procedure :: run => position_hook_run
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => position_hook_member
    end type position_hook_type

    !> A hook of functions taking a region and telling what part of it
    !> they did (`region_function'), like `jit_lock_functions'.
    type, extends(hook_base_type), public :: region_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: add => region_hook_add
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => region_hook_remove
        !> The functions run in the current buffer, in order (for
        !> run-hook-wrapped).
        procedure :: functions => region_hook_functions
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => region_hook_member
    end type region_hook_type

    !> A hook of functions taking a string (`string_function'), like
    !> `comint_output_filter_functions'.
    type, extends(hook_base_type), public :: string_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: add => string_hook_add
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => string_hook_remove
        !> Run the hook's functions with a string (run-hook-with-args).
        procedure :: run => string_hook_run
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => string_hook_member
    end type string_hook_type

    !> A hook of functions each transforming a string
    !> (`string_filter_function'), like `comint_preoutput_filter_functions'.
    type, extends(hook_base_type), public :: string_filter_hook_type
    contains
        !> Add a function to the hook (subr.el's add-hook).
        procedure :: add => string_filter_hook_add
        !> Remove a function from the hook (subr.el's remove-hook).
        procedure :: remove => string_filter_hook_remove
        !> Pass a string through the hook's functions in turn, each given
        !> what the previous one returned.
        procedure :: run => string_filter_hook_run
        !> Whether FN is one of the functions run in the current buffer.
        procedure :: member => string_filter_hook_member
    end type string_filter_hook_type

    !> Normal hook run before changing the major mode of a buffer.
    !> The function `kill_all_local_variables' runs this before doing
    !> anything else.  Port of buffer.c change-major-mode-hook.
    type(hook_type), public, target :: change_major_mode_hook = hook_type(name="change_major_mode_hook")

contains

    !> Call function FN with no arguments (and no prefix argument).
    !> Port of eval.c:funcall_nil.
    recursive subroutine funcall_noargs(fn, status)
        type(value_type), intent(in) :: fn
        type(status_type), intent(out) :: status
        type(value_type) :: saved
        saved = current_prefix_arg
        current_prefix_arg = nil()
        call funcall(fn, status)
        current_prefix_arg = saved
    end subroutine funcall_noargs

    ! ------------------------------------------------------------------
    ! Entries
    ! ------------------------------------------------------------------

    !> Whether A and B are the same function (neither being `t').
    logical function same_function(a, b) result(same)
        type(hook_entry_type), intent(in) :: a, b
        same = .false.
        if (a%is_t .or. b%is_t) return
        if (associated(a%fn)) then
            if (associated(b%fn)) same = associated(a%fn, b%fn)
        else if (associated(a%change_fn)) then
            if (associated(b%change_fn)) same = associated(a%change_fn, b%change_fn)
        else if (associated(a%after_change_fn)) then
            if (associated(b%after_change_fn)) same = associated(a%after_change_fn, b%after_change_fn)
        else if (associated(a%position_fn)) then
            if (associated(b%position_fn)) same = associated(a%position_fn, b%position_fn)
        else if (associated(a%region_fn)) then
            if (associated(b%region_fn)) same = associated(a%region_fn, b%region_fn)
        else if (associated(a%string_fn)) then
            if (associated(b%string_fn)) same = associated(a%string_fn, b%string_fn)
        else if (associated(a%string_filter_fn)) then
            if (associated(b%string_filter_fn)) same = associated(a%string_filter_fn, b%string_filter_fn)
        end if
    end function same_function

    !> The index of the function of E in ENTRIES, or 0.
    integer function find_entry(entries, e) result(k)
        type(hook_entry_type), intent(in) :: entries(:)
        type(hook_entry_type), intent(in) :: e
        integer :: i
        k = 0
        do i = 1, size(entries)
            if (same_function(entries(i), e)) then
                k = i
                return
            end if
        end do
    end function find_entry

    !> Whether ENTRIES has the `t' of a buffer's own value.
    logical function has_t(entries)
        type(hook_entry_type), intent(in) :: entries(:)
        integer :: i
        has_t = .false.
        do i = 1, size(entries)
            if (entries(i)%is_t) then
                has_t = .true.
                return
            end if
        end do
    end function has_t

    !> Add the function of E to ENTRIES at DEPTH (after equal depths when
    !> DEPTH > 0, before them otherwise), keeping them sorted by depth.
    subroutine insert_entry(entries, e, depth)
        type(hook_entry_type), allocatable, intent(inout) :: entries(:)
        type(hook_entry_type), intent(in) :: e
        integer, intent(in) :: depth
        type(hook_entry_type), allocatable :: grown(:)
        integer :: n, at, i
        n = size(entries)
        if (depth > 0) then
            at = n + 1
            do i = n, 1, -1
                if (entries(i)%depth <= depth) exit
                at = i
            end do
        else
            at = 1
            do i = 1, n
                if (entries(i)%depth >= depth) exit
                at = i + 1
            end do
        end if
        allocate (grown(n + 1))
        grown(1:at - 1) = entries(1:at - 1)
        grown(at) = e
        grown(at)%depth = depth
        grown(at + 1:n + 1) = entries(at:n)
        call move_alloc(grown, entries)
    end subroutine insert_entry

    !> ENTRIES without element K.
    subroutine delete_entry(entries, k)
        type(hook_entry_type), allocatable, intent(inout) :: entries(:)
        integer, intent(in) :: k
        type(hook_entry_type), allocatable :: kept(:)
        integer :: n
        n = size(entries)
        allocate (kept(n - 1))
        kept(1:k - 1) = entries(1:k - 1)
        kept(k:n - 1) = entries(k + 1:n)
        call move_alloc(kept, entries)
    end subroutine delete_entry

    ! ------------------------------------------------------------------
    ! Buffers' own values and depths
    ! ------------------------------------------------------------------

    !> The index of the current buffer's own functions in HOOK, or 0.
    integer function local_index(hook) result(k)
        class(hook_base_type), intent(in) :: hook
        integer :: i
        k = 0
        if (cur == 0 .or. .not. allocated(hook%locals)) return
        do i = 1, size(hook%locals)
            if (hook%locals(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do
    end function local_index

    !> The index of the current buffer's own depths in HOOK, or 0.
    integer function local_depths_index(hook) result(k)
        class(hook_base_type), intent(in) :: hook
        integer :: i
        k = 0
        if (cur == 0 .or. .not. allocated(hook%local_depths)) return
        do i = 1, size(hook%local_depths)
            if (hook%local_depths(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do
    end function local_depths_index

    !> The remembered depth of the function of E in DEPTHS, or 0.
    integer function depth_of(depths, e) result(d)
        type(hook_entry_type), allocatable, intent(in) :: depths(:)
        type(hook_entry_type), intent(in) :: e
        integer :: k
        d = 0
        if (.not. allocated(depths)) return
        k = find_entry(depths, e)
        if (k > 0) d = depths(k)%depth
    end function depth_of

    !> Remember that the function of E was added with DEPTH.
    subroutine remember_depth(depths, e, depth)
        type(hook_entry_type), allocatable, intent(inout) :: depths(:)
        type(hook_entry_type), intent(in) :: e
        integer, intent(in) :: depth
        type(hook_entry_type), allocatable :: grown(:)
        integer :: k, n
        if (.not. allocated(depths)) allocate (depths(0))
        k = find_entry(depths, e)
        if (k > 0) then
            depths(k)%depth = depth
            return
        end if
        n = size(depths)
        allocate (grown(n + 1))
        grown(1:n) = depths
        grown(n + 1) = e
        grown(n + 1)%depth = depth
        call move_alloc(grown, depths)
    end subroutine remember_depth

    !> Forget the depth of the function of E in DEPTHS.
    subroutine forget_depth(depths, e)
        type(hook_entry_type), allocatable, intent(inout) :: depths(:)
        type(hook_entry_type), intent(in) :: e
        integer :: k
        if (.not. allocated(depths)) return
        k = find_entry(depths, e)
        if (k > 0) call delete_entry(depths, k)
    end subroutine forget_depth

    !> The depth the function of E goes in at: DEPTH if not zero (which is
    !> remembered), else the depth it was added with before.  LOCAL uses
    !> the current buffer's own depths, made from the default ones when
    !> first needed.
    integer function entry_depth(hook, e, depth, local) result(d)
        class(hook_base_type), intent(inout) :: hook
        type(hook_entry_type), intent(in) :: e
        integer, intent(in) :: depth
        logical, intent(in) :: local
        type(local_depths_type), allocatable :: grown(:)
        integer :: k, n
        d = depth
        k = 0
        if (local) k = local_depths_index(hook)
        if (depth == 0) then
            if (k > 0) then
                d = depth_of(hook%local_depths(k)%depths, e)
            else
                d = depth_of(hook%depths, e)
            end if
            return
        end if
        if (.not. local) then
            call remember_depth(hook%depths, e, depth)
            return
        end if
        if (k == 0) then
            ! (make-local-variable depth-sym): starts as the default.
            if (.not. allocated(hook%local_depths)) allocate (hook%local_depths(0))
            n = size(hook%local_depths)
            allocate (grown(n + 1))
            grown(1:n) = hook%local_depths
            grown(n + 1)%serial = buffers(cur)%serial
            if (allocated(hook%depths)) then
                grown(n + 1)%depths = hook%depths
            else
                allocate (grown(n + 1)%depths(0))
            end if
            call move_alloc(grown, hook%local_depths)
            k = n + 1
        end if
        call remember_depth(hook%local_depths(k)%depths, e, depth)
    end function entry_depth

    !> Remember HOOK among the hooks with buffers' own functions.
    subroutine note_local_hook(hook)
        class(hook_base_type), intent(inout), target :: hook
        type(hook_ref_type), allocatable :: grown(:)
        integer :: i, n
        if (.not. allocated(local_hooks)) allocate (local_hooks(0))
        n = size(local_hooks)
        do i = 1, n
            if (associated(local_hooks(i)%p, hook)) return
        end do
        allocate (grown(n + 1))
        grown(1:n) = local_hooks
        grown(n + 1)%p => hook
        call move_alloc(grown, local_hooks)
    end subroutine note_local_hook

    !> Forget the current buffer's own hook functions, except those of
    !> permanent-local hooks unless KILL_PERMANENT (part of
    !> buffer.c:reset_buffer_local_variables).
    subroutine kill_local_hooks(kill_permanent)
        logical, intent(in) :: kill_permanent
        integer :: i
        if (.not. allocated(local_hooks)) return
        do i = 1, size(local_hooks)
            ! The depth alist is not permanent.
            call kill_local_depths(local_hooks(i)%p)
            if (local_hooks(i)%p%permanent_local .and. .not. kill_permanent) cycle
            call local_hooks(i)%p%kill_local()
        end do
    end subroutine kill_local_hooks

    !> Forget the current buffer's own depths of HOOK.
    subroutine kill_local_depths(hook)
        class(hook_base_type), intent(inout) :: hook
        type(local_depths_type), allocatable :: kept(:)
        integer :: k, n
        k = local_depths_index(hook)
        if (k == 0) return
        n = size(hook%local_depths)
        allocate (kept(n - 1))
        kept(1:k - 1) = hook%local_depths(1:k - 1)
        kept(k:n - 1) = hook%local_depths(k + 1:n)
        call move_alloc(kept, hook%local_depths)
    end subroutine kill_local_depths

    ! ------------------------------------------------------------------
    ! All kinds of hook
    ! ------------------------------------------------------------------

    !> Add the function of E to the hook: see `hook_add'.
    !> Port of subr.el:add-hook.
    subroutine hook_add_entry(self, e, depth, local)
        class(hook_base_type), intent(inout), target :: self
        type(hook_entry_type), intent(in) :: e
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(local_hook_type), allocatable :: grown(:)
        integer :: d, k, n
        logical :: loc
        d = 0
        if (present(depth)) d = depth
        loc = .false.
        if (present(local)) loc = local
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        if (.not. allocated(self%locals)) allocate (self%locals(0))
        k = local_index(self)
        if (loc) then
            if (cur == 0) return
            if (k == 0) then
                ! (set (make-local-variable hook) (list t))
                n = size(self%locals)
                allocate (grown(n + 1))
                grown(1:n) = self%locals
                grown(n + 1)%serial = buffers(cur)%serial
                allocate (grown(n + 1)%entries(1))
                grown(n + 1)%entries(1)%is_t = .true.
                call move_alloc(grown, self%locals)
                k = n + 1
                call note_local_hook(self)
            end if
        else if (k > 0) then
            ! A buffer's own value without `t' (made empty by `clear'):
            ! change that value.
            if (.not. has_t(self%locals(k)%entries)) loc = .true.
        end if
        if (loc) then
            if (find_entry(self%locals(k)%entries, e) == 0) then
                call insert_entry(self%locals(k)%entries, e, entry_depth(self, e, d, .true.))
            end if
        else if (find_entry(self%entries, e) == 0) then
            call insert_entry(self%entries, e, entry_depth(self, e, d, .false.))
        end if
    end subroutine hook_add_entry

    !> Remove the function of E from the hook: see `hook_remove'.
    !> Port of subr.el:remove-hook.
    subroutine hook_remove_entry(self, e, local)
        class(hook_base_type), intent(inout) :: self
        type(hook_entry_type), intent(in) :: e
        logical, intent(in), optional :: local
        integer :: k, i, kd
        logical :: loc
        loc = .false.
        if (present(local)) loc = local
        k = local_index(self)
        ! Do nothing if LOCAL is true but this hook has no local binding.
        if (loc .and. k == 0) return
        ! A buffer's own value without `t': change that value.
        if (k > 0) then
            if (.not. has_t(self%locals(k)%entries)) loc = .true.
        end if
        if (loc) then
            i = find_entry(self%locals(k)%entries, e)
            if (i == 0) return
            call delete_entry(self%locals(k)%entries, i)
            ! Remove auxiliary depth info too: the buffer's own if it has
            ! any, else the default.
            kd = local_depths_index(self)
            if (kd > 0) then
                call forget_depth(self%local_depths(kd)%depths, e)
            else
                call forget_depth(self%depths, e)
            end if
            ! A local value of just (t) is killed.
            if (size(self%locals(k)%entries) == 1) then
                if (self%locals(k)%entries(1)%is_t) call self%kill_local()
            end if
        else
            if (.not. allocated(self%entries)) return
            i = find_entry(self%entries, e)
            if (i == 0) return
            call delete_entry(self%entries, i)
            call forget_depth(self%depths, e)
        end if
    end subroutine hook_remove_entry

    !> Whether the function of E is run in the current buffer.
    logical function hook_member_entry(self, e) result(found)
        class(hook_base_type), intent(in) :: self
        type(hook_entry_type), intent(in) :: e
        integer :: k
        found = .false.
        k = local_index(self)
        if (k > 0) found = find_entry(self%locals(k)%entries, e) > 0
        if (.not. found .and. allocated(self%entries)) found = find_entry(self%entries, e) > 0
    end function hook_member_entry

    !> Run each function of the hook in the current buffer, in order,
    !> stopping at the first error.  In a buffer with its own functions,
    !> those run, with the default functions in the place of `t'.  BEG,
    !> END and OLD_LEN are the arguments for the functions of the kinds
    !> that take them.  Port of eval.c:run_hook_with_args.
    recursive subroutine hook_run_entries(self, beg, end, old_len, status, safe)
        class(hook_base_type), intent(in) :: self
        integer(int64), intent(in) :: beg, end, old_len
        type(status_type), intent(out) :: status
        !> Ignore errors: an error in one function doesn't stop the others
        !> (redisplay's safe calls).
        logical, intent(in), optional :: safe
        type(hook_entry_type), allocatable :: entries(:), global(:)
        integer :: i, j, k
        logical :: keep_going
        status = status_type()
        keep_going = .false.
        if (present(safe)) keep_going = safe
        k = local_index(self)
        ! Copies, in case a function changes the hook.
        if (k > 0) then
            entries = self%locals(k)%entries
        else if (allocated(self%entries)) then
            entries = self%entries
        else
            return
        end if
        do i = 1, size(entries)
            if (entries(i)%is_t) then
                ! t indicates this hook has a local binding; it means to
                ! run the global binding too.
                if (.not. allocated(self%entries)) cycle
                global = self%entries
                do j = 1, size(global)
                    ! In a global value, t should not occur.  If it does,
                    ! we must ignore it to avoid an endless loop.
                    if (global(j)%is_t) cycle
                    call call_entry(global(j), beg, end, old_len, status)
                    if (keep_going) status = status_type()
                    if (.not. status%ok()) return
                end do
            else
                call call_entry(entries(i), beg, end, old_len, status)
                if (keep_going) status = status_type()
                if (.not. status%ok()) return
            end if
        end do
    end subroutine hook_run_entries

    !> Call the function of E with the arguments its kind takes.  A
    !> normal hook's functions are commands: they run without a prefix
    !> argument.
    recursive subroutine call_entry(e, beg, end, old_len, status)
        type(hook_entry_type), intent(in) :: e
        integer(int64), intent(in) :: beg, end, old_len
        type(status_type), intent(out) :: status
        type(value_type) :: saved
        status = status_type()
        if (associated(e%fn)) then
            saved = current_prefix_arg
            current_prefix_arg = nil()
            call e%fn(status)
            current_prefix_arg = saved
        else if (associated(e%change_fn)) then
            call e%change_fn(beg, end, status)
        else if (associated(e%after_change_fn)) then
            call e%after_change_fn(beg, end, old_len, status)
        else if (associated(e%position_fn)) then
            call e%position_fn(beg, status)
        end if
    end subroutine call_entry

    subroutine hook_kill_local(self)
        class(hook_base_type), intent(inout) :: self
        type(local_hook_type), allocatable :: kept(:)
        integer :: k, n
        k = local_index(self)
        if (k == 0) return
        n = size(self%locals)
        allocate (kept(n - 1))
        kept(1:k - 1) = self%locals(1:k - 1)
        kept(k:n - 1) = self%locals(k + 1:n)
        call move_alloc(kept, self%locals)
    end subroutine hook_kill_local

    logical function hook_local_p(self)
        class(hook_base_type), intent(in) :: self
        hook_local_p = local_index(self) > 0
    end function hook_local_p

    !> Whether the hook's value in the current buffer is empty (upstream:
    !> nil): no functions, and for a buffer's own value no `t' either.
    logical function hook_empty(self)
        class(hook_base_type), intent(in) :: self
        integer :: k
        k = local_index(self)
        if (k > 0) then
            hook_empty = size(self%locals(k)%entries) == 0
        else if (allocated(self%entries)) then
            hook_empty = size(self%entries) == 0
        else
            hook_empty = .true.
        end if
    end function hook_empty

    !> Make the hook's value in the current buffer empty, as setting the
    !> variable to nil does upstream: the buffer's own value if it has
    !> one (which then runs nothing, not even the default functions),
    !> else the default value.  Depths are kept.
    subroutine hook_clear(self)
        class(hook_base_type), intent(inout) :: self
        integer :: k
        k = local_index(self)
        if (k > 0) then
            deallocate (self%locals(k)%entries)
            allocate (self%locals(k)%entries(0))
        else
            if (allocated(self%entries)) deallocate (self%entries)
            allocate (self%entries(0))
        end if
    end subroutine hook_clear

    ! ------------------------------------------------------------------
    ! Normal hooks
    ! ------------------------------------------------------------------

    !> Add to the value of HOOK the function FUNCTION.
    !> FUNCTION is not added if already present.
    !>
    !> The place where the function is added depends on the DEPTH
    !> parameter.  DEPTH defaults to 0.  By convention, it should be
    !> a number between -100 and 100 where 100 means that the function
    !> should be at the very end of the list, whereas -100 means that
    !> the function should always come first.
    !> Since nothing is "always" true, don't use 100 nor -100.
    !> When two functions have the same depth, the new one gets added after the
    !> old one if depth is strictly positive and before otherwise.
    !>
    !> The optional argument LOCAL, if true, says to modify
    !> the hook's buffer-local value rather than its global value.
    !> This makes the hook buffer-local, and it makes t a member of the
    !> buffer-local value.  That acts as a flag to run the hook
    !> functions of the global value as well as in the local value.
    !> Port of subr.el:add-hook (`t' for DEPTH is 90).
    subroutine hook_add(self, fn, depth, local)
        class(hook_type), intent(inout), target :: self
        procedure(command_proc) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%fn => fn
        call self%add_entry(e, depth, local)
    end subroutine hook_add

    !> Remove FUNCTION from HOOK's functions.
    !> Do nothing if HOOK does not currently contain FUNCTION.
    !>
    !> The optional argument LOCAL, if true, says to modify
    !> the hook's buffer-local value rather than its default value.
    !> Port of subr.el:remove-hook.
    subroutine hook_remove(self, fn, local)
        class(hook_type), intent(inout) :: self
        procedure(command_proc) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%fn => fn
        call self%remove_entry(e, local)
    end subroutine hook_remove

    !> Run each function of the hook, in order, stopping at the first
    !> error.  In a buffer with its own functions, those run, with the
    !> default functions in the place of `t'.  Port of eval.c:run_hook
    !> (via run_hook_with_args).
    recursive subroutine hook_run(self, status)
        class(hook_type), intent(in) :: self
        type(status_type), intent(out) :: status
        call self%run_entries(0_int64, 0_int64, 0_int64, status)
    end subroutine hook_run

    logical function hook_member(self, fn) result(found)
        class(hook_type), intent(in) :: self
        procedure(command_proc) :: fn
        type(hook_entry_type) :: e
        e%fn => fn
        found = self%member_entry(e)
    end function hook_member

    ! ------------------------------------------------------------------
    ! Hooks of change functions
    ! ------------------------------------------------------------------

    !> Add FN to the hook, as `hook_add' does.  Port of subr.el:add-hook.
    subroutine change_hook_add(self, fn, depth, local)
        class(change_hook_type), intent(inout), target :: self
        procedure(change_function) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%change_fn => fn
        call self%add_entry(e, depth, local)
    end subroutine change_hook_add

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine change_hook_remove(self, fn, local)
        class(change_hook_type), intent(inout) :: self
        procedure(change_function) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%change_fn => fn
        call self%remove_entry(e, local)
    end subroutine change_hook_remove

    !> Call each function of the hook with BEG and END, in order, stopping
    !> at the first error; buffers' own functions as `hook_run'.
    !> Port of eval.c:Frun_hook_with_args.
    recursive subroutine change_hook_run(self, beg, end, status)
        class(change_hook_type), intent(in) :: self
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        call self%run_entries(beg, end, 0_int64, status)
    end subroutine change_hook_run

    logical function change_hook_member(self, fn) result(found)
        class(change_hook_type), intent(in) :: self
        procedure(change_function) :: fn
        type(hook_entry_type) :: e
        e%change_fn => fn
        found = self%member_entry(e)
    end function change_hook_member

    !> Add FN to the hook, as `hook_add' does.  Port of subr.el:add-hook.
    subroutine after_change_hook_add(self, fn, depth, local)
        class(after_change_hook_type), intent(inout), target :: self
        procedure(after_change_function) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%after_change_fn => fn
        call self%add_entry(e, depth, local)
    end subroutine after_change_hook_add

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine after_change_hook_remove(self, fn, local)
        class(after_change_hook_type), intent(inout) :: self
        procedure(after_change_function) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%after_change_fn => fn
        call self%remove_entry(e, local)
    end subroutine after_change_hook_remove

    !> Call each function of the hook with BEG, END and OLD_LEN, in order,
    !> stopping at the first error; buffers' own functions as
    !> `hook_run'.  Port of eval.c:Frun_hook_with_args.
    recursive subroutine after_change_hook_run(self, beg, end, old_len, status)
        class(after_change_hook_type), intent(in) :: self
        integer(int64), intent(in) :: beg, end, old_len
        type(status_type), intent(out) :: status
        call self%run_entries(beg, end, old_len, status)
    end subroutine after_change_hook_run

    logical function after_change_hook_member(self, fn) result(found)
        class(after_change_hook_type), intent(in) :: self
        procedure(after_change_function) :: fn
        type(hook_entry_type) :: e
        e%after_change_fn => fn
        found = self%member_entry(e)
    end function after_change_hook_member

    ! ------------------------------------------------------------------
    ! Position and region hooks
    ! ------------------------------------------------------------------

    !> Add FN to the hook, as `hook_add' does.  Port of subr.el:add-hook.
    subroutine position_hook_add(self, fn, depth, local)
        class(position_hook_type), intent(inout), target :: self
        procedure(position_function) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%position_fn => fn
        call self%add_entry(e, depth, local)
    end subroutine position_hook_add

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine position_hook_remove(self, fn, local)
        class(position_hook_type), intent(inout) :: self
        procedure(position_function) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%position_fn => fn
        call self%remove_entry(e, local)
    end subroutine position_hook_remove

    !> Call each function of the hook with POS, in order, stopping at the
    !> first error; buffers' own functions as `hook_run'.  Port of
    !> eval.c:Frun_hook_with_args.
    recursive subroutine position_hook_run(self, pos, status, safe)
        class(position_hook_type), intent(in) :: self
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        !> Ignore errors, calling every function (redisplay's safe calls).
        logical, intent(in), optional :: safe
        call self%run_entries(pos, 0_int64, 0_int64, status, safe)
    end subroutine position_hook_run

    logical function position_hook_member(self, fn) result(found)
        class(position_hook_type), intent(in) :: self
        procedure(position_function) :: fn
        type(hook_entry_type) :: e
        e%position_fn => fn
        found = self%member_entry(e)
    end function position_hook_member

    !> Add FN to the hook, as `hook_add' does.  Port of subr.el:add-hook.
    subroutine region_hook_add(self, fn, depth, local)
        class(region_hook_type), intent(inout), target :: self
        procedure(region_function) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%region_fn => fn
        call self%add_entry(e, depth, local)
    end subroutine region_hook_add

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine region_hook_remove(self, fn, local)
        class(region_hook_type), intent(inout) :: self
        procedure(region_function) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%region_fn => fn
        call self%remove_entry(e, local)
    end subroutine region_hook_remove

    logical function region_hook_member(self, fn) result(found)
        class(region_hook_type), intent(in) :: self
        procedure(region_function) :: fn
        type(hook_entry_type) :: e
        e%region_fn => fn
        found = self%member_entry(e)
    end function region_hook_member

    !> The functions of the hook run in the current buffer, in order: the
    !> buffer's own, with the global ones in place of `t'.
    function region_hook_functions(self) result(fns)
        class(region_hook_type), intent(in) :: self
        type(region_entry_type), allocatable :: fns(:)
        type(hook_entry_type), allocatable :: entries(:)
        integer :: i, j, k, n
        allocate (fns(0))
        k = local_index(self)
        if (k > 0) then
            entries = self%locals(k)%entries
        else if (allocated(self%entries)) then
            entries = self%entries
        else
            return
        end if
        n = 0
        do i = 1, size(entries)
            if (entries(i)%is_t) then
                if (.not. allocated(self%entries)) cycle
                do j = 1, size(self%entries)
                    if (self%entries(j)%is_t) cycle
                    call push(self%entries(j))
                end do
            else
                call push(entries(i))
            end if
        end do
    contains
        subroutine push(e)
            type(hook_entry_type), intent(in) :: e
            type(region_entry_type), allocatable :: grown(:)
            allocate (grown(size(fns) + 1))
            grown(1:size(fns)) = fns
            grown(size(fns) + 1)%fn => e%region_fn
            call move_alloc(grown, fns)
            n = n + 1
        end subroutine push
    end function region_hook_functions

    !> The entries run in the current buffer, in order: the buffer's own,
    !> with the global ones in place of `t' (copies, in case a function
    !> changes the hook).
    function effective_entries(self) result(run)
        class(hook_base_type), intent(in) :: self
        type(hook_entry_type), allocatable :: run(:)
        type(hook_entry_type), allocatable :: entries(:)
        logical, allocatable :: keep(:)
        integer :: i, k
        allocate (run(0))
        k = local_index(self)
        if (k > 0) then
            entries = self%locals(k)%entries
        else if (allocated(self%entries)) then
            entries = self%entries
        else
            return
        end if
        do i = 1, size(entries)
            if (entries(i)%is_t) then
                ! In a global value, t should not occur.  If it does, we
                ! must ignore it to avoid an endless loop.
                if (.not. allocated(self%entries)) cycle
                allocate (keep(size(self%entries)))
                keep = .not. self%entries%is_t
                run = [run, pack(self%entries, keep)]
                deallocate (keep)
            else
                run = [run, entries(i)]
            end if
        end do
    end function effective_entries

    !> Add FN to the hook, as `hook_add' does.  Port of subr.el:add-hook.
    subroutine string_hook_add(self, fn, depth, local)
        class(string_hook_type), intent(inout), target :: self
        procedure(string_function) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%string_fn => fn
        call self%add_entry(e, depth, local)
    end subroutine string_hook_add

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine string_hook_remove(self, fn, local)
        class(string_hook_type), intent(inout) :: self
        procedure(string_function) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%string_fn => fn
        call self%remove_entry(e, local)
    end subroutine string_hook_remove

    !> Call each function of the hook with STRING, in order, stopping at
    !> the first error; buffers' own functions as `hook_run'.  Port of
    !> eval.c:Frun_hook_with_args.
    recursive subroutine string_hook_run(self, string, status)
        class(string_hook_type), intent(in) :: self
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(hook_entry_type), allocatable :: entries(:)
        integer :: i
        status = status_type()
        entries = effective_entries(self)
        do i = 1, size(entries)
            if (.not. associated(entries(i)%string_fn)) cycle
            call entries(i)%string_fn(string, status)
            if (.not. status%ok()) return
        end do
    end subroutine string_hook_run

    logical function string_hook_member(self, fn) result(found)
        class(string_hook_type), intent(in) :: self
        procedure(string_function) :: fn
        type(hook_entry_type) :: e
        e%string_fn => fn
        found = self%member_entry(e)
    end function string_hook_member

    !> Add FN to the hook, as `hook_add' does.  Port of subr.el:add-hook.
    subroutine string_filter_hook_add(self, fn, depth, local)
        class(string_filter_hook_type), intent(inout), target :: self
        procedure(string_filter_function) :: fn
        integer, intent(in), optional :: depth
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%string_filter_fn => fn
        call self%add_entry(e, depth, local)
    end subroutine string_filter_hook_add

    !> Remove FN from the hook, as `hook_remove' does.
    !> Port of subr.el:remove-hook.
    subroutine string_filter_hook_remove(self, fn, local)
        class(string_filter_hook_type), intent(inout) :: self
        procedure(string_filter_function) :: fn
        logical, intent(in), optional :: local
        type(hook_entry_type) :: e
        e%string_filter_fn => fn
        call self%remove_entry(e, local)
    end subroutine string_filter_hook_remove

    !> Give STRING to the first function of the hook, what it returns to
    !> the next, and so on; RESULT is what the last one returns (STRING if
    !> there are none).  Stops at the first error.  A `t' in a buffer's own
    !> value runs the default functions there.
    recursive subroutine string_filter_hook_run(self, string, result, status)
        class(string_filter_hook_type), intent(in) :: self
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        type(hook_entry_type), allocatable :: entries(:)
        character(len=:), allocatable :: next
        integer :: i
        status = status_type()
        result = string
        entries = effective_entries(self)
        do i = 1, size(entries)
            if (.not. associated(entries(i)%string_filter_fn)) cycle
            call entries(i)%string_filter_fn(result, next, status)
            if (.not. status%ok()) return
            call move_alloc(next, result)
        end do
    end subroutine string_filter_hook_run

    logical function string_filter_hook_member(self, fn) result(found)
        class(string_filter_hook_type), intent(in) :: self
        procedure(string_filter_function) :: fn
        type(hook_entry_type) :: e
        e%string_filter_fn => fn
        found = self%member_entry(e)
    end function string_filter_hook_member
end module eval_fm
