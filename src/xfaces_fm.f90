! xfaces_fm.f90 -- merging and realizing faces for a terminal frame.
! Ported from GNU Emacs 31.1 src/xfaces.c.
!
! Copyright (C) 1993-2026 Free Software Foundation, Inc.
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

!> Realized faces.  A frame's face cache (`frame_type%faces') holds the
!> basic faces at their fixed ids (DEFAULT_FACE_ID,
!> MODE_LINE_ACTIVE_FACE_ID, ...) and every other face redisplay asks
!> for, each with the fully specified attributes it was realized from
!> (`face_type%lface').  `lookup_face' finds or realizes the face for a
!> set of attributes; `face_at_buffer_position' gives the face of buffer
!> text (its `face' text property over the default face, then the
!> region); `merge_faces' merges a named face into a realized one.
!>
!> Face references in text properties are face names (symbols, in the
!> Fortran spelling: `region', `font_lock_comment_face'), property lists
!> of attributes with the Fortran attribute names as keywords
!> (`:foreground', `:inverse_video', `:strike_through'), or lists of
!> face references, the first taking precedence.
!>
!> Overlays' `face' properties are merged (the active region stands in for
!> redisplay's region overlay).
!>
!> Deferred: `face_remapping_alist', mouse faces, face filters
!> (`:filter'), the cons forms (FOREGROUND-COLOR . COLOR) of face
!> references (no dotted pairs), the `reset' attribute value, relative
!> (float) and functional heights in face references, fonts, fontsets,
!> stipples and the window-system faces, `internal_set_lisp_face_attribute'
!> (faces change by assignment in the configuration, then
!> `clear_face_cache').
module xfaces_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type
    use lisp_fm, only: value_type, symbols, nilp, symbolp, stringp, consp, fixnump, xsymbol, xstring, &
                       xfixnum, list_elements
    use lread_fm, only: sym
    use dispextern_fm, only: face_type, FACE_TTY_DEFAULT_COLOR, FACE_TTY_DEFAULT_FG_COLOR, &
                             FACE_TTY_DEFAULT_BG_COLOR, FACE_NO_UNDERLINE, FACE_UNDERLINE_SINGLE, &
                             FACE_UNDERLINE_DOUBLE_LINE, FACE_UNDERLINE_WAVE, FACE_UNDERLINE_DOTS, &
                             FACE_UNDERLINE_DASHES, DEFAULT_FACE_ID, MODE_LINE_ACTIVE_FACE_ID, MARGIN_FACE_ID, &
                             BASIC_FACE_ID_SENTINEL, TTY_CAP_INVERSE, TTY_CAP_UNDERLINE, TTY_CAP_BOLD, &
                             TTY_CAP_DIM, TTY_CAP_ITALIC, TTY_CAP_STRIKE_THROUGH, TTY_CAP_UNDERLINE_STYLED
    use faces_fm, only: face_attributes_type, named_face_type, face_named, faces_equal, face_weight_numeric, &
                        face_slant_numeric, set_frame_display, face_spec_recalc_all, &
                        display_supports_face_attributes_p, default, &
                        FACE_UNSPECIFIED, FACE_NIL, FACE_T, FACE_COLOR, FACE_PLIST, FACE_LIST, FACE_VALUE, &
                        FACE_STYLE_LINE, FACE_STYLE_DOUBLE_LINE, FACE_STYLE_WAVE, FACE_STYLE_DOTS, &
                        FACE_STYLE_DASHES, FACE_CLASS_COLOR, FACE_CLASS_MONO, FACE_BACKGROUND_LIGHT, &
                        FACE_BACKGROUND_DARK
    use frame_fm, only: frame_type, selected_frame, frame_window_p
    use term_fm, only: tty_capable_p
    use tty_colors_fm, only: tty_color_desc, tty_color_assoc, tty_color_standard_values, &
                             tty_defined_color_count, clear_face_cache_hook
    use buffer_fm, only: buffers, cur, set_buffer_internal
    use textprop_fm, only: get_text_property, next_single_property_change
    use handle_fm, only: handle_type
    use itree_fm, only: overlays_at, sort_overlays, overlay_get, overlay_end, overlay_start, overlay_priority, &
                        compare_overlays
    implicit none(type, external)
    private

    public :: realize_basic_faces, recompute_basic_faces, clear_face_cache, lookup_face, lookup_named_face, &
              merge_faces, face_at_buffer_position, merge_face_ref, tty_supports_face_attributes_p, &
              init_frame_faces, frame_background_mode

    !> Suppress/allow boldness of faces with inverse default colors.
    !> If true, bold faces on terminals whose foreground is the default
    !> background color of the display and whose background is the
    !> default foreground color ignore their boldness.  Port of
    !> xfaces.c:tty_suppress_bold_inverse_default_colors_p (set by
    !> tty-suppress-bold-inverse-default-colors).
    logical, public :: tty_suppress_bold_inverse_default_colors_p = .false.

    !> TEXT_PROP_DISTANCE_LIMIT: how far redisplay looks ahead for the
    !> next face change.
    integer(int64), parameter, public :: TEXT_PROP_DISTANCE_LIMIT = 100

    !> How deep face references may nest (named_merge_points detect
    !> cycles upstream; a chain this deep is taken as a cycle).
    integer, parameter :: MAX_MERGE_DEPTH = 32

    !> TTY_SAME_COLOR_THRESHOLD: the largest color_distance at which a
    !> terminal color still counts as the color asked for.
    integer(int64), parameter :: TTY_SAME_COLOR_THRESHOLD = 10000

    !> An RGB color with its terminal index (Emacs_Color).
    type :: emacs_color_type
        integer(int64) :: pixel = FACE_TTY_DEFAULT_COLOR
        integer(int64) :: red = 0, green = 0, blue = 0
    end type emacs_color_type

    !> The names of the basic faces, by face id (realize_basic_faces).
    character(len=26), parameter :: basic_face_names(MODE_LINE_ACTIVE_FACE_ID:MARGIN_FACE_ID) = [ &
                                    "mode_line_active          ", "mode_line_inactive        ", &
                                    "tool_bar                  ", "fringe                    ", &
                                    "header_line_active        ", "header_line_inactive      ", &
                                    "scroll_bar                ", "border                    ", &
                                    "cursor                    ", "mouse                     ", &
                                    "menu                      ", "vertical_border           ", &
                                    "window_divider            ", "window_divider_first_pixel", &
                                    "window_divider_last_pixel ", "internal_border           ", &
                                    "child_frame_border        ", "tab_bar                   ", &
                                    "tab_line_active           ", "tab_line_inactive         ", &
                                    "margin                    "]

contains

    ! ------------------------------------------------------------------
    ! Setting up a frame's faces
    ! ------------------------------------------------------------------

    !> The background mode of a terminal of type TERM_TYPE whose
    !> background color is BG_COLOR ("unspecified-bg" when the terminal
    !> didn't say): light for xterm, rxvt, dtterm and eterm (whose
    !> default background is white), else dark.  The terminal part of
    !> frame.el:frame-set-background-mode (frame-background-mode nil; the
    !> background color xterm.el queries is deferred).
    integer function frame_background_mode(term_type, bg_color) result(mode)
        character(len=*), intent(in) :: term_type, bg_color
        logical :: xterm_like
        xterm_like = starts_with(term_type, "xterm") .or. starts_with(term_type, "rxvt") .or. &
                     starts_with(term_type, "dtterm") .or. starts_with(term_type, "eterm")
        if (bg_color == "unspecified-fg") then
            ! Inverted colors.
            mode = FACE_BACKGROUND_LIGHT
            if (xterm_like) mode = FACE_BACKGROUND_DARK
        else
            mode = FACE_BACKGROUND_DARK
            if (xterm_like) mode = FACE_BACKGROUND_LIGHT
        end if
    contains
        logical function starts_with(s, prefix)
            character(len=*), intent(in) :: s, prefix
            starts_with = .false.
            if (len(s) >= len(prefix)) starts_with = s(1:len(prefix)) == prefix
        end function starts_with
    end function frame_background_mode

    !> Set up the faces of terminal frame F, whose terminal type is
    !> TERM_TYPE: tell the face specs about the display (its class,
    !> background mode and colors), recompute every face's attributes, and
    !> realize the basic faces.  Port of what
    !> frame.el:frame-set-background-mode (with face-set-after-frame-default)
    !> does for a new terminal frame.
    subroutine init_frame_faces(f, term_type)
        type(frame_type), intent(inout) :: f
        character(len=*), intent(in) :: term_type
        integer :: class
        display_supports_face_attributes_p => tty_supports_face_attributes_p
        clear_face_cache_hook => clear_face_cache
        ! display-color-p: tty-display-color-p.
        class = FACE_CLASS_MONO
        if (f%tty%tn_max_colors > 0) class = FACE_CLASS_COLOR
        call set_frame_display(class, frame_background_mode(term_type, "unspecified-bg"), &
                               f%tty%tn_max_colors, "tty")
        if (allocated(f%faces)) deallocate (f%faces)
        call face_spec_recalc_all()
        call realize_basic_faces(f)
    end subroutine init_frame_faces

    !> Clear the face cache of the selected frame and realize its basic
    !> faces again, after face attributes or color mappings changed.  Port
    !> of xfaces.c:Fclear_face_cache (with free_all_realized_faces).
    subroutine clear_face_cache()
        if (.not. allocated(selected_frame%faces)) return
        call recompute_basic_faces(selected_frame)
    end subroutine clear_face_cache

    !> Free the realized faces of F and realize its basic faces again.
    !> Port of xfaces.c:recompute_basic_faces.
    subroutine recompute_basic_faces(f)
        type(frame_type), intent(inout) :: f
        if (allocated(f%faces)) deallocate (f%faces)
        call realize_basic_faces(f)
    end subroutine recompute_basic_faces

    !> Realize basic faces on frame F: the default face, then the faces
    !> with fixed ids (mode_line_active, ...).  Port of
    !> xfaces.c:realize_basic_faces.
    subroutine realize_basic_faces(f)
        type(frame_type), intent(inout) :: f
        integer(int32) :: id
        if (allocated(f%faces)) deallocate (f%faces)
        allocate (f%faces(0:BASIC_FACE_ID_SENTINEL - 1))
        call realize_default_face(f)
        do id = MODE_LINE_ACTIVE_FACE_ID, MARGIN_FACE_ID
            call realize_named_face(f, trim(basic_face_names(id)), id)
        end do
    end subroutine realize_basic_faces

    !> Realize the default face on frame F.  Attributes of the default
    !> face that are not specified get their terminal values: family and
    !> foundry "default", width normal, height 1, weight and slant normal,
    !> no underline, overline, strike-through, box or inverse video,
    !> `extend' nil, and the colors "unspecified-fg" and "unspecified-bg".
    !> They are set on the `default' face itself.  Port of the terminal
    !> part of xfaces.c:realize_default_face.
    subroutine realize_default_face(f)
        type(frame_type), intent(inout) :: f
        associate (lface => default%lface)
            lface%family = "default"
            lface%foundry = lface%family
            lface%width = "normal"
            lface%height = 1
            if (.not. allocated(lface%weight)) lface%weight = "normal"
            if (.not. allocated(lface%slant)) lface%slant = "normal"
            if (lface%extend%state == FACE_UNSPECIFIED) lface%extend = .false.
            if (lface%underline%state == FACE_UNSPECIFIED) lface%underline = .false.
            if (lface%overline%state == FACE_UNSPECIFIED) lface%overline = .false.
            if (lface%strike_through%state == FACE_UNSPECIFIED) lface%strike_through = .false.
            if (lface%box%state == FACE_UNSPECIFIED) lface%box = .false.
            if (lface%inverse_video%state == FACE_UNSPECIFIED) lface%inverse_video = .false.
            ! No foreground-color or background-color frame parameter.
            if (.not. allocated(lface%foreground)) lface%foreground = "unspecified-fg"
            if (.not. allocated(lface%background)) lface%background = "unspecified-bg"
        end associate
        call realize_face(f, default%lface, DEFAULT_FACE_ID)
    end subroutine realize_default_face

    !> Realize the basic face NAME with face id ID on frame F: the
    !> default face's attributes with those of NAME merged in.  A face
    !> not defined is realized as the default face.  Port of
    !> xfaces.c:realize_named_face.
    subroutine realize_named_face(f, name, id)
        type(frame_type), intent(inout) :: f
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: id
        type(face_attributes_type) :: attrs
        type(named_face_type), pointer :: face
        attrs = f%faces(DEFAULT_FACE_ID)%lface
        face => face_named(name)
        ! Merge NAME's face with the default face.
        if (associated(face)) call merge_face_vectors(face%lface, attrs, 0)
        call realize_face(f, attrs, id)
    end subroutine realize_named_face

    !> Realize the fully specified face with attributes ATTRS on frame F
    !> with face id ID, or (ID negative) with the next free id, returned
    !> in NEW_ID.  Port of xfaces.c:realize_face (on a terminal; see
    !> below for window-system frames).
    subroutine realize_face(f, attrs, id, new_id)
        type(frame_type), intent(inout) :: f
        type(face_attributes_type), intent(in) :: attrs
        integer(int32), intent(in) :: id
        integer(int32), intent(out), optional :: new_id
        type(face_type), allocatable :: grown(:)
        integer(int32) :: k
        k = id
        if (k < 0) then
            k = int(ubound(f%faces, 1), int32) + 1
            allocate (grown(0:k))
            grown(0:k - 1) = f%faces
            call move_alloc(grown, f%faces)
        end if
        f%faces(k) = realize_tty_face(attrs)
        ! On a window-system frame, the font part of
        ! xfaces.c:realize_gui_face (font_load_for_lface); its other parts
        ! (colors, box, overline, stipple) are Deferred with the GUI, so
        ! the face is otherwise realized as on a terminal.
        if (frame_window_p(f)) then
            if (associated(f%font_driver)) f%faces(k)%font = f%font_driver%load_for_lface(attrs)
        end if
        if (present(new_id)) new_id = k
    end subroutine realize_face

    !> The terminal face for the fully specified attributes ATTRS.  Port
    !> of xfaces.c:realize_tty_face.
    function realize_tty_face(attrs) result(face)
        type(face_attributes_type), intent(in) :: attrs
        type(face_type) :: face
        integer(int64) :: tem
        face%lface = attrs
        ! Map face attributes to TTY appearances.
        if (face_weight_numeric(attrs%weight) > 100) face%tty_bold_p = .true.
        if (face_slant_numeric(attrs%slant) /= 100) face%tty_italic_p = .true.
        if (attrs%inverse_video%state /= FACE_NIL) face%tty_reverse_p = .true.
        if (attrs%strike_through%state /= FACE_NIL) face%tty_strike_through_p = .true.
        ! Text underline.
        select case (attrs%underline%state)
        case (FACE_NIL)
            face%underline = FACE_NO_UNDERLINE
            face%underline_color = 0
        case (FACE_T)
            face%underline = FACE_UNDERLINE_SINGLE
            face%underline_color = 0
        case (FACE_COLOR)
            face%underline = FACE_UNDERLINE_SINGLE
            face%underline_color = map_tty_color(attrs%underline%color, .true.)
        case (FACE_PLIST)
            ! (:color COLOR :style STYLE).
            face%underline_color = 0
            if (allocated(attrs%underline%color)) then
                ! An empty color is foreground-color.
                if (len(attrs%underline%color) > 0) then
                    face%underline_color = map_tty_color(attrs%underline%color, .true.)
                end if
            end if
            select case (attrs%underline%style)
            case (FACE_STYLE_DOUBLE_LINE)
                face%underline = FACE_UNDERLINE_DOUBLE_LINE
            case (FACE_STYLE_WAVE)
                face%underline = FACE_UNDERLINE_WAVE
            case (FACE_STYLE_DOTS)
                face%underline = FACE_UNDERLINE_DOTS
            case (FACE_STYLE_DASHES)
                face%underline = FACE_UNDERLINE_DASHES
            case default
                face%underline = FACE_UNDERLINE_SINGLE
            end select
        case default
            continue
        end select
        ! Map color names to color indices.
        face%foreground = map_tty_color(attrs%foreground, .true.)
        face%background = map_tty_color(attrs%background, .false.)
        ! Swap colors if face is inverse-video.
        if (face%tty_reverse_p) then
            tem = face%foreground
            face%foreground = face%background
            face%background = tem
        end if
        if (tty_suppress_bold_inverse_default_colors_p .and. face%tty_bold_p .and. &
            face%background == FACE_TTY_DEFAULT_FG_COLOR .and. face%foreground == FACE_TTY_DEFAULT_BG_COLOR) then
            face%tty_bold_p = .false.
        end if
    end function realize_tty_face

    !> The terminal color index of COLOR, a foreground (or underline)
    !> color if FOREGROUND, else a background color: the index of COLOR in
    !> `tty_defined_color_alist', else of the color approximating it
    !> (load_color with tty_defined_color), else the default foreground or
    !> background.  Port of xfaces.c:map_tty_color.
    integer(int64) function map_tty_color(color, foreground) result(pixel)
        character(len=:), allocatable, intent(in) :: color
        logical, intent(in) :: foreground
        integer(int64) :: default_pixel
        integer :: index, r, g, b
        logical :: found
        type(emacs_color_type) :: def
        default_pixel = FACE_TTY_DEFAULT_BG_COLOR
        if (foreground) default_pixel = FACE_TTY_DEFAULT_FG_COLOR
        pixel = default_pixel
        if (.not. allocated(color)) return
        if (len(color) > 0 .and. tty_defined_color_count > 0) then
            ! Associations in tty-defined-color-alist are of the form
            ! (NAME INDEX R G B).  We need the INDEX part.
            call tty_color_assoc(color, found, index, r, g, b)
            if (found) pixel = index
        end if
        if (pixel == default_pixel) then
            ! load_color: a color that can't be loaded gives the frame's
            ! foreground or background.
            if (tty_defined_color(color, def)) pixel = def%pixel
        end if
    end function map_tty_color

    !> Look up COLOR on the terminal: DEF gets its index (the default
    !> colors for "unspecified-fg" and "unspecified-bg") and RGB values.
    !> Port of xfaces.c:tty_defined_color.
    logical function tty_defined_color(color, def) result(status)
        character(len=*), intent(in) :: color
        type(emacs_color_type), intent(out) :: def
        status = .true.
        if (len(color) > 0) then
            status = tty_lookup_color(color, def)
            if (def%pixel == FACE_TTY_DEFAULT_COLOR) then
                if (color == "unspecified-fg") then
                    def%pixel = FACE_TTY_DEFAULT_FG_COLOR
                else if (color == "unspecified-bg") then
                    def%pixel = FACE_TTY_DEFAULT_BG_COLOR
                end if
            end if
        end if
        if (def%pixel /= FACE_TTY_DEFAULT_COLOR) status = .true.
    end function tty_defined_color

    !> Look up COLOR with tty-color-desc: TTY_COLOR gets the terminal
    !> color for it, STD_COLOR (if present) its standard RGB values.
    !> Value is false if COLOR is not a valid color.  Port of
    !> xfaces.c:tty_lookup_color.
    logical function tty_lookup_color(color, tty_color, std_color) result(ok)
        character(len=*), intent(in) :: color
        type(emacs_color_type), intent(inout) :: tty_color
        type(emacs_color_type), intent(out), optional :: std_color
        character(len=:), allocatable :: name
        integer :: index, r, g, b
        logical :: found, has_rgb
        ok = .false.
        call tty_color_desc(color, found, index, r, g, b, name, has_rgb)
        if (found) then
            tty_color%pixel = index
            if (.not. has_rgb) return
            tty_color%red = r
            tty_color%green = g
            tty_color%blue = b
            if (present(std_color)) then
                ! Default STD_COLOR to the same as TTY_COLOR.
                std_color = tty_color
                ! If the descriptor isn't exactly COLOR, look up STD_COLOR
                ! separately.
                if (.not. (name == color .and. len(name) == len(color))) then
                    call tty_color_standard_values(color, found, r, g, b)
                    if (.not. found) return
                    std_color%red = r
                    std_color%green = g
                    std_color%blue = b
                end if
            end if
            ok = .true.
        else if (tty_defined_color_count == 0) then
            ! Called before the colors are set up: not a failure.
            ok = .true.
        end if
    end function tty_lookup_color

    !> The distance between colors X and Y ("Colour metric" by Thiadmer
    !> Riemersma).  Port of xfaces.c:color_distance.
    pure integer(int64) function color_distance(x, y) result(d)
        type(emacs_color_type), intent(in) :: x, y
        integer(int64) :: r, g, b, r_mean
        r = x%red - y%red
        g = x%green - y%green
        b = x%blue - y%blue
        r_mean = shifta(x%red + y%red, 1)
        d = shifta(shifta((2*65536_int64 + r_mean)*r*r, 16) + 4*g*g + &
                   shifta((2*65536_int64 + 65535 - r_mean)*b*b, 16), 16)
    end function color_distance

    ! ------------------------------------------------------------------
    ! Merging face attributes
    ! ------------------------------------------------------------------

    !> Merge the attributes of FROM into TO: first those of the faces FROM
    !> inherits from, then FROM's own, a relative height multiplying TO's.
    !> TO is an absolute face, which inherits from nothing.  Port of
    !> xfaces.c:merge_face_vectors.
    recursive subroutine merge_face_vectors(from, to, depth)
        type(face_attributes_type), intent(in) :: from
        type(face_attributes_type), intent(inout) :: to
        integer, intent(in) :: depth
        integer :: k
        logical :: ok
        ! If FROM inherits from some other faces, merge their attributes
        ! into TO before merging FROM's direct attributes.
        if (from%inherit%state == FACE_LIST) then
            ! The first name takes precedence: merge from the end.
            do k = size(from%inherit%names), 1, -1
                call merge_named_face(trim(from%inherit%names(k)), to, depth + 1, .false., ok)
            end do
        end if
        if (allocated(from%family)) to%family = from%family
        if (allocated(from%foundry)) to%foundry = from%foundry
        if (allocated(from%width)) to%width = from%width
        if (from%height%state == FACE_VALUE) then
            to%height = from%height
        else if (from%height%state == FACE_PLIST) then
            call merge_face_heights(from, to)
        end if
        if (allocated(from%weight)) to%weight = from%weight
        if (allocated(from%slant)) to%slant = from%slant
        if (from%underline%state /= FACE_UNSPECIFIED) to%underline = from%underline
        if (from%overline%state /= FACE_UNSPECIFIED) to%overline = from%overline
        if (from%strike_through%state /= FACE_UNSPECIFIED) to%strike_through = from%strike_through
        if (from%box%state /= FACE_UNSPECIFIED) to%box = from%box
        if (from%inverse_video%state /= FACE_UNSPECIFIED) to%inverse_video = from%inverse_video
        if (allocated(from%foreground)) to%foreground = from%foreground
        if (allocated(from%distant_foreground)) to%distant_foreground = from%distant_foreground
        if (allocated(from%background)) to%background = from%background
        if (from%extend%state /= FACE_UNSPECIFIED) to%extend = from%extend
        to%inherit = ""
    end subroutine merge_face_vectors

    !> A relative height FROM applied to TO's height: relative times
    !> absolute is absolute, relative times relative is relative.  Port
    !> of xfaces.c:merge_face_heights (height functions are not ported).
    subroutine merge_face_heights(from, to)
        type(face_attributes_type), intent(in) :: from
        type(face_attributes_type), intent(inout) :: to
        if (to%height%state == FACE_VALUE) then
            to%height = int(from%height%factor*real(to%height%absolute))
        else if (to%height%state == FACE_PLIST) then
            to%height = from%height%factor*to%height%factor
        else
            to%height = from%height%factor
        end if
    end subroutine merge_face_heights

    !> Merge the attributes of the face named NAME into TO.  With
    !> EXTEND_ONLY, only if the face (or a face it inherits from) gives
    !> `extend' a true value.  OK is false if there is no such face.  Port
    !> of xfaces.c:merge_named_face.
    recursive subroutine merge_named_face(name, to, depth, extend_only, ok)
        character(len=*), intent(in) :: name
        type(face_attributes_type), intent(inout) :: to
        integer, intent(in) :: depth
        logical, intent(in) :: extend_only
        logical, intent(out) :: ok
        type(named_face_type), pointer :: face
        ok = .false.
        ! Too deep: a face inheriting from itself.
        if (depth > MAX_MERGE_DEPTH) return
        face => face_named(name)
        if (.not. associated(face)) return
        ok = .true.
        if (extend_only) then
            ! Filter, but specified; or unspecified, but inherited.
            if (face%lface%extend%state /= FACE_T) then
                if (face%lface%inherit%state /= FACE_LIST) return
                if (face_inherited_extend(face%lface, depth + 1) /= FACE_T) return
            end if
        end if
        call merge_face_vectors(face%lface, to, depth)
    end subroutine merge_named_face

    !> The `extend' attribute of ATTRS, or if that is unspecified, of the
    !> faces ATTRS inherits from.  Port of xfaces.c:face_inherited_attr
    !> (for LFACE_EXTEND_INDEX).
    recursive integer function face_inherited_extend(attrs, depth) result(state)
        type(face_attributes_type), intent(in) :: attrs
        integer, intent(in) :: depth
        type(face_attributes_type) :: inherited
        type(named_face_type), pointer :: parent
        character(len=64), allocatable :: names(:)
        integer :: k, guard
        logical :: ok
        inherited = attrs
        state = inherited%extend%state
        guard = depth
        do while (state == FACE_UNSPECIFIED .and. inherited%inherit%state == FACE_LIST)
            guard = guard + 1
            if (guard > MAX_MERGE_DEPTH) exit
            names = inherited%inherit%names
            if (size(names) > 1) then
                ok = .false.
                do k = 1, size(names)
                    parent => face_named(trim(names(k)))
                    ok = associated(parent)
                    if (.not. ok) exit
                    inherited = parent%lface
                    state = face_inherited_extend(inherited, guard)
                    if (state /= FACE_UNSPECIFIED) exit
                end do
                ! A bad face?
                if (.not. ok) exit
            else
                parent => face_named(trim(names(1)))
                if (.not. associated(parent)) exit
                inherited = parent%lface
                state = inherited%extend%state
            end if
        end do
    end function face_inherited_extend

    !> Merge face attributes from the face reference FACE_REF into TO.
    !> FACE_REF is a face name, a property list of attributes, or a list
    !> of face references, those at the beginning taking precedence.  OK
    !> is false if FACE_REF is invalid (what is valid of it is still
    !> merged).  With EXTEND_ONLY, a face is merged only if it gives
    !> `extend' a true value (ATTR_FILTER LFACE_EXTEND_INDEX).  Port of
    !> xfaces.c:merge_face_ref.
    recursive subroutine merge_face_ref(face_ref, to, ok, extend_only, depth)
        type(value_type), intent(in) :: face_ref
        type(face_attributes_type), intent(inout) :: to
        logical, intent(out) :: ok
        logical, intent(in), optional :: extend_only
        integer, intent(in), optional :: depth
        type(value_type), allocatable :: elts(:)
        logical :: filter, sub_ok
        integer :: d, k
        ok = .true.
        filter = .false.
        if (present(extend_only)) filter = extend_only
        d = 0
        if (present(depth)) d = depth
        if (d > MAX_MERGE_DEPTH) then
            ok = .false.
            return
        end if
        if (nilp(face_ref)) return
        if (consp(face_ref)) then
            call list_elements(face_ref, elts)
            if (keywordp(elts(1))) then
                ! Assume this is the property list form.
                call merge_plist(elts, to, ok, filter, d)
                return
            end if
            ! This is a list of face refs.  Those at the beginning of the
            ! list take precedence over what follows, so we have to merge
            ! from the end backwards.
            do k = size(elts), 1, -1
                call merge_face_ref(elts(k), to, sub_ok, filter, d)
                if (.not. sub_ok) ok = .false.
            end do
            return
        end if
        if (symbolp(face_ref)) then
            ! FACE_REF ought to be a face name.
            call merge_named_face(symbols(xsymbol(face_ref))%name, to, d + 1, filter, ok)
            return
        end if
        ok = .false.
    end subroutine merge_face_ref

    !> Whether V is a keyword (a symbol whose name starts with `:').
    logical function keywordp(v)
        type(value_type), intent(in) :: v
        keywordp = .false.
        if (.not. symbolp(v)) return
        associate (name => symbols(xsymbol(v))%name)
            if (len(name) > 0) keywordp = name(1:1) == ":"
        end associate
    end function keywordp

    !> Whether V is the symbol NAME.
    logical function is_symbol(v, name)
        type(value_type), intent(in) :: v
        character(len=*), intent(in) :: name
        is_symbol = .false.
        if (symbolp(v)) is_symbol = symbols(xsymbol(v))%name == name
    end function is_symbol

    !> The name of symbol V, or "" if V is not a symbol.
    function symbol_name(v) result(name)
        type(value_type), intent(in) :: v
        character(len=:), allocatable :: name
        name = ""
        if (symbolp(v)) name = symbols(xsymbol(v))%name
    end function symbol_name

    !> Merge the property list ELTS (keyword, value, ...) of a face
    !> reference into TO, in order.  With FILTER, nothing is merged unless
    !> `:extend' is given a true value, or is not mentioned and a face
    !> `:inherit' names gives it one.  The property list part of
    !> xfaces.c:merge_face_ref.
    recursive subroutine merge_plist(elts, to, ok, filter, depth)
        type(value_type), intent(in) :: elts(:)
        type(face_attributes_type), intent(inout) :: to
        logical, intent(out) :: ok
        logical, intent(in) :: filter
        integer, intent(in) :: depth
        type(face_attributes_type) :: scratch
        type(value_type) :: parent_face
        logical :: seen, err, sub_ok
        integer :: k
        character(len=:), allocatable :: keyword
        ok = .true.
        if (filter) then
            ! Don't merge this face if `extend' is nil, or not mentioned,
            ! or if it's unspecified and the face doesn't inherit from a
            ! face whose `extend' is non-nil.
            seen = .false.
            k = 1
            do while (k + 1 <= size(elts))
                keyword = symbol_name(elts(k))
                if (keyword == ":extend") then
                    seen = .true.
                    if (nilp(elts(k + 1))) return
                else if (keyword == ":inherit") then
                    parent_face = elts(k + 1)
                end if
                k = k + 2
            end do
            if (.not. seen) then
                if (nilp(parent_face)) return
                call merge_face_ref(parent_face, scratch, sub_ok, .false., depth + 1)
                if (.not. sub_ok) then
                    ok = .false.
                    return
                end if
                if (scratch%extend%state /= FACE_T) return
            end if
        end if
        ! (Once the filter is passed, `:inherit' merges without it.)
        k = 1
        do while (k + 1 <= size(elts))
            keyword = symbol_name(elts(k))
            call merge_attribute(keyword, elts(k + 1), to, err, depth)
            if (err) ok = .false.
            k = k + 2
        end do
    end subroutine merge_plist

    !> Merge the attribute KEYWORD with value VALUE of a face property
    !> list into TO; ERR if it is invalid.  `unspecified' is a no-op.
    recursive subroutine merge_attribute(keyword, value, to, err, depth)
        character(len=*), intent(in) :: keyword
        type(value_type), intent(in) :: value
        type(face_attributes_type), intent(inout) :: to
        logical, intent(out) :: err
        integer, intent(in) :: depth
        logical :: sub_ok
        err = .false.
        ! Specifying `unspecified' is a no-op.
        if (is_symbol(value, "unspecified")) return
        select case (keyword)
        case (":family")
            if (stringp(value)) then
                to%family = xstring(value)
            else
                err = .true.
            end if
        case (":foundry")
            if (stringp(value)) then
                to%foundry = xstring(value)
            else
                err = .true.
            end if
        case (":width")
            err = .true.
            if (face_width_numeric(symbol_name(value)) >= 0) then
                to%width = symbol_name(value)
                err = .false.
            end if
        case (":height")
            ! An absolute height (a relative one is a float).
            if (fixnump(value)) then
                to%height = int(xfixnum(value))
            else
                err = .true.
            end if
        case (":weight")
            err = .true.
            if (face_weight_numeric(symbol_name(value)) >= 0) then
                to%weight = symbol_name(value)
                err = .false.
            end if
        case (":slant")
            err = .true.
            if (face_slant_numeric(symbol_name(value)) >= 0) then
                to%slant = symbol_name(value)
                err = .false.
            end if
        case (":underline")
            if (nilp(value)) then
                to%underline = .false.
            else if (is_symbol(value, "t")) then
                to%underline = .true.
            else if (stringp(value)) then
                to%underline = xstring(value)
            else if (consp(value)) then
                call underline_from_plist(value, to)
            else
                err = .true.
            end if
        case (":overline")
            if (nilp(value)) then
                to%overline = .false.
            else if (is_symbol(value, "t")) then
                to%overline = .true.
            else if (stringp(value)) then
                to%overline = xstring(value)
            else
                err = .true.
            end if
        case (":strike_through")
            if (nilp(value)) then
                to%strike_through = .false.
            else if (is_symbol(value, "t")) then
                to%strike_through = .true.
            else if (stringp(value)) then
                to%strike_through = xstring(value)
            else
                err = .true.
            end if
        case (":box")
            if (nilp(value)) then
                to%box = .false.
            else if (is_symbol(value, "t")) then
                to%box = .true.
            else if (fixnump(value)) then
                if (xfixnum(value) /= 0) then
                    to%box = .true.
                    to%box%line_width = int(xfixnum(value))
                else
                    err = .true.
                end if
            else if (stringp(value)) then
                to%box = .true.
                to%box%state = FACE_COLOR
                to%box%color = xstring(value)
            else if (consp(value)) then
                ! A box property list: drawn on window systems only.
                to%box = .true.
                to%box%state = FACE_PLIST
            else
                err = .true.
            end if
        case (":inverse_video")
            if (nilp(value)) then
                to%inverse_video = .false.
            else if (is_symbol(value, "t")) then
                to%inverse_video = .true.
            else
                err = .true.
            end if
        case (":foreground")
            if (stringp(value)) then
                to%foreground = xstring(value)
            else
                err = .true.
            end if
        case (":distant_foreground")
            if (stringp(value)) then
                to%distant_foreground = xstring(value)
            else
                err = .true.
            end if
        case (":background")
            if (stringp(value)) then
                to%background = xstring(value)
            else
                err = .true.
            end if
        case (":stipple")
            ! Stipples need a window system.
            continue
        case (":inherit")
            ! Just like a normal face reference.
            call merge_face_ref(value, to, sub_ok, .false., depth + 1)
            err = .not. sub_ok
        case (":extend")
            if (nilp(value)) then
                to%extend = .false.
            else if (is_symbol(value, "t")) then
                to%extend = .true.
            else
                err = .true.
            end if
        case default
            err = .true.
        end select
    end subroutine merge_attribute

    !> An underline given as a property list (:color COLOR :style STYLE
    !> :position POS).
    subroutine underline_from_plist(value, to)
        type(value_type), intent(in) :: value
        type(face_attributes_type), intent(inout) :: to
        type(value_type), allocatable :: elts(:)
        integer :: k
        call list_elements(value, elts)
        to%underline = .true.
        to%underline%state = FACE_PLIST
        k = 1
        do while (k + 1 <= size(elts))
            select case (symbol_name(elts(k)))
            case (":color")
                if (stringp(elts(k + 1))) then
                    to%underline%color = xstring(elts(k + 1))
                else if (is_symbol(elts(k + 1), "foreground_color")) then
                    to%underline%color = ""
                end if
            case (":style")
                select case (symbol_name(elts(k + 1)))
                case ("line")
                    to%underline%style = FACE_STYLE_LINE
                case ("double_line")
                    to%underline%style = FACE_STYLE_DOUBLE_LINE
                case ("wave")
                    to%underline%style = FACE_STYLE_WAVE
                case ("dots")
                    to%underline%style = FACE_STYLE_DOTS
                case ("dashes")
                    to%underline%style = FACE_STYLE_DASHES
                case default
                    to%underline%style = 0
                end select
            case (":position")
                to%underline%position = .not. nilp(elts(k + 1))
            case default
                continue
            end select
            k = k + 2
        end do
    end subroutine underline_from_plist

    !> The numeric value of width NAME ("normal": 100), -1 if unknown.
    !> Port of font.c's width_table (FONT_WIDTH_NAME_NUMERIC).
    integer function face_width_numeric(name) result(n)
        character(len=*), intent(in) :: name
        select case (name)
        case ("ultra-condensed", "ultracondensed")
            n = 50
        case ("extra-condensed", "extracondensed")
            n = 63
        case ("condensed", "compressed", "narrow")
            n = 75
        case ("semi-condensed", "semicondensed", "demicondensed")
            n = 87
        case ("normal", "medium", "regular", "unspecified")
            n = 100
        case ("semi-expanded", "semiexpanded", "demiexpanded")
            n = 113
        case ("expanded")
            n = 125
        case ("extra-expanded", "extraexpanded")
            n = 150
        case ("ultra-expanded", "ultraexpanded", "wide")
            n = 200
        case default
            n = -1
        end select
    end function face_width_numeric

    ! ------------------------------------------------------------------
    ! Looking up faces
    ! ------------------------------------------------------------------

    !> The id of the realized face on frame F with attributes ATTRS,
    !> realizing it if there is none.  Port of xfaces.c:lookup_face.
    integer(int32) function lookup_face(f, attrs) result(id)
        type(frame_type), intent(inout) :: f
        type(face_attributes_type), intent(in) :: attrs
        integer(int32) :: k
        if (.not. allocated(f%faces)) call realize_basic_faces(f)
        do k = 0, int(ubound(f%faces, 1), int32)
            if (faces_equal(f%faces(k)%lface, attrs)) then
                id = k
                return
            end if
        end do
        ! If not found, realize a new face.
        call realize_face(f, attrs, -1_int32, id)
    end function lookup_face

    !> The id of the face named NAME merged with the default face on
    !> frame F, or -1 if there is no such face.  Port of
    !> xfaces.c:lookup_named_face.
    integer(int32) function lookup_named_face(f, name) result(id)
        type(frame_type), intent(inout) :: f
        character(len=*), intent(in) :: name
        type(face_attributes_type) :: attrs
        type(named_face_type), pointer :: face
        id = -1
        if (.not. allocated(f%faces)) call realize_basic_faces(f)
        face => face_named(name)
        if (.not. associated(face)) return
        attrs = f%faces(DEFAULT_FACE_ID)%lface
        call merge_face_vectors(face%lface, attrs, 0)
        id = lookup_face(f, attrs)
    end function lookup_named_face

    !> The face with the attributes of the realized face BASE_FACE_ID on
    !> frame F and those of the face named FACE_NAME merged in, or, with
    !> FACE_NAME empty, those of the realized face FACE_ID.  Value is
    !> BASE_FACE_ID if there is no such face.  Port of
    !> xfaces.c:merge_faces.
    integer(int32) function merge_faces(f, face_name, face_id, base_face_id) result(id)
        type(frame_type), intent(inout) :: f
        character(len=*), intent(in) :: face_name
        integer(int32), intent(in) :: face_id, base_face_id
        type(face_attributes_type) :: attrs
        logical :: ok
        id = base_face_id
        if (.not. allocated(f%faces)) return
        if (base_face_id < 0 .or. base_face_id > ubound(f%faces, 1)) return
        ! Begin with attributes from the base face.
        attrs = f%faces(base_face_id)%lface
        if (len(face_name) > 0) then
            call merge_named_face(face_name, attrs, 0, .false., ok)
            if (.not. ok) return
        else
            if (face_id < 0 .or. face_id > ubound(f%faces, 1)) return
            call merge_face_vectors(f%faces(face_id)%lface, attrs, 0)
        end if
        id = lookup_face(f, attrs)
    end function merge_faces

    !> The id of the face for the text at POS in buffer slot BI on frame
    !> F, and in ENDPOS the next position (at most LIMIT and the end of the
    !> accessible text) where a different face may be needed: the default
    !> face with the `face' text property at POS merged in, then the
    !> `face' properties of the overlays at POS in order of increasing
    !> priority, among them the `region' face if POS is in
    !> [REGION_START, REGION_END) (the region overlay redisplay makes for
    !> an active region, of priority (nil . 100)).  With EXTEND_ONLY, only
    !> faces that set `extend' are merged (for filling the rest of a
    !> line).  Port of xfaces.c:face_at_buffer_position (overlays' `window'
    !> properties are not ported; no mouse faces).
    integer(int32) function face_at_buffer_position(f, bi, pos, endpos, limit, region_start, region_end, &
                                                    extend_only) result(id)
        type(frame_type), intent(inout) :: f
        integer(int64), intent(in) :: bi, pos, limit
        integer(int64), intent(out) :: endpos
        integer(int64), intent(in), optional :: region_start, region_end
        logical, intent(in), optional :: extend_only
        type(face_attributes_type) :: attrs
        type(value_type) :: prop
        type(status_type) :: status
        type(handle_type), allocatable :: overlay_vec(:)
        integer(int64) :: saved, end, next_overlay, oendpos, rbeg, rend, p, s
        integer :: i
        logical :: found, filter, in_region, ok, region_merged
        filter = .false.
        if (present(extend_only)) filter = extend_only
        if (.not. allocated(f%faces)) call realize_basic_faces(f)
        endpos = buffers(bi)%zv
        ! Get the `face' text property at POS, and determine the next
        ! position at which the property changes.
        saved = cur
        call set_buffer_internal(bi)
        call get_text_property(pos, sym("face"), prop, status)
        call next_single_property_change(pos, sym("face"), end, found, status, min(limit, endpos))
        if (found) endpos = end
        ! Look at properties from overlays.
        call overlays_at(pos, overlay_vec, next=next_overlay)
        ! overlays_at can return next_overlay beyond the end of the current
        ! narrowing.  We don't want that to leak into the display code.
        if (next_overlay > buffers(bi)%zv) next_overlay = buffers(bi)%zv
        if (next_overlay < endpos) endpos = next_overlay
        ! The region overlay.
        in_region = .false.
        rbeg = 0
        rend = 0
        if (present(region_start) .and. present(region_end)) then
            if (region_start < region_end) then
                if (pos < region_start) then
                    endpos = min(endpos, region_start)
                else if (pos < region_end) then
                    in_region = .true.
                    rbeg = region_start
                    rend = region_end
                end if
            end if
        end if
        id = DEFAULT_FACE_ID
        ! Optimize common cases where we can use the default face.
        if (.not. in_region .and. size(overlay_vec) == 0 .and. nilp(prop)) then
            if (saved /= 0) call set_buffer_internal(saved)
            return
        end if
        ! Begin with attributes from the default face.
        attrs = f%faces(DEFAULT_FACE_ID)%lface
        ! Merge in attributes specified via text properties.
        if (.not. nilp(prop)) call merge_face_ref(prop, attrs, ok, filter)
        ! Now merge the overlay data.
        call sort_overlays(overlay_vec)
        region_merged = .not. in_region
        do i = 1, size(overlay_vec)
            if (.not. region_merged) then
                call overlay_priority(overlay_vec(i), p, s)
                if (compare_overlays(0_int64, 100_int64, rbeg, rend, 0_int64, p, s, overlay_start(overlay_vec(i)), &
                                     overlay_end(overlay_vec(i)), overlay_vec(i)%index) < 0) call merge_region()
            end if
            prop = overlay_get(overlay_vec(i), "face")
            if (.not. nilp(prop)) call merge_face_ref(prop, attrs, ok, filter)
            oendpos = overlay_end(overlay_vec(i))
            if (oendpos < endpos) endpos = oendpos
        end do
        if (.not. region_merged) call merge_region()
        if (saved /= 0) call set_buffer_internal(saved)
        ! Look up a realized face with the given face attributes, or
        ! realize a new one.
        id = lookup_face(f, attrs)
    contains
        !> Merge the region overlay's face.
        subroutine merge_region()
            call merge_named_face("region", attrs, 1, filter, ok)
            endpos = min(endpos, rend)
            region_merged = .true.
        end subroutine merge_region
    end function face_at_buffer_position

    ! ------------------------------------------------------------------
    ! Display support for face attributes
    ! ------------------------------------------------------------------

    !> Whether the selected frame's terminal can display a face with the
    !> attributes ATTRS so that it differs from the default face and is
    !> close in spirit to what the attributes specify.  Port of
    !> xfaces.c:tty_supports_face_attributes_p (with the terminal part of
    !> Fdisplay_supports_face_attributes_p).
    logical function tty_supports_face_attributes_p(attrs) result(supports)
        type(face_attributes_type), intent(in) :: attrs
        type(face_attributes_type) :: def_attrs
        type(emacs_color_type) :: fg_tty_color, fg_std_color, bg_tty_color, bg_std_color, def_color
        integer :: weight, def_weight, slant, def_slant, test_caps
        integer(int64) :: delta_delta
        supports = .false.
        if (.not. allocated(selected_frame%faces)) call realize_basic_faces(selected_frame)
        def_attrs = selected_frame%faces(DEFAULT_FACE_ID)%lface
        test_caps = 0
        ! Terminals support none of these attributes.  We consider :slant
        ! unsupportable on terminals, even though the face code actually
        ! `fakes' them using a dim attribute if possible.
        if (allocated(attrs%family) .or. allocated(attrs%foundry) .or. &
            attrs%height%state /= FACE_UNSPECIFIED .or. allocated(attrs%width) .or. &
            attrs%overline%state /= FACE_UNSPECIFIED .or. attrs%box%state /= FACE_UNSPECIFIED) return
        ! Font weight (bold/dim).
        if (allocated(attrs%weight)) then
            weight = face_weight_numeric(attrs%weight)
            if (weight >= 0) then
                def_weight = face_weight_numeric(def_attrs%weight)
                if (weight > 100) then
                    ! Same as default?
                    if (def_weight > 100) return
                    test_caps = TTY_CAP_BOLD
                else if (weight < 100) then
                    if (def_weight < 100) return
                    test_caps = TTY_CAP_DIM
                else if (def_weight == 100) then
                    return
                end if
            end if
        end if
        ! Font slant.
        if (allocated(attrs%slant)) then
            slant = face_slant_numeric(attrs%slant)
            if (slant >= 0) then
                def_slant = face_slant_numeric(def_attrs%slant)
                if (slant == 100 .or. slant == def_slant) return
                test_caps = ior(test_caps, TTY_CAP_ITALIC)
            end if
        end if
        ! Underlining.
        if (attrs%underline%state /= FACE_UNSPECIFIED) then
            if (attrs%underline%state == FACE_COLOR) then
                test_caps = ior(test_caps, TTY_CAP_UNDERLINE_STYLED)
            else if (attrs%underline%state == FACE_PLIST) then
                ! (:style STYLE ...): an unknown style is unsupported.
                if (attrs%underline%style < FACE_STYLE_LINE .or. attrs%underline%style > FACE_STYLE_DASHES) then
                    return
                end if
                test_caps = ior(test_caps, TTY_CAP_UNDERLINE_STYLED)
            else if (attrs%underline%state == def_attrs%underline%state) then
                return
            else
                test_caps = ior(test_caps, TTY_CAP_UNDERLINE)
            end if
        end if
        ! Inverse video.
        if (attrs%inverse_video%state /= FACE_UNSPECIFIED) then
            if (attrs%inverse_video%state == def_attrs%inverse_video%state) return
            test_caps = ior(test_caps, TTY_CAP_INVERSE)
        end if
        ! Strike through.
        if (attrs%strike_through%state /= FACE_UNSPECIFIED) then
            if (attrs%strike_through%state == def_attrs%strike_through%state) return
            test_caps = ior(test_caps, TTY_CAP_STRIKE_THROUGH)
        end if
        ! Check if foreground color is close enough.
        if (allocated(attrs%foreground)) then
            if (same_color_name(attrs%foreground, def_attrs%foreground)) return
            if (.not. tty_lookup_color(attrs%foreground, fg_tty_color, fg_std_color)) return
            ! Displayed color is too different?
            if (color_distance(fg_tty_color, fg_std_color) > TTY_SAME_COLOR_THRESHOLD) return
            ! Make sure the color is really different from the default.
            if (allocated(def_attrs%foreground)) then
                if (tty_lookup_color(def_attrs%foreground, def_color)) then
                    if (color_distance(fg_tty_color, def_color) <= TTY_SAME_COLOR_THRESHOLD) return
                end if
            end if
        end if
        ! Check if background color is close enough.
        if (allocated(attrs%background)) then
            if (same_color_name(attrs%background, def_attrs%background)) return
            if (.not. tty_lookup_color(attrs%background, bg_tty_color, bg_std_color)) return
            if (color_distance(bg_tty_color, bg_std_color) > TTY_SAME_COLOR_THRESHOLD) return
            if (allocated(def_attrs%background)) then
                def_color = emacs_color_type()
                if (tty_lookup_color(def_attrs%background, def_color)) then
                    if (color_distance(bg_tty_color, def_color) <= TTY_SAME_COLOR_THRESHOLD) return
                end if
            end if
        end if
        ! If both foreground and background are requested, see if the
        ! distance between them is OK.
        if (allocated(attrs%foreground) .and. allocated(attrs%background)) then
            delta_delta = color_distance(fg_std_color, bg_std_color) - color_distance(fg_tty_color, bg_tty_color)
            if (delta_delta > TTY_SAME_COLOR_THRESHOLD .or. delta_delta < -TTY_SAME_COLOR_THRESHOLD) return
        end if
        ! See if the capabilities we selected above are supported, with
        ! the given colors.
        supports = tty_capable_p(selected_frame%tty, test_caps)
    contains
        !> face_attr_equal_p for two color names.
        logical function same_color_name(a, b)
            character(len=*), intent(in) :: a
            character(len=:), allocatable, intent(in) :: b
            same_color_name = .false.
            if (allocated(b)) same_color_name = a == b .and. len(a) == len(b)
        end function same_color_name
    end function tty_supports_face_attributes_p
end module xfaces_fm
