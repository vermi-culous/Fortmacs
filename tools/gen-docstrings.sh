#!/bin/sh
# gen-docstrings.sh -- generate src/docstrings_fm.f90, the documentation
# that the help commands (C-h f, C-h k, C-h v) show.
#
# Copyright (C) 2026 The Fortmacs authors
#
# This file is part of Fortmacs.
#
# Fortmacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Fortmacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

# Upstream collects docstrings into etc/DOC at build time
# (lib-src/make-docfile.c) and Lisp keeps the rest with each definition.
# Here a docstring is the !> comment on a procedure or variable, up to
# its "Port of" citation (the rest of the comment is notes on the port).
# This script collects:
#
#   - commands: every `commands%NAME%run => PROC` in src/.  The
#     docstring is PROC's, or else that of the procedure whose comment
#     cites NAME ("Port of simple.el:NAME" or "Port of cmds.c:FNAME");
#   - functions: the procedures src/fortmacs.f90 exports (the config
#     API), named by their citation or else by their Fortran name with
#     hyphens;
#   - variables: the variables src/fortmacs.f90 exports, named by their
#     citation ("Port of files.el make-backup-files") or likewise, and
#     every other variable with such a citation.
#
# Usage: tools/gen-docstrings.sh [OUTPUT]
#   The default output is src/docstrings_fm.f90.  It is formatted with
#   fprettify, as tools/check.sh requires.

set -eu

cd "$(dirname "$0")/.."
out=${1:-src/docstrings_fm.f90}
tmp=${TMPDIR:-/tmp}/gen-docstrings.$$
trap 'rm -f "$tmp".*' EXIT

# Pass 1: one record per entry, fields separated by \037 and docstring
# lines by \036:
#   F  lisp-name  file  fortran-call  interactive(0/1)  doc
#   V  lisp-name  file  fortran-name  type  array(0/1)  init  enum  doc
find src -name '*.f90' ! -name docstrings_fm.f90 | sort | xargs awk '
function trim(s) { sub(/^[ \t]+/, "", s); sub(/[ \t]+$/, "", s); return s }
function help_name(s) { return s }
# A declaration line with deferred lengths (len=:) written as len=*, so
# the "::" that separates the names is the first colon.
function declline(s) { gsub(/len=:/, "len=*", s); return s }

# The docstring of comment block b (lines joined by \036): the text
# before "Port of".
function docstring(b,    n, i, lines, out, k, line) {
    n = split(b, lines, "\036")
    out = ""
    for (i = 1; i <= n; i++) {
        line = lines[i]
        k = index(line, "Port of")
        # "Port" at the end of a line, "of" starting the next.
        if (k == 0 && i < n && match(line, /(^|[ \t])Port$/) && lines[i + 1] ~ /^of /)
            k = length(line) - 3
        if (k > 0) {
            line = substr(line, 1, k - 1)
            sub(/[ \t]+$/, "", line)
            if (line != "") out = out (out == "" ? "" : "\036") line
            break
        }
        out = out (i == 1 ? "" : "\036") line
    }
    # Drop trailing empty lines.
    while (out ~ /\036$/) sub(/\036$/, "", out)
    return out
}

# The upstream name cited in block b: "Port of FILE:NAME" for a function
# ("" for an internal C function), "Port of FILE NAME" for a variable.
function citation(b, var,    flat, k, rest, file, name) {
    flat = b
    gsub(/\036/, " ", flat)
    gsub(/  +/, " ", flat)
    k = index(flat, "Port of ")
    if (k == 0) return ""
    rest = substr(flat, k + 8)
    if (!match(rest, /^[A-Za-z0-9_\/.-]+\.(c|el|h)/)) return ""
    file = substr(rest, 1, RLENGTH)
    rest = substr(rest, RLENGTH + 1)
    if (var) {
        if (!match(rest, /^ [a-z][A-Za-z0-9*+-]*/)) return ""
        # (lower case: dired-ls-F-marks-symlinks is dired_ls_f_marks_symlinks)
        name = tolower(substr(rest, 2, RLENGTH - 1))
        # A C variable (point_before_last_command_or_undo), not a DEFVAR.
        if (substr(rest, RLENGTH + 1, 1) == "_") return ""
        sub(/-$/, "", name)
        gsub(/-/, "_", name)
        return name
    }
    if (!match(rest, /^:[A-Za-z0-9_*+<>=\/-]+/)) return ""
    name = substr(rest, 2, RLENGTH - 1)
    sub(/-$/, "", name)
    if (file ~ /\.(c|h)$/) {
        if (name !~ /^F[a-z]/) return ""
        name = substr(name, 2)
    }
    # Fortmacs names use underscores where Emacs uses hyphens.
    gsub(/-/, "_", name)
    return name
}

FNR == 1 { block = ""; sig = ""; insig = 0; base = FILENAME; sub(/.*\//, "", base) }
# Submodules (their variables are not accessible) and the names in
# `public ::` statements.
/^submodule/ { submod[base] = 1 }
/^[ \t]*public[ \t]*::/ { inpub = 1; l = $0; sub(/^[^:]*::/, "", l) }
inpub {
    if (!(/^[ \t]*public[ \t]*::/)) l = $0
    cont = (l ~ /&[ \t]*$/); sub(/&[ \t]*$/, "", l)
    np = split(l, pn, ",")
    for (ip = 1; ip <= np; ip++) { nm = trim(pn[ip]); if (nm != "") pubname[nm SUBSEP base] = 1 }
    if (!cont) inpub = 0
}

# Continuation of a procedure header: collect the argument list.
insig {
    line = $0
    sub(/^[ \t]+/, "", line)
    sig = sig line
    if (sig ~ /&[ \t]*$/) { sub(/&[ \t]*$/, "", sig); next }
    insig = 0
    finish_sig()
    next
}

/^[ \t]*!>/ {
    line = $0
    sub(/^[ \t]*!>[ ]?/, "", line)
    sub(/[ \t]+$/, "", line)
    block = block (block == "" ? "" : "\036") line
    next
}

/^[ \t]*abstract interface/ { abstract = 1; block = ""; next }
/^[ \t]*end interface/ { abstract = 0; ingeneric = ""; block = ""; next }

# A named generic interface: documented like a procedure, called like
# its first specific procedure.
/^[ \t]*interface [a-zA-Z0-9_]+[ \t]*$/ {
    gname = tolower($2)
    if (block != "") {
        key = gname SUBSEP base
        pdoc[key] = docstring(block)
        c = citation(block, 0)
        pcite[key] = c
        if (c != "" && !(c in cited)) cited[c] = key
        generic[key] = 1
        ingeneric = key
    }
    block = ""
    next
}
ingeneric != "" && /^[ \t]*module procedure/ {
    line = tolower($0)
    sub(/^[ \t]*module procedure[ \t]*/, "", line)
    sub(/[ \t,].*/, "", line)
    if (!(ingeneric in gfirst)) gfirst[ingeneric] = line
    next
}

# A separate module procedure in a submodule (`module procedure NAME`):
# its comment documents the procedure its parent declares.
ingeneric == "" && /^[ \t]*module procedure[ \t]+[a-zA-Z0-9_]+[ \t]*$/ {
    if (block != "") {
        pname = tolower($3)
        key = pname SUBSEP base
        if (!(key in pdoc)) {
            pdoc[key] = docstring(block)
            c = citation(block, 0)
            pcite[key] = c
            if (c != "" && !(c in cited)) cited[c] = key
        }
        if (!(pname in anydoc) || anydoc[pname] == "") anydoc[pname] = pdoc[key]
    }
    block = ""
    next
}

# Built-in commands (not in comments): `commands%NAME%run => PROC`, and
# `commands%NAME%interactive = .false.` for a function that is not a
# command.
/^[ \t]*commands%[a-z0-9_]+%run[ \t]*=>/ {
    line = $0
    sub(/^[ \t]*commands%/, "", line)
    name = line; sub(/%.*/, "", name); name = help_name(name)
    proc = line; sub(/^[^>]*>[ \t]*/, "", proc); sub(/[^a-z0-9_].*/, "", proc)
    if (!(name in cmd)) { cmd[name] = proc; cmdfile[name] = base }
}
/^[ \t]*commands%[a-z0-9_]+%interactive[ \t]*=[ \t]*\.false\./ {
    line = $0
    sub(/^[ \t]*commands%/, "", line)
    name = line; sub(/%.*/, "", name)
    noninteractive[help_name(name)] = 1
}

{
    line = tolower($0)
    if (!abstract && match(line, /^[ \t]*((pure|elemental|impure|recursive|module|non_recursive)[ \t]+|(integer|logical|real|character|type)(\([^)]*\))?[ \t]+)*(subroutine|function)[ \t]+[a-z0-9_]+/)) {
        head = substr(line, 1, RLENGTH)
        kind = (head ~ /subroutine[ \t]+[a-z0-9_]+$/) ? "subroutine" : "function"
        pname = head; sub(/.*[ \t]/, "", pname)
        pblock = block
        sig = trim(substr($0, RLENGTH + 1))
        sig = pname sig
        if (sig ~ /&[ \t]*$/) { sub(/&[ \t]*$/, "", sig); insig = 1; block = ""; next }
        finish_sig()
    } else if (block != "" && line !~ /parameter/ && match(declline(line), /^[ \t]*(integer|logical|character|real|type[ \t]*\()[^:!]*::[ \t]*[a-z0-9_]+/)) {
        decl = substr(declline(line), 1, RLENGTH)
        vname = decl; sub(/.*[ \t:]/, "", vname)
        type = decl; sub(/[ \t]*::.*/, "", type); sub(/^[ \t]+/, "", type)
        rest = substr($0, RLENGTH + 1)
        # A declaration continued over several lines is joined into one.
        while (rest ~ /&[ \t]*$/ && (getline nextl) > 0) {
            sub(/[ \t]*&[ \t]*$/, " ", rest)
            sub(/^[ \t]*&?[ \t]*/, "", nextl)
            rest = rest nextl
        }
        is_array = (rest ~ /^\(/) ? 1 : 0
        init = ""
        if (!is_array && match(rest, /^[ \t]*=[ \t]*/)) {
            init = substr(rest, RLENGTH + 1)
            sub(/[ \t]*(,|&|!).*$/, "", init)
            if (init ~ /^"/ || init ~ "^\047") {
                init = substr(rest, RLENGTH + 1)
                sub(/[ \t]*$/, "", init)
            }
        }
        # A setting object (data_fm): its initial value is the value=
        # argument of its constructor.
        if (type ~ /_setting\)/) {
            init = ""
            if (match(rest, /value[ \t]*=[ \t]*[^,)]+/)) {
                init = substr(rest, RSTART, RLENGTH)
                sub(/value[ \t]*=[ \t]*/, "", init)
            }
        } else if (type ~ /^type/) {
            # Other derived types have no printed value to compare.
            init = ""
        }
        key = vname SUBSEP base
        vdoc[key] = docstring(block)
        vcite[key] = citation(block, 1)
        vtype[key] = type
        vpub[key] = (type ~ /public/ || (key in pubname)) && !(base in submod)
        varr[key] = is_array
        vinit[key] = init
    }
    block = ""
}

function finish_sig(    s, key, c) {
    s = sig
    sub(/[ \t]*result[ \t]*\(.*$/, "", s)
    sub(/[ \t]*bind[ \t]*\(.*$/, "", s)
    sub(/[ \t]*!.*$/, "", s)
    gsub(/[ \t]*,[ \t]*/, ", ", s)
    key = pname SUBSEP base
    if (!(pname in anycall)) anycall[pname] = (kind == "subroutine" ? "call " : "") s
    if (pblock == "") return
    if (!(key in pdoc)) {
        pdoc[key] = docstring(pblock)
        pcall[key] = (kind == "subroutine" ? "call " : "") s
        c = citation(pblock, 0)
        pcite[key] = c
        if (c != "" && !(c in cited)) { cited[c] = key }
    }
    # Any file: for commands registered with a procedure from elsewhere.
    if (!(pname in anydoc)) anydoc[pname] = pdoc[key]
}

END {
    for (key in generic) {
        split(key, kp, SUBSEP)
        pcall[key] = (gfirst[key] in anycall) ? anycall[gfirst[key]] : ""
        sub(gfirst[key] "\\(", kp[1] "(", pcall[key])
    }

    # The config API: names src/fortmacs.f90 takes from each module.
    file = "src/fortmacs.f90"
    inuse = 0
    while ((getline l < file) > 0) {
        if (l ~ /^[ \t]*implicit none/) break
        if (l ~ /^[ \t]*use [a-z0-9_]+, only:/) {
            mod = l; sub(/^[ \t]*use /, "", mod); sub(/,.*/, "", mod)
            sub(/^[^:]*only:/, "", l)
            inuse = 1
        }
        if (!inuse) continue
        cont = (l ~ /&[ \t]*$/)
        sub(/&[ \t]*$/, "", l)
        n = split(l, names, ",")
        for (i = 1; i <= n; i++) {
            nm = trim(names[i])
            if (nm != "") api[nm] = mod ".f90"
        }
        if (!cont) inuse = 0
    }

    # Functions: the API procedures, then the commands.
    for (nm in api) {
        key = nm SUBSEP api[nm]
        if (key in pdoc) {
            lisp = pcite[key]
            if (lisp == "") lisp = help_name(nm)
            # Several API procedures can port one function (capitalize_char
            # and capitalize_string): the first by name, whatever order
            # this awk visits the array in.
            if ((lisp in fowner) && fowner[lisp] < nm) continue
            fowner[lisp] = nm
            fdoc[lisp] = pdoc[key]; ffile[lisp] = api[nm]; fcall[lisp] = pcall[key]
            finter[lisp] = 0
        } else if (key in vdoc) {
            lisp = vcite[key]
            if (lisp == "") lisp = help_name(nm)
            printf "V\037%s\037%s\037%s\037%s\037%d\037%s\037%s\n", lisp, api[nm], nm, vtype[key], \
                varr[key], vinit[key], vdoc[key]
        }
    }
    # Every other variable that cites the upstream variable it ports
    # (upstream documents every DEFVAR and defvar, not only the
    # settings a configuration uses).
    for (key in vdoc) {
        if (vcite[key] == "") continue
        if (!vpub[key] && !(key in pubname)) continue
        split(key, kp, SUBSEP)
        if (kp[2] in submod) continue
        if ((kp[1] in api) && api[kp[1]] == kp[2]) continue
        printf "V\037%s\037%s\037%s\037%s\037%d\037%s\037%s\n", vcite[key], kp[2], kp[1], vtype[key], \
            varr[key], vinit[key], vdoc[key]
    }
    for (name in cmd) {
        proc = cmd[name]
        # The docstring of the command itself, else of the API function, else
        # of the procedure citing it.
        d = (proc SUBSEP cmdfile[name] in pdoc) ? pdoc[proc SUBSEP cmdfile[name]] : anydoc[proc]
        if (d == "" && (name in fdoc)) d = fdoc[name]
        if (d == "" && (name in cited)) d = pdoc[cited[name]]
        fdoc[name] = d
        if (!(name in ffile)) { ffile[name] = cmdfile[name]; fcall[name] = "" }
        finter[name] = (name in noninteractive) ? 0 : 1
    }
    # A command without a docstring of its own that runs the procedure of
    # another command is an alias of it (join_line of delete_indentation):
    # it gets the docstring of that command, the first such name in order
    # whatever order this awk visits the array in.
    for (name in cmd) {
        if (fdoc[name] != "") continue
        best = ""
        for (other in cmd)
            if (other != name && cmd[other] == cmd[name] && fdoc[other] != "" && (best == "" || other < best))
                best = other
        if (best != "") alias_doc[name] = fdoc[best]
    }
    for (name in alias_doc) fdoc[name] = alias_doc[name]
    for (lisp in fdoc)
        printf "F\037%s\037%s\037%s\037%d\037%s\n", lisp, ffile[lisp], fcall[lisp], finter[lisp], fdoc[lisp]
}
' | LC_ALL=C sort -t "$(printf '\037')" -k1,1 -k2,2 >"$tmp.records"

# The named constants of enumerated settings (integer variables whose
# value is one of a set of parameters, like search_upper_case).
# A declaration continued over several lines is joined into one.
find src -name '*.f90' ! -name docstrings_fm.f90 | sort |
    xargs awk '
        cont { sub(/^[ \t]*/, ""); line = line $0 }
        !cont && /integer, parameter, public ::/ { line = $0 }
        !cont && !/integer, parameter, public ::/ { next }
        {
            cont = sub(/[ \t]*&[ \t]*$/, " ", line)
            if (!cont) print line
        }' >"$tmp.params" || true

# The user options (tools/user-options), without comments.
grep -v '^#' tools/user-options | grep -v '^[ \t]*$' >"$tmp.uopts" || true

# Pass 2: the Fortran module.
awk -F '\037' -v params="$tmp.params" -v uopts="$tmp.uopts" '
function q(s) { gsub(/"/, "\"\"", s); return "\"" s "\"" }

# A string literal for s, in pieces of at most 90 characters.
function lit(s, ind,    out) {
    out = ""
    while (length(s) > 90) {
        out = out q(substr(s, 1, 90)) "// &\n" ind
        s = substr(s, 91)
    }
    return out q(s)
}

# The kind of value set_variable reads for variable i: OPTION_LOGICAL,
# OPTION_INTEGER, OPTION_REAL, OPTION_STRING, OPTION_CHOICE (one of the
# named constants of an enumerated setting), or OPTION_NONE (hooks,
# procedures, arrays and other derived types).
function okind(i,    t) {
    t = vtype[i]
    if (varr[i]) return "OPTION_NONE"
    if (venum[i] != "" && (t ~ /^integer/ || t ~ /integer_setting\)/)) return "OPTION_CHOICE"
    if (t ~ /logical_setting\)/ || t ~ /^logical/) return "OPTION_LOGICAL"
    if (t ~ /integer_setting\)/ || t ~ /^integer/) return "OPTION_INTEGER"
    if (t ~ /^real/) return "OPTION_REAL"
    if (t ~ /string_setting\)/ || t ~ /^character/) return "OPTION_STRING"
    return "OPTION_NONE"
}

# Whether variable i is a setting object (it can have buffer-local values).
function osetting(i) { return vtype[i] ~ /(logical|integer|string)_setting\)/ }

# Fortran statements assigning `val` to variable i, given buffer-locally
# when `local` is true and the variable is a setting object.
function oassign(i, val, ind) {
    if (osetting(i))
        return ind "if (local) then\n" ind "    call " vfort[i] "%set_local(" val ")\n" ind "else\n" \
            ind "    " vfort[i] " = " val "\n" ind "end if"
    return ind vfort[i] " = " val
}

# Fortran expression for docstring d (lines joined by \036), indented.
function docexpr(d, ind,    n, i, lines, out, line, piece) {
    if (d == "") return "\"\""
    n = split(d, lines, "\036")
    out = ""
    for (i = 1; i <= n; i++) {
        line = lines[i]
        # Long lines go in pieces, to stay within 132 columns.
        while (length(line) > 90) {
            piece = substr(line, 1, 90)
            line = substr(line, 91)
            out = out q(piece) "// &\n" ind
        }
        out = out q(line)
        if (i < n) out = out "//LF// &\n" ind
    }
    return out
}

BEGIN {
    nu = 0
    while ((getline l < uopts) > 0) { uopt[l] = 1; nu++; uname[nu] = l }
    while ((getline l < params) > 0) {
        sub(/.*::/, "", l)
        n = split(l, defs, ",")
        for (i = 1; i <= n; i++) {
            d = defs[i]; gsub(/[ \t]/, "", d)
            split(d, kv, "=")
            pval[kv[1]] = kv[2]
            group[kv[1]] = l
        }
    }
}

$1 == "F" { nf++; fname[nf] = $2; ffile[nf] = $3; fcall[nf] = $4; finter[nf] = $5; fdoc[nf] = $6 }
$1 == "V" {
    nv++; vname[nv] = $2; vfile[nv] = $3; vfort[nv] = $4; vtype[nv] = $5; varr[nv] = $6
    vinit[nv] = $7; vdoc[nv] = $8
    mods[$3] = mods[$3] (mods[$3] == "" ? "" : ", ") $4
    # The procedures naming the value of a procedure setting.
    if ($5 ~ /function_setting\)/ && !named_cmd++)
        mods["callint_fm.f90"] = mods["callint_fm.f90"] (mods["callint_fm.f90"] == "" ? "" : ", ") "command_name_of"
    if ($5 ~ /indent_line_setting\)/ && !named_indent++)
        mods["indent_fm.f90"] = mods["indent_fm.f90"] (mods["indent_fm.f90"] == "" ? "" : ", ") "indent_line_function_name"
    if ($7 ~ /^[A-Z][A-Z0-9_]*$/ && ($7 in group)) {
        venum[nv] = group[$7]
        n = split(group[$7], defs, ",")
        for (i = 1; i <= n; i++) {
            d = defs[i]; gsub(/[ \t]/, "", d); sub(/=.*/, "", d)
            mods[$3] = mods[$3] ", " d
        }
    }
}

END {
    # Every user option must be a documented variable.
    for (i = 1; i <= nv; i++) vidx[vname[i]] = i
    for (k = 1; k <= nu; k++)
        if (!(uname[k] in vidx)) {
            print "tools/user-options: " uname[k] " is not a documented variable" > "/dev/stderr"
            exit 1
        }
    print "! docstrings_fm.f90 -- the documentation C-h f, C-h k and C-h v show."
    print "! Generated by tools/gen-docstrings.sh from the !> comments in src/; do"
    print "! not edit."
    print "!"
    print "! Copyright (C) 2026 The Fortmacs authors"
    print "!"
    print "! This file is part of Fortmacs."
    print "!"
    print "! Fortmacs is free software: you can redistribute it and/or modify"
    print "! it under the terms of the GNU General Public License as published by"
    print "! the Free Software Foundation, either version 3 of the License, or"
    print "! (at your option) any later version."
    print "!"
    print "! Fortmacs is distributed in the hope that it will be useful,"
    print "! but WITHOUT ANY WARRANTY; without even the implied warranty of"
    print "! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
    print "! GNU General Public License for more details."
    print "!"
    print "! You should have received a copy of the GNU General Public License"
    print "! along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>."
    print ""
    print "!> The documentation of the commands, the config API'"'"'s procedures and"
    print "!> its variables, collected from their docstrings by"
    print "!> tools/gen-docstrings.sh (the counterpart of etc/DOC, which"
    print "!> lib-src/make-docfile.c collects from the C sources).  Variables are"
    print "!> read here, so their current values can be shown."
    print "module docstrings_fm"
    for (m in mods) mlist[++nm] = m
    # Sort the module names (few of them; insertion sort).
    for (i = 2; i <= nm; i++) {
        x = mlist[i]
        for (j = i - 1; j >= 1 && mlist[j] > x; j--) mlist[j + 1] = mlist[j]
        mlist[j + 1] = x
    }
    print "    use, intrinsic :: iso_fortran_env, only: int64"
    print "    use print_fm, only: fortran_repr"
    for (i = 1; i <= nm; i++) {
        m = mlist[i]; sub(/\.f90$/, "", m)
        line = "    use " m ", only: "
        n = split(mods[mlist[i]], ids, ", ")
        for (k = 1; k <= n; k++) {
            if (length(line) + length(ids[k]) > 100) {
                print line "&"
                line = "        "
            }
            line = line ids[k] (k < n ? ", " : "")
        }
        print line
    }
    print "    implicit none(type, external)"
    print "    private"
    print ""
    print "    public :: function_doc, variable_doc"
    print "    public :: user_option_info, set_user_option_logical, set_user_option_integer, set_user_option_real, &"
    print "              set_user_option_string, set_user_option_choice"
    print ""
    print "    !> Kinds of value of a user option, as `set_variable` reads it."
    print "    integer, parameter, public :: OPTION_NONE = 0, OPTION_LOGICAL = 1, OPTION_INTEGER = 2, OPTION_REAL = 3, &"
    print "                                  OPTION_STRING = 4, OPTION_CHOICE = 5"
    print ""
    print "    !> What help shows about a function or variable."
    print "    type, public :: doc_entry_type"
    print "        !> The docstring, before substitute-command-keys; \"\" if none."
    print "        character(len=:), allocatable :: doc"
    print "        !> The source file that defines it."
    print "        character(len=:), allocatable :: file"
    print "        !> A function: how Fortran calls it (\"\" for a command only)."
    print "        !> A variable: its Fortran name."
    print "        character(len=:), allocatable :: fortran"
    print "        !> A function that is a command."
    print "        logical :: interactive = .false."
    print "        !> A variable: its value and its value at startup (\"\" when"
    print "        !> unknown), as Fortran."
    print "        character(len=:), allocatable :: value, original"
    print "        !> A variable that is a user option (tools/user-options)."
    print "        logical :: user_option = .false."
    print "    end type doc_entry_type"
    print ""
    w = 1
    for (i = 1; i <= nf; i++) if (length(fname[i]) > w) w = length(fname[i])
    printf "    !> The documented functions, sorted.\n"
    printf "    character(len=%d), parameter, public :: function_names(%d) = [character(len=%d) :: &\n", w, nf, w
    for (i = 1; i <= nf; i++)
        printf "        %s%s\n", q(fname[i]), (i < nf ? ", &" : "]")
    print ""
    w = 1
    for (i = 1; i <= nv; i++) if (length(vname[i]) > w) w = length(vname[i])
    printf "    !> The documented variables, sorted.\n"
    printf "    character(len=%d), parameter, public :: variable_names(%d) = [character(len=%d) :: &\n", w, nv, w
    for (i = 1; i <= nv; i++)
        printf "        %s%s\n", q(vname[i]), (i < nv ? ", &" : "]")
    print ""
    w = 1
    for (k = 1; k <= nu; k++) if (length(uname[k]) > w) w = length(uname[k])
    printf "    !> The user options (tools/user-options), sorted.\n"
    printf "    character(len=%d), parameter, public :: user_option_names(%d) = [character(len=%d) :: &\n", w, nu, w
    for (k = 1; k <= nu; k++)
        printf "        %s%s\n", q(uname[k]), (k < nu ? ", &" : "]")
    print ""
    print "    character(len=*), parameter :: LF = achar(10)"
    print ""
    print "    interface repr"
    print "        module procedure repr_logical, repr_integer, repr_int64, repr_int64s, repr_real, repr_string, repr_strings"
    print "    end interface repr"
    print ""
    print "contains"
    print ""
    print "    !> The documentation of the function called `name`; `found` is false"
    print "    !> for an unknown name."
    print "    subroutine function_doc(name, entry, found)"
    print "        character(len=*), intent(in) :: name"
    print "        type(doc_entry_type), intent(out) :: entry"
    print "        logical, intent(out) :: found"
    print ""
    print "        found = .true."
    print "        entry%fortran = \"\""
    print ""
    print "        select case (name)"
    for (i = 1; i <= nf; i++) {
        print "        case (" q(fname[i]) ")"
        print "            entry%file = " q(ffile[i])
        if (fcall[i] != "") print "            entry%fortran = " lit(fcall[i], "                ")
        if (finter[i]) print "            entry%interactive = .true."
        print "            entry%doc = " docexpr(fdoc[i], "                        ")
    }
    print "        case default"
    print "            found = .false."
    print "        end select"
    print "    end subroutine function_doc"
    print ""
    print "    !> The documentation and value of the variable called `name`;"
    print "    !> `found` is false for an unknown name."
    print "    subroutine variable_doc(name, entry, found)"
    print "        character(len=*), intent(in) :: name"
    print "        type(doc_entry_type), intent(out) :: entry"
    print "        logical, intent(out) :: found"
    print ""
    print "        found = .true."
    print "        entry%original = \"\""
    print ""
    print "        select case (name)"
    for (i = 1; i <= nv; i++) {
        print "        case (" q(vname[i]) ")"
        print "            entry%file = " q(vfile[i])
        print "            entry%fortran = " q(vfort[i])
        if (vname[i] in uopt) print "            entry%user_option = .true."
        if (vinit[i] != "") {
            init = vinit[i]
            sub(/_int(32|64)$/, "", init)
            print "            entry%original = " q(init)
        }
        if (venum[i] != "") {
            # The value of a setting object is its value component.
            ex = vfort[i]
            if (vtype[i] ~ /_setting\)/) ex = ex "%value"
            print "            select case (" ex ")"
            n = split(venum[i], defs, ",")
            for (k = 1; k <= n; k++) {
                d = defs[k]; gsub(/[ \t]/, "", d); sub(/=.*/, "", d)
                print "            case (" d ")"
                print "                entry%value = " q(d)
            }
            print "            case default"
            print "                entry%value = repr(" ex ")"
            print "            end select"
        } else {
            # Values as Fortran: a string or array that is not allocated
            # is "unallocated", a procedure pointer the name of its procedure or
            # null().  Other derived types (hooks, keymaps, syntax tables)
            # have no printed representation yet: help leaves the value out.
            if (vtype[i] ~ /(integer|logical)_setting\)/) print "            entry%value = repr(" vfort[i] "%value)"
            else if (vtype[i] ~ /string_setting\)/) {
                print "            if (allocated(" vfort[i] "%value)) then"
                print "                entry%value = repr(" vfort[i] "%value)"
                print "            else"
                print "                entry%value = \"unallocated\""
                print "            end if"
            } else if (vtype[i] ~ /function_setting\)/) {
                print "            if (associated(" vfort[i] "%value)) then"
                print "                entry%value = command_name_of(" vfort[i] "%value)"
                print "            else"
                print "                entry%value = \"null()\""
                print "            end if"
            } else if (vtype[i] ~ /indent_line_setting\)/)
                print "            entry%value = indent_line_function_name(" vfort[i] "%value)"
            else if (vtype[i] ~ /value_type/ && !varr[i]) print "            entry%value = fortran_repr(" vfort[i] ")"
            else if (vtype[i] ~ /allocatable/ && vtype[i] !~ /^type/) {
                print "            if (allocated(" vfort[i] ")) then"
                print "                entry%value = repr(" vfort[i] ")"
                print "            else"
                print "                entry%value = \"unallocated\""
                print "            end if"
            } else if (vtype[i] !~ /^type/) print "            entry%value = repr(" vfort[i] ")"
        }
        print "            entry%doc = " docexpr(vdoc[i], "                        ")
    }
    print "        case default"
    print "            found = .false."
    print "        end select"
    print "    end subroutine variable_doc"
    print ""
    print "    !> The kind of value the user option called `name` takes (OPTION_NONE"
    print "    !> if `set_variable` cannot set it, or for an unknown name), and"
    print "    !> whether it is a setting object, which can have buffer-local values."
    print "    subroutine user_option_info(name, kind, setting)"
    print "        character(len=*), intent(in) :: name"
    print "        integer, intent(out) :: kind"
    print "        logical, intent(out) :: setting"
    print ""
    print "        kind = OPTION_NONE"
    print "        setting = .false."
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) == "OPTION_NONE") continue
        print "        case (" q(vname[i]) ")"
        print "            kind = " okind(i)
        if (osetting(i)) print "            setting = .true."
    }
    print "        case default"
    print "        end select"
    print "    end subroutine user_option_info"
    print ""
    print "    !> Set the logical user option called `name` to `v`, buffer-locally if"
    print "    !> `local` (a setting object)."
    print "    subroutine set_user_option_logical(name, v, local)"
    print "        character(len=*), intent(in) :: name"
    print "        logical, intent(in) :: v, local"
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_LOGICAL") continue
        print "        case (" q(vname[i]) ")"
        print oassign(i, "v", "            ")
    }
    print "        case default"
    print "            if (local) return"
    print "        end select"
    print "    end subroutine set_user_option_logical"
    print ""
    print "    !> Set the integer user option called `name` to `v`, buffer-locally if"
    print "    !> `local` (a setting object); `ok` is false if `v` is out of its range."
    print "    subroutine set_user_option_integer(name, v, local, ok)"
    print "        character(len=*), intent(in) :: name"
    print "        integer(int64), intent(in) :: v"
    print "        logical, intent(in) :: local"
    print "        logical, intent(out) :: ok"
    print ""
    print "        ok = .true."
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_INTEGER") continue
        print "        case (" q(vname[i]) ")"
        if (vtype[i] ~ /^integer,/ || vtype[i] ~ /^integer$/) {
            print "            if (v < -huge(0) - 1 .or. v > huge(0)) then"
            print "                ok = .false."
            print "                return"
            print "            end if"
            print oassign(i, "int(v)", "            ")
        } else print oassign(i, "v", "            ")
    }
    print "        case default"
    print "            if (local) return"
    print "        end select"
    print "    end subroutine set_user_option_integer"
    print ""
    print "    !> Set the real user option called `name` to `v`."
    print "    subroutine set_user_option_real(name, v)"
    print "        character(len=*), intent(in) :: name"
    print "        real, intent(in) :: v"
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_REAL") continue
        print "        case (" q(vname[i]) ")"
        print "            " vfort[i] " = v"
    }
    print "        case default"
    print "            if (v > 0) return"
    print "        end select"
    print "    end subroutine set_user_option_real"
    print ""
    print "    !> Set the string user option called `name` to `v`, buffer-locally if"
    print "    !> `local` (a setting object)."
    print "    subroutine set_user_option_string(name, v, local)"
    print "        character(len=*), intent(in) :: name, v"
    print "        logical, intent(in) :: local"
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_STRING") continue
        print "        case (" q(vname[i]) ")"
        print oassign(i, "v", "            ")
    }
    print "        case default"
    print "            if (local) return"
    print "        end select"
    print "    end subroutine set_user_option_string"
    print ""
    print "    !> Set the enumerated user option called `name` to its named constant"
    print "    !> `constant` (in upper case), buffer-locally if `local` (a setting"
    print "    !> object); `ok` is false if `constant` is not one of its constants."
    print "    subroutine set_user_option_choice(name, constant, local, ok)"
    print "        character(len=*), intent(in) :: name, constant"
    print "        logical, intent(in) :: local"
    print "        logical, intent(out) :: ok"
    print ""
    print "        ok = .true."
    print ""
    print "        select case (name)"
    for (k = 1; k <= nu; k++) {
        i = vidx[uname[k]]
        if (okind(i) != "OPTION_CHOICE") continue
        print "        case (" q(vname[i]) ")"
        print "            select case (constant)"
        n = split(venum[i], defs, ",")
        for (m = 1; m <= n; m++) {
            d = defs[m]; gsub(/[ \t]/, "", d); sub(/=.*/, "", d)
            print "            case (" q(toupper(d)) ")"
            print oassign(i, d, "                ")
        }
        print "            case default"
        print "                ok = .false."
        print "            end select"
    }
    print "        case default"
    print "            ok = .false."
    print "        end select"
    print "    end subroutine set_user_option_choice"
    print ""
    print "    function repr_logical(v) result(s)"
    print "        logical, intent(in) :: v"
    print "        character(len=:), allocatable :: s"
    print ""
    print "        s = merge(\".true. \", \".false.\", v)"
    print "        s = trim(s)"
    print "    end function repr_logical"
    print ""
    print "    function repr_integer(v) result(s)"
    print "        integer, intent(in) :: v"
    print "        character(len=:), allocatable :: s"
    print "        character(len=12) :: buf"
    print ""
    print "        write (buf, \"(i0)\") v"
    print "        s = trim(buf)"
    print "    end function repr_integer"
    print ""
    print "    ! A real with at least one digit"
    print "    ! after the point, no trailing zeros."
    print "    function repr_real(v) result(s)"
    print "        real, intent(in) :: v"
    print "        character(len=:), allocatable :: s"
    print "        character(len=32) :: buf"
    print ""
    print "        write (buf, \"(f0.6)\") v"
    print "        s = trim(buf)"
    print "        do while (len(s) > 2)"
    print "            if (s(len(s):len(s)) /= \"0\" .or. s(len(s) - 1:len(s) - 1) == \".\") exit"
    print "            s = s(1:len(s) - 1)"
    print "        end do"
    print "        if (s(1:1) == \".\") s = \"0\"//s"
    print "        if (len(s) > 1) then"
    print "            if (s(1:2) == \"-.\") s = \"-0\"//s(2:)"
    print "        end if"
    print "    end function repr_real"
    print ""
    print "    function repr_int64(v) result(s)"
    print "        integer(int64), intent(in) :: v"
    print "        character(len=:), allocatable :: s"
    print "        character(len=21) :: buf"
    print ""
    print "        write (buf, \"(i0)\") v"
    print "        s = trim(buf)"
    print "    end function repr_int64"
    print ""
    print "    !> An array of integers as a Fortran array constructor."
    print "    function repr_int64s(v) result(s)"
    print "        integer(int64), intent(in) :: v(:)"
    print "        character(len=:), allocatable :: s"
    print "        integer :: i"
    print ""
    print "        s = \"[\""
    print ""
    print "        do i = 1, size(v)"
    print "            if (i > 1) s = s//\", \""
    print "            s = s//repr_int64(v(i))"
    print "        end do"
    print ""
    print "        s = s//\"]\""
    print "    end function repr_int64s"
    print ""
    print "    !> A string as a Fortran literal."
    print "    function repr_string(v) result(s)"
    print "        character(len=*), intent(in) :: v"
    print "        character(len=:), allocatable :: s"
    print "        integer :: i"
    print ""
    print "        s = \"\"\"\""
    print ""
    print "        do i = 1, len_trim(v)"
    print "            s = s//v(i:i)"
    print "            if (v(i:i) == \"\"\"\") s = s//\"\"\"\""
    print "        end do"
    print ""
    print "        s = s//\"\"\"\""
    print "    end function repr_string"
    print ""
    print "    !> An array of strings as a Fortran array constructor."
    print "    function repr_strings(v) result(s)"
    print "        character(len=*), intent(in) :: v(:)"
    print "        character(len=:), allocatable :: s"
    print "        integer :: i"
    print ""
    print "        s = \"[\""
    print ""
    print "        do i = 1, size(v)"
    print "            if (i > 1) s = s//\", \""
    print "            s = s//repr_string(v(i))"
    print "        end do"
    print ""
    print "        s = s//\"]\""
    print "    end function repr_strings"
    print "end module docstrings_fm"
}
' "$tmp.records" >"$tmp.f90"

# No blank lines inside procedure bodies (PORTING.md, Style).
awk '/^    (subroutine|function) / { body = 1 } /^    end (subroutine|function) / { body = 0 } !(body && /^$/)' \
    "$tmp.f90" >"$tmp.out"
if command -v fprettify >/dev/null 2>&1; then
    fprettify --config-file .fprettify.rc "$tmp.out" 2>/dev/null
fi
mv "$tmp.out" "$out"
