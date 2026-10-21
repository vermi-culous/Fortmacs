# blank-lines.awk -- the project's blank-line rule for Fortran sources.
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

# Inside a procedure (subroutine or function), exactly one blank line
# separates the declarations from the first statement when there are
# declarations, and there are no other blank lines, except around a
# `contains` and between contained procedures.  Outside procedures, runs
# of blank lines shrink to one.  (Neither the fortran-lang style guide
# nor stdlib's says anything about blank lines, so this is ours.)
#
# Prints the file with the rule applied.  Usage:
#   awk -f tools/blank-lines.awk FILE > FIXED

function lower_stmt(s) {
    s = tolower(s)
    sub(/^[ \t]+/, "", s)
    return s
}

function is_proc_start(s) {
    if (s ~ /^end[ \t]/ || s ~ /^end$/) return 0
    if (s ~ /^module[ \t]+procedure/) return 0
    if (s ~ /^procedure[ \t]*\(/) return 0
    return s ~ /^((pure|elemental|recursive|impure|non_recursive|module)[ \t]+)*((integer|real|logical|character|complex|double[ \t]+precision)([ \t]*\([^)]*\))?[ \t]+|type[ \t]*\([^)]*\)[ \t]+|class[ \t]*\([^)]*\)[ \t]+)?((pure|elemental|recursive|impure|module)[ \t]+)*(subroutine|function)[ \t]+[a-z_]/
}

function is_proc_end(s) {
    return s ~ /^end[ \t]+(subroutine|function)([ \t!]|$)/ || s ~ /^end[ \t]*$/
}

function is_declaration(s) {
    return s ~ /^(use[ \t,]|implicit[ \t]|import([ \t,]|$)|integer|real[ \t(,:]|logical|character|complex|double[ \t]+precision|type[ \t]*[(,:]|class[ \t]*\(|procedure[ \t]*\(|external|intrinsic|dimension|parameter|save([ \t]|$)|target|pointer|allocatable|optional|intent|private|public|protected|volatile|asynchronous|abstract[ \t]+interface|interface|end[ \t]+interface|data[ \t]|namelist|generic|enum|end[ \t]+enum|enumerator)/
}

# Emit the comment lines held back, with one blank line first if `blank`;
# blank lines between them stay (one each) only outside procedures.
function flush(blank,   i, last_blank) {
    if (blank) print ""
    last_blank = blank
    for (i = 1; i <= nheld; i++) {
        if (held[i] == "") {
            if (depth == 0 && !last_blank) print ""
            last_blank = 1
        } else {
            print held[i]
            last_blank = 0
        }
    }
    nheld = 0
    nblank = 0
}

BEGIN { depth = 0; nheld = 0; nblank = 0; cont = 0 }

{
    line = $0
    stmt = lower_stmt(line)

    # Continuation lines of a statement pass through.
    if (cont) {
        print line
        cont = (line ~ /&[ \t]*(![^"']*)?$/)
        next
    }
    if (stmt == "") {
        if (nheld > 0) held[++nheld] = ""
        else nblank++
        next
    }
    if (stmt ~ /^!/) {
        # Comments wait for the statement they introduce; blank lines
        # before them count as before that statement.
        if (nheld == 0) pre_blank = nblank
        held[++nheld] = line
        nblank = 0
        next
    }
    blanks = (nheld > 0) ? pre_blank : nblank
    cont = (line ~ /&[ \t]*(![^"']*)?$/)

    if (depth == 0) {
        flush(blanks > 0)
    } else if (stmt ~ /^contains([ \t!]|$)/) {
        flush(blanks > 0)
        phase[depth] = "contains"
    } else if (is_proc_end(stmt)) {
        flush(phase[depth] == "contains" && blanks > 0)
    } else if (phase[depth] == "contains") {
        flush(blanks > 0)
    } else if (is_declaration(stmt) && phase[depth] != "exec") {
        flush(0)
        phase[depth] = "decl"
    } else if (is_proc_start(stmt)) {
        # An interface body or a contained procedure.
        flush(phase[depth] == "contains" && blanks > 0)
    } else if (phase[depth] == "decl") {
        flush(1)
        phase[depth] = "exec"
    } else {
        flush(0)
        phase[depth] = "exec"
    }

    print line
    if (is_proc_start(stmt)) {
        depth++
        phase[depth] = "header"
    } else if (is_proc_end(stmt) && depth > 0) {
        depth--
    }
}

END { flush(0) }
