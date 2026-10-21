#!/bin/sh
# check.sh -- the single gate run locally and in CI: format, lint, build,
# test.
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

# Usage: tools/check.sh [--fix] [--valgrind] [--no-style]
#   --fix       reformat sources and apply safe lint fixes in place instead
#               of failing on format drift.
#   --valgrind  also run the tests under valgrind and fail on any definite
#               leak or memory error (see tools/valgrind.supp).
#   --no-style  skip the format and lint checks (for distro jobs that only
#               need the build and tests; style is checked elsewhere).

set -eu

FFLAGS="-std=f2018 -Wall -Wextra -fcheck=all -g"
# The linter version CI installs (tools/check.sh only reports it).
FORTITUDE_VERSION=0.9.2
DIRS="src app test"
fix=false
memcheck=false
style=true
for arg in "$@"; do
    case "$arg" in
        --fix) fix=true ;;
        --valgrind) memcheck=true ;;
        --no-style) style=false ;;
        *) echo "usage: tools/check.sh [--fix] [--valgrind] [--no-style]" >&2; exit 2 ;;
    esac
done

cd "$(dirname "$0")/.."

if $style; then
    # Lint: Fortitude's default rules, configured in fpm.toml.  Fixes go
    # first so the formatter tidies what they change.
    if ! command -v fortitude >/dev/null 2>&1; then
        echo "fortitude $FORTITUDE_VERSION not found" >&2
        exit 1
    fi
    if $fix; then
        fortitude check --fix $DIRS >/dev/null 2>&1 || true
    fi

    if $fix; then
        fprettify --config-file .fprettify.rc --recursive $DIRS
    else
        drift=$(fprettify --config-file .fprettify.rc --recursive --diff $DIRS)
        if [ -n "$drift" ]; then
            printf '%s\n' "$drift"
            echo "format drift: run tools/check.sh --fix" >&2
            exit 1
        fi
    fi

    fortitude check $DIRS
fi

fpm build --profile debug --flag "$FFLAGS"
fpm test --profile debug --flag "$FFLAGS"

if $memcheck; then
    fpm test --profile debug --flag "$FFLAGS" --runner \
        "valgrind -q --leak-check=full --errors-for-leak-kinds=definite --error-exitcode=9 --suppressions=tools/valgrind.supp"
fi
