# Porting Fortmacs

This file is the single source of truth for what has been ported, what is
next, and how each port is done. Update it in the same PR as the code.

Reference source: GNU Emacs tag `emacs-31.1`
(commit `a360712c9d272d950d8d8255ef74570f7e90b7d9`). Fetch it with
`tools/fetch-upstream.sh`; it lands in the git-ignored `upstream/emacs/`.

## Roadmap

### Phase 0 — Scaffold
fpm layout, `src/sys/`, CI on x86-64 and aarch64, upstream fetch script,
this file.

### Phase 1 — Data structures
1. `fm_error`, `fm_handle` — status/error type and the generational handle
   registry. Foundations with no single upstream file.
2. `fm_character` — UTF-8 encoding and char↔byte conversion
   (`character.c`, `character.h`).
3. `fm_buffer` + `fm_insdel` — gap buffer, insert/delete, create/kill,
   current buffer (`buffer.c`, `buffer.h`, `insdel.c`); plus the
   char↔byte conversion half of `fm_marker` (`marker.c`), which insdel needs.
4. `fm_marker` — marker objects and their adjustment on insert/delete
   (`marker.c`); `kill-buffer`/`erase-buffer` completed in a submodule of
   fm_buffer (which can use fm_marker/fm_insdel without a module cycle).
   Narrowing and `goto-char` come with `fm_editfns` (`editfns.c`).
5. Value model — symbols and the minimal value type that text properties,
   keymaps and buffer-local variables need (see Open questions).
6. `fm_intervals` + `fm_textprop` — interval tree and text properties
   (`intervals.c`, `textprop.c`).

### Phase 2 — Command loop and keymaps
`src/sys/` termios raw mode, `fm_keymap` (`keymap.c`), `fm_keyboard`
(`keyboard.c`), `fm_cmds` (`cmds.c`).

### Phase 3 — Terminal display
Minimal `term.c` / `dispnew.c` / `xdisp.c` subset.
**Milestone: open a file, edit it, save it.**

### Phase 4 — Config API
Fortran-native `init.f90` API, `fortmacs --recompile`.

### Phase 5 — Generations, then features
Lightweight numbered generations, `--rollback`, `--list-generations`;
then features in whatever order the user asks for.

## Per-module workflow (definition of done)

1. **Scope.** List the upstream functions being ported and the ones
   deferred (with reason) in the module's status row below.
2. **Tests first.** Port the matching `test/src/*-tests.el` cases to
   test-drive (`test/test_fm_<name>.f90`) where they exist; add our own for
   uncovered behaviour. Name each ported test after its ERT test.
3. **Implement.** One module per upstream C file, `fm_` prefix. Cite the
   upstream file/function in a comment on every ported procedure, e.g.
   `!> Port of insdel.c:insert_1_both.`
4. **Headers.** Ported files carry the upstream FSF notice plus ours
   (template below); new files carry ours only.
5. **Gate.** `tools/check.sh` passes (fprettify clean, debug profile
   `-std=f2018 -Wall -Wextra -fcheck=all -g`), and CI is green on both
   architectures.
6. **Record.** Update the status table, one module per PR.

### Header for ported files

```fortran
! fm_<name>.f90 -- <one line>.
! Ported from GNU Emacs 31.1 src/<file>.c.
!
! Copyright (C) <upstream years> Free Software Foundation, Inc.
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: ... (GPLv3-or-later text, as in fm_version.f90)
```

Copy the FSF year range exactly from the upstream file.

## Status

| Module | Upstream | Ported | Deferred | Tests | State |
|---|---|---|---|---|---|
| fm_version | — | — | — | test_fm_version | done |
| fm_error | data.c `syms_of_data` error table (subset), fileio.c `file-error`, print.c `print_error_message` layout | status type, codes, `condition_matches`, `format_error` | remaining error symbols (added as modules need them) | test_fm_error | done |
| fm_handle | — (replaces GC'd pointers) | slot table, generational handles | — | test_fm_handle | done |
| fm_buffer | buffer.c, buffer.h (+ intervals.c `temp_set_point_both`, lisp.h `modiff_incr`) | struct buffer_text / struct buffer (subset), registry, `get-buffer`, `get-buffer-create`, `generate-new-buffer-name`, `buffer-name`, `buffer-last-name`, `rename-buffer`, `buffer-list`, `set-buffer`, `kill-buffer` (core), `buffer-modified-p`, `restore-buffer-modified-p`, modified ticks, `barf-if-buffer-read-only`, BUF_BYTE_ADDRESS / FETCH_BYTE / FETCH_CHAR / next/prev char len, BUF_COMPUTE_UNCHANGED | indirect buffers, buffer-local vars, overlays, undo, files/locking, hooks, `autosaved` state, `inhibit-read-only`, `erase-buffer`, marker/window parts of `kill-buffer`, `other-buffer` ordering | test_fm_buffer (ports bug27966 and the file-independent parts of test-buffer-modifications, test-restore-buffer-modified-p, test-buffer-chars-modified-ticks) | done |
| fm_marker | marker.c | `buf_charpos_to_bytepos`, `buf_bytepos_to_charpos`, position cache, CHAR_TO_BYTE / BYTE_TO_CHAR | marker objects (step 4) | via test_fm_insdel | partial |
| fm_insdel | insdel.c | gap motion, `make_gap` (larger/smaller), `insert`, `insert_char`, `insert_before_markers`, `insert_1_both`, `adjust_point`, `del_range`, `del_range_1/_byte/_both/_2`, `modify_text`, `prepare_to_modify_buffer` (read-only check) | marker adjustment, intervals, undo, change hooks, compositions, quit during gap motion, `copy_text`, `insert_from_string/buffer`, `replace_range` | test_fm_insdel (own; no upstream insdel-tests.el) | partial |
| fm_character | character.c, character.h | encoding table, `char_string`, `multibyte_length`, `string_char_and_length`, `raw_prev_char_len`, raw-byte (eight-bit) conversions, `multibyte_chars_in_text`, `parse_str_as_multibyte`, `str_as_multibyte`, `count_size_as_multibyte`, `str_to_multibyte`, `str_as_unibyte`, `char_resolve_modifier_mask`, small predicates | `chars_in_text` (needs current buffer → fm_buffer); `char_width`/`string-width` and `alphabeticp` etc. (need char-tables → display phase); `translate_char`; Lisp-level DEFUNs | test_fm_character (own; character-tests.el only tests `string-width`, deferred) | done |

## Conventions learned while porting

- Bytes are default-kind `character` elements: build them with `char(i)` and
  read them with `ichar(c, kind=int32)`. Not `achar`, whose result outside
  0..127 is processor-dependent (gfortran warns).
- Upstream's unchecked fast paths (`check = false`, no end pointer) are not
  ported; everything bound-checks against `len(str)`.
- Functions that must return two values become `pure subroutine`s (a pure
  function may not have `intent(out)` arguments).
- fpm tree-shakes the library: a module is only compiled once something
  (a test or the app) uses it, so register a module's test suite at once.
- Registry slots are reused after kill; any cache keyed by slot must also
  check `buffers(i)%serial` (see the regression test in test_fm_insdel).
- Text uses Emacs's internal encoding (UTF-8 extended to 22-bit codes, raw
  bytes as C0/C1 two-byte forms), so files with invalid UTF-8 round-trip.

## Open questions

Decisions the porting work is waiting on, or defaults taken provisionally.
Each lists the default being used meanwhile.

1. **Merging.** May Claude merge its own PRs once CI is green, or should
   every PR wait for review? *Default: PRs stay open; later work is stacked
   on the previous branch.*
2. **Value model.** Text properties, keymaps and buffer-local variables are
   Lisp data upstream. Without Elisp we still need symbols and a small value
   type. *Proposed default: interned symbols (`fm_symbol`, after
   `lread.c:intern`) referenced by handle, and a tagged `value_type`
   holding nil / integer / symbol / string / list-of-values, with explicit
   free. To be confirmed before Phase 1 step 5.*
3. **Error-code naming.** Error codes are `FM_<SYMBOL>` parameters, except
   the root `error` condition, which is `FM_GENERIC_ERROR` because Fortran
   names are case-insensitive and `FM_ERROR` collides with module
   `fm_error`. *Default: keep as is.*
4. **Buffer internals visibility.** `fm_buffer` exposes `buffers(:)` and
   the current slot `cur` (read-only outside the module) so fm_insdel and
   fm_marker can work like upstream's buffer.h macros. *Default: keep;
   higher layers use the procedures only.*
5. **Upstream LLM policy (FYI).** The emacs-31.1 tree contains an
   `AGENTS.md` stating that GNU Emacs does not accept LLM-generated
   contributions. That governs patches sent to Emacs, not this separate
   GPL project, but it means Fortmacs code (largely Claude-written) should
   never be offered upstream. *Default: read upstream as reference only;
   never propose changes to it.*

## Session log

Resume point for the next working session, newest first.

- 2026-10-20 — Phase 1 step 3 (`fm_buffer`, `fm_insdel`, conversion half of `fm_marker`) on `phase1-buffer`, stacked on PR #3. Next: step 4, marker objects.
- 2026-10-20 — Phase 1 step 2 (`fm_character`) on `phase1-character` (PR #3).
- 2026-10-20 — Phase 1 step 1 (`fm_error`, `fm_handle`) on `phase1-error-handle` (PR #2), stacked on PR #1.
- 2026-10-20 — Phase 0 scaffold on branch `phase0-scaffold` (PR #1, CI green).
