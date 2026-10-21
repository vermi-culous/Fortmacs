# Fortmacs

A rewrite of GNU Emacs 31.1 in modern Fortran (2018+), for Linux on x86-64
and aarch64. Configuration is Fortran too: your `init.f90` is compiled into
the binary, xmonad-style. There is no Elisp.

**Status:** early porting. Fortmacs runs as a bare-bones terminal editor:
open files (`fortmacs FILE` or `C-x C-f`), type, delete, move within lines,
and save (`C-x C-s`, with a `FILE~` backup; `C-x C-w` to write elsewhere).
Most of Emacs is still missing: moving between lines, completion, undo,
search and much more. See [PORTING.md](PORTING.md) for the roadmap and
progress.

<!-- progress:start -->
**Progress:**

<table>
<tr><th rowspan="2">Total</th><th colspan="3">Emacs</th><th colspan="4">Packages</th></tr>
<tr><th>All</th><th>C</th><th>Elisp</th><th>All</th><th>GNU<br>ELPA</th><th>NonGNU<br>ELPA</th><th>MELPA</th></tr>
<tr><td align="center"><img src="docs/progress/total.svg" width="28" height="120" alt="Total: ≆0.75%"></td><td align="center"><img src="docs/progress/emacs.svg" width="28" height="120" alt="All: ≆0.75%"></td><td align="center"><img src="docs/progress/c.svg" width="28" height="120" alt="C: ≆4.75%"></td><td align="center"><img src="docs/progress/elisp.svg" width="28" height="120" alt="Elisp: ≆0.25%"></td><td align="center"><img src="docs/progress/packages.svg" width="28" height="120" alt="All: 0.00%"></td><td align="center"><img src="docs/progress/gnu-elpa.svg" width="28" height="120" alt="GNU ELPA: 0.00%"></td><td align="center"><img src="docs/progress/nongnu-elpa.svg" width="28" height="120" alt="NonGNU ELPA: 0.00%"></td><td align="center"><img src="docs/progress/melpa.svg" width="28" height="120" alt="MELPA: 0.00%"></td></tr>
<tr><td align="center">466<br>of 63,999</td><td align="center">466<br>of 56,864</td><td align="center">413<br>of 8,547</td><td align="center">53<br>of 48,317</td><td align="center">0<br>of 7,135</td><td align="center">0<br>of 509</td><td align="center">0<br>of 293</td><td align="center">0<br>of 6,333</td></tr>
<tr><td align="center">≆0.75%</td><td align="center">≆0.75%</td><td align="center">≆4.75%</td><td align="center">≆0.25%</td><td align="center">0.00%</td><td align="center">0.00%</td><td align="center">0.00%</td><td align="center">0.00%</td></tr>
</table>

Total is Emacs (its C functions and Elisp definitions) and the packages
together. A rough estimate from `tools/progress.sh`.
<!-- progress:end -->

## Building

Requires gfortran ≥ 13, [fpm](https://fpm.fortran-lang.org) and, for
development, [fprettify](https://github.com/fortran-lang/fprettify) 0.3.7
and [Fortitude](https://fortitude.readthedocs.io) 0.9.2.

```sh
fpm build
fpm run -- --version
fpm run -- FILE         # edit FILE in this terminal (C-x C-s saves, C-x C-c exits)
tools/check.sh          # format + lint check, debug build, tests (what CI runs)
tools/check.sh --fix    # reformat and apply safe lint fixes in place
```

CI runs the same check on Ubuntu and Fedora (x86-64 and aarch64) and on
Arch Linux (x86-64), and builds and tests on Guix weekly.

## Reference source

Ports follow GNU Emacs tag `emacs-31.1`. `tools/fetch-upstream.sh` places a
verified shallow checkout in `upstream/emacs/` (git-ignored).

## Why Fortran?

It started as a way to have fun while learning the language. Along the way
it became clear that Fortran brings real benefits over Emacs Lisp:

- **Real parallel threads.** [See below](#real-parallel-threads): this is
  the biggest difference.
- **Compiled ahead of time to fast native code.** Elisp normally runs as
  byte-code. Since Emacs 28 it can be natively compiled through libgccjit,
  which the manual puts at roughly 2.5–5× faster than byte-code. Fortran is
  compiled ahead of time by optimizing compilers built for exactly that
  kind of speed.
- **Less to wait for.** These are design properties, not benchmarks
  (Fortmacs hasn't been measured against Emacs yet):
  - **No garbage-collection pauses.** Emacs stops now and then to collect
    garbage, which is why so many configs raise `gc-cons-threshold`.
    Fortmacs frees objects explicitly, so it has no collector to pause for.
  - **Nothing to load at startup.** Emacs reads and evaluates your init files
    every time it starts; in Fortmacs your config is already compiled into
    the binary.
  - **Native data.** Elisp values are tagged objects, and floating-point
    numbers are allocated on the heap. Fortran works with plain machine
    integers, reals and arrays.
- **Errors caught at compile time.** Fortran is statically typed. With
  `implicit none` and declared argument intents, many mistakes are compile
  errors instead of runtime surprises.
- **A stable, standardized language.** Fortran is an ISO standard (most
  recently Fortran 2023) with many independent compilers: GNU, LLVM Flang,
  Intel, NAG and others. Code from decades ago still compiles. Elisp exists
  only inside GNU Emacs.
- **Configuration is real code.** Your `init.f90` is compiled with the
  editor and type-checked, and every rebuild is kept so you can roll back.
- **Readable syntax.** Lisp's parentheses have devoted fans, but for some
  people the sea of them is easy to get lost in. Every form nests
  inside another, so meaning hides in nesting depth, a block ends wherever
  its parentheses happen to balance, and one misplaced paren silently
  changes what the code does. Fortran spells its structure out in words:
  blocks say where they end (`end if`, `end do`, `end function`),
  variables are declared up front with their types, and code reads top to
  bottom. A function that finds the longest line in the buffer, in Elisp:

  ```elisp
  (defun longest-line ()
    "Return the number of the longest line in the buffer."
    (save-excursion
      (goto-char (point-min))
      (let ((best 0) (best-line 1) (line 1))
        (while (not (eobp))
          (let ((width (- (line-end-position) (line-beginning-position))))
            (when (> width best)
              (setq best width
                    best-line line)))
          (forward-line 1)
          (setq line (1+ line)))
        best-line)))
  ```

  Seven levels deep, ending in a pile of closing parentheses whose
  matches you have to count. In Fortran, written the way Fortmacs's own
  code is:

  ```fortran
  !> The number of the longest line in the buffer.
  integer(int64) function longest_line() result(best_line)
      integer(int64) :: opoint, best, width, line, shortage

      opoint = point()
      best = 0
      best_line = 1
      line = 1
      call goto_char(point_min())
      do while (.not. eobp())
          width = line_end_position() - line_beginning_position()
          if (width > best) then
              best = width
              best_line = line
          end if
          shortage = forward_line(1_int64)
          line = line + 1
      end do
      call goto_char(opoint)
  end function longest_line
  ```

  None of this makes Elisp wrong. Preferring Fortran's syntax is simply a
  preference, and more options can never hurt.

### Real parallel threads

Emacs has threads, but only one of them can run at a time. Every Elisp
thread needs a single global lock, and Emacs passes it on only at
well-defined moments: an explicit `thread-yield`, waiting for keyboard
input or subprocess output, or a mutex or `thread-join`. A long
computation in any thread therefore holds up the whole editor, however
many cores the machine has.

Fortmacs is designed around real parallelism from the start:

- **The editor never waits on background work.** One thread owns the
  buffers and the screen, as in Emacs, so typing, commands and redisplay
  need no locks and stay responsive.
- **Heavy work runs on other cores at the same time.** Searching and
  indexing large files, reading and writing files, parsing for syntax
  highlighting, and talking to language servers run on POSIX threads in
  parallel with editing and with each other.
- **Safe by construction.** Workers get their own copy of the data they
  need and report back through a queue that the command loop checks
  between commands, so no two threads ever touch the same buffer.
- **No second process to manage.** Parallel work stays inside one
  program, in compiled Fortran, instead of being farmed out to another
  Emacs.

Worker threads are not implemented yet; they arrive with the first
feature that needs them (see "Threads" under design decisions in
[PORTING.md](PORTING.md)).

### The trade-off

You can't evaluate code in a running session. Changing the
config means rebuilding (`fortmacs --recompile`).

## Why AI assistance?

As the progress block shows, tens of thousands of definitions need
rewriting in Fortran. That isn't only Elisp but Emacs's internal C as
well. One person would have to master three languages, and even then it
would be a monumental undertaking. Emacs is niche as it is; Emacs in
Fortran would likely never happen.

And that is only Emacs itself. Much of what people love about Emacs lives
in its packages: GNU ELPA, NonGNU ELPA and MELPA hold over 7,000 of them
(509, 293 and 6,333), every one written in Elisp. For Fortmacs to be
usable, everyone's favorite package has to be available too, so they all
need translating as well. It simply isn't realistic.

An AI assistant changes the math:

- **It already reads all three languages.** It can study an upstream C or
  Elisp function and write the Fortran port with the source cited, instead
  of someone learning each codebase first.
- **It handles the mechanical bulk.** Much of the work is faithful,
  repetitive translation that follows a fixed set of conventions. An
  assistant applies them consistently across thousands of functions.
- **It ports the tests too.** Upstream's test suites come over alongside
  the code, so each port is checked against Emacs's own expectations.
- **The safeguards stay human.** Every change is built, tested and checked
  for memory leaks in CI on two architectures, and a human reviews and
  merges every pull request.

GNU Emacs does not accept AI-generated contributions. Fortmacs respects
that: upstream is used as a read-only reference, and nothing from this
project is ever sent back to it.

## License

GPLv3 or later, matching upstream; see [LICENSE](LICENSE). Files ported
from GNU Emacs keep the Free Software Foundation copyright notice alongside
ours.
