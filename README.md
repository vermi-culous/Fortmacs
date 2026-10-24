<h1 align="center"><img src="docs/logo/logo.svg" alt="Fortmacs" width="408"></h1>

> **Not usable yet.** This is an early, experimental port; it builds and
> runs, but most of Emacs is missing.

GNU Emacs 31.1 rewritten in modern Fortran (2018+), for Linux on x86-64 and
aarch64. There is no Elisp: your configuration is a Fortran module compiled
into the binary, and every rebuild is a numbered generation you can roll
back to, as in Guix.

<!-- progress:start -->
**Progress:** early porting. Fortmacs runs as a bare-bones terminal editor; most of Emacs is still
missing. See [PORTING.md](PORTING.md) for the roadmap and what works so far.

<table>
<tr><th rowspan="2">Total</th><th colspan="3">Emacs</th><th colspan="4">Packages</th></tr>
<tr><th>All</th><th>C</th><th>Elisp</th><th>All</th><th>GNU<br>ELPA</th><th>NonGNU<br>ELPA</th><th>MELPA</th></tr>
<tr><td align="center"><img src="docs/progress/total.svg" width="28" height="120" alt="Total: ≆2.39%"></td><td align="center"><img src="docs/progress/emacs.svg" width="28" height="120" alt="All: ≆2.69%"></td><td align="center"><img src="docs/progress/c.svg" width="28" height="120" alt="C: ≆9.02%"></td><td align="center"><img src="docs/progress/elisp.svg" width="28" height="120" alt="Elisp: ≆1.57%"></td><td align="center"><img src="docs/progress/packages.svg" width="28" height="120" alt="All: 0.00%"></td><td align="center"><img src="docs/progress/gnu-elpa.svg" width="28" height="120" alt="GNU ELPA: 0.00%"></td><td align="center"><img src="docs/progress/nongnu-elpa.svg" width="28" height="120" alt="NonGNU ELPA: 0.00%"></td><td align="center"><img src="docs/progress/melpa.svg" width="28" height="120" alt="MELPA: 0.00%"></td></tr>
<tr><td align="center">1,530<br>of 63,999</td><td align="center">1,530<br>of 56,864</td><td align="center">771<br>of 8,547</td><td align="center">759<br>of 48,317</td><td align="center">0<br>of 7,135</td><td align="center">0<br>of 509</td><td align="center">0<br>of 293</td><td align="center">0<br>of 6,333</td></tr>
<tr><td align="center">≆2.39%</td><td align="center">≆2.69%</td><td align="center">≆9.02%</td><td align="center">≆1.57%</td><td align="center">0.00%</td><td align="center">0.00%</td><td align="center">0.00%</td><td align="center">0.00%</td></tr>
</table>

Total is Emacs (its C functions and Elisp definitions) and the packages
together. A rough estimate from `tools/progress.sh`.
<!-- progress:end -->

The project is funded entirely by one person. Every agent run costs tokens,
so token usage and its cost set the pace.

## Building

Requires gfortran ≥ 13 and [fpm](https://fpm.fortran-lang.org); development
also needs [fprettify](https://github.com/fortran-lang/fprettify) 0.3.7 and
[Fortitude](https://fortitude.readthedocs.io) 0.9.2.

```sh
fpm build
fpm run -- FILE         # edit FILE (C-x C-s saves, C-x C-c exits)
tools/check.sh          # format, lint, debug build and tests, as CI runs them
tools/check.sh --fix    # reformat and apply safe lint fixes
```

CI runs the same check on Ubuntu and Fedora (x86-64 and aarch64) and Arch
Linux (x86-64) for every pull request and after each merge, and on Guix
weekly.

## Configuring

Write your configuration in `~/.config/fortmacs/init.f90`
([docs/init.f90](docs/init.f90) is an example): a module `user_config` whose
`configure` subroutine runs at startup. Settings are variables
(`tab_width = 4`), built-in commands are objects
(`commands%upcase_region%disabled = .false.`), and keys are bound with
`keymap_global_set("C-c d", "my_command", status)`.

```sh
fpm install --profile release   # fortmacs, its library and modules, in ~/.local
fortmacs --recompile            # build init.f90 into a new generation
fortmacs -q                     # start without your configuration
fortmacs --list-generations     # every build, the current one marked
fortmacs --rollback             # back to the generation before
fortmacs --switch-generation 3  # or to any generation
```

A failed build shows the compiler's errors and leaves your last good build
running. Each generation is kept with the `init.f90` it was built from in
`~/.local/state/fortmacs/generations`.

## Why Fortran?

It started as a fun way to learn the language, and Fortran turned out to
suit an editor well:

1. **Real parallel threads.** Emacs runs one thread at a time under a global
   lock. Fortmacs keeps one thread for buffers and the screen, as Emacs does,
   and will run heavy work (searching large files, file I/O, syntax parsing,
   language servers) on other cores, each worker with its own copy of the
   data. Not implemented yet; see "Threads" in [PORTING.md](PORTING.md).
2. **Fast native code.** Compiled ahead of time by optimizing compilers,
   with no garbage-collection pauses (objects are freed explicitly) and no
   init files to evaluate at startup.
3. **Errors caught at compile time.** Static types, `implicit none` and
   argument intents turn many mistakes into compile errors, in your
   configuration too.
4. **A stable standard.** Fortran is an ISO standard with many independent
   compilers; code from decades ago still compiles.
5. **Syntax.** Blocks say where they end (`end if`, `end do`) instead of
   ending where parentheses balance.
   [Compare the two side by side](https://vermi-culous.github.io/Fortmacs/syntax.html).
   That's a preference, not a verdict on Elisp.

6. **Help that speaks Fortran.** Emacs is at its best as an environment for
   the language it's written in: it documents every function and variable,
   shows their current values and jumps to their source. Fortmacs aims to
   bring that to Fortran: help shows Fortran names, values and source files,
   so exploring and configuring the editor feels like working in Fortran.
   A language server such as fortls supports editing Fortran code; this is
   about the editor itself being inspectable in Fortran.

The trade-off: you can't evaluate code in a running session. Changing the
configuration means rebuilding.

## Differences from Emacs

Fortmacs follows Emacs 31.1 exactly, except where being Fortran requires
otherwise:

- Names use underscores: `M-x find_file`, `C-h v fill_column`,
  `text_mode_hook`. Keys and mode-line text are unchanged.
- There is no Lisp to evaluate at run time: no `M-:`, `eval-buffer` or
  `ielm`, and `*scratch*` starts in Fundamental mode.
- Help shows Fortran values (`.true.`, `8`, `"text"`, named constants) and
  names the Fortran file a definition lives in.
- Configuration and packages are Fortran modules compiled into the binary;
  commands that save settings write Fortran into `init.f90`.

## Why AI assistance?

The progress block counts tens of thousands of definitions to rewrite, in
Emacs's C as well as its Elisp, plus over 7,000 packages (509 in GNU ELPA,
293 in NonGNU ELPA, 6,333 in MELPA), every one written in Elisp. Package
maintainers shouldn't be expected to port their work to an experimental
niche project, and one person or a handful of porters could never keep up;
it would become an endless race over which packages matter most. Emacs
already works, so Emacs in Fortran would likely never happen. This is for
fun. Let's see where it goes.

Telling an assistant to "rewrite Emacs in Fortran" doesn't produce a working
program by itself: a faithful port takes constant direction. Left alone, it
drifted toward Lisp written in Fortran:

- Configuration calls like `call put("upcase-region", "disabled", .false.,
  status)` with helper functions such as `nil()` and `sym()`, which became
  plain component assignment: `commands%upcase_region%disabled = .false.`
- Mode hooks reached through `call major_modes%text_mode%hook%add(...)`,
  which made you know whether a mode is major or minor; they became objects
  named after their Emacs variables: `call text_mode_hook%add(my_procedure)`.

What an assistant changes:

1. **It reads all three languages.** It ports an upstream C or Elisp
   function to Fortran and cites the source, without someone learning each
   codebase first.
2. **It handles the mechanical bulk,** applying the project's conventions
   consistently across thousands of functions.
3. **It ports the tests too,** so each port is checked against Emacs's own
   expectations.
4. **It works in parallel.** Up to eight agents port separate modules at
   once; one lead session reviews and combines their work.
5. **It is faster.** About 65,000 lines of Fortran and tests, not
   counting generated tables, in the first three days (roughly 850 an
   hour), where a person porting by hand might manage a few hundred a day.
   Review steers it through suggestions and directions; what limits the
   pace is token usage and its cost.
6. **The safeguards stay human.** Every change is built, tested and checked
   for memory leaks in CI on two architectures, and a human reviews and
   merges every pull request.

GNU Emacs does not accept AI-generated contributions. Fortmacs respects
that: upstream is a read-only reference, and nothing from this project is
sent back to it.

## Reference source

Ports follow GNU Emacs tag `emacs-31.1`. `tools/fetch-upstream.sh` places a
verified shallow checkout in `upstream/emacs/` (git-ignored).

## License

GPLv3 or later, matching upstream; see [LICENSE](LICENSE). Files ported from
GNU Emacs keep the Free Software Foundation copyright notice alongside ours.
