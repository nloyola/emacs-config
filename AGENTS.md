# AGENTS.md

## Purpose

This repository contains a personal Emacs configuration. Changes should preserve fast startup, keep related settings grouped, and avoid editing generated package/cache state unless the task explicitly requires it.

## Repository Layout

- `init.el`: startup bootstrap. It installs Elpaca, defines helpers, and loads every file in `config/` in lexicographic order. Packages are managed by Elpaca; `package.el` is disabled.
- `config/*.org`: authoritative configuration source. Emacs Lisp lives inside `#+BEGIN_SRC emacs-lisp` blocks and is executed directly at startup.
- `config/00-usage.org`: documentation only - a key binding reference. It contains no source blocks, and is explicitly not the source of truth for implementation.
- `lisp/*.el`: custom local libraries, themes, and project helpers.
- `test/*.el`: ERT tests covering the `lisp/` libraries.
- `snippets/`: Yasnippet definitions organized by major mode.
- `users/`: per-host overrides. `99-host.org` loads the subdirectory named after `hostname -s`, so these are keyed by machine, not by login name.
- `css/`: vendored stylesheet for the GitHub-styled Markdown preview (`nl/markdown-preview`).
- `disabled/`: retired config kept for reference. Outside `config/`, so it is never loaded.

## Edit Rules

- Prefer editing `config/*.org` for behavior changes that belong in the main configuration.
- Keep new config in the most relevant numbered Org file so load order stays intentional.
- Put reusable Emacs Lisp in `lisp/*.el` when it would be awkward to maintain inline in an Org block.
- Update `snippets/` only for snippet changes; do not mix snippet work into unrelated config edits.
- Treat `users/` as local overrides. Avoid broad changes there unless the task is explicitly machine-specific.
- When adding, removing, or rebinding a global key, update `config/00-usage.org` in the same change. It is maintained by hand and drifts silently otherwise.

## Execution Model

- `init.el` calls `load-config-org`, which evaluates all `config/*.org` files in sorted order.
- Org source blocks tagged with `:tangle no` are skipped by the loader.
- Top-level headings in `config/*.org` are meaningful for startup logging and error reporting. Keep them clear.

## Avoid Editing

These directories/files are local state or generated artifacts and should normally be left alone:

- `elpaca/`
- `elpa/`
- `elpa.old/`
- `etc/`
- `eln-cache/`
- `autosave/`
- `lsp-cache/`
- `workspace/`
- `tree-sitter/`
- `transient/`
- `.cache/`
- `tmp/`

## Validation

- For Org config edits, check block structure carefully so `#+BEGIN_SRC` / `#+END_SRC` pairs remain balanced.
- For Lisp edits, prefer small focused changes and keep lexical binding intact where already used.
- When changing anything under `lisp/`, run the test suite from the repository root:

  ```sh
  emacs -Q --batch -L lisp -l test/nl-nordita-tests.el -f ert-run-tests-batch-and-exit
  ```

- If further validation is needed, use a non-interactive Emacs startup check such as loading `init.el` in batch mode, provided the task warrants it.

## Style

- Match the existing Emacs Lisp style in surrounding files.
- Preserve the numbered organization of `config/*.org`.
- Keep comments brief and useful; most sections already self-document through headings.
