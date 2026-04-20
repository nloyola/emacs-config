# AGENTS.md

## Purpose

This repository contains a personal Emacs configuration. Changes should preserve fast startup, keep related settings grouped, and avoid editing generated package/cache state unless the task explicitly requires it.

## Repository Layout

- `init.el`: startup bootstrap. It initializes packages, defines helpers, and loads every file in `config/` in lexicographic order.
- `config/*.org`: authoritative configuration source. Emacs Lisp lives inside `#+BEGIN_SRC emacs-lisp` blocks and is executed directly at startup.
- `config.org`: legacy monolithic Org file being phased out. Do not add new configuration there unless the task is explicitly about migration or cleanup.
- `lisp/*.el`: custom local libraries, themes, and project helpers.
- `snippets/`: Yasnippet definitions organized by major mode.
- `users/`: machine or user-specific support files.

## Edit Rules

- Prefer editing `config/*.org` for behavior changes that belong in the main configuration.
- Keep new config in the most relevant numbered Org file so load order stays intentional.
- Do not add new settings to `config.org`; move or maintain behavior in `config/` instead.
- Put reusable Emacs Lisp in `lisp/*.el` when it would be awkward to maintain inline in an Org block.
- Update `snippets/` only for snippet changes; do not mix snippet work into unrelated config edits.
- Treat `users/` as local overrides. Avoid broad changes there unless the task is explicitly machine-specific.

## Execution Model

- `init.el` calls `load-config-org`, which evaluates all `config/*.org` files in sorted order.
- Org source blocks tagged with `:tangle no` are skipped by the loader.
- Top-level headings in `config/*.org` are meaningful for startup logging and error reporting. Keep them clear.

## Avoid Editing

These directories/files are local state or generated artifacts and should normally be left alone:

- `elpa/`
- `elpa.old/`
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
- If validation is needed, use a non-interactive Emacs startup check such as loading `init.el` in batch mode, provided the task warrants it.

## Style

- Match the existing Emacs Lisp style in surrounding files.
- Preserve the numbered organization of `config/*.org`.
- Keep comments brief and useful; most sections already self-document through headings.
