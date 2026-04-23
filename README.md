# Emacs Configuration

Personal Emacs configuration. Org-driven, modular, and tuned for fast startup.

## Layout

- `init.el` — startup bootstrap; initializes packages and loads every file in `config/` in lexicographic order.
- `config/*.org` — authoritative configuration. Emacs Lisp lives in `#+BEGIN_SRC emacs-lisp` blocks and is evaluated directly at startup (no tangle step).
- `lisp/*.el` — local libraries, themes, and per-language project helpers (`nl-*-project.el`).
- `snippets/` — Yasnippet definitions by major mode.
- `users/` — machine or user-specific overrides.

## Install

```sh
git clone <this-repo> ~/.emacs.d
emacs
```

On first launch, packages are installed from the configured ELPA/MELPA archives.

## Editing Configuration

Prefer editing the numbered files in `config/`:

| File | Area |
|------|------|
| `10-basic.org` | Core settings |
| `20-fonts.org` | Fonts |
| `30-hooks.org` | Global hooks |
| `40-functions.org` | Helper functions |
| `50-theme.org` | Theme |
| `60-builtin.org` | Built-in packages |
| `70-completion.org` | Completion stack |
| `71-editing.org` | Editing |
| `72-ui.org` | UI |
| `73-dev.org` | Development tools |
| `74-git.org` | Git / Magit |
| `75-ai.org` | AI integrations |
| `76-shells.org` | Shells |
| `80-lang-web.org` | Web languages |
| `81-lang-backend.org` | Backend languages |
| `82-text.org` | Text modes |
| `90-org.org` | Org-mode |
| `99-host.org` | Host-local |

Blocks tagged `:tangle no` are skipped by the loader. See `AGENTS.md` for full edit rules.

## Generated / Local State

The following are ignored or machine-local and should not be edited by hand:
`elpa/`, `elpa.old/`, `eln-cache/`, `autosave/`, `lsp-cache/`, `workspace/`, `tree-sitter/`, `transient/`, `tmp/`.
