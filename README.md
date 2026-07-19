# Emacs Configuration

Personal Emacs configuration. Org-driven, modular, and tuned for fast startup.

## Layout

- `init.el` - startup bootstrap; installs Elpaca and loads every file in `config/` in lexicographic order.
- `config/*.org` - authoritative configuration. Emacs Lisp lives in `#+BEGIN_SRC emacs-lisp` blocks and is evaluated directly at startup (no tangle step).
- `lisp/*.el` - local libraries, themes, and per-language project helpers (`nl-*-project.el`).
- `test/*.el` - ERT tests for the `lisp/` libraries.
- `snippets/` - Yasnippet definitions by major mode.
- `users/` - per-host overrides, in a directory named after `hostname -s`.
- `css/` - vendored stylesheet for the GitHub-styled Markdown preview.
- `disabled/` - retired config kept for reference; not loaded.

## Install

```sh
git clone <this-repo> ~/.emacs.d
emacs
```

Packages are managed by [Elpaca](https://github.com/progfolio/elpaca), which
`init.el` bootstraps by cloning it into `elpaca/` on first launch; `package.el`
is disabled (`package-enable-at-startup nil`). Expect the first start to be slow
while every package is cloned and built.

## Editing Configuration

Prefer editing the numbered files in `config/`:

| File | Area |
|------|------|
| `00-usage.org` | Key binding reference (documentation only) |
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

## Tests

The `lisp/` libraries have ERT tests. Run them from the repository root:

```sh
emacs -Q --batch -L lisp -l test/nl-nordita-tests.el -f ert-run-tests-batch-and-exit
```

## Generated / Local State

The following are ignored or machine-local and should not be edited by hand:
`elpaca/`, `elpa/`, `elpa.old/`, `eln-cache/`, `autosave/`, `lsp-cache/`, `workspace/`, `tree-sitter/`, `transient/`, `etc/`, `.cache/`, `tmp/`.
