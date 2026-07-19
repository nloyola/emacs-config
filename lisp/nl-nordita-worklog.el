;;; nl-nordita-worklog.el --- Monthly Nordita work report -*- lexical-binding: t; -*-

;;; Commentary:
;; Generates the monthly "what did you work on" report for Nordita from the
;; norweb-2021 git history, embeds it in the "Nordita Hours" org-roam note
;; (~/Sync/RoamNotes/20240801182901-nordita_hours.org), and prepares it for
;; email as styled HTML.
;;
;; Two commands, kept separate so the prose can be proofread in between:
;;
;; - `nl/nordita-generate-work-summary': runs `claude -p' in the norweb repo
;;   (asynchronously - it takes 30-90 seconds) and inserts the resulting
;;   `** Week' subtrees under the month's heading.  Refuses to run if the month
;;   already has them, so hand-edited prose is never silently overwritten.
;;
;; - `nl/nordita-email-work-summary': exports those subtrees to HTML styled like
;;   a rendered GitHub issue, archives it to Dropbox, opens it, and opens a
;;   Gmail compose tab.  Gmail's compose URL carries plain text only, so the
;;   report is pasted from the browser rather than prefilled - copying rendered
;;   content preserves the styling.
;;
;; The content rules (what to summarize, tone, week grouping) live in the
;; `org-weekly-git-summary' skill, not here.  This file only drives it.
;;
;; The note's src blocks are one-line calls into these functions; this file is
;; loaded lazily (see the `use-package' in config/90-org.org).

;;; Code:

(require 'org)
(require 'ox-html)
(require 'subr-x)
(require 'url-util)
(require 'nl-nordita-timesheet)

;;;; Configuration -----------------------------------------------------------

(defvar nl/nordita-worklog-repo (expand-file-name "~/src/nordita/norweb-2021/")
  "Git repository the monthly work summary is generated from.")

(defvar nl/nordita-worklog-claude (expand-file-name "~/.local/bin/claude")
  "Path to the claude CLI that generates the summary.")

(defvar nl/nordita-worklog-recipient "elizabeth.yang@su.se"
  "Primary To: address for the monthly work report.")

(defvar nl/nordita-worklog-cc
  "mikael.fogelstrom@su.se,hans.muhlen@su.se"
  "Cc: addresses for the monthly work report.")

(defvar nl/nordita-worklog-greeting "Hi Elizabeth,"
  "Opening line of the emailed report.")

(defvar nl/nordita-worklog-prompt
  "Use the org-weekly-git-summary skill in embedded mode for %s."
  "Prompt handed to `claude -p'.  %s is the month, e.g. \"July 2026\".
Embedded mode is defined in the skill: stdout only, no file header, and
headings demoted so weeks are `**' and topic groups `***'.")

;; The archive directory is deliberately not redeclared here:
;; `nl/nordita-timesheet-dropbox' already names it.  Two variables for one
;; directory drift apart on the next folder move.

(defvar nl/nordita-worklog-css
  "body { font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", \"Noto Sans\", Helvetica, Arial, sans-serif; font-size: 16px; line-height: 1.5; color: #1f2328; max-width: 900px; margin: 0 auto; padding: 32px 24px; }
h1.title { font-size: 2em; font-weight: 600; text-align: left; padding-bottom: .3em; margin: 0 0 16px; border-bottom: 1px solid #d1d9e0; }
h2 { font-size: 1.5em; font-weight: 600; padding-bottom: .3em; margin: 24px 0 16px; border-bottom: 1px solid #d1d9e0; }
h3 { font-size: 1.25em; font-weight: 600; margin: 24px 0 16px; }
p { margin: 0 0 16px; }
ul, ol { margin: 0 0 16px; padding-left: 2em; }
li + li { margin-top: .25em; }
a { color: #0969da; text-decoration: none; }
hr { height: 1px; margin: 32px 0 24px; background: #d1d9e0; border: 0; }
code { background: #eff1f3; border-radius: 6px; padding: .2em .4em; font-size: 85%; font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }
.outline-2, .outline-3 { margin: 0; }"
  "CSS inlined into the emailed HTML: GitHub's rendered-markdown look.
Kept here rather than in a file because it has to be inlined into the export
anyway, and email clients strip external stylesheets.")

;;;; Reading the note ---------------------------------------------------------

(defun nl/nordita-worklog--subtree-end (marker)
  "End position of the subtree at MARKER, in MARKER's buffer."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (org-end-of-subtree t t))))

(defun nl/nordita-worklog--month-level (marker)
  "Outline level of the month heading at MARKER.
1 for a current-year month at the top level, 2 for one grouped under a
`* YEAR' parent.  Everything else here is expressed relative to it, so the two
layouts `nl/nordita-timesheet--insert-section-for' can produce behave alike."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (org-outline-level))))

(defun nl/nordita-worklog--week-regexp (level)
  "Regexp matching a week heading nested directly under a month at LEVEL."
  (format "^\\*\\{%d\\} Week " (1+ level)))

(defun nl/nordita-worklog--week-start (marker)
  "Position of the first week heading under MARKER, or nil."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (let ((end (nl/nordita-worklog--subtree-end marker))
           (regexp (nl/nordita-worklog--week-regexp
                    (nl/nordita-worklog--month-level marker))))
       (goto-char marker)
       (when (re-search-forward regexp end t)
         (match-beginning 0))))))

(defun nl/nordita-worklog--week-text (marker)
  "Org text of the \"** Week\" subtrees under MARKER, promoted one level.
Returns nil when the month has none.

Only the week subtrees are taken.  The hours table, its `#+TBLFM' total and the
`C-c C-c' src blocks all sit above the first week heading, and those are
timesheet internals that must not reach the report's recipients - excluding them
by construction is safer than filtering them out afterwards."
  (let ((beg (nl/nordita-worklog--week-start marker)))
    (when beg
      (with-current-buffer (marker-buffer marker)
        (org-with-wide-buffer
         (let ((end (nl/nordita-worklog--subtree-end marker)))
           (nl/nordita-worklog--promote
            (buffer-substring-no-properties beg end)
            (nl/nordita-worklog--month-level marker))))))))

(defun nl/nordita-worklog--promote (text n)
  "Strip N leading stars from every heading line in TEXT.
Called with the month's level, so weeks end up at `*' and topic groups at `**'
whether the month sat at the top level or under a year parent, and the export
renders weeks as h2 and topics as h3 under the title's h1."
  (if (<= n 0)
      text
    (replace-regexp-in-string (format "^\\*\\{%d\\}\\(\\*+ \\)" n) "\\1" text)))

(defun nl/nordita-worklog--demote (text n)
  "Add N stars to every heading line in TEXT."
  (if (<= n 0)
      text
    (replace-regexp-in-string "^\\(\\*+ \\)" (concat (make-string n ?*) "\\1")
                              text)))

(defun nl/nordita-worklog--read-month ()
  "Prompt for a month with a section in the current buffer."
  (let ((months (mapcar #'car (nl/nordita-timesheet--table-months))))
    (unless months (user-error "No month sections found in this buffer"))
    (completing-read (format "Month (default %s): " (car months))
                     months nil t nil nil (car months))))

;;;; Generating ---------------------------------------------------------------

(defun nl/nordita-worklog--strip-ansi (text)
  "Remove ANSI terminal escape sequences from TEXT.
The claude CLI emits cursor-control codes (ESC[?25h and friends) when it is
spawned from Emacs, though not when its output is redirected to a file from a
shell.  Left in, they end up as literal junk in the note and the report."
  (replace-regexp-in-string
   "\e\\][^\a\e]*\\(?:\a\\|\e\\\\\\)\\|\e\\[[0-9;?]*[A-Za-z]\\|\e[=>]" "" text))

(defun nl/nordita-worklog--strip-fence (text)
  "Remove a trailing markdown code fence from TEXT.
Embedded mode asks for bare org, but the model sometimes wraps the whole reply
in a ```org block regardless.  The opening fence is already discarded by taking
everything from the first week heading; the closing one sits at the very end,
where nothing else would have removed it, and lands in the note as a literal
``` line after the last paragraph."
  (string-trim-right
   (replace-regexp-in-string "\n[ \t]*```[a-zA-Z]*[ \t]*\\'" "" text)))

(defun nl/nordita-worklog--clean (output)
  "Org text from claude's OUTPUT, or nil if it contains no summary at all.

Takes everything from the first \"** Week\" heading onward, less any trailing
code fence.  Anything before it - a stray file header, or a sentence of preamble
the model added despite embedded mode - is dropped.  Generation is
nondeterministic and takes a minute, so throwing away an otherwise-good run over
a preamble just means paying for the month twice; the safety property that
matters is that output with no \"** Week\" heading at all never reaches the note."
  (let ((output (nl/nordita-worklog--strip-ansi output)))
    (when (string-match "^\\*\\* Week " output)
      (concat (nl/nordita-worklog--strip-fence
               (string-trim-right (substring output (match-beginning 0))))
              "\n"))))

(defun nl/nordita-worklog--preamble (output)
  "Text OUTPUT carried before its first \"** Week\" heading, ignoring keyword
lines and a wrapping code fence, or nil if there was none.  Surfaced rather than
silently discarded: a preamble is usually harmless, but it is also where the
model would put a warning about the data it found.

An opening ```org line is not a preamble but the other half of the fence
`nl/nordita-worklog--strip-fence' removes, so it does not count here either -
otherwise every fenced reply would report a preamble that needs no attention,
and the warning would stop meaning anything."
  (let* ((output (nl/nordita-worklog--strip-ansi output))
         (start  (string-match "^\\*\\* Week " output)))
    (when start
      (let ((head (string-trim
                   (replace-regexp-in-string
                    "^\\(?:#\\+\\|```\\)[^\n]*$" "" (substring output 0 start)))))
        (unless (string-empty-p head) head)))))

(defun nl/nordita-worklog--insert (marker text)
  "Insert TEXT at the end of the subtree at MARKER.
The skill emits weeks at `**', which nests correctly only under a top-level
month.  TEXT is demoted to the month's own depth first: inserted at `**' under a
month that is itself `**', the weeks would land as sibling months instead of
children, leaving the month with no summary the rest of this file could find."
  (let ((text (nl/nordita-worklog--demote
               text (1- (nl/nordita-worklog--month-level marker)))))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char (nl/nordita-worklog--subtree-end marker))
       (unless (bolp) (insert "\n"))
       (insert "\n" text)))))

;;;###autoload
(defun nl/nordita-generate-work-summary (month)
  "Generate MONTH's work summary from the norweb repo and insert it in the note.
MONTH is a \"YYYY-MM\" string.  Runs `claude -p' asynchronously (30-90 seconds)
against `nl/nordita-worklog-repo'; the summary is inserted when it finishes.

Refuses to run if MONTH already has \"** Week\" sections - delete them first to
regenerate.  On failure the output is left in *nordita-worklog* and the note is
not touched."
  (interactive (list (nl/nordita-worklog--read-month)))
  (unless (file-executable-p nl/nordita-worklog-claude)
    (user-error "No claude executable at %s" nl/nordita-worklog-claude))
  (let* ((marker (nl/nordita-timesheet--marker-for month))
         (pretty (nl/nordita-timesheet--month-name month))
         (buf    (get-buffer-create "*nordita-worklog*")))
    (when (nl/nordita-worklog--week-start marker)
      (user-error "%s already has generated Week sections - delete them to regenerate"
                  pretty))
    (with-current-buffer buf (erase-buffer))
    (let ((default-directory nl/nordita-worklog-repo)
          ;; Discourage the CLI from emitting colour and cursor control in the
          ;; first place.  `nl/nordita-worklog--strip-ansi' is what actually
          ;; guarantees a clean result; this just keeps the buffer readable.
          (process-environment (append '("TERM=dumb" "NO_COLOR=1")
                                       process-environment)))
      (make-process
       :name "nordita-worklog"
       :buffer buf
       :noquery t
       ;; `git fetch' is allowed alongside `git log' because the skill reports
       ;; `origin/development': the local remote-tracking ref is only as current
       ;; as the last fetch, so without it a month's summary quietly loses
       ;; whatever merged after that.  Both are read-only on the repo.
       :command (list nl/nordita-worklog-claude
                      "-p" (format nl/nordita-worklog-prompt pretty)
                      "--allowedTools" "Bash(git log:*)" "Bash(git fetch:*)"
                      "Skill")
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let* ((code (process-exit-status proc))
                  (out  (with-current-buffer buf (buffer-string)))
                  (text (nl/nordita-worklog--clean out)))
             (cond
              ((not (eq code 0))
               (display-buffer buf)
               (message "Work summary for %s failed (exit %s) - see *nordita-worklog*"
                        pretty code))
              ((null text)
               (display-buffer buf)
               (message "Work summary for %s came back with no Week sections - see *nordita-worklog*"
                        pretty))
              (t
               (nl/nordita-worklog--insert marker text)
               (if (nl/nordita-worklog--preamble out)
                   (progn
                     (display-buffer buf)
                     (message "Work summary for %s inserted, but the model added a preamble - check *nordita-worklog*"
                              pretty))
                 (message "Work summary for %s inserted - proofread, then email it."
                          pretty)))))))))
    (message "Generating %s work summary from the norweb repo (30-90s)..." pretty)))

;;;; Emailing -----------------------------------------------------------------

(defun nl/nordita-worklog--export (month body)
  "Export BODY as MONTH's report HTML and return the file name."
  (let ((file   (expand-file-name (format "work_items_%s.html" month)
                                  nl/nordita-timesheet-dropbox))
        (pretty (nl/nordita-timesheet--month-name month)))
    (with-temp-buffer
      (insert (format "#+TITLE: Nordita work report - %s\n" pretty))
      (insert "#+OPTIONS: toc:nil html-postamble:nil num:nil ^:{}\n")
      ;; No #+SETUPFILE: the note's readtheorg theme pulls external CSS and JS,
      ;; which email clients strip.  Multiple #+HTML_HEAD lines concatenate.
      (insert "#+HTML_HEAD: <style>\n")
      (dolist (line (split-string nl/nordita-worklog-css "\n" t))
        (insert "#+HTML_HEAD: " line "\n"))
      (insert "#+HTML_HEAD: </style>\n\n")
      ;; Greeting and sign-off are part of the export so the whole page is the
      ;; email body and one paste suffices.
      (insert nl/nordita-worklog-greeting "\n\n")
      (insert (format "Here is my work report for %s.\n\n" pretty))
      (insert body)
      ;; A rule before the sign-off: trailing text lands inside the last topic
      ;; section, so without it "Best regards" reads as part of that section.
      (insert "\n-----\n\nBest regards,\n\nNelson\n")
      (org-mode)
      (let ((org-html-head-include-default-style nil)
            (org-html-head-include-scripts nil)
            (org-html-validation-link nil)
            (org-export-use-babel nil))
        (org-export-to-file 'html file)))
    file))

;;;###autoload
(defun nl/nordita-email-work-summary (month)
  "Export MONTH's work report to HTML and open a Gmail compose tab for it.
MONTH is a \"YYYY-MM\" string.  The HTML is archived to
`nl/nordita-timesheet-dropbox' as work_items_MONTH.html - a record of exactly
what was sent - and opened in the browser.

Gmail's compose URL carries plain text only, so the compose tab comes up with
the recipients and subject filled in but an empty body: copy the whole opened
page and paste it in.  Copying rendered content preserves the styling."
  (interactive (list (nl/nordita-worklog--read-month)))
  (let* ((marker  (nl/nordita-timesheet--marker-for month))
         (pretty  (nl/nordita-timesheet--month-name month))
         (body    (nl/nordita-worklog--week-text marker))
         (subject (format "Nordita work report - %s" pretty)))
    (unless body
      (user-error "%s has no Week sections - generate the summary first" pretty))
    (let ((file (nl/nordita-worklog--export month body)))
      (call-process "xdg-open" nil 0 nil file)
      (browse-url
       (concat "https://mail.google.com/mail/?view=cm&fs=1"
               "&to=" (url-hexify-string nl/nordita-worklog-recipient)
               "&cc=" (url-hexify-string nl/nordita-worklog-cc)
               "&su=" (url-hexify-string subject)))
      (message "Report %s written to %s - copy the opened page into the compose tab."
               month file))))

(provide 'nl-nordita-worklog)
;;; nl-nordita-worklog.el ends here
