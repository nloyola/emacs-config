;;; nl-nordita-timesheet.el --- Nordita timesheet pipeline + month-table generator -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives the monthly Nordita timesheet from the "Nordita Hours" org-roam note
;; (~/Sync/RoamNotes/20240801182901-nordita_hours.org).
;;
;; Two things live here:
;;
;; - The pipeline (`nl/nordita-generate-timesheet' / `nl/nordita-timesheet-generate'):
;;   read a month's hours table from the current buffer, export it to
;;   hours_YYYY-MM.csv, run the `timesheet' script to fill the PDF, copy both to
;;   Dropbox, open the PDF, and open a Gmail compose tab.
;;
;; - The table generator (`nl/nordita-insert-month-by-name' and friends): insert
;;   a fresh month section - heading, a per-month `C-c C-c' block, and a zeroed
;;   weekday table.
;;
;; The note's src blocks are just one-line calls into these functions; this file
;; is loaded lazily (see the `use-package' in config/90-org.org).

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'url-util)

;;;; Configuration -----------------------------------------------------------

(defvar nl/nordita-timesheet-uv (expand-file-name "~/.local/bin/uv")
  "Path to the uv executable that runs the timesheet script.")

(defvar nl/nordita-timesheet-project (expand-file-name "~/src/nordita/timesheet/")
  "Directory of the timesheet Python project (holds the PDF template).")

(defvar nl/nordita-timesheet-dropbox (expand-file-name "~/Dropbox/nordita/timesheets/")
  "Directory the generated CSV and PDF are archived to.")

(defvar nl/nordita-timesheet-recipient "emina.muratspahic@su.se"
  "Primary To: address for the timesheet email.")

(defvar nl/nordita-timesheet-cc "elizabeth.yang@su.se"
  "Cc: address for the timesheet email.")

(defvar nl/nordita-timesheet-holiday-prefix "HOLIDAY: "
  "Prefix for a Swedish holiday in a generated month table's notes column.")

;;;; Reading the note's month tables -----------------------------------------

(defun nl/nordita-timesheet--table-months ()
  "List of (MONTH . MARKER) for YYYY-MM headings whose subtree holds a table.
In document order (newest first, as in the note).  Matches headings at any level
so months nested under a year parent are found too.  Months kept as
#+begin_src csv blocks have no table and are skipped."
  (let (result)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward "^\\*+ \\([0-9]\\{4\\}-[0-9]\\{2\\}\\)" nil t)
       (let* ((month (match-string-no-properties 1))
              (beg (match-beginning 0))
              (end (save-excursion (goto-char beg) (org-end-of-subtree t t))))
         (when (save-excursion (goto-char beg) (re-search-forward "^[ \t]*|" end t))
           (push (cons month (copy-marker beg)) result)))))
    (nreverse result)))

(defun nl/nordita-timesheet--marker-for (month)
  "Marker at the start of MONTH's heading, or a user-error if MONTH has no table."
  (or (cdr (assoc month (nl/nordita-timesheet--table-months)))
      (user-error "No month table found for %s" month)))

(defun nl/nordita-timesheet--table-csv (marker)
  "\"day,hours\" CSV for the table in the subtree at MARKER.
Keeps only rows with a numeric day (drops the header, the hlines, and the
trailing total row) and the first two columns (drops any notes column).  More
robust than orgtbl-to-csv, which would include the total row and break the
script.  Runs in the marker's buffer so it is safe from any current buffer."
  (let ((table
         (with-current-buffer (marker-buffer marker)
           (org-with-wide-buffer
            (goto-char marker)
            (let ((end (org-end-of-subtree t t)))
              (goto-char marker)
              (if (re-search-forward "^[ \t]*|" end t)
                  (org-table-to-lisp)
                (user-error "No table found for that month")))))))
    (concat
     "day,hours\n"
     (mapconcat
      (lambda (row)
        (format "%s,%s" (string-trim (nth 0 row)) (string-trim (nth 1 row))))
      (seq-filter
       (lambda (row)
         (and (listp row)
              (string-match-p "\\`[0-9]+\\'" (string-trim (or (nth 0 row) "")))))
       table)
      "\n")
     "\n")))

(defun nl/nordita-timesheet--month-name (month)
  "\"2026-06\" -> \"June 2026\"."
  (format "%s %s"
          (nth (1- (string-to-number (substring month 5 7)))
               '("January" "February" "March" "April" "May" "June" "July"
                 "August" "September" "October" "November" "December"))
          (substring month 0 4)))

;;;; The pipeline ------------------------------------------------------------

(defun nl/nordita-generate-timesheet (month)
  "Run the whole timesheet pipeline for MONTH (a \"YYYY-MM\" string).
Reads MONTH's table from the current buffer, exports it to hours_YYYY-MM.csv,
runs the timesheet script to fill the PDF, copies both files to Dropbox, opens
the PDF, and opens a Gmail compose tab addressed to the Nordita contact.
Signals a user-error and stops if the script exits non-zero (see the
*timesheet* buffer)."
  (let* ((project  nl/nordita-timesheet-project)
         (marker   (nl/nordita-timesheet--marker-for month))
         (csv-name (format "hours_%s.csv" month))
         (pdf-name (format "nordita-timesheet-%s.pdf" month))
         (csv-path (expand-file-name csv-name project))
         (pdf-path (expand-file-name pdf-name project))
         ;; Build the CSV while still in the note buffer, before with-temp-file.
         (csv      (nl/nordita-timesheet--table-csv marker))
         (default-directory project))
    ;; Export the month's table to CSV.
    (with-temp-file csv-path (insert csv))
    ;; Fill the PDF (cwd = project so the timesheet.pdf template resolves).
    (let ((buf (get-buffer-create "*timesheet*")))
      (with-current-buffer buf (erase-buffer))
      (let ((code (call-process nl/nordita-timesheet-uv nil buf t "run" "timesheet"
                                (format "--month=%s" month)
                                "--output" pdf-name csv-name)))
        (unless (eq code 0)
          (display-buffer buf)
          (user-error "timesheet failed (exit %s) - see the *timesheet* buffer" code))))
    ;; Archive both files to Dropbox.
    (copy-file csv-path (expand-file-name csv-name nl/nordita-timesheet-dropbox) t)
    (copy-file pdf-path (expand-file-name pdf-name nl/nordita-timesheet-dropbox) t)
    ;; Open the PDF for a visual check.
    (call-process "xdg-open" nil 0 nil pdf-path)
    ;; Open a Gmail compose tab; drag the PDF in and send.
    (let* ((pretty  (nl/nordita-timesheet--month-name month))
           (subject (format "Nordita timesheet - %s" pretty))
           (body    (format (concat "Hi Emina,\n\n"
                                    "Please find attached my Nordita timesheet for %s.\n\n"
                                    "Best regards,\nNelson")
                            pretty)))
      (browse-url
       (concat "https://mail.google.com/mail/?view=cm&fs=1"
               "&to=" (url-hexify-string nl/nordita-timesheet-recipient)
               "&cc=" (url-hexify-string nl/nordita-timesheet-cc)
               "&su=" (url-hexify-string subject)
               "&body=" (url-hexify-string body))))
    (message "Timesheet %s: CSV + PDF written, copied to Dropbox, PDF opened, compose tab ready."
             month)))

(defun nl/nordita-timesheet-generate ()
  "Pick a month with a table from the current buffer and run the pipeline."
  (interactive)
  (let ((months (mapcar #'car (nl/nordita-timesheet--table-months))))
    (unless months (user-error "No month tables found in this buffer"))
    (nl/nordita-generate-timesheet
     (completing-read (format "Timesheet month (default %s): " (car months))
                      months nil t nil nil (car months)))))

;;;; Generating a fresh month section ----------------------------------------

(defvar nl/nordita-timesheet--month-names
  '("January" "February" "March" "April" "May" "June" "July"
    "August" "September" "October" "November" "December")
  "Full English month names, index 0 = January.")

(defun nl/nordita-timesheet--month-weekday-times (year month)
  "List of time values, in order, for every Mon-Fri in MONTH (1-12) of YEAR.
The single place the month's working days are enumerated: both the table rows
and the \"Weeks N-M\" heading derive from it, so they cannot disagree."
  (let* ((next-month (if (= month 12) 1 (1+ month)))
         (next-year (if (= month 12) (1+ year) year))
         (month-end (time-subtract (encode-time 0 0 0 1 next-month next-year)
                                   (seconds-to-time 1)))
         (date (encode-time 0 0 0 1 month year))
         times)
    (while (time-less-p date month-end)
      (unless (member (format-time-string "%u" date) '("6" "7"))
        (push date times))
      (setq date (time-add date (days-to-time 1))))
    (nreverse times)))

(defun nl/nordita-timesheet--month-weekdays (year month)
  "List of \"DD\" day-of-month strings for every Mon-Fri in MONTH (1-12) of YEAR."
  (mapcar (lambda (time) (format-time-string "%d" time))
          (nl/nordita-timesheet--month-weekday-times year month)))

(defun nl/nordita-timesheet--easter (year)
  "(MONTH . DAY) of Easter Sunday in YEAR, by the anonymous Gregorian algorithm.
Emacs has no Swedish holiday support, and three of the holidays below are
Easter-derived, so the date is computed here rather than pulled from a table
that would need extending every year."
  (let* ((a (% year 19))
         (b (/ year 100))
         (c (% year 100))
         (d (/ b 4))
         (e (% b 4))
         (f (/ (+ b 8) 25))
         (g (/ (+ (- b f) 1) 3))
         (h (% (+ (* 19 a) (- b d g) 15) 30))
         (i (/ c 4))
         (k (% c 4))
         (l (% (- (+ 32 (* 2 e) (* 2 i)) h k) 7))
         (m (/ (+ a (* 11 h) (* 22 l)) 451))
         (n (+ h l (- (* 7 m)) 114)))
    (cons (/ n 31) (1+ (% n 31)))))

(defun nl/nordita-timesheet--day-offset (month day year offset)
  "(MONTH . DAY) OFFSET days from MONTH/DAY of YEAR."
  (let ((time (time-add (encode-time 0 0 0 day month year)
                        (days-to-time offset))))
    (cons (string-to-number (format-time-string "%m" time))
          (string-to-number (format-time-string "%d" time)))))

(defun nl/nordita-timesheet--midsummer-eve (year)
  "(MONTH . DAY) of Midsommarafton in YEAR: the Friday falling 19-25 June."
  (let ((day 19))
    (while (not (string= (format-time-string "%u" (encode-time 0 0 0 day 6 year)) "5"))
      (setq day (1+ day)))
    (cons 6 day)))

(defun nl/nordita-timesheet-swedish-holidays (year)
  "Alist of ((MONTH . DAY) . NAME) for Swedish holidays in YEAR.

Covers the red days plus the three days Sweden effectively shuts down for -
Midsommarafton, Julafton and Nyårsafton - because those are non-working days in
practice and Midsommarafton is always a Friday, so it always lands on a working
weekday.

Påskdagen, Pingstdagen, Midsommardagen and Alla helgons dag are deliberately
absent: each always falls at a weekend, and the timesheet table holds only
Mon-Fri."
  (let ((easter (nl/nordita-timesheet--easter year)))
    (cl-flet ((from-easter (offset)
                (nl/nordita-timesheet--day-offset
                 (car easter) (cdr easter) year offset)))
      (list
       (cons '(1 . 1)   "Nyårsdagen")
       (cons '(1 . 6)   "Trettondedag jul")
       (cons (from-easter -2) "Långfredagen")
       (cons (from-easter 1)  "Annandag påsk")
       (cons '(5 . 1)   "Första maj")
       (cons (from-easter 39) "Kristi himmelsfärdsdag")
       (cons '(6 . 6)   "Sveriges nationaldag")
       (cons (nl/nordita-timesheet--midsummer-eve year) "Midsommarafton")
       (cons '(12 . 24) "Julafton")
       (cons '(12 . 25) "Juldagen")
       (cons '(12 . 26) "Annandag jul")
       (cons '(12 . 31) "Nyårsafton")))))

(defun nl/nordita-timesheet--holiday-note (year month day)
  "Notes-column text for MONTH/DAY of YEAR: the holiday name, or an empty cell."
  (let ((name (cdr (assoc (cons month day)
                          (nl/nordita-timesheet-swedish-holidays year)))))
    (if name
        (concat nl/nordita-timesheet-holiday-prefix name)
      "")))

(defun nl/nordita-timesheet--week-span (year month)
  "(FIRST-WEEK . LAST-WEEK): ISO week of MONTH's first and last working day,
for the \"Weeks N-M\" heading.

Both ends come from weekdays, since the table holds only Mon-Fri.  A December
whose final days fall in ISO week 1 of the next year is clamped back to the last
weekday still inside the span, so the heading reads \"Weeks 49-52\" rather than a
backwards \"Weeks 49-1\"; those trailing days keep their rows in the table."
  (let* ((times (nl/nordita-timesheet--month-weekday-times year month))
         (week (lambda (time)
                 (string-to-number (format-time-string "%V" time))))
         (first (funcall week (car times)))
         (last (car (last (seq-filter (lambda (w) (>= w first))
                                      (mapcar week times))))))
    (cons first last)))

(defun nl/nordita-timesheet--month-section (year month &optional level)
  "Org section string for MONTH (1-12) of YEAR: the heading, the three per-month
`C-c C-c' blocks (timesheet, work summary, report email), and a zeroed weekday
table with a total row and TBLFM sum.
LEVEL is the heading depth in stars (default 1; use 2 for a month nested under a
year parent)."
  (let* ((tag   (format "%04d-%02d" year month))
         (span  (nl/nordita-timesheet--week-span year month))
         (days  (nl/nordita-timesheet--month-weekdays year month))
         (stars (make-string (or level 1) ?*)))
    (concat
     (format "%s %s: Weeks %d–%d\n" stars tag (car span) (cdr span))
     "#+begin_src emacs-lisp :results silent :exports none\n"
     (format "(nl/nordita-generate-timesheet \"%s\")\n" tag)
     "#+end_src\n\n"
     "| day | hours | notes |\n|-----+-------+-------|\n"
     (mapconcat
      (lambda (d)
        (format "| %s | 0 | %s |" d
                (nl/nordita-timesheet--holiday-note
                 year month (string-to-number d))))
      days "\n")
     "\n|-----+-------+-------|\n|     |       |       |\n"
     "#+TBLFM: @>$2=vsum(@2$2..@-1$2)\n\n"
     ;; The report blocks sit after the table: the timesheet block reads the
     ;; table directly above it, while these two work on the summary that the
     ;; first of them appends to the end of the section.
     "#+begin_src emacs-lisp :results silent :exports none\n"
     (format "(nl/nordita-generate-work-summary \"%s\")\n" tag)
     "#+end_src\n\n"
     "#+begin_src emacs-lisp :results silent :exports none\n"
     (format "(nl/nordita-email-work-summary \"%s\")\n" tag)
     "#+end_src\n")))

;;;; Placing a month section (current-year flat, other years grouped) --------

(defun nl/nordita-timesheet--tag-value (tag)
  "Sortable integer for TAG: \"YYYY-MM\" -> YYYY*100+MM, \"YYYY\" -> YYYY*100.
Larger is more recent.  Preserves match data so it is safe to call mid-search."
  (save-match-data
    (if (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)\\'" tag)
        (+ (* 100 (string-to-number (match-string 1 tag)))
           (string-to-number (match-string 2 tag)))
      (* 100 (string-to-number tag)))))

(defun nl/nordita-timesheet--present-month-tags ()
  "All YYYY-MM tags that already have a heading (any level, table or csv block)."
  (let (tags)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward "^\\*+ \\([0-9]\\{4\\}-[0-9]\\{2\\}\\)" nil t)
       (push (match-string-no-properties 1) tags)))
    (nreverse tags)))

(defun nl/nordita-timesheet--upcoming-months (n)
  "List of N+1 \"YYYY-MM\" tags: the current month and the following N months."
  (let* ((now (decode-time (current-time)))
         (base (+ (* (nth 5 now) 12) (1- (nth 4 now))))
         tags)
    (dotimes (i (1+ n))
      (let ((v (+ base i)))
        (push (format "%04d-%02d" (/ v 12) (1+ (% v 12))) tags)))
    (nreverse tags)))

(defun nl/nordita-timesheet--toplevel-insert-pos (value)
  "Position to insert a new top-level month-region entry sorting at VALUE:
the start of the first existing top-level YYYY[-MM] heading older than VALUE, or
`point-max' if none."
  (save-excursion
    (goto-char (point-min))
    (catch 'pos
      (while (re-search-forward "^\\* \\([0-9]\\{4\\}\\)\\(?:-\\([0-9]\\{2\\}\\)\\)?\\b" nil t)
        (let ((val (+ (* 100 (string-to-number (match-string 1)))
                      (if (match-string 2) (string-to-number (match-string 2)) 0))))
          (when (< val value)
            (throw 'pos (match-beginning 0)))))
      (point-max))))

(defun nl/nordita-timesheet--find-year-parent (year)
  "Marker at the `* YEAR' parent heading, or nil if absent."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^\\* %d[ \t]*$" year) nil t)
      (copy-marker (match-beginning 0)))))

(defun nl/nordita-timesheet--ensure-year-parent (year)
  "Return a marker at the `* YEAR' parent, creating it at the right top-level
position (newest-first) if it does not yet exist."
  (or (nl/nordita-timesheet--find-year-parent year)
      (progn
        (save-excursion
          (goto-char (nl/nordita-timesheet--toplevel-insert-pos (* 100 year)))
          (insert (format "* %d\n\n" year)))
        (nl/nordita-timesheet--find-year-parent year))))

(defun nl/nordita-timesheet--insert-under-parent (parent tag section)
  "Insert SECTION (a level-2 month section for TAG) under PARENT (a marker),
at the newest-first position among its `**' children.  Return the position of
the inserted heading."
  (let ((value (nl/nordita-timesheet--tag-value tag))
        (end (save-excursion (goto-char parent) (org-end-of-subtree t t)))
        (pos nil))
    (save-excursion
      (goto-char parent)
      (forward-line 1)
      (catch 'done
        (while (re-search-forward "^\\*\\* \\([0-9]\\{4\\}-[0-9]\\{2\\}\\)" end t)
          (when (< (nl/nordita-timesheet--tag-value (match-string 1)) value)
            (setq pos (match-beginning 0))
            (throw 'done nil)))))
    (setq pos (or pos end))
    (save-excursion (goto-char pos) (insert section "\n"))
    pos))

(defun nl/nordita-timesheet--align-table-at (pos)
  "Align the table of the month section whose heading is at POS.
Generated rows vary in width once holiday notes are present, so the section
would otherwise land visibly ragged and need a manual TAB to straighten out."
  (save-excursion
    (goto-char pos)
    (let ((end (save-excursion (org-end-of-subtree t t))))
      (when (re-search-forward "^[ \t]*|" end t)
        (org-table-align)))))

(defun nl/nordita-timesheet--insert-section-for (tag)
  "Insert a fresh month section for TAG (\"YYYY-MM\") into the current buffer.
Months of the current year go top-level; other years nest as `**' under a
`* YEAR' parent (created if needed).  Everything is placed newest-first, and the
table is aligned.  Return the position of the inserted month heading."
  (let* ((year  (string-to-number (substring tag 0 4)))
         (month (string-to-number (substring tag 5 7)))
         (current-year (string-to-number (format-time-string "%Y")))
         (pos
          (if (and (= year current-year)
                   (not (nl/nordita-timesheet--find-year-parent year)))
              ;; Current year, no parent yet: top-level, newest-first.
              (let ((pos (nl/nordita-timesheet--toplevel-insert-pos
                          (nl/nordita-timesheet--tag-value tag))))
                (save-excursion
                  (goto-char pos)
                  (insert (nl/nordita-timesheet--month-section year month 1) "\n"))
                pos)
            ;; Other year (or a current year that already has a parent): grouped.
            (nl/nordita-timesheet--insert-under-parent
             (nl/nordita-timesheet--ensure-year-parent year)
             tag
             (nl/nordita-timesheet--month-section year month 2)))))
    (nl/nordita-timesheet--align-table-at pos)
    pos))

;;;; Interactive entry points ------------------------------------------------

(defun nl/nordita-insert-month ()
  "Pick an upcoming month (the next 12) not yet in the note and insert its
section, placed newest-first: top-level for the current year, nested under a
`* YEAR' parent otherwise.  Point is left on the new heading."
  (interactive)
  (let* ((present (nl/nordita-timesheet--present-month-tags))
         (candidates (seq-remove (lambda (tag) (member tag present))
                                 (nl/nordita-timesheet--upcoming-months 12))))
    (unless candidates
      (user-error "All of the next 12 months are already in the note"))
    (let* ((tag (completing-read
                 (format "Add month (default %s): " (car candidates))
                 candidates nil t nil nil (car candidates)))
           (pos (nl/nordita-timesheet--insert-section-for tag)))
      (goto-char pos)
      (message "Inserted %s. Fill in the hours, then C-c C-c its block." tag))))

(defun nl/nordita-insert-month-by-name (month year)
  "Insert a fresh month section for MONTH (1-12) of YEAR, placed newest-first
(top-level for the current year, else nested under a `* YEAR' parent).
Interactively, choose the month by name and confirm the year (default: current).
Use `nl/nordita-insert-month' to pick from the upcoming months instead."
  (interactive
   (let* ((default (nth (1- (string-to-number (format-time-string "%m")))
                        nl/nordita-timesheet--month-names))
          (name (completing-read "Month: " nl/nordita-timesheet--month-names nil t nil nil default))
          (year (read-number "Year: " (string-to-number (format-time-string "%Y")))))
     (list (1+ (seq-position nl/nordita-timesheet--month-names name #'string-equal)) year)))
  (goto-char (nl/nordita-timesheet--insert-section-for (format "%04d-%02d" year month))))

(provide 'nl-nordita-timesheet)
;;; nl-nordita-timesheet.el ends here
