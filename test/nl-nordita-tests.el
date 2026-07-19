;;; nl-nordita-tests.el --- Tests for the Nordita timesheet/worklog pipeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Run from the repository root:
;;
;;   emacs -Q --batch -L lisp -l test/nl-nordita-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org)
(require 'nl-nordita-timesheet)
(require 'nl-nordita-worklog)

;;;; Helpers ------------------------------------------------------------------

(defconst nl-nordita-tests--summary
  "** Week 23 - Jun 1-5\n*** Norweb\n- did a thing\n"
  "A summary as the org-weekly-git-summary skill emits it: weeks at `**'.")

(defmacro nl-nordita-tests--with-note (text &rest body)
  "Run BODY in a temporary Org buffer containing TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     ,@body))

(defconst nl-nordita-tests--flat-note
  "* 2026-06: Weeks 23-27\n\n| day | hours |\n|-----+-------|\n| 01 | 7 |\n\n* 2026-05: Weeks 18-22\n\n| day | hours |\n| 01 | 7 |\n"
  "A current-year month at the top level, as the note stores it.")

(defconst nl-nordita-tests--nested-note
  "* 2025\n** 2025-06: Weeks 23-27\n\n| day | hours |\n|-----+-------|\n| 02 | 7 |\n\n** 2025-05: Weeks 18-22\n\n| day | hours |\n| 01 | 7 |\n"
  "A month of another year, grouped under a `* YEAR' parent.")

;;;; Week spans ---------------------------------------------------------------

(ert-deftest nl-nordita-week-span-is-never-backwards ()
  "A December ending inside ISO week 1 must not report \"Weeks 49-1\".
The last days of some Decembers belong to week 1 of the following year, which
made the heading run backwards."
  (dolist (year '(2024 2025 2026 2027 2028))
    (let ((span (nl/nordita-timesheet--week-span year 12)))
      (should (<= (car span) (cdr span))))))

(ert-deftest nl-nordita-week-span-clamps-december-2025 ()
  "Dec 2025 runs Mon 1 Dec (week 49) to Wed 31 Dec, which ISO puts in week 1.
The span stops at the last weekday still inside it: Fri 26 Dec, week 52."
  (should (equal (nl/nordita-timesheet--week-span 2025 12) '(49 . 52))))

(ert-deftest nl-nordita-week-span-keeps-week-53 ()
  "Clamping must not swallow a legitimate week 53 (Dec 2026 ends on a Thursday)."
  (should (equal (nl/nordita-timesheet--week-span 2026 12) '(49 . 53))))

(ert-deftest nl-nordita-week-span-starts-at-first-weekday ()
  "A month starting at a weekend takes its first week from the first weekday.
June 2025 starts on a Sunday; Mon 2 Jun is in week 23."
  (should (equal (car (nl/nordita-timesheet--week-span 2025 6)) 23))
  ;; Aug 2026 starts on a Saturday; Mon 3 Aug is in week 32.
  (should (equal (car (nl/nordita-timesheet--week-span 2026 8)) 32)))

;;;; Weekday enumeration ------------------------------------------------------

(ert-deftest nl-nordita-month-weekdays-are-sane ()
  "Every month yields 19-23 unique weekdays, in order, none of them a weekend.
Guards the day-by-day walk against drift across Sweden's clock changes."
  (dolist (year '(2024 2025 2026))
    (dotimes (i 12)
      (let* ((month (1+ i))
             (days (nl/nordita-timesheet--month-weekdays year month))
             (numbers (mapcar #'string-to-number days)))
        (should (<= 19 (length days) 23))
        (should (equal numbers (delete-dups (copy-sequence numbers))))
        (should (equal numbers (sort (copy-sequence numbers) #'<)))
        (dolist (day numbers)
          (should-not
           (member (format-time-string
                    "%u" (encode-time 0 0 0 day month year))
                   '("6" "7"))))))))

;;;; Worklog: the two month layouts ------------------------------------------

(ert-deftest nl-nordita-worklog-roundtrips-a-toplevel-month ()
  "The original layout keeps working: insert, find, extract."
  (nl-nordita-tests--with-note nl-nordita-tests--flat-note
    (let ((marker (nl/nordita-timesheet--marker-for "2026-06")))
      (should (equal (nl/nordita-worklog--month-level marker) 1))
      (should-not (nl/nordita-worklog--week-start marker))
      (nl/nordita-worklog--insert marker nl-nordita-tests--summary)
      (should (nl/nordita-worklog--week-start marker))
      ;; Promoted for export: weeks at `*', topics at `**'.
      (should (string-match-p "^\\* Week 23 "
                              (nl/nordita-worklog--week-text marker)))
      (should (string-match-p "^\\*\\* Norweb"
                              (nl/nordita-worklog--week-text marker))))))

(ert-deftest nl-nordita-worklog-roundtrips-a-nested-month ()
  "A month under a `* YEAR' parent behaves the same as a top-level one.
The summary used to be inserted verbatim at `**', making it a sibling month
rather than a child: `--week-start' then found nothing, so the guard against
overwriting hand-edited prose never fired and emailing reported no summary."
  (nl-nordita-tests--with-note nl-nordita-tests--nested-note
    (let ((marker (nl/nordita-timesheet--marker-for "2025-06")))
      (should (equal (nl/nordita-worklog--month-level marker) 2))
      (nl/nordita-worklog--insert marker nl-nordita-tests--summary)
      (should (nl/nordita-worklog--week-start marker))
      ;; Demoted on the way in, so the week really is a child of the month.
      (should (string-match-p "^\\*\\*\\* Week 23 " (buffer-string)))
      ;; ...and the next month is still where it was.
      (should (string-match-p "^\\*\\* 2025-05:" (buffer-string)))
      ;; Promoted on the way out to the same shape as the flat case.
      (let ((text (nl/nordita-worklog--week-text marker)))
        (should (string-match-p "^\\* Week 23 " text))
        (should (string-match-p "^\\*\\* Norweb" text))
        (should-not (string-match-p "2025-05" text))))))

(ert-deftest nl-nordita-worklog-nested-summary-stays-inside-its-month ()
  "The inserted summary must not leak into the following month's subtree."
  (nl-nordita-tests--with-note nl-nordita-tests--nested-note
    (let ((june (nl/nordita-timesheet--marker-for "2025-06")))
      (nl/nordita-worklog--insert june nl-nordita-tests--summary)
      (should-not (nl/nordita-worklog--week-start
                   (nl/nordita-timesheet--marker-for "2025-05"))))))

(ert-deftest nl-nordita-worklog-regeneration-guard-fires-when-nested ()
  "The \"already has Week sections\" guard is what stops a second run from
duplicating the summary; it is driven by `--week-start', so it silently failed
for nested months."
  (nl-nordita-tests--with-note nl-nordita-tests--nested-note
    (let ((marker (nl/nordita-timesheet--marker-for "2025-06")))
      (nl/nordita-worklog--insert marker nl-nordita-tests--summary)
      (should (nl/nordita-worklog--week-start marker)))))

;;;; Star shifting ------------------------------------------------------------

(ert-deftest nl-nordita-worklog-shift-is-a-noop-at-zero ()
  "Demoting or promoting by zero leaves the text alone, so the top-level month
path is byte-for-byte what it was before levels were taken into account."
  (should (equal (nl/nordita-worklog--demote nl-nordita-tests--summary 0)
                 nl-nordita-tests--summary))
  (should (equal (nl/nordita-worklog--promote nl-nordita-tests--summary 0)
                 nl-nordita-tests--summary)))

(ert-deftest nl-nordita-worklog-shift-leaves-body-text-alone ()
  "Only heading lines move; a list item or a bare `*' in prose must not."
  (let ((text "** Week 23\n- a * in prose\n*not a heading*\n"))
    (should (equal (nl/nordita-worklog--demote text 1)
                   "*** Week 23\n- a * in prose\n*not a heading*\n"))))

(provide 'nl-nordita-tests)
;;; nl-nordita-tests.el ends here
