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

(provide 'nl-nordita-tests)
;;; nl-nordita-tests.el ends here
