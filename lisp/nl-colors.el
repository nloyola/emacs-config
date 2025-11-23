;;; -*- lexical-binding: t; -*-

(require 'color)

(defun nl/replace-hex-with-oklch-at-point ()
  "Replace the hex color on the current line with oklch(L% C H)."
  (interactive)
  (require 'color)
  (let ((line-start (line-beginning-position))
        (line-end (line-end-position)))
    (save-excursion
      (goto-char line-start)
      (when (re-search-forward "#[0-9A-Fa-f]\\{6\\}" line-end t)
        (let* ((m-start (match-beginning 0))
               (m-end (match-end 0))
               (hex (buffer-substring-no-properties m-start m-end))
               (rgb (color-name-to-rgb hex))
               (oklab (apply #'color-srgb-to-oklab rgb))
               (L (nth 0 oklab))
               (A (nth 1 oklab))
               (B (nth 2 oklab))
               (C (sqrt (+ (* A A) (* B B))))
               (H-rad (atan B A))
               (H-deg (* 180.0 (/ H-rad float-pi)))
               (H (if (< H-deg 0) (+ H-deg 360.0) H-deg))
               (Lp (* L 100.0))
               (out (format "oklch(%.1f%% %.4f %.2f)" Lp C H)))
          (delete-region m-start m-end)
          (goto-char m-start)
          (insert out))))))

(provide 'nl-colors)
