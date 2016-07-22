;;; init-autocorrect --- configures autocorrection of spelling mistakes

;;; Commentary:
;; Autocorrect spelling mistakes
;; Taken from http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html

;;; Code:
(define-key ctl-x-map "\C-i"
  `gp-ispell-word-then-abbrev)

(defun gp-ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev.  Otherwise it will
be global."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (progn
               (backward-word)
               (and (setq bef (thing-at-point 'word))
                    (not (ispell-word nil 'quiet)))))
      (setq aft (thing-at-point 'word)))
    (when (and aft bef (not (equal aft bef)))
      (setq aft (downcase aft))
      (setq bef (downcase bef))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob")))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)

(provide 'init-autocorrect)
;;; init-autocorrect.el ends here
