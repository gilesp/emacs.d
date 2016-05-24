;;; init-linenumbers --- Sets up nlinum

;;; Commentary:
;;; Configure line numbering globally (I work with code more than plain text)

;;; Code:
(require-package 'nlinum)
;; smart width for line numbers
;;(setq nlinum-format "%d")

;; Preset width nlinum
(add-hook 'nlinum-mode-hook
          (lambda ()
            (when nlinum-mode
              (setq nlinum--width
                    ;; works with the default `nlinum-format'
                    (length (number-to-string
                             (count-lines (point-min) (point-max)))))
              ;; use this instead if your `nlinum-format' has one space
              ;; (or other character) after the number
              ;;(1+ (length (number-to-string
              ;;             (count-lines (point-min) (point-max)))))
              (nlinum--flush))))

;; enable line numbers mode
(require 'nlinum)
(global-nlinum-mode 1)

(provide 'init-linenumbers)
;;; init-linenumbers.el ends here
