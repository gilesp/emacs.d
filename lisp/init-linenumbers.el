;;; init-linenumbers --- Sets up linum

;;; Commentary:
;;; Configure line numbering globally (I work with code more than plain text)

;;; Code:
(defun linum-format-func (line)
  "Align linum LINE number to the right."
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format "%%%dd " w) line) 'face 'linum)))

(defvar linum-format)
(setq linum-format 'linum-format-func)
;;(setq linum-format "%d ")

(defun linum-update-window-scale-fix (win)
  "Fix linum spacing for WIN scaled text."
  (set-window-margins win
		      (ceiling (* (if (boundp 'text-scale-mode-step)
				      (expt text-scale-mode-step
					    text-scale-mode-amount) 1)
				  (if (car (window-margins))
				      (car (window-margins)) 1)
				  ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

(global-linum-mode t)

(provide 'init-linenumbers)
;;; init-linenumbers.el ends here
