;;; init-editing --- General editing config
;;; Commentary:

;;; Code:

;; sentences end with a single space
(setq sentence-end-double-space nil)

;; Unicode!
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Usually I want to go to the right indentation on a new line
(global-set-key (kbd "RET") 'newline-and-indent)


(defun gp/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'gp/smarter-move-beginning-of-line)

;; Comment or uncomment region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)


;; scrolling!
(use-package smooth-scrolling
  :init
  (progn
    (require 'smooth-scrolling)
    (setq smooth-scroll-margin 5)))

(provide 'init-editing)
;;; init-editing.el ends here
