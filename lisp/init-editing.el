;;; init-editing --- General editing config
;;; Commentary:

;;; Code:

;; sentences end with a single space
(setq sentence-end-double-space nil)

;; font config.
;; If it's installed, use Hack font, otherwise fall back to DejaVu Sans Mono
(if (member "Hack" (font-family-list))
    (progn
      (set-face-attribute 'default nil :font "Hack")
      (add-to-list 'default-frame-alist '(font . "Hack"))
      (set-fontset-font t 'unicode "Hack" nil))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono")
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono"))
  (set-fontset-font t 'unicode "DejaVu Sans Mono" nil))

;; specify fallback fonts for all unicode characters not in main font
(when (member "Noto Sans" (font-family-list))
  (set-fontset-font t 'unicode "Noto Sans" nil 'append))
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'append))

;; Unicode!
;; UTF-8 All the things!
; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)
(setq locale-coding-system 'utf-8-unix)
(setq default-file-name-coding-system 'utf-8-unix)
(setq buffer-file-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
;; set the default encoding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
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
    (smooth-scrolling-mode 1)
    (setq smooth-scroll-margin 5)))

;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator "::")

(provide 'init-editing)
;;; init-editing.el ends here
