;;; init-editing --- General editing config
;;; Commentary:

;;; Code:

;; sentences end with a single space
(setq sentence-end-double-space nil)

;; font config.
(set-face-attribute 'default nil :font "Hack")
(set-face-attribute 'fixed-pitch nil :font "Hack")
(set-face-attribute 'variable-pitch nil :font "Roboto")

;; ;; If it's installed, use Hack font, otherwise fall back to DejaVu Sans Mono
;; (if (member "Hack" (font-family-list))
;;     (progn
;;       (set-face-attribute 'default nil :font "Hack")
;;       (add-to-list 'default-frame-alist '(font . "Hack"))
;;       (set-fontset-font t 'unicode "Hack" nil))
;;   (set-face-attribute 'default nil :font "DejaVu Sans Mono")
;;   (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono"))
;;   (set-fontset-font t 'unicode "DejaVu Sans Mono" nil))

;; ;; specify fallback fonts for all unicode characters not in main font
;; (when (member "Noto Sans" (font-family-list))
;;   (set-fontset-font t 'unicode "Noto Sans" nil 'append))
;; (when (member "Symbola" (font-family-list))
;;   (set-fontset-font t 'unicode "Symbola" nil 'append))

;; disable use of tabs for indentation
;; tabs lost, get over it.
(setq-default indent-tabs-mode nil)

;; Unicode!
;; UTF-8 All the things!
(setq locale-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(set-language-environment "UTF-8")
;; set the default encoding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
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


;; Nicer wrapping of long lines
(use-package adaptive-wrap
  :diminish
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config
  ;;(setq-default adaptive-wrap-extra-indent 2)
  (global-visual-line-mode +1))

;; Save history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(savehist-autosave)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(search-ring
        regexp-search-ring))

;; keep a list of recently opened files
(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")

;; scrolling!
;; (setq scroll-step 1)
(setq scroll-conservatively 101)
(setq auto-window-vscroll nil)
;; I'm still trying to figure out how to stop the mouse wheel from scrolling past the end of the file
;; so for the time being, I'm just disabling scrollbars
(setq scroll-bar-height nil)
(setq scroll-bar-width 5)
(setq scroll-error-top-bottom t)
(setq scroll-bar-adjust-thumb-portion nil)

;; uniquify buffer names
(require 'uniquify)

;; Enable use of hydras
(use-package hydra
  :config
  (defhydra hydra-goto-line (goto-map "")
    "goto-line"
    ("g" goto-line "go")
    ("m" set-mark-command "mark" :bind nil)
    ("q" nil "quit")))

(provide 'init-editing)
;;; init-editing.el ends here
