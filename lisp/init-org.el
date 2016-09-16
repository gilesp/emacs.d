;; init-org --- Configure Org Mode

;;; Commentary:
;;
;; load and configure org-mode

;;; Code:
(defvar org-agenda-files)
(defvar org-agenda-window-setup)
(defvar org-directory)
(defvar org-todo-keyword-faces)
(defvar org-todo-keywords)
(defvar org-capture-templates)
(defvar org-odt-schema-dir)
(defvar org-odt-styles-dir)
(defvar org-hide-emphasis-markers)
(defvar org-src-fontify-natively)


(use-package writegood-mode
  :bind (("C-c C-g g" . writegood-grade-level)
         ("C-c C-g e" . writegood-reading-ease)))

(use-package org
  :mode (("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  :ensure org-plus-contrib
  :init
  (require-package 'org-bullets)
  (let ((my-org-modules
         '(ox-md
           ox-gfm
           ox-html)))
    (dolist (m my-org-modules)
      (add-to-list 'org-modules m)))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture))
  :config
  (mapc 'require org-modules)
  ;; configuration
  (setq org-directory "~/Documents/Dropbox/org")

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  
  ;; agenda
  (setq org-agenda-window-setup (quote current-window))
  (setq org-agenda-files (list (expand-file-name "todo.org" org-directory)))
  
  (setq org-todo-keyword-faces
        (quote
         (("DONE" . success)
          ("STARTED" . diary)
          ("WAITING" . warning)
          ("TODO" . error))))
  (setq org-todo-keywords '((sequence "TODO(t)" "PLAN(p)" "NEXT-ACTION(n)" "STARTED(s)" "WAITING(w@/!)" "DEFERRED(e)" "APPT" "|" "DONE(d!/!)" "CANCELLED(c@/!)")))

  (setq org-capture-templates
        '(("t" "Todo"
           entry (file+headline "todo.org" "Tasks")
           "* TODO [#B] %?")
          ("j" "Journal Entry"
           entry (file+datetree "journal.org")
           "* %?"
           :empty-lines 1)
          ))

  ;; actually emphasise text (e.g. show as italic instead of /italic/)
  (setq org-hide-emphasis-markers t)

  ;; syntax highlight code blocks
  (setq org-src-fontify-natively t)

  ;; replace list indicators with bullet points
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (org-bullets-mode 1)

  ;; enable on the fly spell checking
  (flyspell-mode 1)

  ;;set variable pitch font for org, but keep fixed width for code etc.
  (variable-pitch-mode t)
  (setq line-spacing 3)

  ;; disable linum
  (linum-mode -1)

  (defun gp-adjoin-to-list-or-symbol (element list-or-symbol)
    (let ((list (if (not (listp list-or-symbol))
                    (list list-or-symbol)
                  list-or-symbol)))
      (require 'cl-lib)
      (cl-adjoin element list)))

  (mapc
   (lambda (face)
     (set-face-attribute
      face nil
      :inherit
      (gp-adjoin-to-list-or-symbol
       'fixed-pitch
       (face-attribute face :inherit))))
   (list 'org-code 'org-block 'org-block-begin-line 'org-block-end-line 'org-verbatim 'org-macro 'org-table))
  )

;; TODO: Figure out where the odt schema files live so I can include them in the config
;; Require OpenDocument Text export mode
;;  (setq org-odt-schema-dir "~/.emacs.d/org-mode/etc/schema")
;;  (setq org-odt-styles-dir "~/.emacs.d/org-mode/etc/styles")
;;  (require 'ox-odt nil t)

(provide 'init-org)
;;; init-org.el ends here
