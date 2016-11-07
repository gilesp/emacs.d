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

(use-package org-bullets
 :ensure t
 :config
 (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; :ensure org-plus-contrib
(use-package org
  :mode (("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :pin org ;; Use version from orgmode.org/elpa instead of gnu
  :preface
  ;; hook to run when org-mode is started, to turn on certain modes etc.
  (defun gp/org-mode-hook ()
    ;;set variable pitch font for org, but keep fixed width for code etc.
    (variable-pitch-mode t)

    ;; disable linum
    (linum-mode -1)

    ;; enable on the fly spell checking
    (flyspell-mode 1))
  :init
  (use-package ox-tufte
    :defer t)
  (use-package ox-gfm
    :defer t)
  :config
  (progn
    (require 'ox-md)
    (require 'ox-gfm)
    (require 'ox-html)
    (require 'ox-tufte)
    (require 'org-protocol)

    ;; This functionality isn't in a package yet.
    ;; I installed it into ~/.emacs.d/site-lisp with wget https://raw.githubusercontent.com/alphapapa/org-protocol-capture-html/master/org-protocol-capture-html.el
    (require 'org-protocol-capture-html)
    
    (add-hook 'org-mode-hook 'gp/org-mode-hook)
    
    ;; configuration
    (setq org-directory "~/Documents/Dropbox/org")
    (setq line-spacing 3)
    (setq org-startup-with-inline-images t)
    
    ;;
    ;; Agenda config
    ;;
    ;; All the agenda/capture/refile stuff is heavily influenced by http://doc.norang.ca/org-mode.html
    (setq org-agenda-window-setup (quote current-window))
    (setq org-agenda-files (list (expand-file-name "todo.org" org-directory)
                                 (expand-file-name "refile.org" org-directory)
                                 (expand-file-name "projects/" org-directory)
                                 (expand-file-name "projects/work/" org-directory)
                                 (expand-file-name "projects/personal/" org-directory)
                                 (expand-file-name "personal_schedule.org" org-directory)))
    
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    
    (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d)")
                              (sequence "WAITING(w@/!)" "DEFERRED(e)" "|" "CANCELLED(c@/!)" "MEETING")))
    (setq org-todo-keyword-faces
          (quote
           (("DONE" . success)
            ("STARTED" . underline)
            ("WAITING" . warning)
            ("TODO" . error)
            ("NEXT" . highlight)
            ("DEFERRED" . default)
            ("CANCELLED" . default)
            ("MEETING" . diary))))

    ;; allow changing from any task todo state to another by selecting appropriate key from the menu
    ;; C-c C-t KEY
    (setq org-use-fast-todo-selection t)

    ;; set appropriate tags on tasks (used for filtering)
    (setq org-todo-state-tags-triggers
          '(("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("DEFERRED" ("WAITING") ("DEFERRED" . t))
            (done ("WAITING") ("DEFERRED"))
            ("TODO" ("WAITING") ("CANCELLED") ("DEFERRED"))
            ("NEXT" ("WAITING") ("CANCELLED") ("DEFERRED"))
            ("DONE" ("WAITING") ("CANCELLED") ("DEFERRED"))))
    
    (setq org-capture-templates
          '(("t" "Todo"
             entry (file "refile.org" "Tasks")
             "* TODO [#B] %?")
            ("n" "Note"
             entry (file "refile.org" "Notes")
             "* %? :NOTE:\n%U\n%a\n")
            ("j" "Journal Entry"
             entry (file+datetree "journal.org")
             "* %?"
             :empty-lines 1)
            ("p" "Protocol"
             entry (file+headline "refile.org" "Notes")
             "* %:description :NOTE:RESEARCH:\n#+BEGIN_QUOTE\n%i\n\n -- %:link %u\n #+END_QUOTE\n\n%?")
            ("L" "Protocol Link"
             entry (file+headline "refile.org" "Notes")
             "* %? [[%:link][%:description]] :NOTE:RESEARCH:\nCaptured On: %u")
            ("P" "Pandoc Converted Website"
             entry (file+headline "refile.org" "Notes")
             "* %a :NOTE:RESEARCH:\n\n%U %?\n\n%:initial")
            ))

    ;;
    ;; Refile config
    ;;
    ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))


    ;; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes (quote confirm))

    ;; Exclude DONE state tasks from refile targets
    (defun gp/verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets"
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))

    (setq org-refile-target-verify-function 'gp/verify-refile-target)

    ;;
    ;; Custom agenda views
    ;;
    ;; do not dim blocked tasks
    (setq org-agenda-dim-blocked-tasks nil)

    ;; compact the block agenda view
    (setq org-agenda-compact-blocks t)

    ;; custom agenda commands
    (setq org-agenda-custom-commands
          '(("N" "Notes" tags "NOTE"
             ((org-agenda-overriding-header "Notes")
              (org-tags-match-list-sublevels t)))
            (" " "Agenda"
             ((agenda "" nil)
              (tags "REFILE"
                    ((org-agenda-overriding-header "Tasks to Refile")
                     (org-tags-match-list-sublevels nil)))
              (tags-todo "WORK/NEXT"
                         ((org-agenda-overriding-header "Work Next Tasks")
                          (org-tags-match-list-sublevels nil)))
              (tags-todo "PERSONAL/NEXT"
                         ((org-agenda-overriding-header "Personal Next Tasks")
                          (org-tags-match-list-sublevels nil)))
              nil))))
    ;;
    ;; General Config
    ;;
    ;; actually emphasise text (e.g. show as italic instead of /italic/)
    (setq org-hide-emphasis-markers t)

    ;; syntax highlight code blocks
    (setq org-src-fontify-natively t)

    ;; replace list indicators with bullet points
    (font-lock-add-keywords 'org-mode
                            '(("^ +\\([-*]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
    

    
    ))

(eval-after-load "org"
  '(progn
     (defun gp/adjoin-to-list-or-symbol (element list-or-symbol)
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
         (gp/adjoin-to-list-or-symbol
          'fixed-pitch
          (face-attribute face :inherit))))
      (list 'org-code 'org-block 'org-block-begin-line 'org-block-end-line 'org-verbatim 'org-macro 'org-table 'org-link 'org-footnote 'org-date))
     )
  )
                 
;; TODO: Figure out where the odt schema files live so I can include them in the config
;; These live in ~/.emacs.d/elpa/org-YYYYMMDD/etc/ but I'm not sure if I need to specify them now
;; Require OpenDocument Text export mode
;;  (setq org-odt-schema-dir "~/.emacs.d/org-mode/etc/schema")
;;  (setq org-odt-styles-dir "~/.emacs.d/org-mode/etc/styles")
;;  (require 'ox-odt nil t)

(provide 'init-org)
;;; init-org.el ends here
