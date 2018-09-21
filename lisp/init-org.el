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

;; (setq package-check-signature nil)

(use-package olivetti
  :diminish)

;; :ensure org-plus-contrib
(use-package org
  :pin org ;; Use version from orgmode.org/elpa instead of gnu
  :mode
    ("\\.org\\'" . org-mode)
  :bind
    (("C-c l" . org-store-link)
    ("C-c a" . org-agenda)
    ("C-c c" . org-capture)
    ("C-c r" . gp/org-refile-hydra/body))
  :custom
    (org-directory "~/Documents/Dropbox/org")
    (org-startup-indented t)
    (org-default-notes-file (expand-file-name "dump.org" org-directory))
    (org-startup-with-inline-images nil)
    (org-completion-use-ido nil) ;; so we can use helm for refiling
    (org-outline-path-complete-in-steps nil) ;; so we can use helm for refiling
    (org-hide-leading-stars t) ;; Only show one bullet per heading
    (org-src-window-setup 'current-window) ;; edit src blocks in place, rather than a new window
    (org-fontify-quote-and-verse-blocks t)
    (org-fontify-whole-heading-line t)
    (org-hide-emphasis-markers t) ;; actually emphasise text (e.g. show as italic instead of /italic/)
    (org-src-fontify-natively t) ;; syntax highlight code blocks
    (org-edit-src-content-indentation 0) ;; don't indent src unnecessarily
  :hook
    (org-mode . variable-pitch-mode)
    (org-mode . olivetti-mode)
    (org-mode . (lambda () (olivetti-set-width 120)))
  :config
  (progn
    (require 'org-protocol)
    ;; configuration
    ;;
    ;; General Config
    ;;
    (flyspell-mode 1) ;; enable on the fly spell checking
    
    (fringe-mode 2)
    
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch :background "gray10")
    (set-face-attribute 'org-level-1 nil :height 1.6 :foreground "#b7b8b9")
    (set-face-attribute 'org-level-2 nil :height 1.5 :foreground "#b7b8b9")
    (set-face-attribute 'org-level-3 nil :height 1.25 :foreground "#b7b8b9")
    (set-face-attribute 'org-level-4 nil :height 1.1 :foreground "#b7b8b9")
    
    ;; (set-face-attribute 'org-block-background nil :inherit 'fixed-pitch)
    
    ;; replace list indicators with bullet points
    (font-lock-add-keywords 'org-mode
                            '(("^ +\\([-*]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; (let* ((variable-tuple
    ;; 	    '(:font   "Noto Serif"))
    ;;        (base-font-color (face-foreground 'default nil 'default))
    ;;        (headline       `(:inherit default :weight bold :foreground ,base-font-color)))

    ;;   (custom-theme-set-faces
    ;;    'user
    ;;    `(org-level-8        ((t (,@headline ,@variable-tuple))))
    ;;    `(org-level-7        ((t (,@headline ,@variable-tuple))))
    ;;    `(org-level-6        ((t (,@headline ,@variable-tuple))))
    ;;    `(org-level-5        ((t (,@headline ,@variable-tuple))))
    ;;    `(org-level-4        ((t (,@headline ,@variable-tuple :height 1.1))))
    ;;    `(org-level-3        ((t (,@headline ,@variable-tuple :height 1.25))))
    ;;    `(org-level-2        ((t (,@headline ,@variable-tuple :height 1.5))))
    ;;    `(org-level-1        ((t (,@headline ,@variable-tuple :height 1.75))))
    ;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))
    ;;
    ;; Agenda config
    ;;
    ;; All the agenda/capture/refile stuff is heavily influenced by http://doc.norang.ca/org-mode.html
    (setq org-agenda-window-setup (quote current-window))
    (setq org-agenda-files (list (expand-file-name "inbox.org" org-directory)
                                 (expand-file-name "todo.org" org-directory)
                                 (expand-file-name "dump.org" org-directory)
                                 (expand-file-name "bookmarks.org" org-directory)
                                 (expand-file-name "projects/" org-directory)))
    
    ;; (add-hook 'org-mode-hook 'turn-on-auto-fill)
    
    (setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "BLOCKED(b!)" "|" "DONE(d)" "CANCELLED(c)")))
    
    (setq org-todo-keyword-faces
          (quote
           (("DONE" . success)
            ("CANCELLED" . default)
            ("STARTED" . underline)
            ("BLOCKED" . warning)
            ("TODO" . error))))

    ;; allow changing from any task todo state to another by selecting appropriate key from the menu
    ;; C-c C-t KEY
    (setq org-use-fast-todo-selection t)

    ;; set appropriate tags on tasks (used for filtering)
    (setq org-todo-state-tags-triggers
          '(("CANCELLED" ("CANCELLED" . t))
            ("BLOCKED" ("WAITING" . t))
            (done ("WAITING"))
            ("TODO" ("WAITING") ("CANCELLED"))
            ("DONE" ("WAITING") ("CANCELLED"))))
    
    (setq org-capture-templates
          '(("t" "Todo"
             entry (file "inbox.org")
             "* TODO %? \nAdded: %T\n")
            ("m" "Multi-part Todo"
             entry (file "inbox.org")
             "* TODO %? [/]\nAdded: %T\n- [ ] ")
            ("n" "Note"
             entry (file "dump.org")
             "* %? :NOTE:\n%U\n")
            ("l" "Link"
             entry (file "bookmarks.org")
             "* %:annotation\n:PROPERTIES:\n:CREATED: %u\n:URL: %:link\n:END:\n%:initial\n%?"
             :empty-lines 1)))

    ;; Tags with fast selection keys
    (setq org-tag-alist (quote ((:startgroup)
                                ("@errand" . ?e)
                                ("@office" . ?o)
                                ("@home" . ?h)
                                (:endgroup)
                                ("WAITING" . ?w)
                                ("PERSONAL" . ?P)
                                ("WORK" . ?W)
                                ("NOTE" . ?n)
                                ("CANCELLED" . ?c))))

    ;; Bulk archive all done tasks in a file
    (defun gp/org-archive-done-tasks ()
      (interactive)
      (org-map-entries
       (lambda ()
         (org-archive-subtree)
         (setq org-map-continue-from (outline-previous-heading)))
       "/DONE" 'file))
    
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

    (defun gp/refile-to (file headline &optional arg)
      "Move current headline to specified location"
      (let ((pos (save-excursion
                   (find-file (expand-file-name file org-directory))
                   (org-find-exact-headline-in-buffer headline))))
        (org-refile arg nil (list headline file nil pos)))
      (switch-to-buffer (current-buffer)))

    (defun gp/refile-to-project-notes ()
      "Move current item to the notes section of a project file"
      (interactive)
      (let ((project_file (read-file-name "Project file: " (expand-file-name "projects" org-directory))))
        (gp/refile-to project_file "Notes")))

    (defun gp/refile-to-project-tasks ()
      "Move current item to the tasks section of a project file"
      (interactive)
      (let ((project_file (read-file-name "Project file: " (expand-file-name "projects" org-directory))))
        (gp/refile-to project_file "Tasks")))
    
    ;; (defun gp/refile-to-bookmarks ()
    ;;   "Move current headline to bookmarks"
    ;;   (interactive)
    ;;   (org-mark-ring-push)
    ;;   (gp/refile-to "bookmarks.org" "New")
    ;;   (org-mark-ring-goto))

    (defhydra gp/org-refile-hydra (:foreign-keys run)
      "Refile"
      ("t" (gp/refile-to-project-tasks) "Refile to project tasks")
      ("n" (gp/refile-to-project-notes) "Refile to project notes")
      ("d" (gp/org-archive-done-tasks) "Archive all done tasks")
      ;; ("o" (my/refile "shopping.org" "Office supplies") "Refile to Office supplies")
      ;; ("e" (my/refile "tasks.org" "Email tasks") "Email tasks")
      ;; ("r" (my/refile "tasks.org" "Research tasks") "Research tasks")
      ("j" org-refile-goto-last-stored "Jump to last refile")
      ("q" nil "cancel"))
    
    ;; Or whatever you want your keybinding to be
    ;; (global-set-key (kbd "C-c r") 'gp/org-refile-hydra/body)

    ;;
    ;; Custom agenda views
    ;;
    ;; do not dim blocked tasks
    ;; (setq org-agenda-dim-blocked-tasks nil)

    ;; compact the block agenda view
    (setq org-agenda-compact-blocks t)

    ;; custom agenda commands
    (setq org-agenda-custom-commands
          '((" " "Agenda"
             ((tags "REFILE"
                    ((org-agenda-overriding-header "Things to Refile")
                     (org-tags-match-list-sublevels nil)))
              (todo "STARTED"
                    ((org-agenda-overriding-header "In Progress")))
              (todo "BLOCKED"
                    ((org-agenda-overriding-header "Blocked Tasks")))
              
              (tags-todo "-REFILE/!-STARTED-BLOCKED"
                    ((org-agenda-overriding-header "Backlog")))
              nil)
             )))
    ))

(use-package ox-tufte
  :after (org)
  :defer t)

(use-package ox-gfm
  :after (org)
  :defer t)

(use-package org-bullets
  :after (org)
  :init
  (setq org-bullets-bullet-list '("◉")) ;; no bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-indent
  :ensure nil
  :diminish)

 ;; (use-package org-variable-pitch
 ;;   :after (org)
 ;;   :hook (org-mode . org-variable-pitch-minor-mode))
  
(provide 'init-org)
;;; init-org.el ends here
