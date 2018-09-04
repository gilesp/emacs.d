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

;; (use-package writegood-mode
;;   :bind (("C-c C-g g" . writegood-grade-level)
;;          ("C-c C-g e" . writegood-reading-ease)))

(setq package-check-signature nil)

(use-package olivetti
  :diminish)

;; :ensure org-plus-contrib
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c r" . gp/org-refile-hydra/body))
  :pin org ;; Use version from orgmode.org/elpa instead of gnu
  :preface
  ;; hook to run when org-mode is started, to turn on certain modes etc.
  (defun gp/org-mode-hook ()
    ;; disable linum
    (linum-mode -1)

    ;; enable on the fly spell checking
    (flyspell-mode 1)

    ;; center the text area and set a minimal fringe
    (olivetti-mode)
    (olivetti-set-width 120)
    (fringe-mode 2))
  :hook (org-mode . gp/org-mode-hook)
  :init
  (setq org-directory "~/Documents/Dropbox/org")
  (setq org-default-notes-file (expand-file-name "dump.org" org-directory))
  (setq org-startup-with-inline-images t)
  (setq org-completion-use-ido nil) ;; so we can use helm for refiling
  (setq org-outline-path-complete-in-steps nil) ;; so we can use helm for refiling
  :config
  (progn
    ;; This functionality isn't in a package yet.
    ;; I installed it into ~/.emacs.d/site-lisp with wget https://raw.githubusercontent.com/alphapapa/org-protocol-capture-html/master/org-protocol-capture-html.el
    ;; (require 'org-protocol-capture-html)
    (require 'org-protocol)
    
    ;; configuration

    ;;
    ;; General Config
    ;;
    
    ;; edit src blocks in place, rather than a new window
    (setq org-src-window-setup 'current-window)
    
    ;; actually emphasise text (e.g. show as italic instead of /italic/)
    (setq org-hide-emphasis-markers t)

    ;; syntax highlight code blocks
    (setq org-src-fontify-natively t)

    ;; replace list indicators with bullet points
    (font-lock-add-keywords 'org-mode
                            '(("^ +\\([-*]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
    
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
  (setq org-bullets-bullet-list
        '("⚫" "◉" "◎" "○"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'init-org)
;;; init-org.el ends here
