;;
;; org-mode configuration
;;
;; load org-mode from source
;; You first need to check it out from git (git://orgmode.org/org-mode.git)
;; and build it with "make uncompiled"
(add-to-list 'load-path (expand-file-name "~/.emacs.d/org-mode/lisp"))
;; automatically use org mode for .org, .org_archive and .txt files
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)
;; standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; see the custom-file for the agenda files configuration
;; I'm setting it to use all files in a specified directory

(provide 'init-org)
