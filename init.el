;;; init.el
;;
;; This is the first thing emacs loads
;; so we can specify all our customisations here
;; however, it makes sens to use emacs' own customisations functionality
;; (which you can access with M-x customize) for stylistic customisations
;; and use init.el for more complex stuff such as keybindings
;;
;; Define a custom location for the file emacs uses to store customisations:
(setq custom-file "~/.emacs.d/lisp/custom.el")

;;
;; perform any other configuration we require
;;

;; org-mode configuration
(require 'org)
;; automatically use org mode for .org, .org_archive and .txt files
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
;; standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; don't forget to load our custom file
(load custom-file)
