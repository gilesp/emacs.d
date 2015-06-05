;;
;; This is the first thing emacs loads, so we can specify all our 
;; customisations here however, it makes sens to use emacs' own 
;; customisations functionality (which you can access with M-x customize)
;; for stylistic customisations and use init.el for more complex stuff such 
;; as keybindings or specific feature configurations
;;

;; make the lisp directory available to easily load sub-files from
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Define a custom location for the file emacs uses to store customisations:
;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;-------------------------------------------------------------------------
;; Load any other configuration we require
;;
;; There will be a corresponding file in the lisp directory
;; Doing it this way keeps init.el clean and makes it more obvious
;; what each bit of configuration refers to
;;-------------------------------------------------------------------------

(require 'init-org)

;;
;; Make "C-x o" prompt for a target window when more than 2 are available
;(require-package 'switch-window)
;(require 'switch-window)
;(setq switch-window-shortcut-style 'alphabet)
;(global-set-key (kbd "C-x o") 'switch-window)
