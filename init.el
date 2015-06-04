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

;; perform any other configuration we require

;; don't forget to load our custom file
(load custom-file)
