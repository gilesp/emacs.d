;;; init.el --- Load all the things!

;;; Commentary:

;; This is the first thing Emacs loads, so we can specify all our
;; customisations here however, it makes sense to use Emacs' own
;; customisations functionality (which you can access with M-x customize)
;; for stylistic customisations and use init.el for more complex stuff such
;; as keybindings or specific feature configurations
;;

;;; Code:
;; make the Lisp directory available to easily load sub-files from
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Define a custom location for the file Emacs uses to store customisations:
;; Keep Emacs Custom-settings in separate file
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;-------------------------------------------------------------------------
;; Trivial configuration
;;
;; Move to a separate init file when more than a couple of lines required
;;-------------------------------------------------------------------------

;; Enable auto reloading when files change (will prompt for confirmation)
(global-auto-revert-mode t)

;; Enable overwriting selected text
(delete-selection-mode t)

;;-------------------------------------------------------------------------
;; Load any other configuration we require
;;
;; There will be a corresponding file in the lisp directory
;; Doing it this way keeps init.el clean and makes it more obvious
;; what each bit of configuration refers to
;;-------------------------------------------------------------------------

(require 'init-server)
(require 'init-packages)

;; (require 'init-exec-path-from-shell)
(require 'init-flycheck)
(require 'init-theme)
(require 'init-org)
(require 'init-switch-window)
(require 'init-helm)
(require 'init-duplicateline)
(require 'init-browse-url-chrome)
(require 'init-modeline)

(require 'init-zoom)
(require 'init-spaceline)
(require 'init-which-key)
(require 'init-neotree)
(require 'init-magit)
(require 'init-hugo-blogging)
(require 'init-linenumbers)
;; (require 'init-google-this)

;; load machine specific init file
;; it looks for a file names <hostname>.el and loads it if present
;; use it for require-ing different configs for each machine you use
(setq gilesp-local-filename (concat system-name ".el"))
(setq gilesp-local-file (expand-file-name gilesp-local-filename user-emacs-directory))
(when (file-readable-p gilesp-local-file)
  (load-file gilesp-local-file))

(provide 'init)
;;; init.el ends here
