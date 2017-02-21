;;; init.el --- Load all the things!

;;; Commentary:

;; This is the first thing Emacs loads, so we can specify all our
;; customisations here however, it makes sense to use Emacs' own
;; customisations functionality (which you can access with M-x customize)
;; for stylistic customisations and use init.el for more complex stuff such
;; as keybindings or specific feature configurations
;;

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; make the Lisp directory available to easily load sub-files from
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; store any non-packaged lisp code in a local site-lisp
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; to avoid flycheck warnings when requiring files, inherit load-path
(setq-default flycheck-emacs-lisp-load-path 'inherit)

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

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Enable auto reloading when files change (will prompt for confirmation)
(global-auto-revert-mode t)

;; Enable overwriting selected text
(delete-selection-mode t)

;; y or n instead of yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; enable visual line mode with indicators
;(visual-line-mode)
;(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;(setq fill-column 80)

;;-------------------------------------------------------------------------
;; Load any other configuration we require
;;
;; There will be a corresponding file in the lisp directory
;; Doing it this way keeps init.el clean and makes it more obvious
;; what each bit of configuration refers to
;;-------------------------------------------------------------------------

(require 'init-server)
(require 'init-packages)
(require 'init-editing)
(require 'init-linenumbers)
(require 'init-autocompletion)

;; (require 'init-exec-path-from-shell)
(require 'init-flycheck)
(require 'init-theme)
(require 'init-switch-window)
(require 'init-helm)
(require 'init-projectile)
(require 'init-duplicateline)
(require 'init-browse-url-chrome)
(require 'init-modeline)
(require 'init-org)
(require 'init-markdown)
(require 'init-zoom)
(require 'init-spaceline)
(require 'init-which-key)
(require 'init-neotree)
(require 'init-magit)
(require 'init-hugo-blogging)
(require 'init-yasnippet)
;; (require 'init-typescript)

;; (require 'init-google-this)

;; load machine specific init file
;; it looks for a file names <hostname>.el and loads it if present
;; use it for require-ing different configs for each machine you use
(defvar gilesp-local-filename (concat system-name ".el"))
(defvar gilesp-local-file (expand-file-name gilesp-local-filename user-emacs-directory))
(when (file-readable-p gilesp-local-file)
  (load-file gilesp-local-file))

(provide 'init)
;;; init.el ends here
