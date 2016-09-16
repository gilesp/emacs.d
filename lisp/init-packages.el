;;; init-packages --- utility functions to ensure packages are loaded

;;; Commentary:
;;; Code:
(require 'package)

(setq package-enable-at-startup nil)

;; Setup package repositores
(add-to-list 'package-archives '("melpa" ."http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" ."http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; Utility function to load packages when required by other config files
(defun require-package (package)
  "DEPRECATED - use use-package instead Ensure requested PACKAGE is installed."
  (if (package-installed-p package)
      t
    (progn
      (package-refresh-contents)
      (package-install package))))

;; Bootstrap use-package
(require-package 'use-package)
;; makesure use-package auto install packages
(setq use-package-always-ensure t)

;; Enable use of common lisp functions
;;(require-package 'cl-lib)
;;(require 'cl-lib)

(provide 'init-packages)
;;; init-packages ends here
