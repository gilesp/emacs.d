;;; init-packages --- utility functions to ensure packages are loaded

;;; Commentary:
;;; Code:
(require 'package)

(setq package-enable-at-startup nil)

;; Setup package repositores
(add-to-list 'package-archives '("melpa" ."http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" ."http://stable.melpa.org/packages/"))

(package-initialize)


;; Utility function to load packages when required by other config files
(defun require-package (package)
  "Ensure requested PACKAGE is installed."
  (if (package-installed-p package)
      t
    (progn
      (package-refresh-contents)
      (package-install package))))

;; Bootstrap use-package
(require-package 'use-package)

;; Enable use of common lisp functions
;;(require-package 'cl-lib)
;;(require 'cl-lib)

(provide 'init-packages)
;;; init-packages ends here
