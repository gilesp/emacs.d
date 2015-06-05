(require 'package)

;; Setup package repositores
(add-to-list 'package-archives '("melpa" ."http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" ."http://stable.melpa.org/packages/"))

;; Utility function to load packages when required by other config files
(defun require-package (package)
  "Install given package"
  (if (package-installed-p package)
      t
    (progn
      (package-refresh-contents)
      (package-install package))))

(setq package-enable-at-startup nil)
(package-initialize)

(require-package 'cl-lib)
(require 'cl-lib)

(provide 'init-packages)
