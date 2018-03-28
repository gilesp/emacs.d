;;; init-packages --- utility functions to ensure packages are loaded

;;; Commentary:
;;; Code:
(require 'package)

(setq package-enable-at-startup nil)

;; Setup package repositores
(add-to-list 'package-archives '("melpa" ."https://melpa.org/packages/"))
;;(add-to-list 'package-archives '("melpa-stable" ."http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

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

(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

;; Enable use of common lisp functions
;;(require-package 'cl-lib)
;;(require 'cl-lib)

(provide 'init-packages)
;;; init-packages ends here
