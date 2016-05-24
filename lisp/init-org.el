;; init-org --- Configure Org Mode

;;; Commentary:
;;
;; load org-mode from source
;; You first need to check it out from git (git://orgmode.org/org-mode.git)
;; and build it with "make uncompiled"

;;; Code:
(add-to-list 'load-path (expand-file-name "~/.emacs.d/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/org-mode/contrib/lisp"))

;; (require 'org)

;; automatically use org mode for .org, .org_archive and .txt files
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; see the custom-file for the agenda files configuration
;; I'm setting it to use all files in a specified directory


(defun gp-org-mode-hook ()
  "Org mode startup hook."
  ;; Require markdown export mode
  (require 'ox-md nil t)
  ;; Require github flavoured markdown export mode
  (require 'ox-gfm)
  ;; Require html export mode
  (require 'ox-html nil t)
  ;; Require OpenDocument Text export mode
  (setq org-odt-schema-dir "~/.emacs.d/org-mode/etc/schema")
  (setq org-odt-styles-dir "~/.emacs.d/org-mode/etc/styles")
  
  (require 'ox-odt nil t)
  
  (turn-on-auto-fill)
  )

(add-hook 'org-mode-hook 'gp-org-mode-hook)

(provide 'init-org)
;;; init-org.el ends here
