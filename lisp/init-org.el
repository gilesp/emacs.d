;; init-org --- Configure Org Mode

;;; Commentary:
;;
;; load org-mode from source
;; You first need to check it out from git (git://orgmode.org/org-mode.git)
;; and build it with "make uncompiled"

;;; Code:

(require-package 'org-bullets)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/org-mode/contrib/lisp"))

;; (require 'org)

;; automatically use org mode for .org, .org_archive and .txt files
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; standard key bindings
(global-set-key "\C-ca" 'org-agenda)
;; see the custom-file for the agenda files configuration
;; I'm setting it to use all files in a specified directory

;; capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "todo.org" "Tasks")
         "* TODO [#B] %?")))

(setq org-mobile-directory "~/Documents/Dropbox/org/mobile")
(setq org-mobile-inbox-for-pull "~/Documents/Dropbox/org/mobilesinbox.org")

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

  ;; actually emphasise text (e.g. show as italic instead of /italic/)
  (setq org-hide-emphasis-markers t)

  ;; syntax highlight code blocks
  (setq org-src-fontify-natively t)

  ;; replace list indicators with bullet points
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


  (require 'org-bullets)
  (org-bullets-mode 1)

  ;;set variable pitch font for org, but keep fixed width for code etc.
  (variable-pitch-mode t)
  (setq line-spacing 3)
  )

(add-hook 'org-mode-hook 'gp-org-mode-hook)

(defun gp-adjoin-to-list-or-symbol (element list-or-symbol)
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    (require 'cl-lib)
    (cl-adjoin element list)))

(eval-after-load "org"
  '(mapc
    (lambda (face)
      (set-face-attribute
       face nil
       :inherit
       (gp-adjoin-to-list-or-symbol
        'fixed-pitch
        (face-attribute face :inherit))))
    (list 'org-code 'org-block 'org-block-begin-line 'org-block-end-line 'org-verbatim 'org-macro 'org-table)))

(provide 'init-org)
;;; init-org.el ends here
