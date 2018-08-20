;; init-lsp.el -- Lsp mode configuration

;;; Commentary:
;; configure lsp-mode
;; This relies on a suitable lspserver being present.
;; install javascript-typescript-lspserver with this command:
;; npm install -g javascript-typescript-langserver
;;

;;; Code:
(defun gp/set-projectile-root ()
  (when lsp--cur-workspace
    (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))

;;NOTE: javascript-typescript-langserver doesn't take into account the
;;completion prefix, which causes some glitchy completion when using
;;company. lsp-javascript-typescript doesn't handle this yet. For now
;;the following can be used as a fix:

(defun gp/company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun gp/js-hook nil
  (make-local-variable 'company-transformers)
  (push 'gp/company-transformer company-transformers))

(add-hook 'js-mode-hook 'gp/js-hook)

(use-package lsp-mode
  :defer t
  :hook (lsp-before-open . gp/set-projectile-root))

;; `company' backend for `lsp-mode'
(use-package company-lsp
  :after company lsp-mode
  :init
  (push 'company-lsp company-backends))

(use-package web-mode
  :defer t
  :mode "\\.html\\'")

(use-package js2-mode
  :defer t
  :mode "\\.js\\'"
  :hook (js2-mode . gp/js-hook)
  :init
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun gp/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint)))))

(use-package add-node-modules-path
  :hook (js2-mode . add-node-modules-path))

(use-package rjsx-mode
  :defer t
  :after js2-mode)

(use-package lsp-javascript-typescript
  :requires lsp-mode
  :defer t
  :hook ((js-mode
          typescript-mode
          js2-mode
          rjsx-mode) .
          lsp-javascript-typescript-enable))

(provide 'init-lsp)
;;; init-lsp.el ends here
