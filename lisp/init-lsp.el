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

;; (add-hook 'js-mode-hook 'gp/js-hook)

(use-package lsp-mode
  :defer t
  :init
  (setq lsp-inhibit-message nil ; you may set this to t to hide messages from message area
        lsp-eldoc-render-all nil
        lsp-highlight-symbol-at-point nil)
  :hook (lsp-before-open . gp/set-projectile-root))

;; company backend for lsp-mode
(use-package company-lsp
  :config
  (require 'company-lsp)
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet t
	company-lsp-cache-candidates t
	company-lsp-async t
	company-lsp-enable-recompletion t))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable nil
	lsp-ui-flycheck-enable t
	lsp-ui-doc-enable nil
	lsp-ui-peek-enable nil
	lsp-ui-imenu-enable nil
	lsp-ui-flycheck-live-reporting t)
  ;; (setq lsp-ui-sideline-enable t
  ;;       lsp-ui-sideline-show-symbol t
  ;;       lsp-ui-sideline-show-hover t
  ;;       lsp-ui-sideline-show-code-actions t
  ;;       lsp-ui-sideline-update-mode 'point)
  )

;; -----------------------------------------------
;; Java Config
;; -----------------------------------------------
(use-package lsp-java
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :config
  (add-hook 'java-mode-hook  'lsp-java-enable)
  (add-hook 'java-mode-hook  'flycheck-mode)
  (add-hook 'java-mode-hook  'company-mode)
  (add-hook 'java-mode-hook  (lambda () (lsp-ui-flycheck-enable t)))
  (add-hook 'java-mode-hook  'lsp-ui-sideline-mode)
  ;; can this be made dynamic, so that it's based on the projectile root? projectile-project-root
  ;; perhaps making use of the gp/set-projectile-root function?
  (setq lsp-java--workspace-folders (list "~/projects/available/ellis-brigham-smdb")))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-use-custom-font t))

;; -----------------------------------------------
;; Javascript Config
;; -----------------------------------------------
(use-package web-mode
  :defer t
  :mode "\\.html\\'")

(use-package js2-mode
  :defer t
  :mode
  ("\\.js$" . js2-mode)
  ("\\.json$" . json-mode)
  :hook (js2-mode . gp/js-hook)
  :init
  ;;NOTE: javascript-typescript-langserver doesn't take into account the
  ;;completion prefix, which causes some glitchy completion when using
  ;;company. lsp-javascript-typescript doesn't handle this yet. For now
  ;;the following can be used as a fix:
  (defun gp/company-transformer (candidates)
    (let ((completion-ignore-case t))
      (all-completions (company-grab-symbol) candidates)))
  (defun gp/js-hook nil
    (make-local-variable 'company-transformers)
    (push 'gp/company-transformer company-transformers)))

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
