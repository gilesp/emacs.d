;; init-lsp.el -- Lsp mode configuration

;;; Commentary:
;; configure lsp-mode
;; This relies on a suitable lspserver being present.
;; install javascript-typescript-lspserver with this command:
;; npm install -g javascript-typescript-langserver
;;

;;; Code:

(use-package lsp-mode :commands lsp)
(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setf lsp-ui-sideline-enable nil)
  (setf lsp-ui-imenu-enable t)
  (setf lsp-ui-doc-enable t)
  (setf lsp-prefer-flymake t)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
(use-package company-lsp :commands company-lsp)

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
  ;;(add-hook 'java-mode-hook  'lsp-ui-sideline-mode)
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
;;
;; NOTE: For this to work, you need to run the following commands:
;; npm install -g typescript
;; npm install -g typescript-language-server
;;
(use-package web-mode
  :defer t
  :mode "\\.html\\'")

(use-package js2-mode
  :defer t
  :mode
  ("\\.js$" . js2-mode)
  ("\\.json$" . json-mode)
  :config
  (setq js-indent-level 2)
  (setq js2-basic-offset 2))

(use-package add-node-modules-path
  :hook (js2-mode . add-node-modules-path))

(use-package rjsx-mode
  :defer t
  :after js2-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
