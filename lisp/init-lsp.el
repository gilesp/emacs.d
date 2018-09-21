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
  :ensure t
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

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     nil
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

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
