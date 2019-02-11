;; init-javascript-typescript.el -- An attempt to get javascript, typescript, jsx and tsx editing configured

;;; Commentary:
;; The aim is to use tide as a language server (I've tried eglot and
;; lsp but each have their own issues, while tide has been rock
;; solid) and js2-mode, typescript-mode and web-mode as required.

;;; Code:

(defun gp/setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun gp/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package tide
  :functions tide-setup
  :after (typescript-mode company flycheck)
  :hook (typescript-mode . gp/setup-tide-mode)
  :config
  (setq tide-sort-completions-by-kind t))

(use-package web-mode
  :mode (("\\.jsx$" . web-mode)
	 ("\\.tsx$" . web-mode)
	 ("\\.html$" . web-mode)
	 ("\\.json$" . web-mode))
  :commands web-mode
  :init
  (defun gp/web-mode-jsx-setup ()
    "Configure web-mode to support jsx and tide properly"
    (gp/setup-tide-mode)
    (add-hook 'flycheck-mode-hook #'gp/use-eslint-from-node-modules)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))
  (defun gp/web-mode-tsx-setup ()
    "Configure web-mode to support tsx and tide properly"
    (gp/setup-tide-mode)
    (add-hook 'flycheck-mode-hook #'gp/use-eslint-from-node-modules)
    (flycheck-add-mode 'typescript-tslint 'web-mode))
  :hook ((web-mode . (lambda()
		       (when (string-equal "jsx" (file-name-extension buffer-file-name))
			 (gp/web-mode-jsx-setup))))
	 (web-mode . (lambda()
		       (when (string-equal "tsx" (file-name-extension buffer-file-name))
			 (gp/web-mode-tsx-setup))))
	 (web-mode . company-mode))
  :config
  (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
  (setq web-mode-attr-indent-offset 2) ; web-mode, attributes in html file
  (setq web-mode-css-indent-offset 2) ; web-mode, css in html file
  (setq web-mode-code-indent-offset 2) ; web-mode, js code in html file
  ;; configure jsx-tide checker to run after your default jsx checker
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  )

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)
  (setq js2-basic-offset 2)
  (setq indent-tabs-mode nil)
  (add-hook 'flycheck-mode-hook #'gp/use-eslint-from-node-modules)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  ;;(flycheck-add-next-checker 'javascript-eslint 'append)
  (gp/setup-tide-mode))

(provide 'init-javascript-typescript)
;;; init-javascript-typescript.el ends here
