;;; init-lang-javascript -- configure javascript support

;;; Commentary:
;; An alternative (hopefully lighter touch) approach to init-webdev.el

;;; Code:

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
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defvar flycheck-javascript-eslint-executable)
(defun gp/setup-local-eslint ()
  "If ESLint found in node_modules directory - use that for flycheck.
 Intended for use in PROJECTILE-AFTER-SWITCH-PROJECT-HOOK."
  (interactive)
  (let ((local-eslint (expand-file-name "./node_modules/.bin/eslint")))
    (setq flycheck-javascript-eslint-executable
          (and (file-exists-p local-eslint) local-eslint))))

;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :bind (:map js2-mode-map
              (("C-x C-e" . js-send-last-sexp)
               ("C-M-x" . js-send-last-sexp-and-go)
               ("C-c C-b" . js-send-buffer-and-go)
               ("C-c C-l" . js-load-file-and-go)))
  :mode
  ("\\.js$" . js2-mode)
  ("\\.json$" . js2-jsx-mode)
  :config
  (custom-set-variables '(js2-strict-inconsistent-return-warning nil))
  (custom-set-variables '(js2-strict-missing-semi-warning nil))

  (setq js-indent-level 2)
  (setq js2-indent-level 2)
  (setq js2-basic-offset 2)

  (use-package nvm
    :init
    (nvm-use (caar (last (nvm--installed-versions)))))
  
  
  ;; tern :- IDE like features for javascript and completion
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package tern
    :config
    (defun gp/js-mode-hook ()
      "Hook for `js-mode'."
      (set (make-local-variable 'company-backends)
           '((company-tern company-files)))
      (flycheck-select-checker 'javascript-eslint)
      (flycheck-mode)
      (company-mode))
    (add-hook 'js2-mode-hook 'gp/js-mode-hook))

  (add-hook 'js2-mode-hook 'tern-mode)

  (add-hook 'flycheck-mode-hook #'gp/setup-local-eslint)
  
  ;; company backend for tern
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package company-tern)

  ;; Run a JavaScript interpreter in an inferior process window
  ;; https://github.com/redguardtoo/js-comint
  (use-package js-comint
    :config
    (setq inferior-js-program-command "node"))

  ;; js2-refactor :- refactoring options for emacs
  ;; https://github.com/magnars/js2-refactor.el
  (use-package js2-refactor :defer t
    :diminish js2-refactor-mode
    :config
    (js2r-add-keybindings-with-prefix "C-c j r"))
  (add-hook 'js2-mode-hook 'js2-refactor-mode))

(provide 'init-lang-javascript)
;;; init-lang-javascript ends here
