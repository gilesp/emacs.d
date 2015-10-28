;;; init-jsx --- Configure highlighting for jsx files

(require-package 'flycheck)
(require-package 'js2-mode)
(require-package 'json-mode)
(require-package 'web-mode)

(require 'flycheck)
(require 'web-mode)

;; Activate automatically for .jsx and .js files
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))

;; Ensure web-mode uses the jsx content type for js files
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-css-colorization t)
  (setq indent-tabs-mode nil)
  (subword-mode)
  (js2-minor-mode)

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
	(let ((web-mode-enable-part-face nil))
	  ad-do-it)
      ad-do-it))

  (flycheck-mode)
  ;; disable jshint so we can use eslint instead
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; remove trailing whitespace
  (add-hook 'local-write-file-hooks
	    (lambda()
	      (delete-trailing-whitespace)
	      nil))
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)


;; adjust indents for json-mode to 2 spaces
(defun my-json-mode-hook ()
  "hooks for json-mode."
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(add-hook 'json-mode-hook 'my-json-mode-hook)
 
(provide 'init-jsx)
;;; init-jsx.el ends here.
