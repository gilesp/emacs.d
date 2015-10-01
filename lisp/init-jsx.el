;;; init-jsx --- Configure highlighting for jsx files

(require-package 'flycheck)
(require-package 'js2-mode)
(require-package 'json-mode)
(require-package 'web-mode)

(require 'flycheck)
(require 'web-mode)

;; Activate automatically for .jsx, .android.js and .ios.js files
(add-to-list 'auto-mode-alist '("\\.js[x]$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.android.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ios.js$" . web-mode))

(setq web-mode-engines-alist '(("jsx" . "\\.js[x]$")
			       ("jsx" . "\\.android.js$")
			       ("jsx" . "\\.ios.js$")))

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]$")
				     ("jsx" . "\\.android.js$")
				     ("jsx" . "\\.ios.js$")))


;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-css-colorization t)
  (subword-mode)
  (js2-minor-mode)
  ;;(flycheck-mode)
  (add-hook 'local-write-file-hooks
	    (lambda()
	      (delete-trailing-whitespace)
	      nil))
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; disable jshint so we can use eslint instead
;;(setq-default flycheck-disabled-checkers
;;	      (append flycheck-disabled-checkers
;;		      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
;;(flycheck-add-mode 'javascript-eslint 'web-mode)


(provide 'init-jsx)
;;; init-jsx.el ends here.
