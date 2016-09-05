;;; init-webdev --- Configure web-mode
;;; Commentary:
;;; Currently sets up indentation and highlighting in web-mode
;;; Uses eslint, flycheck and js2 modes
;;; autocomplete provided by tern.
;;; supports jsx

;;; Code:
(require-package 'js2-mode)
(require-package 'json-mode)
(require-package 'web-mode)
(require-package 'tern)
(require-package 'company-tern)
(require-package 'color-identifiers-mode)
(require-package 'origami)
(require-package 'sass-mode)

(require 'web-mode)
(require 'js2-mode)
(require 'json-mode)
(require 'flycheck)
(require 'tern)
(require 'company-tern)
(require 'color-identifiers-mode)
(require 'origami)
(require 'sass-mode)

;; I need to use a combination of web-mode and js2-mode as web-mode
;; and eslint aren't compatible with each other. I use web-mode for
;; html etc. and jsx files (which are mixed content) but js2-mode for
;; pure javascript. json-mode is used for JSON. Amazing.

(setq-default js2-global-externs '("require"))

(defun gp-js2-mode-hook ()
  "Setup js2-mode as I like it."
  (tern-mode)
  (subword-mode)
  (color-identifiers-mode)
  (origami-mode)
  (setq indent-tabs-mode nil)
  (gp-setup-webdev-indent 2)
  (setq js2-bounce-indent-p t)
  (setq js2-highlight-external-variables nil)
  (setq js2-mode-show-parse-errors nil)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-select-checker 'javascript-eslint)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(json-jsonlist)))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (add-to-list 'company-backends 'company-tern)
  
  ;; we're using eslint for parsing, so js2-can shut up
  (setq js2-mode-show-parse-errors nil)
  
  ;; remove trailing whitespace
  (add-hook 'local-write-file-hooks
	    (lambda()
	      (delete-trailing-whitespace)
	      nil))

  (define-key ctl-x-map "f"
    `origami-recursively-toggle-node)
  
  ;; For Angular, enable M-x imenu for navigation
  (eval-after-load 'js2-mode
    '(progn
       ;; angular imenu
       (add-to-list 'web-mode-imenu-regexp-list
                    '(" \\(ng-[a-z]*\\)=\"\\([a-zA-Z0-9]*\\)" 1 2 "="))))  
  )

(defun gp-tern-setup ()
  "Setup tern.
Don't forget to create a .tern-project file in the root of your
javascript project.
http://ternjs.net/doc/manual.html#configuration"
  (add-hook 'js2-mode-hook (lambda () (gp-js2-mode-hook)))

  ;; function to kill ther tern process incase it stops responding
  (defun delete-tern-process ()
    (interactive)
    (delete-process "Tern"))
  )

(defun gp-setup-webdev-indent (n)
  "Set indentation for webdev related modes to N spaces."
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-attr-indent-offset 2) ; web-mode, attributes in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  (setq sass-indent-ffset n) ; sass-mode
  )

(defun gp-setup-webdev-auto-list ()
  "Configure file mode mapping for web-mode and js2-mode."
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; Use web-mode for jsx files (mixed content)
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  ;; Ensure web-mode uses the jsx content type for js files
  (setq web-mode-content-types-alist '(("jsx" . "\\.jsx?\\'")))
  
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
  )

;; adjust indents for web-mode to 2 spaces
(defun gp-web-mode-hook ()
  "Hooks for Web mode."

  ;; enable other modes that we need
  (subword-mode) ;; make things CamelCase aware
  ;;  (js2-minor-mode) ;; use js2 for highlighting
  (tern-mode) ;; use the tern server for refactoring and autocompletion

  (setq indent-tabs-mode nil) ;; spaces instead of tabs
  (gp-setup-webdev-indent 2) ;; configure various indentation values

  (setq web-mode-enable-css-colorization t)

  ;; enable proper highlight of mixed jsx content
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
	(let ((web-mode-enable-part-face nil))
	  ad-do-it)
      ad-do-it))

  ;; disable jshint so we can use eslint instead
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))

  ;; enable eslint for flycheck when in web-mode
  (lambda ()
    (when (equal web-mode-content-type "jsx")
      (flycheck-select-checker 'javascript-eslint)))
  
  (lambda ()
    (when (equal web-mode-content-type "html")
      (flycheck-select-checker 'html-tidy)))

  ;; remove trailing whitespace
  (add-hook 'local-write-file-hooks
	    (lambda()
	      (delete-trailing-whitespace)
	      nil))
  )
(add-hook 'web-mode-hook  'gp-web-mode-hook)


;; adjust indents for json-mode to 2 spaces
(defun gp-json-mode-hook ()
  "Hooks for json-mode."
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(add-hook 'json-mode-hook 'gp-json-mode-hook)

(gp-setup-webdev-auto-list)
(gp-tern-setup)

(provide 'init-webdev)
;;; init-webdev.el ends here
