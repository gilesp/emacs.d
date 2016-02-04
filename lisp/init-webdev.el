;;; init-webdev --- Configure web-mode
;;; Currently sets up indentation and highlighting in web-mode
;;; Uses eslint, flycheck and js2 modes
;;; autocomplete provided by tern.
;;; supports jsx

(require-package 'flycheck)
(require-package 'js2-mode)
(require-package 'json-mode)
(require-package 'web-mode)
(require-package 'tern)
(require-package 'tern-auto-complete)

(require 'flycheck)
(require 'web-mode)

(defun gp-setup-web-mode-indent (n)
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-attr-indent-offset 2) ; web-mode, attributes in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  )

(defun gp-setup-web-mode-auto-list ()
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; Activate automatically for .jsx and .js files
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))

  ;; Ensure web-mode uses the jsx content type for js files
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  )

;; adjust indents for web-mode to 2 spaces
(defun gp-web-mode-hook ()
  "Hooks for Web mode."

  ;; enable other modes that we need
  (subword-mode) ;; make things CamelCase aware
;;  (js2-minor-mode) ;; use js2 for highlighting
  (tern-mode) ;; use the tern server for refactoring and autocompletion
  (auto-complete-mode) ;; enable autocompleteion
  (flycheck-mode) ;;enable syntax validation

  (setq indent-tabs-mode nil) ;; spaces instead of tabs
  (gp-setup-web-mode-indent 2) ;; configure various indentation values

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
  (if (equal web-mode-content-type "jsx")
      (let ((flycheck-add-mode 'javascript-eslint 'web-mode))))

  (lambda ()
            (when (equal web-mode-content-type "html")
              ;; enable eslint flycheck
              (flycheck-select-checker 'html-tidy)))

  ;; remove trailing whitespace
  (add-hook 'local-write-file-hooks
	    (lambda()
	      (delete-trailing-whitespace)
	      nil))

  ;; For Angular, enable M-x imenu for navigation
  (eval-after-load 'web-mode
  '(progn
     ;; angular imenu
     (add-to-list 'web-mode-imenu-regexp-list
                  '(" \\(ng-[a-z]*\\)=\"\\([a-zA-Z0-9]*\\)" 1 2 "="))))

  ;; setup tern
  ;; Don't forget to create a .tern-project file in the root of your javascript project
  ;; http://ternjs.net/doc/manual.html#configuration
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)))

  ;; function to kill ther tern process incase it stops responding
  (defun delete-tern-process ()
    (interactive)
    (delete-process "Tern"))
  )
(add-hook 'web-mode-hook  'gp-web-mode-hook)


;; adjust indents for json-mode to 2 spaces
(defun my-json-mode-hook ()
  "hooks for json-mode."
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(add-hook 'json-mode-hook 'gp-json-mode-hook)

(gp-setup-web-mode-auto-list)

(provide 'init-webdev)
;;; init-jsx.el ends here.
