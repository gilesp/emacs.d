;; init-javascript-2019.el -- My latest attempt at doing [java|type]script development in emacs

;;; Commentary:
;; I need to be able to work with javascript and typescript, and also
;; handle mixed mode stuff like react jsx.

;; I think there are two main approaches to try:
;;
;; Option 1:
;; eglot, with javascript-typescript-langserver, web-mode for highlighting, company-mode for completion
;;
;; Option 2:
;; Tide & Indium.
;; Should I also use web-mode and company-mode or something else?
;; js2-mode & rjsx-mode? How to get mixed content working?
;;
;; I'm currently trialling Option 1

;; =============
;; Project Setup
;; =============
;;
;; You need to have a language server installed.
;; Currently I am using javascript-typescript-langserver
;; So do an npm install -g javascript-typescript-langserver
;;
;; Additionally, you will want to configure a tsconfig.json in the
;; root of your project, to tell the language server what sort of
;; project you're working on.
;;
;; For a react app, I have the following tsconfig.json:
;;
;; {
;;   "compilerOptions": {
;;     "target": "es2017",
;;     "module": "es2015",
;;     "moduleResolution": "node",
;;     "allowSyntheticDefaultImports": true,
;;     "noEmit": true,
;;     "checkJs": true,
;;     "allowJs": true,
;;     "jsx": "react",
;;     "lib": ["es2017", "dom"]
;;   },
;;   "exclude": [
;;     "dist",
;;     "node_modules"
;;   ],
;;   "include": [
;;     "src/**/*.js"
;;   ]
;; }
;;
;; But obviously, you'll need to modify accordingly.
;;
;; ============
;; Known Issues
;; ============
;;
;; javascript-typescript-langserver doesn't support formatting, so
;; eglot-format fails. the alternative langserver
;; (typescript-language-server) does support formatting, but insists
;; on using tabs. Plus I ran into issues with it reporting errors that
;; didn't exist after multiple edits.

;;; Code:

;; Language features

;; Eglot uses the major mode to determine which server to start
;; since web-mode handle multiple files, eglot can't know which one we want.
;; Until eglot is enhanced (https://github.com/joaotavora/eglot/issues/47) we need
;; to create trivial sub-modes for each language server
(define-derived-mode gp-vue-mode web-mode "Vue")
(define-derived-mode gp-js-mode web-mode "Javascript")

;; Configure eglot
;;
;; NOTE: Need to do an `npm install -g javascript-typescript-langserver` for eglot to work with javascript
(use-package eglot
  :defer t
  :hook ((gp-js-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(gp-js-mode . ("javascript-typescript-stdio"))))

;; Syntax highlighting etc.
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . gp-js-mode)
         ("\\.jsx\\'" . gp-js-mode)
         ("\\.ts\\'" . gp-js-mode)
         ("\\.tsx\\'" . gp-js-mode)
         ("\\.html\\'" . web-mode)
         ("\\.vue\\'" . gp-vue-mode)
	 ("\\.json\\'" . web-mode))
  :commands web-mode
  :hook ((web-mode-hook . company-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-part-face t)
  (setq web-mode-content-types-alist
	'(("jsx" . "\\.js[x]?\\'")))
  )

(provide 'init-javascript-2019)
;;; init-javascript-2019.el ends here
