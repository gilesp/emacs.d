;;; initeglot --- Configure Language Server Protocol support
;;; Commentary:
;;; Use eglot for development, using language server protocol

;;; Code:
(use-package eglot
  :defer t
  :bind (:map eglot-mode-map
              ("C-c h" . eglot-help-at-point)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename))
  :hook ((js2-mode . eglot-ensure)
	 (java-mode . eglot-ensure)))

(use-package web-mode
  :defer t
  :mode "\\.html\\'")

(use-package js2-mode
  :defer t
  :mode "\\.js$"
  :init
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
  :config
  (setq js2-basic-offset 2)
  (add-hook 'flycheck-mode-hook #'gp/use-eslint-from-node-modules))

(use-package js2-refactor
  :after (js2-mode hydra)
  :hook (js2-mode . js2-refactor-mode)
  :bind ("C-c <C-m>" . js2-refactor-hydra/body)
  :config
  ;; Hydra copied from https://gist.github.com/anachronic/7af88c62db136727cd1fed17ee0a662f
  (defhydra js2-refactor-hydra (:color blue :hint nil)
    "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_q_]  quit"
    ("ee" js2r-expand-node-at-point)
    ("cc" js2r-contract-node-at-point)
    ("ef" js2r-extract-function)
    ("em" js2r-extract-method)
    ("tf" js2r-toggle-function-expression-and-declaration)
    ("ta" js2r-toggle-arrow-function-and-expression)
    ("ip" js2r-introduce-parameter)
    ("lp" js2r-localize-parameter)
    ("wi" js2r-wrap-buffer-in-iife)
    ("ig" js2r-inject-global-in-iife)
    ("ag" js2r-add-to-globals-annotation)
    ("ev" js2r-extract-var)
    ("iv" js2r-inline-var)
    ("rv" js2r-rename-var)
    ("vt" js2r-var-to-this)
    ("ao" js2r-arguments-to-object)
    ("ti" js2r-ternary-to-if)
    ("sv" js2r-split-var-declaration)
    ("ss" js2r-split-string)
    ("uw" js2r-unwrap)
    ("lt" js2r-log-this)
    ("dt" js2r-debug-this)
    ("sl" js2r-forward-slurp)
    ("ba" js2r-forward-barf)
    ("k" js2r-kill)
    ("q" nil)))

(use-package rjsx-mode
  :defer t
  :after js2-mode)

(provide 'init-eglot)
;;; init-eglot.el ends here
