;; yaml-mode

(require-package 'yaml-mode)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(provide 'init-yaml-mode)
