;;; init-flycheck --- configure flycheck globally
;;; Commentary:
;;; Code:
(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (flycheck-pos-tip-mode))
  
(provide 'init-flycheck)
;;; init-flycheck.el ends here
