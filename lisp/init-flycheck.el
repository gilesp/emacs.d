;;; init-flycheck --- configure flycheck globally
;;; Commentary:
;;; Code:
(use-package flycheck
  :defer 1
  :diminish flycheck-mode
  :hook
  (after-init . global-flycheck-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
