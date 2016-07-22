;;; init-flycheck --- configure flycheck globally
;;; Commentary:
;;; Code:
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
