;;; init-flycheck --- configure flycheck globally
;;; Commentary:
;;; Code:
(require-package 'flycheck)

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
