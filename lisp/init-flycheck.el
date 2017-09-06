;;; init-flycheck --- configure flycheck globally
;;; Commentary:
;;; Code:
(use-package flycheck
  :init
  (global-flycheck-mode)
  :diminish flycheck-mode)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
