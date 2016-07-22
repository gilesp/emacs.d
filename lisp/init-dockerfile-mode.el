;;; init-dockerfile-mode --- Load and configure dockerfile-mode
;;; Commentary:
;; Load and configure dockerfile-mode

;;; Code:
(require-package 'dockerfile-mode)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(provide 'init-dockerfile-mode)
;;; init-dockerfile-mode.el ends here
