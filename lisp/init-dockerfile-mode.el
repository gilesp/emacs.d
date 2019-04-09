;;; init-dockerfile-mode --- Load and configure dockerfile-mode
;;; Commentary:
;; Load and configure dockerfile-mode

;;; Code:
(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :config
  (setq tab-width 4)
  (setq tab-stop-list (number-sequence 4 120 4)))

(provide 'init-dockerfile-mode)
;;; init-dockerfile-mode.el ends here
