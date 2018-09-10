;;; init-spaceline --- Configure spaceline mode
;;; Commentary:
;;; see https://github.com/TheBB/spaceline

;;; Code:
(use-package spaceline)

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-emacs-theme)
  (spaceline-helm-mode t))

(provide 'init-spaceline)
;;; init-spaceline.el ends here
