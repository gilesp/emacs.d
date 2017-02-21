;;; init-projectile.el --- Configuration for projectile

;;; Commentary:
;; This is the main configuration for projectile, but there is also
;; helm config in init-helm.el

;;; Code:
(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'helm)
    (setq projectile-enable-caching t)
    (add-to-list 'projectile-globally-ignored-files "node-modules"))
  :config
  (projectile-global-mode))

;; enable the helm versions of projectile functions
(use-package helm-projectile
  :config
  (helm-projectile-on))

(provide 'init-projectile)
;;; init-projectile.el ends here
