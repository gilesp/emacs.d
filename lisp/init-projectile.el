;;; init-projectile.el --- Configuration for projectile

;;; Commentary:
;; This is the main configuration for projectile, but there is also
;; helm config in init-helm.el

;;; Code:
(use-package projectile
  :demand
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :defines projectile-globally-ignored-files
  :init
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'projectile-vc)
  :config
  (add-to-list 'projectile-globally-ignored-files "node-modules")
  (projectile-mode))

;; enable the helm versions of projectile functions
(use-package helm-projectile
  :config
  (helm-projectile-on))

(provide 'init-projectile)
;;; init-projectile.el ends here
