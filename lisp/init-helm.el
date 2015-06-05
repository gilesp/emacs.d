;; Helm
;; See http://tudo.github.io/helm-intro.html for more details

(require-package 'helm)
(require 'helm)
(require 'helm-config)

;; Key Bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)

;; fuzzy matching for helm-mini
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

;; resize the helm buffer automatically
(helm-autoresize-mode t)
(helm-mode 1)

(provide 'init-helm)
