;;; init-helm.el --- Configuration for Helm

;;; Commentary:
;;; See http://tudo.github.io/helm-intro.html for more details

;;; Code:
(require-package 'helm)
(require-package 'helm-ls-git)
(require-package 'helm-descbinds)
(require 'helm)
(require 'helm-config)
(require 'helm-ls-git)
(require 'helm-descbinds)

;; Key Bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

;; fuzzy matching for helm-mini
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

;; fuzzy matching for helm-apropos
;; C-x c a
(setq helm-apropos-fuzzy-match t)

;; resize the helm buffer automatically
(helm-autoresize-mode t)
(helm-mode 1)
(helm-descbinds-mode)

;; specify a readable selection
(set-face-attribute 'helm-selection nil
                    :background "goldenrod"
                    :foreground "dark slate gray"
                    :slant 'italic)

(provide 'init-helm)
;;; init-helm.el ends here
