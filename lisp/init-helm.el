;;; init-helm.el --- Configuration for Helm

;;; Commentary:
;;; See http://tudo.github.io/helm-intro.html for more details

;;; Code:
(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (require 'helm-ls-git)
    (require 'helm-git-grep)

    (setq helm-candidate-number-limit 100
          helm-idle-delay 0.01
          helm-input-idle-delay 0.01
          helm-yas-display-key-on-candidate t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-apropos-fuzzy-match t
          )
    
    (helm-mode))
  :config
  (progn
    (helm-autoresize-mode)
    (set-face-attribute 'helm-selection nil
                        :background "goldenrod"
                        :foreground "dark slate gray"
                        :slant 'italic)
    ;; Disable line numbers in helm buffers
    (when linum-mode
      (add-hook 'helm-after-initialize-hook (lambda ()
                                              (with-helm-buffer
                                                (linum-mode 0))))))
  :bind (("M-x" . helm-M-x)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-d" . helm-browse-project)
         ("C-x c o" . helm-occur)
         ("C-h a" . helm-apropos)
         ("M-y" . helm-show-kill-ring)
         ))

(use-package helm-descbinds
  :defer t
  :init
  (progn
    (helm-descbinds-mode t))
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package helm-ls-git
  :defer t
  :config
  (progn
    (setq helm-ls-git-status-command 'magit-status-internal)))

(use-package helm-git-grep
  :defer t
  :bind (("C-x C-g" . helm-git-grep)
         ("C-c g" . helm-git-grep-from-helm)
         :map isearch-mode-map
         ("C-c g" . helm-git-grep-from-isearch)))

(use-package helm-swoop
  :bind
  ("C-s" . helm-swoop))

(provide 'init-helm)
;;; init-helm.el ends here
