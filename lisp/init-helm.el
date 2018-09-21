;;; init-helm.el --- Configuration for Helm

;;; Commentary:
;;; See http://tudo.github.io/helm-intro.html for more details

;;; Code:
(use-package helm
  :diminish helm-mode
  :defines helm-idle-delay helm-yas-display-key-on-candidate helm-M-x-requires-pattern helm-ff-skip-boring-files helm-buffers-fuzzy-matching helm-recentf-fuzzy-match helm-apropos-fuzzy-match helm-ff-file-name-history-use-recentf helm-command-prefix-key
  :init
    (helm-mode)
    (setq helm-command-prefix-key "C-c h")
  :config
    (require 'helm-config)
    (setq helm-candidate-number-limit 100
          helm-idle-delay 0.01
          helm-input-idle-delay 0.01
          helm-yas-display-key-on-candidate t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
	  helm-ff-file-name-history-use-recentf t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-apropos-fuzzy-match t)
    (helm-autoresize-mode)
  :bind (("M-x" . helm-M-x)
	 ("M-s o" . helm-occur)
	 ("M-/" . helm-dabbrev)
	 ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("C-x C-d" . helm-browse-project)
         ("C-h a" . helm-apropos)
         ("M-y" . helm-show-kill-ring)))

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
