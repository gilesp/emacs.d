;; Magit
;; See http://magit.vc/

(require-package 'magit)
(require-package 'git-gutter)

;; Bind magit status to C-x g for ease of use
(global-set-key (kbd "C-x g") 'magit-status)

;; Bind magit dispatch popup
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(setq global-magit-file-buffer-mode t)
(global-git-gutter-mode t)

(provide 'init-magit)
