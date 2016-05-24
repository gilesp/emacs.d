;;; init-magit --- Configure magit

;;; Commentary:
;; See http://magit.vc/

;;; Code:
(require-package 'magit)
(require-package 'git-gutter)

;; Bind magit status to C-x g for ease of use
(global-set-key (kbd "C-x g") 'magit-status)

;; Bind magit dispatch popup
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(setq global-magit-file-buffer-mode t)
(global-git-gutter-mode t)

(provide 'init-magit)
;;; init-magit ends here
