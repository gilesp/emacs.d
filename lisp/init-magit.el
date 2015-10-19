;; Magit
;; See http://magit.vc/

(require-package 'magit)

;; Bind magit status to C-x g for ease of use
(global-set-key (kbd "C-x g") 'magit-status)

(provide 'init-magit)
