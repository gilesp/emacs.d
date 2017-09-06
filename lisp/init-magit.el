;;; init-magit --- Configure magit

;;; Commentary:
;; See http://magit.vc/

;;; Code:

(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (custom-set-variables
   '(git-gutter:update-interval 2))
  
  :config
  (global-git-gutter-mode))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq
   magit-process-connection-type t
   global-magit-file-buffer-mode t
   git-commit-summary-max-length 120))

(provide 'init-magit)
;;; init-magit ends here
