;;; init-switch-window --- configure better window switching

;;; Commentary:
;; Make "C-x o" prompt for a target window when more than 2 are available

;;; Code:
(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x 1" . switch-window-then-maximize)
         ("C-x 2" . switch-window-then-split-below)
         ("C-x 3" . switch-window-then-split-right)
         ("C-x 0" . switch-window-then-delete)))

(provide 'init-switch-window)
;;; init-switch-window.el ends here
