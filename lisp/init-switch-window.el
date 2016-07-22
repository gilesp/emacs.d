;;; init-switch-window --- configure better window switching

;;; Commentary:
;; Make "C-x o" prompt for a target window when more than 2 are available

;;; Code:
(use-package switch-window
  :config
  (setq switch-window-shortcut-style 'alphabet)
  :bind (("C-x o" . switch-window)))

(provide 'init-switch-window)
;;; init-switch-window.el ends here
