;;
;; Make "C-x o" prompt for a target window when more than 2 are available
(require-package 'switch-window)
(require 'switch-window)
(setq switch-window-shortcut-style 'alphabet)
(global-set-key (kbd "C-x o") 'switch-window)

(provide 'init-switch-window)
