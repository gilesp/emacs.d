;; Google-This
;; Google searching from Emacs
;; https://github.com/Malabarba/emacs-google-this/

(require-package 'google-this)
(require 'google-this)
(google-this-mode 1)

(global-set-key (kbd "C-x g") 'google-this-mode-submap)
