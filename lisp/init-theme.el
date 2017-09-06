;;; init-theme --- Initialise theme!

;;; Commentary:
;; Ensure we have our desired theme installed and use it.

;;; Code:
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-gruvbox-dark-hard t)
  (add-hook 'after-make-frame-functions
  (lambda (frame)
    (select-frame frame)
    (if (display-graphic-p)
      (progn
        (set-face-attribute 'linum frame
          :background (face-attribute 'default :background)
          :foreground (face-attribute 'linum :foreground)
          :height 0.9))))))

(provide 'init-theme)
;;; init-theme ends here
