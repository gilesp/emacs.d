;;; init-theme --- Initialise theme!

;;; Commentary:
;; Ensure we have our desired theme installed and use it.

;;; Code:
(use-package base16-theme
  :ensure t
  :config
  ;; (load-theme 'base16-gruvbox-dark-hard t)
  ;; (load-theme 'base16-tomorrow-night t)
  ;; (load-theme 'base16-woodland t)
  (load-theme 'base16-brewer t))

(provide 'init-theme)
;;; init-theme ends here
