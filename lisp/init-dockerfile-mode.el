(add-to-list 'load-path (expand-file-name "lisp/dockerfile-mode" user-emacs-directory))
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(provide 'init-dockerfile-mode)
