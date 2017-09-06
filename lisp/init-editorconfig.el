;;; init-editorconfig --- Configuration for editorconfig plugin

;;; Commentary:
;;; MHR use editorconfig, so rather than mangle my typescript setup, I'm using this plugin

;;; Code:
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(provide 'init-editorconfig)
;;; init-editorconfig.el ends here
