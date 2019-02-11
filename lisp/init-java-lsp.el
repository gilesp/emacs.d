;; init-java-lsp.el -- Lsp mode java configuration

;;; Commentary:
;; An alternative approach to java development, using lsp-mode

;;; Code:
(use-package lsp-mode)
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp-java-enable))

(provide 'init-java-lsp)
;;; init-java-esp.el ends here
