;;; init-csharp --- Configure csharp mode

;;; Commentary:
;; See https://joshwolfe.ca/post/emacs-for-csharp/

;;; Code:

(use-package omnisharp
  :after company
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp))

(provide 'init-csharp)
;;; init-csharp ends here
