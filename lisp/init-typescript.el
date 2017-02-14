;;; init-typescript.el --- Configuration for tide

;;; Commentary:

;;; Code:
(use-package tide
  :functions tide-setup
  :config
  (defun gp-setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (add-hook 'typescript-mode-hook 'gp-setup-tide-mode)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (setq tide-format-options '(:tabSize 2
                                       :convertTabsToSpaces t
                                       :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                                       :placeOpenBraceOnNewLineForFunctions nil
                                       :))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (gp-setup-tide-mode))))
  )
  
(provide 'init-typescript)
;;; init-typescript.el ends here
