;; init-autocompletion.el --- configuration of an auto completion mechanism

;;; Commentary:
;; setting up company-mode

;;; Code:

(defun gp/complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

(use-package company
  :bind (("\t" . gp/complete-or-indent))
  :defer t
  :config (global-company-mode))

(provide 'init-autocompletion)
;;; init-autocompletion.el ends here
