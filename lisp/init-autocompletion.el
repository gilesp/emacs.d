;; init-autocompletion.el --- configuration of an auto completion mechanism

;;; Commentary:
;; setting up company-mode

;;; Code:

(defun gp/complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

;; (use-package company
;;   :bind (("\t" . gp/complete-or-indent))
;;   :diminish company-mode
;;   :defer t
;;   :config (global-company-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :demand
  :init
  (setq company-idle-delay 0.3)
  (setq company-begin-commands '(self-insert-command))
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-limit 20)
  (setq company-dabbrev-downcase nil)
  :bind (("\t" . gp/complete-or-indent))
  :config
  (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :after (company)
  :config
  (company-quickhelp-mode))

(provide 'init-autocompletion)
;;; init-autocompletion.el ends here
