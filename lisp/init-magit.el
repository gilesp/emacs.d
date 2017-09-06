;;; init-magit --- Configure magit

;;; Commentary:
;; See http://magit.vc/

;;; Code:

(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode))

(require-package 'magit)

(require 'magit)

;; Bind magit status to C-x g for ease of use
(global-set-key (kbd "C-x g") 'magit-status)

;; Bind magit dispatch popup
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(setq magit-process-connection-type t)
(setq global-magit-file-buffer-mode t)

(setq magit-process-password-prompt-regexps
      '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
        ;; match-group 99 is used to identify a host
        "^\\(Enter \\)?[Pp]assword\\( for '\\(?99:.*\\)'\\)?: ?$"
        "^.*'s password: ?$"
        "^Yubikey for .*: ?$"
        "^Enter PIN for '.*': ?$"))

(setq git-commit-summary-max-length 120)
(provide 'init-magit)
;;; init-magit ends here
