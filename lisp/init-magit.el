;;; init-magit --- Configure magit

;;; Commentary:
;; See http://magit.vc/

;;; Code:
(require-package 'magit)
(require-package 'git-gutter)

(require 'magit)
(require 'git-gutter)

;; Bind magit status to C-x g for ease of use
(global-set-key (kbd "C-x g") 'magit-status)

;; Bind magit dispatch popup
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(setq magit-process-connection-type t)
(setq global-magit-file-buffer-mode t)
(global-git-gutter-mode t)

(setq magit-process-password-prompt-regexps
      '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
        ;; match-group 99 is used to identify a host
        "^\\(Enter \\)?[Pp]assword\\( for '\\(?99:.*\\)'\\)?: ?$"
        "^.*'s password: ?$"
        "^Yubikey for .*: ?$"
        "^Enter PIN for '.*': ?$"))

(provide 'init-magit)
;;; init-magit ends here
