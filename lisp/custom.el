(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(auto-save-default nil)
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(column-number-mode t)
 '(default-frame-alist (quote ((width . 120) (height . 52))))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote right))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(magit-process-password-prompt-regexps
   (quote
    ("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$" "^\\(Enter \\)?[Pp]assword\\( for '\\(?99:.*\\)'\\)?: ?$" "^.*'s password: ?$" "^Yubikey for .*: ?$" "^Enter PIN for '.*': ?$")))
 '(make-backup-files t)
 '(mouse-wheel-progressive-speed nil)
 '(org-agenda-files (quote ("~/Documents/org")))
 '(org-todo-keyword-faces
   (quote
    (("DONE" . success)
     ("IN-PROGRESS" . diary)
     ("WAITING" . warning)
     ("TODO" . error))))
 '(org-todo-keywords (quote ((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE"))))
 '(pop-up-frames nil)
 '(safe-local-variable-values
   (quote
    ((docker-image-name . "concierge-server")
     (docker-image-name . "concierge-meeting-listener")
     (docker-image-name . "react-native"))))
 '(scalable-fonts-allowed t)
 '(scroll-step 1)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 128 :width normal))))
 '(linum ((t (:background "#3f3f3f" :foreground "#636363" :height 0.9)))))
