;; Java development configuration
;; Makes use of Eclipse and Eclim

;; Make sure you have installed Eclipse locallay.
;; Make sure you have installed eclim (http://eclim.org/install.html) locally.
(require-package 'emacs-eclim)

;; Variables
(setq eclim-auto-save t
      eclim-executable "/home/giles/software/eclipse/eclim"
      eclimd-wait-for-process nil
      eclimd-default-workspace "home/giles/projects/workspace-eclim"
      eclim-use-yasnippet nil
      eclim-autoupdate-problems nil
      eclim-print-debug-messages nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.3
      )

(defun my-java-mode-hook ()
  (auto-fill-mode)
  (gtags-mode)
  ;;  (flyspell-prog-mode)
  ;;  (flymake-mode)
  (eclim-mode)
  (subword-mode)
  (yas-minor-mode)
  (smartscan-mode)
  (idle-highlight-mode)

  (define-key c-mode-base-map (kbd "<f2>") 'eclim-problems)
  (define-key c-mode-base-map (kbd "M-m") 'eclim-java-find-declaration)
  (define-key c-mode-base-map (kbd "C-M-j") 'tkj-insert-serial-version-uuid)
  (define-key c-mode-base-map (kbd "C-M-o") 'eclim-java-import-organize)
  (define-key c-mode-base-map (kbd "C-S-e") 'eclim-problems-show-errors)
  (define-key c-mode-base-map (kbd "C-S-w") 'eclim-problems-show-warnings)
  (define-key c-mode-base-map (kbd "C-<f9>") 'tkj-eclim-maven-run-quick-package)
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (define-key c-mode-base-map (kbd "C-q") 'eclim-java-show-documentation-for-current-element)
  (define-key c-mode-base-map (kbd "M-RET") 'eclim-problems-correct)
  (define-key c-mode-base-map (kbd "M-<f7>") 'eclim-java-find-references)
  (define-key c-mode-base-map (kbd "M-i") 'eclim-java-implement) ;; IDEA is C-i
  (define-key c-mode-base-map (kbd "S-<f6>") 'eclim-java-refactor-rename-symbol-at-point)
  (define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)

  (define-key c-mode-base-map (kbd "<f7>") 'gud-down)
  (define-key c-mode-base-map (kbd "<f8>") 'gud-next)
  (define-key c-mode-base-map (kbd "<f9>") 'gud-cont)

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '+))

(add-hook 'java-mode-hook 'my-java-mode-hook)



(provide 'init-java)
