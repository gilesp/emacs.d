;; Java development configuration
;; Makes use of Eclipse and Eclim

;; Make sure you have installed Eclipse locally.
;; Make sure you have installed eclim (http://eclim.org/install.html) locally.
(require-package 'emacs-eclim)
(require-package 'company)

;; Variables
(setq eclim-auto-save t
      eclim-eclipse-dirs "/home/giles/software/eclipse"
      eclim-executable "/home/giles/software/eclipse/eclim"
      eclimd-default-workspace "home/giles/projects/eclipse/workspace"
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      )

(require 'eclim)
(require 'eclimd)
(global-eclim-mode)

(defun my-java-mode-hook ()
  ;; auto complete
  (company-eclim)
  ;; key-bindings
  (define-key eclim-mode-map (kbd "C-c C-n") 'eclim-problems-next-same-window)
  (define-key eclim-mode-map (kbd "C-c C-p")  'eclim-problems-previous-same-window)
  
;;  (eclim-mode)
;;  (auto-fill-mode)
  ;;  (gtags-mode)
  ;;  (flyspell-prog-mode)
  ;;  (flymake-mode)
;;  (eclim-mode)
;;  (subword-mode)
;;  (yas-minor-mode)
;;  (smartscan-mode)
;;  (idle-highlight-mode)
;;  (help-at-pt-set-timer)
  
;;  (define-key c-mode-base-map (kbd "<f2>") 'eclim-problems)
;;  (define-key c-mode-base-map (kbd "M-m") 'eclim-java-find-declaration)
;;  (define-key c-mode-base-map (kbd "C-M-j") 'tkj-insert-serial-version-uuid)
;;  (define-key c-mode-base-map (kbd "C-M-o") 'eclim-java-import-organize)
;;  (define-key c-mode-base-map (kbd "C-S-e") 'eclim-problems-show-errors)
;;  (define-key c-mode-base-map (kbd "C-S-w") 'eclim-problems-show-warnings)
;;  (define-key c-mode-base-map (kbd "C-<f9>") 'tkj-eclim-maven-run-quick-package)
;;  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
;;  (define-key c-mode-base-map (kbd "C-q") 'eclim-java-show-documentation-for-current-element)
;;  (define-key c-mode-base-map (kbd "M-RET") 'eclim-problems-correct)
;;  (define-key c-mode-base-map (kbd "M-<f7>") 'eclim-java-find-references)
;;  (define-key c-mode-base-map (kbd "M-i") 'eclim-java-implement) ;; IDEA is C-i
;;  (define-key c-mode-base-map (kbd "S-<f6>") 'eclim-java-refactor-rename-symbol-at-point)
;;  (define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)

;;  (define-key c-mode-base-map (kbd "<f7>") 'gud-down)
;;  (define-key c-mode-base-map (kbd "<f8>") 'gud-next)
;;  (define-key c-mode-base-map (kbd "<f9>") 'gud-cont)

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '+))

(add-hook 'java-mode-hook 'my-java-mode-hook)


(provide 'init-java)
