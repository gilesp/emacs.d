;;; init-yasnippet --- Setup yasnippet
;;; Commentary:
;;; By default, yasnippet will look for snippets in ~/.emacs.d/snippets

;;; Code:
(require-package 'yasnippet)

;; enable this if you want to also look for snippets in a particular directory
;;(setq yas-snippet-dirs (append yas-snippet-dirs
;;                               '("~/Downloads/interesting-snippets")))

(require 'yasnippet)

(message "required yasnippet")

(defun gp-yas-helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet.
Put this into `yas-prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))

(setq yas-prompt-functions (append yas-prompt-functions '(gp-yas-helm-prompt)))

(yas-global-mode 1)
(message "yas-global-mode enabled")
(provide 'init-yasnippet)
(message "init-yasnippet done.")
;;; init-yasnippet ends here
