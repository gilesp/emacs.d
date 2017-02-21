;;; init-yasnippet --- Setup yasnippet
;;; Commentary:
;;; By default, yasnippet will look for snippets in ~/.emacs.d/snippets

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
  (progn
    (yas-global-mode)
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (setq yas-expand-only-for-last-commands nil)
    (bind-key "\t" 'hippie-expand yas-minor-mode-map)
    ;(add-to-list 'yas-prompt-functions 'gp/yas-helm-prompt)
    )
  )

(defun gp/yas-helm-prompt (prompt choices &optional display-fn)
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

(provide 'init-yasnippet)
;;; init-yasnippet ends here
