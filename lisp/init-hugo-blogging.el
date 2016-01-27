;; Functionality to support blogging with org mode and hugo

(require-package 'toml-mode)

(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

;; function to insert ISO8601 timestamp
(defun insert-timestamp ()
  "Insert the current time at point, as an ISO8601 timestamp."
  (interactive)
  (insert (format-time-string "%Y-%n-%dT%T%z")))

(provide 'init-hugo-blogging)
