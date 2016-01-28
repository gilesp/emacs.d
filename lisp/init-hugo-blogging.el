;; Functionality to support blogging with org mode and hugo

(require-package 'toml-mode)

(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

;; function to insert ISO8601 timestamp
(defun insert-timestamp ()
  "Insert the current time at point, as an ISO8601 timestamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%TZ")))

;; configure markdown export
(setq org-publish-project-alist
      '(
        ("org-gilesp-blog"
         ;; Path to org files
         :base-directory "~/Documents/vurtcouk/org-content/"
         :base-extension "org"

         ;; Path to hugo project
         :publishing-directory "~/Documents/vurtcouk/content/"
         :recursive t
         :publishing-function org-md-publish-to-md
         )

        ("gilesp-blog" :components ("org-gilesp-blog"))
        ))
         
(provide 'init-hugo-blogging)
