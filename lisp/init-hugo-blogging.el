;; Functionality to support blogging with org mode and hugo

(require-package 'toml-mode)

(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

;; function to insert ISO8601 timestamp
(defun insert-timestamp ()
  "Insert the current time at point, as an ISO8601 timestamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%TZ")))


;; org publish method that will use github flavoured markdown (prodived by ox-gfm)
;; rather than plain markdown
;;;###autoload
(defun gp-org-gfm-publish-to-md (plist filename pub-dir)
  "Publish an org file to Github Flavoured Markdown.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'gfm filename ".md" plist pub-dir))


;; configure markdown export
(setq org-publish-project-alist
      '(
        ("vurtuk-blog"
         ;; Path to org files
         :base-directory "~/Documents/vurtcouk/org-content/"
         :base-extension "org"

         ;; Path to hugo project
         :publishing-directory "~/Documents/vurtcouk/content/"
         :recursive t
         :publishing-function gp-org-gfm-publish-to-md
         )

        ;; ("gp-blog" :components ("org-gilesp-blog"))
        ))
         
(provide 'init-hugo-blogging)
