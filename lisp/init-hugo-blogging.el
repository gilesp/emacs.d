;;; init-hugo-blogging --- Setup orgmode to publish markdown for my blog

;;; Commentary:
;; Functionality to support blogging with org mode and hugo

;;; Code:
(use-package toml-mode
  :mode "\\.toml\\'")

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

PLIST is the property list for the given project.
FILENAME is the filename of the Org file to be published.
PUB-DIR is the publishing directory.

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
         :headline-levels 3
         :body-only t
         )
        ;; ("gp-blog" :components ("org-gilesp-blog"))
        ))

(provide 'init-hugo-blogging)
;;; init-hugo-blogging ends here
