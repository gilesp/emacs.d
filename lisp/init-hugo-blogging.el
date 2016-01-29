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


;; Abbreviations useful for hugo blogging

;; enable abbrev-mode when in org-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

(define-skeleton skel-org-block-src
  "Insert a src block"
  ""
  "#+BEGIN_SRC \n"
  _ - \n
  "#+END_SRC\n")

(define-abbrev org-mode-abbrev-table "src" "" 'skel-org-block-src)

(define-skeleton skel-org-blog-frontmatter
  "Insert hugo frontmatter"
  "title:"
  "#+BEGIN_EXPORT md\n"
  "+++\n"
  "title = \"" str "\"\n"
  "description = \""_"\"\n"
  "date = \n"
  "tags = []\n"
  "+++\n"
  "#+END_EXPORT\n")

(define-abbrev org-mode-abbrev-table "hugofront" "" 'skel-org-blog-frontmatter)
  
(provide 'init-hugo-blogging)
