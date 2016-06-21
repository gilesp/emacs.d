;;; init-markdown.el -- Configuration for markdown mode.

;;; Commentary:
;;; See http://jblevins.org/projects/markdown-mode/ for more details.

;;; Code:
(require-package 'markdown-mode)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "gfm-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(provide 'init-markdown)
;;; init-markdown.el ends here
