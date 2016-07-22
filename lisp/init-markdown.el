;;; init-markdown.el -- Configuration for markdown mode.

;;; Commentary:
;;; See http://jblevins.org/projects/markdown-mode/ for more details.

;;; Code:
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'init-markdown)
;;; init-markdown.el ends here
