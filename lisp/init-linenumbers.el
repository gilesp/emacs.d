;; Configure line numbering globally (I work with code more than plain text)

;; smart width for line numbers
(setq linum-format "%d")

;; enable line numbers mode
(require 'linum)
(global-linum-mode 1)

(provide 'init-linenumbers)
