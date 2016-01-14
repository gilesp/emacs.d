;;; init-swift --- Configure swift-mode

(require-package 'flycheck)
(require-package 'swift-mode)

(require 'flycheck)
(require 'swift-mode)

(add-to-list 'flycheck-checkers 'swift)

(provide 'init-swift)
