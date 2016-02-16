;;; init-swift --- Configure swift-mode

(require-package 'swift-mode)

(require 'swift-mode)

(add-to-list 'flycheck-checkers 'swift)

(provide 'init-swift)
