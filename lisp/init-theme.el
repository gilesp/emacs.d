;;; init-theme --- Initialise theme!

;;; Commentary:
;; Ensure we have our desired theme installed and use it.

;;; Code:
;;(require-package 'aurora-theme)
(require-package 'labburn-theme)
(require-package 'rainbow-identifiers)

;;(load-theme 'aurora t)
(load-theme 'labburn t)

(setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)

(provide 'init-theme)
;;; init-theme ends here
