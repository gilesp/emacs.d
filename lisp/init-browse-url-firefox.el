;;; init-browse-url-firefox --- Use firefox for browing urls

;;; Commentary:
;; Use Firefox as the default browser when clicking on links

;;; Code:
(defvar browse-url-generic-program)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(provide 'init-browse-url-firefox)
;;; init-browse-url-firefox.el ends here
