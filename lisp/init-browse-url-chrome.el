;;; init-browse-url-chrome --- Use google chrome for browing urls

;;; Commentary:
;; Use Google chrome as the default browser when clicking on links

;;; Code:
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(provide 'init-browse-url-chrome)
;;; init-browse-url-chrome.el ends here
