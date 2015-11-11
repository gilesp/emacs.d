(require-package 'erc-image)
(require-package 'erc-tweet)
(require-package 'erc-youtube)

(require 'notifications)
(require 'auth-source)
(require 'tls)
(require 'erc)
(require 'erc-image)
(require 'erc-youtube)
(require 'erc-tweet)

;; Only add channels to the modeline when my nick or keywords are mentioned
(setq erc-current-nick-hichlight-type 'nick-or-keyword)

;; Highlight keywords
(setq erc-keywords '("\\b[lL]abs\\b" "\\bbrown bag\\b" "\\bbrown bags\\b" "\\bgiles\\b" "\\bGiles\\b"))

;; Ignore channel event noise
;; See https://www.alien.net.au/irc/irc2numerics.html for details of numeric irc message codes
(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE" "324" "329" "332" "333" "353" "477"))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE" "329" "332" "477"))

(setq erc-track-use-faces t)
(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))
(setq erc-track-priority-faces-only 'all)

;; Add auto-away mode
(add-to-list 'erc-modules 'autoaway)
(setq erc-autoaway-idle-seconds 600)
(setq erc-auto-set-away t)
(setq erc-autoaway-use-emacs-idle t)
;; Add image support
(add-to-list 'erc-modules 'image)
;; Add youtube link support
(setq erc-youtube-apiv3-key "AIzaSyCaCPohQUGypBSzsBAaTdZVBM93w6eO0sU")
(add-to-list 'erc-modules 'youtube)
;; Add twitter link support
(add-to-list 'erc-modules 'tweet)

(erc-update-modules)

;; Function to start erc
(defun start-slack ()
  "Connect to Slack (via IRC)."
  (interactive)
  ;; auth-source is used for credentials, nece why password is nil
  (erc-tls :server "blackpepper.irc.slack.com"
           :port 6697
           :password nil
           :nick "giles.paterson"
           :full-name "giles.paterson")
  (setq erc-autojoin-channels-alist '(
                                      ("blackpepper.irc.slack.com" "#general"))))


;; use notify-osd (install with apt-get) to inform me of messages
(defun erc-global-notify (match-type nick message)
  "Notify when a message is recieved."
  (when erc-public-away-p
    (notifications-notify
     :title nick
     :body message
     :app-icon "/usr/share/notify-osd/icons/hicolor/scalable/status/notification-message-im.svg"
     :urgency 'low)))

(add-hook 'erc-text-matched-hook 'erc-global-notify)

(provide 'init-slack)
