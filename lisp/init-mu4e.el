;;; init-mu4w.el --- Configuration for mu4e

;;; Commentary:
;;; set up mu4e mail handling

;;; Code:
(require 'mu4e)

(setq
 mu4e-maildir       "~/.mail/blackpepper"
 mu4e-sent-folder   "/[Gmail].Sent Mail"
 mu4e-drafts-folder "/[Gmail].Drafts"
 mu4e-trash-folder  "/[Gmail].Bin")

(setq
  mu4e-get-mail-command "mbsync blackpepper"
  mu4e-update-interval 300)             ;; update every 5 minutes

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; something about ourselves
(setq
   user-mail-address "giles.paterson@blackpepper.co.uk"
   user-full-name  "Giles Paterson"
   mu4e-compose-signature
    (concat
      "Giles Paterson\n"
      "Innovation Manager\n"
      "http://www.blackpepper.co.uk\n"))

(setq mu4e-maildir-shortcuts
    '( ("/INBOX"               . ?i)
       ("/[Gmail].Sent Mail"   . ?s)
       ("/[Gmail].Trash"       . ?t)))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; save attachment to my desktop (this can also be a function)
(setq mu4e-attachment-dir "~/Downloads")

;; attempt to show images when viewing messages
(setq mu4e-view-show-images t)

(require 'mu4e-contrib)


(defun gp-mu4e-shr2text () 
  "Html to text using the shr engine; this can be used in `mu4e-html2text-command' in a new enough Emacs.
Based on code by Titus von der Malsburg."
  (interactive) 
  (let ((dom (libxml-parse-html-region (point-min) (point-max)))) 
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

(setq mu4e-html2text-command 'gp-mu4e-shr2text)

(add-hook 'mu4e-view-mode-hook
  (lambda()
    ;; try to emulate some of the eww key-bindings
    (local-set-key (kbd "<tab>") 'shr-next-link)
    (local-set-key (kbd "<backtab>") 'shr-previous-link)))

(setq shr-color-visible-luminance-min 80)

(setq mu4e-view-prefer-html t)

;;; ------------------------------------------
;;; Sending Mail
;;; ------------------------------------------
(require 'smtpmail)

;; authentication details are stored in .authinfo
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/queue to set up first
(setq smtpmail-queue-mail nil  ;; start in normal mode
      smtpmail-queue-dir   "~/Maildir/queue/cur")

;;; init-mu4e.el ends here
