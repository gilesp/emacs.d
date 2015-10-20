;;
;; org-mode configuration
;;
;; load org-mode from source
;; You first need to check it out from git (git://orgmode.org/org-mode.git)
;; and build it with "make uncompiled"
(add-to-list 'load-path (expand-file-name "~/.emacs.d/org-mode/lisp"))

(require 'org)

;; automatically use org mode for .org, .org_archive and .txt files
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; see the custom-file for the agenda files configuration
;; I'm setting it to use all files in a specified directory

;; Require markdown export mode
(require 'ox-md nil t)

;; Require odt export mode
;; (require 'ox-odt nil t)

;; Require html export mode
(require 'ox-html nil t)

;; Word count
(defvar count-words-buffer
  nil
  "*Number of words in the buffer.")

(defun wicked/update-wc ()
  (interactive)
  (setq count-words-buffer (number-to-string (count-words-buffer)))
  (force-mode-line-update))
  
; only setup timer once
(unless count-words-buffer
  ;; seed count-words-paragraph
  ;; create timer to keep count-words-paragraph updated
  (run-with-idle-timer 1 t 'wicked/update-wc))

;; add count words paragraph the mode line
(unless (memq 'count-words-buffer global-mode-string)
  (add-to-list 'global-mode-string "words: " t)
  (add-to-list 'global-mode-string 'count-words-buffer t)) 

;; count number of words in current paragraph
(defun count-words-buffer ()
  "Count the number of words in the current paragraph."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
	(forward-word 1)
        (setq count (1+ count)))
      count)))

(provide 'init-org)
