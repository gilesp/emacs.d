;;; init-modeline -- Mode line configuration
;;; Commentary:
;;; Customisation of the modeline, to make it a bit nicer.

;;; Code:
;; Disable box outline
;; (set-face-attribute 'mode-line nil
;;                     :box '(:line-width 1 :color "gray40" :style nil))

;; (set-face-attribute 'mode-line-inactive nil
;;                     :box '(:line-width 1 :color "gray10" :style nil))

;; (set-face-attribute 'mode-line-highlight nil
;;                     :box '(:line-width 1 :color "gray10" :style nil))

(defun gp/shorten-directory (dir max-length)
  "Shorten directory name `DIR' up to `MAX-LENGTH' characters."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defvar gp/mode-line-directory
  (list
   '(:eval (if (buffer-file-name) (concat " " (gp/shorten-directory default-directory 20)) " "))))

(defvar gp/mode-line-buffer-name
      ;; the buffer name; the file name as a tool tip
      (list
       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                           'help-echo (buffer-file-name)))))

(defvar gp/mode-line-position
      ;; line and column
      (list
       "[" ;; '%02' to set to 2 chars at least; prevents flickering
       (propertize "%02l" 'face 'font-lock-type-face) ":"
       (propertize "%02c" 'face 'font-lock-type-face)
       "] " ))

(defvar gp/mode-line-relative-position
      ;; relative position, size of file
      (list
       "["
       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
       "/"
       (propertize "%I" 'face 'font-lock-constant-face) ;; size
       "] "))

(defvar gp/mode-line-major-mode
      ;; the current major mode for the buffer.
      (list
       "["
       
       '(:eval (propertize 'global-mode-string 'face 'font-lock-string-face
                           'help-echo buffer-file-coding-system))
       "] "))

(defvar gp/mode-line-insert-indicator
      ;; insert vs overwrite mode, input-method in a tooltip
      (list
       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                           'face 'font-lock-preprocessor-face
                           'help-echo (concat "Buffer is in "
                                              (if overwrite-mode "overwrite" "insert") " mode")))))

(defvar gp/mode-line-modified-indicator
      ;; was this buffer modified since the last save?
      (list
       '(:eval (when (buffer-modified-p)
                 (concat ","  (propertize "Mod"
                                          'face 'font-lock-warning-face
                                          'help-echo "Buffer has been modified"))))))

(defvar gp/mode-line-read-only-indicator
      ;; is this buffer read-only?
      (list
       '(:eval (when buffer-read-only
                 (concat ","  (propertize "RO"
                                          'face 'font-lock-type-face
                                          'help-echo "Buffer is read-only"))))))

(defvar gp/mode-line-time
      ;; add the time, with the date and the emacs uptime in the tooltip
      (list
       '(:eval (propertize (format-time-string "%H:%M")
                           'help-echo
                           (concat (format-time-string "%c; ")
                                   (emacs-uptime "Uptime:%hh"))))))

(defvar gp/mode-line-padding
      ;; fill with '-'
      (list
       "%-"))

(setq-default mode-line-format
              (list
               " "
               gp/mode-line-directory
               gp/mode-line-buffer-name
               gp/mode-line-position
               gp/mode-line-relative-position
               gp/mode-line-major-mode
               "["
               gp/mode-line-insert-indicator
               gp/mode-line-modified-indicator
               gp/mode-line-read-only-indicator
               "] "
               ;;gp/mode-line-time
               "--"
               gp/mode-line-padding
                ))

(provide 'init-modeline)
;;; init-modeline.el ends here
