;; camcorder config
;; You need to install recordmydesktop and ffmpeg to use.
;; see https://github.com/Malabarba/camcorder.el for details
;;
;; Tool for capturing screencasts directly from Emacs.
;;
;; To use it, simply call M-x camcorder-record.
;; A new smaller frame will popup and recording starts.
;; When you’re finished, hit F12.
;;
;; View screencasts are generated in ogv format, and you can even pause the recording with F11! You can also convert the .ogv file to a gif by issuing the command M-x camcorder-convert-to-gif.
;; 
;; If you want to record without a popup frame, use M-x camcorder-mode.

(require-package 'camcorder)


(provide 'init-camcorder)

