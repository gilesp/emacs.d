;;; init-ivy.el --- Configuration for ivy (and swiper and counsel)

;;; Commentary:
;;; Tinkering around to see if ivy is worth switching to instead of helm.

;;; Code:

(use-package ivy
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :init
  (ivy-mode 1) ; eneable ivy mode globally at startup
  :config
  ; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 20)
  ;; set count format
  (setq ivy-count-format "(%d/%d) ")

  (setq ivy-display-style 'fancy)
  ;; no regexp by default
  ;(setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  ;(setq ivy-re-builders-alist
	;; allow input not in order
        ;'((t   . ivy--regex-ignore-order)))
  )


(use-package counsel
  :bind* ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
   ("C-c f"   . counsel-git)       ; search for files in git repo
   ("C-c s"   . counsel-git-grep)  ; search for regexp in git repo
   ("C-c /"   . counsel-ag)        ; search for regexp in git repo using ag
   ("C-c l"   . counsel-locate))   ; search for files or else using locate
  )

(use-package swiper
  :ensure t
  :bind
  (([remap isearch-forward]  . swiper)
   ([remap isearch-backward] . swiper))
  :config
  (setq swiper-action-recenter t))

(provide 'init-ivy)
;;; init-ivy ends here
