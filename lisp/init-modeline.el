;; Disable box outline
(set-face-attribute 'mode-line nil
                    :box '(:line-width 1 :color "gray40" :style nil))

(set-face-attribute 'mode-line-inactive nil
                    :box '(:line-width 1 :color "gray10" :style nil))

(set-face-attribute 'mode-line-highlight nil
                    :box '(:line-width 1 :color "gray10" :style nil))

(provide 'init-modeline)
