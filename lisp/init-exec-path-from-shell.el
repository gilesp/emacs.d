;;; init-exec-path-from-shell --- configure environment variable loading

;;; Commentary:

;;; Code:
(require-package 'exec-path-from-shell)

(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(provide 'init-exec-path-from-shell)
;;; init-exec-path-from-shell ends here
