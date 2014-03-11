(require 'helm-c-moccur)
(setq helm-c-moccur-helm-idle-delay 0.1)
(define-key helm-c-moccur-helm-map (kbd "C-h") 'delete-backward-char)
