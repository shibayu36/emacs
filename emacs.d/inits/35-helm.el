(require 'helm)
(require 'helm-config)

;; C-hで一文字削除になるように
(define-key helm-map (kbd "C-h") 'delete-backward-char)
