(require 'helm-c-moccur)
(setq
 helm-c-moccur-helm-idle-delay 0.1
 ;; バッファの情報をハイライトする
 helm-c-moccur-higligt-info-line-flag t
 ;; 現在選択中の候補の位置を他のwindowに表示する
 helm-c-moccur-enable-auto-look-flag t)

(define-key helm-c-moccur-helm-map (kbd "C-h") 'delete-backward-char)
