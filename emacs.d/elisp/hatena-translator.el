;; はてなトランスレーター.el

;; はてなトランスレーターを使うためのEmacs Lispです．
;; Emacs 23では動きました．古いバージョンでは分かりません．
;;
;; popup.elはここから入手する
;; http://github.com/m2ym/auto-complete
;; deferred.elはここから入手する
;; https://github.com/kiwanami/emacs-deferred/
;; json.elなければここから
;; http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs

;; 設定例
;; (add-hook 'html-mode-hook
;;           '(lambda ()
;;              (require 'hatena-translator)
;;              (define-key html-mode-map "\C-xt" 'hatena-translator:popup-msgid-at-point)
;;              (define-key html-mode-map "\C-xT" 'hatena-translator:open-msgid-at-point)
;;              ))
;;
;; Macの場合はCommand+t, Command+Tでできると便利です．
;;
;; (add-hook 'html-mode-hook
;;           '(lambda ()
;;              (require 'hatena-translator)
;;              (define-key html-mode-map [(super t)] 'hatena-translator:popup-msgid-at-point)
;;              (define-key html-mode-map [(super T)] 'hatena-translator:open-msgid-at-point)
;;              ))
;;
;; translatorに接続するときにBASIC認証を求められるので入力してください．
;; 最初の1回は失敗することがあり，失敗したらやり直すとうまくいくことがあります．

(require 'cl)
(require 'url)
(require 'deferred)
(require 'json)
(require 'popup)

(defun hatena-translator:get-msgid-at-point ()
  "カーソル位置のmsgidを返します．見つからないときnilを返します"
  (let ((msgid (replace-regexp-in-string "'" "" (or (thing-at-point 'symbol) ""))))
    (if (> (length msgid) 0) msgid nil)
    ))

(defun hatena-translator:popup-msgid-at-point ()
  "カーソル位置のmsgidの日本語をポップアップします"
  (interactive)
  (lexical-let ((msgid (hatena-translator:get-msgid-at-point)))
    (if msgid
        (deferred:$
          (deferred:url-retrieve (format "http://translator.hatena.ne.jp/entries.ja.json?msgid=%s&api_key=AX7LNJeM6fYkOrqjE35AtMnkr1UxB3TO2iMCp70X" msgid))
          (deferred:nextc it
            (lambda (buf)
              (let ((data (with-current-buffer (deferred:url-delete-header buf) (buffer-string))))
                (let ((value (cdr (car (cdr (car (json-read-from-string data)))))))
                  (popup-tip (if value value "未登録です"))
                  )))))
      (popup-tip "msgidが見つかりません")
      )))

(defun hatena-translator:open-msgid-at-point ()
  "カーソル位置のmsgidの日本語をtranslatorで開きます"
  (interactive)
  (let ((msgid (hatena-translator:get-msgid-at-point)))
    (if msgid
        (browse-url (format "http://translator.hatena.ne.jp/entries?msgid=%s" msgid))
      (popup-tip "msgidが見つかりません")
      )))

(provide 'hatena-translator)