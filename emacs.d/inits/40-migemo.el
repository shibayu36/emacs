(when (and (executable-find "cmigemo")
           (require 'migemo nil t))

  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-coding-system 'utf-8)

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)

  ;; キャッシュの設定
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1000)

  (load-library "migemo")
  (migemo-init)

  ;; 昔の設定、とりあえず残しておく
  ;; ;; cmigemoを使う
  ;; (setq migemo-command "cmigemo")
  ;; ;; migemoのコマンドラインオプション
  ;; (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  ;; ;; migemo辞書の場所
  ;; (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  ;; ;; cmigemoで必須の設定
  ;; (setq migemo-user-dictionary nil)
  ;; (setq migemo-regex-dictionary nil)
  ;; ;; キャッシュの設定
  ;; (setq migemo-use-pattern-alist t)
  ;; (setq migemo-use-frequent-pattern-alist t)
  ;; (setq migemo-pattern-alist-length 1000)
  ;; (setq migemo-coding-system 'utf-8-unix)

  ;; ;; migemoを起動する
  ;; (migemo-init)
  )
