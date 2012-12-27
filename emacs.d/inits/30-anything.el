(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.2
   ;; タイプして再描写するまでの時間。デフォルトは0.1
   anything-input-idle-delay 0.1
   ;; 候補の最大表示数。デフォルトは50
   anything-candidate-number-limit 50
   ;; 候補が多いときに体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    (setq anything-sources
          '(anything-c-source-buffers
            anything-c-source-recentf
            anything-c-source-man-pages
            anything-c-source-emacs-commands
;;            anything-c-source-emacs-functions
            anything-c-source-files-in-current-dir
            ))

    ;; root権限でアクションを実行するときのコマンド
    ;; デフォルトは"su"
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)
  (and (equal current-language-environment "Japanese")
       (executable-find "cmigemo")
       (require 'anything-migemo nil t))
  (when (require 'anything-complete nil t)
    ;; M-xによる補完をAnythingで行なう
    ;; (anything-read-string-mode 1)
    ;; lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install))

  (require 'anything-grep nil t)

  ;;; P88
  ;;; manやinfoを調べるコマンドを作成してみる
  ;; anything-for-document 用のソースを定義
  (setq anything-for-document-sources
      (list anything-c-source-man-pages
            anything-c-source-info-cl
            anything-c-source-info-pages
            anything-c-source-info-elisp
            anything-c-source-apropos-emacs-commands
            anything-c-source-apropos-emacs-functions
            anything-c-source-apropos-emacs-variables))
  ;; anything-for-document コマンドを作成
  (defun anything-for-document ()
    "Preconfigured `anything' for anything-for-document."
    (interactive)
    (anything anything-for-document-sources (thing-at-point 'symbol) nil nil nil "*anything for document*"))

  ;;; P90
  ;;; anything-project: プロジェクトからファイルを絞り込み
  ;; (install-elisp "http://github.com/imakado/anything-project/raw/master/anything-project.el")
  ;; (when (require 'anything-project nil t)
  ;;   (setq ap:project-files-filters
  ;;         (list
  ;;          (lambda (files)
  ;;            (remove-if 'file-directory-p files)
  ;;            (remove-if '(lambda (file) (string-match-p "~$" file)) files)))))

  ;;; anything-c-moccur: MoccurのAnythingインターフェイス
  ;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el")
  (when (require 'anything-c-moccur nil t)
    (setq
     ;; anything-c-moccur用 `anything-idle-delay'
     anything-c-moccur-anything-idle-delay 0.1
     ;; バッファの情報をハイライトする
     anything-c-moccur-higligt-info-line-flag t
     ;; 現在選択中の候補の位置を他のwindowに表示する
     anything-c-moccur-enable-auto-look-flag t
     ;; 起動時にポイントの位置の単語を初期パターンにする
     anything-c-moccur-enable-initial-pattern t))

  ;; kill ringを表示

  (require 'anything-migemo)

  ;; (require 'anything-hatena-bookmark)

  ;; anything-custom-filelist
  (defun anything-custom-filelist ()
    (interactive)
    (anything-other-buffer
     (append
      '(anything-c-source-ffap-line
        anything-c-source-ffap-guesser
        anything-c-source-buffers+
        )
      (anything-c-sources-git-project-for)
      '(anything-c-source-recentf
        anything-c-source-bookmarks
        anything-c-source-file-cache
        anything-c-source-filelist
        ))
     "*anything file list*"))

  ;; C-hで一文字削除になるように
  (define-key anything-map (kbd "C-h") 'delete-backward-char)
  (define-key anything-c-moccur-anything-map (kbd "C-h") 'delete-backward-char)
  )
