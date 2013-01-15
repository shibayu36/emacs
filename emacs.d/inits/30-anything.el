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

  ;; anything-c-source-emacs-functionsの高速化
  (setq anything-c-source-emacs-functions
        '((name . "Emacs Functions")
          (init .  (lambda ()
                     (with-current-buffer (anything-candidate-buffer 'global)
                       (insert
                        (mapconcat
                         'identity
                         (let (commands)
                           (mapatoms (lambda (a)
                                       (if (functionp a)
                                           (push (symbol-name a) commands))))
                           (sort commands 'string-lessp))
                         "\n")))))
          (candidates-in-buffer)
          (type . function)
          (requires-pattern . 2)))

  (when (require 'anything-config nil t)
    (setq anything-sources
          '(anything-c-source-buffers
            anything-c-source-recentf
            anything-c-source-man-pages
            anything-c-source-emacs-commands
            anything-c-source-emacs-functions
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

  (require 'anything-migemo)

  ;; (require 'anything-hatena-bookmark)

  ;; C-hで一文字削除になるように
  (define-key anything-map (kbd "C-h") 'delete-backward-char))
