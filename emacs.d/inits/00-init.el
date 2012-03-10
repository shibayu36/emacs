;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;キーの設定;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq system-type 'darwin)       ; もし、システムが Mac のとき
;;  (setq mac-command-key-is-meta nil) ; コマンドキーをメタにしない
;;  (setq mac-option-modifier 'meta)   ; オプションキーをメタに
  ;;superを割り当てておくと、linux、windowsでうまく使えない
  ;;(setq mac-command-modifier 'super) ; コマンドキーを Super に
  (setq mac-pass-control-to-system t)) ; コントロールキーを Mac ではなく Emacs に渡す

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; exec-pathの設定 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://sakito.jp/emacs/emacsshell.html#path
;; より下に記述した物が PATH の先頭に追加されます

(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

(setenv "DYLD_FALLBACK_LIBRARY_PATH"
        (concat "/usr/local/mysql/lib:"
                "/usr/local/lib:"
                (getenv "DYLD_FALLBACK_LIBRARY_PATH")))
(setenv "NODE_PATH"
        (concat "~/node_modules/:"
                (getenv "NODE_PATH")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;emacs本体の設定;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;文字コード設定
(setq default-buffer-file-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8-unix)
(set-language-environment "Japanese")
(setq locale-coding-system 'utf-8)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;;HTMLのMETAタグコーディング無視
(setq auto-coding-functions nil)

;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;;yes-noの選択肢をy-nにする
(fset 'yes-or-no-p 'y-or-n-p)

;;; Autosaveファイルの場所指定
;;(setq auto-save-list-file-prefix (expand-file-name "~/.emacs.d/.autosave/"))
;;オートセーブファイルを作らない
(setq auto-save-default nil)

;; Backup fileの場所指定
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/.backup"))
            backup-directory-alist))

;;; スクロールを一行ずつにする
;; (setq scroll-step 1)
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;;タブの代わりに半角スペースを使う
(setq-default tab-width 4 indent-tabs-mode nil)

;; 保存時に無駄なスペースを削除
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;ログの記録行数を増やす
(setq message-log-max 10000)

;;;サーバ起動
(require 'server)

;;;クライアントを終了するとき終了するかどうかを聞かない
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;自動再読み込み
(global-auto-revert-mode 1)

;;; 定義マクロファイル設定
(defvar kmacro-save-file "~/.emacs.d/inits/70-mymacros.el")

;; ブックマーク設定
;;ブックマークを変更したら即保存
(setq bookmark-save-flag 1)
(progn
  (setq bookmark-sort-flag nil)
  (defun bookmark-arrange-latest-top ()
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (cons latest (delq latest bookmark-alist))))
    (bookmark-save))
  (add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top))


;; key-chord
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)

;;; debug用
(setq debug-on-error t)

