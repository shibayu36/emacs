
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;ロードパス追加設定;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/")
        (expand-file-name "~/.emacs.d/elisp/")
        (expand-file-name "~/.emacs.d/elisp/anything/")
        (expand-file-name "~/.emacs.d/elisp/mode/html/")
        (expand-file-name "~/.emacs.d/elisp/mode/nxhtml/")
        (expand-file-name "~/.emacs.d/elisp/mode/css/")
        (expand-file-name "~/.emacs.d/elisp/mode/php-mode/")
        )
       load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;キーの設定;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq system-type 'darwin)       ; もし、システムが Mac のとき
  (setq mac-command-key-is-meta nil) ; コマンドキーをメタにしない
  (setq mac-option-modifier 'meta)   ; オプションキーをメタに
  ;;superを割り当てておくと、linux、windowsでうまく使えない
  ;;(setq mac-command-modifier 'super) ; コマンドキーを Super に
  (setq mac-pass-control-to-system t)) ; コントロールキーを Mac ではなく Emacs に渡す




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;キーバインドの設定;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\e[3~" 'delete-char)

(global-set-key "\C-x\C-i" 'indent-region) ; 選択範囲をインデント
(global-set-key "\C-m" 'newline-and-indent) ; リターンで改行とインデント
(global-set-key "\C-j" 'newline) ; 改行

(global-set-key "\C-cc" 'comment-region) ; C-c c を範囲指定コメントに
(global-set-key "\C-cu" 'uncomment-region) ; C-c u を範囲指定コメント解除に

(define-key global-map (kbd "C-;") 'anything);;anything用キーバインド





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



;;HTMLのMETAタグコーディング無視
(setq auto-coding-functions nil)
;; 対応する括弧を光らせる。
(show-paren-mode t)

;; 選択部分のハイライト
(transient-mark-mode t)

;; default mode is text mode
(setq default-major-mode 'text-mode)

;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; 表示位置の調節
(setq default-frame-alist
(append (list ' ;(top . 0) ; 起動時の表示位置（右から）
;; '(left . 120) ; 起動時の表示位置（左から）
;; '(width . 90) ; 起動時のサイズ（幅）
'(height . 44) ; 起動時のサイズ（縦）
;; '(alpha . (85 50))
)
default-frame-alist))


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
(setq scroll-step 1)
;;; スクロールバーを右側に表示する
;;(set-scroll-bar-mode 'right)
;;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;;; 同じバッファ名の時 <2> とかではなく、ディレクトリ名で区別
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)



;;emacsのVCでバッファの履歴を引き継ぐための設定
;; checkin 後に実行される関数をフック
(add-hook 'vc-checkin-hook
          '(lambda ()
              (setq buffer-undo-list higepon-san)))
;; checkin 前に実行される関数をフック
(add-hook 'vc-before-checkin-hook
          '(lambda ()
             (setq higepon-san buffer-undo-list)))


;;タブの代わりに半角スペースを使う
(setq-default tab-width 4 indent-tabs-mode nil)


;;フォントロックモード
(global-font-lock-mode t)

;; ;; タブ, 全角スペース, 行末空白を色付き表示
;;シェルモードで邪魔だったため消去
;; (defface my-face-b-1 '((t (:background "NavajoWhite4"))) nil) ; 全角スペース
;; (defface my-face-b-2 '((t (:background "gray10"))) nil) ; タブ
;; (defface my-face-u-1 '((t (:background "SteelBlue" :underline t))) nil) ; 行末空白
;; (defvar my-face-b-1 'my-face-b-1)
;; (defvar my-face-b-2 'my-face-b-2)
;; (defvar my-face-u-1 'my-face-u-1)
;; (defadvice font-lock-mode (before my-font-lock-mode ())
;;   (font-lock-add-keywords
;;    major-mode
;;    '(("\t" 0 my-face-b-2 append)
;;      ("　" 0 my-face-b-1 append)
;;      ("[ \t]+$" 0 my-face-u-1 append)
;;      )))
;; (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
;; (ad-activate 'font-lock-mode)



;;等幅文字設定
;;carbonでの設定
;;(if (= emacs-major-version 22)
;;  (require 'carbon-font))
;;(fixed-width-set-fontset "hiramaru" 12)
;;cocoaでの設定
(when (= emacs-major-version 23)
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (setq face-font-rescale-alist
        '(("^-apple-hiragino.*" . 1.2)
          (".*osaka-bold.*" . 1.2)
          (".*osaka-medium.*" . 1.2)
          (".*courier-bold-.*-mac-roman" . 1.0)
          (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
          (".*monaco-bold-.*-mac-roman" . 0.9)
          ("-cdac$" . 1.3))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;diredの設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "init-dired")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ここからインストールした拡張の設定;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;install-elispコマンド設定
;; まず、install-elisp のコマンドを使える様にします。
(require 'install-elisp)
;; 次に、Elisp ファイルをインストールする場所を指定します。
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")


;;auto-complete-modeの読み込み
(require 'auto-complete)
(global-auto-complete-mode t)



;;auto-install設定
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;;anything 設定
(require 'anything)
(require 'anything-config)
(setq anything-sources
      '(anything-c-source-buffers
        anything-c-source-colors
        anything-c-source-recentf
        anything-c-source-man-pages
        anything-c-source-emacs-commands
        anything-c-source-emacs-functions
        anything-c-source-files-in-current-dir
        ))


;;recentf設定
(recentf-mode 1)

;;shellモード設定
(load "init-shell")

;;編集モードの設定
(load "init-edit-mode")

;;abbrev設定
(load "init-abbrev")
