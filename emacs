;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;ロードパス追加設定;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/")
        (expand-file-name "~/.emacs.d/elisp/")
        (expand-file-name "~/.emacs.d/elisp/anything/")
        (expand-file-name "~/.emacs.d/elisp/abbrev/")
        (expand-file-name "~/.emacs.d/elisp/pymacs/")
        (expand-file-name "~/.emacs.d/elisp/mode/html/")
        (expand-file-name "~/.emacs.d/elisp/mode/nxhtml/")
        (expand-file-name "~/.emacs.d/elisp/mode/css/")
        (expand-file-name "~/.emacs.d/elisp/mode/php-mode/")
        (expand-file-name "~/.emacs.d/elisp/mode/python-mode/")
        (expand-file-name "~/.emacs.d/elisp/mode/perl/")
        (expand-file-name "~/.emacs.d/elisp/mode/psgml/")
        (expand-file-name "~/.emacs.d/elisp/auto-complete/")
        )
       load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;バイトコンパイル設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
(global-set-key "\C-x\C-g" 'goto-line) ;C-x C-gで行ジャンプ
(global-set-key "\C-cm" 'my-mac-toggle-max-window);全画面表示の設定
(define-key global-map (kbd "C-]") 'anything);;anything用キーバインド
;;(global-set-key "\C-;" 'anything)




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

;;色の設定
(set-face-foreground 'font-lock-comment-face "red")

;;行番号の表示
(require 'linum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 編集行を目立たせる（現在行をハイライト表示する）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "pale green"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;; (setq hl-line-face 'underline) ; 下線
(global-hl-line-mode)


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
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete//ac-dict")
(ac-config-default)




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

;;abbrev設定
(load "init-abbrev")

;;pymacsの設定
(load "init-pymacs")

;;; 定義した基本的な関数群
(if (file-exists-p "~/.emacs.d/my-funcs.el")
    (load "my-funcs.el"))

;;flymakeの設定
(load "init-flymake.el")

;;編集モードの設定
(load "init-edit-mode")

;;term-modeの設定
(load "init-term-mode.el")

;;tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(setq recentf-auto-cleanup 'never)

