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
        (expand-file-name "~/.emacs.d/elisp/yasnippet")
        (expand-file-name "~/.emacs.d/elisp/moccur")
        (expand-file-name "~/.emacs.d/elisp/apel")
        (expand-file-name "~/.emacs.d/elisp/mode/")
        (expand-file-name "~/.emacs.d/elisp/mode/html/")
        (expand-file-name "~/.emacs.d/elisp/mode/nxhtml/")
        (expand-file-name "~/.emacs.d/elisp/mode/css/")
        (expand-file-name "~/.emacs.d/elisp/mode/php-mode/")
        (expand-file-name "~/.emacs.d/elisp/mode/python-mode/")
        (expand-file-name "~/.emacs.d/elisp/mode/perl/")
        (expand-file-name "~/.emacs.d/elisp/mode/psgml/")
        (expand-file-name "~/.emacs.d/elisp/mode/yml/")
        (expand-file-name "~/.emacs.d/elisp/mode/javascript/")
        (expand-file-name "~/.emacs.d/elisp/mode/yatex/")
        (expand-file-name "~/.emacs.d/elisp/auto-complete/")
        (expand-file-name "~/.emacs.d/elisp/mode/hatena/")
        (expand-file-name "~/.emacs.d/elisp/mode/ruby/")
        (expand-file-name "~/.emacs.d/elisp/mode/magit/share/emacs/site-lisp/")
        (expand-file-name "~/.emacs.d/elisp/mode/git/")
        )
       load-path))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;バイトコンパイル設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "/opt/local/sbin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/sbin")
(add-to-list 'exec-path "~/bin")


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
(define-key global-map "\C-xF" 'mac-toggle-max-window)
(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-x ?") 'help-command)

(define-key global-map (kbd "C-c C-a") 'delete-trailing-whitespace)

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

;; メニューバーにファイルパスを表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

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

;;windowの設定
(setq default-frame-alist
      (append (list
               '(width . 175)
               '(height . 47)
               '(top . 0)
               '(left . 0)
               '(alpha . (90 60))) ;;ここ
              default-frame-alist))

;;画面最大化
(defun mac-toggle-max-window ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))
(setq mac-autohide-menubar-on-maximize nil)

(custom-set-variables
 '(display-time-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
 )

;;画面端まで来たら折り返す
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; 保存時に無駄なスペースを削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 編集行を目立たせる（現在行をハイライト表示する）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#98FB98"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;; (setq hl-line-face 'underline) ; 下線
(global-hl-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;全角空白、タブ、行末の空白を目立たせる;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface my-face-tab         '((t (:background "Yellow"))) nil :group 'my-faces)
(defface my-face-zenkaku-spc '((t (:background "LightBlue"))) nil :group 'my-faces)
(defface my-face-spc-at-eol  '((t (:foreground "Red" :underline t))) nil :group 'my-faces)
(defvar my-face-tab         'my-face-tab)
(defvar my-face-zenkaku-spc 'my-face-zenkaku-spc)
(defvar my-face-spc-at-eol  'my-face-spc-at-eol)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-tab append)
     ("　" 0 my-face-zenkaku-spc append)
     ("[ \t]+$" 0 my-face-spc-at-eol append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

(font-lock-mode t)
(font-lock-fontify-buffer)

;;等幅文字設定
;;carbonでの設定
(when (= emacs-major-version 22)
 (require 'carbon-font)
 (fixed-width-set-fontset "hiramaru" 12))
;; cocoaでの設定
(when (= emacs-major-version 23)
    (set-default-font
     "-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))

;;;サーバ起動
(require 'server)

;;;クライアントを終了するとき終了するかどうかを聞かない
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


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

;; info.elの読み込み
(require 'info)

;;anything 設定
(load "init-anything.el")

;;recentf設定
(recentf-mode 1)
(setq recentf-max-saved-items 1000)

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

;;womanの設定
(setq woman-use-own-frame nil)

;;編集モードの設定
(load "init-edit-mode")

;;term-modeの設定
(load "init-term-mode.el")

;;tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(setq recentf-auto-cleanup 'never)


;;カラーテーマ
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

;;yasnippet
(load "init-yasnippet.el")

;;moccur拡張
(require 'moccur-edit)
(require 'color-moccur)
(setq moccur-split-word t)

;;自動再読み込み
(global-auto-revert-mode 1)

;;gitに関する設定のロード
(load "init-git.el")

;; grepから直接置換できるように
(require 'grep-edit)

;; migemoに関する設定
(load "init-migemo.el")

;; undohistの設定
(when (require 'undohist nil t)
  (undohist-initialize))

;; undo-treeモードの設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; elscreenの設定
(load "init-elscreen")

;; jaunte.el
(require 'jaunte)
(global-set-key (kbd "C-c C-j") 'jaunte)

;; set-perl5lib-glob-from-git-root
(require 'set-perl5lib-glob-from-git-root)

;; redo
(require 'redo)
(global-set-key "\M-/" 'redo)

;; sudo-ext
(server-start)
(require 'sudo-ext)

;; outputz
;; (load "init-outputz")