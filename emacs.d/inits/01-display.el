;; 対応する括弧を光らせる。
(show-paren-mode t)

;; 選択部分のハイライト
(transient-mark-mode t)

;; 行間
(setq-default line-spacing 0)

;;; 同じバッファ名の時 <2> とかではなく、ディレクトリ名で区別
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; メニューバーにファイルパスを表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;;フォントロックモード
(global-font-lock-mode t)

;;色の設定
(set-face-foreground 'font-lock-comment-face "red")

;;windowの設定
(setq default-frame-alist
      (append (list
               '(width . 175)
               '(height . 47)
               '(top . 0)
               '(left . 0)
               '(alpha . (100 60)))
              default-frame-alist))

;;画面最大化
(defun mac-toggle-max-window ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))
(setq mac-autohide-menubar-on-maximize nil)

;;; tool-bar使わない
(setq tool-bar-mode nil)

;;画面端まで来たら折り返す
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)

;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.1)

;;モードラインに時刻を表示する
(display-time)

;; 行番号・桁番号を表示
(line-number-mode 1)
(column-number-mode 1)

;; モードラインの割合表示を総行数表示
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)

(when my-lines-page-mode
  (setq my-mode-line-format "%d")
  (if size-indication-mode
      (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
  (cond ((and (eq line-number-mode t) (eq column-number-mode t))
         (setq my-mode-line-format (concat my-mode-line-format " (%%l,%%c)")))
        ((eq line-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
        ((eq column-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))

  (setq mode-line-position
        '(:eval (format my-mode-line-format
                        (count-lines (point-max) (point-min))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; フォント設定 ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute
 'default nil
 :family "Menlo"
 :height 120)

(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Hiragino Kaku Gothic ProN"))

(setq face-font-rescale-alist
      '((".*Menlo.*" . 1.0)
        ("Hiragino.*" . 1.2)
        (".*nfmotoyacedar-bold.*" . 1.2)
        (".*nfmotoyacedar-medium.*" . 1.2)
        ("-cdac$" . 1.3)))

;;カラーテーマ
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

;;; スクロールバー消す
(set-scroll-bar-mode 'nil)
