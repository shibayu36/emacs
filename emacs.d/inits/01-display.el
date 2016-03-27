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
(setq ns-use-native-fullscreen nil) ;; nativeのフルスクリーン使わない

;;; tool-bar使わない
(tool-bar-mode 0)

;;画面端まで来たら折り返す
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)

;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.1)

;; 行番号・桁番号を表示
(line-number-mode 1)
(column-number-mode 1)

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

(defun font-big ()
  (interactive)
  (set-face-attribute 'default nil
                      :family "Menlo"
                      :height 200))

(defun font-small ()
  (interactive)
  (set-face-attribute 'default nil
                      :family "Menlo"
                      :height 120))

;;; スクロールバー消す
(set-scroll-bar-mode nil)

;; display-buffer時に変に分割されるのを防ぐ
(setq pop-up-windows nil)
(setq split-height-threshold nil)
(setq split-width-threshold nil)
