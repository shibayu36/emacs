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
               '(alpha . 100)
              default-frame-alist)))

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
;; (defface hlline-face
;;   '((((class color)
;;       (background dark))
;;      (:background "dark slate gray"))
;;     (((class color)
;;       (background light))
;;      (:background  "#98FB98"))
;;     (t
;;      ()))
;;   "*Face used by hl-line.")
;; (setq hl-line-face 'hlline-face)
;; (global-hl-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; フォント設定 ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://d.hatena.ne.jp/setoryohei/20110117/1295336454
;; フレームのフォントを設定
(let* ((size 12) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
       (asciifont "Menlo") ; ASCIIフォント
       (jpfont "Hiragino Kaku Gothic ProN") ; 日本語フォント
       (applefont "AppleMyungjo")
       (h (* size 10))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont))
       (apple-fontspec (font-spec :family applefont)))
  (set-face-attribute 'default nil :family asciifont :height h)
  (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
  (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font nil '(#x0370 . #x03FF) fontspec)    ; ギリシャ文字
  (set-fontset-font nil '(#xE000 . #xF8FF) apple-fontspec)    ; アップルマークとか
  )
;; フォントサイズの比を設定
(dolist (elt '(("^-apple-hiragino.*" . 1.2)
               (".*osaka-bold.*" . 1.2)
               (".*osaka-medium.*" . 1.2)
               (".*courier-bold-.*-mac-roman" . 1.0)
               (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
               (".*monaco-bold-.*-mac-roman" . 0.9)))
  (add-to-list 'face-font-rescale-alist elt))

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
