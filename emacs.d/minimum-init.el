;;; ロードパス追加設定
(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/")
        (expand-file-name "~/.emacs.d/elisp/")
        (expand-file-name "~/.emacs.d/elisp/el-get/el-get/")

        (expand-file-name "~/.emacs.d/elisp/mode/")
        (expand-file-name "~/.emacs.d/elisp/mode/jshint-mode/")
        (expand-file-name "~/Dropbox/config-file/.emacs.d/elisp/"))
       load-path))

(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/elisp/themes/"))

;;; ELPA 設定
(require 'package)

(setq package-user-dir "~/.emacs.d/elisp/elpa/")
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;;; el-get
(require 'el-get)
(setq el-get-dir "~/.emacs.d/elisp/el-get/")

;;; キー設定
(when (eq system-type 'darwin)       ; もし、システムが Mac のとき
  (setq mac-pass-control-to-system t)) ; コントロールキーを Mac ではなく Emacs に渡す

(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

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

;;; binds
(global-set-key (kbd "C-h") 'delete-backward-char)

;;カラーテーマ
(load-theme 'dark-laptop t t)
(enable-theme 'dark-laptop)
(set-face-attribute 'isearch nil :foreground "white" :background "blue") ;; isearchのforegroundがおかしいのを修正

;; 対応する括弧を光らせる。
(show-paren-mode t)

;; 選択部分のハイライト
(transient-mark-mode t)

;;; tool-bar使わない
(tool-bar-mode 0)

;;windowの設定
(setq default-frame-alist
      (append (list
               '(width . 175)
               '(height . 47)
               '(top . 0)
               '(left . 0)
               '(alpha . (100 60)))
              default-frame-alist))

;;; other-window周り
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)


;; 使い捨てファイルを開けるように
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y-%m-%d-%H%M%S.")
