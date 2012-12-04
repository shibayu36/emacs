;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;キーバインドの設定;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'key-chord)
(require 'space-chord)
(require 'smartchr)

(global-set-key "\C-h" 'delete-backward-char)
;; (global-set-key "\e[3~" 'delete-char)

(global-set-key "\C-x\C-i" 'indent-region) ; 選択範囲をインデント
(global-set-key "\C-m" 'newline-and-indent) ; リターンで改行とインデント
(global-set-key "\C-j" 'newline) ; 改行

(define-key global-map "\C-xF" 'ns-toggle-fullscreen)
(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-x ?") 'help-command)

(define-key global-map (kbd "C-c C-a") 'delete-trailing-whitespace)
(global-set-key (kbd "C-t") 'other-window-or-split)

(define-key global-map (kbd "C-;") 'anything-filelist+);;anything-filelist+
(define-key global-map (kbd "C-:") 'anything);;anything
(global-set-key (kbd "C-x C-h") 'anything-for-document)
(global-set-key (kbd "C-M-o")
                'anything-c-moccur-occur-by-moccur)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(define-key global-map [(control ?:)] 'anything-migemo)
(global-set-key (kbd "C-c C-g") 'git-grep)
(global-set-key (kbd "C-c g") 'anything-git-grep-all)
(define-key global-map [(control @)] 'anything-gtags-from-here)

(global-set-key (kbd "C-t") 'other-window-or-split)

(global-set-key (kbd "M-N") 'next-error)
(global-set-key (kbd "M-P") 'previous-error)

(global-set-key (kbd "C-x v") 'magit-status)

(global-set-key (kbd "<f5>") 'slime-js-reload)

(global-set-key (kbd "C-M-g") 'igrep-find)
(global-set-key (kbd "C-M-f") 'find-dired)

;;; 辞書引く
(define-key global-map (kbd "C-M-d") 'ns-popup-dictionary)

;;; direx other window
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

;; カーソル位置に目印つけるやつ
(global-set-key (kbd "M-SPC") 'bm-toggle)
(global-set-key (kbd "M-[") 'bm-previous)
(global-set-key (kbd "M-]") 'bm-next)

;; point-undo
(define-key global-map (kbd "<f7>") 'point-undo)
(define-key global-map (kbd "S-<f7>") 'point-redo)

;; redo
(global-set-key "\M-/" 'redo)

;;; jaunte
;;; カーソルの移動
(global-set-key (kbd "C-c C-j") 'jaunte)

;;; mode compile
(global-set-key "\C-cc" 'mode-compile)

;;; key-combo setting
(require 'key-combo)
(key-combo-mode 0)
(key-combo-define-global (kbd "=") '(" = " " == "))
(key-combo-define-global (kbd "=>") " => ")
(key-combo-define-global (kbd ">") '(">"))
(key-combo-define-global (kbd ">=") " >= ")

;;;
(global-set-key "\C-x@" '(lambda ()
                           (interactive)
                           (split-window-horizontally-n 3)))
