;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;キーバインドの設定;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'key-chord)
(require 'space-chord)
(require 'smartchr)

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\e[3~" 'delete-char)

(global-set-key "\C-x\C-i" 'indent-region) ; 選択範囲をインデント
(global-set-key "\C-m" 'newline-and-indent) ; リターンで改行とインデント
(global-set-key "\C-j" 'newline) ; 改行

(define-key global-map "\C-xF" 'mac-toggle-max-window)
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
(global-set-key (kbd "C-c g") 'anything-git-grep-all)
(define-key global-map [(control @)] 'anything-gtags-from-here)

(global-set-key (kbd "C-t") 'other-window-or-split)

(global-set-key (kbd "M-N") 'next-error)
(global-set-key (kbd "M-P") 'previous-error)

(global-set-key (kbd "C-x v") 'magit-status)

(key-chord-define-global "RR" 'remember)

(define-key global-map (kbd "C-c C-f") 'anything-git-project)

(global-set-key "\C-cc" 'mode-compile)

(global-set-key (kbd "C-M-g") 'igrep-find)

;;; 辞書引く
(define-key global-map (kbd "C-M-d") 'ns-popup-dictionary)
