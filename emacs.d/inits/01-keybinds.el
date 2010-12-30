;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;キーバインドの設定;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'key-chord)
(require 'space-chord)

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\e[3~" 'delete-char)

(global-set-key "\C-x\C-i" 'indent-region) ; 選択範囲をインデント
(global-set-key "\C-m" 'newline-and-indent) ; リターンで改行とインデント
(global-set-key "\C-j" 'newline) ; 改行

(global-set-key "\C-cc" 'comment-region) ; C-c c を範囲指定コメントに
(global-set-key "\C-cu" 'uncomment-region) ; C-c u を範囲指定コメント解除に
(define-key global-map "\C-xF" 'mac-toggle-max-window)
(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-x ?") 'help-command)

(define-key global-map (kbd "C-c C-a") 'delete-trailing-whitespace)
(global-set-key (kbd "C-t") 'other-window-or-split)

(define-key global-map (kbd "C-;") 'anything-for-files);;anything-for-files
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

(key-chord-define-global "rr" 'remember)

(define-key global-map (kbd "C-c C-f") 'anything-git-project)

(global-set-key "\C-c\C-c" 'mode-compile)
