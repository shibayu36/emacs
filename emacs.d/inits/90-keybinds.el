;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;キーバインドの設定;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)

(require 'space-chord)
(require 'smartchr)

(require 'key-combo)
(key-combo-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; 通常操作 ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-x\C-i" 'indent-region) ; 選択範囲をインデント
(global-set-key "\C-m" 'newline-and-indent) ; リターンで改行とインデント
(global-set-key "\C-j" 'newline) ; 改行
(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(define-key global-map (kbd "C-c C-a") 'delete-trailing-whitespace)
(global-set-key (kbd "C-t") 'other-window-or-split)
;; 複数行移動
(global-set-key "\M-n" (kbd "C-u 5 C-n"))
(global-set-key "\M-p" (kbd "C-u 5 C-p"))

;; フルスクリーン
(global-set-key (kbd "C-x F") 'ns-toggle-fullscreen)
(global-set-key (kbd "C-x ?") 'help-command)

;;; 少しずつスクロール
(define-key global-map (kbd "C-M-n") 'scroll-down-in-place)
(define-key global-map (kbd "C-M-p") 'scroll-up-in-place)

;;; 置換
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c R") 'foreign-regexp/query-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; for anything ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-;") 'anything-custom-filelist) ;;自分の定義
(global-set-key (kbd "C-:") 'anything);;anything
(global-set-key (kbd "C-x C-z") 'anything-resume)
(global-set-key (kbd "C-x C-h") 'anything-for-document)
(global-set-key (kbd "C-M-o")
                'anything-c-moccur-occur-by-moccur)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(define-key global-map [(control ?:)] 'anything-migemo)
(global-set-key (kbd "C-c g") 'anything-git-grep-all)
(define-key global-map [(control @)] 'anything-exuberant-ctags-select-from-here)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; その他 ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c C-g") 'git-grep)

(global-set-key (kbd "M-N") 'next-error)
(global-set-key (kbd "M-P") 'previous-error)

(global-set-key (kbd "C-x v") 'magit-status)

(global-set-key (kbd "<f5>") 'slime-js-reload)

(global-set-key (kbd "C-M-g") 'grep-find)
(global-set-key (kbd "C-M-f") 'find-dired)

;;; 辞書引く
(define-key global-map (kbd "C-M-d") 'ns-popup-dictionary)

(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

;; bmモード
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

;;; 複数行移動

;;; view mode
;;; なんかanythingと競合する
;; (key-chord-define-global "jk" 'view-mode)

;;; cua-mode
(define-key global-map (kbd "<C-return>") 'CUA-cmd-begin-rectangle)

;;; org-mode用
(define-key global-map (kbd "C-c l") 'org-store-link)

;;; yasnippet
(global-set-key (kbd "C-c y") 'yas-insert-snippet)

;;; lisp mode用
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "L") (smartchr '("L" " ; => ")))

;;; for dired
(define-key dired-mode-map "\C-m" 'dired-my-advertised-find-file)
(define-key dired-mode-map "^" 'dired-my-up-directory)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;; isearch-mode
(define-key isearch-mode-map (kbd "C-o") 'anything-c-moccur-from-isearch)

;;; direx-mode
(global-set-key (kbd "C-x C-j") 'direx-project:jump-to-project-root-other-window)
(define-key direx:direx-mode-map (kbd "TAB") 'direx:maybe-find-node)
