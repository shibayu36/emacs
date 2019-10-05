;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;キーバインドの設定;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smartchr)

(require 'smartrep)

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
(global-set-key (kbd "C-x F") 'toggle-frame-maximized)
(global-set-key (kbd "C-x ?") 'help-command)

;;; 少しずつスクロール
(define-key global-map (kbd "C-s-n") 'scroll-down-in-place)
(define-key global-map (kbd "C-s-p") 'scroll-up-in-place)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; for anything ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-;") 'anything-custom-filelist) ;;自分の定義
(global-set-key (kbd "C-x C-z") 'anything-resume)
(define-key global-map [(control ?:)] 'anything-migemo)
;; (global-set-key (kbd "C-@") 'helm-etags-plus-select-region)
(global-set-key (kbd "C-@") 'helm-etags-plus-select)
(global-set-key (kbd "M-@") 'helm-etags-plus-history-go-back)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;; helm-occur
(global-set-key (kbd "C-M-o") 'helm-occur)

;;; helm-for-document
(global-set-key (kbd "C-x C-h") 'helm-for-document)

;;; helm-hatena-bookamrk
(global-set-key (kbd "C-x C-b") 'helm-hatena-bookmark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; その他 ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c C-g") 'git-grep)
(global-set-key (kbd "C-c g") 'git-grep-from-root)

(global-set-key (kbd "M-N") 'next-error)
(global-set-key (kbd "M-P") 'previous-error)

(global-set-key (kbd "C-x v") 'magit-status)

(global-set-key (kbd "<f5>") 'slime-js-reload)

(global-set-key (kbd "C-M-g") 'ack)
(global-set-key (kbd "C-M-f") 'find-dired)

;;; 辞書引く
;;; (define-key global-map (kbd "C-M-d") 'ns-popup-dictionary)

(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

;;; abbrev
(global-set-key (kbd "M-SPC") 'expand-abbrev)

;; point-undo
(define-key global-map (kbd "C-7") 'point-undo)
(define-key global-map (kbd "M-7") 'point-redo)

;; redo
(global-set-key "\M-/" 'redo)

;;; quickrun
(global-set-key "\C-cc" 'quickrun-with-arg)

;;; auto-complete
;; (define-key global-map (kbd "<C-tab>") 'ac-fuzzy-complete)

;;; org-mode用
(define-key global-map (kbd "C-c l") 'org-store-link)

;;; yasnippet
(global-set-key (kbd "C-c y") 'helm-yas-complete)

;;; lisp mode用
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)
(define-key emacs-lisp-mode-map (kbd "L") (smartchr '("L" " ; => ")))

;;; isearch-mode
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;; replace
(global-set-key (kbd "C-c r") 'anzu-query-replace)
(global-set-key (kbd "C-c R") 'anzu-query-replace-regexp)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
;; (global-set-key (kbd "C-c r") 'query-replace)
;; (global-set-key (kbd "C-c R") 'foreign-regexp/query-replace)

;;; direx-mode
(global-set-key (kbd "C-x C-j") 'direx:jump-to-project-directory)
(define-key direx:direx-mode-map (kbd "TAB") 'direx:maybe-find-node)

;;; multiple-cursors
(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
(smartrep-define-key
 global-map "C-." '(("C-n" . 'mc/mark-next-like-this)
                    ("C-p" . 'mc/mark-previous-like-this)
                    ("*"   . 'mc/mark-all-like-this)))

;;; open-github
(global-set-key (kbd "C-c o f") 'helm-open-github-from-file)
(global-set-key (kbd "C-c o c") 'helm-open-github-from-commit)
(global-set-key (kbd "C-c o i") 'helm-open-github-from-issues)

;;; server-edit
(global-set-key (kbd "C-c C-c C-c") 'server-edit)

;;; M-xはhelmを使う
(global-set-key (kbd "M-x") 'helm-M-x)

;;; highlight-symbol
(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)

(global-set-key (kbd "C-x C-d") 'dash-at-point)

;;; goto-last-change
(define-key global-map (kbd "<f8>") 'goto-last-change)
(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)

;;; auto-programming
(global-set-key (kbd "M-l") 'auto-programming)

;; google-translate
(global-set-key (kbd "C-M-t") 'google-translate-enja-or-jaen)

;; avy
(global-set-key (kbd "C-:") 'avy-goto-char-timer)

;; vc-annotate
(define-key vc-annotate-mode-map (kbd "P") 'vc-annotate-open-pr-at-line)

;;; profiler
(global-set-key (kbd "C-c p s") 'profiler-start)
(global-set-key (kbd "C-c p r") 'profiler-report)

;;; vscode
(define-key global-map (kbd "C-c C-v") 'open-by-vscode)
