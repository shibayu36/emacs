(setq view-read-only t)

(require 'view)
;; less感覚の操作
(define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
(define-key view-mode-map (kbd "?") 'View-search-regexp-backward)
(define-key view-mode-map (kbd "G") 'View-goto-line-last)
(define-key view-mode-map (kbd "b") 'View-scroll-page-backward)
(define-key view-mode-map (kbd "f") 'View-scroll-page-forward)
;; vi/w3m感覚の操作
(define-key view-mode-map (kbd "h") 'backward-char)
(define-key view-mode-map (kbd "l") 'forward-char)
(define-key view-mode-map (kbd "j") 'scroll-down-in-place)
(define-key view-mode-map (kbd "k") 'scroll-up-in-place)
;; bm.elの設定
(require 'bm)
(define-key view-mode-map (kbd "m") 'bm-toggle)
(define-key view-mode-map (kbd "[") 'bm-previous)
(define-key view-mode-map (kbd "]") 'bm-next)

(require 'viewer)
(viewer-stay-in-setup) ;; 書き込み不能ファイルは書き込みにならないように

;; view-modeのときmode-lineに色付け
(setq viewer-modeline-color-unwritable "tomato")
(setq viewer-modeline-color-view "orange")
(viewer-change-modeline-color-setup)

;; logファイルはviewer
(setq view-mode-by-default-regexp "\\.log$")
