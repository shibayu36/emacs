;; (require 'flyspell)
;; (require 'ispell)
;; (setq ispell-program-name "aspell")

;; ;;; 日本語混じりの文章チェック
;; (eval-after-load "ispell"
;;  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; ;;; keybinding取られるの防ぐ
;; (define-key flyspell-mode-map (kbd "C-;") nil)
;; (define-key flyspell-mode-map (kbd "C-.") nil)
;; (define-key flyspell-mode-map (kbd "C-,") nil)
