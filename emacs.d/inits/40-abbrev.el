;; 保存先を指定する
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
;; 略称展開のキーバインドを指定する
(define-key esc-map  " " 'expand-abbrev) ;; M-SPC
;; 起動時に保存した略称を読み込む
(quietly-read-abbrev-file)
;; 略称を保存する
(setq save-abbrevs t)
;;自動展開無効
(add-hook 'pre-command-hook
          (lambda ()
            (setq abbrev-mode nil)))
;;abbrev-complete設定
(load "abbrev-complete")
;;dabbrevにおけるハイライト設定
;; (require 'dabbrev-highlight)
