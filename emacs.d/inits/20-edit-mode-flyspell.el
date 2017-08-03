;; (require 'ispell)
;; (setq ispell-program-name "aspell")
;; (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

;; ;;; flyspell
;; (require 'flyspell)
;; (mapc
;;  (lambda (hook)
;;    (add-hook hook 'flyspell-prog-mode))
;;  '(
;;    ;; コメント領域だけflyspellかけたいmode
;;    cperl-mode-hook
;;    typescript-mode-hook
;;    ))

;; (mapc
;;  (lambda (hook)
;;    (add-hook hook
;;              '(lambda () (flyspell-mode 1))))
;;  '(
;;    ;; 全領域flyspellかけたいmode
;;    markdown-mode-hook
;;    ))
