;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;javascriptモード;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))

;;; コメントスタイルを//に
(setq web-mode-comment-formats (remove '("javascript" . "/*") web-mode-comment-formats))
(add-to-list 'web-mode-comment-formats '("javascript" . "//"))
;; (add-hook
;;  'js2-mode-hook
;;  (lambda ()
;;    (setq js2-basic-offset 2)))
;; (setq js2-basic-offset 2)

;; ;;; js2-modeでのalignルール
;; (add-hook
;;  'align-load-hook
;;  (lambda ()
;;    (add-to-list
;;     'align-rules-list
;;     '(javascript-equal-delimiter
;;       (regexp . "\\(\\s-*\\)=")
;;       (repeat . t)
;;       (modes  . '(js2-mode))))
;;    (add-to-list
;;     'align-rules-list
;;     '(javascript-object-delimiter
;;       (regexp . ":\\(\\s-*\\)")
;;       (repeat . t)
;;       (modes  . '(js2-mode))))))

;; ;;; flow
;; (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
