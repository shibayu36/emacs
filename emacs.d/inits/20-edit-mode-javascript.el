;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;javascriptモード;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)

;;; js2-modeでのalignルール
(add-hook
 'align-load-hook
 (lambda ()
   (add-to-list
    'align-rules-list
    '(javascript-equal-delimiter
      (regexp . "\\(\\s-*\\)=")
      (repeat . t)
      (modes  . '(js2-mode))))
   (add-to-list
    'align-rules-list
    '(javascript-object-delimiter
      (regexp . ":\\(\\s-*\\)")
      (repeat . t)
      (modes  . '(js2-mode))))))
