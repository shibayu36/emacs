;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;javascriptモード;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

(require 'flymake-jslint)
(add-hook 'js-mode-hook 'flymake-jslint-load)
