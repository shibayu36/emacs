;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;javascriptモード;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun my-js2-mode-hook ()
  ;; (require 'espresso)
  ;; (setq ;; espresso-indent-level 4
  ;;       indent-tabs-mode nil
  ;;       c-basic-offset 4)
  ;; (c-toggle-auto-state 0)
  ;; (c-toggle-hungry-state 1)
  ;; (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)

  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)
