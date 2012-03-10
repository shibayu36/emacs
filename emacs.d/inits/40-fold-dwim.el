;; ブロックの折畳みと展開
;; http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/fold-dwim.el
;; (when (require 'fold-dwim nil t)
;;   (require 'hideshow nil t)
;;   ;; 機能を利用するメジャーモード一覧
;;   (let ((hook))
;;     (dolist (hook
;;              '(emacs-lisp-mode-hook
;;                c-mode-common-hook
;;                python-mode-hook
;;                php-mode-hook
;;                ruby-mode-hook
;;                js2-mode-hook
;;                css-mode-hook
;;                cperl-mode
;;                apples-mode-hook))
;;       (add-hook hook 'hs-minor-mode))))