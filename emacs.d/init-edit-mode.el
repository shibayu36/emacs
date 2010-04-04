;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;html-helper-mode設定;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'html-helper-mode "html-helper-mode" "Ya HTML" t)
(setq auto-mode-alist 
      (append '(
                ("\\.\\(html\\|htm\\)\\'" . html-helper-mode)
                ) auto-mode-alist))
(add-hook 'html-helper-mode-hook
          '(lambda ()
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (setq intelligent-tab nil)
             ) t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;phpモード設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'php-mode)
;;abbrev対応
(add-hook 'php-mode-hook
          '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))
;;TAB幅指定
(add-hook 'php-mode-user-hook
          '(lambda ()
             (setq tab-width 2)
             (setq indent-tabs-mode nil))
          )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;cssモード設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'css-mode "css-mode" "CSS mode" t)
(setq auto-mode-alist       
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;pythonモード設定;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;メジャーモードの自動起動設定
(autoload 'python-mode "python-mode" "Major mode for editing Python programs" t)
(autoload 'py-shell "python-mode" "Python shell" t)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
;;インデントをスペースに
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil))))

;;ipython設定
;;(setq ipython-command "/usr/local/bin/ipython")
;;(require 'ipython)
             
;;pycompleteの有効化
;;なぜかうまくいかない
;; (add-hook 'python-mode-hook '(lambda ()
;;                                (require 'pycomplete)
;;                                ))

;;pysmellの有効化
;;うまくいかない
;; (require 'pysmell)
;; (add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))
