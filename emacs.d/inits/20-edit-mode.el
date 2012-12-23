;; default mode is text mode
(setq default-major-mode 'text-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;mmmモード;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face "navy")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;html-mode設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'zencoding-mode)
(setq auto-mode-alist
      (append '(
                ("\\.\\(html\\|htm\\|tt\\|tt2\\|tx\\)\\'" . html-mode)
                ) auto-mode-alist))
(add-hook 'html-mode-hook
          '(lambda ()
             (require 'hatena-translator)
             (zencoding-mode)
             (define-key html-mode-map [(meta t)] 'hatena-translator:popup-msgid-at-point)
             (define-key html-mode-map [(meta T)] 'hatena-translator:open-msgid-at-point)
             (define-key zencoding-mode-keymap (kbd "<C-return>") 'zencoding-expand-line)
             (define-key zencoding-mode-keymap (kbd "C-c C-m") 'zencoding-expand-line)
             (define-key zencoding-preview-keymap (kbd "C-m") 'zencoding-expand-yas)
             (define-key zencoding-preview-keymap (kbd "RET") 'zencoding-expand-yas)
             (define-key zencoding-preview-keymap (kbd "<return>") 'zencoding-expand-yas)
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;yatex;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;はてなmode;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'simple-hatena-mode)
(setq simple-hatena-default-id "shiba_yu36")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;magit-mode;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; evernote mode ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evernote-mode)
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; c-shap mode ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(defun my-csharp-mode-fn ()
  "function that runs when csharp-mode is initialized for a buffer."
  (turn-on-auto-revert-mode)
  (setq indent-tabs-mode nil)
  ;; (require 'flymake)
  ;; (flymake-mode 1)
  (require 'yasnippet)
  (yas/minor-mode-on)
  (require 'rfringe))
(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; yaml mode ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; nginx mode ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'nginx-mode)
;; (add-to-list 'auto-mode-alist '("\\.nginx\\.conf$" . nginx-mode))
