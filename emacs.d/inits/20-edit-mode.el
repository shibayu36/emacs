;; default mode is text mode
(setq major-mode 'text-mode)
(add-hook 'text-mode-hook
          '(lambda ()
             (company-mode -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;mmmモード;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face "navy")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;html-mode設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'zencoding-mode)
;; (setq auto-mode-alist
;;       (append '(
;;                 ("\\.\\(html\\|htm\\|tt\\|tt2\\|tx\\)\\'" . html-mode)
;;                 ) auto-mode-alist))
;; (add-hook 'html-mode-hook
;;           '(lambda ()
;;              (require 'hatena-translator)
;;              (zencoding-mode)
;;              (define-key html-mode-map [(meta t)] 'hatena-translator:popup-msgid-at-point)
;;              (define-key html-mode-map [(meta T)] 'hatena-translator:open-msgid-at-point)
;;              (define-key html-mode-map (kbd "T") (smartchr '("T" "[%- `!!' %]" "[% `!!' %]")))
;;              (define-key html-mode-map (kbd "C-c C-n") 'sgml-skip-tag-forward)
;;              (define-key html-mode-map (kbd "C-c C-p") 'sgml-skip-tag-backward)
;;              (define-key zencoding-mode-keymap (kbd "<C-return>") 'zencoding-expand-line)
;;              (define-key zencoding-mode-keymap (kbd "C-c C-m") 'zencoding-expand-line)
;;              (define-key zencoding-preview-keymap (kbd "C-m") 'zencoding-expand-yas)
;;              (define-key zencoding-preview-keymap (kbd "RET") 'zencoding-expand-yas)
;;              (define-key zencoding-preview-keymap (kbd "<return>") 'zencoding-expand-yas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;phpモード設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'php-mode)
;;abbrev対応
;; (add-hook 'php-mode-hook
;;           '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))
;;TAB幅指定
;; (add-hook 'php-mode-user-hook
;;           '(lambda ()
;;              (setq tab-width 2)
;;              (setq indent-tabs-mode nil))
;;           )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;cssモード設定;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'css-mode)
(setq css-indent-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;lessモード設定;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'less-css-mode)
(setq less-css-indent-level 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; yaml mode ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; nginx mode ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'nginx-mode)
(add-to-list 'auto-mode-alist '("\\.nginx.conf$" . nginx-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; sh mode ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

;;; crontab mode
(setq crontab-mode-map nil)
(require 'crontab-mode)
(add-to-list 'auto-mode-alist '("crontab$" . crontab-mode))

;;; graphviz dot mode
(require 'graphviz-dot-mode)

(require 'lua-mode)

(require 'graphql-mode)
(setq graphql-indent-level 2)
(add-to-list 'auto-mode-alist '("gql$" . graphql-mode))
