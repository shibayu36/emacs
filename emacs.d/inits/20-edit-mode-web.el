(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tt\\'" . web-mode))

(setq web-mode-engines-alist
      '(("template-toolkit" . "\\.html?\\'" )))

(defun web-mode-element-close-and-indent ()
  (interactive)
  (web-mode-element-close)
  (indent-for-tab-command))

(add-hook 'web-mode-hook
          '(lambda ()
             (require 'hatena-translator)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-enable-auto-indentation nil)
             (define-key web-mode-map (kbd "C-c /") 'web-mode-element-close-and-indent)
             (define-key web-mode-map [(meta t)] 'hatena-translator:popup-msgid-at-point)
             (define-key web-mode-map [(meta T)] 'hatena-translator:open-msgid-at-point)
             (define-key web-mode-map (kbd "T") (smartchr '("T" "[%- `!!' %]" "[% `!!' %]")))))

(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-enable-auto-indentation nil)))
