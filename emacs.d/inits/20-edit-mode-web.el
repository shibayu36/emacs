(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("template-toolkit" . "\\.html?\\'" )))

(add-hook 'web-mode-hook
          '(lambda ()
             (require 'hatena-translator)
             (setq web-mode-markup-indent-offset 2)
             (define-key web-mode-map [(meta t)] 'hatena-translator:popup-msgid-at-point)
             (define-key web-mode-map [(meta T)] 'hatena-translator:open-msgid-at-point)
             (define-key web-mode-map (kbd "T") (smartchr '("T" "[%- `!!' %]" "[% `!!' %]")))))
