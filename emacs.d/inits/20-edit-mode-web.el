(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tt\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))

(setq web-mode-engines-alist
      '(
        ("template-toolkit" . "\\.html?\\'" )
;;        ("razor" . "scala\\.html\\'")
        ))

(defun web-mode-element-close-and-indent ()
  (interactive)
  (web-mode-element-close)
  (indent-for-tab-command))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-auto-indentation nil)

(custom-set-faces
 '(web-mode-doctype-face           ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-tag-face          ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-tag-bracket-face  ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-attr-name-face    ((t (:foreground "#87CEEB"))))
 '(web-mode-html-attr-equal-face   ((t (:foreground "#FFFFFF"))))
 '(web-mode-html-attr-value-face   ((t (:foreground "#D78181"))))

 '(web-mode-css-at-rule-face       ((t (:foreground "#DFCF44"))))
 '(web-mode-css-selector-face      ((t (:foreground "#DFCF44"))))
 '(web-mode-css-pseudo-class       ((t (:foreground "#DFCF44"))))
 '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-css-string-face        ((t (:foreground "#D78181")))))

(add-hook 'web-mode-hook
          '(lambda ()
             (require 'hatena-translator)
             (define-key web-mode-map (kbd "C-c /") 'web-mode-element-close-and-indent)
             (define-key web-mode-map [(meta t)] 'hatena-translator:popup-msgid-at-point)
             (define-key web-mode-map [(meta T)] 'hatena-translator:open-msgid-at-point)
             (define-key web-mode-map (kbd "T") (smartchr '("T" "[%- `!!' %]" "[% `!!' %]")))))

(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-enable-auto-indentation nil)))
