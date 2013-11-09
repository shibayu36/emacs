(require 'go-mode-load)

(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)))

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "green"
                    :weight 'bold)
