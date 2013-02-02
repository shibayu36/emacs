;;auto-complete-modeの読み込み
(require 'auto-complete-config)
(setq ac-delay 0.01)
(setq ac-auto-show-menu 0.2)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/ac-dict")
(ac-config-default)
(setq ac-use-menu-map t)
