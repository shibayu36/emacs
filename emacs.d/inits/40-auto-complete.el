;;auto-complete-modeの読み込み
(require 'auto-complete-config)
(ac-config-default)
(setq ac-delay 0.1)
(setq ac-auto-show-menu 0.2)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/ac-dict")
(setq ac-use-menu-map t)
(setq ac-use-fuzzy t)
