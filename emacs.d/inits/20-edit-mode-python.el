(require 'python)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; methodのドキュメントはminibufferに表示
(setq jedi:tooltip-method 'nil)
(set-face-attribute 'jedi:highlight-function-argument nil
                    :foreground "green")

(require 'virtualenvwrapper)
(require 'auto-virtualenvwrapper)
(add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
