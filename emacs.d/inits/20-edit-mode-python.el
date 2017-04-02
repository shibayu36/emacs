(require 'python-mode)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(require 'virtualenvwrapper)
(require 'auto-virtualenvwrapper)
(add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
