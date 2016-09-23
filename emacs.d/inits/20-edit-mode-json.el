(require 'json-mode)
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))
(require 'json-reformat)
(setq json-reformat:indent-width 2)
