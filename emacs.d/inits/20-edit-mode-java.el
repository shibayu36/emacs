(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 2)
            ;; meghanada-mode on
            (meghanada-mode t)))
