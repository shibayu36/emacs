(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(with-eval-after-load 'typescript-mode
  (dolist (key '("{" "}" "(" ")" ":" ";" ","))
    (define-key typescript-mode-map key nil)))

(require 'tide)
(with-eval-after-load 'tide
  (define-key typescript-mode-map (kbd "C-@") 'tide-jump-to-definition)
  (define-key typescript-mode-map (kbd "M-@") 'tide-jump-back))

(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode t)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode t)
            (company-mode-on)))
