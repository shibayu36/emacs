;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;ruby-mode;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq ruby-indent-level tab-width)
             (setq ruby-deep-indent-paren-style nil)
             (setq ruby-deep-indent-paren nil)
             (setq ruby-insert-encoding-magic-comment nil)))

;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

(require 'rcodetools)
(define-key ruby-mode-map (kbd "C-c C-d") 'xmp)

;; rsense
(setq rsense-home "/usr/local/Cellar/rsense/0.3/libexec")
(require 'rsense)
(add-hook 'ruby-mode-hook
          '(lambda ()
             ;; .や::を入力直後から補完開始
             (add-to-list 'ac-sources 'ac-source-rsense-method)
             (add-to-list 'ac-sources 'ac-source-rsense-constant)
             ;; C-x .で補完出来るようキーを設定
             (define-key ruby-mode-map (kbd "C-x .") 'ac-complete-rsense)))

(require 'rbenv)
(global-rbenv-mode)

;; flycheck
(add-hook 'ruby-mode-hook
          (lambda ()
            (flycheck-mode t)
            (setq flycheck-rubocop-lint-only t)))
(add-hook 'ruby-mode-hook 'smartparens-mode)
