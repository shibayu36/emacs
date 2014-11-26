(require 'go-mode)

(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)))

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "green"
                    :weight 'bold)

;;; helm-doc
(defvar my/helm-go-source
  '((name . "Helm Go")
    (candidates . go-packages)
    (action . (("Show document" . godoc)
               ("Import package" . my/helm-go-import-add)))))

(defun my/helm-go-import-add (candidate)
  (dolist (package (helm-marked-candidates))
    (go-import-add current-prefix-arg package)))

(defun my/helm-go ()
  (interactive)
  (helm :sources '(my/helm-go-source) :buffer "*helm go*"))

;;; flycheck
(add-hook 'go-mode-hook 'flycheck-mode)

;;; auto import
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
