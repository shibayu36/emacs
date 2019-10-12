(require 'go-mode)

(add-hook 'go-mode-hook #'lsp)

(require 'go-rename)

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
(require 'flycheck-gometalinter)
(flycheck-gometalinter-setup)
(setq flycheck-gometalinter-fast t) ;; only run fast linters
(setq flycheck-gometalinter-test t) ;; use in tests files
(add-hook 'go-mode-hook 'flycheck-mode)

;;; go-mode-map
(define-key go-mode-map (kbd "C-x C-h") 'my/helm-go)
(define-key go-mode-map (kbd "C-@") 'godef-jump)
(define-key go-mode-map (kbd "M-@") 'pop-tag-mark)
(define-key go-mode-map (kbd "M-t") 'godef-describe)

;;; auto import
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(require 'gotest)
(setq go-test-verbose t)
(push '("\*Go Test\*" :regexp t :height 0.5 :stick t) popwin:special-display-config)

;;; lsp-mode
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("bingo" "--mode" "stdio" "--logfile" "/tmp/lspserver.log" "--trace" "--pprof" ":6060"))
;;                   :major-modes '(go-mode)
;;                   :server-id 'bingo))

;; (add-hook 'go-mode-hook 'lsp)

(defun shibayu36/go-mode-hook ()
  ;; golangではハードタブを可視化しない
  (make-local-variable 'whitespace-style)
  (setq whitespace-style
      '(face
        trailing
        spaces
        space-mark))
  ;; タブ幅を2に
  (setq tab-width 2)
  ;; company-mode利用。go-lsp導入しようとしたらこの辺必要
  (auto-complete-mode -1)
  (company-mode +1)
  ;; company補完はlspを利用する
  (setq-local company-backends '(company-lsp))
  )
(add-hook 'go-mode-hook 'shibayu36/go-mode-hook)

;;; Key bindings
(define-key go-mode-map (kbd "C-c C-t") 'go-test-current-file)
(define-key go-mode-map (kbd "C-c t") 'go-test-current-test)
