;;; scala mode
(require 'scala-mode)
(require 'sbt-mode)

;;; Use popwin for sbt-mode
(push '("\*sbt\*" :regexp t :height 0.5 :stick t) popwin:special-display-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; ensime settings ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ensime)
(setq ensime-startup-snapshot-notification nil)
(setq ensime-startup-notification nil)
(setq ensime-eldoc-hints nil) ;; カーソル移動が重くなるのでやめる
(setq ensime-completion-style nil) ;; 補完は遅いので無効
(setq ensime-typecheck-when-idle nil) ;; 定期的にtypecheckするのをやめる
(setq ensime-sem-high-enabled-p nil) ;; semantic highlightをしない

(defun ensime-restart ()
  "Restart the ensime server."
  (interactive)
  (ensime-shutdown)
  (sit-for 2)
  (ensime))

;;; 今のファイルのimportが書かれている部分にpopupする
(defun scala/popup-on-last-import ()
  (interactive)
  (popwin:popup-buffer (current-buffer) :height 0.4)
  (re-search-backward "^import " nil t))

(custom-set-faces
 '(ensime-errline-highlight ((t (:inherit flycheck-error))))
 '(ensime-warnline-highlight ((t (:inherit flycheck-warning)))))

;;; ensimeのタグジャンプを使うようにする
(defun scala/use-ensime-definition-jump ()
  (interactive)
  (define-key scala-mode-map (kbd "C-@") 'ensime-edit-definition)
  (define-key scala-mode-map (kbd "M-@") 'ensime-pop-find-definition-stack))

;;; ctagsのタグジャンプを使うようにする
(defun scala/use-ctags-definition-jump ()
  (interactive)
  (define-key scala-mode-map (kbd "C-@") nil)
  (define-key scala-mode-map (kbd "M-@") nil))

(defun sbt/test-only-current-spec ()
  "Run test with current file."
  (interactive)
  (sbt-command
   (format "testOnly %s" (scala/find-spec-name-with-package-current-buffer))))

(defun sbt/test-only-current-describe ()
  "Run current describe test"
  (interactive)
  (sbt-command
   (format "testOnly %s -- -z \"%s\""
           (scala/find-spec-name-with-package-current-buffer)
           (scala/find-nearest-spec-describe-current-buffer))))

(defun scala/copy-test-only-current-describe ()
  "Copy current describe test to kill ring"
  (interactive)
  (kill-new
   (format "testOnly %s -- -z \"%s\""
           (scala/find-spec-name-with-package-current-buffer)
           (scala/find-nearest-spec-describe-current-buffer))))

(defun scala/copy-test-only-current-spec ()
  "Copy current spec to kill ring"
  (interactive)
  (kill-new
   (format "testOnly %s" (scala/find-spec-name-with-package-current-buffer))))

(defun scala/find-spec-name-with-package-current-buffer ()
  "Find spec name with package in current buffer."
  (interactive)
  (let* ((package-name (scala/find-package-name-current-buffer))
         (spec-name (scala/find-spec-name-current-buffer)))
    (if (string= package-name "")
        spec-name
      (format "%s.%s" package-name spec-name))))

(defun scala/find-package-name-current-buffer ()
  "Find package name in current buffer"
  (interactive)
  (let* ((matched-package ""))
    (save-excursion
      (when (re-search-backward "^package \\(.+\\)$" nil t)
        (setq matched-package (match-string 1))))
    matched-package))

(defun scala/find-spec-name-current-buffer ()
  "Find spec name of current buffer."
  (interactive)
  (let* ((matched-spec-name ""))
    (save-excursion
      (when (re-search-backward "^class \\([^ ]+Spec\\) " nil t)
        (setq matched-spec-name (match-string 1))))
    matched-spec-name))

(defun scala/find-nearest-spec-describe-current-buffer ()
  (interactive)
  (let* ((matched-describe-name ""))
    (save-excursion
      (when (re-search-backward "\\bdescribe(\"\\([^\"]+\\\)\")" nil t)
        (setq matched-describe-name (match-string 1))))
    matched-describe-name))

(defun shibayu36/scala-mode-hook ()
  (setq scala-indent:use-javadoc-style t)
  (flycheck-mode t)
  (auto-complete-mode -1)
  (company-mode +1)
  ;; (flycheck-scala-sbt-init)
  )
(add-hook 'scala-mode-hook 'shibayu36/scala-mode-hook)

(defun shibayu36/ensime-mode-hook ()
  ;; save時にensimeのtypecheckを行わないように
  (remove-hook 'ensime-source-buffer-saved-hook 'ensime-typecheck-current-buffer))
(add-hook 'ensime-mode-hook 'shibayu36/ensime-mode-hook)

;; ensimeを有効化したら、ensimeのタグジャンプを使うようにする
;; (advice-add 'ensime :after #'scala/use-ensime-definition-jump)

;;; scalaでのalignルール
(add-hook
 'align-load-hook
 (lambda ()
   (add-to-list
    'align-rules-list
    '(scala-allow-delimiter
      (regexp . "\\(\\s-*\\)->")
      (repeat . t)
      (modes  . '(scala-mode))))
   (add-to-list
    'align-rules-list
    '(scala-allow2-delimiter
      (regexp . "\\(\\s-*\\)=>")
      (repeat . t)
      (modes  . '(scala-mode))))))

;;; Key bindings
(define-key ensime-mode-map (kbd "C-c C-t") nil) ;; Remove ensime prefix bindings
(define-key scala-mode-map (kbd "C-x C-j") 'open-by-intellij)
(define-key scala-mode-map (kbd "M-t") 'ensime-type-at-point)
(define-key scala-mode-map (kbd ",") (smartchr '("," " => " " -> ")))
(define-key scala-mode-map (kbd "<") (smartchr '("<" " <- ")))
(define-key scala-mode-map (kbd "C-c C-c C-u") 'scala/popup-on-last-import)
(define-key scala-mode-map (kbd "C-c C-t") 'scala/copy-test-only-current-spec)
(define-key scala-mode-map (kbd "C-c t") 'scala/copy-test-only-current-describe)
