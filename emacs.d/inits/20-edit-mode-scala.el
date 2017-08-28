;;; scala mode
(require 'scala-mode)
(require 'sbt-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; ensime settings ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ensime)
(setq ensime-startup-snapshot-notification nil)
(setq ensime-startup-notification nil)
(setq ensime-eldoc-hints nil) ;; カーソル移動が重くなるのでやめる
(setq ensime-completion-style nil) ;; 一旦補完はなし
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

(define-key scala-mode-map (kbd "C-x C-j") 'open-by-intellij)
(define-key scala-mode-map (kbd "M-t") 'ensime-type-at-point-full-name)
(define-key scala-mode-map (kbd ",") (smartchr '("," " => " " -> ")))
(define-key scala-mode-map (kbd "<") (smartchr '("<" " <- ")))
(define-key scala-mode-map (kbd "C-c C-c C-u") 'scala/popup-on-last-import)

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
  (let* ((package-name (scala/find-package-name-current-buffer))
         (spec-name (scala/find-spec-name-current-buffer))
         (spec-name-with-package
          (if (string= package-name "")
              spec-name
            (format "%s.%s" package-name spec-name))))
    (sbt-command (format "testOnly %s" spec-name-with-package))))

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

(defun shibayu36/scala-mode-hook ()
  (setq scala-indent:use-javadoc-style t))
(add-hook 'scala-mode-hook 'shibayu36/scala-mode-hook)

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
