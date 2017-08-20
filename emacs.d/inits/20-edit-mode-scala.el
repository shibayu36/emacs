;;; scala mode
(require 'scala-mode)
(require 'sbt-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; ensime settings ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ensime)
(setq ensime-startup-snapshot-notification nil)
(setq ensime-startup-notification nil)
(setq ensime-eldoc-hints nil)
(setq ensime-completion-style nil)
(setq ensime-typecheck-when-idle nil) ;; 定期的にtypecheckするのをやめる
(setq ensime-sem-high-enabled-p nil) ;; semantic highlightをしない

(defun ensime-restart ()
  "Restart the ensime server."
  (interactive)
  (ensime-shutdown)
  (sit-for 2)
  (ensime))

;;; 今のファイルのimportが書かれている部分にpopupすうｒ
(defun scala/popup-on-last-import ()
  (interactive)
  (popwin:popup-buffer (current-buffer) :height 0.4)
  (re-search-backward "^import " nil t))

(custom-set-faces
 '(ensime-errline-highlight ((t (:inherit flycheck-error))))
 '(ensime-warnline-highlight ((t (:inherit flycheck-warning)))))

(define-key scala-mode-map (kbd "C-x C-j") 'open-by-intellij)
(define-key scala-mode-map (kbd "C-@") 'ensime-edit-definition)
(define-key scala-mode-map (kbd "M-@") 'ensime-pop-find-definition-stack)
(define-key scala-mode-map (kbd "M-t") 'ensime-type-at-point-full-name)
(define-key scala-mode-map (kbd ",") (smartchr '("," " => ")))
(define-key scala-mode-map (kbd "C-c C-c C-u") 'scala/popup-on-last-import)

;;; ensimeのタグジャンプを使うようにする
(defun scala/use-ensime-definition-jump ()
  (interactive)
  (define-key scala-mode-map (kbd "C-@") 'ensime-edit-definition))

;;; ctagsのタグジャンプを使うようにする
(defun scala/use-ctags-definition-jump ()
  (interactive)
  (define-key scala-mode-map (kbd "C-@") nil))

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
