(defvar acp-paren-alist
  '((?{ . ?})
    (?[ . ?])
    (?( . ?))
    (?\" . ?\")
    (?\' . ?\'))
  "*括弧の対の alist。")

(defvar acp-insertion-functions
  '((mark-active . acp-surround-with-paren)
    (t . acp-insert-paren))
  "*括弧を挿入するときの関数一覧。
各要素は以下:
\(CONDITION . FUNCTION)
CONDITION を評価した結果、最初に真となる要素の FUNCTION が実行される。
真となる要素が存在しない場合 `self-insert-command' が実行される。")


(define-minor-mode acp-mode
  "自動で括弧を閉じるモード。"
  nil "ACP" (make-sparse-keymap)
  (when acp-mode
    (acp-setup-keymap)))
      
(easy-mmode-define-global-mode
 acp-global-mode acp-mode acp-mode-turn-on)

(defun acp-mode-turn-on ()
  (interactive)
  (acp-mode 1))

(defun acp-mode-turn-off ()
  (interactive)
  (acp-mode -1))

(defun acp-setup-keymap ()
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (x)
            (define-key map (char-to-string (car x)) 'acp-self-insert-command))
          acp-paren-alist)
    (setcdr (assq 'acp-mode minor-mode-map-alist) map)))

(defun acp-self-insert-command (n)
  (interactive "p")
  (setq n (or n 1))
  (let ((pair (assoc last-input-char acp-paren-alist)))
    (catch 'break
      (mapc (lambda (x)
              (when (eval (car x))
                (funcall (cdr x) n)
                (throw 'break t)))
            (append acp-insertion-functions
                    '((t . self-insert-command)))))))

(defun acp-current-pair ()
  (assoc last-input-char acp-paren-alist))

(defun acp-insert-paren (n)
  (insert-char (car (acp-current-pair)) n)
  (insert-char (cdr (acp-current-pair)) n)
  (backward-char n))

(defun acp-surround-with-paren (n)
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (insert-char (car (acp-current-pair)) n)
      (goto-char (point-max))
      (insert-char (cdr (acp-current-pair)) n))))

(provide 'acp)
;;; acp.el ends here
