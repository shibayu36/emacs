
;;再帰的にバイトコンパイルを行う
(defun my-byte-compile-directory ()
  (interactive)
  (defun byte-compile-directories (dir)
    (if (file-directory-p dir)
        (byte-compile-directory-r (mapcar (function (lambda (f) (concat dir "/" f)))
                                          (directory-files dir)))))
  (defun byte-compile-directory-r (file-list)
    (cond ((null (car file-list))
           nil)
          ((and (file-directory-p (car file-list))
                (not (string-match "/\.\.?$" (car file-list))))
           (byte-compile-directories (car file-list))
           (if (not (null (cdr file-list)))
               (progn
                 (byte-compile-directories (cadr file-list))
                 (byte-compile-directory-r (cdr file-list)))))
          ((string-match "\.el$" (car file-list))
           (progn
             (byte-compile-file (car file-list))
             (byte-compile-directory-r (cdr file-list))))
          (t
           (if (not (null (cdr file-list)))
               (byte-compile-directory-r (cdr file-list))))))
  (byte-compile-directories (replace-regexp-in-string "/$" "" default-directory)))



;;perlインデント整形ツール
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))

(defun my-mac-toggle-max-window ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))


;;perlのsyntaxチェック
(defun perl-syntax-check()
  (interactive)
  (shell-command
   (concat "perl -wc " (file-name-nondirectory (buffer-file-name)))))


;; gitルートからPERL5LIBにPATH通す
(defun setup-perl5lib ()
   (interactive)
  (set-perl5lib-glob-from-git-root "lib")
  (set-perl5lib-glob-from-git-root "t/lib")
  (set-perl5lib-glob-from-git-root "modules/*/lib"))


;;; delete-trailing-whitespaceのbefore-hook設定をトグルする
(require 'cl)
(defun toggle-delete-trailing-whitespace-setting ()
  (interactive)
  (cond ((find 'delete-trailing-whitespace before-save-hook)
         (remove-hook 'before-save-hook 'delete-trailing-whitespace))
        (
         (add-hook 'before-save-hook 'delete-trailing-whitespace)))
  )


(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))


;; macro
(defun kmacro-save (symbol)
  (interactive "SName for last kbd macro: ")
  (name-last-kbd-macro symbol)
  (with-current-buffer (find-file-noselect kmacro-save-file)
    (goto-char (point-max))
    (insert-kbd-macro symbol)
    (basic-save-buffer)))

;; テスト実行用
(defun run-perl-method-test ()
  (interactive)
  (let ((command compile-command))
    (save-excursion
      (when (or
             (re-search-backward "\\bsub\s+\\([_[:alnum:]]+\\)\s*:\s*Test" nil t)
             (re-search-forward "\\bsub\s+\\([_[:alnum:]]+\\)\s*:\s*Test" nil t))
        (setq command
              (format "TEST_METHOD=%s %s -w %s"
                      (match-string 1) (perlbrew-mini-get-current-perl-path) (expand-file-name buffer-file-name)))))
    (when command (compile command))))


;;; htmlize
(defun htmlize-and-browse ()
  (interactive)
  (defcustom
    htmlize-and-browse-directory-path temporary-file-directory
    "htmlize-and-browse-temporary-file-directory"
    :type 'string
    :group 'htmlize-and-browse)
  (setq htmlize-and-browse-buffer-file-name (concat "htmlize-and-browse-" (format-time-string "%Y%m%d%H%M%S" (current-time)) ".html"))
  (setq htmlize-and-browse-buffer-file-path (concat htmlize-and-browse-directory-path htmlize-and-browse-buffer-file-name))
  (with-current-buffer (htmlize-buffer)
    (write-file htmlize-and-browse-buffer-file-path)
    (set-buffer-modified-p nil)
    (kill-buffer htmlize-and-browse-buffer-file-name)
    (shell-command (concat "open " htmlize-and-browse-buffer-file-path))
  ))

(defun htmlize-and-browse-by-safari ()
  (interactive)
  (defcustom
    htmlize-and-browse-directory-path temporary-file-directory
    "htmlize-and-browse-temporary-file-directory"
    :type 'string
    :group 'htmlize-and-browse)
  (setq htmlize-and-browse-buffer-file-name (concat "htmlize-and-browse-" (format-time-string "%Y%m%d%H%M%S" (current-time)) ".html"))
  (setq htmlize-and-browse-buffer-file-path (concat htmlize-and-browse-directory-path htmlize-and-browse-buffer-file-name))
  (with-current-buffer (htmlize-buffer)
    (write-file htmlize-and-browse-buffer-file-path)
    (set-buffer-modified-p nil)
    (kill-buffer htmlize-and-browse-buffer-file-name)
    (shell-command (concat "open -a safari " htmlize-and-browse-buffer-file-path))
  ))

;;; perlのuse sort
(defun sort-perl-use (beg end)
  (interactive (list (region-beginning) (region-end)))
  (sort-regexp-fields nil "^.+$" "[^;]+" beg end))

;;; 画面分割
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
