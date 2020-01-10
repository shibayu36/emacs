;;; フルスクリーン
(defun my-mac-toggle-max-window ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))


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
    (if (>= (window-body-width) 210)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))

(defun split-window-horizontally-3 ()
  (interactive)
  (split-window-horizontally-n 3))

;; macro
(defun kmacro-save (symbol)
  (interactive "SName for last kbd macro: ")
  (name-last-kbd-macro symbol)
  (with-current-buffer (find-file-noselect kmacro-save-file)
    (goto-char (point-max))
    (insert-kbd-macro symbol)
    (basic-save-buffer)))

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

;;; スクロールのみする
(defun scroll-up-in-place (n)
  (interactive "p")
  (scroll-down n))

(defun scroll-down-in-place (n)
  (interactive "p")
  (scroll-up n))

;;; finderでdirectoryを開く
(defun open-current-dir-with-finder ()
  (interactive)
  (shell-command (concat "open .")))

;;; browserでファイルを開く
(defun browse-current-file ()
  (interactive)
  (let ((data-url
         (concat "file://" (buffer-file-name)))
        (default-browser
          (replace-regexp-in-string
           "[\n\r]+$" ""
           (shell-command-to-string (expand-file-name "~/bin/default-browser")))))
    (shell-command (concat "open -b " default-browser " " data-url))))

;;; 現在のファイルをIntelliJで開く
(defun open-by-intellij ()
  (interactive)
  (shell-command
   (format "/Applications/IntelliJ\\ IDEA\\ CE.app/Contents/MacOS/idea --line %d %s >/dev/null 2>&1"
           (line-number-at-pos)
           (buffer-file-name)))
  (shell-command "open -a /Applications/IntelliJ\\ IDEA\\ CE.app"))

;;; 現在のファイルをvscodeで開く
(defun open-by-vscode ()
  (interactive)
  (shell-command
   (format "code -r -g %s:%d:%d"
           (buffer-file-name)
           (line-number-at-pos)
           (current-column))))

;;; debug用
(defmacro d (expr)
 `(let ((_var (eval ',expr)))
    (run-at-time 0 nil 'display-buffer "*Messages*")
    (message "%S=%S" ',expr _var)
    _var))

;; 日付挿入
(defun insert-current-time ()
  (interactive)
  (insert (format-time-string "%a %b %d %H:%M:%S %Y" (current-time))))

;;; Open the directory of current buffer on iTerm
(defun open-default-directory-on-iterm ()
  (interactive)
  (execute-on-iterm (format "cd %s" default-directory)))

(defun copy-buffer-absolute-path ()
  (interactive)
  (kill-new (buffer-file-name (current-buffer))))

(defun replace-strings-in-region-by-list ($list)
  "Replace strings in a region according to $list"
  (if mark-active
      (let* (($beg (region-beginning))
             ($end (region-end))
             ($word (buffer-substring-no-properties $beg $end)))
        (mapc (lambda ($r)
                (setq $word (replace-regexp-in-string (car $r) (cdr $r) $word)))
              $list)
        (delete-region $beg $end)
        (insert $word))
    (error "Need to make region")))

;;; 全角数字を半角数字に
(defun convert-to-single-byte-number-region ()
  "Convert multi-byte number in region into single-byte number"
  (interactive)
  (replace-strings-in-region-by-list
   '(("１" . "1")
     ("２" . "2")
     ("３" . "3")
     ("４" . "4")
     ("５" . "5")
     ("６" . "6")
     ("７" . "7")
     ("８" . "8")
     ("９" . "9")
     ("０" . "0"))))
