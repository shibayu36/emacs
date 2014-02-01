(defun git-blame-current-buffer () ;; shell-command を使うより、vc-annotete を利用した方がさらに過去にさかのぼれるので良いのでは？
  (interactive)
  (let ((result-buf "*git blame*")
        (popup-context (git-blame-current-line))
        (hoge-line (line-number-at-pos)))
    (shell-command (concat "git blame " (buffer-file-name (current-buffer))) result-buf)
    (view-buffer-other-window result-buf t
                              (lambda (buf)
                                (kill-buffer-and-window)))
  (switch-to-buffer result-buf)
  (goto-line hoge-line)
  (recenter)
  (popup-tip popup-context)))

(defalias 'g 'git-blame-current-buffer)

(defun git-blame-current-line ()
  (interactive)
  (let ((old-buf (current-buffer))
    (blame-buf (get-buffer-create "*blame*"))
    (line-num (number-to-string (line-number-at-pos))))
    (set-buffer blame-buf)
    (erase-buffer)
    (call-process "git-blame-oneline" nil "*blame*" t (buffer-file-name old-buf) line-num)
    (setq content (buffer-string))
    (set-buffer old-buf)
    (when (not (eq (length content) 0)) ;;この when に入らない場合は想定していないです
      content)))
