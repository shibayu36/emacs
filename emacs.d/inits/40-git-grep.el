(defun git-grep (grep-dir command-args)
  (interactive
   (let ((root default-directory))
     (list
      (read-file-name
       "Directory for git grep: " root root t)
      (read-shell-command
            "Run git-grep (like this): "
            (format "git --no-pager grep -I -n -i -e %s"
                    "")
            'git-grep-history))))
  (let ((grep-use-null-device nil)
        (command
         (format (concat
                  "cd %s && "
                  "%s")
                 grep-dir
                 command-args)))
    (grep command)))

;;; rootからgit grepする
(defun git-grep-from-root ()
  (interactive)
  (let* ((command
          (read-shell-command
           "Run git-grep (like this): "
            (format "git --no-pager grep -I -n -i -e %s"
                    "")
            'git-grep-history)))
    (git-grep (vc-root-dir) command)))
