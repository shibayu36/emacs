(defun git-grep (grep-dir command-args)
  (interactive
   (let ((root (concat (git-root-directory) "/")))
     (list
      (read-file-name
       "Directory for git grep: " root root t)
      (read-shell-command
            "Run git-grep (like this): "
            (format "PAGER='' git grep -I -n -i -e %s"
                    "")
            'git-grep-history))))
  (let ((grep-use-null-device nil)
        (grep-command
         (format (concat
                  "cd %s && "
                  "%s")
                 grep-dir
                 command-args)))
    (grep grep-command)))