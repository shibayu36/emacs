(defun ack (grep-dir command-args)
  (interactive
   (let ((root default-directory))
     (list
      (read-file-name
       "Directory for ack: " root root t)
      (read-shell-command
            "Run ack (like this): "
            (format "ack -H --nocolor --nogroup %s"
                    "")
            'ack-history))))
  (let ((grep-use-null-device nil)
        (command
         (format (concat
                  "cd %s && "
                  "%s")
                 grep-dir
                 command-args)))
    (grep command)))
