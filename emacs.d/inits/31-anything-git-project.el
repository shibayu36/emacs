(when (require 'anything nil t)

  (defun anything-c-sources-git-project-for (pwd)
    (loop for elt in
          '(("Modified files (%s)" . "--modified")
            ("Untracked files (%s)" . "--others --exclude-standard")
            ("All controlled files in this project (%s)" . ""))
          collect
          `((name . ,(format (car elt) pwd))
            (init . (lambda ()
                      (unless (and ,(string= (cdr elt) "") ;update candidate buffer every time except for that of all project files
                                   (anything-candidate-buffer))
                        (with-current-buffer
                            (anything-candidate-buffer 'global)
                          (insert
                           (shell-command-to-string
                            ,(format "git ls-files $(git rev-parse --show-cdup) %s"
                                     (cdr elt))))))))
            (candidates-in-buffer)
            (type . file))))

  (defun anything-git-project ()
    (interactive)
    (let* ((pwd (shell-command-to-string "echo -n `pwd`"))
           (sources (anything-c-sources-git-project-for pwd)))
      (anything-other-buffer sources
                             (format "*Anything git project in %s*" pwd))))

)