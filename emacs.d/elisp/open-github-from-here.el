(defun open-github-from-here:chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

(defun open-github-from-here:git-project-p ()
  (string=
   (open-github-from-here:chomp
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))

(defun open-github-from-here ()
  (interactive)
  (cond ((and (open-github-from-here:git-project-p) (use-region-p))
         (shell-command
          (format "open-github-from-file %s %d %d"
                  (file-name-nondirectory (buffer-file-name))
                  (line-number-at-pos (region-beginning))
                  (line-number-at-pos (region-end)))))
        ((open-github-from-here:git-project-p)
         (shell-command
          (format "open-github-from-file %s %d"
                  (file-name-nondirectory (buffer-file-name))
                  (line-number-at-pos))))))

(provide 'open-github-from-here)
