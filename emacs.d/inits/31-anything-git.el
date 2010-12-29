(when (require 'anything nil t)
  (require 'anything-git-grep)
  (setq anything-git-grep-all-sources
        (list anything-c-source-git-grep
              anything-c-source-git-submodule-grep))
  (defun anything-git-grep-all ()
    "git grep project and submodule."
    (interactive)
    (anything anything-git-grep-all-sources (thing-at-point 'symbol) nil nil nil "*anything git grep all*"))
  )