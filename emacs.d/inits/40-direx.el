(require 'direx)
(require 'direx-project)
(push '(direx:direx-mode :position left :width 0.3 :dedicated t)
      popwin:special-display-config)

;;; gitのrootを一旦表示してから
;;; 表示
;; (defun direx:find-project-directory ()
;;   (interactive)
;;   (direx-project:jump-to-project-root-noselect)
;;   (direx:jump-to-directory-other-window))

(defun direx:find-git-project-directory ()
  (interactive)
  (let* ((is-inside-work-dir))
    (setq is-inside-work-dir
          (chomp
           (shell-command-to-string "git rev-parse --is-inside-work-tree")))
    (cond ((string= is-inside-work-dir "true")
           (direx:find-directory-noselect
            (chomp
             (shell-command-to-string "git rev-parse --show-toplevel")))))
    (direx:jump-to-directory-other-window)))
