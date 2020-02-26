(require 'direx)
(require 'direx-project)

(defun direx:jump-to-git-project-directory ()
  (interactive)
  (let* ((git-root-dir))
    (setq git-root-dir (vc-root-dir))
    (unless (string= git-root-dir "")
      (direx:find-directory-noselect git-root-dir))
    (direx:jump-to-directory-other-window)))

(defun direx:jump-to-project-directory ()
  (interactive)
  (let ((result (ignore-errors
                  (direx-project:jump-to-project-root-other-window)
                  t)))
    (unless result
      (direx:jump-to-directory-other-window))))
