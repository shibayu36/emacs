(eval-when-compile (require 'cl))
(require 'vc-git)
(provide 'anything-git-grep)

(defvar anything-c-source-git-grep-cache nil "path")

(defun anything-git-grep-init ()
  (setq anything-c-source-git-grep-cache
        (vc-git-root (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))))

(defun anything-git-grep-process ()
  (if anything-c-source-git-grep-cache
      (let ((default-directory anything-c-source-git-grep-cache))
        (apply 'start-process "git-grep-process" nil
               "git" "--no-pager" "grep" "--full-name" "-n" "--no-color"
               (nbutlast
                (apply 'append
                       (mapcar
                        (lambda (x) (list "-e" x "--and"))
                        (split-string anything-pattern "[ \t]" t))))))
    '()))

(defun anything-git-submodule-grep-process ()
  (if anything-c-source-git-grep-cache
      (let ((default-directory anything-c-source-git-grep-cache))
        (start-process-shell-command
         "git-submodule-grep-process" nil
         "git" "--no-pager" "submodule" "foreach"
         (format "'git grep --full-name -n --no-color %s | sed s!^!$path/!'"
                 (mapconcat (lambda (x)
                              (format "-e %s " (shell-quote-argument x)))
                            (split-string anything-pattern "[ \t]" t)
                            "--and "))
         " | grep -v '^Entering '"))
    '()))

(defun anything-git-grep-transformer (cds source)
  (mapcar (lambda (candidate)
            (let ((list (split-string candidate ":")))
              (if (not (>= (length list) 3))
                  candidate
                (let ((file-name (first list))
                      (line-number (second list))
                      (line (apply 'concat (cddr list))))
                  (cons (format "%s:%s:\n  %s" file-name line-number line)
                        candidate)))))
          cds))

(defun anything-git-grep-goto (candidate)
  (let ((list (split-string candidate ":")))
    (when (>= (length list) 3)
      (let ((top-dir anything-c-source-git-grep-cache)
            (file-name (first list))
            (line-number (second list)))
        (find-file (file-truename (expand-file-name file-name  top-dir)) top-dir)
        (goto-line (string-to-number line-number))))))

(defvar anything-c-source-git-grep
  '((name . "Git Grep")
    (multiline)
    (init . anything-git-grep-init)
    (candidates . anything-git-grep-process)
    (filtered-candidate-transformer anything-git-grep-transformer)
    (action . (("Git Grep Goto " . anything-git-grep-goto)))
    (candidate-number-limit . 300)
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defvar anything-c-source-git-submodule-grep
  '((name . "Git Submodule Grep")
    (multiline)
    (init . anything-git-grep-init)
    (candidates . anything-git-submodule-grep-process)
    (filtered-candidate-transformer anything-git-grep-transformer)
    (action . (("Git Submodule Grep Goto " . anything-git-grep-goto)))
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defun anything-git-grep ()
  "Anything Git Grep"
  (interactive)
  (anything-other-buffer 'anything-c-source-git-grep "*anything git grep*"))

(defun anything-git-submodule-grep ()
  "Anything Git Submodule Grep"
  (interactive)
  (anything-other-buffer 'anything-c-source-git-submodule-grep "*anything git submodule grep*"))
