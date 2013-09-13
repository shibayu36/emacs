;; anything-custom-filelist
(require 'anything-config)
(require 'anything-git-files)
(defun anything-custom-filelist ()
  (interactive)
  (let* ((git-source `())
         (sources))
    (cond ((anything-git-files:git-p)
           (setq git-source `(anything-git-files:modified-source
                               anything-git-files:untracked-source
                               anything-git-files:all-source
                               ))))
    (setq sources `(;; anything-c-source-ffap-line
                    ;; anything-c-source-ffap-guesser
                    anything-c-source-buffers+
                    ,@git-source
                    anything-c-source-recentf
                    anything-c-source-bookmarks
                    anything-c-source-file-cache
                    anything-c-source-filelist))
    (anything-other-buffer sources "*anything for files*")))
