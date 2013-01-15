;; anything-custom-filelist
(defun anything-custom-filelist ()
  (interactive)
  (anything-other-buffer
   (append
    '(anything-c-source-ffap-line
      anything-c-source-ffap-guesser
      anything-c-source-buffers+
      )
    (anything-c-sources-git-project-for)
    '(anything-c-source-recentf
      anything-c-source-bookmarks
      anything-c-source-file-cache
      anything-c-source-filelist
      ))
   "*anything file list*"))
