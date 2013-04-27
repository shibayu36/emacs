;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (git-gutter:toggle git-gutter:clear git-gutter
;;;;;;  git-gutter:previous-hunk git-gutter:next-hunk git-gutter:popup-hunk
;;;;;;  git-gutter:revert-hunk global-git-gutter-mode git-gutter-mode)
;;;;;;  "git-gutter/git-gutter" "../../../../../.emacs.d/elisp/el-get/git-gutter/git-gutter.el"
;;;;;;  "cfa6ac0f96a18aabb3fd61b558b70d24")
;;; Generated autoloads from ../../../../../.emacs.d/elisp/el-get/git-gutter/git-gutter.el

(autoload 'git-gutter-mode "git-gutter/git-gutter" "\
Git-Gutter mode

\(fn &optional ARG)" t nil)

(defvar global-git-gutter-mode nil "\
Non-nil if Global-Git-Gutter mode is enabled.
See the command `global-git-gutter-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-gutter-mode'.")

(custom-autoload 'global-git-gutter-mode "git-gutter/git-gutter" nil)

(autoload 'global-git-gutter-mode "git-gutter/git-gutter" "\
Toggle Git-Gutter mode in all buffers.
With prefix ARG, enable Global-Git-Gutter mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Git-Gutter mode is enabled in all buffers where
`(lambda nil (when (and (buffer-file-name) (not (memq major-mode git-gutter:disabled-modes))) (git-gutter-mode 1)))' would do it.
See `git-gutter-mode' for more information on Git-Gutter mode.

\(fn &optional ARG)" t nil)

(autoload 'git-gutter:revert-hunk "git-gutter/git-gutter" "\
Revert current hunk.

\(fn)" t nil)

(autoload 'git-gutter:popup-hunk "git-gutter/git-gutter" "\
popup current diff hunk

\(fn &optional DIFFINFO)" t nil)

(autoload 'git-gutter:next-hunk "git-gutter/git-gutter" "\
Move to next diff hunk

\(fn ARG)" t nil)

(autoload 'git-gutter:previous-hunk "git-gutter/git-gutter" "\
Move to previous diff hunk

\(fn ARG)" t nil)

(autoload 'git-gutter "git-gutter/git-gutter" "\
Show diff information in gutter

\(fn)" t nil)

(autoload 'git-gutter:clear "git-gutter/git-gutter" "\
clear diff information in gutter

\(fn)" t nil)

(autoload 'git-gutter:toggle "git-gutter/git-gutter" "\
toggle to show diff information

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elisp/el-get/git-gutter/git-gutter.el"
;;;;;;  "../../../../../.emacs.d/elisp/el-get/git-gutter/test-git-gutter.el"
;;;;;;  "../../../../../.emacs.d/elisp/el-get/open-github-from-here/open-github-from-here.el")
;;;;;;  (20859 27606 489952))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
