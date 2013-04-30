(setq el-get-dir "~/.emacs.d/elisp/el-get/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;; define el-get repository
(setq el-get-sources
      '(
        (:name open-github-from-here
               :type github
               :description "open github from here"
               :pkgname "shibayu36/emacs-open-github-from-here"
               :branch "development")
        (:name anything-git-files
               :type github
               :pkgname "tarao/anything-git-files-el")
        ))

;; Packages to install from el-get
(defvar my/el-get-packages
  '(
    open-github-from-here
    anything-git-files
    el-expectations
    perl-completion
    anything-exuberant-ctags
    key-chord
    space-chord
    smartchr
    sequential-command
    sequential-command-config
    mode-compile
    )
  "A list of packages to install from el-get at launch.")

(el-get 'sync my/el-get-packages)
