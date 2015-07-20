(setq el-get-dir (locate-user-emacs-file "elisp/el-get/"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;; define el-get repository
(setq el-get-sources
      '(
        (:name anything-git-files
               :type github
               :pkgname "tarao/anything-git-files-el")
        (:name direx-project
               :type http
               :url "https://raw.github.com/m2ym/direx-el/master/direx-project.el")
        (:name auto-highlight-symbol
               :type github
               :pkgname "emacsmirror/auto-highlight-symbol")))

;; Packages to install from el-get
(defvar my/el-get-packages
  '(
    anything-git-files
    el-expectations
    perl-completion
    anything-exuberant-ctags
    smartchr
    sequential-command
    sequential-command-config
    mode-compile
    direx-project
    abbrev-complete
    python-mode
    auto-highlight-symbol
    )
  "A list of packages to install from el-get at launch.")

(el-get 'sync my/el-get-packages)
