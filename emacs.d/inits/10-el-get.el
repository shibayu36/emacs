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
               :pkgname "emacsmirror/auto-highlight-symbol")
        (:name auto-programming
               :type github
               :pkgname "hitode909/emacs-auto-programming")
        (:name flycheck-scala-sbt
               :type github
               :pkgname "syohex/flycheck-scala-sbt")))

;; Packages to install from el-get
(defvar my/el-get-packages
  '(
    anything-git-files
    el-expectations
    perl-completion
    smartchr
    sequential-command
    sequential-command-config
    mode-compile
    direx-project
    abbrev-complete
    auto-highlight-symbol
    auto-programming
    with-eval-after-load-feature
    flycheck-scala-sbt
    )
  "A list of packages to install from el-get at launch.")

(el-get 'sync my/el-get-packages)
