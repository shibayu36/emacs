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
        (:name set-perl5lib
               :type http
               :url "https://gist.github.com/syohex/1333926/raw/cabc5569d82971dc9fedf3198c4ae1dd858381c3/set-perl5lib.el")
        (:name set-perl5lib-glob-from-git-root
               :type http
               :url "https://gist.github.com/hitode909/617915/raw/a1cd2f87282830c34ca98afddda0c1376f00c2cf/set-perl5lib-glob-from-git-root.el")
        (:name key-combo
               :type github
               :pkgname "uk-ar/key-combo")
        (:name auto-highlight-symbol
               :type github
               :pkgname "emacsmirror/auto-highlight-symbol")
        ))

;; Packages to install from el-get
(defvar my/el-get-packages
  '(
    smartchr
    set-perl5lib
    set-perl5lib-glob-from-git-root
    )
  "A list of packages to install from el-get at launch.")

(el-get 'sync my/el-get-packages)
