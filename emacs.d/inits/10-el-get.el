(setq el-get-dir "~/.emacs.d/elisp/el-get/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq el-get-sources
      '(
        (:name open-github-from-here
               :type github
               :description "open github from here"
               :url "https://github.com/shibayu36/emacs-open-github-from-here")
        ))

(el-get 'sync)

