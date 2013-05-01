;;; dash-at-point-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (dash-at-point dash-at-point-mode-alist dash-at-point)
;;;;;;  "dash-at-point" "dash-at-point.el" (20862 28812))
;;; Generated autoloads from dash-at-point.el

(let ((loads (get 'dash-at-point 'custom-loads))) (if (member '"dash-at-point" loads) nil (put 'dash-at-point 'custom-loads (cons '"dash-at-point" loads))))

(defvar dash-at-point-mode-alist '((c++-mode . "cpp") (c-mode . "c") (coffee-mode . "coffee") (common-lisp-mode . "lisp") (cperl-mode . "perl") (css-mode . "css") (emacs-lisp-mode . "elisp") (erlang-mode . "erlang") (go-mode . "go") (haskell-mode . "haskell") (html-mode . "html") (java-mode . "java") (js2-mode . "javascript") (lua-mode . "lua") (objc-mode . "iphoneos") (perl-mode . "perl") (php-mode . "php") (python-mode . "python3") (ruby-mode . "ruby") (scala-mode . "scala") (vim-mode . "vim")) "\
Alist which maps major modes to Dash docset tags.
Each entry is of the form (MAJOR-MODE . DOCSET-TAG) where
MAJOR-MODE is a symbol and DOCSET-TAG is a corresponding tag
for one or more docsets in Dash.")

(custom-autoload 'dash-at-point-mode-alist "dash-at-point" t)

(defvar dash-at-point-docset nil "\
Variable used to specify the docset for the current buffer.
Users can set this to override the default guess made using
`dash-at-point-mode-alist', allowing the docset to be determined
programatically.

For example, Ruby on Rails programmers might add an \"allruby\"
tag to the Rails, Ruby and Rubygems docsets in Dash, and then add
code to `rinari-minor-mode-hook' or `ruby-on-rails-mode-hook'
which sets this variable to \"allruby\" so that Dash will search
the combined docset.")

(autoload 'dash-at-point "dash-at-point" "\
Search for the word at point in Dash.
If the optional prefix argument EDIT-SEARCH is specified,
the user will be prompted to edit the search string first.

\(fn &optional EDIT-SEARCH)" t nil)

;;;***

;;;### (autoloads nil nil ("dash-at-point-pkg.el") (20862 28812 495964))

;;;***

(provide 'dash-at-point-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dash-at-point-autoloads.el ends here
