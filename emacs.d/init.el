;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;ロードパス追加設定;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/")
        (expand-file-name "~/.emacs.d/elisp/")
        (expand-file-name "~/.emacs.d/elisp/anything/")
        (expand-file-name "~/.emacs.d/elisp/abbrev/")
        (expand-file-name "~/.emacs.d/elisp/pymacs/")
        (expand-file-name "~/.emacs.d/elisp/yasnippet")
        (expand-file-name "~/.emacs.d/elisp/moccur")
        (expand-file-name "~/.emacs.d/elisp/apel")
        (expand-file-name "~/.emacs.d/elisp/mode/")
        (expand-file-name "~/.emacs.d/elisp/mode/html/")
        (expand-file-name "~/.emacs.d/elisp/mode/nxhtml/")
        (expand-file-name "~/.emacs.d/elisp/mode/css/")
        (expand-file-name "~/.emacs.d/elisp/mode/php-mode/")
        (expand-file-name "~/.emacs.d/elisp/mode/python-mode/")
        (expand-file-name "~/.emacs.d/elisp/mode/perl/")
        (expand-file-name "~/.emacs.d/elisp/mode/psgml/")
        (expand-file-name "~/.emacs.d/elisp/mode/yml/")
        (expand-file-name "~/.emacs.d/elisp/mode/javascript/")
        (expand-file-name "~/.emacs.d/elisp/mode/yatex/")
        (expand-file-name "~/.emacs.d/elisp/auto-complete/")
        (expand-file-name "~/.emacs.d/elisp/mode/hatena/")
        (expand-file-name "~/.emacs.d/elisp/mode/ruby/")
        (expand-file-name "~/.emacs.d/elisp/mode/magit/share/emacs/site-lisp/")
        (expand-file-name "~/.emacs.d/elisp/mode/git/")
        (expand-file-name "~/.emacs.d/elisp/mode/evernote/")
        (expand-file-name "~/.emacs.d/elisp/mode/mmm/")
        (expand-file-name "~/.emacs.d/elisp/mode/twittering-mode/")
        (expand-file-name "~/.emacs.d/elisp/mode/c-sharp/")
        (expand-file-name "~/.emacs.d/elisp/mode/jshint-mode/")
        (expand-file-name "~/.emacs.d/elisp/org/")
        (expand-file-name "~/.emacs.d/elisp/expand-region/")
        )
       load-path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   init-loader   ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;設定ファイルはinits以下に置いていて、init-loaderによって読み込まれる
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")
