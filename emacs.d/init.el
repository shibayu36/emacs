;;; emacs -q -lした時に、user-emacs-directoryが変わるように
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;ロードパス追加設定;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (locate-user-emacs-file "elisp/"))
(add-to-list 'load-path (locate-user-emacs-file "elisp/el-get/el-get/"))
(add-to-list 'load-path (locate-user-emacs-file "elisp/mode/"))
(add-to-list 'load-path (locate-user-emacs-file "elisp/mode/jshint-mode/"))
(add-to-list 'load-path (expand-file-name "~/Dropbox/config-file/.emacs.d/elisp/"))

(add-to-list 'custom-theme-load-path (locate-user-emacs-file "elisp/themes/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; ELPA 設定 ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(setq package-user-dir (locate-user-emacs-file "elisp/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   init-loader   ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;設定ファイルはinits以下に置いていて、init-loaderによって読み込まれる
(require 'init-loader)
(init-loader-load (locate-user-emacs-file "inits"))
(init-loader-load "~/Dropbox/config-file/.emacs.d/inits")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "f07583bdbcca020adecb151868c33820dfe3ad5076ca96f6d51b1da3f0db7105" "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" "2c73700ef9c2c3aacaf4b65a7751b8627b95a1fd8cebed8aa199f2afb089a85f" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (flow-minor-mode ensime lua-mode gotest ruby-block rsense rfringe redo+ point-undo plenv mmm-mode migemo log4e lispxmp less-css-mode json-mode jedi jaunte init-loader htmlize highlight-symbol helm-perldoc helm-open-github helm-etags-plus helm-c-yasnippet helm-c-moccur helm-ack guide-key grep-a-lot goto-last-change go-eldoc go-autocomplete gitignore-mode gitconfig-mode git-gutter-fringe gist free-keys foreign-regexp fold-dwim flymake-jslint elscreen eldoc-extension edit-server edit-list direx dash-at-point cperl-mode command-log-mode col-highlight coffee-mode codic auto-virtualenvwrapper auto-install auto-complete-c-headers auto-compile auto-async-byte-compile anzu anything-exuberant-ctags all-ext ack ac-octave ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:foreground "yellow" :weight bold))))
 '(cperl-hash-face ((t (:foreground "Red" :weight bold))))
 '(ensime-errline-highlight ((t (:inherit flycheck-error))))
 '(ensime-warnline-highlight ((t (:inherit flycheck-warning)))))
