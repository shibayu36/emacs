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

(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/elisp/themes/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; ELPA 設定 ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(setq package-user-dir "~/.emacs.d/elisp/elpa/")
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   init-loader   ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;設定ファイルはinits以下に置いていて、init-loaderによって読み込まれる
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")
(init-loader-load "~/Dropbox/config-file/.emacs.d/inits")
