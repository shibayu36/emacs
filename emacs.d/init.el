;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;ロードパス追加設定;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/")
        (expand-file-name "~/.emacs.d/elisp/")
        (expand-file-name "~/.emacs.d/elisp/el-get/el-get/")

        (expand-file-name "~/.emacs.d/elisp/mode/")
        (expand-file-name "~/.emacs.d/elisp/mode/jshint-mode/")
        (expand-file-name "~/Dropbox/config-file/.emacs.d/elisp/")
        )
       load-path))

(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/elisp/themes/"))

;;; Cask
(require 'cask)
(cask-initialize)

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
