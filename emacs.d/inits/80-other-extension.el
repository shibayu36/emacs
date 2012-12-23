;;install-elispコマンド設定
;; まず、install-elisp のコマンドを使える様にします。
(require 'install-elisp)
;; 次に、Elisp ファイルをインストールする場所を指定します。
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")

;;auto-install設定
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
;; (auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; info.elの読み込み
(require 'info)

;;womanの設定
(setq woman-use-own-frame nil)

;;カラーテーマ
(require 'color-theme)
(require 'zenburn)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-zenburn)))
(color-theme-initialize)
(color-theme-dark-laptop)

;;moccur拡張
(require 'moccur-edit)
(require 'color-moccur)
(setq moccur-split-word t)
;; migemoがrequireできる環境ならmigemoを使う
(when (require 'migemo nil t) ;第三引数がnon-nilだとloadできなかった場合にエラーではなくnilを返す
  (setq moccur-use-migemo t))

;; undohistの設定
;;; なんかぶっ壊れる
;; (when (require 'undohist nil t)
;;   (undohist-initialize))

;; undo-treeモードの設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; jaunte.el
(require 'jaunte)

;; set-perl5lib-glob-from-git-root
(require 'set-perl5lib-glob-from-git-root)

;; redo
(require 'redo+)
(setq undo-no-redo t)
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

;; sudo-ext
(require 'sudo-ext)

;; sequential-command
(require 'sequential-command-config)
(sequential-command-setup-keys)

;; point-undo
(require 'point-undo)

;; col-highlight
(require 'col-highlight)
(toggle-highlight-column-when-idle 1)
(col-highlight-set-interval 3)
(custom-set-faces
 '(col-highlight ((t (:background "dark slate gray")))))

;; gist
(require 'gist)

;; Helpバッファにメモできるように
(require 'usage-memo)
(setq umemo-base-directory "~/.emacs.d/umemo")
(umemo-initialize)

;; 使い捨てファイルを開けるように
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y-%m-%d-%H%M%S.")

;; diffの設定
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; ブロック折り畳み
(require 'hideshow)
(require 'fold-dwim)

;; htmlize
(require 'htmlize)

;;; clipboard-to-kill-ring
;; (require 'clipboard-to-kill-ring)
;; (clipboard-to-kill-ring t)

;;; zlc
(require 'zlc)

;;; 矩形選択
(require 'cua-mode)
(require 'cua)
(CUA-mode 'emacs)
