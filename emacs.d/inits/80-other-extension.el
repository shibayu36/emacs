;;auto-install設定
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
;; (auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; info.elの読み込み
(require 'info)

;;womanの設定
(setq woman-use-own-frame nil)

;;moccur拡張
;; (require 'moccur-edit)
;; (require 'color-moccur)
;; (setq moccur-split-word t)
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
(cua-mode t)
(setq cua-enable-cua-keys nil)

;;; goto-last-change
(require 'goto-chg)
