;;install-elispコマンド設定
;; まず、install-elisp のコマンドを使える様にします。
(require 'install-elisp)
;; 次に、Elisp ファイルをインストールする場所を指定します。
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")


;;auto-complete-modeの読み込み
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/ac-dict")
(ac-config-default)

;;auto-install設定
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
;; (auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; info.elの読み込み
(require 'info)

;;womanの設定
(setq woman-use-own-frame nil)

;;tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(setq recentf-auto-cleanup 'never)

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

;; undohistの設定
;; (when (require 'undohist nil t)
;;   (undohist-initialize))

;; undo-treeモードの設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; jaunte.el
(require 'jaunte)
(global-set-key (kbd "C-c C-j") 'jaunte)

;; set-perl5lib-glob-from-git-root
(require 'set-perl5lib-glob-from-git-root)

;; redo
(require 'redo+)
(global-set-key "\M-/" 'redo)
(setq undo-no-redo t)
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

;; sudo-ext
(server-start)
(require 'sudo-ext)

;; sequential-command
(require 'sequential-command-config)
(sequential-command-setup-keys)

;; point-undo
(require 'point-undo)
(define-key global-map (kbd "<f7>") 'point-undo)
(define-key global-map (kbd "S-<f7>") 'point-redo)

;; カーソル位置に目印つけるやつ
(setq-default bm-buffer-persistence nil)
(setq bm-restore-repository-on-load t)
(require 'bm)
(add-hook 'find-file-hooks 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'after-save-hook 'bm-buffer-save)
(add-hook 'after-revert-hook 'bm-buffer-restore)
(add-hook 'vc-before-checkin-hook 'bm-buffer-save)
(global-set-key (kbd "M-SPC") 'bm-toggle)
(global-set-key (kbd "M-[") 'bm-previous)
(global-set-key (kbd "M-]") 'bm-next)

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
(require 'clipboard-to-kill-ring)
(clipboard-to-kill-ring t)
