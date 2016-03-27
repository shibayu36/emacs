(require 'magit)

;; diff色変更
(set-face-foreground 'magit-diff-added "#00FF00")
(set-face-background 'magit-diff-added "#000000")
(set-face-foreground 'magit-diff-added-highlight "#00FF00")
(set-face-background 'magit-diff-added-highlight "gray20")
(set-face-foreground 'magit-diff-removed "#FF0000")
(set-face-background 'magit-diff-removed "#000000")
(set-face-foreground 'magit-diff-removed-highlight "#FF0000")
(set-face-background 'magit-diff-removed-highlight "gray20")
(set-face-background 'magit-diff-lines-boundary "blue")

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

;; commit時のメッセージの改行をやめる
(setq git-commit-fill-column 10000)

;;; magit-statusしてもwindow構成かわらないようにする設定
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun my/magit-quit-session ()
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)

(defadvice git-commit-commit (after move-to-magit-buffer activate)
  (delete-window))

(require 'gitconfig-mode)
(require 'gitignore-mode)
