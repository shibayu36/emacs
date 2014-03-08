(require 'magit)

(set-face-background 'magit-item-highlight "#202020")
(set-face-foreground 'magit-diff-add "green")
(set-face-foreground 'magit-diff-del "red")
(set-face-foreground 'magit-diff-file-header "blue")

;; commit時のメッセージの改行をやめる
(setq git-commit-fill-column 10000)

;;; git commit mode
(setq git-commit-mode-hook nil) ;; auto-fillもflyspellも使わない

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
