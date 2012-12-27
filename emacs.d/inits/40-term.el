;; ============================================================
;; ansi-term
;; ============================================================


;; (defadvice ansi-term (after ansi-term-after-advice (arg))
;;   "run hook as after advice"
;;   (run-hooks 'ansi-term-after-hook))
;; (ad-activate 'ansi-term)

;; (defun my-term-switch-line-char ()
;;   "Switch `term-in-line-mode' and `term-in-char-mode' in `ansi-term'"
;;   (interactive)
;;   (cond
;;    ((term-in-line-mode)
;;     (term-char-mode)
;;     (hl-line-mode -1))
;;    ((term-in-char-mode)
;;     (term-line-mode)
;;     (hl-line-mode 1))))

;; (defadvice anything-c-kill-ring-action (around my-anything-kill-ring-term-advice activate)
;;   "In term-mode, use `term-send-raw-string' instead of `insert-for-yank'"
;;   (if (eq major-mode 'term-mode)
;;       (letf (((symbol-function 'insert-for-yank) (symbol-function 'term-send-raw-string)))
;;         ad-do-it)
;;     ad-do-it))

;; (defvar ansi-term-after-hook nil)
;; (add-hook 'ansi-term-after-hook
;;           (lambda ()
;;             ;; shell-pop
;;             (define-key term-raw-map (kbd "<f2>") 'shell-pop)
;;             ;; これがないと M-x できなかったり
;;             (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
;;             ;; 貼り付け
;;             (define-key term-raw-map (kbd "C-y") 'term-paste)
;;             (define-key term-raw-map (kbd "M-y") 'anything-show-kill-ring)
;;             ;; C-t で line-mode と char-mode を切り替える
;;             (define-key term-raw-map (kbd "C-t") 'my-term-switch-line-char)
;;             (define-key term-mode-map (kbd "C-t") 'my-term-switch-line-char)
;;             ;; Tango!
;;             (setq ansi-term-color-vector
;;                   [unspecified
;;                    "#000000"           ; black
;;                    "#ff3c3c"           ; red
;;                    "#84dd27"           ; green
;;                    "#eab93d"           ; yellow
;;                    "#135ecc"           ; blue
;;                    "#f47006"           ; magenta
;;                    "#89b6e2"           ; cyan
;;                    "#ffffff"]          ; white
;;                   )
;;             (setq system-uses-terminfo nil)
;;             ))

;; ============================================================
;; shell-pop
;; ============================================================

;; (require 'shell-pop)

;; (shell-pop-set-internal-mode "ansi-term")
;; (shell-pop-set-internal-mode-shell shell-file-name)
;; (shell-pop-set-internal-mode-shell "/bin/zsh")
;; (shell-pop-set-window-height 90)
;; (setq shell-pop-window-position "bottom")

;; (global-set-key (kbd "<f2>") 'shell-pop)
