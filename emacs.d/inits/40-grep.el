;; igrep.el
(require 'igrep)
(igrep-define lgrep (igrep-use-zgrep nil)(igrep-regex-option "-n -0u8"))
(igrep-find-define lgrep (igrep-use-zgrep nil)(igrep-regex-option "-n -0u8"))
(setq igrep-find-use-xargs nil)

;; grepから直接置換できるように
;; (require 'grep-edit)
;; (defadvice grep-edit-change-file (around inhibit-read-only activate)
;;   ""
;;   (let ((inhibit-read-only t))
;;     ad-do-it))
;; (defun my-grep-edit-setup ()
;;   (define-key grep-mode-map '[up] nil)
;;   (define-key grep-mode-map "\C-c\C-c" 'grep-edit-finish-edit)
;;   (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes."))
;;   (set (make-local-variable 'inhibit-read-only) t)
;;   )
;; (add-hook 'grep-setup-hook 'my-grep-edit-setup t)
(require 'wgrep)
(setq wgrep-enable-key "r")

(require 'grep-a-lot)
(grep-a-lot-advise igrep)
