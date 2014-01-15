;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; exec-pathの設定 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://sakito.jp/emacs/emacsshell.html#path
;; より下に記述した物が PATH の先頭に追加されます

(require 'exec-path-from-shell)
(let ((envs '("PATH" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

(setenv "NODE_PATH"
        (concat "~/node_modules/:"
                (getenv "NODE_PATH")))
