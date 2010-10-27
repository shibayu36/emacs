;; -*- coding: utf-8; mode:emacs-lisp -*-
;; set-perl5lib-glob-from-git-root.el
;;
;; * 概要
;; Gitリポジトリのrootからのパスを指定してPERL5LIBに追加する
;; globも使える
;;
;; * こうすると大変重いので，
;; (add-hook 'cperl-mode-hook
;;           '(lambda ()
;;              (setq indent-tabs-mode nil)
;;              (set-perl5lib-glob-from-git-root "lib")
;;              (set-perl5lib-glob-from-git-root "t/lib")
;;              (set-perl5lib-glob-from-git-root "modules/*/lib")))
;;
;; * 明示的に実行するとよいと思います
;; (defun setup-perl5lib ()
;;   (interactive)
;;   (set-perl5lib-glob-from-git-root "lib")
;;   (set-perl5lib-glob-from-git-root "t/lib")
;;   (set-perl5lib-glob-from-git-root "modules/*/lib"))

(defun set-perl5lib-glob-from-git-root (rule)
  "GitのリポジトリのrootからのglobしてPERL5LIBに追加"
  (interactive "*Mglob from repository top: ")
  (if (= (shell-command "git status >& /dev/null") 0)
    (let
        ((paths (split-string (replace-regexp-in-string "\n+$" "" (shell-command-to-string (concat "echo $(git rev-parse --show-cdup)" rule))) " "))
         (pushed-paths ()))
      (loop for path in (split-string (replace-regexp-in-string "\n+$" "" (shell-command-to-string (concat "echo $(git rev-parse --show-cdup)" rule))) " ")
            do
            (let* (
                   (current-perl5lib (getenv "PERL5LIB"))
                   (full-path (expand-file-name path)))
              (message "Checking %s" full-path)
              (when (and (file-exists-p full-path) (not (string-match full-path current-perl5lib)))
                (setenv "PERL5LIB" (concat full-path ":" current-perl5lib))
                (setq pushed-paths (append (list full-path) pushed-paths))
                )
              ))
      (message "Added %s into PERL5LIB" pushed-paths)
      )
    (message "fatal: Not a git repository")
    )
  )

(provide 'set-perl5lib-glob-from-git-root)
