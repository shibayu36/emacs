(when (require 'anything nil t)
  (require 'anything-gtags)

  (defmacro my-let-env (environments &rest body)
    `(let ((process-environment process-environment))
       ,@(mapcar (lambda (env) `(setenv ,@env)) environments)
       (progn ,@body)))

  ;; gtags-select-with-root
  (defun anything-c-source-gtags-select-with-root (name gtagsroot)
    (lexical-let ((gtagsroot (expand-file-name gtagsroot)))
      `((name . ,name)
        (init
         . ,(lambda ()
              (my-let-env
               (("GTAGSROOT" gtagsroot))
               (call-process-shell-command
                "global -c" nil (anything-candidate-buffer 'global)))))
        (candidates-in-buffer)
        (action
         ("Goto the location"
          . ,(lambda (candidate)
               (my-let-env
                (("GTAGSROOT" gtagsroot))
                (gtags-push-context)
                (gtags-goto-tag candidate ""))))
         ("Goto the location (other-window)"
          . ,(lambda (candidate)
               (my-let-env
                (("GTAGSROOT" gtagsroot))
                (gtags-push-context)
                (gtags-goto-tag candidate "" t))))
         ("Move to the referenced point"
          . ,(lambda (candidate)
               (my-let-env
                (("GTAGSROOT" gtagsroot))
                (gtags-push-context)
                (gtags-goto-tag candidate "r"))))))))


  ;;anything-gtagsのソース設定
  (defvar anything-c-source-gtags-select-with-home-perl-lib
    (anything-c-source-gtags-select-with-root "GTAGS ~/perl5" "~/perl5"))

  (defun anything-gtags-select-all ()
    (interactive)
    (anything-other-buffer
     '(anything-c-source-gtags-select
       anything-c-source-gtags-select-with-home-perl-lib)
     "*anything gtags*"))

  (require 'anything-git-grep)
  (setq anything-git-grep-all-sources
        (list anything-c-source-git-grep
              anything-c-source-git-submodule-grep))
  (defun anything-git-grep-all ()
    "git grep project and submodule."
    (interactive)
    (anything anything-git-grep-all-sources (thing-at-point 'symbol) nil nil nil "*anything git grep all*"))
  )