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

  ;; imenu, gtags, perlのgtagsから読み込み
  ;; perlのやつは固まるので一旦排除
  (defun anything-gtags-select-all ()
    (interactive)
    (anything-other-buffer
     '(anything-c-source-imenu
       anything-c-source-gtags-select)
     "*anything gtags*"))

  (defun anything-gtags-from-here ()
    (interactive)
    (anything
     :sources '(anything-c-source-gtags-select)
     :input (thing-at-point 'symbol)))

  )
