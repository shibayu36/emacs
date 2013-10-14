(require 'all-ext)

(eval-after-load "helm-c-moccur"
  '(progn
     (defun all-from-helm-moccur ()
       "Call `all' from `helm' content."
       (interactive)
       (helm-run-after-quit
        'all-from-anything-occur-internal "helm-moccur"
        helm-c-moccur-buffer helm-current-buffer))))
