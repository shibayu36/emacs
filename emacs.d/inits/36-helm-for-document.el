(require 'helm-elisp)

(defun helm-for-document ()
  "Preconfigured helm for documents"
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (helm :sources
          (mapcar (lambda (func)
                    (funcall func default))
                  '(helm-def-source--emacs-commands
                    helm-def-source--emacs-functions
                    helm-def-source--emacs-variables
                    helm-def-source--emacs-faces
                    helm-def-source--helm-attributes))
          :buffer "*helm for document*"
          :input default)))
