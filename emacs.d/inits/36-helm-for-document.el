(require 'helm-elisp)
(require 'helm-man)

(setq helm-for-document-sources
      (list
       helm-source-man-pages))

(setq helm-for-document-sources
      (nconc
       (mapcar (lambda (func)
                 (funcall func))
               '(helm-def-source--emacs-commands
                 helm-def-source--emacs-functions
                 helm-def-source--emacs-variables
                 helm-def-source--emacs-faces
                 helm-def-source--helm-attributes))
       helm-for-document-sources))

(defun helm-for-document ()
  "Preconfigured `helm' for helm-for-document."
  (interactive)
  (helm
   :sources helm-for-document-sources
   :input (thing-at-point 'symbol)
   :resume nil
   :preselect nil
   :buffer "*helm for document*"
   :candidate-number-limit 20))
