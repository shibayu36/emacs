(require 'helm-etags-plus)
(setq helm-etags-plus-use-absolute-path nil)
(set-face-foreground 'helm-etags-plus-file-face "green")

;;; select with region or with thing-at-point
(defun helm-etags-plus-select-region ()
  (interactive)
  (cond ((use-region-p)
         (helm-etags-plus-select-internal
          (format "\\_<%s\\_>"
                  (buffer-substring (region-beginning) (region-end)))))
        (t
         (helm-etags-plus-select))))
