(require 'anything-exuberant-ctags)

(setq anything-for-tags
      (list anything-c-source-imenu
            anything-c-source-exuberant-ctags-select))

(defun anything-for-tags ()
  "Preconfigured `anything' for anything-for-tags."
  (interactive)
  (anything anything-for-tags
            (thing-at-point 'symbol)
            nil nil nil "*anything for tags*"))
