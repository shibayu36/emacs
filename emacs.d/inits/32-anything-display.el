(require 'split-root)
(defun anything-display-function--split-root (buf)
  (let ((percent 50.0))
    (set-window-buffer (split-root-window  (truncate (* (frame-height) (/ percent 100.0)))) buf)))
(setq anything-display-function 'anything-display-function--split-root)