(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-hook
 'octave-mode-hook
 '(lambda ()
    (progn
      (local-set-key (kbd "C-h") 'delete-backward-char))))
