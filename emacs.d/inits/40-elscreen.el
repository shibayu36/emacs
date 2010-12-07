(when (require 'elscreen nil t)
  (if window-system
      (define-key elscreen-map (kbd "C-j") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-j") 'suspend-emacs)))