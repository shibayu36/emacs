(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(setq mode-compile-always-save-buffer-p t)
(setq mode-compile-never-edit-command-p t)
(setq mode-compile-reading-time 0)

(setq compilation-window-height 18)
(when (>= emacs-major-version 23)

  (defun my-split-window-by-split-root (window)
    (if (or (compilation-buffer-p buffer)
            (equal name-of-buffer "*Shell Command Output*"))
        (split-root-window compilation-window-height)
      (split-window-sensibly window)))
  (setq split-window-preferred-function 'my-split-window-by-split-root))
