(require 'vc-git)
(provide 'my-git)

(defun my-git-toplevel ()
  (vc-git-root (file-truename (if (buffer-file-name)
                                  (file-name-directory (buffer-file-name))
                                default-directory))))

;; return git toplevel of root project
(defun my-git-root ()
  (flet ((iter (path)
               (let ((p (and path (vc-git-root path))))
                 (or (and p (iter (my-parent-directory p))) p))))
    (iter (file-truename (if (buffer-file-name)
                             (file-name-directory (buffer-file-name))
                           default-directory)))))

(defun my-parent-directory (path)
  (let ((parent (file-name-directory (directory-file-name (file-name-directory path)))))
    (if (string-equal parent path) nil parent)))

(defvar my-current-git-toplevel nil)
(make-variable-buffer-local 'my-current-git-toplevel)
(defvar my-current-git-root nil)
(make-variable-buffer-local 'my-current-git-root)
(defun my-current-git-toplevel ()
  (or my-current-git-toplevel
    (setq my-current-git-toplevel (my-git-toplevel))))
(defun my-current-git-root ()
  (or my-current-git-root
    (setq my-current-git-root (my-git-root))))