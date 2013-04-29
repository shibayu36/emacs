(defun my-melpa-packages-installed-p ()
  (loop for p in my-melpa-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun install-my-melpa-packages ()
  (unless (my-melpa-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p my-melpa-packages)
      (unless (package-installed-p p)
        (package-install p)))))


;; Packages to install from MELPA
(defvar my-melpa-packages
  '(
    anything
    yasnippet
    git-gutter
    helm
    helm-open-github
    )
  "A list of packages to install from MELPA at launch.")

;; Install Melpa packages
(install-my-melpa-packages)
