;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup ELPA
(require 'package)

(setq package-user-dir "~/.emacs.d/elisp/elpa/")
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

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
  '(git-gutter
    helm
    helm-open-github
    )
  "A list of packages to install from MELPA at launch.")

;; Install Melpa packages
(install-my-melpa-packages)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup el-get
(setq el-get-dir "~/.emacs.d/elisp/el-get/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;; define el-get repository
(setq el-get-sources
      '(
        (:name open-github-from-here
               :type github
               :description "open github from here"
               :pkgname "shibayu36/emacs-open-github-from-here"
               :branch "development")
        (:name anything-git-files
               :type github
               :pkgname "tarao/anything-git-files-el")
        ))

;; Packages to install from el-get
(defvar my-el-get-packages
  '(
    open-github-from-here
    anything-git-files
    )
  "A list of packages to install from el-get at launch.")

(el-get 'sync my-el-get-packages)
