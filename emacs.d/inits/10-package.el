;; Packages to install from ELPA and MELPA
(defvar my/packages
  '(
    anything
    auto-complete
    auto-install
    color-theme
    color-moccur
    company
    elscreen
    expand-region
    git-gutter
    helm
    helm-c-moccur
    helm-open-github
    init-loader
    jaunte
    key-chord
    magit
    org
    open-junk-file
    point-undo
    popwin
    quickrun
    redo+
    undo-tree
    undohist
    yasnippet
    )
  "A list of packages to install from MELPA at launch.")

;; Install Melpa packages
(dolist (package my/packages)
  (when (or (not (package-installed-p package)))
    (package-install package)))
