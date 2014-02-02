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
    git-gutter-fringe
    grep-a-lot
    helm
    helm-open-github
    helm-descbinds
    htmlize
    init-loader
    jaunte
    key-chord
    magit
    org
    open-junk-file
    paredit
    point-undo
    popwin
    quickrun
    redo+
    undo-tree
    undohist
    yasnippet
    zlc
    wgrep
    bm
    dash
    dash-at-point
    direx

    lispxmp
    edit-list
    eldoc-extension
    ruby-block
    ruby-end
    rsense
    mmm-mode
    zencoding-mode
    php-mode
    less-css-mode
    rfringe
    yaml-mode
    nginx-mode
    foreign-regexp
    popup
    fold-dwim
    recentf-ext
;;    slime
    sudo-ext
    col-highlight
    gist
    usage-memo

    auto-async-byte-compile
    deferred
    concurrent
    migemo
    viewer
    perlbrew
    plenv
    multiple-cursors
    smartrep
    guide-key

    pcache
    logito
    gh

    edit-server

    anzu
    all
    all-ext
    evil

    highlight-symbol

    go-mode
    go-autocomplete
    go-eldoc

    markdown-mode

    flycheck

    flymake-jslint
    free-keys

    helm-hatena-bookmark

    ac-ispell

    rbenv

    smart-mode-line
    )
  "A list of packages to install from MELPA at launch.")

;; Install Melpa packages
(dolist (package my/packages)
  (when (or (not (package-installed-p package)))
    (package-install package)))
