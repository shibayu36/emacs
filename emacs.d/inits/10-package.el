;; Packages to install from ELPA and MELPA
(defvar my/packages
  '(
    anything
    auto-complete
    color-moccur
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
    recentf-ext
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

    rbenv

    smart-mode-line

    helm-perldoc

    gitconfig-mode
    gitignore-mode

    smartparens

    helm-c-moccur

    crontab-mode

    scala-mode2

    codic

    ensime

    noflet
    )
  "A list of packages to install from MELPA at launch.")

;; Install Melpa packages
(dolist (package my/packages)
  (when (or (not (package-installed-p package)))
    (package-install package)))
