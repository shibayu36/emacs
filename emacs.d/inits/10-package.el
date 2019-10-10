(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/"))
      package-archive-priorities '(("melpa" . 1)))


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

    helm-perldoc

    gitconfig-mode
    gitignore-mode

    smartparens

    helm-c-moccur

    crontab-mode

    codic

    noflet

    typescript-mode
    company
    tide
    cperl-mode

    web-mode

    graphviz-dot-mode

    meghanada

    python-mode
    jedi
    virtualenvwrapper
    auto-virtualenvwrapper

    avy

    anything-exuberant-ctags

    helm-c-yasnippet

    flycheck-gometalinter
    gotest
    go-rename

    use-package

    lua-mode

    lsp-mode

    flow-minor-mode

    graphql-mode
    )
  "A list of packages to install from MELPA at launch.")

;; Install Melpa packages
(dolist (package my/packages)
  (when (or (not (package-installed-p package)))
    (package-install package)))

(use-package ensime
  :ensure t
  :pin melpa-stable)
