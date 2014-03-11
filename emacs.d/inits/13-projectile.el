(require 'projectile)
(setq projectile-project-root-files
      '(".projectile"        ; projectile project marker
        "Gemfile"            ; Bundler file
        "package.json"       ; npm package file
        "cpanfile"           ; CPAN dependencies for Perl applications
        ".git"               ; Git VCS root dir
        ))
