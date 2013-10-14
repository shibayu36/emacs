(require 'git-gutter+)
(global-git-gutter+-mode t)

(setq git-gutter+-modified-sign " ")
(setq git-gutter+-added-sign "+")
(setq git-gutter+-deleted-sign "-")

(set-face-background 'git-gutter+-modified "purple")
(set-face-foreground 'git-gutter+-added "green")
(set-face-foreground 'git-gutter+-deleted "red")
