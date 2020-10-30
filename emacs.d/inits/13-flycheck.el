(require 'flycheck)
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

(flycheck-define-checker textlint
  "A linter for prose."
  :command ("textlint" "--format" "unix"
            source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))
(add-to-list 'flycheck-checkers 'textlint)
