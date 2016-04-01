(require 'flycheck)

(flycheck-define-checker julia-lint
  "A Julia syntax checker using Lint.jl."
  :command ("julia" "-e"
            "using Lint; lintfile(ARGS[1])" source-inplace)
  :error-patterns
  ((info line-start
            (zero-or-more blank)
            (file-name) " "
            (one-or-more (not digit))
            line " INFO" (one-or-more blank) (message) line-end)
   (warning line-start
            (zero-or-more blank)
            (file-name) " "
            (one-or-more (not digit))
            line " WARN" (one-or-more blank) (message) line-end)
   (error line-start
          (zero-or-more blank)
          (file-name) " "
          (one-or-more (not digit))
          line " " (or "ERROR" "FATAL") (one-or-more blank) (message) line-end))
  :modes julia-mode)

(add-to-list 'flycheck-checkers 'julia-lint)

(provide 'flycheck-julia)
