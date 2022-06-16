(define-mode diff-mode "Diff" text-mode
   (syntax-set-lang 'diff)
)

(add-to-list 'file-match-mode '(".*\\.patch$" . diff-mode))
(add-to-list 'file-match-mode '(".*\\.diff$" . diff-mode))

(define diff-syntax-addition-match "(addition) @addition")
(define diff-syntax-deletion-match "(deletion) @deletion")
(define diff-syntax-context-match "(context) @context")
(define diff-syntax-location-match "(location) @location")

(define diff-syntax-addition-style '(:fg "green"))
(define diff-syntax-deletion-style '(:fg "red"))
(define diff-syntax-context-style '(:fg "cyan"))
(define diff-syntax-location-style '(:fg "blue"))

(syntax-set-style 'diff diff-syntax-addition-match diff-syntax-addition-style)
(syntax-set-style 'diff diff-syntax-deletion-match diff-syntax-deletion-style)
(syntax-set-style 'diff diff-syntax-context-match diff-syntax-context-style)
(syntax-set-style 'diff diff-syntax-location-match diff-syntax-location-style)
