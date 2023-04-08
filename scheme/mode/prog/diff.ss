(define-mode diff-mode "Diff" text-mode
   (syntax-set-lang 'diff))

(add-to-list 'file-match-mode '(".*\\.patch$" . diff-mode))
(add-to-list 'file-match-mode '(".*\\.diff$" . diff-mode))

(define diff-syntax-addition-match "(addition) @diff-addition")
(define diff-syntax-deletion-match "(deletion) @diff-deletion")
(define diff-syntax-context-match "(context) @diff-context")
(define diff-syntax-location-match "(location) @diff-location")

(define-style syntax-diff-addition '(fg: "green"))
(define-style syntax-diff-deletion '(fg: "red"))
(define-style syntax-diff-context  '(fg: "cyan"))
(define-style syntax-diff-location '(fg: "blue"))

(syntax-highlight 'diff
   (string-append
      diff-syntax-addition-match
      diff-syntax-deletion-match
      diff-syntax-context-match
      diff-syntax-location-match))
