(define-mode org-mode "Org" text-mode
   (syntax-set-lang 'org)
)

(add-to-list 'file-match-mode '(".*\\.org" . org-mode))
