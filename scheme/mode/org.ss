(define-mode org-mode "Org" text-mode
   (syntax-set-lang 'org)
)

(add-to-list 'file-match-mode '(".*\\.org" . org-mode))

(define org-syntax-head-line1-match
   "(headline (stars) @OrgStars1 (#match? @OrgStars1 \"^(\\\\*{3})*\\\\*$\") (item) @OrgHeadlineLevel1)"
)

(define org-syntax-head-line2-match
   "(headline (stars) @OrgStars2 (#match? @OrgStars2 \"^(\\\\*{3})*\\\\*\\\\*$\") (item) @OrgHeadlineLevel2)"
)

(define org-syntax-head-line3-match
   "(headline (stars) @OrgStars3 (#match? @OrgStars3 \"^(\\\\*{3})*\\\\*\\\\*\\\\*$\") (item) @OrgHeadlineLevel3)"
)
(define org-syntax-todo-match "(item . (expr) @OrgKeywordTodo (#eq? @OrgKeywordTodo \"TODO\"))")
(define org-syntax-done-match "(item . (expr) @OrgKeywordDone (#eq? @OrgKeywordDone \"DONE\"))")
(define org-syntax-list-bullet-match "(bullet) @OrgListBullet")

(define org-syntax-head-line1-style    '(:fg "blue"))
(define org-syntax-head-line2-style    '(:fg "magenta"))
(define org-syntax-head-line3-style    '(:fg "cyan"))
(define org-syntax-todo-style          '(:fg "red" :attr "bold"))
(define org-syntax-done-style          '(:fg "green" :attr "bold"))
(define org-syntax-list-bullet-style   '(:fg "bright-yellow" :attr "bold"))

(syntax-set-style 'org org-syntax-head-line1-match org-syntax-head-line1-style)
(syntax-set-style 'org org-syntax-head-line2-match org-syntax-head-line2-style)
(syntax-set-style 'org org-syntax-head-line3-match org-syntax-head-line3-style)
(syntax-set-style 'org org-syntax-todo-match org-syntax-todo-style)
(syntax-set-style 'org org-syntax-done-match org-syntax-done-style)
(syntax-set-style 'org org-syntax-list-bullet-match org-syntax-list-bullet-style)

