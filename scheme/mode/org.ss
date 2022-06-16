(define-mode org-mode "Org" text-mode
   (syntax-set-lang 'org)
)

(add-to-list 'file-match-mode '(".*\\.org$" . org-mode))

(define org-syntax-head-line1-match
   "(headline (stars) @OrgStars1 (#match? @OrgStars1 \"^(\\\\*{3})*\\\\*$\") (item) @OrgHeadlineLevel1)"
)

(define org-syntax-head-line2-match
   "(headline (stars) @OrgStars2 (#match? @OrgStars2 \"^(\\\\*{3})*\\\\*\\\\*$\") (item) @OrgHeadlineLevel2)"
)

(define org-syntax-head-line3-match
   "(headline (stars) @OrgStars3 (#match? @OrgStars3 \"^(\\\\*{3})*\\\\*\\\\*\\\\*$\") (item) @OrgHeadlineLevel3)"
)
(define org-syntax-bold-match
   "(paragraph [
    ((expr \"*\" @bold.start) (expr \"*\" @bold.end))
    (expr \"*\" @bold.start \"*\" @bold.end)
   ] @bold)"
)
(define org-syntax-italic-match
   "(paragraph [
    ((expr \"/\" @italic.start) (expr \"/\" @italic.end))
    (expr \"/\" @italic.start \"/\" @italic.end)
   ] @italic)"
)
(define org-syntax-underline-match
   "(paragraph [
    ((expr \"_\" @underline.start) (expr \"_\" @underline.end))
    (expr \"_\" @underline.start \"_\" @underline.end)
   ] @underline)"
)
(define org-syntax-todo-match "(item . (expr) @OrgKeywordTodo (#eq? @OrgKeywordTodo \"TODO\"))")
(define org-syntax-done-match "(item . (expr) @OrgKeywordDone (#eq? @OrgKeywordDone \"DONE\"))")
(define org-syntax-list-bullet-match "(bullet) @OrgListBullet")

(define org-syntax-head-line1-style    '(:fg "blue"))
(define org-syntax-head-line2-style    '(:fg "magenta"))
(define org-syntax-head-line3-style    '(:fg "cyan"))
(define org-syntax-todo-style          '(:fg "red" :attr "bold"))
(define org-syntax-done-style          '(:fg "green" :attr "bold"))
(define org-syntax-bold-style          '(:attr "bold"))
(define org-syntax-italic-style        '(:attr "italic"))
(define org-syntax-underline-style     '(:attr "underline"))
(define org-syntax-list-bullet-style   '(:fg "bright-yellow" :attr "bold"))

(syntax-set-style 'org org-syntax-head-line1-match org-syntax-head-line1-style)
(syntax-set-style 'org org-syntax-head-line2-match org-syntax-head-line2-style)
(syntax-set-style 'org org-syntax-head-line3-match org-syntax-head-line3-style)
(syntax-set-style 'org org-syntax-todo-match org-syntax-todo-style)
(syntax-set-style 'org org-syntax-done-match org-syntax-done-style)
(syntax-set-style 'org org-syntax-bold-match org-syntax-bold-style)
(syntax-set-style 'org org-syntax-italic-match org-syntax-italic-style)
(syntax-set-style 'org org-syntax-underline-match org-syntax-underline-style)
(syntax-set-style 'org org-syntax-list-bullet-match org-syntax-list-bullet-style)
