(define-mode org-mode "Org" text-mode
   (syntax-set-lang 'org)
)

(file-match-mode-add '(".*\\.org$" . org-mode))

(define org-syntax-headline1-match
   "(headline (stars) @OrgStars1 (#match? @OrgStars1 \"^(\\\\*{3})*\\\\*$\") (item) @headline1)"
)

(define org-syntax-headline2-match
   "(headline (stars) @OrgStars2 (#match? @OrgStars2 \"^(\\\\*{3})*\\\\*\\\\*$\") (item) @headline2)"
)

(define org-syntax-headline3-match
   "(headline (stars) @OrgStars3 (#match? @OrgStars3 \"^(\\\\*{3})*\\\\*\\\\*\\\\*$\") (item) @headline3)"
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
(define org-syntax-todo-match "(item . (expr) @keyword-todo (#eq? @keyword-todo \"TODO\"))")
(define org-syntax-done-match "(item . (expr) @keyword-done (#eq? @keyword-done \"DONE\"))")
(define org-syntax-list-bullet-match "(bullet) @list-bullet")

(syntax-highlight 'org
   (string-append
      org-syntax-headline1-match
      org-syntax-headline2-match
      org-syntax-headline3-match
      org-syntax-todo-match
      org-syntax-done-match
      org-syntax-bold-match
      org-syntax-italic-match
      org-syntax-underline-match
      org-syntax-list-bullet-match))
