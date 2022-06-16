(define-mode dts-mode "Device-tree" text-mode
   (syntax-set-lang 'dts)
)

(add-to-list 'file-match-mode '(".*\\.dts$" . dts-mode))
(add-to-list 'file-match-mode '(".*\\.dtsi$" . dts-mode))

(define dts-syntax-keyword-match
   "[
    \"/dts-v1/\"
    \"/delete-node/\"
    \"/delete-property/\"
    \"#define\"
    \"#include\"
   ] @keyword"
)
(define dts-syntax-operator-match
   "[
    \"!\"
    \"~\"
    \"-\"
    \"+\"
    \"*\"
    \"/\"
    \"%\"
    \"||\"
    \"&&\"
    \"|\"
    \"^\"
    \"&\"
    \"=\"
    \"==\"
    \"!=\"
    \"<\"
    \">=\"
    \"<=\"
    \">\"
    \"<<\"
    \">>\"
   ] @operator"
)
(define dts-syntax-bracket-match
   "[
    \"(\"
    \")\"
    \"{\"
    \"}\"
    \"<\"
    \">\"
    \"[\"
    \"]\"
   ] @punctuation.bracket"
)
(define dts-syntax-delimiter-match
   "[
    \",\"
    \";\"
   ] @punctuation.delimiter"
)
(define dts-syntax-comment-match "(comment) @comment")
(define dts-syntax-reference-match "(reference) @constant")
(define dts-syntax-address-match "(unit_address) @tag")
(define dts-syntax-variable-match "(identifier) @variable")
(define dts-syntax-property-match "(property name: (identifier) @property)")
(define dts-syntax-label-match "(labeled_item label: (identifier) @label)")
(define dts-syntax-string-match "(string_literal) @string")
(define dts-syntax-number-match "(integer_literal) @number")

(define dts-syntax-keywords-style      '(:fg "green"))
(define dts-syntax-bracket-style       '(:fg "yellow"))
(define dts-syntax-delimiter-style     '(:fg "white"))
(define dts-syntax-operator-style      '(:fg "yellow" :attr "bold"))
(define dts-syntax-comment-style       '(:fg "bright-black"))
(define dts-syntax-reference-style     '(:fg "red"))
(define dts-syntax-address-style       '(:fg "blue"))
(define dts-syntax-variable-style      '(:fg "green"))
(define dts-syntax-property-style      '(:fg "cyan"))
(define dts-syntax-label-style         '(:fg "magenta"))
(define dts-syntax-string-style        '(:fg "bright-yellow"))
(define dts-syntax-number-style        '(:fg "blue"))

(syntax-set-style 'dts dts-syntax-keyword-match dts-syntax-keywords-style)
(syntax-set-style 'dts dts-syntax-operator-match dts-syntax-operator-style)
(syntax-set-style 'dts dts-syntax-comment-match dts-syntax-comment-style)
(syntax-set-style 'dts dts-syntax-reference-match dts-syntax-reference-style)
(syntax-set-style 'dts dts-syntax-address-match dts-syntax-address-style)
(syntax-set-style 'dts dts-syntax-variable-match dts-syntax-variable-style)
(syntax-set-style 'dts dts-syntax-property-match dts-syntax-property-style)
(syntax-set-style 'dts dts-syntax-label-match dts-syntax-label-style)
(syntax-set-style 'dts dts-syntax-bracket-match dts-syntax-bracket-style)
(syntax-set-style 'dts dts-syntax-delimiter-match dts-syntax-delimiter-style)
(syntax-set-style 'dts dts-syntax-string-match dts-syntax-string-style)
(syntax-set-style 'dts dts-syntax-number-match dts-syntax-number-style)
