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
   ] @bracket"
)
(define dts-syntax-delimiter-match
   "[
    \",\"
    \";\"
   ] @delimiter"
)
(define dts-syntax-comment-match "(comment) @comment")
(define dts-syntax-constant-match "(reference) @constant")
(define dts-syntax-tag-match "(unit_address) @tag")
(define dts-syntax-variable-match "(identifier) @variable")
(define dts-syntax-property-match "(property name: (identifier) @property)")
(define dts-syntax-label-match "(labeled_item label: (identifier) @label)")
(define dts-syntax-string-match "(string_literal) @string")
(define dts-syntax-number-match "(integer_literal) @number")

(syntax-highlight 'dts
   (string-append
      dts-syntax-keyword-match
      dts-syntax-operator-match
      dts-syntax-bracket-match
      dts-syntax-delimiter-match
      dts-syntax-comment-match
      dts-syntax-constant-match
      dts-syntax-tag-match
      dts-syntax-variable-match
      dts-syntax-property-match
      dts-syntax-label-match
      dts-syntax-string-match
      dts-syntax-number-match))
