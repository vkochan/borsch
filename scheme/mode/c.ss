(define c-mode-compile-buffer
   (lambda ()
      (term (format "(gcc -c ~a) || read" (buffer-filename)))
   )
)

(define c-mode-compile-and-run-buffer
   (lambda ()
      (let (
            [prog (path-root (buffer-filename))]
            [file (buffer-filename)]
           )
         (term (format "(gcc ~a -o ~a && ~a) ; read" file prog prog))
      )
   )
)

(define-mode c-mode "C" text-mode
   (bind-key-local "C-c C-c" c-mode-compile-buffer)
   (bind-key-local "C-c C-r" c-mode-compile-and-run-buffer)
   (syntax-set-lang 'c)
)

(add-to-list 'file-ext-mode '("h" . c-mode))
(add-to-list 'file-ext-mode '("c" . c-mode))

(define c-syntax-keywords-match
   "[
    \"break\"
    \"case\"
    \"const\"
    \"continue\"
    \"default\"
    \"do\"
    \"else\"
    \"enum\"
    \"extern\"
    \"for\"
    \"if\"
    \"inline\"
    \"return\"
    \"sizeof\"
    \"static\"
    \"struct\"
    \"switch\"
    \"typedef\"
    \"union\"
    \"volatile\"
    \"while\"
    \"#define\"
    \"#elif\"
    \"#else\"
    \"#endif\"
    \"#if\"
    \"#ifdef\"
    \"#ifndef\"
    \"#include\"
     (preproc_directive)
    ] @keyword"
)

(define c-syntax-preproc-match
   "[
    \"#define\"
    \"#elif\"
    \"#else\"
    \"#endif\"
    \"#if\"
    \"#ifdef\"
    \"#ifndef\"
    \"#include\"
     (preproc_directive)
    ] @preproc"
)

(define c-syntax-type-id-match         "(type_identifier) @type")
(define c-syntax-type-primitive-match  "(primitive_type) @type")
(define c-syntax-type-sized-spec-match "(sized_type_specifier) @type")

(define c-syntax-string-match "(string_literal) @string")
(define c-syntax-string-sys-lib-match "(system_lib_string) @string")

(define c-syntax-func-match "(call_expression function: (identifier) @function)")
(define c-syntax-func-call-match "(call_expression function: (field_expression field: (field_identifier) @function))")
(define c-syntax-func-decl-match "(function_declarator declarator: (identifier) @function) (preproc_function_def name: (identifier) @function.special)")

(define c-syntax-operator-match
   "[
    \"--\"
    \"-\"
    \"-=\"
    \"->\"
    \"=\"
    \"!=\"
    \"|=\"
    \"&=\"
    \"*\"
    \"&\"
    \"~\"
    \"|\"
    \"!\"
    \"&&\"
    \"+\"
    \"++\"
    \"+=\"
    \"<\"
    \"==\"
    \">\"
    \"||\"
   ] @operator"
)

(define c-syntax-constant-null-match "(null) @constant")
(define c-syntax-constant-match "((identifier) @constant (#match? @constant \"^[A-Z][A-Z\\d_]*$\"))")
(define c-syntax-number-match "(number_literal) @number")
(define c-syntax-char-match "(char_literal) @number")

(define c-syntax-property-match "(field_identifier) @property")
(define c-syntax-label-match "(statement_identifier) @label")
(define c-syntax-variable-match "(identifier) @variable")

(define c-syntax-delimiter-match "[\".\" \";\"] @delimiter")

(define c-syntax-comment-match "(comment) @comment")

(define c-syntax-keywords-style      '(:fg "bright-yellow"))
(define c-syntax-preproc-style       '(:fg "green"))
(define c-syntax-type-style          '(:fg "green"))
(define c-syntax-string-style        '(:fg "bright-yellow"))
(define c-syntax-func-style          '(:fg "cyan"))
(define c-syntax-operator-style      '(:fg "yellow" :bg "bold"))
(define c-syntax-delimiter-style     '(:fg "green"))
(define c-syntax-comment-style       '(:fg "bright-black"))
(define c-syntax-variable-style      '(:fg "cyan"))
(define c-syntax-number-style        '(:fg "blue"))
(define c-syntax-constant-style      '(:fg "blue"))
(define c-syntax-label-style         '(:fg "red"))

(syntax-set-style 'c c-syntax-constant-null-match c-syntax-constant-style)
(syntax-set-style 'c c-syntax-keywords-match c-syntax-keywords-style)
(syntax-set-style 'c c-syntax-type-id-match c-syntax-type-style)
(syntax-set-style 'c c-syntax-type-primitive-match c-syntax-type-style)
(syntax-set-style 'c c-syntax-type-sized-spec-match c-syntax-type-style)
(syntax-set-style 'c c-syntax-string-match c-syntax-string-style)
(syntax-set-style 'c c-syntax-string-sys-lib-match c-syntax-string-style)
(syntax-set-style 'c c-syntax-operator-match c-syntax-operator-style)
(syntax-set-style 'c c-syntax-delimiter-match c-syntax-delimiter-style)
(syntax-set-style 'c c-syntax-comment-match c-syntax-comment-style)
(syntax-set-style 'c c-syntax-variable-match c-syntax-variable-style)
(syntax-set-style 'c c-syntax-number-match c-syntax-number-style)
(syntax-set-style 'c c-syntax-preproc-match c-syntax-preproc-style)
;;(syntax-set-style 'c c-syntax-property-match c-syntax-variable-style)
;;(syntax-set-style 'c c-syntax-label-match c-syntax-label-style)
(syntax-set-style 'c c-syntax-func-call-match c-syntax-func-style)
(syntax-set-style 'c c-syntax-func-decl-match c-syntax-func-style)
(syntax-set-style 'c c-syntax-func-match c-syntax-func-style)
