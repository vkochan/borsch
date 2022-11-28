(define scheme-eval
   (lambda (s)
      (let (
            [code (open-string-input-port (format "(begin ~a)" s))]
            [ret '()]
            [out ""]
           )
         (set! out (with-output-to-string
                      (lambda ()
                         (set! ret (try eval-port->str code))
                      )
                   )
         )
         (close-port code)
         (set! out (string-append out (second ret)))
         (message out)
      )
   )
)

(define scheme-eval-buffer
   (lambda ()
      (if (text-is-selection-set?)
         (let ([sel (text-selection)])
            (text-clear-selection)
            (scheme-eval sel)
         )
         ;; else
         (scheme-eval (text-string))
      )
   )
)

(define scheme-extract-word
   (lambda ()
      (pregexp-replace* "\\[|\\]|\\'|\\(|\\)" (text-longword) "")
   )
)

(define-mode scheme-mode "Scheme" text-mode
   (bind-key-local "C-c C-c" scheme-eval-buffer)
   (syntax-set-lang 'scheme)
   (define-local text-word-func scheme-extract-word)
)

(add-to-list 'file-match-mode '(".*\\.scm$" . scheme-mode))
(add-to-list 'file-match-mode '(".*\\.sls$" . scheme-mode))
(add-to-list 'file-match-mode '(".*\\.ss$" . scheme-mode))
(add-to-list 'file-match-mode '(".*\\.el$" . scheme-mode))

(define scheme-syntax-function-match "(list . (symbol) @function)")
(define scheme-syntax-number-match "(number) @number")
(define scheme-syntax-char-match   "(character) @character")
(define scheme-syntax-constant-builtin-match "(boolean) @constant.builtin")
(define scheme-syntax-symbol-match "(abbreviation  \"'\" (symbol)) @constant")
(define scheme-syntax-string-match "[(string) (character)] @string")
(define scheme-syntax-comment-match "[(comment) (block_comment) (directive)] @comment")
(define scheme-syntax-bracket-match "[\"(\" \")\" \"[\" \"]\" \"{\" \"}\"] @punctuation.bracket")
(define scheme-syntax-keyword-match
   "(list
     .
     (symbol) @keyword
     (#match? @keyword
      \"^((define)|(let)|(let\\*)|(lambda)|(if)|(cond)|(case)|(else)|(and)|(or)|(not)|(set!)|(begin)|(when))$\"))"
)
(define scheme-syntax-operator-match
   "(list
     .
     (symbol) @operator
     (#match? @operator \"^([+*/<>=-]|(<=)|(>=))$\"))"
)

(define scheme-syntax-keyword-style             '(:fg "green"))
(define scheme-syntax-string-style              '(:fg "bright-yellow"))
(define scheme-syntax-function-style            '(:fg "cyan"))
(define scheme-syntax-operator-style            '(:fg "yellow" :attr "bold"))
(define scheme-syntax-bracket-style             '(:fg "yellow" :attr "bold"))
(define scheme-syntax-comment-style             '(:fg "bright-black"))
(define scheme-syntax-number-style              '(:fg "blue"))
(define scheme-syntax-symbol-style              '(:fg "cyan"))
(define scheme-syntax-constant-builtin-style    '(:fg "blue"))

(syntax-set-style 'scheme scheme-syntax-string-match scheme-syntax-string-style)
(syntax-set-style 'scheme scheme-syntax-function-match scheme-syntax-function-style)
(syntax-set-style 'scheme scheme-syntax-keyword-match scheme-syntax-keyword-style)
(syntax-set-style 'scheme scheme-syntax-comment-match scheme-syntax-comment-style)
(syntax-set-style 'scheme scheme-syntax-number-match scheme-syntax-number-style)
(syntax-set-style 'scheme scheme-syntax-symbol-match scheme-syntax-symbol-style)
(syntax-set-style 'scheme scheme-syntax-constant-builtin-match scheme-syntax-constant-builtin-style)
(syntax-set-style 'scheme scheme-syntax-bracket-match scheme-syntax-bracket-style)
