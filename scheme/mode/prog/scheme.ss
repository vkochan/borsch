(define (scheme-eval s)
   (let ([code (open-string-input-port (format "(begin ~a)" s))]
         [ret ""]
         [out ""])
      (set! out (with-output-to-string
                   (lambda ()
                      (try
                         (begin
                            (set! ret (eval-port->str code)))
                      (lambda (ex)
                         (set! ret (error->string ex))))
                   )))
      (close-port code)
      (set! out (string-append out ret))
      (message out)))

(define (scheme-eval-buffer)
   (if (text-is-selection-set?)
      (let ([sel (text-selection)])
         (text-clear-selection)
         (scheme-eval sel))
      ;; else
      (scheme-eval (text-string))))

(define (scheme-extract-word)
   (pregexp-replace* "\\[|\\]|\\'|\\(|\\)" (text-longword) ""))

(define-mode scheme-mode "Scheme" text-mode
   (bind-key-local "C-c C-c" scheme-eval-buffer)
   (syntax-set-lang 'scheme)
   (define-local text-word-func scheme-extract-word))

(add-to-list 'file-match-mode '(".*\\.scm$" . scheme-mode))
(add-to-list 'file-match-mode '(".*\\.sls$" . scheme-mode))
(add-to-list 'file-match-mode '(".*\\.sps$" . scheme-mode))
(add-to-list 'file-match-mode '(".*\\.ss$" . scheme-mode))
(add-to-list 'file-match-mode '(".*\\.el$" . scheme-mode))

(define scheme-syntax-function-match "(list . (symbol) @function)")
(define scheme-syntax-number-match "(number) @number")
(define scheme-syntax-char-match   "(character) @character")
(define scheme-syntax-constant-builtin-match "(boolean) @constant-builtin")
(define scheme-syntax-symbol-match "(quote  \"'\" (symbol)) @constant")
(define scheme-syntax-string-match "[(string) (character)] @string")
(define scheme-syntax-comment-match "[(comment) (block_comment) (directive)] @comment")
(define scheme-syntax-bracket-match "[\"(\" \")\" \"[\" \"]\" \"{\" \"}\"] @bracket")
(define scheme-syntax-keyword-match
   "(list
     .
     (symbol) @keyword
     (#match? @keyword
      \"^((define)|(define-syntax)|(syntax-case)|(let)|(let\\*)|(lambda)|(case-lambda)|(if)|(cond)|(case)|(else)|(and)|(or)|(not)|(set!)|(begin)|(when))$\"))")

(define scheme-syntax-operator-match
   "(list
     .
     (symbol) @operator
     (#match? @operator \"^([+*/<>=-]|(<=)|(>=))$\"))")

(syntax-highlight 'scheme
   (string-append
      scheme-syntax-function-match
      scheme-syntax-number-match
      scheme-syntax-char-match
      scheme-syntax-symbol-match
      scheme-syntax-string-match
      scheme-syntax-comment-match
      scheme-syntax-bracket-match
      scheme-syntax-keyword-match
      scheme-syntax-constant-builtin-match
      scheme-syntax-operator-match))
