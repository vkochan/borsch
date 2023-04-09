(define __cs_buf_parser_set (foreign-procedure "cs_buf_parser_set" (int string) scheme-object))
(define __cs_stx_lang_style_add (foreign-procedure "cs_stx_lang_style_add" (string int int int string string) scheme-object))
(define __cs_stx_lang_style_del (foreign-procedure "cs_stx_lang_style_del" (string string) void))
(define __cs_stx_lang_style_clear (foreign-procedure "cs_stx_lang_style_clear" (string string) void))
(define __cs_stx_highlight_qry (foreign-procedure "cs_stx_highlight_qry" (string string) void))

(define syntax-set-lang
   (lambda (lang)
      (call-foreign (__cs_buf_parser_set (current-buffer) (symbol->string lang)))
   )
)

(define syntax-add-style
   (lambda (lang qry style)
      (let ([lst (if (symbol? style)
                     (list -1 -1 -1)
                     (style->list style)
                 )
            ]
           )
         (call-foreign (__cs_stx_lang_style_add (symbol->string lang)
                                  (list-ref lst 0)
                                  (list-ref lst 1)
                                  (list-ref lst 2)
                                  (if (symbol? style)
                                      (symbol->string style)
                                      #f
                                  )
                                  qry))
      )
   )
)

(define syntax-delete-style
   (lambda (lang qry)
      (call-foreign
         (__cs_stx_lang_style_del (symbol->string lang)
                                     qry))
   )
)

(define syntax-set-style
   (lambda (lang qry style)
      (syntax-delete-style lang qry)
      (syntax-add-style lang qry style)
   )
)

(define syntax-delete-all-styles
   (lambda (lang)
      (call-foreign
         (__cs_stx_lang_style_clear (symbol->string lang)))
   )
)

(define (syntax-highlight lang qry)
   (call-foreign
      (__cs_stx_highlight_qry (symbol->string lang) qry)))

(define-style syntax-headline1    '(fg: "blue"))
(define-style syntax-headline2    '(fg: "magenta"))
(define-style syntax-headline3    '(fg: "cyan"))
(define-style syntax-keyword-todo  '(fg: "red" attr: "bold"))
(define-style syntax-keyword-done  '(fg: "green" attr: "bold"))
(define-style syntax-bold          '(attr: "bold"))
(define-style syntax-italic        '(attr: "italic"))
(define-style syntax-underline     '(attr: "underline"))
(define-style syntax-list-bullet   '(fg: "bright-yellow" attr: "bold"))

(define-style syntax-type             '(fg: "white"   attr: "bold"))
(define-style syntax-macro            '(fg: "magenta" attr: "italic"))
(define-style syntax-function         '(fg: "yellow"))
(define-style syntax-function-special '(fg: "yellow"))
(define-style syntax-bracket          '(fg: "yellow" attr: "bold"))
(define-style syntax-delimiter        '(fg: "white"))
(define-style syntax-string           '(fg: "bright-yellow"))
(define-style syntax-keyword          '(fg: "green"))
(define-style syntax-variable         '(fg: "cyan"))
(define-style syntax-comment          '(fg: "bright-black"))
(define-style syntax-symbol           '(fg: "cyan"))
(define-style syntax-label            '(fg: "magenta"))
(define-style syntax-property         '(fg: "cyan"))
(define-style syntax-tag              '(fg: "blue"))
(define-style syntax-operator         '(fg: "yellow" attr: "bold"))
(define-style syntax-number           '(fg: "blue"   attr: "bold"))
(define-style syntax-constant         '(fg: "blue"   attr: "bold"))
(define-style syntax-constant-builtin '(fg: "red"    attr: "bold"))