(define __cs_buf_parser_set (foreign-procedure "cs_buf_parser_set" (int string) scheme-object))
(define __cs_stx_lang_style_add (foreign-procedure "cs_stx_lang_style_add" (string int int int string) scheme-object))
(define __cs_stx_lang_style_del (foreign-procedure "cs_stx_lang_style_del" (string string) void))
(define __cs_stx_lang_style_clear (foreign-procedure "cs_stx_lang_style_clear" (string string) void))

(define syntax-set-lang
   (lambda (lang)
      (call-foreign (__cs_buf_parser_set (current-buffer) (symbol->string lang)))
   )
)

(define syntax-add-style
   (lambda (lang qry style)
      (let ([lst (style->list style)])
         (call-foreign (__cs_stx_lang_style_add (symbol->string lang)
                                  (list-ref lst 0)
                                  (list-ref lst 1)
                                  (list-ref lst 2)
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

(define syntax-delete-all-styles
   (lambda (lang)
      (call-foreign
         (__cs_stx_lang_style_clear (symbol->string lang)))
   )
)
