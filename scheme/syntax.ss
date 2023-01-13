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
