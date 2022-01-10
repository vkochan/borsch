(define __cs_term_keys_send (foreign-procedure "cs_term_keys_send" (int string) int))
(define __cs_term_text_send (foreign-procedure "cs_term_text_send" (int string) int))
(define __cs_term_create (foreign-procedure "cs_term_create" (string string) scheme-object))
(define __cs_term_text_get (foreign-procedure "cs_term_text_get" (int) scheme-object))

(define term-send-keys
   (case-lambda
      [(keys)
       (__cs_term_keys_send (buffer-current) keys)]

      [(bid keys)
       (__cs_term_keys_send bid keys)]
   )
)

(define term-send-text
   (case-lambda
      [(text)
       (__cs_term_text_send (buffer-current) text)]

      [(bid text)
       (__cs_term_text_send bid text)]
   )
)

(define term-string
   (case-lambda
      [()
       (term-string (buffer-current))]

      [(b)
       (__cs_term_text_get b)]
   )
)

(define term
   (case-lambda
      [()
       (term #f "")]

      [(prog)
       (term prog "")]

      [(prog title)
       (let ([w (__cs_term_create prog title)])
          (run-hooks 'window-create-hook w)
          w
       )
      ]
   )
)
