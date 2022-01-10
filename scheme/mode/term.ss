(define __cs_win_keys_send (foreign-procedure "cs_win_keys_send" (int string) int))
(define __cs_win_text_send (foreign-procedure "cs_win_text_send" (int string) int))
(define __cs_win_create (foreign-procedure "cs_win_create" (string string) scheme-object))

(define term-send-keys
   (case-lambda
      [(keys)
       (__cs_win_keys_send (__cs_win_current_get) keys)]

      [(wid keys)
       (__cs_win_keys_send wid keys)]
   )
)

(define term-send-text
   (case-lambda
      [(text)
       (__cs_win_text_send (__cs_win_current_get) text)]

      [(wid text)
       (__cs_win_text_send wid text)]
   )
)

(define term
   (case-lambda
      [()
       (term #f "")]

      [(prog)
       (term prog "")]

      [(prog title)
       (let ([w (__cs_win_create prog title)])
          (run-hooks 'window-create-hook w)
          w
       )
      ]
   )
)
