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

(define term-mode-copy-enter
   (lambda ()
      (let* (
             [b (buffer-new)]
             [c (buffer-current)]
             [s (term-string c)]
            )
         (with-buffer b
            (text-mode)
            (buffer-set-keymap 'term-mode-copy-map)
            (buffer-set-name "Term Copy")
            (define-local orig-buf c)
            (window-switch-buffer b)
            (insert s)
         )
      )
   )
)

(define term-mode-copy-exit
   (lambda ()
      (let ([c (buffer-current)])
         (window-switch-buffer (get-local orig-buf))
         (buffer-delete c)
      )
   )
)

(define term-mode-paste
   (lambda ()
      (term-send-text reg)
   )
)

(define term-mode-copy-map
  (let ([map (make-keymap 'text-mode-cmd-map)])
     (bind-key map "<Esc>" term-mode-copy-exit)
     (bind-key map "q" term-mode-copy-exit)
     map
  )
)

(define term-mode-map
  (let ([map (make-keymap)])
     (bind-key map "C-y" term-mode-copy-enter)
     (bind-key map "C-p" term-mode-paste)
     map
  )
)

(define term
   (case-lambda
      [()
       (term #f "")]

      [(prog)
       (term prog "")]

      [(prog title)
       (let* (
              [w (__cs_term_create prog title)]
              [b (window-buffer w)]
             )
          (buffer-set-keymap 'term-mode-map)
          (run-hooks 'window-create-hook w)
          w
       )
      ]
   )
)
