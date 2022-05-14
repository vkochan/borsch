(define __cs_term_keys_send (foreign-procedure "cs_term_keys_send" (int string) int))
(define __cs_term_text_send (foreign-procedure "cs_term_text_send" (int string) int))
(define __cs_term_create (foreign-procedure "cs_term_create" (string string string) scheme-object))
(define __cs_term_text_get (foreign-procedure "cs_term_text_get" (int) scheme-object))

(define term-send-keys
   (case-lambda
      [(keys)
       (call-foreign (__cs_term_keys_send (current-buffer) keys))]

      [(bid keys)
       (call-foreign (__cs_term_keys_send bid keys))]
   )
)

(define term-send-text
   (case-lambda
      [(text)
       (call-foreign (__cs_term_text_send (current-buffer) text))]

      [(bid text)
       (call-foreign (__cs_term_text_send bid text))]
   )
)

(define term-string
   (case-lambda
      [()
       (term-string (current-buffer))]

      [(b)
       (call-foreign (__cs_term_text_get b))]
   )
)

(define term-mode-copy-enter
   (lambda ()
      (let* (
             [b (buffer-new)]
             [c (current-buffer)]
             [s (term-string c)]
            )
         (with-current-buffer b
            (text-mode)
            (bind-key-local "<Esc>" term-mode-copy-exit)
            (bind-key-local "q" term-mode-copy-exit)
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
      (let ([c (current-buffer)])
         (window-switch-buffer (get-local orig-buf))
         (buffer-delete c)
      )
   )
)

(define term-mode-paste
   (lambda ()
      (term-send-text copybuf-reg)
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
       (term #f)]

      [(prog)
       (term prog "")]

      [(prog title)
       (term prog title (current-cwd))]

      [(prog title cwd)
       (let* (
              [w (call-foreign (__cs_term_create prog title cwd))]
              [b (window-buffer w)]
             )
          (define-local major-mode 'term-mode)
          (buffer-set-keymap 'term-mode-map)
          (buffer-set-mode-name "Term")
          (run-hooks 'window-create-hook w)
          w
       )
      ]
   )
)
