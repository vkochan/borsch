(define __cs_term_keys_send (foreign-procedure "cs_term_keys_send" (int string) int))
(define __cs_term_text_send (foreign-procedure "cs_term_text_send" (int string) int))
(define __cs_term_create (foreign-procedure "cs_term_create" (string string string) scheme-object))
(define __cs_term_text_get (foreign-procedure "cs_term_text_get" (int) scheme-object))

(define vterm-send-keys
   (case-lambda
      [(keys)
       (call-foreign (__cs_term_keys_send (current-buffer) keys))]

      [(bid keys)
       (call-foreign (__cs_term_keys_send bid keys))]
   )
)

(define vterm-send-text
   (case-lambda
      [(text)
       (call-foreign (__cs_term_text_send (current-buffer) text))]

      [(bid text)
       (call-foreign (__cs_term_text_send bid text))]
   )
)

(define vterm-string
   (case-lambda
      [()
       (vterm-string (current-buffer))]

      [(b)
       (call-foreign (__cs_term_text_get b))]
   )
)

(define vterm-mode-copy-enter
   (lambda ()
      (let* (
             [b (buffer-new)]
             [c (current-buffer)]
             [s (vterm-string c)]
            )
         (with-current-buffer b
            (text-mode)
            (bind-key-local "<Esc>" vterm-mode-copy-exit)
            (bind-key-local "q" vterm-mode-copy-exit)
            (buffer-set-name "Term Copy")
            (define-local orig-buf c)
            (window-switch-buffer b)
            (insert s)
         )
      )
   )
)

(define vterm-mode-copy-exit
   (lambda ()
      (let ([c (current-buffer)])
         (window-switch-buffer (get-local orig-buf))
         (buffer-delete c)
      )
   )
)

(define vterm-mode-paste
   (lambda ()
      (vterm-send-text copybuf-reg)
   )
)

(define vterm-mode-map
  (let ([map (make-keymap)])
     (bind-key map "C-y" vterm-mode-copy-enter)
     (bind-key map "C-p" vterm-mode-paste)
     map
  )
)

(define vterm
   (case-lambda
      [()
       (vterm #f "" (current-cwd))]

      [(prog)
       (vterm prog "" (current-cwd))]

      [(prog title)
       (vterm prog title (current-cwd))]

      [(prog title cwd)
       (let* (
              [w (call-foreign (__cs_term_create prog title cwd))]
              [b (window-buffer w)]
             )
          (define-local major-mode 'vterm-mode)
          (buffer-set-keymap 'vterm-mode-map)
          (buffer-set-mode-name "VTerm")
          (run-hooks 'window-create-hook w)
          w
       )
      ]
   )
)
