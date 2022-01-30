(define lisp-mode-eval
   (lambda (s)
      (let (
            [code (open-string-input-port s)]
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

(define lisp-mode-eval-buffer
   (lambda ()
      (lisp-mode-eval (buffer-string))
   )
)

(define-mode lisp-mode "Lisp" text-mode
   (bind-key-local "C-c C-c" lisp-mode-eval-buffer)
)
