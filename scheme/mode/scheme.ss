(define scheme-mode-eval
   (lambda (s)
      (let (
            [code (open-string-input-port (format "(begin ~a)" s))]
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

(define scheme-mode-eval-buffer
   (lambda ()
      (scheme-mode-eval (buffer-string))
   )
)

(define-mode scheme-mode "Scheme" text-mode
   (bind-key-local "C-c C-c" scheme-mode-eval-buffer)
)
