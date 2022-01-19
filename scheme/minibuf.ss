(define __cs_minibuf_create (foreign-procedure "cs_minibuf_create" () scheme-object))

(define minibuf #f)

(define minibuf-create
   (lambda ()
      (let ([m (__cs_minibuf_create)])
         (set! minibuf (window-buffer m))
      )
   )
)

(add-hook 'on-message-hook
   (lambda (m)
      (when minibuf 
         (with-buffer minibuf
            (insert (format "~a\n" m))
         )
      )
   )
)

(add-hook 'on-error-hook
   (lambda (e)
      (when minibuf
         (with-buffer minibuf
            (insert (format "~a\n" e) '(style (:fg "red")))
         )
      )
   )
)

(add-hook 'key-press-hook
   (lambda (k)
      (when minibuf
         (with-buffer minibuf
            (erase-buffer)
         )
      )
   )
)
