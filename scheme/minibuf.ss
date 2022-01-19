(define __cs_minibuf_create (foreign-procedure "cs_minibuf_create" () scheme-object))

(define minibuf-window #f)
(define minibuf-buffer #f)

(define minibuf-create
   (lambda ()
      (let ([m (__cs_minibuf_create)])
         (set! minibuf-buffer (window-buffer m))
         (set! minibuf-window m)
      )
   )
)

(add-hook 'on-message-hook
   (lambda (m)
      (when minibuf-buffer
         (window-set-height minibuf-window (1+ (lines-count m)))
         (with-buffer minibuf-buffer
            (insert (format "~a\n" m))
            (cursor-set 0)
         )
      )
   )
)

(add-hook 'on-error-hook
   (lambda (e)
      (when minibuf-buffer
         (window-set-height minibuf-window (1+ (lines-count e)))
         (with-buffer minibuf-buffer
            (insert (format "~a\n" e) '(style (:fg "red")))
            (cursor-set 0)
         )
      )
   )
)

(add-hook 'key-press-hook
   (lambda (k)
      (when minibuf-buffer
         (with-buffer minibuf-buffer
            (window-set-height minibuf-window 1)
            (erase-buffer)
         )
      )
   )
)
