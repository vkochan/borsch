(library (borsch runtime)
   (export
      do-quit
      message)
   (import
      (borsch base)
      (borsch buffer)
      (borsch text)
      (chezscheme))
   
(define __cs_do_quit (foreign-procedure "cs_do_quit" () void))

(define (message s)
   (let ([b (buffer-get "*Messages*")])
      (when b
         (with-current-buffer b
            (text-insert (format "~a\n" s))))
      (run-hooks 'message-hook s)))

(define (do-quit)
   (run-hooks 'exit-hook)
   (call-foreign (__cs_do_quit)))

)
