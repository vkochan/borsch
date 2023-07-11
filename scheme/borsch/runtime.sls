(library (borsch runtime)
   (export
      do-quit
      message
      config-dir
      runtime-initialize
      runtime-cleanup)
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

(define *config-dir* #f)

(define (config-dir)
   *config-dir*)
   
(define __cs_runtime_init (foreign-procedure "cs_runtime_init" () int))
(define __cs_runtime_cleanup (foreign-procedure "cs_runtime_cleanup" () void))

(define (runtime-initialize)
   (set! *config-dir* (string-append (getenv "HOME") "/.config/borsch"))
   (__cs_runtime_init))

(define (runtime-cleanup)
   (__cs_runtime_cleanup))

)
