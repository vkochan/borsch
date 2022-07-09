(define __cs_process_create (foreign-procedure "cs_process_create"
				(string string boolean boolean boolean string boolean) scheme-object))

(define __cs_process_wait (foreign-procedure "cs_process_wait" (int) scheme-object))

(define __cs_process_is_alive (foreign-procedure __collect_safe "cs_process_is_alive" (int) scheme-object))

(define __cs_process_del (foreign-procedure "cs_process_del" (int) void))

(define __cs_evt_fd_handler_add (foreign-procedure __collect_safe "cs_evt_fd_handler_add" (int void*) int))

(define __cs_evt_fd_handler_del (foreign-procedure __collect_safe "cs_evt_fd_handler_del" (int) void))

(define-record-type process (fields port-in port-out port-err pid buffer on-exit (mutable cb)))

(define %process-pid-ht (make-eq-hashtable))
(define %process-fd-ht (make-eq-hashtable))

(define __process-on-read-async
   (lambda (proc)
      (let (
            [proc-out (process-port-out proc)]
            [buf      (process-buffer proc)]
           )
         (let ([s (get-string-some proc-out)])
            (when (not (eof-object? s))
               (with-current-buffer buf
                  (save-cursor
                     (move-buffer-end)
                     (insert s)
                  )
               )
            )
         )
      )
   )
)

(define __process-fd-handle
   (lambda (fd)
      (let ([proc (hashtable-ref %process-fd-ht fd #f)])
         (when proc
            (__process-on-read-async proc)
         )
      )
   )
)

(define __process-fd-cb
   (lambda ()
      (let ([code (foreign-callable (lambda (fd obj) (__process-fd-handle fd)) (int void*) void)])
	 (lock-object code)
	 code
      )
   )
)

(define process-wait
   (lambda (proc)
      (let ([status (call-foreign (__cs_process_wait (process-pid proc)))])
         (call-foreign (__cs_process_del (process-pid proc)))
         status
      )
   )
)

(define process-create
   (case-lambda
      [(prog)
       (process-create prog #f #f)
      ]

      [(prog buf)
       (process-create prog buf #f)
      ]

      [(prog buf on-exit)
       (let (
             [p (call-foreign (__cs_process_create prog (current-cwd) #t #t #f "" #t))]
            )
          (let (
                [in-fd  (list-ref p 0)]
                [out-fd (list-ref p 1)]
                [err-fd (list-ref p 2)]
                [pid    (list-ref p 3)]
               )
             (let (
                   [port-in (if (eq? in-fd -1) #f (open-fd-output-port in-fd (buffer-mode block) (native-transcoder)))]
                   [port-out (if (eq? out-fd -1) #f (open-fd-input-port out-fd (buffer-mode block) (native-transcoder)))]
                   [port-err (if (eq? err-fd -1) #f (open-fd-input-port err-fd (buffer-mode block) (native-transcoder)))]
                  )
                (let ([proc (make-process port-in port-out port-err pid buf on-exit #f)])
                   (hashtable-set! %process-pid-ht pid proc)
                   (when buf
                      (let ([cb (__process-fd-cb)])
                         (process-cb-set! proc cb)
                         (hashtable-set! %process-fd-ht out-fd proc)
                         (call-foreign (__cs_evt_fd_handler_add out-fd (foreign-callable-entry-point cb)))
                      )
                   )
                   proc
                )
             )
          )
       )
      ]
   )
)

(define-syntax (with-process-temp-buffer stx)
   (syntax-case stx ()
      ((_ cmd exp ...)
       #`(let ([b (buffer-new)])
            (process-create cmd b
               (lambda (b)
                  (begin
                     exp
		     ...
                  )
                  (buffer-delete b)
               )
            )
         )
      )
   )
)

(define-syntax (with-process-buffer stx)
   (syntax-case stx ()
      ((_ cmd buf exp ...)
       #`(let ([b buf])
            (process-create cmd b
               (lambda (b)
                  (begin
                     exp
		     ...
                  )
               )
            )
         )
      )
   )
)

(define process-get-output
   (lambda (cmd)
      (let (
            [proc (process-create cmd)]
           )
         (let (
               [proc-out (process-port-out proc)]
               [proc-in (process-port-in proc)]
               [pid (process-pid proc)]
               [ret ""]
              )
            (while (not (port-eof? proc-out))
               (set! ret (string-append ret (get-string-some proc-out)))
            )
            (close-port proc-out)
            (close-port proc-in)
            (let ([status (process-wait proc)])
               (list status ret)
            )
         )
      )
   )
)

(define process-with-input
   (lambda (cmd str)
      (let (
            [proc (process-create cmd)]
           )
         (let (
               [proc-out (process-port-out proc)]
               [proc-in (process-port-in proc)]
               [pid (process-pid proc)]
              )
            (put-string proc-in str)
            (close-port proc-out)
            (close-port proc-in)
            (let ([status (process-wait proc)])
               (list status)
            )
         )
      )
   )
)

(define process-with-input/output
   (lambda (cmd str)
      (let (
            [proc (process-create cmd)]
           )
         (let (
               [proc-out (process-port-out proc)]
               [proc-in (process-port-in proc)]
               [pid (process-pid proc)]
               [ret ""]
              )
            (put-string proc-in str)
            (close-port proc-in)
            (while (not (port-eof? proc-out))
               (set! ret (string-append ret (get-string-some proc-out)))
            )
            (close-port proc-out)
            (let ([status (process-wait proc)])
               (list status ret)
            )
         )
      )
   )
)

(define program-exists?
   (lambda (prog)
      (= 0 (system (format "command -v ~a > /dev/null" prog)))
   )
)

(define on-process-exit
   (lambda (pid)
      (let ([proc (hashtable-ref %process-pid-ht pid #f)])
         (when proc
            (when (process-buffer proc)
               (__process-on-read-async proc)
               (let ([fd (port-file-descriptor (process-port-out proc))])
                  (hashtable-delete! %process-fd-ht fd)
                  (call-foreign (__cs_evt_fd_handler_del fd))
               )
               (close-port (process-port-out proc))
               (close-port (process-port-in proc))
            )
            (when (process-on-exit proc)
               ((process-on-exit proc) (process-buffer proc))
               (unlock-object (process-cb proc))
            )
            (hashtable-delete! %process-pid-ht pid)
            (call-foreign (__cs_process_del pid))
         )
      )
   )
)
(add-hook 'process-exit-hook on-process-exit)
