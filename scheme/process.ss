(define __cs_process_create (foreign-procedure "cs_process_create"
				(string string boolean boolean boolean scheme-object boolean) scheme-object))

(define __cs_process_wait (foreign-procedure "cs_process_wait" (int) scheme-object))

(define __cs_process_is_alive (foreign-procedure __collect_safe "cs_process_is_alive" (int) scheme-object))

(define __cs_process_status_get (foreign-procedure __collect_safe "cs_process_status_get" (int) scheme-object))

(define __cs_process_del (foreign-procedure "cs_process_del" (int) void))

(define __cs_process_kill (foreign-procedure "cs_process_kill" (int) void))

(define __cs_evt_fd_handler_add (foreign-procedure __collect_safe "cs_evt_fd_handler_add" (int void*) int))

(define __cs_evt_fd_handler_del (foreign-procedure __collect_safe "cs_evt_fd_handler_del" (int) void))

(define __process-environment (list (cons "QWE" "ASD")))

(define process-environment
   (lambda ()
      __process-environment
   )
)

(define process-set-environment
   (lambda (name value)
      (let ([var (assoc name (process-environment))])
         (if var
            (set-cdr! var value)
            ;; else
            (set! __process-environment (append (process-environment) (list (cons name value))))
         )
      )
   )
)

(define process-get-environment
   (lambda (name value)
      (let ([var (assoc name (process-environment))])
         (if var (cdr var) #f)
      )
   )
)

(define-syntax (with-process-environment stx)
   (syntax-case stx ()
      ((_ env exp ...)
       #`(let ([e (process-environment)])
            (fluid-let ([__process-environment e])
               (for-each
                  (lambda (v)
                     (process-set-environment (car v) (cdr v))
                  )
                  env
               )
               (begin
                  exp
                  ...
               )
            )
         )
      )
   )
)

(define-record-type process
                       (fields port-in
                               port-out
                               port-err
                               pid
                               buffer-out
                               buffer-err
                               on-exit
                               (mutable cb)
                        ))

(define %process-pid-ht (make-eq-hashtable))
(define %process-fd-ht (make-eq-hashtable))

(define process-is-alive?
   (lambda (pid)
      (call-foreign (__cs_process_is_alive pid))
   )
)

(define process-kill
   (lambda (pid)
      (call-foreign (__cs_process_kill pid))
   )
)

(define __process-on-read-async
   (lambda (proc)
      (let (
            [proc-out (process-port-out proc)]
            [buf      (process-buffer-out proc)]
           )
         (let ([s (get-string-some proc-out)])
            (when (not (eof-object? s))
               (if (buffer-is-valid? buf)
                  (with-current-buffer buf
                     (save-cursor
                        (move-buffer-end)
                        (insert s)
                     )
                  )
                  ;; else
                  (begin
                     (process-kill (process-pid proc))
                  )
               )
            )
         )
      )
   )
)

(define __process-on-read-sync
   (lambda (proc-out buf)
      (let (
            [proc-out proc-out]
            [buf      buf]
           )
         (let ([s (get-string-some proc-out)])
            (while (not (eof-object? s))
               (with-current-buffer buf
                  (save-cursor
                     (move-buffer-end)
                     (insert s)
                  )
               )
               (set! s (get-string-some proc-out))
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

(define process-status
   (lambda (pid)
      (call-foreign (__cs_process_status_get pid))
   )
)

(define process-create
   (case-lambda
      [(prog)
       (process-create prog #f #f)
      ]

      [(prog buf-out)
       (process-create prog buf-out #f)
      ]

      [(prog buf-out on-exit)
       (process-create prog buf-out #f on-exit)
      ]

      [(prog buf-out buf-err on-exit)
       (let*(
             [env (process-environment)]
             [p (call-foreign (__cs_process_create prog (current-cwd) #t #t (not (equal? buf-err #f)) env #t))]
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
                (let ([proc (make-process port-in port-out port-err pid buf-out buf-err on-exit #f)])
                   (hashtable-set! %process-pid-ht pid proc)
                   (when buf-out
                      (let ([cb (__process-fd-cb)])
                         (process-cb-set! proc cb)
                         (hashtable-set! %process-fd-ht out-fd proc)
                         (call-foreign (__cs_evt_fd_handler_add out-fd (foreign-callable-entry-point cb)))
                      )
                   )
                   (when buf-err
                      (let ([cb (__process-fd-cb)])
                         (process-cb-set! proc cb)
                         (hashtable-set! %process-fd-ht err-fd proc)
                         (call-foreign (__cs_evt_fd_handler_add err-fd (foreign-callable-entry-point cb)))
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
               (lambda (proc-status buf-out buf-err)
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
               (lambda (proc-status buf-out buf-err)
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
      (let (
            [proc (hashtable-ref %process-pid-ht pid #f)]
            [alive? (process-is-alive? pid)]
           )
         (when proc
            (when (process-buffer-out proc)
               (when alive?
                  (__process-on-read-sync (process-port-out proc) (process-buffer-out proc))
               )
               (let ([fd (port-file-descriptor (process-port-out proc))])
                  (hashtable-delete! %process-fd-ht fd)
                  (call-foreign (__cs_evt_fd_handler_del fd))
               )
               (close-port (process-port-out proc))
            )
            (when (process-buffer-err proc)
               (when alive?
                  (__process-on-read-sync (process-port-err proc) (process-buffer-err proc))
               )
               (let ([fd (port-file-descriptor (process-port-err proc))])
                  (hashtable-delete! %process-fd-ht fd)
                  (call-foreign (__cs_evt_fd_handler_del fd))
               )
               (close-port (process-port-err proc))
            )
            (close-port (process-port-in proc))
            (when (process-on-exit proc)
               (when alive?
                  ((process-on-exit proc)
                      (process-status pid)
                      (process-buffer-out proc)
                      (process-buffer-err proc)
                  )
               )
               (unlock-object (process-cb proc))
            )
            (hashtable-delete! %process-pid-ht pid)
            (call-foreign (__cs_process_del pid))
         )
      )
   )
)
(add-hook 'process-exit-hook on-process-exit)
