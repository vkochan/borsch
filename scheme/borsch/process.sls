(library (borsch process)
   (export
      process-environment
      process-set-environment
      process-get-environment
      with-process-environment
      process-port-in
      process-port-out
      process-port-err
      process-pid
      process-pty
      process-buffer-out
      process-buffer-err
      process-is-alive?
      process-is-async?
      process-kill
      process-wait
      process-send-text
      process-status
      process-set-filter
      process-create
      process-create-plist
      make-process
      with-process-temp-buffer
      with-process-buffer
      process-get-output
      process-with-input
      process-with-input/output
      program-exists?
      process-initialize
      process-destroy-dead)
   (import
      (chezscheme)
      (borsch base)
      (borsch buffer)
      (borsch lists))

(define __cs_process_create (foreign-procedure "cs_process_create"
				(string string boolean boolean boolean scheme-object boolean boolean) scheme-object))

(define __cs_process_wait (foreign-procedure "cs_process_wait" (int) scheme-object))

(define __cs_process_is_alive (foreign-procedure "cs_process_is_alive" (int) scheme-object))

(define __cs_process_is_async (foreign-procedure "cs_process_is_async" (int) scheme-object))

(define __cs_process_status_get (foreign-procedure "cs_process_status_get" (int) scheme-object))

(define __cs_process_del (foreign-procedure "cs_process_del" (int) void))

(define __cs_process_kill (foreign-procedure "cs_process_kill" (int) void))

(define __cs_evt_fd_handler_add (foreign-procedure "cs_evt_fd_handler_add" (int void*) int))

(define __cs_evt_fd_handler_del (foreign-procedure "cs_evt_fd_handler_del" (int) void))

(define __cs_process_destroy_dead (foreign-procedure "cs_process_destroy_dead" () void))

(define __cs_process_filter_enable (foreign-procedure "cs_process_filter_enable" (int boolean) scheme-object))

(define __cs_process_text_send (foreign-procedure "cs_process_text_send" (int string) int))

(define (process-destroy-dead)
   (call-foreign (__cs_process_destroy_dead)))
   
(define __process-environment '())

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

(define-record-type process-entry
   (fields
      port-in
      port-out
      port-err
      pid
      pty
      buffer-out
      buffer-err
      on-exit
      (mutable port-reader)
      (mutable filter)
    )
)

(define %process-pid-ht (make-eq-hashtable))
(define %process-fd-ht (make-eq-hashtable))

(define (process-port-in proc)
   (process-entry-port-in proc))

(define (process-port-out proc)
   (process-entry-port-out proc))

(define (process-port-err proc)
   (process-entry-port-err proc))

(define (process-pid proc)
   (process-entry-pid proc))

(define (process-pty proc)
   (process-entry-pty proc))

(define (process-buffer-out proc)
   (process-entry-buffer-out proc))

(define (process-buffer-err proc)
   (process-entry-buffer-err proc))

(define (process-on-exit proc)
   (process-entry-on-exit proc))

(define (process-port-reader proc)
   (process-entry-port-reader proc))

(define (process-filter proc)
   (process-entry-filter proc))

(define (process-filter-set! proc filter)
   (process-entry-filter-set! proc filter))

(define process-is-alive?
   (lambda (pid)
      (call-foreign (__cs_process_is_alive pid))
   )
)

(define process-is-async?
   (lambda (proc)
      (call-foreign (__cs_process_is_async (process-pid proc)))
   )
)

(define process-kill
   (lambda (pid)
      (call-foreign (__cs_process_kill pid))
   )
)

(define (__process_append_buffer b s)
   (let ([curs (buffer-cursor b)])
      (buffer-set-cursor b (buffer-end-pos b))
      (buffer-insert-text b s)
      (buffer-set-cursor b curs) ))

(define __process-on-read-async
   (lambda (proc)
      (let (
            [proc-out (process-port-out proc)]
            [buf      (process-buffer-out proc)]
           )
         (let ([s (get-string-some proc-out)])
            (when (not (eof-object? s))
               (if (buffer-is-valid? buf)
                  (__process_append_buffer buf s)
                  ;; else
                  (process-kill (process-pid proc))
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
         (when (buffer-is-valid? buf)
            (let ([s (get-string-some proc-out)])
               (while (not (eof-object? s))
                  (__process_append_buffer buf s)
                  (set! s (get-string-some proc-out))
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

(define __make-process-reader
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

(define (process-send-text proc text)
   (call-foreign (__cs_process_text_send (process-pid proc) text)))

(define process-status
   (lambda (pid)
      (call-foreign (__cs_process_status_get pid))
   )
)

(define (process-set-filter proc func)
   (process-filter-set! proc func)
   (call-foreign (__cs_process_filter_enable (process-pid proc) (or func))))

(define process-create
   (case-lambda
      [(cmd)
       (process-create cmd #f #f)
      ]

      [(cmd buf-out)
       (process-create cmd buf-out #f)
      ]

      [(cmd buf-out on-exit)
       (process-create cmd buf-out #f on-exit)
      ]

      [(cmd buf-out buf-err on-exit)
       (process-create cmd buf-out buf-err on-exit #t)
      ]

      [(cmd buf-out buf-err on-exit async?)
       (process-create cmd buf-out buf-err on-exit #t #f)
      ]

      [(cmd buf-out buf-err on-exit async? pty?)
       (process-create cmd buf-out buf-err on-exit async? pty? #f)
      ]

      [(cmd buf-out buf-err on-exit async? pty? filter)
       (let*(
             [env (process-environment)]
             [p (call-foreign (__cs_process_create
                                 cmd
                                 (current-cwd)
                                 (not pty?)
                                 (if (or (not pty?) buf-out) #t #f)
                                 (not (not buf-err))
                                 env
                                 pty?
                                 async?))
             ]
            )
          (let (
                [reader (if (or buf-out buf-err) (__make-process-reader) #f)]
                [in-fd  (list-ref p 0)]
                [out-fd (list-ref p 1)]
                [err-fd (list-ref p 2)]
                [pid    (list-ref p 3)]
                [pty    (list-ref p 4)]
               )
             (when (and pty? filter)
                (call-foreign (__cs_process_filter_enable pid #t)))
             (let (
                   [port-in (if (eq? in-fd -1) #f (open-fd-output-port in-fd (buffer-mode block) (native-transcoder)))]
                   [port-out (if (eq? out-fd -1) #f (open-fd-input-port out-fd (buffer-mode block) (native-transcoder)))]
                   [port-err (if (eq? err-fd -1) #f (open-fd-input-port err-fd (buffer-mode block) (native-transcoder)))]
                  )
                (let ([proc (make-process-entry port-in port-out port-err pid pty buf-out buf-err on-exit reader filter)])
                   (hashtable-set! %process-pid-ht pid proc)
                   (when buf-out
                      (hashtable-set! %process-fd-ht out-fd proc)
                      (call-foreign (__cs_evt_fd_handler_add out-fd (foreign-callable-entry-point reader)))
                   )
                   (when buf-err
                      (hashtable-set! %process-fd-ht err-fd proc)
                      (call-foreign (__cs_evt_fd_handler_add err-fd (foreign-callable-entry-point reader)))
                   )
                   proc
                )
             )
          )
       )
      ]
   )
)

(define (process-create-plist plist)
   (let ([cmd     (plist-get plist 'cmd:)]
         [buf-out (plist-get plist 'stdout:)]
         [buf-err (plist-get plist 'stderr:)]
         [on-exit (plist-get plist 'on-exit:)]
         [async?  (plist-get plist 'async?: #t)]
         [pty?    (plist-get plist 'pty?: #t)]
         [filter    (plist-get plist 'filter:)])
      (process-create cmd buf-out buf-err on-exit async? pty? filter)))

(define-syntax make-process
   (lambda (x)
      (syntax-case x ()
         ((_)
          #'(make-process "sh"))
         ((_ cmd e* ...)
          (let loop ([ret (list)]
                     [e #'(e* ...)])
             (syntax-case e ()
                (() #`(process-create-plist (list #,@(append (list #''cmd: #'cmd) ret))))

                ((k v kv* ...)
                 (and (identifier? #'k)
                      (member (syntax->datum #'k) '(stdout: stderr: on-exit: async?: pty?: filter:)))
                 (loop (append ret (list #''k #'v)) #'(kv* ...)))))))))

(define-syntax (with-process-temp-buffer stx)
   (syntax-case stx ()
      ((_ cmd exp ...)
       #`(let ([b (make-buffer)])
            (process-create cmd b
               (lambda (proc-status buf-out buf-err)
                  (with-current-buffer buf-out
                     (begin
                        exp
		        ...
                     )
                  )
                  (delete-buffer buf-out)
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
            [proc (process-create cmd #f #f #f #f)]
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
            [proc (process-create cmd #f #f #f #f)]
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
            [proc (process-create cmd #f #f #f #f)]
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

(define process-exit-handler
   (lambda (pid)
      (let (
            [proc (hashtable-ref %process-pid-ht pid #f)]
           )
         (when proc
            (when (process-buffer-out proc)
               (__process-on-read-sync (process-port-out proc) (process-buffer-out proc))
               (let ([fd (port-file-descriptor (process-port-out proc))])
                  (hashtable-delete! %process-fd-ht fd)
                  (call-foreign (__cs_evt_fd_handler_del fd))
               )
               (close-port (process-port-out proc))
            )
            (when (process-buffer-err proc)
               (__process-on-read-sync (process-port-err proc) (process-buffer-err proc))
               (let ([fd (port-file-descriptor (process-port-err proc))])
                  (hashtable-delete! %process-fd-ht fd)
                  (call-foreign (__cs_evt_fd_handler_del fd))
               )
               (close-port (process-port-err proc))
            )
            (when (process-port-in proc)
               (close-port (process-port-in proc))
            )
            (when (process-on-exit proc)
               (try
                  ((process-on-exit proc)
                     (process-status pid)
                     (process-buffer-out proc)
                     (process-buffer-err proc)))
            )
            (when (process-port-reader proc)
               (unlock-object (process-port-reader proc))
            )
            (when (process-is-async? proc)
               (hashtable-delete! %process-pid-ht pid)
               (call-foreign (__cs_process_del pid))
            )
         )
      )
   )
)

(define (process-filter-handler pid str)
   (let ([proc (hashtable-ref %process-pid-ht pid #f)])
      (when proc
         (when (process-filter proc)
            (try
               ((process-filter proc)
                  proc
                  str)
            )))))

(define (process-initialize)
   (add-hook 'process-exit-hook process-exit-handler)
   (add-hook 'process-filter-hook process-filter-handler))

)
