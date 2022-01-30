(define __cs_evt_fd_handler_add (foreign-procedure __collect_safe "cs_evt_fd_handler_add" (int void*) int))
(define __cs_evt_fd_handler_del (foreign-procedure __collect_safe "cs_evt_fd_handler_del" (int) void))
(define __cs_process_is_alive (foreign-procedure __collect_safe "cs_process_is_alive" (int) scheme-object))

(define __process-fd-entry-delete!
   (lambda (p)
      (let (
            [stdin (%process-stdin p)]
            [stdout (%process-stdout p)]
            [stderr (%process-stderr p)]
            [fd (%process-fd p)]
            [cb (%process-cb p)]
           )
         (hashtable-delete! %process-fd-ht fd)
         (__cs_evt_fd_handler_del fd)
         (close-port stdout)
         (close-port stderr)
         (close-port stdin)
         (unlock-object cb)
      )
   )
)

(define __process-fd-handle
   (lambda (fd)
      (let ([p (hashtable-ref %process-fd-ht fd #f)])
         (when p
            (let (
                  [stdout (%process-stdout p)]
                  [stderr (%process-stderr p)]
                  [stdin   (%process-stdin p)]
                  [buf       (%process-buf p)]
                  [cb        (%process-cb p)]
                 )
               (let ([s (get-string-some stdout)])
                  (when (not (eof-object? s))
                     (with-buffer buf
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

(define-record-type %process (fields stdin stdout stderr pid fd buf cb))

(define %process-fd-ht (make-eq-hashtable))

(define process-start
   (case-lambda
      [(buf cmd)
       (let-values (
                    [(stdin stdout stderr pid)
                       (open-process-ports cmd (buffer-mode block) (native-transcoder))]
                   )
         (set-port-nonblocking! stdout #t)

         (let* (
                [cb (__process-fd-cb)]
                [fd (port-file-descriptor stdout)]
                [p (make-%process stdin stdout stderr pid fd buf cb)]
               )
            (__cs_evt_fd_handler_add fd (foreign-callable-entry-point cb))
            (hashtable-set! %process-fd-ht fd p)
         )
       )
      ]
   )
)

(add-hook 'idle-hook
   (lambda ()
      (let (
            [proc-list (hashtable-values %process-fd-ht)]
            [to-delete '()]
           )
         (vector-for-each
            (lambda (p)
               (when (and
                        (not (__cs_process_is_alive (%process-pid p)))
                        (port-eof? (%process-stdout p))
                     )
                  (set! to-delete (append to-delete (list p)))
               )
            ) proc-list
         )
         (for-each
            (lambda (p)
               (__process-fd-entry-delete! p)
            ) to-delete
         )
      )
   )
)
