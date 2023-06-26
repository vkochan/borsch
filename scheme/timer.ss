(define __cs_timer_add (foreign-procedure "cs_timer_add" (void*) scheme-object))
(define __cs_timer_del (foreign-procedure "cs_timer_del" (int) void))
(define __cs_timer_interval_set (foreign-procedure "cs_timer_interval_set" (int long) void))
(define __cs_timer_time_set (foreign-procedure "cs_timer_interval_set" (int long long) void))

(define %timer-fd-ht (make-eq-hashtable))

(define timer-cb
   (lambda (p)
      (let ([code (foreign-callable (lambda (obj) (try (p) )) (void*) void)])
	 (lock-object code)
	 code
      )
   )
)

(define date->time
   (lambda (dt-or-tm)
      (if (date? dt-or-tm)
         (let ([dt dt-or-tm])
            (date->time-utc dt)
         )
         ;; else
         (let ([tm dt-or-tm])
            tm
         )
      )
   )
)

(define timer-set-time
   (lambda (tid dt-or-tm)
      (let ([tm (date->time dt-or-tm)])
         (let (
               [nsec (time-nanosecond tm)]
               [sec (time-second tm)]
              )
            (call-foreign (__cs_timer_time_set tid sec nsec))
         )
      )
   )
)

(define timer-set-interval
   (lambda (tid ms)
      (call-foreign (__cs_timer_interval_set tid ms))
   )
)

(define make-timer
   (lambda (dt-or-tm fn)
      (let (
            [tm (date->time dt-or-tm)]
            [cb (timer-cb fn)]
           )
         (let (
               [tid (call-foreign (__cs_timer_add (foreign-callable-entry-point cb)))]
              )
            (if tid
               (begin
                  (hashtable-set! %timer-fd-ht tid cb)
                  (if (time? tm)
                     (timer-set-time tid tm)
                     ;; else
                     (timer-set-interval tid tm)
                  )
                  tid
               )
               ;; else
               #f
            )
         )
      )
   )
)

(define timer-delete
   (lambda (id)
      (let ([cb (hashtable-ref %timer-fd-ht id #f)])
         (when cb
            (call-foreign (__cs_timer_del id))
            (unlock-object cb)
         )
      )
   )
)
