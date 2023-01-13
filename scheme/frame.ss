(define __cs_frame_current_get (foreign-procedure __collect_safe "cs_frame_current_get" () int))
(define __cs_frame_current_set (foreign-procedure __collect_safe "cs_frame_current_set" (int) int))
(define __cs_frame_name_get (foreign-procedure __collect_safe "cs_frame_name_get" (int) scheme-object))
(define __cs_frame_name_set (foreign-procedure "cs_frame_name_set" (int string) int))
(define __cs_frame_cwd_get (foreign-procedure __collect_safe "cs_frame_cwd_get" (int) scheme-object))
(define __cs_frame_cwd_set (foreign-procedure "cs_frame_cwd_set" (int string) int))

(define current-frame
   (lambda ()
      (call-foreign (__cs_frame_current_get))
   )
)

(define-syntax (with-current-frame stx)
   (syntax-case stx ()
      ((_ frame exp ...)
       #`(let (
               [from (current-frame)]
               [to frame]
              )
            (frame-switch to)
            (begin
               exp
               ...
            )
            (frame-switch from)
         )
      )
   )
)

(define frame-switch
   (lambda (tag)
      (call-foreign (__cs_frame_current_set tag))
      (run-hooks 'frame-switch-hook tag)
   )
)

(define frame-switch-1
   (lambda ()
      (frame-switch 1)
   )
)

(define frame-switch-2
   (lambda ()
      (frame-switch 2)
   )
)

(define frame-switch-3
   (lambda ()
      (frame-switch 3)
   )
)

(define frame-switch-4
   (lambda ()
      (frame-switch 4)
   )
)

(define frame-switch-5
   (lambda ()
      (frame-switch 5)
   )
)

(define frame-switch-6
   (lambda ()
      (frame-switch 6)
   )
)

(define frame-switch-7
   (lambda ()
      (frame-switch 7)
   )
)

(define frame-switch-8
   (lambda ()
      (frame-switch 8)
   )
)

(define frame-switch-9
   (lambda ()
      (frame-switch 9)
   )
)

(define frame-switch-all
   (lambda ()
      (frame-switch 0)
   )
)

(define frame-name
   (case-lambda
      [()
       (call-foreign (__cs_frame_name_get (current-frame)))]

      [(tag)
       (call-foreign (__cs_frame_name_get tag))]
   )
)

(define frame-set-name
   (case-lambda
      [(name)
       (frame-set-name (current-frame) name)
      ]

      [(tag name)
       (call-foreign (__cs_frame_name_set tag name))
       (run-hooks 'change-frame-name-hook)
      ]
   )
)

(define frame-cwd
   (case-lambda
      [()
       (call-foreign (__cs_frame_cwd_get (current-frame)))]

      [(tag)
       (call-foreign (__cs_frame_cwd_get tag))]
   )
)

(define frame-set-cwd
   (case-lambda
      [(cwd)
       (frame-set-cwd (current-frame) cwd)]

      [(tag cwd)
       (call-foreign (__cs_frame_cwd_set tag cwd))
       (run-hooks 'change-cwd-hook)
      ]
   )
)
