(define __cs_view_current_get (foreign-procedure __collect_safe "cs_view_current_get" () int))
(define __cs_view_current_set (foreign-procedure __collect_safe "cs_view_current_set" (int) int))
(define __cs_view_name_get (foreign-procedure __collect_safe "cs_view_name_get" (int) scheme-object))
(define __cs_view_name_set (foreign-procedure "cs_view_name_set" (int string) int))
(define __cs_view_cwd_get (foreign-procedure __collect_safe "cs_view_cwd_get" (int) scheme-object))
(define __cs_view_cwd_set (foreign-procedure "cs_view_cwd_set" (int string) int))

(define current-view
   (lambda ()
      (call-foreign (__cs_view_current_get))
   )
)

(define view-switch
   (lambda (tag)
      (call-foreign (__cs_view_current_set tag))
      (run-hooks 'view-switch-hook tag)
   )
)

(define view-switch-1
   (lambda ()
      (view-switch 1)
   )
)

(define view-switch-2
   (lambda ()
      (view-switch 2)
   )
)

(define view-switch-3
   (lambda ()
      (view-switch 3)
   )
)

(define view-switch-4
   (lambda ()
      (view-switch 4)
   )
)

(define view-switch-5
   (lambda ()
      (view-switch 5)
   )
)

(define view-switch-6
   (lambda ()
      (view-switch 6)
   )
)

(define view-switch-7
   (lambda ()
      (view-switch 7)
   )
)

(define view-switch-8
   (lambda ()
      (view-switch 8)
   )
)

(define view-switch-9
   (lambda ()
      (view-switch 9)
   )
)

(define view-switch-all
   (lambda ()
      (view-switch 0)
   )
)

(define view-name
   (case-lambda
      [()
       (call-foreign (__cs_view_name_get (current-view)))]

      [(tag)
       (call-foreign (__cs_view_name_get tag))]
   )
)

(define view-set-name
   (case-lambda
      [(name)
       (call-foreign (__cs_view_name_set (current-view) name))]

      [(tag name)
       (call-foreign (__cs_view_name_set tag name))]
   )
)

(define view-cwd
   (case-lambda
      [()
       (call-foreign (__cs_view_cwd_get (current-view)))]

      [(tag)
       (call-foreign (__cs_view_cwd_get tag))]
   )
)

(define view-set-cwd
   (case-lambda
      [(cwd)
       (view-set-cwd (current-view) cwd)]

      [(tag cwd)
       (call-foreign (__cs_view_cwd_set tag cwd))
       (run-hooks 'change-cwd-hook)
      ]
   )
)
