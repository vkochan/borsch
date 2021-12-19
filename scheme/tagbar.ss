(define __cs_tagbar_status_align (foreign-procedure __collect_safe "cs_tagbar_status_align" (int) int))
(define __cs_tagbar_show (foreign-procedure __collect_safe "cs_tagbar_show" (boolean) int))
(define __cs_tagbar_status_set (foreign-procedure "cs_tagbar_status_set" (string) int))

(define tagbar-set-status-align
   (lambda (a)
      (let ([v (case a
                  ['right 0]
                  ['left  1]
               )
            ]
           )
           (__cs_tagbar_status_align v)
      )
   )
)

(define tagbar-show
   (lambda (s)
      (__cs_tagbar_show s)
   )
)

(define tagbar-set-status
   (lambda (s)
      (__cs_tagbar_status_set s)
   )
)
