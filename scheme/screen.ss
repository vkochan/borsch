(define __cs_screen_width_get (foreign-procedure __collect_safe "cs_screen_width_get" () scheme-object))
(define __cs_screen_height_get (foreign-procedure __collect_safe "cs_screen_height_get" () scheme-object))

(define (screen-width)
  (call-foreign (__cs_screen_width_get)))

(define (screen-height)
  (call-foreign (__cs_screen_height_get)))
