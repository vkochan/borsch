(define __cs_screen_width_get (foreign-procedure __collect_safe "cs_screen_width_get" () scheme-object))
(define __cs_screen_height_get (foreign-procedure __collect_safe "cs_screen_height_get" () scheme-object))

(define (ui-screen-width)
  (call-foreign (__cs_screen_width_get)))

(define (ui-screen-height)
  (call-foreign (__cs_screen_height_get)))
