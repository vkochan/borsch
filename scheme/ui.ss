(define __cs_screen_width_get (foreign-procedure __collect_safe "cs_screen_width_get" () scheme-object))
(define __cs_screen_height_get (foreign-procedure __collect_safe "cs_screen_height_get" () scheme-object))
(define __cs_ui_init (foreign-procedure "cs_ui_init" (int) void))
(define __cs_ui_process (foreign-procedure "cs_ui_process" () void))

(define (ui-init ui-type)
   (call-foreign (__cs_ui_init ui-type)))

(define (ui-process)
   (call-foreign (__cs_ui_process)))

(define (ui-screen-width)
   (call-foreign (__cs_screen_width_get)))

(define (ui-screen-height)
   (call-foreign (__cs_screen_height_get)))
