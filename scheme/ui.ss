(define __cs_screen_width_get (foreign-procedure "cs_screen_width_get" () scheme-object))
(define __cs_screen_height_get (foreign-procedure "cs_screen_height_get" () scheme-object))
(define __cs_ui_init (foreign-procedure "cs_ui_init" (int) void))
(define __cs_ui_event_process (foreign-procedure "cs_ui_event_process" () void))
(define __cs_ui_refresh_screen (foreign-procedure "cs_ui_refresh_screen" () void))
(define __cs_ui_clear_screen (foreign-procedure "cs_ui_clear_screen" () void))
(define __cs_ui_draw_char (foreign-procedure "cs_ui_draw_char" (int int wchar int int int int) void))

(define (ui-init ui-type)
   (call-foreign (__cs_ui_init ui-type)))

(define (ui-process)
   (process-destroy-dead)
   (ui-process-event)
   (window-draw-all)
   (ui-update))

(define (ui-process-event)
   (call-foreign (__cs_ui_event_process)))

(define (ui-screen-width)
   (call-foreign (__cs_screen_width_get)))

(define (ui-screen-height)
   (call-foreign (__cs_screen_height_get)))

(define (ui-update)
   (call-foreign (__cs_ui_refresh_screen)))

(define (ui-clear)
   (call-foreign (__cs_ui_clear_screen)))

(define ui-draw-char
   (case-lambda
      [(x y ch)
       (ui-draw-char x y ch 1 "default" "default" "normal")]

      [(x y ch fg bg)
       (ui-draw-char x y ch 1 fg bg "normal")]

      [(x y ch n fg bg)
       (ui-draw-char x y ch n fg bg "normal")]

      [(x y ch n fg bg style)
       (let ([fg-num (color-name->number fg)]
             [bg-num (color-name->number bg)]
             [style-num (style-name->number style)])
          (call-foreign (__cs_ui_draw_char x y ch n fg-num bg-num style-num)))]))

(define ui-draw-text
   (case-lambda
      [(x y str)
       (ui-draw-text x y str "default" "default" "normal")]

      [(x y str fg bg)
       (ui-draw-text x y str fg bg "normal")]

      [(x y str fg bg style)
       (let ([fg-num (color-name->number fg)]
             [bg-num (color-name->number bg)]
             [style-num (style-name->number style)])
          (let loop ([x x] [chars (string->list str)])
             (when (not (null? chars))
                (call-foreign (__cs_ui_draw_char x y (first chars) 1 fg-num bg-num style-num))
                (loop (+ x 1) (cdr chars)))))]))
