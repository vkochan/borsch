(define __cs_win_is_visible (foreign-procedure __collect_safe "cs_win_is_visible" (int) boolean))
(define __cs_win_first_get (foreign-procedure __collect_safe "cs_win_first_get" (int) scheme-object))
(define __cs_win_prev_get (foreign-procedure __collect_safe "cs_win_prev_get" (int) scheme-object))
(define __cs_win_next_get (foreign-procedure __collect_safe "cs_win_next_get" (int) scheme-object))
(define __cs_win_first_set (foreign-procedure __collect_safe "cs_win_first_set" (int) scheme-object))
(define __cs_win_prev_set (foreign-procedure __collect_safe "cs_win_prev_set" (int int) scheme-object))
(define __cs_win_next_set (foreign-procedure __collect_safe "cs_win_next_set" (int int) scheme-object))
(define __cs_win_current_get (foreign-procedure __collect_safe "cs_win_current_get" () scheme-object))
(define __cs_win_current_set (foreign-procedure __collect_safe "cs_win_current_set" (int) int))
(define __cs_win_new (foreign-procedure "cs_win_new" (int) scheme-object))
(define __cs_win_del (foreign-procedure __collect_safe "cs_win_del" (int) int))
(define __cs_win_title_get (foreign-procedure __collect_safe "cs_win_title_get" (int) scheme-object))
(define __cs_win_title_set (foreign-procedure "cs_win_title_set" (int string) int))
(define __cs_win_buf_get (foreign-procedure __collect_safe "cs_win_buf_get" (int) scheme-object))
(define __cs_win_width_get (foreign-procedure __collect_safe "cs_win_width_get" (int) scheme-object))
(define __cs_win_height_get (foreign-procedure __collect_safe "cs_win_height_get" (int) scheme-object))
(define __cs_win_viewport_width_get (foreign-procedure __collect_safe "cs_win_viewport_width_get" (int) scheme-object))
(define __cs_win_viewport_height_get (foreign-procedure __collect_safe "cs_win_viewport_height_get" (int) scheme-object))
(define __cs_win_size_set (foreign-procedure __collect_safe "cs_win_size_set" (int int int) void))
(define __cs_win_border_set (foreign-procedure __collect_safe "cs_win_border_set" (int boolean) void))
(define __cs_win_buf_switch (foreign-procedure __collect_safe "cs_win_buf_switch" (int int) void))
(define __cs_win_prev_selected (foreign-procedure __collect_safe "cs_win_prev_selected" () scheme-object))
(define __cs_win_viewport_pos (foreign-procedure __collect_safe "cs_win_viewport_pos" (int char) scheme-object))
(define __cs_win_viewport_coord (foreign-procedure __collect_safe "cs_win_viewport_coord" (int int) scheme-object))
(define __cs_win_scroll (foreign-procedure __collect_safe "cs_win_scroll" (int char int) scheme-object))
(define __cs_win_sidebar_set (foreign-procedure __collect_safe "cs_win_sidebar_set" (int int) void))
(define __cs_win_sidebar_get (foreign-procedure __collect_safe "cs_win_sidebar_get" (int) scheme-object))
(define __cs_win_sidebar_draw (foreign-procedure "cs_win_sidebar_draw" (int int int string int int int) scheme-object))
(define __cs_win_update (foreign-procedure "cs_win_update" (int) void))
(define __cs_widget_create (foreign-procedure "cs_widget_create" (string int int int int int) scheme-object))
(define __cs_win_coord_get (foreign-procedure __collect_safe "cs_win_coord_get" (int) scheme-object))
(define __cs_win_draw_all (foreign-procedure __collect_safe "cs_win_draw_all" (boolean) void))
(define __cs_win_layout_is_changed (foreign-procedure __collect_safe "cs_win_layout_is_changed" () boolean))
(define __cs_win_update_layout (foreign-procedure __collect_safe "cs_win_update_layout" () void))

(define %widget-list% (list))

(define (window-layout-is-changed)
   (call-foreign (__cs_win_layout_is_changed)))

(define (window-update-layout)
   (call-foreign (__cs_win_update_layout)))

(define (window-draw-all)
   (let ([enforce (window-layout-is-changed)])
      (when enforce
         (window-update-layout))
      (call-foreign (__cs_win_draw_all enforce))))

(define window-is-visible?
   (case-lambda
      [()
       (window-is-visible? (current-window))]

      [(wid)
       (call-foreign (__cs_win_is_visible wid))]))

(define (window-last-master)
   (let ([nm (layout-n-master)]
         [m #f])
      (window-for-all
         (lambda (w)
            (and (> nm 0)
                 (begin
                    (set! nm (- nm 1))
                    (set! m w)
                    #t))))
      m))

(define window-first
   (case-lambda
     [()
      (window-first (current-frame))]

     [(fr)
      (call-foreign (__cs_win_first_get (frame-id fr)))]))

(define window-next
   (case-lambda
      [()
       (window-next (current-window))]

      [(wid)
       (call-foreign (__cs_win_next_get wid))]))

(define window-prev
   (case-lambda
      [()
       (window-prev (current-window))]

      [(wid)
       (call-foreign (__cs_win_prev_get wid))]))

(define window-set-first
   (case-lambda
      [()
       (window-set-first (current-window))]

      [(wid)
       (call-foreign (__cs_win_first_set wid))]))

(define window-set-prev
   (case-lambda
      [(prev)
       (window-set-prev (current-window) prev)]

      [(wid prev)
       (call-foreign (__cs_win_prev_set wid prev))]))

(define window-set-next
   (case-lambda
      [(next)
       (window-set-next (current-window) next)]

      [(wid next)
       (call-foreign (__cs_win_next_set wid next))]))

(define (%window-list% fr)
   (let ([win (window-first fr)]
         [lst   '()])
      (while win
         (set! lst (append lst (list win)))
         (set! win (window-next win)))
      lst))

(define window-list
   (case-lambda
     [()
      (window-list (current-frame))
     ]

     [(fr)
      (append (%window-list% fr) %widget-list%)]))

(define (window-last)
   (list-ref (window-list) (- (length (window-list)) 1)))

(define (window-for-each fn)
   (let ([ls (window-list)])
      (for-each
         (lambda (w)
            (fn w))
         ls)))

(define (window-for-all fn)
   (let ([ls (window-list)])
      (for-all
         (lambda (w)
            (fn w))
         ls)))

(define window-upper
   (case-lambda
      [()
       (window-upper (current-window))]

      [(wid)
       (window-by-pos (+ 1 (window-x wid))
                      (- (window-y wid) 1))]))

(define window-lower
   (case-lambda
      [()
       (window-lower (current-window))]

      [(wid)
       (window-by-pos (window-x wid)
                      (+ (window-y wid)
                         (window-height wid)))]))

(define window-right
   (case-lambda
      [()
       (window-right (current-window))]

      [(wid)
       (window-by-pos (+ (window-x wid) (window-width wid) 1)
                      (window-y wid))]))

(define window-left
   (case-lambda
      [()
       (window-left (current-window))]

      [(wid)
       (window-by-pos (- (window-x) 2)
                      (window-y wid))]))

(define (current-window)
   (call-foreign (__cs_win_current_get)))

(define (window-prev-selected)
    (call-foreign (__cs_win_prev_selected)))

(define (window-select wid)
   (when wid
      (call-foreign (__cs_win_current_set wid))
      (run-hooks 'window-select-hook wid)))

(define (window-select-left)
   (window-select (window-left)))

(define (window-select-right)
   (window-select (window-right)))

(define (window-select-upper)
   (window-select (or (window-upper)
                      (window-prev)
                      (window-last))))

(define (window-select-lower)
   (window-select (or (window-lower)
                      (window-next)
                      (window-first))))

(define (window-create b)
   (let ([w (call-foreign (__cs_win_new b))])
      (when w
         (buffer-ref-get b)
         (if (layout-is-sticky?)
            (let ([m (window-last-master)])
               (when m (window-set-next m w)))
            ;; else
            (let ()
               (window-set-first w)))
         (run-hooks 'window-create-hook w)
         (window-select w))
      w))

(define (%window-delete% w)
   (call-foreign (__cs_win_del w)))

(define window-delete
    (case-lambda
       [()
	(window-delete (current-window))]

       [(w)
        (let ([n (buffer-name (window-buffer w))])
           (let ([b (window-buffer w)])
              (%window-delete% w)
              (buffer-ref-put b)
	      (when (>= 1 (buffer-ref-count b))
                 (buffer-ref-put b))
              (run-hooks 'window-delete-hook w)))]))

(define window-close
    (case-lambda
       [()
	(window-close (current-window))]

       [(w)
        (buffer-ref-put (window-buffer))
        (%window-delete% w)]))

(define window-name
   (case-lambda
      [()
       (window-name (current-window))]

      [(wid)
       (call-foreign (__cs_win_title_get wid))]))

(define window-set-name
   (case-lambda
      [(title)
       (window-set-name (current-window) title)]

      [(wid title)
       (call-foreign (__cs_win_title_set wid title))]))

(define window-is-maximized?
   (case-lambda
      [()
       (window-is-maximized? (current-window))]

      [(wid)
       (and (equal? (current-window) wid)
            (layout-is-maximized?))]))

(define window-is-master?
   (case-lambda
      [()
       (window-is-master? (current-window))]

      [(wid)
       (let ([nm (layout-n-master)]
             [m? #f])
          (window-for-all
             (lambda (w)
                (and (> nm 0)
                     (not
                        (let ([w-eq? (equal? w wid)])
                           (set! m? w-eq?)
                           m?))
                   (begin
                      (set! nm (- nm 1))
                      (set! m w)
                      #t))))
          m?)]))

(define (symb->win-state sym)
   (case sym
      ['maximized 1]
      ['master    2]))

(define window-set-master
   (case-lambda
      [()
       (window-set-master (current-window))]

      [(wid)
       (window-set-first wid)]))

(define window-is-sticky?
   (case-lambda
      [()
       (window-is-sticky? (current-window))]

      [(wid)
       (and (window-is-master? wid) (layout-is-sticky?))]))

(define window-buffer
   (case-lambda
      [()
       (window-buffer (current-window))]

      [(wid)
       (call-foreign (__cs_win_buf_get wid))]))

(define window-width
   (case-lambda
      [()
       (window-width (current-window))]

      [(wid)
       (call-foreign (__cs_win_width_get wid))]))

(define window-height
   (case-lambda
      [()
       (window-height (current-window))]

      [(wid)
       (call-foreign (__cs_win_height_get wid))]))

(define window-set-width
   (case-lambda
      [(w)
       (window-set-width (current-window) w)]

      [(wid w)
       (call-foreign (__cs_win_size_set wid w -1))]))

(define window-set-height
   (case-lambda
      [(h)
       (window-set-height (current-window) h)]

      [(wid h)
       (call-foreign (__cs_win_size_set wid -1 h))]))

(define window-set-border
   (case-lambda
      [(h)
       (window-set-border (current-window) h)]

      [(wid h)
       (call-foreign (__cs_win_border_set wid -1 h))]))

(define window-switch-buffer
   (case-lambda
      [(b)
       (window-switch-buffer (current-window) b)]

      [(wid b)
       (buffer-ref-put (window-buffer wid))
       (call-foreign (__cs_win_buf_switch wid b))
       (buffer-ref-get (window-buffer wid))]))

(define window-begin-pos
   (case-lambda
      [()
       (window-begin-pos (current-window))]

      [(w)
       (call-foreign (__cs_win_viewport_pos w #\H))]))

(define window-end-pos
   (case-lambda
      [()
       (window-end-pos (current-window))]

      [(w)
       (call-foreign (__cs_win_viewport_pos w #\L))]))

(define window-pos->coord
   (case-lambda
      [(p)
       (window-pos->coord (current-window) p)]

      [(w p)
       (call-foreign (__cs_win_viewport_coord w p))]))

(define (window-lines-coord w)
   (when (not (buffer-is-vterm? (window-buffer w)))
      (let ([b (window-buffer w)])
         (let* ([start (window-begin-pos w)]
                [end (text-line-begin-pos b (window-end-pos w))]
                [coord (window-pos->coord w start)]
                [lst '()])
            (while (and (<= start end) (not (= start (text-end-pos))))
               (when coord
                  (let ([line-x (list-ref coord 0)]
                        [line-y (list-ref coord 1)]
                        [line-n (list-ref coord 2)])
                     (set! lst (append lst (list (list line-x line-y line-n start))))))
               (set! start (text-next-line-pos b start))
               (set! coord (window-pos->coord w start)))
            lst))))

(define window-inner-width
   (case-lambda
      [()
       (window-inner-width (current-window))]

      [(wid)
       (call-foreign (__cs_win_viewport_width_get wid))]))

(define window-inner-height
   (case-lambda
      [()
       (window-inner-height (current-window))]

      [(wid)
       (call-foreign (__cs_win_viewport_height_get wid))]))

(define window-scroll-page-down
   (case-lambda
      [()
       (window-scroll-page-down (current-window))]

      [(w)
       (call-foreign (__cs_win_scroll w #\f 0))]))

(define window-scroll-page-up
   (case-lambda
      [()
       (window-scroll-page-up (current-window))]

      [(w)
       (call-foreign (__cs_win_scroll w #\b 0))]))

(define window-scroll-halfpage-down
   (case-lambda
      [()
       (window-scroll-halfpage-down (current-window))]

      [(w)
       (call-foreign (__cs_win_scroll w #\d 0))]))

(define window-scroll-halfpage-up
   (case-lambda
      [()
       (window-scroll-halfpage-up (current-window))]

      [(w)
       (call-foreign (__cs_win_scroll w #\u 0))]))

(define window-scroll-down
   (case-lambda
      [(n)
       (window-scroll-down (current-window))]

      [(w n)
       (call-foreign (__cs_win_scroll w #\l n))]))

(define window-scroll-up
   (case-lambda
      [(n)
       (window-scroll-down (current-window))]

      [(w n)
       (call-foreign (__cs_win_scroll w #\L n))]))

(define window-set-sidebar-width
   (case-lambda
      [(l)
       (window-set-sidebar-width (current-window) l)]

      [(w l)
       (call-foreign (__cs_win_sidebar_set w l))]))

(define window-sidebar-width
   (case-lambda
      [()
       (window-sidebar-width (current-window))]

      [(w)
       (call-foreign (__cs_win_sidebar_get w))]))

(define window-draw-sidebar
   (case-lambda
      [(w x y t)
       (window-draw-sidebar w x y t '(fg: "white" bg: "bright-blue"))]

      [(w x y t s)
       (let ([l (style->list  s)])
         (call-foreign (__cs_win_sidebar_draw w x y t
                                (list-ref l 0)
                                (list-ref l 1)
                                (list-ref l 2))))]))

(define window-update
    (case-lambda 
       [()
        (window-update (current-window))]

       [(w)
        (call-foreign (__cs_win_update w))]))

(define (widget-create name x y w h type)
   (let ([wtype (cond [(eq? type 'top) 1]
                      [(eq? type 'bottom) 2])])
      (let ([wid (call-foreign (__cs_widget_create name x y w h wtype))])
         (when (> wid 0)
            (set! %widget-list% (append %widget-list% (list wid))))
         wid)))

(define window-x
   (case-lambda
      [()
       (window-x (current-window))]

      [(wid)
       (let ([pos (call-foreign (__cs_win_coord_get wid))])
          (car pos))]))

(define window-y
   (case-lambda
      [()
       (window-y (current-window))]

      [(wid)
       (let ([pos (call-foreign (__cs_win_coord_get wid))])
          (cdr pos))]))

(define (window-find fn)
   (find
      (lambda (w)
         (fn w))
      (window-list)))

(define (window-by-pos x y)
   (window-find
      (lambda (w)
         (let ([wx (window-x w)]
               [wy (window-y w)]
               [ww (window-width w)]
               [wh (window-height w)])
            (and (and (>= x wx) (< x (+ wx ww)))
                 (and (>= y wy) (< y (+ wy wh))))))))
