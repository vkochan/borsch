(define __cs_win_first_get (foreign-procedure "cs_win_first_get" (int) scheme-object))
(define __cs_win_prev_get (foreign-procedure "cs_win_prev_get" (int) scheme-object))
(define __cs_win_next_get (foreign-procedure "cs_win_next_get" (int) scheme-object))
(define __cs_win_first_set (foreign-procedure "cs_win_first_set" (int) scheme-object))
(define __cs_win_prev_set (foreign-procedure "cs_win_prev_set" (int int) scheme-object))
(define __cs_win_next_set (foreign-procedure "cs_win_next_set" (int int) scheme-object))
(define __cs_win_current_set (foreign-procedure "cs_win_current_set" (int) int))
(define __cs_win_new (foreign-procedure "cs_win_new" (int boolean) scheme-object))
(define __cs_win_del (foreign-procedure "cs_win_del" (int) int))
(define __cs_win_title_get (foreign-procedure "cs_win_title_get" (int) scheme-object))
(define __cs_win_title_set (foreign-procedure "cs_win_title_set" (int string) int))
(define __cs_win_buf_get (foreign-procedure "cs_win_buf_get" (int) scheme-object))
(define __cs_win_width_get (foreign-procedure "cs_win_width_get" (int) scheme-object))
(define __cs_win_height_get (foreign-procedure "cs_win_height_get" (int) scheme-object))
(define __cs_win_viewport_width_get (foreign-procedure "cs_win_viewport_width_get" (int) scheme-object))
(define __cs_win_viewport_height_get (foreign-procedure "cs_win_viewport_height_get" (int) scheme-object))
(define __cs_win_size_set (foreign-procedure "cs_win_size_set" (int int int) void))
(define __cs_win_move (foreign-procedure "cs_win_move" (int int int) void))
(define __cs_win_border_set (foreign-procedure "cs_win_border_set" (int boolean) void))
(define __cs_win_buf_switch (foreign-procedure "cs_win_buf_switch" (int int) void))
(define __cs_win_viewport_pos (foreign-procedure "cs_win_viewport_pos" (int char) scheme-object))
(define __cs_win_viewport_coord (foreign-procedure "cs_win_viewport_coord" (int int) scheme-object))
(define __cs_win_viewport_cell_set (foreign-procedure "cs_win_viewport_cell_set" (int int int int int int wchar int) void))
(define __cs_win_scroll (foreign-procedure "cs_win_scroll" (int char int) scheme-object))
(define __cs_win_sidebar_set (foreign-procedure "cs_win_sidebar_set" (int int) void))
(define __cs_win_sidebar_get (foreign-procedure "cs_win_sidebar_get" (int) scheme-object))
(define __cs_win_update (foreign-procedure "cs_win_update" (int) scheme-object))
(define __cs_win_update_cursor (foreign-procedure "cs_win_update_cursor" (int) void))
(define __cs_win_coord_get (foreign-procedure "cs_win_coord_get" (int) scheme-object))
(define __cs_win_draw (foreign-procedure "cs_win_draw" (int boolean) void))
(define __cs_win_has_title (foreign-procedure "cs_win_has_title" (int) boolean))

(define %window-layout-changed% #f)

(define %widget-list-top% (list))
(define %widget-list-bottom% (list))
(define %widget-list% (list))

(define (widget-list)
   %widget-list%)

(define (window-layout-is-changed)
   (or %window-layout-changed%
       (ui-size-changed)))

(define (window-layout-set-changed changed?)
   (set! %window-layout-changed% changed?))

(define (window-update-layout-size)
   (let ([waw 0]
         [wah 0]
         [wax 0]
         [way 0]
         [top_h 0]
         [bot_h 0])
      (for-each
         (lambda (w)
            (cond
               [(widget-is-top? w)
                (set! top_h (+ top_h
                               (window-height w)))]
               [(widget-is-bottom? w)
                (set! bot_h (+ bot_h
                               (window-height w)))]))
         (widget-list))
      (set! wah (- (ui-screen-height)
                   bot_h))
      (set! waw (ui-screen-width))
      (set! wah (- wah
                   top_h))
      (set! way (+ way
                   top_h))
      (layout-set-x wax)
      (layout-set-y way)
      (layout-set-width waw)
      (layout-set-height wah)
      (for-each
         (lambda (w)
            (window-set-width w (ui-screen-width))
            (when (widget-is-bottom? w)
               (window-move w 0 (- (ui-screen-height)
                                   bot_h))))
         (widget-list)))
   (layout-arrange)
   (window-layout-set-changed #f))

(define window-draw-char
   (case-lambda
      [(w x y ch)
       (window-draw-char w x y ch 1 '(fg: "default" bg: "default" attr: "normal"))]

      [(w x y ch n)
       (window-draw-char w x y ch n '(fg: "default" bg: "default" attr: "normal"))]

      [(w x y ch n opts)
       (ui-draw-char (+ (window-x w) x) (+ (window-y w) y) ch n opts)]))

(define window-draw-text
   (case-lambda
      [(w x y str)
       (window-draw-text w x y str '(fg: "default" bg: "default" attr: "normal"))]

      [(w x y str opts)
       (ui-draw-text (+ (window-x w) x) (+ (window-y w) y) str opts)]))

(define window-has-title?
   (case-lambda
      [()
       (window-has-title? (current-window))]

      [(wid)
       (call-foreign (__cs_win_has_title wid))]))

(define window-is-dirty?
   (case-lambda
      [()
       (window-is-dirty? (current-window))]

      [(w)
       (buffer-is-dirty? (window-buffer w))]))

(define window-set-text-style
   (case-lambda
      [(start end style)
       (window-set-text-style (current-window) style)]

      [(wid start end style)
       (let ([ls-style (style->list style)])
          (call-foreign (__cs_win_viewport_cell_set wid start end
                                                    (list-ref ls-style 0)
                                                    (list-ref ls-style 1)
                                                    (list-ref ls-style 2)
                                                    (list-ref ls-style 3)
                                                    (list-ref ls-style 4))))]))

(define (window-draw-title w)
   (define (cursor-row/col w)
      (with-current-buffer (window-buffer w)
         (let ([curs (cursor)])
            (let ([coord (window-pos->coord w curs)])
               (if coord
                  (let ([x (list-ref coord 0)]
                        [y (list-ref coord 1)]
                        [l (list-ref coord 2)])
                     (list l x))
                  ;; else
                  (list 0 0))))))

   (define (title-style w)
      (if (equal? w (current-window))
         '(fg: "black" bg: "white")
         ;; else
         '(fg: "white" bg: "bright-black")))

   (let* ([wx (window-x w)]
          [wy (window-y w)]
          [ww (window-width w)]
          [wh (window-height w)]
          [bf (window-buffer w)]
          [ro? (buffer-is-readonly? bf)]
          [mo? (buffer-is-modified? bf)]
          [st? (and (window-is-master? w) (window-is-sticky? w))]
          [bn (buffer-name bf)]
          [mn (buffer-mode-name bf)]
          [sn (buffer-state-name bf)]
          [style (title-style w)]
          [row-col (cursor-row/col w)])
      (window-draw-char w 0 (- wh 1) #\space ww style)
      (let* ([row (list-ref row-col 0)]
             [col (list-ref row-col 1)]
             [st (format "[~d:~d] ~a(~a) ~a ~a ~a | "
                         row
                         col
                         (if mo? "[+]" "")
                         mn
                         sn
                         (if st? "*" "")
                         (if ro? "[RO]" ""))]
             [max (- ww (string-length st) 5 )]
             [nl (string-length bn)])
         (when (> nl max)
            (let ([over (- nl max)]) 
               (if (> over nl)
                  (set! bn "")
                  ;; else
                  (begin
                     (set! st (string-append st "..."))
                     (set! bn (substring bn over nl))))))
         (set! st (string-append st bn))
         (window-draw-text w 0 (- wh 1) st style))))


(define window-draw
   (case-lambda
      [(w)
       (window-draw w #f)]

      [(w enforce?)
       (when (and w (or enforce? (window-is-dirty? w)) (window-is-visible? w))
          (when (equal? w (current-window))
             (window-update-cursor w))
          (window-update w) 
          (run-hooks 'text-draw-hook w)
          (call-foreign (__cs_win_draw w enforce?))
          (run-hooks 'window-draw-hook w)
          (when (window-has-title? w)
             (window-draw-title w)))]))

(define window-is-widget?
   (case-lambda
      [()
       (window-is-widget? (current-window))]

      [(w)
       (member w (widget-list))]))

(define widget-is-top?
   (case-lambda
      [()
       (widget-is-top? (current-window))]

      [(w)
       (member w %widget-list-top%)]))

(define widget-is-bottom?
   (case-lambda
      [()
       (widget-is-top? (current-window))]

      [(w)
       (member w %widget-list-bottom%)]))

(define (window-draw-all)
   (let ([redraw? (window-layout-is-changed)])
      (when redraw?
         (ui-clear)
         (window-update-layout-size))
      (for-each
         (lambda (w)
            (when (not (equal? w (current-window)))
               (window-draw w redraw?)))
         (widget-list))
      (for-each
         (lambda (w)
            (when (not (equal? w (current-window)))
               (window-draw w redraw?)))
         (window-list))
      (window-draw (current-window) redraw?)))

(define window-is-visible?
   (case-lambda
      [()
       (window-is-visible? (current-window))]

      [(w)
       (or
          (window-is-widget? w)
          (if (layout-is-maximized?)
             (or (equal? w (current-window))
                 (equal? w (window-prev-selected)))
             ;; else
             #t))]))

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
       (call-foreign (__cs_win_first_set wid))
       (window-layout-set-changed #t)]))

(define window-set-prev
   (case-lambda
      [(prev)
       (window-set-prev (current-window) prev)]

      [(wid prev)
       (call-foreign (__cs_win_prev_set wid prev))
       (window-layout-set-changed #t)]))

(define window-set-next
   (case-lambda
      [(next)
       (window-set-next (current-window) next)]

      [(wid next)
       (call-foreign (__cs_win_next_set wid next))
       (window-layout-set-changed #t)]))

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
      (window-list (current-frame))]

     [(fr)
      (%window-list% fr)]))

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
   (frame-current-window))

(define (window-prev-selected)
    (frame-prev-focused-window))

(define (window-focus wid)
   (when (and wid
              (not (equal? wid (current-window))))
      (when (and (current-window)
                 (not (window-is-widget? (current-window))))
         (frame-set-prev-focused-window (current-window)))
      (frame-set-current-window wid)
      (call-foreign (__cs_win_current_set wid))
      (run-hooks 'window-focus-hook wid)))

(define (window-focus-left)
   (window-focus (window-left)))

(define (window-focus-right)
   (window-focus (window-right)))

(define (window-focus-upper)
   (window-focus (or (window-upper)
                     (window-prev)
                     (window-last))))

(define (window-focus-lower)
   (window-focus (or (window-lower)
                     (window-next)
                     (window-first))))

(define (window-create b)
   (let ([w (call-foreign (__cs_win_new b #f))])
      (when w
         (buffer-ref-get b)
         (if (layout-is-sticky?)
            (let ([m (window-last-master)])
               (when m (window-set-next m w)))
            ;; else
            (let ()
               (window-set-first w)))
         (run-hooks 'window-create-hook w)
         (window-layout-set-changed #t)
         (window-focus w))
      w))

(define (%window-delete% w)
   (frame-delete-window w)
   (when (equal? w (current-window))
      (let ([focus (or (frame-prev-focused-window)
                       (window-next w)
                       (window-prev w))])
         (when focus
            (frame-set-current-window #f)
            (window-focus focus))))
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
              (run-hooks 'window-delete-hook w)
              (window-layout-set-changed #t)))]))

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

(define window-set-size
   (case-lambda
      [(w h)
       (window-set-size (current-window) w h)]

      [(wid w h)
       (let ([old-w (window-width w)]
             [old-h (window-height w)])
          (when (or (and (positive? w)
                         (not (eq? w old-w)))
                    (and (positive? h)
                         (not (eq? h old-h))))
             (call-foreign (__cs_win_size_set wid w h))
             (window-layout-set-changed #t)))]))

(define window-set-width
   (case-lambda
      [(w)
       (window-set-width (current-window) w)]

      [(wid w)
       (window-set-size wid w -1)]))

(define window-move
   (case-lambda
      [(x y)
       (window-move (current-window) x y)]

      [(wid x y)
       (call-foreign (__cs_win_move wid x y))]))

(define window-set-height
   (case-lambda
      [(h)
       (window-set-height (current-window) h)]

      [(wid h)
       (window-set-size wid -1 h)]))

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

(define window-update-cursor
    (case-lambda 
       [()
        (window-update-cursor (current-window))]

       [(w)
        (call-foreign (__cs_win_update_cursor w))]))

(define window-update
    (case-lambda 
       [()
        (window-update (current-window))]

       [(w)
        (call-foreign (__cs_win_update w))]))

(define (widget-create name x y w h type)
   (let*([bid (buffer-new name #f)]
         [wid (call-foreign (__cs_win_new bid #t))])
      (when (> wid 0)
         (case type
            ['top    (set! %widget-list-top% (append %widget-list-top% (list wid)))]
            ['bottom (set! %widget-list-bottom% (append %widget-list-bottom% (list wid)))])
         (set! %widget-list% (append %widget-list% (list wid)))
         (window-set-width wid w)
         (window-set-height wid h)
         (window-move wid x y)
         (window-layout-set-changed #t))
      wid))

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

(add-hook 'frame-switch-hook
          (lambda (f)
             (window-layout-set-changed #t) ))
(add-hook 'ui-update-hook
          (lambda ()
             (window-draw-all)) )
