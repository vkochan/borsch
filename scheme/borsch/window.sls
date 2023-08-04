(library (borsch window)
   (export
      window-id
      widget-list
      window-reload-buffer
      buffer-is-visible?
      window-draw-char
      window-draw-text
      window-has-title?
      window-is-dirty?
      window-set-text-style
      window-draw-title
      window-draw-selection
      window-draw
      window-is-widget?
      widget-is-top?
      widget-is-bottom?
      window-is-visible?
      window-last-master
      window-first
      window-next
      window-prev
      window-set-first
      window-set-prev
      window-set-next
      window-list
      window-last
      window-for-each
      window-for-all
      window-upper
      window-lower
      window-right
      window-left
      current-window
      window-prev-selected
      window-focus
      window-focus-left
      window-focus-right
      window-focus-upper
      window-focus-lower
      window-create
      window-delete
      window-close
      window-is-maximized?
      window-is-master?
      window-set-master
      window-is-sticky?
      window-buffer
      window-width
      window-height
      window-set-size
      window-set-width
      window-move
      window-set-height
      window-set-border
      window-switch-buffer
      window-begin-pos
      window-end-pos
      window-pos->coord
      window-lines-coord
      window-inner-width
      window-inner-height
      window-scroll-page-down
      window-scroll-page-up
      window-scroll-halfpage-down
      window-scroll-halfpage-up
      window-scroll-down
      window-scroll-up
      window-set-sidebar-width
      window-sidebar-width
      window-update-cursor
      window-update
      widget-create
      window-x
      window-y
      window-find
      window-by-pos
      window-initialize)
   (import (chezscheme)
           (borsch base)
           (borsch lists)
           (borsch ui)
           (borsch buffer)
           (borsch text)
           (borsch frame)
           (borsch style))

(define __cs_win_current_set (foreign-procedure "cs_win_current_set" (void*) int))
(define __cs_win_new (foreign-procedure "cs_win_new" (int boolean) scheme-object))
(define __cs_win_del (foreign-procedure "cs_win_del" (void*) int))
(define __cs_win_width_get (foreign-procedure "cs_win_width_get" (void*) scheme-object))
(define __cs_win_height_get (foreign-procedure "cs_win_height_get" (void*) scheme-object))
(define __cs_win_viewport_width_get (foreign-procedure "cs_win_viewport_width_get" (void*) scheme-object))
(define __cs_win_viewport_height_get (foreign-procedure "cs_win_viewport_height_get" (void*) scheme-object))
(define __cs_win_size_set (foreign-procedure "cs_win_size_set" (void* int int) void))
(define __cs_win_move (foreign-procedure "cs_win_move" (void* int int) void))
(define __cs_win_border_set (foreign-procedure "cs_win_border_set" (void* boolean) void))
(define __cs_win_buf_switch (foreign-procedure "cs_win_buf_switch" (void* int) void))
(define __cs_win_viewport_pos (foreign-procedure "cs_win_viewport_pos" (void* char) scheme-object))
(define __cs_win_viewport_coord (foreign-procedure "cs_win_viewport_coord" (void* int) scheme-object))
(define __cs_win_viewport_cell_set (foreign-procedure "cs_win_viewport_cell_set" (void* int int int int int wchar int) void))
(define __cs_win_scroll (foreign-procedure "cs_win_scroll" (void* char int) scheme-object))
(define __cs_win_sidebar_set (foreign-procedure "cs_win_sidebar_set" (void* int) void))
(define __cs_win_sidebar_get (foreign-procedure "cs_win_sidebar_get" (void*) scheme-object))
(define __cs_win_update (foreign-procedure "cs_win_update" (void*) scheme-object))
(define __cs_win_update_cursor (foreign-procedure "cs_win_update_cursor" (void*) void))
(define __cs_win_coord_get (foreign-procedure "cs_win_coord_get" (void*) scheme-object))
(define __cs_win_draw (foreign-procedure "cs_win_draw" (void* boolean) void))
(define __cs_win_has_title (foreign-procedure "cs_win_has_title" (void*) boolean))
(define __cs_win_view_reload (foreign-procedure "cs_win_view_reload" (void* int) void))

(define-record-type $window
   (fields
      id
      (mutable buffer)
      (mutable prev)
      (mutable next)) )

(define (window-id w)
   ($window-id w) )

(define $widget-list-top (list))
(define $widget-list-bottom (list))
(define $widget-list (list))

(define (widget-list)
   $widget-list)

(define (window-reload-buffer buf)
   (window-for-each
      (lambda (win)
         (when (equal? buf (window-buffer win))
            (call-foreign (__cs_win_view_reload (window-id win) (buffer-id buf))) ))))

(define (buffer-is-visible? buf)
   (call/cc
      (lambda (return)
         (window-for-each
            (lambda (win)
               (when (equal? (window-buffer win) buf)
                  (return #t) )))
         (return #f) )))

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

      [(win)
       (call-foreign (__cs_win_has_title (window-id win)))]))

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

      [(win start end style)
       (let ([ls-style (style->list style)])
          (call-foreign (__cs_win_viewport_cell_set (window-id win) start end
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

(define (window-draw-selection win)
   (let ([buf (window-buffer win)])
      (with-current-buffer buf
         (when (text-is-selection-set?)
            (let* ([range (text-selection-range)]
                   [start (first range)]
                   [end (- (second range)
                           1)])
               (window-set-text-style win start end '(fg: "white" bg: "blue")) )))))

(define window-draw
   (case-lambda
      [(w)
       (window-draw w #f)]

      [(win enforce?)
       (when (and win (or enforce? (window-is-dirty? win)) (window-is-visible? win))
          (when (equal? win (current-window))
             (window-update-cursor win))
          (window-update win) 
          (with-current-buffer (window-buffer win)
             (let ([pre-draw (get-local pre-draw-func)])
                (when pre-draw
                   (pre-draw) ))
             (buffer-set-dirty #f) )
          (window-draw-selection win)
          (run-hooks 'text-draw-hook win)
          (call-foreign (__cs_win_draw (window-id win) enforce?))
          (run-hooks 'window-draw-hook win)
          (when (window-has-title? win)
             (window-draw-title win)))] ))

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
       (member w $widget-list-top)]))

(define widget-is-bottom?
   (case-lambda
      [()
       (widget-is-top? (current-window))]

      [(w)
       (member w $widget-list-bottom)]))

(define window-is-visible?
   (case-lambda
      [()
       (window-is-visible? (current-window))]

      [(w)
       (or
          (window-is-widget? w)
          (if (frame-is-maximized?)
             (or (equal? w (current-window))
                 (equal? w (window-prev-selected)))
             ;; else
             #t))]))

(define (window-last-master)
   (let ([nm (frame-n-master)]
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
      (frame-first-window fr) ]))

(define window-next
   (case-lambda
      [()
       (window-next (current-window))]

      [(w)
       ($window-next w)]))

(define window-prev
   (case-lambda
      [()
       (window-prev (current-window))]

      [(w)
       ($window-prev w)]))

(define ($window-remove w)
   (when w
      (when (equal? w (frame-first-window))
         (frame-set-first-window ($window-next w)))
      (when (equal? w (frame-last-window))
         (frame-set-last-window ($window-prev w)))
      (let ([prev ($window-prev w)]
            [next ($window-next w)])
         (when prev
            ($window-next-set! prev next))
         (when next
            ($window-prev-set! next prev)) )
      ($window-prev-set! w #f)
      ($window-next-set! w #f) ))

(define ($window-insert-next win next)
   (if ($window-next win)
      ($window-prev-set! ($window-next win) next)
      ;; else
      (frame-set-last-window next))
   ($window-next-set! next ($window-next win))
   ($window-prev-set! next win)
   ($window-next-set! win next)
   (when (not (frame-first-window))
      (frame-set-first-window win))
   (when (not (frame-last-window))
      (frame-set-last-window win)) )

(define ($window-insert-prev win prev)
   (if ($window-prev win)
      ($window-next-set! ($window-prev win) prev)
      ;; else
      (frame-set-first-window prev))
   ($window-prev-set! prev ($window-prev win))
   ($window-next-set! prev win)
   ($window-prev-set! win prev)
   (when (not (frame-first-window))
      (frame-set-first-window win))
   (when (not (frame-last-window))
      (frame-set-last-window win)) )

(define window-set-first
   (case-lambda
      [()
       (window-set-first (current-window))]

      [(w)
       (if (not (frame-first-window))
          (begin
             (frame-set-first-window w)
             (frame-set-last-window w))
          ;; else
          (begin
             ($window-remove w)
             ($window-insert-prev (frame-first-window) w)))
       (ui-needs-update #t)]))

(define window-set-prev
   (case-lambda
      [(prev)
       (window-set-prev (current-window) prev)]

      [(w prev)
       ($window-remove prev)
       ($window-insert-prev  w prev)
       (ui-needs-update #t)]))

(define window-set-next
   (case-lambda
      [(next)
       (window-set-next (current-window) next)]

      [(w next)
      ($window-remove next)
      ($window-insert-next w next)
      (ui-needs-update #t)]))

(define ($window-list fr)
   (let loop ([next (window-first fr)]
              [ls   (list)])
      (if next
         (loop ($window-next next)
               (append ls (list next)) )
         ;; else
         ls )))

(define window-list
   (case-lambda
     [()
      (window-list (current-frame))]

     [(fr)
      ($window-list fr)]))

(define (window-last)
   (frame-last-window))

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

(define (window-focus win)
   (when (and win
              (not (equal? win (current-window))))
      (when (and (current-window)
                 (not (window-is-widget? (current-window))))
         (frame-set-prev-focused-window (current-window)))
      (frame-set-current-window win)
      (when (current-buffer)
         (buffer-set-dirty (current-buffer) #t) )
      (current-buffer (window-buffer win))
      (call-foreign (__cs_win_current_set (window-id win)))
      (buffer-set-dirty (current-buffer) #t)
      (run-hooks 'window-focus-hook win)))

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

(define ($window-new buf widget?)
   (buffer-set-dirty buf #t)
   (make-$window (__cs_win_new (buffer-id buf) widget?) buf #f #f) )

(define (window-create b)
   (let ([win ($window-new b #f)])
      (when win
         (buffer-ref-get b)
         (if (frame-is-sticky?)
            (let ([m (window-last-master)])
               (when m (window-set-next m win)))
            ;; else
            (let ()
               (window-set-first win)))
         (run-hooks 'window-create-hook win)
         (ui-needs-update #t)
         (window-focus win))
      win))

(define ($window-delete win)
   (frame-delete-window win)
   ($window-remove win)
   (when (equal? win (current-window))
      (let ([focus (or (frame-prev-focused-window)
                       (window-next win)
                       (window-prev win))])
         (frame-set-current-window #f)
         (when focus
            (window-focus focus))))
   (call-foreign (__cs_win_del (window-id win))))

(define window-delete
    (case-lambda
       [()
	(window-delete (current-window))]

       [(w)
        (let ([b (window-buffer w)])
           ($window-delete w)
           (buffer-ref-put b)
	   (when (>= 1 (buffer-ref-count b))
              (buffer-ref-put b))
           (run-hooks 'window-delete-hook w)
           (ui-needs-update #t))]))

(define window-close
    (case-lambda
       [()
	(window-close (current-window))]

       [(w)
        (buffer-ref-put (window-buffer))
        ($window-delete w)]))

(define window-is-maximized?
   (case-lambda
      [()
       (window-is-maximized? (current-window))]

      [(wid)
       (and (equal? (current-window) wid)
            (frame-is-maximized?))]))

(define window-is-master?
   (case-lambda
      [()
       (window-is-master? (current-window))]

      [(wid)
       (let ([nm (frame-n-master)]
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
                      (set! m? w)
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

      [(win)
       (window-set-first win)]))

(define window-is-sticky?
   (case-lambda
      [()
       (window-is-sticky? (current-window))]

      [(wid)
       (and (window-is-master? wid) (frame-is-sticky?))]))

(define window-buffer
   (case-lambda
      [()
       (window-buffer (current-window))]

      [(win)
       ($window-buffer win)]))

(define window-width
   (case-lambda
      [()
       (window-width (current-window))]

      [(win)
       (call-foreign (__cs_win_width_get (window-id win))) ]))

(define window-height
   (case-lambda
      [()
       (window-height (current-window))]

      [(win)
       (call-foreign (__cs_win_height_get (window-id win)))]))

(define window-set-size
   (case-lambda
      [(w h)
       (window-set-size (current-window) w h)]

      [(win w h)
       (let ([old-w (window-width win)]
             [old-h (window-height win)])
          (when (or (and (positive? w)
                         (not (eq? w old-w)))
                    (and (positive? h)
                         (not (eq? h old-h))))
             (call-foreign (__cs_win_size_set (window-id win) w h))
             (ui-needs-update #t)))]))

(define window-set-width
   (case-lambda
      [(w)
       (window-set-width (current-window) w)]

      [(win w)
       (window-set-size win w -1)]))

(define window-move
   (case-lambda
      [(x y)
       (window-move (current-window) x y)]

      [(win x y)
       (call-foreign (__cs_win_move (window-id win) x y))]))

(define window-set-height
   (case-lambda
      [(h)
       (window-set-height (current-window) h)]

      [(win h)
       (window-set-size win -1 h)]))

(define window-set-border
   (case-lambda
      [(h)
       (window-set-border (current-window) h)]

      [(win h)
       (call-foreign (__cs_win_border_set (window-id win) -1 h))]))

(define window-switch-buffer
   (case-lambda
      [(b)
       (window-switch-buffer (current-window) b)]

      [(win buf)
       (buffer-ref-put (window-buffer win))
       (call-foreign (__cs_win_buf_switch (window-id win) (buffer-id buf)))
       ($window-buffer-set! win buf)
       (buffer-ref-get (window-buffer win))
       (buffer-set-dirty buf #t)
       (when (equal? win (current-window))
          (current-buffer buf)) ]))

(define window-begin-pos
   (case-lambda
      [()
       (window-begin-pos (current-window))]

      [(win)
       (call-foreign (__cs_win_viewport_pos (window-id win) #\H))]))

(define window-end-pos
   (case-lambda
      [()
       (window-end-pos (current-window))]

      [(win)
       (call-foreign (__cs_win_viewport_pos (window-id win) #\L))]))

(define window-pos->coord
   (case-lambda
      [(p)
       (window-pos->coord (current-window) p)]

      [(win p)
       (call-foreign (__cs_win_viewport_coord (window-id win) p))]))

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

      [(win)
       (call-foreign (__cs_win_viewport_width_get (window-id win)))]))

(define window-inner-height
   (case-lambda
      [()
       (window-inner-height (current-window))]

      [(win)
       (call-foreign (__cs_win_viewport_height_get (window-id win))) ]))

(define window-scroll-page-down
   (case-lambda
      [()
       (window-scroll-page-down (current-window))]

      [(win)
       (call-foreign (__cs_win_scroll (window-id win) #\f 0))]))

(define window-scroll-page-up
   (case-lambda
      [()
       (window-scroll-page-up (current-window))]

      [(win)
       (call-foreign (__cs_win_scroll (window-id win) #\b 0))]))

(define window-scroll-halfpage-down
   (case-lambda
      [()
       (window-scroll-halfpage-down (current-window))]

      [(win)
       (call-foreign (__cs_win_scroll (window-id win) #\d 0))]))

(define window-scroll-halfpage-up
   (case-lambda
      [()
       (window-scroll-halfpage-up (current-window))]

      [(win)
       (call-foreign (__cs_win_scroll (window-id win) #\u 0))]))

(define window-scroll-down
   (case-lambda
      [(n)
       (window-scroll-down (current-window))]

      [(win n)
       (call-foreign (__cs_win_scroll (window-id win) #\l n))]))

(define window-scroll-up
   (case-lambda
      [(n)
       (window-scroll-down (current-window))]

      [(win n)
       (call-foreign (__cs_win_scroll (window-id win) #\L n))]))

(define window-set-sidebar-width
   (case-lambda
      [(l)
       (window-set-sidebar-width (current-window) l)]

      [(win l)
       (call-foreign (__cs_win_sidebar_set (window-id win) l))
       (buffer-set-dirty (window-buffer) #t)
       ]))

(define window-sidebar-width
   (case-lambda
      [()
       (window-sidebar-width (current-window))]

      [(win)
       (call-foreign (__cs_win_sidebar_get (window-id win)))]))

(define window-update-cursor
    (case-lambda 
       [()
        (window-update-cursor (current-window))]

       [(win)
        (call-foreign (__cs_win_update_cursor (window-id win)))
        ]))

(define window-update
    (case-lambda 
       [()
        (window-update (current-window))]

       [(win)
        (call-foreign (__cs_win_update (window-id win)))]))

(define (widget-create name x y w h type)
   (let*([bid (buffer-new name #f)]
         [win ($window-new bid #t)])
      (case type
         ['top    (set! $widget-list-top (append $widget-list-top (list win)))]
         ['bottom (set! $widget-list-bottom (append $widget-list-bottom (list win)))])
      (set! $widget-list (append $widget-list (list win)))
      (frame-remove-buffer bid)
      (window-set-width win w)
      (window-set-height win h)
      (window-move win x y)
      (ui-needs-update #t)
      win))

(define window-x
   (case-lambda
      [()
       (window-x (current-window))]

      [(win)
       (let ([pos (call-foreign (__cs_win_coord_get (window-id win)))])
          (car pos))]))

(define window-y
   (case-lambda
      [()
       (window-y (current-window))]

      [(win)
       (let ([pos (call-foreign (__cs_win_coord_get (window-id win)))])
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

(define (window-initialize)
   (add-hook 'frame-delete-hook
             (lambda (f)
                (for-each
                   (lambda (w)
                      (with-current-frame f
                         (window-delete w)))
                   (window-list f) )))

   (add-hook 'frame-switch-hook
             (lambda (f)
                (when (current-window)
                   (call-foreign (__cs_win_current_set (window-id (current-window))))
                   (current-buffer (window-buffer (current-window))))
                (ui-needs-update #t) )))
)
