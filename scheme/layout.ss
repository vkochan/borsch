(define %layout-x% 0)
(define %layout-y% 0)
(define %layout-width% 10)
(define %layout-height% 10)

(define (layout->symb l)
   (case l 
      [0 'tiled       ]
      [1 'grid        ]
      [2 'bstack      ]
      [3 'maximized   ]))

(define (layout->name l)
   (case l 
      [0 "[]="]
      [1 "+++"]
      [2 "TTT"]
      [3 "[ ]"]))

(define (symb->layout s)
   (case s 
      ['tiled       0]
      ['grid        1]
      ['bstack      2]
      ['maximized   3]))

(define current-layout
   (case-lambda
      [()
       (current-layout (current-frame))]

      [(fr)
       (frame-layout fr)]))

(define (layout-x)
   %layout-x%)

(define (layout-set-x x)
   (set! %layout-x% x))

(define (layout-y)
   %layout-y%)

(define (layout-set-y y)
   (set! %layout-y% y))

(define (layout-width)
   %layout-width%)

(define (layout-set-width w)
   (set! %layout-width% w))

(define (layout-height)
   %layout-height%)

(define (layout-set-height h)
   (set! %layout-height% h))

(define TEXT-SYMBOL-LTEE  #\x251c)
(define TEXT-SYMBOL-RTEE  #\x2524)
(define TEXT-SYMBOL-TTEE  #\x252C)
(define TEXT-SYMBOL-VLINE #\x2502)
(define TEXT-SYMBOL-PLUS  #\x253c)

(define (layout-arrange-tiled)
   (let*([wax (layout-x)]
         [way (layout-y)]
         [waw (layout-width)]
         [wah (layout-height)]
         [lax wax]
         [lay (- way 1)]
         [law waw]
         [lah wah]
         [i 0]
         [n (length (window-list))]
         [nx lax]
         [ny lay]
         [m (max 1 (min n (layout-n-master)))]
         [mw (cond [(= n m) waw]
                   [else (flonum->fixnum (inexact (* (layout-%-master) waw)))])]
         [mh (fx/ wah m)]
         [th (cond [(= n m) 0]
                   [else (fx/ wah (- n m))])])

      (define (arrange-master w)
         (set! nw mw)
         (set! nh (cond [(< i (- m 1)) mh]
                        [else (- (+ lay wah) ny)])))

      (define (arrange-tile w)
         (when (= i m)
            (set! ny lay)
            (set! nx (+ nx mw))
            (ui-draw-char-vert nx ny TEXT-SYMBOL-VLINE wah)
            (ui-draw-char nx ny TEXT-SYMBOL-TTEE)
            (set! nx (+ 1 nx))
            (set! nw (- waw mw 1))
         )
         (set! nh (cond [(< i (- n 1)) th]
                        [else (- (+ lay wah) ny)]))
         (when (> i m)
            (ui-draw-char (- nx 1) ny TEXT-SYMBOL-LTEE 1)))

      (define (fill-n-masters)
         (let loop ([i 1]
                    [ny (+ lay mh)])
            (when (< i m)
               (let ([ch (cond [(> (fxmodulo (- ny 1) th) 0) TEXT-SYMBOL-RTEE]
                               [else                         TEXT-SYMBOL-PLUS])]
                     [x (- nx 1)]
                     [y ny])
                  (ui-draw-char x y ch))
               (loop (+ i 1) (+ ny mh)))))

      (window-for-each
         (lambda (w)
            (cond [(< i m) (arrange-master w)]
                  [else    (arrange-tile w)])
            (window-move w nx (+ ny (- way lay)))
            (window-set-height w nh)
            (window-set-width w nw)
            (set! ny (+ ny nh))
            (set! i (+ i 1))))
      ;; Fill in n-master intersections
      (when (> n m)
         (fill-n-masters))))

(define (layout-arrange-bstack)
   (let*([wax (layout-x)]
         [way (layout-y)]
         [waw (layout-width)]
         [wah (layout-height)]
         [lax wax]
         [lay (- way 1)]
         [law waw]
         [lah wah]
         [i 0]
         [n (length (window-list))]
         [nx lax]
         [ny lay]
         [nh 0]
         [m (max 1 (min n (layout-n-master)))]
         [mh (cond [(= n m) lah]
                   [else (flonum->fixnum (inexact (* (layout-%-master) lah)))])]
         [mw (fx/ law m)]
         [tw (cond [(= n m) 0]
                   [else (fx/ law
                              (- n m))])])
      (define (arrange-master w)
         (when (> i 0)
            (ui-draw-char-vert nx ny TEXT-SYMBOL-VLINE nh)
            (ui-draw-char nx ny TEXT-SYMBOL-TTEE)
            (set! nx (+ nx 1)))
         (set! nh mh)
         (set! nw (cond [(< i
                            (- m 1))
                         mw]
                        [else (- (+ lax law)
                                 nx)])))

      (define (arrange-tile w)
         (when (= i m)
            (set! nx lax)
            (set! ny (+ ny mh))
            (set! nh (- (+ lay lah)
                        ny)))
         (when (> i m)
            (ui-draw-char-vert nx ny TEXT-SYMBOL-VLINE nh)
            (ui-draw-char nx ny TEXT-SYMBOL-TTEE)
            (set! nx (+ nx
                        1))
         )
         (set! nw (cond [(< i
                            (- n
                               1))
                         tw]
                        [else (- (+ lax
                                    law)
                                 nx)])))

      (define (fill-n-masters)
         (set! nx lax)
         (let loop ([i 0])
            (when (< i m)
               (when (> i 0)
                  (ui-draw-char nx ny TEXT-SYMBOL-PLUS)
                  (set! nx (+ nx 1))
               )
               (set! nw (cond [(< i
                                  (- m 1))
                               mw]
                              [else (- (+ lax law)
                                       nx)]))
               (set! nx (+ nx nw))
               (loop (+ i 1)))))
 
      (window-for-each
         (lambda (w)
            (cond [(< i m) (arrange-master w)]
                  [else    (arrange-tile w)])
            (window-move w nx (+ ny (- way lay)))
            (window-set-height w nh)
            (window-set-width w nw)
            (set! nx (+ nx nw))
            (set! i (+ i 1))))
      ;; Fill in n-master intersections
      (when (> n m)
         (fill-n-masters))))

(define (layout-arrange-grid)
   (define (grid-cols n)
      (let loop ([cols 0])
         (cond [(or (> cols (fx/ n 2))
                    (>= (fx* cols cols) n))
                cols]
               [else (loop (+ cols 1))])))

   (define (grid-rows n cols)
      (cond [(and (> cols 0)
                  (>= (* (- cols 1)
                         cols)
                      n))
             (- cols 1)]
            [else cols]))

   (let*([wax (layout-x)]
         [way (layout-y)]
         [waw (layout-width)]
         [wah (layout-height)]
         [law waw]
         [lah wah]
         [lax wax]
         [lay (- way 1)]
         [i 0]
         [n (length (window-list))]
         [cols (grid-cols n)]
         [rows (grid-rows n cols)]
         [nh (fx/ lah (cond [(> rows 0) rows] [else 1]))]
         [nw (fx/ law (cond [(> cols 0) cols] [else 1]))]
         [nx 0]
         [ny 0]
         [aw 0]
         [ah 0])

      (window-for-each
         (lambda (w)
            ;; if there are less windows in the last row than normal adjust the
            ;; split rate to fill the empty space
            (when (and (> rows
                          1)
                       (= i
                          (- (* rows cols)
                             cols))
                       (<= (- n i)
                           (fxmodulo n cols)))
               (set! nw (fx/ law
                             (- n i))))
            (set! nx (+ lax
                        (* nw
                           (fxmodulo i cols))))
            (set! ny (+ lay
                        (* nh
                           (fx/ i cols))))
            ;; adjust height/width of last row/column's windows
            (set! ah (cond [(>= i
                                (* cols
                                   (- rows 1)))
                            (- lah
                               (* nh rows))]
                           [else 0]))
            ;; special case if there are less windows in the last row
            (set! aw (cond [(and (> rows 1)
                                 (= i (- n 1))
                                 (< (- n i)
                                    (fxmodulo n cols)))
                            ;; (n % cols) == number of windows in the last row
                            (- law
                               (* nw
                                  (fxmodulo n cols)))]
                           [else (cond [(= (fxmodulo (+ i 1)
                                                     cols)
                                           0)
                                        (- law
                                           (* nw cols))]
                                       [else 0])]))
            (when (> (fxmodulo i cols)
                     0)
               (ui-draw-char-vert nx ny TEXT-SYMBOL-VLINE (+ nh ah))
               ;; if we are on the first row, or on the last one and there are fewer windows
               ;; than normal whose border does not match the line above, print a top tree char
               ;; otherwise a plus sign.
               (cond [(or (<= i cols)
                          (and (>= i
                                   (- (* rows cols)
                                      cols))
                               (> (fxmodulo n cols)
                                  0)
                               (> (fxmodulo (- cols
                                               (fxmodulo n cols))
                                            2)
                                  0)))
                      (ui-draw-char nx ny TEXT-SYMBOL-TTEE)]
                     [else
                      (ui-draw-char nx ny TEXT-SYMBOL-PLUS)])
               (set! nx (+ nx 1))
               (set! aw (- aw 1))
            )
            (window-set-height w (+ nh ah))
            (window-set-width w (+ nw aw))
            (window-move w nx (+ ny
                                 (- way lay)))
            (set! i (+ i 1))
            ))))

(define (layout-arrange-maximized)
   (window-for-each
      (lambda (w)
         (window-move w (layout-x) (layout-y))
         (window-set-height w (layout-height))
         (window-set-width w (layout-width)))))

(define layout-arrange
   (case-lambda
      [()
       (layout-arrange (current-layout))]

      [(symb)
       (case symb
          ['maximized (layout-arrange-maximized)]
          ['grid      (layout-arrange-grid)]
          ['tiled     (layout-arrange-tiled)]
          ['bstack    (layout-arrange-bstack)])]))

(define layout-name
   (case-lambda
      [()
       (layout-name (current-layout))]

      [(symb)
       (layout->name (symb->layout symb))]))

(define layout-set
   (case-lambda
      [(l)
       (layout-set (current-frame) l)]

      [(fr l)
       (frame-set-prev-layout fr (current-layout fr))
       (frame-set-layout fr l)
       (ui-needs-update #t)
       (run-hooks 'layout-changed-hook)]))

(define layout-set-tiled
   (case-lambda
      [()
       (layout-set-tiled (current-frame))]

      [(fr)
       (layout-set fr 'tiled)]))

(define layout-set-grid
   (case-lambda
      [()
       (layout-set-grid (current-frame))]

      [(fr)
       (layout-set fr 'grid)]))

(define layout-set-bstack
   (case-lambda
      [()
       (layout-set-bstack (current-frame))]

      [(fr)
       (layout-set fr 'bstack)]))

(define layout-set-maximized
   (case-lambda
      [()
       (layout-set-maximized (current-frame))]

      [(fr)
       (layout-set fr 'maximized)]))

(define layout-toggle-maximized
   (case-lambda
      [()
       (layout-toggle-maximized (current-frame))]

      [(fr)
       (if (equal? (current-layout fr) 'maximized)
          (layout-set fr (frame-prev-layout fr))
          ;; else
          (layout-set fr 'maximized))]))

(define layout-is-tiled?
   (case-lambda
      [()
       (equal? 'tiled (current-layout))]

      [(fr)
       (equal? 'tiled (current-layout fr))]))

(define layout-is-grid?
   (case-lambda
      [()
       (equal? 'grid (current-layout))]

      [(fr)
       (equal? 'grid (current-layout fr))]))

(define layout-is-bstack?
   (case-lambda
      [()
       (equal? 'bstack (current-layout))]

      [(fr)
       (equal? 'bstack (current-layout fr))]))

(define layout-is-maximized?
   (case-lambda
      [()
       (equal? 'maximized (current-layout))]

      [(fr)
       (equal? 'maximized (current-layout fr))]))

(define layout-n-master
   (case-lambda
      [()
       (layout-n-master (current-frame))]

      [(fr)
       (frame-n-master fr)]))

(define layout-set-n-master
   (case-lambda
      [(n)
       (layout-set-n-master (current-frame) n)]

      [(fr n)
       (frame-set-n-master fr n)
       (ui-needs-update #t)]))

(define layout-n-master+
   (case-lambda
      [()
       (layout-n-master+ (current-frame) 1)]

      [(n)
       (layout-n-master+ (current-frame) n)]

      [(fr n)
       (layout-set-n-master fr (+ (layout-n-master fr)
                                  n))]))

(define layout-n-master-
   (case-lambda
      [()
       (layout-n-master- (current-frame) 1)]

      [(n)
       (layout-n-master- (current-frame) n)]

      [(fr n)
       (layout-set-n-master fr (- (layout-n-master fr)
                                  n))]))

(define layout-%-master
   (case-lambda
      [()
       (layout-%-master (current-frame))]

      [(fr)
       (frame-%-master fr)]))

(define layout-set-%-master
   (case-lambda
      [(f)
       (layout-set-%-master (current-frame) f)]

      [(fr f)
       (frame-set-%-master fr f)
       (ui-needs-update #t)]))

(define layout-%-master+
   (case-lambda
      [()
       (layout-%-master+ (current-frame) 0.05)]

      [(f)
       (layout-%-master+ (current-frame) f)]

      [(fr f)
       (layout-set-%-master fr (+ (frame-%-master fr)
                                 f))]))

(define layout-%-master-
   (case-lambda
      [()
       (layout-%-master- (current-frame) 0.05)]

      [(f)
       (layout-%-master- (current-frame) f)]

      [(fr f)
       (layout-set-%-master fr (- (frame-%-master fr)
                                 f))]))

(define layout-set-sticky
   (case-lambda
      [(s)
       (layout-set-sticky (current-frame) s)]

      [(fr s)
       (frame-set-sticky fr s)
       (ui-needs-update #t)]))

(define layout-is-sticky?
   (case-lambda
      [()
       (layout-is-sticky? (current-frame))]

      [(fr)
       (frame-is-sticky? fr)]))
 
(define layout-toggle-sticky
   (case-lambda
      [()
       (layout-set-sticky (not (layout-is-sticky?)))]

      [(fr)
       (layout-set-sticky fr (not (layout-is-sticky? fr)))]))

(define (layout-update)
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
   (layout-arrange) )

(define (layout-draw)
   (let ([redraw? (ui-needs-update?)])
      (when redraw?
         (layout-update))
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

(add-hook 'ui-update-hook
          (lambda ()
             (layout-draw)) )
