(define __cs_layout_current_get (foreign-procedure "cs_layout_current_get" (int) int))
(define __cs_layout_current_set (foreign-procedure "cs_layout_current_set" (int int) int))
(define __cs_layout_nmaster_get (foreign-procedure "cs_layout_nmaster_get" (int) int))
(define __cs_layout_nmaster_set (foreign-procedure "cs_layout_nmaster_set" (int int) int))
(define __cs_layout_fmaster_get (foreign-procedure "cs_layout_fmaster_get" (int) float))
(define __cs_layout_fmaster_set (foreign-procedure "cs_layout_fmaster_set" (int float) int))
(define __cs_layout_sticky_get (foreign-procedure "cs_layout_sticky_get" (int) boolean))
(define __cs_layout_sticky_set (foreign-procedure "cs_layout_sticky_set" (int boolean) int))
(define __cs_layout_arrange (foreign-procedure "cs_layout_arrange" (int int int int int) void))
(define __cs_layout_xy (foreign-procedure "cs_layout_xy" () scheme-object))
(define __cs_layout_wh (foreign-procedure "cs_layout_wh" () scheme-object))

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
       (layout->symb (call-foreign (__cs_layout_current_get (frame-id fr))))]))

(define (layout-x)
   (let ([xy (call-foreign (__cs_layout_xy))])
      (car xy)))

(define (layout-y)
   (let ([xy (call-foreign (__cs_layout_xy))])
      (cdr xy)))

(define (layout-width)
   (let ([wh (call-foreign (__cs_layout_wh))])
      (car wh)))

(define (layout-height)
   (let ([wh (call-foreign (__cs_layout_wh))])
      (cdr wh)))

(define layout-arrange
   (case-lambda
      [()
       (layout-arrange (current-layout))]

      [(symb)
       (call-foreign (__cs_layout_arrange
                        (symb->layout symb)
                        (layout-x)
                        (layout-y)
                        (layout-width)
                        (layout-height)))]))

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
       (call-foreign (__cs_layout_current_set (frame-id fr)  (symb->layout l)))
       (frame-set-layout fr l)
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
       (equal? 'tiled (current-layout (frame-id fr)))]))

(define layout-is-grid?
   (case-lambda
      [()
       (equal? 'grid (current-layout))]

      [(fr)
       (equal? 'grid (current-layout (frame-id fr)))]))

(define layout-is-bstack?
   (case-lambda
      [()
       (equal? 'bstack (current-layout))]

      [(fr)
       (equal? 'bstack (current-layout (frame-id fr)))]))

(define layout-is-maximized?
   (case-lambda
      [()
       (equal? 'maximized (current-layout))]

      [(fr)
       (equal? 'maximized (current-layout (frame-id fr)))]))

(define layout-n-master
   (case-lambda
      [()
       (layout-n-master (current-frame))]

      [(fr)
       (call-foreign (__cs_layout_nmaster_get (frame-id fr)))]))

(define layout-set-n-master
   (case-lambda
      [(n)
       (layout-set-n-master (current-frame) n)]

      [(fr n)
       (call-foreign (__cs_layout_nmaster_set (frame-id fr) n))]))

(define layout-n-master+
   (case-lambda
      [()
       (layout-n-master+ (current-frame) 1)]

      [(n)
       (layout-n-master+ (current-frame) n)]

      [(fr n)
       (call-foreign (__cs_layout_nmaster_set
                        (frame-id fr) (+ (call-foreign (__cs_layout_nmaster_get (frame-id fr))) n)))]))

(define layout-n-master-
   (case-lambda
      [()
       (layout-n-master- (current-frame) 1)]

      [(n)
       (layout-n-master- (current-frame) n)]

      [(fr n)
       (call-foreign (__cs_layout_nmaster_set
                        (frame-id fr) (- (call-foreign (__cs_layout_nmaster_get (frame-id fr))) n)))]))

(define layout-%-master
   (case-lambda
      [()
       (layout-%-master (current-frame))]

      [(fr)
       (call-foreign (__cs_layout_fmaster_get (frame-id fr)))]))

(define layout-set-%-master
   (case-lambda
      [(f)
       (layout-set-%-master (current-frame) f)]

      [(fr f)
       (call-foreign (__cs_layout_fmaster_set (frame-id fr) f))]))

(define layout-%-master+
   (case-lambda
      [()
       (layout-%-master+ (current-frame) 0.05)]

      [(f)
       (layout-%-master+ (current-frame) f)]

      [(fr f)
       (call-foreign (__cs_layout_fmaster_set
                        (frame-id fr) (+ (call-foreign (__cs_layout_fmaster_get (frame-id fr))) f)))]))

(define layout-%-master-
   (case-lambda
      [()
       (layout-%-master- (current-frame) 0.05)]

      [(f)
       (layout-%-master- (current-frame) f)]

      [(fr f)
       (call-foreign (__cs_layout_fmaster_set
                        (frame-id fr) (- (call-foreign (__cs_layout_fmaster_get (frame-id fr))) f)))]))

(define layout-set-sticky
   (case-lambda
      [(s)
       (layout-set-sticky (current-frame) s)]

      [(fr s)
       (call-foreign (__cs_layout_sticky_set (frame-id fr) s))]))

(define layout-is-sticky?
   (case-lambda
      [()
       (layout-is-sticky? (current-frame))]

      [(fr)
       (call-foreign (__cs_layout_sticky_get (frame-id fr)))]))

(define layout-toggle-sticky
   (case-lambda
      [()
       (layout-set-sticky (not (layout-is-sticky?)))]

      [(fr)
       (layout-set-sticky fr (not (layout-is-sticky? fr)))]))
