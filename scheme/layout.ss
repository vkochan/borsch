(define __cs_layout_current_get (foreign-procedure __collect_safe "cs_layout_current_get" (int) int))
(define __cs_layout_current_set (foreign-procedure __collect_safe "cs_layout_current_set" (int int) int))
(define __cs_layout_nmaster_get (foreign-procedure __collect_safe "cs_layout_nmaster_get" (int) int))
(define __cs_layout_nmaster_set (foreign-procedure __collect_safe "cs_layout_nmaster_set" (int int) int))
(define __cs_layout_fmaster_get (foreign-procedure __collect_safe "cs_layout_fmaster_get" (int) float))
(define __cs_layout_fmaster_set (foreign-procedure __collect_safe "cs_layout_fmaster_set" (int float) int))
(define __cs_layout_sticky_get (foreign-procedure __collect_safe "cs_layout_sticky_get" (int) boolean))
(define __cs_layout_sticky_set (foreign-procedure __collect_safe "cs_layout_sticky_set" (int boolean) int))

(define layout->symb
   (lambda (l)
      (case l 
         [0 'tiled       ]
         [1 'grid        ]
         [2 'bstack      ]
         [3 'maximized   ]
      )
   )
)

(define layout->name
   (lambda (l)
      (case l 
         [0 "[]="        ]
         [1 "+++"        ]
         [2 "TTT"        ]
         [3 "[ ]"        ]
      )
   )
)

(define symb->layout
   (lambda (s)
      (case s 
         ['tiled       0]
         ['grid        1]
         ['bstack      2]
         ['maximized   3]
      )
   )
)

(define layout-current
   (case-lambda
        [()
         (layout->symb (call-foreign (__cs_layout_current_get (current-frame))))]

        [(tag)
         (layout->symb (call-foreign (__cs_layout_current_get tag)))]
   )
)

(define layout-name
   (case-lambda
      [()
       (layout-name (layout-current))
      ]

      [(symb)
       (layout->name (symb->layout symb))
      ]
   )
)

(define layout-set
   (case-lambda
        [(l)
         (layout-set (current-frame) l)]

        [(tag l)
         (call-foreign (__cs_layout_current_set tag  l))
         (run-hooks 'layout-changed-hook)
        ]
   )
)

(define layout-set-tiled
   (case-lambda
        [()
         (layout-set-tiled (current-frame))]

        [(tag)
         (layout-set tag (symb->layout 'tiled))]
   )
)

(define layout-set-grid
   (case-lambda
        [()
         (layout-set-grid (current-frame))]

        [(tag)
         (layout-set tag (symb->layout 'grid))]
   )
)

(define layout-set-bstack
   (case-lambda
        [()
         (layout-set-bstack (current-frame))]

        [(tag)
         (layout-set tag (symb->layout 'bstack))]
   )
)

(define layout-set-maximized
   (case-lambda
        [()
         (layout-set-maximized (current-frame))]

        [(tag)
         (layout-set tag (symb->layout 'maximized))]
   )
)

(define layout-is-tiled?
   (case-lambda
        [()
         (equal? 'tiled (layout-current))]

        [(tag)
         (equal? 'tiled (layout-current tag))]
   )
)

(define layout-is-grid?
   (case-lambda
        [()
         (equal? 'grid (layout-current))]

        [(tag)
         (equal? 'grid (layout-current tag))]
   )
)

(define layout-is-bstack?
   (case-lambda
        [()
         (equal? 'bstack (layout-current))]

        [(tag)
         (equal? 'bstack (layout-current tag))]
   )
)

(define layout-is-maximized?
   (case-lambda
        [()
         (equal? 'maximized (layout-current))]

        [(tag)
         (equal? 'maximized (layout-current tag))]
   )
)

(define layout-n-master
   (case-lambda
      [()
       (call-foreign (__cs_layout_nmaster_get (current-frame)))]

      [(tag)
       (call-foreign (__cs_layout_nmaster_get tag))]
   )
)

(define layout-set-n-master
   (case-lambda
      [(n)
       (call-foreign (__cs_layout_nmaster_set (current-frame) n))]

      [(tag n)
       (call-foreign (__cs_layout_nmaster_set tag n))]
   )
)

(define layout-n-master+
   (case-lambda
      [()
       (call-foreign (__cs_layout_nmaster_set
          (current-frame) (+ (call-foreign (__cs_layout_nmaster_get (current-frame))) 1)
       ))]

      [(n)
       (call-foreign (__cs_layout_nmaster_set
          (current-frame) (+ (call-foreign (__cs_layout_nmaster_get (current-frame))) n)
       ))]

      [(tag n)
       (call-foreign (__cs_layout_nmaster_set
          tag (+ (call-foreign (__cs_layout_nmaster_get tag)) n)
       ))]
   )
)

(define layout-n-master-
   (case-lambda
      [()
       (call-foreign (__cs_layout_nmaster_set
          (current-frame) (- (call-foreign (__cs_layout_nmaster_get (current-frame))) 1)
       ))]

      [(n)
       (call-foreign (__cs_layout_nmaster_set
          (current-frame) (- (call-foreign (__cs_layout_nmaster_get (current-frame))) n)
       ))]

      [(tag n)
       (call-foreign (__cs_layout_nmaster_set
          tag (- (call-foreign (__cs_layout_nmaster_get tag)) n)
       ))]
   )
)

(define layout-%-master
   (case-lambda
      [()
       (call-foreign (__cs_layout_fmaster_get (current-frame)))]

      [(tag)
       (call-foreign (__cs_layout_fmaster_get tag))]
   )
)

(define layout-set-%-master
   (case-lambda
      [(f)
       (call-foreign (__cs_layout_fmaster_set (current-frame) f))]

      [(tag f)
       (call-foreign (__cs_layout_fmaster_set tag f))]
   )
)

(define layout-%-master+
   (case-lambda
      [()
       (call-foreign (__cs_layout_fmaster_set
          (current-frame) (+ (call-foreign (__cs_layout_fmaster_get (current-frame))) 0.05)
       ))]

      [(f)
       (call-foreign (__cs_layout_fmaster_set
          (current-frame) (+ (call-foreign (__cs_layout_fmaster_get (current-frame))) f)
       ))]

      [(tag f)
       (call-foreign (__cs_layout_fmaster_set
          tag (+ (call-foreign (__cs_layout_fmaster_get tag)) f)
       ))]
   )
)

(define layout-%-master-
   (case-lambda
      [()
       (call-foreign (__cs_layout_fmaster_set
          (current-frame) (- (call-foreign (__cs_layout_fmaster_get (current-frame))) 0.05)
       ))]

      [(f)
       (call-foreign (__cs_layout_fmaster_set
          (current-frame) (- (call-foreign (__cs_layout_fmaster_get (current-frame))) f)
       ))]

      [(tag f)
       (call-foreign (__cs_layout_fmaster_set
          tag (- (call-foreign (__cs_layout_fmaster_get tag)) f)
       ))]
   )
)

(define layout-set-sticky
   (case-lambda
        [(s)
         (call-foreign (__cs_layout_sticky_set (current-frame) s))]

        [(tag s)
         (call-foreign (__cs_layout_sticky_set tag s))]
   )
)

(define layout-is-sticky?
   (case-lambda
        [()
         (call-foreign (__cs_layout_sticky_get (current-frame)))]

        [(tag)
         (call-foreign (__cs_layout_sticky_get tag))]
   )
)

(define layout-toggle-sticky
   (case-lambda
      [()
       (layout-set-sticky (not (layout-is-sticky?)))]

      [(tag)
       (layout-set-sticky tag (not (layout-is-sticky? tag)))]
   )
)
