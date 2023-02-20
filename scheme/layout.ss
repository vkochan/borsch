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

        [(frame)
         (layout->symb (call-foreign (__cs_layout_current_get frame)))]
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

        [(frame l)
         (call-foreign (__cs_layout_current_set frame  l))
         (run-hooks 'layout-changed-hook)
        ]
   )
)

(define layout-set-tiled
   (case-lambda
        [()
         (layout-set-tiled (current-frame))]

        [(frame)
         (layout-set frame (symb->layout 'tiled))]
   )
)

(define layout-set-grid
   (case-lambda
        [()
         (layout-set-grid (current-frame))]

        [(frame)
         (layout-set frame (symb->layout 'grid))]
   )
)

(define layout-set-bstack
   (case-lambda
        [()
         (layout-set-bstack (current-frame))]

        [(frame)
         (layout-set frame (symb->layout 'bstack))]
   )
)

(define layout-set-maximized
   (case-lambda
        [()
         (layout-set-maximized (current-frame))]

        [(frame)
         (layout-set frame (symb->layout 'maximized))]
   )
)

(define layout-is-tiled?
   (case-lambda
        [()
         (equal? 'tiled (layout-current))]

        [(frame)
         (equal? 'tiled (layout-current frame))]
   )
)

(define layout-is-grid?
   (case-lambda
        [()
         (equal? 'grid (layout-current))]

        [(frame)
         (equal? 'grid (layout-current frame))]
   )
)

(define layout-is-bstack?
   (case-lambda
        [()
         (equal? 'bstack (layout-current))]

        [(frame)
         (equal? 'bstack (layout-current frame))]
   )
)

(define layout-is-maximized?
   (case-lambda
        [()
         (equal? 'maximized (layout-current))]

        [(frame)
         (equal? 'maximized (layout-current frame))]
   )
)

(define layout-n-master
   (case-lambda
      [()
       (call-foreign (__cs_layout_nmaster_get (current-frame)))]

      [(frame)
       (call-foreign (__cs_layout_nmaster_get frame))]
   )
)

(define layout-set-n-master
   (case-lambda
      [(n)
       (call-foreign (__cs_layout_nmaster_set (current-frame) n))]

      [(frame n)
       (call-foreign (__cs_layout_nmaster_set frame n))]
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

      [(frame n)
       (call-foreign (__cs_layout_nmaster_set
          frame (+ (call-foreign (__cs_layout_nmaster_get frame)) n)
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

      [(frame n)
       (call-foreign (__cs_layout_nmaster_set
          frame (- (call-foreign (__cs_layout_nmaster_get frame)) n)
       ))]
   )
)

(define layout-%-master
   (case-lambda
      [()
       (call-foreign (__cs_layout_fmaster_get (current-frame)))]

      [(frame)
       (call-foreign (__cs_layout_fmaster_get frame))]
   )
)

(define layout-set-%-master
   (case-lambda
      [(f)
       (call-foreign (__cs_layout_fmaster_set (current-frame) f))]

      [(frame f)
       (call-foreign (__cs_layout_fmaster_set frame f))]
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

      [(frame f)
       (call-foreign (__cs_layout_fmaster_set
          frame (+ (call-foreign (__cs_layout_fmaster_get frame)) f)
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

      [(frame f)
       (call-foreign (__cs_layout_fmaster_set
          frame (- (call-foreign (__cs_layout_fmaster_get frame)) f)
       ))]
   )
)

(define layout-set-sticky
   (case-lambda
        [(s)
         (call-foreign (__cs_layout_sticky_set (current-frame) s))]

        [(frame s)
         (call-foreign (__cs_layout_sticky_set frame s))]
   )
)

(define layout-is-sticky?
   (case-lambda
        [()
         (call-foreign (__cs_layout_sticky_get (current-frame)))]

        [(frame)
         (call-foreign (__cs_layout_sticky_get frame))]
   )
)

(define layout-toggle-sticky
   (case-lambda
      [()
       (layout-set-sticky (not (layout-is-sticky?)))]

      [(frame)
       (layout-set-sticky frame (not (layout-is-sticky? frame)))]
   )
)
