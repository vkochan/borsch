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
         (layout-current (current-frame))]

        [(fr)
         (layout->symb (call-foreign (__cs_layout_current_get (frame-id fr))))]
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

        [(fr l)
         (frame-set-prev-layout fr (layout-current fr))
         (call-foreign (__cs_layout_current_set (frame-id fr)  l))
         (run-hooks 'layout-changed-hook)
        ]
   )
)

(define layout-set-tiled
   (case-lambda
        [()
         (layout-set-tiled (current-frame))]

        [(fr)
         (layout-set fr (symb->layout 'tiled))]
   )
)

(define layout-set-grid
   (case-lambda
        [()
         (layout-set-grid (current-frame))]

        [(fr)
         (layout-set fr (symb->layout 'grid))]
   )
)

(define layout-set-bstack
   (case-lambda
        [()
         (layout-set-bstack (current-frame))]

        [(fr)
         (layout-set fr (symb->layout 'bstack))]
   )
)

(define layout-set-maximized
   (case-lambda
        [()
         (layout-set-maximized (current-frame))]

        [(fr)
         (layout-set fr (symb->layout 'maximized))]
   )
)

(define layout-toggle-maximized
   (case-lambda
      [()
       (layout-toggle-maximized (current-frame))]

      [(fr)
       (if (equal? (layout-current fr) 'maximized)
          (layout-set fr (symb->layout (frame-prev-layout fr)))
          ;; else
          (layout-set fr (symb->layout 'maximized))
       )
      ]
   )
)

(define layout-is-tiled?
   (case-lambda
        [()
         (equal? 'tiled (layout-current))]

        [(fr)
         (equal? 'tiled (layout-current (frame-id fr)))]
   )
)

(define layout-is-grid?
   (case-lambda
        [()
         (equal? 'grid (layout-current))]

        [(fr)
         (equal? 'grid (layout-current (frame-id fr)))]
   )
)

(define layout-is-bstack?
   (case-lambda
        [()
         (equal? 'bstack (layout-current))]

        [(fr)
         (equal? 'bstack (layout-current (frame-id fr)))]
   )
)

(define layout-is-maximized?
   (case-lambda
        [()
         (equal? 'maximized (layout-current))]

        [(fr)
         (equal? 'maximized (layout-current (frame-id fr)))]
   )
)

(define layout-n-master
   (case-lambda
      [()
       (layout-n-master (current-frame))]

      [(fr)
       (call-foreign (__cs_layout_nmaster_get (frame-id fr)))]
   )
)

(define layout-set-n-master
   (case-lambda
      [(n)
       (layout-set-n-master (current-frame) n)]

      [(fr n)
       (call-foreign (__cs_layout_nmaster_set (frame-id fr) n))]
   )
)

(define layout-n-master+
   (case-lambda
      [()
       (layout-n-master+ (current-frame) 1)]

      [(n)
       (layout-n-master+ (current-frame) n)]

      [(fr n)
       (call-foreign (__cs_layout_nmaster_set
          (frame-id fr) (+ (call-foreign (__cs_layout_nmaster_get (frame-id fr))) n)
       ))]
   )
)

(define layout-n-master-
   (case-lambda
      [()
       (layout-n-master- (current-frame) 1)]

      [(n)
       (layout-n-master- (current-frame) n)]

      [(fr n)
       (call-foreign (__cs_layout_nmaster_set
          (frame-id fr) (- (call-foreign (__cs_layout_nmaster_get (frame-id fr))) n)
       ))]
   )
)

(define layout-%-master
   (case-lambda
      [()
       (layout-%-master (current-frame))]

      [(fr)
       (call-foreign (__cs_layout_fmaster_get (frame-id fr)))]
   )
)

(define layout-set-%-master
   (case-lambda
      [(f)
       (layout-set-%-master (current-frame) f)]

      [(fr f)
       (call-foreign (__cs_layout_fmaster_set (frame-id fr) f))]
   )
)

(define layout-%-master+
   (case-lambda
      [()
       (layout-%-master+ (current-frame) 0.05)]

      [(f)
       (layout-%-master+ (current-frame) f)]

      [(fr f)
       (call-foreign (__cs_layout_fmaster_set
          (frame-id fr) (+ (call-foreign (__cs_layout_fmaster_get (frame-id fr))) f)
       ))]
   )
)

(define layout-%-master-
   (case-lambda
      [()
       (layout-%-master- (current-frame) 0.05)]

      [(f)
       (layout-%-master- (current-frame) f)]

      [(fr f)
       (call-foreign (__cs_layout_fmaster_set
          (frame-id fr) (- (call-foreign (__cs_layout_fmaster_get (frame-id fr))) f)
       ))]
   )
)

(define layout-set-sticky
   (case-lambda
        [(s)
         (layout-set-sticky (current-frame) s)]

        [(fr s)
         (call-foreign (__cs_layout_sticky_set (frame-id fr) s))]
   )
)

(define layout-is-sticky?
   (case-lambda
        [()
         (layout-is-sticky? (current-frame))]

        [(fr)
         (call-foreign (__cs_layout_sticky_get (frame-id fr)))]
   )
)

(define layout-toggle-sticky
   (case-lambda
      [()
       (layout-set-sticky (not (layout-is-sticky?)))]

      [(fr)
       (layout-set-sticky fr (not (layout-is-sticky? fr)))]
   )
)
