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

(define layout-current
   (case-lambda
        [()
         (layout->symb (__cs_layout_current_get (__cs_view_current_get)))]

        [(tag)
         (layout->symb (__cs_layout_current_get tag))]
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

(define layout-switch
   (case-lambda
        [(l)
         (layout-switch (__cs_view_current_get) l)]

        [(tag l)
         (__cs_layout_current_set tag  l)
         (run-hook 'layout-switch-hook l)
        ]
   )
)

(define layout-switch-tiled
   (case-lambda
        [()
         (layout-switch-tiled (__cs_view_current_get))]

        [(tag)
         (layout-switch tag (symb->layout 'tiled))]
   )
)

(define layout-switch-grid
   (case-lambda
        [()
         (layout-switch-grid (__cs_view_current_get))]

        [(tag)
         (layout-switch tag (symb->layout 'grid))]
   )
)

(define layout-switch-bstack
   (case-lambda
        [()
         (layout-switch-bstack (__cs_view_current_get))]

        [(tag)
         (layout-switch tag (symb->layout 'bstack))]
   )
)

(define layout-switch-maximized
   (case-lambda
        [()
         (layout-switch-maximized (__cs_view_current_get))]

        [(tag)
         (layout-switch tag (symb->layout 'maximized))]
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
       (__cs_layout_nmaster_get (__cs_view_current_get))]

      [(tag)
       (__cs_layout_nmaster_get tag)]
   )
)

(define layout-set-n-master
   (case-lambda
      [(n)
       (__cs_layout_nmaster_set (__cs_view_current_get) n)]

      [(tag n)
       (__cs_layout_nmaster_set tag n)]
   )
)

(define layout-n-master+
   (case-lambda
      [()
       (__cs_layout_nmaster_set
          (__cs_view_current_get) (+ (__cs_layout_nmaster_get (__cs_view_current_get)) 1)
       )]

      [(n)
       (__cs_layout_nmaster_set
          (__cs_view_current_get) (+ (__cs_layout_nmaster_get (__cs_view_current_get)) n)
       )]

      [(tag n)
       (__cs_layout_nmaster_set
          tag (+ (__cs_layout_nmaster_get tag) n)
       )]
   )
)

(define layout-n-master-
   (case-lambda
      [()
       (__cs_layout_nmaster_set
          (__cs_view_current_get) (- (__cs_layout_nmaster_get (__cs_view_current_get)) 1)
       )]

      [(n)
       (__cs_layout_nmaster_set
          (__cs_view_current_get) (- (__cs_layout_nmaster_get (__cs_view_current_get)) n)
       )]

      [(tag n)
       (__cs_layout_nmaster_set
          tag (- (__cs_layout_nmaster_get tag) n)
       )]
   )
)

(define layout-%-master
   (case-lambda
      [()
       (__cs_layout_fmaster_get (__cs_view_current_get))]

      [(tag)
       (__cs_layout_fmaster_get tag)]
   )
)

(define layout-set-%-master
   (case-lambda
      [(f)
       (__cs_layout_fmaster_set (__cs_view_current_get) f)]

      [(tag f)
       (__cs_layout_fmaster_set tag f)]
   )
)

(define layout-%-master+
   (case-lambda
      [()
       (__cs_layout_fmaster_set
          (__cs_view_current_get) (+ (__cs_layout_fmaster_get (__cs_view_current_get)) 0.05)
       )]

      [(f)
       (__cs_layout_fmaster_set
          (__cs_view_current_get) (+ (__cs_layout_fmaster_get (__cs_view_current_get)) f)
       )]

      [(tag f)
       (__cs_layout_fmaster_set
          tag (+ (__cs_layout_fmaster_get tag) f)
       )]
   )
)

(define layout-%-master-
   (case-lambda
      [()
       (__cs_layout_fmaster_set
          (__cs_view_current_get) (- (__cs_layout_fmaster_get (__cs_view_current_get)) 0.05)
       )]

      [(f)
       (__cs_layout_fmaster_set
          (__cs_view_current_get) (- (__cs_layout_fmaster_get (__cs_view_current_get)) f)
       )]

      [(tag f)
       (__cs_layout_fmaster_set
          tag (- (__cs_layout_fmaster_get tag) f)
       )]
   )
)

(define layout-set-sticky
   (case-lambda
        [(s)
         (__cs_layout_sticky_set (__cs_view_current_get) s)]

        [(tag s)
         (__cs_layout_sticky_set tag s)]
   )
)

(define layout-is-sticky?
   (case-lambda
        [()
         (__cs_layout_sticky_get (__cs_view_current_get))]

        [(tag)
         (__cs_layout_sticky_get tag)]
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
