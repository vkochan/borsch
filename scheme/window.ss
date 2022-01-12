(define __cs_win_get_by_coord (foreign-procedure __collect_safe "cs_win_get_by_coord" (int int) scheme-object))
(define __cs_win_is_visible (foreign-procedure __collect_safe "cs_win_is_visible" (int) boolean))
(define __cs_win_first_get (foreign-procedure __collect_safe "cs_win_first_get" () scheme-object))
(define __cs_win_prev_get (foreign-procedure __collect_safe "cs_win_prev_get" (int) scheme-object))
(define __cs_win_next_get (foreign-procedure __collect_safe "cs_win_next_get" (int) scheme-object))
(define __cs_win_upper_get (foreign-procedure __collect_safe "cs_win_upper_get" (int) scheme-object))
(define __cs_win_lower_get (foreign-procedure __collect_safe "cs_win_lower_get" (int) scheme-object))
(define __cs_win_right_get (foreign-procedure __collect_safe "cs_win_right_get" (int) scheme-object))
(define __cs_win_left_get (foreign-procedure __collect_safe "cs_win_left_get" (int) scheme-object))
(define __cs_win_current_get (foreign-procedure __collect_safe "cs_win_current_get" () scheme-object))
(define __cs_win_current_set (foreign-procedure __collect_safe "cs_win_current_set" (int) int))
(define __cs_win_new (foreign-procedure "cs_win_new" (int) scheme-object))
(define __cs_win_del (foreign-procedure __collect_safe "cs_win_del" (int) int))
(define __cs_win_close (foreign-procedure __collect_safe "cs_win_close" (int) int))
(define __cs_win_title_get (foreign-procedure __collect_safe "cs_win_title_get" (int) scheme-object))
(define __cs_win_title_set (foreign-procedure "cs_win_title_set" (int string) int))
(define __cs_win_tag_set (foreign-procedure __collect_safe "cs_win_tag_set" (int int) int))
(define __cs_win_tag_toggle (foreign-procedure __collect_safe "cs_win_tag_toggle" (int int) int))
(define __cs_win_tag_add (foreign-procedure __collect_safe "cs_win_tag_add" (int int) int))
(define __cs_win_tag_del (foreign-procedure __collect_safe "cs_win_tag_del" (int int) int))
(define __cs_win_state_get(foreign-procedure __collect_safe "cs_win_state_get" (int) int))
(define __cs_win_state_set(foreign-procedure __collect_safe "cs_win_state_set" (int int) int))
(define __cs_win_state_toggle(foreign-procedure __collect_safe "cs_win_state_toggle" (int int) int))
(define __cs_win_buf_get (foreign-procedure __collect_safe "cs_win_buf_get" (int) scheme-object))
(define __cs_win_popup (foreign-procedure __collect_safe "cs_win_popup" (int boolean) scheme-object))
(define __cs_win_size_set (foreign-procedure __collect_safe "cs_win_size_set" (int int int) void))
(define __cs_win_border_set (foreign-procedure __collect_safe "cs_win_border_set" (int boolean) void))
(define __cs_win_buf_switch (foreign-procedure __collect_safe "cs_win_buf_switch" (int int) void))
(define __cs_win_prev_selected (foreign-procedure __collect_safe "cs_win_prev_selected" () scheme-object))

(define window-first
   (lambda ()
       (__cs_win_first_get)
   )
)

(define window-next
   (case-lambda
      [()
       (__cs_win_next_get (__cs_win_current_get))]

      [(wid)
       (__cs_win_next_get wid)]
   )
)

(define window-prev
   (case-lambda
      [()
       (__cs_win_prev_get (__cs_win_current_get))]

      [(wid)
       (__cs_win_prev_get wid)]
   )
)

(define window-list
   (lambda ()
      (let ([win (window-first)]
            [lst   '()]
	   )

         (while win
            (set! lst (append lst (list
                                    (list win (window-name win) (window-buffer))
				  )
            )         )
            (set! win (window-next win))
         )

	 lst
      )
   )
)

(define window-upper
   (case-lambda
      [()
       (__cs_win_upper_get (__cs_win_current_get))]

      [(wid)
       (__cs_win_upper_get wid)]
   )
)

(define window-lower
   (case-lambda
      [()
       (__cs_win_lower_get (__cs_win_current_get))]

      [(wid)
       (__cs_win_lower_get wid)]
   )
)

(define window-right
   (case-lambda
      [()
       (__cs_win_right_get (__cs_win_current_get))]

      [(wid)
       (__cs_win_right_get wid)]
   )
)

(define window-left
   (case-lambda
      [()
       (__cs_win_left_get (__cs_win_current_get))]

      [(wid)
       (__cs_win_left_get wid)]
   )
)

(define window-current
   (lambda ()
      (__cs_win_current_get)
   )
)

(define window-prev-selected
   (lambda ()
       (__cs_win_prev_selected)
   )
)

(define window-select
   (lambda (wid)
      (__cs_win_current_set wid)
      (run-hooks 'window-select-hook wid)
   )
)

(define window-select-left
   (lambda ()
      (window-select (window-left))
   )
)

(define window-select-right
   (lambda ()
      (window-select (window-right))
   )
)

(define window-select-upper
   (lambda ()
      (window-select (window-upper))
   )
)

(define window-select-lower
   (lambda ()
      (window-select (window-lower))
   )
)

(define window-create
   (case-lambda
      [()
       (window-create 0)]

      [(b)
       (let ([w (__cs_win_new b)])
         (when w (run-hooks 'window-create-hook w))
         w
       )
      ]
   )
)

(define window-delete
    (case-lambda
       [()
	(window-delete (__cs_win_current_get))]

       [(w)
        (let ([n (buffer-name (window-buffer w))])
           (if (equal? n "*Messages*")
              (window-close w)
              ;; else
              (begin
	         (__cs_win_del w)
                 (run-hooks 'window-delete-hook w)
              )
           )
        )
       ]
    )
)

(define window-close
    (case-lambda
       [()
	(window-close (__cs_win_current_get))]

       [(w)
	(__cs_win_close w)
        (run-hooks 'window-close-hook w)
       ]
    )
)

(define window-name
   (case-lambda
      [()
       (__cs_win_title_get (__cs_win_current_get))]

      [(wid)
       (__cs_win_title_get wid)]
   )
)

(define window-set-name
   (case-lambda
      [(title)
       (__cs_win_title_set (__cs_win_current_get) title)]

      [(wid title)
       (__cs_win_title_set wid title)]
   )
)

(define window-set-tag
   (case-lambda
      [(tag)
       (__cs_win_tag_set (__cs_win_current_get) tag)]

      [(wid tag)
       (__cs_win_tag_set wid tag)]
   )
)

(define window-toggle-tag
   (case-lambda
      [(tag)
       (__cs_win_tag_toggle (__cs_win_current_get) tag)]

      [(wid tag)
       (__cs_win_tag_toggle wid tag)]
   )
)

(define window-tag+
   (case-lambda
      [(tag)
       (__cs_win_tag_add (__cs_win_current_get) tag)]

      [(wid tag)
       (__cs_win_tag_add wid tag)]
   )
)

(define window-tag-
   (case-lambda
      [(tag)
       (__cs_win_tag_del (__cs_win_current_get) tag)]

      [(wid tag)
       (__cs_win_tag_del wid tag)]
   )
)

(define win-state->symb
   (lambda (st)
      (case st
         [0 'minimized]
	 [1 'maximized]
         [2 'master   ]
      )
   )
)

(define __window-get
   (case-lambda
      [(st)
       (win-state->symb (__cs_win_state_get (__cs_win_current_get)))]

      [(wid st)
       (win-state->symb (__cs_win_state_get (wid)))]
   )
)

(define window-is-minimized?
   (case-lambda
      [()
       (equal? (__window-get) 'minimized)]

      [(wid)
       (equal? (__window-get wid) 'minimized)]
   )
)

(define window-is-maximized?
   (case-lambda
      [()
       (equal? (__window-get) 'maximized)]

      [(wid)
       (equal? (__window-get wid) 'maximized)]
   )
)

(define window-is-master?
   (case-lambda
      [()
       (equal? (__window-get) 'master)]

      [(wid)
       (equal? (__window-get wid) 'master)]
   )
)

(define symb->win-state
   (lambda (sym)
      (case sym
         ['minimized 0]
         ['maximized 1]
         ['master    2]
      )
   )
)

(define __window-set
   (case-lambda
      [(st)
       (__cs_win_state_set (__cs_win_current_get) (symb->win-state st))]

      [(wid st)
       (__cs_win_state_set wid (symb->win-state st))]
   )
)

(define window-set-minimized
   (case-lambda
      [()
       (window-set-minimized (__cs_win_current_get))]

      [(wid)
       (__window-set wid 'minimized)
       (run-hooks 'window-minimize-hook wid)
      ]
   )
)

(define window-set-maximized
   (case-lambda
      [()
       (window-set-maximized (__cs_win_current_get))]

      [(wid)
       (__window-set wid 'maximized)
       (run-hooks 'window-maximize-hook wid)
      ]
   )
)

(define window-set-master
   (case-lambda
      [()
       (__window-set 'master)]

      [(wid)
       (__window-set wid 'master)]
   )
)

(define __window-toggle
   (case-lambda
      [(st)
       (__cs_win_state_toggle (__cs_win_current_get) (symb->win-state st))]

      [(wid st)
       (__cs_win_state_toggle wid (symb->win-state st))]
   )
)

(define window-toggle-minimized
   (case-lambda
      [()
       (window-toggle-minimized (window-current))]

      [(wid)
       (__window-toggle wid 'minimized)
       (when (window-is-minimized? wid)
          (run-hooks 'window-minimize-hook wid)
       )
      ]
   )
)

(define window-toggle-maximized
   (case-lambda
      [()
       (window-toggle-maximized (window-current))]

      [(wid)
       (__window-toggle wid 'maximized)
       (when (window-is-maximized? wid)
          (run-hooks 'window-maximize-hook wid)
       )
      ]
   )
)

(define window-buffer
   (case-lambda
      [()
       (__cs_win_buf_get (__cs_win_current_get))]

      [(wid)
       (__cs_win_buf_get wid)]
   )
)

(define window-popup
   (case-lambda
      [(e)
       (__cs_win_popup (__cs_win_current_get) e)]

      [(wid e)
       (__cs_win_popup wid e)]
   )
)

(define window-set-width
   (case-lambda
      [(w)
       (__cs_win_size_set (__cs_win_current_get) w -1)]

      [(wid w)
       (__cs_win_size_set wid w -1)]
   )
)

(define window-set-height
   (case-lambda
      [(h)
       (__cs_win_size_set (__cs_win_current_get) -1 h)]

      [(wid h)
       (__cs_win_size_set wid -1 h)]
   )
)

(define window-set-border
   (case-lambda
      [(h)
       (__cs_win_border_set (__cs_win_current_get) -1 h)]

      [(wid h)
       (__cs_win_border_set wid -1 h)]
   )
)

(define window-switch-buffer
   (case-lambda
      [(b)
       (__cs_win_buf_switch (__cs_win_current_get) b)]

      [(wid b)
       (__cs_win_buf_switch wid b)]
   )
)
