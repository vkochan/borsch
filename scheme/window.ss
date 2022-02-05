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
(define __cs_win_viewport_pos (foreign-procedure __collect_safe "cs_win_viewport_pos" (int char) scheme-object))
(define __cs_win_viewport_coord (foreign-procedure __collect_safe "cs_win_viewport_coord" (int int) scheme-object))
(define __cs_win_scroll (foreign-procedure __collect_safe "cs_win_scroll" (int char int) scheme-object))
(define __cs_win_sidebar_set (foreign-procedure __collect_safe "cs_win_sidebar_set" (int int) void))
(define __cs_win_sidebar_get (foreign-procedure __collect_safe "cs_win_sidebar_get" (int) scheme-object))
(define __cs_win_sidebar_draw (foreign-procedure "cs_win_sidebar_draw" (int int int string int int int) scheme-object))

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
                                    (list win (window-name win) (window-buffer win))
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

(define current-window
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
       (window-toggle-minimized (current-window))]

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
       (window-toggle-maximized (current-window))]

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

(define window-viewport-begin
   (case-lambda
      [()
       (window-viewport-begin (current-window))]

      [(w)
       (__cs_win_viewport_pos w #\H)]
   )
)

(define window-viewport-end
   (case-lambda
      [()
       (window-viewport-end (current-window))]

      [(w)
       (__cs_win_viewport_pos w #\L)]
   )
)

(define window-viewport-coord
   (case-lambda
      [(p)
       (window-viewport-coord (current-window) p)]

      [(w p)
       (__cs_win_viewport_coord w p)]
   )
)

(define window-viewport-lines-coord
   (lambda (w)
      (when (not (buffer-is-term? (window-buffer w)))
         (let (
               [b (window-buffer w)]
              )
               (let* (
                      [start (window-viewport-begin w)]
                      [end (line-begin-pos b (window-viewport-end w))]
                      [coord (window-viewport-coord w start)]
                      [lst '()]
                     )
                  (while (and (<= start end) (not (= start (buffer-end-pos))))
                     (when coord
                        (set! lst (append lst (list coord)))
                     )
                     (set! start (next-line-pos b start))
                     (set! coord (window-viewport-coord w start))
                  )
		  lst
               )
         )
      )
   )
)

(define window-scroll-page-down
   (case-lambda
      [()
       (window-scroll-page-down (current-window))]

      [(w)
       (__cs_win_scroll w #\f 0)]
   )
)

(define window-scroll-page-up
   (case-lambda
      [()
       (window-scroll-page-up (current-window))]

      [(w)
       (__cs_win_scroll w #\b 0)]
   )
)

(define window-scroll-halfpage-down
   (case-lambda
      [()
       (window-scroll-halfpage-down (current-window))]

      [(w)
       (__cs_win_scroll w #\d 0)]
   )
)

(define window-scroll-halfpage-up
   (case-lambda
      [()
       (window-scroll-halfpage-up (current-window))]

      [(w)
       (__cs_win_scroll w #\u 0)]
   )
)

(define window-scroll-down
   (case-lambda
      [(n)
       (window-scroll-down (current-window))]

      [(w n)
       (__cs_win_scroll w #\l n)]
   )
)

(define window-scroll-up
   (case-lambda
      [(n)
       (window-scroll-down (current-window))]

      [(w n)
       (__cs_win_scroll w #\L n)]
   )
)

(define window-set-sidebar-width
   (case-lambda
      [(l)
       (window-set-sidebar-width (current-window) l)]

      [(w l)
       (__cs_win_sidebar_set w l)]
   )
)

(define window-sidebar-width
   (case-lambda
      [()
       (window-sidebar-width (current-window))]

      [(w)
       (__cs_win_sidebar_get w)]
   )
)

(define window-draw-sidebar
   (case-lambda
      [(w x y t)
       (window-draw-sidebar w x y t '(:fg "default" :bg "default" :attr "normal"))
      ]

      [(w x y t s)
       (let ([l (style->list  s)])
         (__cs_win_sidebar_draw w x y t
                                (list-ref l 0)
                                (list-ref l 1)
                                (list-ref l 2))
       )
      ]
   )
)
