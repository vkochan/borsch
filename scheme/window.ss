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
(define __cs_win_create (foreign-procedure "cs_win_create" (string string) scheme-object))
(define __cs_win_new (foreign-procedure "cs_win_new" () scheme-object))
(define __cs_win_del (foreign-procedure __collect_safe "cs_win_del" (int) int))
(define __cs_win_title_get (foreign-procedure __collect_safe "cs_win_title_get" (int) scheme-object))
(define __cs_win_title_set (foreign-procedure "cs_win_title_set" (int string) int))
(define __cs_win_tag_set (foreign-procedure __collect_safe "cs_win_tag_set" (int int) int))
(define __cs_win_tag_toggle (foreign-procedure __collect_safe "cs_win_tag_toggle" (int int) int))
(define __cs_win_tag_add (foreign-procedure __collect_safe "cs_win_tag_add" (int int) int))
(define __cs_win_tag_del (foreign-procedure __collect_safe "cs_win_tag_del" (int int) int))
(define __cs_win_state_get(foreign-procedure __collect_safe "cs_win_state_get" (int) int))
(define __cs_win_state_set(foreign-procedure __collect_safe "cs_win_state_set" (int int) int))
(define __cs_win_state_toggle(foreign-procedure __collect_safe "cs_win_state_toggle" (int int) int))
(define __cs_win_keys_send (foreign-procedure "cs_win_keys_send" (int string) int))
(define __cs_win_text_send (foreign-procedure "cs_win_text_send" (int string) int))
(define __cs_win_buf_get (foreign-procedure __collect_safe "cs_win_buf_get" (int) scheme-object))
(define __cs_win_mark_highlight (foreign-procedure __collect_safe "cs_win_mark_highlight" (int boolean) void))

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

(define window-select
   (lambda (wid)
      (__cs_win_current_set wid)
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

(define window-new
   (lambda ()
      (__cs_win_new)
   )
)

(define window-shell
   (case-lambda
      [()
       (__cs_win_create #f "")]

      [(prog)
       (__cs_win_create prog "")]

      [(prog title)
       (__cs_win_create prog title)]
   )
)

(define window-delete
    (case-lambda
       [()
	(__cs_win_del (__cs_win_current_get))]

       [(wid)
	(__cs_win_del wid)]
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
       (__window-set 'minimized)]

      [(wid)
       (__window-set wid 'minimized)]
   )
)

(define window-set-maximized
   (case-lambda
      [()
       (__window-set 'maximized)]

      [(wid)
       (__window-set wid 'maximized)]
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
       (__window-toggle 'minimized)]

      [(wid)
       (__window-toggle wid 'minimized)]
   )
)

(define window-toggle-maximized
   (case-lambda
      [()
       (__window-toggle 'maximized)]

      [(wid)
       (__window-toggle wid 'maximized)]
   )
)

(define window-send-keys
   (case-lambda
      [(keys)
       (__cs_win_keys_send (__cs_win_current_get) keys)]

      [(wid keys)
       (__cs_win_keys_send wid keys)]
   )
)

(define window-send-text
   (case-lambda
      [(text)
       (__cs_win_text_send (__cs_win_current_get) text)]

      [(wid text)
       (__cs_win_text_send wid text)]
   )
)

(define window-pager
   (case-lambda
      [()
       (__cs_win_pager_mode (__cs_win_current_get))]

      [(wid)
       (__cs_win_pager_mode wid)]
   )
)

(define window-eval
   (lambda ()
      (window-shell "borsch-eval -i" "eval")
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

(define window-highlight-mark
   (case-lambda
      [(e)
       (__cs_win_mark_highlight (__cs_win_current_get) e)]

      [(wid e)
       (__cs_win_mark_highlight wid e)]
   )
)
