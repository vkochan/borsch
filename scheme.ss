;; Misc helpers
(define file>
   (lambda (file str)
      (let ([p (open-output-file file 'truncate)])
          (put-string p str)
	  (close-port p)
      )
   )
)

(define file>>
   (lambda (file str)
      (let ([p (open-output-file file 'append)])
          (put-string p str)
	  (close-port p)
      )
   )
)

(define swap
   (lambda (x y)
      (let ([t x])
	 (set! x y)
	 (set! y t)
      )
   )
)

(define first car)

(define second
   (lambda (ls)
      (car (cdr ls))
   )
)

(define empty?
   (lambda (ls)
      (> (length ls) 0)
   )
)

(define % modulo)

(define fmt format)

(define lines-count
   (lambda (str)
      (let ([n (string-length str)]
            [i 0]
            [c 0])
         (do ([i i (1+ i)])
            ((>= i n ))

            (if (char=? (string-ref str i) #\newline)
               (set! c (1+ c))
            )
         )
         c
      )
   )
)

(define any->str
   (lambda (x)
      (call-with-string-output-port
         (lambda (p)
            (pretty-print x p)
         )
      )
   )
)

(define eval-port->str
   (lambda (p)
      (let ([e (eval (read p))])
         (if (and (not (equal? e #!eof))
                  (not (equal? e (void)))
		  )
            (if (and (list? e) (not (null? e)) (list? (first e)))
               (with-output-to-string
                  (lambda ()
                     (display "(\n")
                     (for-each
                        (lambda (i)
                           (display "   ")
                           (pretty-print i)
                        )
                        e
                     )
                     (display ")\n")
		  )
               )
               ;; else
               (any->str e)
            )
            ;; else
            ""
         )
      )
   )
)

(define sym->str symbol->string)

(define-syntax (while stx)
  (syntax-case stx ()
	       ((_ condition expression ...)
		#`(do ()
		    ((not condition))
		    expression
		    ...))))
;;

;; Event handling
(define __on-event-cb-list '())

(define err->str
   (lambda (e)
      (let-values ([(op g) (open-string-output-port)])
         (display-condition e op)
         (g)
      )
   )
)

(define (try fn . args)
   (let ([fn fn]
         [args args])
     (append '() (call/cc
        (lambda (k)
           (with-exception-handler
              (lambda (x)
                 (k (list 1 (err->str x)))
              )

              (lambda ()
	         (k (list 0 (apply fn args)))
              )
           )
        )
     ))
   )
)

(define __do-eval-file
   (lambda (in out)
      (let* ([ip (open-input-file in)]
            [op (open-output-file out 'truncate)]
            [out ""]
            [ret '()])

         (set! out (with-output-to-string
                      (lambda ()
                         (set! ret (try eval-port->str ip))
                      )
                   )
         )

	 (put-string op out)

         (put-string op (second ret))

         (if (not (= (first ret) 0))
	    (put-string op "\n")
         )

	 (flush-output-port op)
	 (close-port ip)
	 (close-port op)

	 (first ret)
      )
   )
)

(define __on-event-handler
   (lambda (ev wid)
       (define __evt->symb
	  (lambda (ev)
	     (case ev
	        [0  'window-create   ]
		[1  'window-switch   ]
		[2  'window-minimize ]
		[3  'window-maximize ]
		[4  'window-delete   ]
		[5  'window-copy     ]
		[20 'view-switch  ]
		[40 'layout-switch]
		[else #f]
             )
          )
       )

       (for-each
	  (lambda (f)
	     (try f (__evt->symb ev) wid)
          )
	  __on-event-cb-list
       )
   )
)
;;

;; FFI
(define key-cb
   (lambda (p)
      (let ([code (foreign-callable (lambda () (try p)) () void)])
	 (lock-object code)
	 (foreign-callable-entry-point code)
      )
   )
)

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
(define __cs_win_create (foreign-procedure "cs_win_create" (string) scheme-object))
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
(define __cs_win_pager_mode (foreign-procedure __collect_safe "cs_win_pager_mode" (int) int))
(define __cs_win_copy_mode (foreign-procedure __collect_safe "cs_win_copy_mode" (int) int))
(define __cs_win_capture (foreign-procedure __collect_safe "cs_win_capture" (int) scheme-object))
(define __cs_win_buf_get (foreign-procedure __collect_safe "cs_win_buf_get" (int) scheme-object))

(define __cs_buf_current_get (foreign-procedure __collect_safe "cs_buf_current_get" () scheme-object))
(define __cs_buf_name_get (foreign-procedure __collect_safe "cs_buf_name_get" (int) scheme-object))
(define __cs_buf_name_set (foreign-procedure "cs_buf_name_set" (int string) void))
(define __cs_buf_by_name (foreign-procedure "cs_buf_by_name" (string) scheme-object))
(define __cs_buf_text_insert (foreign-procedure "cs_buf_text_insert" (int string) void))

(define __cs_view_current_get (foreign-procedure __collect_safe "cs_view_current_get" () int))
(define __cs_view_current_set (foreign-procedure __collect_safe "cs_view_current_set" (int) int))
(define __cs_view_name_get (foreign-procedure __collect_safe "cs_view_name_get" (int) scheme-object))
(define __cs_view_name_set (foreign-procedure "cs_view_name_set" (int string) int))
(define __cs_view_cwd_get (foreign-procedure __collect_safe "cs_view_cwd_get" (int) scheme-object))
(define __cs_view_cwd_set (foreign-procedure "cs_view_cwd_set" (int string) int))

(define __cs_layout_current_get (foreign-procedure __collect_safe "cs_layout_current_get" (int) int))
(define __cs_layout_current_set (foreign-procedure __collect_safe "cs_layout_current_set" (int int) int))
(define __cs_layout_nmaster_get (foreign-procedure __collect_safe "cs_layout_nmaster_get" (int) int))
(define __cs_layout_nmaster_set (foreign-procedure __collect_safe "cs_layout_nmaster_set" (int int) int))
(define __cs_layout_fmaster_get (foreign-procedure __collect_safe "cs_layout_fmaster_get" (int) float))
(define __cs_layout_fmaster_set (foreign-procedure __collect_safe "cs_layout_fmaster_set" (int float) int))
(define __cs_layout_sticky_get (foreign-procedure __collect_safe "cs_layout_sticky_get" (int) boolean))
(define __cs_layout_sticky_set (foreign-procedure __collect_safe "cs_layout_sticky_set" (int boolean) int))

(define __cs_tagbar_status_align (foreign-procedure __collect_safe "cs_tagbar_status_align" (int) int))
(define __cs_tagbar_show (foreign-procedure __collect_safe "cs_tagbar_show" (boolean) int))
(define __cs_tagbar_status_set (foreign-procedure "cs_tagbar_status_set" (string) int))

(define __cs_bind_key (foreign-procedure "cs_bind_key" (string void*) int))
(define __cs_unbind_key (foreign-procedure "cs_unbind_key" (string) int))

(define __cs_copy_buf_get (foreign-procedure __collect_safe "cs_copy_buf_get" () scheme-object))
(define __cs_copy_buf_set (foreign-procedure "cs_copy_buf_set" (string) int))

(define __cs_do_quit (foreign-procedure "cs_do_quit" () void))
;;

;; Public API
(define bind-key
   (lambda (k p)
      (__cs_bind_key k (key-cb p))
   )
)

(define unbind-key
   (lambda (k)
      (__cs_unbind_key k)
   )
)

(define add-hook
   (lambda (f)
      (set! __on-event-cb-list (append __on-event-cb-list (list f)))
   )
)

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

(define window-switch
   (lambda (wid)
      (__cs_win_current_set wid)
   )
)

(define window-switch-left
   (lambda ()
      (window-switch (window-left))
   )
)

(define window-switch-right
   (lambda ()
      (window-switch (window-right))
   )
)

(define window-switch-upper
   (lambda ()
      (window-switch (window-upper))
   )
)

(define window-switch-lower
   (lambda ()
      (window-switch (window-lower))
   )
)

(define window-new
   (lambda ()
      (__cs_win_new)
   )
)

(define window-exec
   (case-lambda
      [()
       (__cs_win_create #f)]

      [(prog)
       (__cs_win_create prog)]
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

(define window-name-set
   (case-lambda
      [(title)
       (__cs_win_title_set (__cs_win_current_get) title)]

      [(wid title)
       (__cs_win_title_set wid title)]
   )
)

(define window-tag-set
   (case-lambda
      [(tag)
       (__cs_win_tag_set (__cs_win_current_get) tag)]

      [(wid tag)
       (__cs_win_tag_set wid tag)]
   )
)

(define window-tag-toggle
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
      (window-exec "borsch-eval -i")
   )
)

(define window-copy
   (case-lambda
      [()
       (__cs_win_copy_mode (__cs_win_current_get))]

      [(wid)
       (__cs_win_copy_mode wid)]
   )
)

(define window-paste
   (case-lambda
      [()
       (window-send-text (copy-buffer))]

      [(wid)
       (window-send-text wid (copy-buffer))]
   )
)

(define window-capture
   (case-lambda
      [()
       (__cs_win_capture (__cs_win_current_get))]

      [(wid)
       (__cs_win_capture wid)]
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

(define buffer-current
   (lambda ()
      (__cs_buf_current_get)
   )
)

(define buffer-name
   (case-lambda
      [()
         (__cs_buf_name_get (__cs_buf_current_get))]

      [(b)
         (__cs_buf_name_get b)]
   )
)

(define buffer-name-set
   (case-lambda
      [(n)
         (__cs_buf_name_set (__cs_buf_current_get) n)]

      [(b n)
         (__cs_buf_name_set b n)]
   )
)

(define buffer-get
   (lambda (n)
      (__cs_buf_by_name n)
   )
)

(define text-insert
   (case-lambda
      [(t)
      (__cs_buf_text_insert (__cs_buf_current_get) t)]

      [(b t)
      (__cs_buf_text_insert b t)]
   )
)

(define view-current
   (lambda ()
      (__cs_view_current_get)
   )
)

(define view-switch
   (lambda (tag)
      (__cs_view_current_set tag)
   )
)

(define view-switch-1
   (lambda ()
      (view-switch 1)
   )
)

(define view-switch-2
   (lambda ()
      (view-switch 2)
   )
)

(define view-switch-3
   (lambda ()
      (view-switch 3)
   )
)

(define view-switch-4
   (lambda ()
      (view-switch 4)
   )
)

(define view-switch-5
   (lambda ()
      (view-switch 5)
   )
)

(define view-switch-6
   (lambda ()
      (view-switch 6)
   )
)

(define view-switch-7
   (lambda ()
      (view-switch 7)
   )
)

(define view-switch-8
   (lambda ()
      (view-switch 8)
   )
)

(define view-switch-9
   (lambda ()
      (view-switch 9)
   )
)

(define view-switch-all
   (lambda ()
      (view-switch 0)
   )
)

(define view-name
   (case-lambda
      [()
       (__cs_view_name_get (__cs_view_current_get))]

      [(tag)
       (__cs_view_name_get tag)]
   )
)

(define view-name-set
   (case-lambda
      [(name)
       (__cs_view_name_set (__cs_view_current_get) name)]

      [(tag name)
       (__cs_view_name_set tag name)]
   )
)

(define view-cwd
   (case-lambda
      [()
       (__cs_view_cwd_get (__cs_view_current_get))]

      [(tag)
       (__cs_view_cwd_get tag)]
   )
)

(define view-cwd-set
   (case-lambda
      [(cwd)
       (__cs_view_cwd_set (__cs_view_current_get) cwd)]

      [(tag cwd)
       (__cs_view_cwd_set tag cwd)]
   )
)

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
         (__cs_layout_current_set (__cs_view_current_get) (symb->layout l))]

        [(tag l)
         (__cs_layout_current_set tag (symb->layout l))]
   )
)

(define layout-switch-tiled
   (case-lambda
        [()
         (__cs_layout_current_set (__cs_view_current_get) (symb->layout 'tiled))]

        [(tag)
         (__cs_layout_current_set tag (symb->layout 'tiled))]
   )
)

(define layout-switch-grid
   (case-lambda
        [()
         (__cs_layout_current_set (__cs_view_current_get) (symb->layout 'grid))]

        [(tag)
         (__cs_layout_current_set tag (symb->layout 'grid))]
   )
)

(define layout-switch-bstack
   (case-lambda
        [()
         (__cs_layout_current_set (__cs_view_current_get) (symb->layout 'bstack))]

        [(tag)
         (__cs_layout_current_set tag (symb->layout 'bstack))]
   )
)

(define layout-switch-maximized
   (case-lambda
        [()
         (__cs_layout_current_set (__cs_view_current_get) (symb->layout 'maximized))]

        [(tag)
         (__cs_layout_current_set tag (symb->layout 'maximized))]
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

(define layout-n-master-set
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

(define layout-%-master-set
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

(define tagbar-status-align
   (lambda (a)
      (let ([v (case a
                  ['right 0]
                  ['left  1]
               )
            ]
           )
           (__cs_tagbar_status_align v)
      )
   )
)

(define tagbar-show
   (lambda (s)
      (__cs_tagbar_show s)
   )
)

(define tagbar-status-set
   (lambda (s)
      (__cs_tagbar_status_set s)
   )
)

(define copy-buffer-set #f)
(define copy-buffer+ #f)

(define copy-buffer
   (lambda ()
      (__cs_copy_buf_get)
   )
)

(define do-quit __cs_do_quit)

;;;;;;;;;;;;;;;;;;;;;;;
;; Default key bindings
;;;;;;;;;;;;;;;;;;;;;;;
(bind-key "C-g c"       window-exec)
(bind-key "C-g C-x"     window-eval)
(bind-key "C-g x x"     window-delete)
(bind-key "M-h"         window-switch-left)
(bind-key "C-g h"       window-switch-left)
(bind-key "M-l"         window-switch-right)
(bind-key "C-g l"       window-switch-right)
(bind-key "M-j"         window-switch-lower)
(bind-key "C-g j"       window-switch-lower)
(bind-key "M-k"         window-switch-upper)
(bind-key "C-g k"       window-switch-upper)
(bind-key "C-g <Enter>" window-set-master)
(bind-key "C-g ."       window-set-minimized)
(bind-key "C-g y"       window-copy)
(bind-key "C-g p"       window-paste)
(bind-key "C-g /"       window-pager)

(bind-key "M-1"     view-switch-1)
(bind-key "C-g v 1" view-switch-1)
(bind-key "M-2"     view-switch-2)
(bind-key "C-g v 2" view-switch-2)
(bind-key "M-3"     view-switch-3)
(bind-key "C-g v 3" view-switch-3)
(bind-key "M-4"     view-switch-4)
(bind-key "C-g v 4" view-switch-4)
(bind-key "M-5"     view-switch-5)
(bind-key "C-g v 5" view-switch-5)
(bind-key "M-6"     view-switch-6)
(bind-key "C-g v 6" view-switch-6)
(bind-key "M-7"     view-switch-7)
(bind-key "C-g v 7" view-switch-7)
(bind-key "M-8"     view-switch-8)
(bind-key "C-g v 8" view-switch-8)
(bind-key "M-9"     view-switch-9)
(bind-key "C-g v 9" view-switch-9)
(bind-key "M-0"     view-switch-all)
(bind-key "C-g v 0" view-switch-all)

(bind-key "C-g i"   layout-n-master+)
(bind-key "C-g d"   layout-n-master-)
(bind-key "C-g H"   layout-%-master-)
(bind-key "C-g L"   layout-%-master+)
(bind-key "C-g C-s" layout-toggle-sticky)
(bind-key "C-g f"   layout-switch-tiled)
(bind-key "C-g g"   layout-switch-grid)
(bind-key "C-g b"   layout-switch-bstack)
(bind-key "C-g m"   window-toggle-maximized)

(bind-key "C-g q q" do-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start with a shell window
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(window-exec)
