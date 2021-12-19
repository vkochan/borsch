(load "common.ss")
(load "tagbar.ss")
(load "layout.ss")
(load "view.ss")
(load "window.ss")

;; Event handling
(define __on-event-cb-list '())

(define __on-event-handler
   (lambda (ev wid)
       (define __evt->symb
	  (lambda (ev)
	     (case ev
	        [0  'window-create   ]
		[1  'window-select   ]
		[2  'window-minimize ]
		[3  'window-maximize ]
		[4  'window-delete   ]
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

(define __cs_kmap_add (foreign-procedure "cs_kmap_add" (string) scheme-object))
(define __cs_kmap_parent_set (foreign-procedure "cs_kmap_parent_set" (int string) void))
(define __cs_kmap_del (foreign-procedure __collect_safe "cs_kmap_del" (int) void))

(define __cs_buf_kmap_get (foreign-procedure __collect_safe "cs_buf_kmap_get" (int) scheme-object))
(define __cs_buf_kmap_set (foreign-procedure "cs_buf_kmap_set" (int string) scheme-object))
(define __cs_buf_current_get (foreign-procedure __collect_safe "cs_buf_current_get" () scheme-object))
(define __cs_buf_first_get (foreign-procedure __collect_safe "cs_buf_first_get" () scheme-object))
(define __cs_buf_next_get (foreign-procedure __collect_safe "cs_buf_next_get" (int) scheme-object))
(define __cs_buf_name_get (foreign-procedure __collect_safe "cs_buf_name_get" (int) scheme-object))
(define __cs_buf_name_set (foreign-procedure "cs_buf_name_set" (int string) void))
(define __cs_buf_by_name (foreign-procedure "cs_buf_by_name" (string) scheme-object))
(define __cs_buf_text_insert (foreign-procedure "cs_buf_text_insert" (int string) scheme-object))
(define __cs_buf_text_insert_nl (foreign-procedure "cs_buf_text_insert_nl" (int int) scheme-object))
(define __cs_buf_text_insert_file (foreign-procedure "cs_buf_text_insert_file" (int string) scheme-object))
(define __cs_buf_text_input_enable (foreign-procedure __collect_safe "cs_buf_text_input_enable" (int boolean) void))
(define __cs_buf_text_obj_pos (foreign-procedure "cs_buf_text_obj_pos" (int int char int) scheme-object))
(define __cs_buf_text_range_del (foreign-procedure "cs_buf_text_range_del" (int int int) scheme-object))
(define __cs_buf_text_get (foreign-procedure "cs_buf_text_get" (int int int) scheme-object))
(define __cs_buf_cursor_get (foreign-procedure __collect_safe "cs_buf_cursor_get" (int) scheme-object))
(define __cs_buf_cursor_set (foreign-procedure __collect_safe "cs_buf_cursor_set" (int int) void))
(define __cs_buf_mode_set (foreign-procedure "cs_buf_mode_set" (int string) void))

(define __cs_bind_key (foreign-procedure "cs_bind_key" (string void* int string) int))
(define __cs_unbind_key (foreign-procedure "cs_unbind_key" (string int) int))

(define __cs_copy_buf_get (foreign-procedure __collect_safe "cs_copy_buf_get" () scheme-object))
(define __cs_copy_buf_set (foreign-procedure "cs_copy_buf_set" (string) int))

(define __cs_do_quit (foreign-procedure "cs_do_quit" () void))
;;

;; Public API
(define global-keymap 1)

(define bind-key
   (case-lambda
      [(k p)
       (bind-key 0 k p)]

      [(m k t)
       (if (procedure? t)
          (__cs_bind_key k (key-cb t) m "")
          ;; else
          (__cs_bind_key k 0 m (symbol->string t))
       )
      ]
   )
)

(define unbind-key
   (case-lambda
      [(k)
       (__cs_unbind_key k 0)]

      [(m k)
       (__cs_unbind_key k m)]
   )
)

(define make-keymap
   (case-lambda
      [()
       (__cs_kmap_add (symbol->string 'global-keymap))]

      [(p)
       (__cs_kmap_add (symbol->string p))]
   )
)

(define keymap-set-parent
   (lambda (k p)
       (__cs_kmap_parent_set k p)
   )
)

(define keymap-del
   (lambda (k)
       (__cs_kmap_del k)
   )
)

(define mode-gen-map-symb
   (lambda (m)
         (string->symbol
            (string-append (symbol->string m) "-map"))
   )
)

(define mode-gen-map-value
   (lambda (m)
      (let ([s (mode-gen-map-symb m)])
         (if (top-level-bound? s)
            (top-level-value s)
            ;; else
            #f
         )
      )
   )
)

(define mode-gen-hook-symb
   (lambda (m)
         (string->symbol
            (string-append (symbol->string m) "-hook"))
   )
)

(define mode-gen-hook-value
   (lambda (h)
      (let ([s (mode-gen-hook-symb h)])
         (if (top-level-bound? s)
            (top-level-value s)
            ;; else
            #f
         )
      )
   )
)

(define run-hooks
   (lambda (symb)
      (if (top-level-bound? symb)
         (let ([hook-list (top-level-value symb)])
            (for-each
               (lambda (h)
                  (h)
               ) hook-list
            )
         )
      )
   )
)

(define add-hook
   (lambda (h f)
      (if (not (top-level-bound? h))
         (define-top-level-value h (list))
      )
      (let ([h-lst (top-level-value h)])
         (if (not (member f h-lst))
            (set-top-level-value! h (append h-lst (list f)))
         )
      )
   )
)

(define remove-hook
   (lambda (h f)
      (if (top-level-bound? h)
         (let ([h-lst (top-level-value h)])
            (set-top-level-value! h (remove f h-lst))
         )
      )
   )
)

(define-syntax (define-mode stx)
   (syntax-case stx ()
      ((_ mode name parent exp ...)
       #`(define-top-level-value 'mode
            (lambda ()
               (when parent
                  ((top-level-value 'parent))
               )
	       (let ([m-map (mode-gen-map-symb 'mode)])
                  (when (top-level-bound? m-map)
                     (when parent
	                (let ([p-map (mode-gen-map-symb 'parent)])
                           (keymap-set-parent (top-level-value m-map) p-map)
                        )
		     )
                     (buffer-set-keymap m-map)
                  )
		  (buffer-set-mode name)
                  exp
                  ...
                  (run-hooks
                     (mode-gen-hook-symb 'mode))
               )
            )
         )
      )
   )
)

(define buffer-set-mode
   (case-lambda
      [(n)
         (__cs_buf_mode_set (buffer-current) n)]

      [(b n)
         (__cs_buf_mode_set b n)]
   )
)

(define buffer-keymap
   (case-lambda
      [()
       (__cs_buf_kmap_get (buffer-current))]

      [(b)
       (__cs_buf_kmap_get b)]
   )
)

(define buffer-set-keymap
   (case-lambda
      [(k)
       (__cs_buf_kmap_set (buffer-current) (symbol->string k))]

      [(b k)
       (__cs_buf_kmap_set b (symbol->string k))]
   )
)

(define buffer-current
   (lambda ()
      (__cs_buf_current_get)
   )
)

(define-syntax (with-buffer stx)
   (syntax-case stx ()
	       ((_ b exp ...)
		#`(fluid-let ([buffer-current (lambda () b)])
		    (begin
                       exp
		       ...
                    )
                  )
               )
   )
)

(define cursor
   (case-lambda
     [()
      (__cs_buf_cursor_get (buffer-current))]

     [(b)
      (__cs_buf_cursor_get b)]
   )
)

(define cursor-set
   (case-lambda
     [(p)
      (let ()
         (__cs_buf_cursor_set (buffer-current) p)
	 p
      )
     ]

     [(b p)
      (let ()
         (__cs_buf_cursor_set b p)
	 p
      )
     ]
   )
)

(define-syntax (cursor-save stx)
   (syntax-case stx ()
	       ((_ exp ...)
		#`(let ([curs (cursor)])
		    (begin
                       exp
		       ...
                    )
                    (cursor-set curs)
                  )
               )
   )
)

(define buffer-name
   (case-lambda
      [()
         (__cs_buf_name_get (buffer-current))]

      [(b)
         (__cs_buf_name_get b)]
   )
)

(define buffer-set-name
   (case-lambda
      [(n)
         (__cs_buf_name_set (buffer-current) n)]

      [(b n)
         (__cs_buf_name_set b n)]
   )
)

(define buffer-get
   (lambda (n)
      (__cs_buf_by_name n)
   )
)

(define buffer-first
   (lambda ()
       (__cs_buf_first_get)
   )
)

(define buffer-next
   (case-lambda
      [()
       (__cs_buf_next_get (buffer-current))]

      [(bid)
       (__cs_buf_next_get bid)]
   )
)

(define buffer-list
   (lambda ()
      (let ([buf (buffer-first)]
            [lst   '()]
	   )

         (while buf
            (set! lst (append lst (list
                                    (list buf (buffer-name buf))
				  )
            )         )
            (set! buf (buffer-next buf))
         )

	 lst
      )
   )
)

(define buffer-set-input
   (case-lambda
      [(e)
      (__cs_buf_text_input_enable (buffer-current) e)]

      [(b e)
      (__cs_buf_text_input_enable b e)]
   )
)

(define insert
   (case-lambda
      [(t)
      (__cs_buf_text_insert (buffer-current) t)]

      [(b t)
      (__cs_buf_text_insert b t)]
   )
)

(define insert-nl
   (case-lambda
      [()
      (__cs_buf_text_insert_nl (buffer-current) (cursor))]

      [(b)
      (__cs_buf_text_insert_nl b (cursor))]
   )
)

(define insert-file
   (case-lambda
      [(t)
      (__cs_buf_text_insert_file (buffer-current) t)]

      [(b t)
      (__cs_buf_text_insert_file b t)]
   )
)

(define move-next-char
   (case-lambda
      [()
      (cursor-set (next-char-pos))]

      [(b)
      (cursor-set b (next-char-pos b))]
   )
)

(define move-prev-char
   (case-lambda
      [()
      (cursor-set (prev-char-pos))]

      [(b)
      (cursor-set b (prev-char-pos b))]
   )
)

(define move-next-word
   (case-lambda
      [()
      (cursor-set (next-word-pos))]

      [(b)
      (cursor-set b (next-word-pos b))]
   )
)

(define move-prev-word
   (case-lambda
      [()
      (cursor-set (prev-word-pos))]

      [(b)
      (cursor-set b (prev-word-pos b))]
   )
)

(define move-word-end
   (case-lambda
      [()
      (cursor-set (word-end-pos))]

      [(b)
      (cursor-set b (word-end-pos b))]
   )
)

(define move-next-longword
   (case-lambda
      [()
      (cursor-set (next-longword-pos))]

      [(b)
      (cursor-set b (next-longword-pos b))]
   )
)

(define move-prev-longword
   (case-lambda
      [()
      (cursor-set (prev-longword-pos))]

      [(b)
      (cursor-set b (prev-longword-pos b))]
   )
)

(define move-longword-end
   (case-lambda
      [()
      (cursor-set (longword-end-pos))]

      [(b)
      (cursor-set b (longword-end-pos b))]
   )
)

(define move-prev-line
   (case-lambda
      [()
      (cursor-set (prev-line-pos))]

      [(b)
      (cursor-set b (prev-line-pos b))]
   )
)

(define move-next-line
   (case-lambda
      [()
      (cursor-set (next-line-pos))]

      [(b)
      (cursor-set b (next-line-pos b))]
   )
)

(define move-next-line-begin
   (case-lambda
      [()
      (cursor-set (next-line-begin-pos))]

      [(b)
      (cursor-set b (next-line-begin-pos b))]
   )
)

(define move-prev-line-end
   (case-lambda
      [()
      (cursor-set (prev-line-begin-pos))]

      [(b)
      (cursor-set b (prev-line-begin-pos b))]
   )
)

(define move-line-start
   (case-lambda
      [()
      (cursor-set (line-start-pos))]

      [(b)
      (cursor-set b (line-start-pos b))]
   )
)

(define move-line-finish
   (case-lambda
      [()
      (cursor-set (line-finish-pos))]

      [(b)
      (cursor-set b (line-finish-pos b))]
   )
)

(define move-line-begin
   (case-lambda
      [()
      (cursor-set (line-begin-pos))]

      [(b)
      (cursor-set b (line-begin-pos b))]
   )
)

(define move-line-end
   (case-lambda
      [()
      (cursor-set (line-end-pos))]

      [(b)
      (cursor-set b (line-end-pos b))]
   )
)

(define move-buffer-begin
   (case-lambda
      [()
      (cursor-set (buffer-begin-pos))]

      [(b)
      (cursor-set b (buffer-begin-pos b))]
   )
)

(define move-buffer-end
   (case-lambda
      [()
      (cursor-set (buffer-end-pos))]

      [(b)
      (cursor-set b (buffer-end-pos b))]
   )
)

(define next-char-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\c 1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\c 1)]
   )
)

(define prev-char-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\c -1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\c -1)]
   )
)

(define next-word-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\w 1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\w 1)]
   )
)

(define prev-word-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\w -1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\w -1)]
   )
)

(define word-end-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\e 1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\e 1)]
   )
)

(define next-longword-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\W 1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\W 1)]
   )
)

(define prev-longword-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\W -1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\W -1)]
   )
)

(define longword-end-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\E 1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\E 1)]
   )
)

(define prev-line-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\l -1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\l -1)]
   )
)

(define next-line-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\l 1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\l 1)]
   )
)

(define next-line-begin-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\L 1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\L 1)]
   )
)

(define prev-line-end-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\L -1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\L -1)]
   )
)

(define line-start-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\0 -1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\0 -1)]
   )
)

(define line-finish-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\0 1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\0 1)]
   )
)

(define line-begin-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\0 -1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\0 -1)]
   )
)

(define line-end-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\1 1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\1 1)]
   )
)

(define buffer-begin-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\g 1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\g 1)]
   )
)

(define buffer-end-pos
   (case-lambda
      [()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\g -1)]

      [(b)
      (__cs_buf_text_obj_pos b (cursor b) #\g -1)]
   )
)

(define buffer-string
   (case-lambda
      [()
       (__cs_buf_text_get (buffer-current)
                          (buffer-begin-pos)
                          (- (buffer-end-pos) (buffer-begin-pos)))]

      [(s l)
       (__cs_buf_text_get (buffer-current) s l)]

      [(b)
       (__cs_buf_text_get b
                          (buffer-begin-pos b)
                          (- (buffer-end-pos b) (buffer-begin-pos b)))]

      [(b s l)
       (__cs_buf_text_get b s l)]
   )
)

(define delete-range
   (case-lambda
      [(s e)
      (__cs_buf_text_range_del (buffer-current) s e)]

      [(b s e)
      (__cs_buf_text_range_del b s e)]
   )
)

(define replace-range
   (case-lambda
      [(s e t)
      (delete-range s e)
      (cursor-set s)
      (insert t)
      ]

      [(b s e t)
      (delete-range b s e)
      (cursor-set b s)
      (insert b t)
      ]
   )
)

(define cursor-obj-delete
   (case-lambda
      [(fn)
      (let (
            [end (fn)]
            [start (cursor)]
           )
           (delete-range start end)
      )]

      [(b fn)
      (let (
            [end (fn b)]
            [start (cursor b)]
           )
           (delete-range b start end)
      )]
   )
)
(define delete-next-char
   (case-lambda
      [()
       (cursor-obj-delete move-next-char)]

      [(b)
       (cursor-obj-delete b move-next-char)]
   )
)
(define delete-char delete-next-char)

(define delete-prev-char
   (case-lambda
      [()
       (cursor-obj-delete move-prev-char)]

      [(b)
       (cursor-obj-delete b move-prev-char)]
   )
)

(define delete-next-word
   (case-lambda
      [()
       (cursor-obj-delete move-next-word)]

      [(b)
       (cursor-obj-delete b move-next-word)]
   )
)
(define delete-word delete-next-word)

(define delete-prev-word
   (case-lambda
      [()
       (cursor-obj-delete move-prev-word)]

      [(b)
       (cursor-obj-delete b move-prev-word)]
   )
)

(define delete-word-end
   (case-lambda
      [()
       (cursor-obj-delete move-word-end)]

      [(b)
       (cursor-obj-delete b move-word-end)]
   )
)

(define delete-next-longword
   (case-lambda
      [()
       (cursor-obj-delete move-next-longword)]

      [(b)
       (cursor-obj-delete b move-next-longword)]
   )
)
(define delete-longword delete-next-longword)

(define delete-prev-longword
   (case-lambda
      [()
       (cursor-obj-delete move-prev-longword)]

      [(b)
       (cursor-obj-delete b move-prev-longword)]
   )
)

(define delete-longword-end
   (case-lambda
      [()
       (cursor-obj-delete move-longword-end)]

      [(b)
       (cursor-obj-delete b move-longword-end)]
   )
)

(define delete-next-line-begin
   (case-lambda
      [()
       (cursor-obj-delete move-next-line-begin)]

      [(b)
       (cursor-obj-delete b move-next-line-begin)]
   )
)

(define delete-prev-line-end
   (case-lambda
      [()
       (cursor-obj-delete move-prev-line-end)]

      [(b)
       (cursor-obj-delete b move-prev-line-end)]
   )
)

(define delete-line-start
   (case-lambda
      [()
       (cursor-obj-delete move-line-start)]

      [(b)
       (cursor-obj-delete b move-line-start)]
   )
)

(define delete-line-finish
   (case-lambda
      [()
       (cursor-obj-delete move-line-finish)]

      [(b)
       (cursor-obj-delete b move-line-finish)]
   )
)

(define delete-line-begin
   (case-lambda
      [()
       (cursor-obj-delete move-line-begin)]

      [(b)
       (cursor-obj-delete b move-line-begin)]
   )
)

(define delete-line-end
   (case-lambda
      [()
       (cursor-obj-delete move-line-end)]

      [(b)
       (cursor-obj-delete b move-line-end)]
   )
)

(define delete-buffer-begin
   (case-lambda
      [()
       (cursor-obj-delete move-buffer-begin)]

      [(b)
       (cursor-obj-delete b move-buffer-begin)]
   )
)

(define delete-buffer-end
   (case-lambda
      [()
       (cursor-obj-delete move-buffer-end)]

      [(b)
       (cursor-obj-delete b move-buffer-end)]
   )
)

(define copy-buffer-set #f)
(define copy-buffer+ #f)

(define copy-buffer
   (lambda ()
      (__cs_copy_buf_get)
   )
)

(define text-mode-cmd
   (lambda ()
      (buffer-set-keymap 'text-mode-cmd-map)
      (buffer-set-mode "Text <N>")
      (buffer-set-input #f)
   )
)

(define text-mode-ins
   (lambda ()
      (buffer-set-keymap 'text-mode-ins-map)
      (buffer-set-mode "Text <I>")
      (buffer-set-input #t)
   )
)

(define text-mode-map
   (let ([map (make-keymap)])
      map
   )
)

(define text-mode-cmd-map
   (let ([map (make-keymap 'text-mode-map)])
      (bind-key map "h" (lambda () (move-prev-char)))
      (bind-key map "l" (lambda () (move-next-char)))
      (bind-key map "j" (lambda () (move-next-line)))
      (bind-key map "k" (lambda () (move-prev-line)))
      (bind-key map "w" (lambda () (move-next-word)))
      (bind-key map "W" (lambda () (move-next-longword)))
      (bind-key map "b" (lambda () (move-prev-word)))
      (bind-key map "B" (lambda () (move-prev-longword)))
      (bind-key map "e" (lambda () (move-word-end)))
      (bind-key map "E" (lambda () (move-longword-end)))
      (bind-key map "x" (lambda () (delete-next-char)))
      (bind-key map "X" (lambda () (delete-prev-char)))
      (bind-key map "g g" (lambda () (move-buffer-begin)))
      (bind-key map "G" (lambda () (move-buffer-end)))
      (bind-key map "i" text-mode-ins)
      map
   )
)

(define text-mode-ins-map
   (let ([map (make-keymap 'text-mode-map)])
      (bind-key map "<Enter>" (lambda () (insert-nl)))
      (bind-key map "<Backspace>" (lambda () (delete-prev-char)))
      (bind-key map "<Esc>" text-mode-cmd)
      map
   )
)

(define-mode text-mode "Text" #f
   (buffer-set-input #t)
   (text-mode-cmd)
)

(define do-quit __cs_do_quit)

;;;;;;;;;;;;;;;;;;;;;;;
;; Default key bindings
;;;;;;;;;;;;;;;;;;;;;;;
(bind-key "C-g c"       window-shell)
(bind-key "C-g C-x"     window-eval)
(bind-key "C-g x x"     window-delete)
(bind-key "M-h"         window-select-left)
(bind-key "C-g h"       window-select-left)
(bind-key "M-l"         window-select-right)
(bind-key "C-g l"       window-select-right)
(bind-key "M-j"         window-select-lower)
(bind-key "C-g j"       window-select-lower)
(bind-key "M-k"         window-select-upper)
(bind-key "C-g k"       window-select-upper)
(bind-key "C-g <Enter>" window-set-master)
(bind-key "C-g ."       window-set-minimized)

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
(window-shell)
