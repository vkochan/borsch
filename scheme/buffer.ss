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
(define __cs_buf_text_obj_range (foreign-procedure "cs_buf_text_obj_range" (int int char boolean) scheme-object))
(define __cs_buf_text_range_del (foreign-procedure "cs_buf_text_range_del" (int int int) scheme-object))
(define __cs_buf_text_get (foreign-procedure "cs_buf_text_get" (int int int) scheme-object))

(define __cs_buf_text_fg_set (foreign-procedure "cs_buf_text_fg_set" (int int) void))
(define __cs_buf_text_bg_set (foreign-procedure "cs_buf_text_bg_set" (int int) void))
(define __cs_buf_text_style_set (foreign-procedure "cs_buf_text_style_set" (int int) void))
(define __cs_buf_text_fg_get (foreign-procedure "cs_buf_text_fg_get" (int) scheme-object))
(define __cs_buf_text_bg_get (foreign-procedure "cs_buf_text_bg_get" (int) scheme-object))
(define __cs_buf_text_style_get (foreign-procedure "cs_buf_text_style_get" (int) scheme-object))

(define __cs_buf_cursor_get (foreign-procedure __collect_safe "cs_buf_cursor_get" (int) scheme-object))
(define __cs_buf_cursor_set (foreign-procedure __collect_safe "cs_buf_cursor_set" (int int) void))
(define __cs_buf_mode_set (foreign-procedure "cs_buf_mode_set" (int string) void))

(define __cs_buf_file_open (foreign-procedure "cs_buf_file_open" (int string) scheme-object))
(define __cs_buf_save (foreign-procedure "cs_buf_save" (int) scheme-object))

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
   (lambda (n)
      (__cs_buf_mode_set (buffer-current) n)
   )
)

(define buffer-keymap
   (lambda ()
       (__cs_buf_kmap_get (buffer-current))
   )
)

(define buffer-set-keymap
   (lambda (k)
      (__cs_buf_kmap_set (buffer-current) (symbol->string k))
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
   (lambda ()
      (__cs_buf_cursor_get (buffer-current))
   )
)

(define cursor-set
   (lambda (p)
      (let ()
         (__cs_buf_cursor_set (buffer-current) p)
	 p
      )
   )
)

(define-syntax (save-cursor stx)
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
   (lambda ()
      (__cs_buf_name_get (buffer-current))
   )
)

(define buffer-set-name
   (lambda (n)
      (__cs_buf_name_set (buffer-current) n)
   )
)

(define buffer-new
   (case-lambda
      [() 
       (window-buffer (window-new))]

      [(n) 
       (let ([b (window-buffer (window-new))])
          (buffer-set-name n)
          b
       )
      ]
   )
)

(define buffer-get
   (lambda (n)
      (__cs_buf_by_name n)
   )
)

(define buffer-open-file
   (lambda (f)
      (__cs_buf_file_open (buffer-current) f)
   )
)

(define buffer-save
   (lambda ()
      (__cs_buf_save (buffer-current))
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
   (lambda (e)
      (__cs_buf_text_input_enable (buffer-current) e)
   )
)

(define set-text-style
   (lambda (s . e)
      (let ([a (append (list s) e)])
         (for-each
            (lambda (o)
               (cond
                  [(equal? (car o) ':fg)
		     (__cs_buf_text_fg_set (buffer-current) (color-name->number (cadr o)))]
                  [(equal? (car o) ':bg)
		     (__cs_buf_text_bg_set (buffer-current) (color-name->number (cadr o)))]
                  [(equal? (car o) ':style)
		     (__cs_buf_text_style_set (buffer-current) (style-name->number (cadr o)))]
               )
            )
            a
         )
      )
   )
)

(define insert
   (lambda (t)
      (__cs_buf_text_insert (buffer-current) t)
   )
)

(define insert-nl
   (lambda ()
      (__cs_buf_text_insert_nl (buffer-current) (cursor))
   )
)

(define insert-file
   (lambda (t)
      (__cs_buf_text_insert_file (buffer-current) t)
   )
)

(define move-next-char
   (lambda ()
      (cursor-set (next-char-pos))
   )
)

(define move-prev-char
   (lambda ()
      (cursor-set (prev-char-pos))
   )
)

(define move-next-word
   (lambda ()
      (cursor-set (next-word-pos))
   )
)

(define move-prev-word
   (lambda ()
      (cursor-set (prev-word-pos))
   )
)

(define move-word-end
   (lambda ()
      (cursor-set (word-end-pos))
   )
)

(define move-next-longword
   (lambda ()
      (cursor-set (next-longword-pos))
   )
)

(define move-prev-longword
   (lambda ()
      (cursor-set (prev-longword-pos))
   )
)

(define move-longword-end
   (lambda ()
      (cursor-set (longword-end-pos))
   )
)

(define move-prev-line
   (lambda ()
      (cursor-set (prev-line-pos))
   )
)

(define move-next-line
   (lambda ()
      (cursor-set (next-line-pos))
   )
)

(define move-next-line-begin
   (lambda ()
      (cursor-set (next-line-begin-pos))
   )
)

(define move-prev-line-end
   (lambda ()
      (cursor-set (prev-line-begin-pos))
   )
)

(define move-line-start
   (lambda ()
      (cursor-set (line-start-pos))
   )
)

(define move-line-finish
   (lambda ()
      (cursor-set (line-finish-pos))
   )
)

(define move-line-begin
   (lambda ()
      (cursor-set (line-begin-pos))
   )
)

(define move-line-end
   (lambda ()
      (cursor-set (line-end-pos))
   )
)

(define move-buffer-begin
   (lambda ()
      (cursor-set (buffer-begin-pos))
   )
)

(define move-buffer-end
   (lambda ()
      (cursor-set (buffer-end-pos))
   )
)

(define next-char-pos
   (case-lambda
      [()
       (__cs_buf_text_obj_pos (buffer-current) (cursor) #\c 1)]

      [(s)
       (__cs_buf_text_obj_pos (buffer-current) s #\c 1)]
   )
)

(define prev-char-pos
   (case-lambda
      [()
       (__cs_buf_text_obj_pos (buffer-current) (cursor) #\c -1)]

      [(s)
       (__cs_buf_text_obj_pos (buffer-current) s #\c -1)]
   )
)

(define next-word-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\w 1)
   )
)

(define prev-word-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\w -1)
   )
)

(define word-end-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\e 1)
   )
)

(define next-longword-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\W 1)
   )
)

(define prev-longword-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\W -1)
   )
)

(define longword-end-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\E 1)
   )
)

(define prev-line-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\l -1)
   )
)

(define next-line-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\l 1)
   )
)

(define next-line-begin-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\L 1)
   )
)

(define prev-line-end-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\L -1)
   )
)

(define line-start-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\0 -1)
   )
)

(define line-finish-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\0 1)
   )
)

(define line-begin-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\0 -1)
   )
)

(define line-end-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\1 1)
   )
)

(define buffer-begin-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\g 1)
   )
)

(define buffer-end-pos
   (lambda ()
      (__cs_buf_text_obj_pos (buffer-current) (cursor) #\g -1)
   )
)

(define buffer-string
   (case-lambda
      [()
       (__cs_buf_text_get (buffer-current)
                          (buffer-begin-pos)
                          (- (buffer-end-pos) (buffer-begin-pos)))]

      [(s e)
       (__cs_buf_text_get (buffer-current) s (- e s))]
   )
)

(define extract-char
   (case-lambda
      [()
         (extract-char (cursor))]

      [(s)
         (let ([str (buffer-string s (next-char-pos s))])
            (if (> (string-length str) 0)
               (string-ref str 0)
               ;; else
               #f
            )
         )
      ]
   )
)

(define extract-word
   (case-lambda
      [()
       (extract-word (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\w #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-longword
   (case-lambda
      [()
       (extract-longword (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\W #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-line
   (case-lambda
      [()
       (extract-line (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\l #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-line-inner
   (case-lambda
      [()
       (extract-line-inner (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\l #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-square-brackets
   (case-lambda
      [()
       (extract-square-brackets (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\[ #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-square-brackets-inner
   (case-lambda
      [()
       (extract-square-brackets-inner (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\[ #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-curly-brackets
   (case-lambda
      [()
       (extract-curly-brackets (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\{ #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-curly-brackets-inner
   (case-lambda
      [()
       (extract-curly-brackets-inner (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\{ #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-angle-brackets
   (case-lambda
      [()
       (extract-angle-brackets (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\< #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-angle-brackets-inner
   (case-lambda
      [()
       (extract-angle-brackets-inner (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\< #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-parens
   (case-lambda
      [()
       (extract-parens (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\( #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-parens-inner
   (case-lambda
      [()
       (extract-parens-inner (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\( #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-quote
   (case-lambda
      [()
       (extract-quote (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\" #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-quote-inner
   (case-lambda
      [()
       (extract-quote-inner (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\" #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-single-quote
   (case-lambda
      [()
       (extract-single-quote (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\' #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-single-quote-inner
   (case-lambda
      [()
       (extract-single-quote-inner (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\' #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-back-quote
   (case-lambda
      [()
       (extract-back-quote (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\` #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-back-quote-inner
   (case-lambda
      [()
       (extract-back-quote-inner (cursor))]

      [(s)
       (let ([r (__cs_buf_text_obj_range (buffer-current) s #\` #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define delete-range
   (lambda (s e)
      (__cs_buf_text_range_del (buffer-current) s e)
   )
)

(define replace-range
   (lambda (s e t)
      (delete-range s e)
      (cursor-set s)
      (insert t)
   )
)

(define cursor-obj-delete
   (lambda (fn)
      (let (
            [end (fn)]
            [start (cursor)]
           )
           (delete-range start end)
      )
   )
)

(define delete-next-char
   (lambda ()
      (cursor-obj-delete move-next-char)
   )
)
(define delete-char delete-next-char)

(define delete-prev-char
   (lambda ()
      (cursor-obj-delete move-prev-char)
   )
)

(define delete-next-word
   (lambda ()
      (cursor-obj-delete move-next-word)
   )
)
(define delete-word delete-next-word)

(define delete-prev-word
   (lambda ()
      (cursor-obj-delete move-prev-word)
   )
)

(define delete-word-end
   (lambda ()
      (cursor-obj-delete move-word-end)
   )
)

(define delete-next-longword
   (lambda ()
      (cursor-obj-delete move-next-longword)
   )
)
(define delete-longword delete-next-longword)

(define delete-prev-longword
   (lambda ()
      (cursor-obj-delete move-prev-longword)
   )
)

(define delete-longword-end
   (lambda ()
      (cursor-obj-delete move-longword-end)
   )
)

(define delete-next-line-begin
   (lambda ()
      (cursor-obj-delete move-next-line-begin)
   )
)

(define delete-prev-line-end
   (lambda ()
      (cursor-obj-delete move-prev-line-end)
   )
)

(define delete-line-start
   (lambda ()
      (cursor-obj-delete move-line-start)
   )
)

(define delete-line-finish
   (lambda ()
      (cursor-obj-delete move-line-finish)
   )
)

(define delete-line-begin
   (lambda ()
      (cursor-obj-delete move-line-begin)
   )
)

(define delete-line-end
   (lambda ()
      (cursor-obj-delete move-line-end)
   )
)

(define delete-buffer-begin
   (lambda ()
      (cursor-obj-delete move-buffer-begin)
   )
)

(define delete-buffer-end
   (lambda ()
      (cursor-obj-delete move-buffer-end)
   )
)
