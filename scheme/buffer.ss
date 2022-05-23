(define __cs_buf_new (foreign-procedure "cs_buf_new" (string) scheme-object))
(define __cs_buf_del (foreign-procedure __collect_safe "cs_buf_del" (int) void))
(define __cs_buf_kmap_get (foreign-procedure __collect_safe "cs_buf_kmap_get" (int) scheme-object))
(define __cs_buf_kmap_set (foreign-procedure "cs_buf_kmap_set" (int string) scheme-object))
(define __cs_buf_current_get (foreign-procedure __collect_safe "cs_buf_current_get" () scheme-object))
(define __cs_buf_first_get (foreign-procedure __collect_safe "cs_buf_first_get" () scheme-object))
(define __cs_buf_next_get (foreign-procedure __collect_safe "cs_buf_next_get" (int) scheme-object))
(define __cs_buf_name_get (foreign-procedure __collect_safe "cs_buf_name_get" (int) scheme-object))
(define __cs_buf_name_set (foreign-procedure "cs_buf_name_set" (int string) void))
(define __cs_buf_readonly_set (foreign-procedure __collect_safe "cs_buf_readonly_set" (int boolean) void))
(define __cs_buf_readonly_get (foreign-procedure __collect_safe "cs_buf_readonly_get" (int) scheme-object))
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

(define __cs_buf_prop_style_add (foreign-procedure "cs_buf_prop_style_add" (int int int int int int int) scheme-object))
(define __cs_buf_prop_kmap_add (foreign-procedure "cs_buf_prop_kmap_add" (int int int int) scheme-object))
(define __cs_buf_prop_del (foreign-procedure "cs_buf_prop_del" (int int int int) void))

(define __cs_buf_cursor_get (foreign-procedure __collect_safe "cs_buf_cursor_get" (int) scheme-object))
(define __cs_buf_cursor_set (foreign-procedure __collect_safe "cs_buf_cursor_set" (int int) void))
(define __cs_buf_mode_name_set (foreign-procedure "cs_buf_mode_name_set" (int string) void))
(define __cs_buf_state_name_set (foreign-procedure "cs_buf_state_name_set" (int string) void))

(define __cs_buf_file_open (foreign-procedure "cs_buf_file_open" (int string) scheme-object))
(define __cs_buf_file_set (foreign-procedure "cs_buf_file_set" (int string) void))
(define __cs_buf_file_get (foreign-procedure "cs_buf_file_get" (int) scheme-object))
(define __cs_buf_save (foreign-procedure "cs_buf_save" (int) scheme-object))

(define __cs_buf_mark_set (foreign-procedure "cs_buf_mark_set" (int int) void))
(define __cs_buf_mark_get (foreign-procedure "cs_buf_mark_get" (int) scheme-object))
(define __cs_buf_mark_clear (foreign-procedure "cs_buf_mark_clear" (int) void))

(define __cs_win_mark_highlight (foreign-procedure __collect_safe "cs_win_mark_highlight" (int boolean) void))

(define __cs_buf_is_visible (foreign-procedure __collect_safe "cs_buf_is_visible" (int) scheme-object))
(define __cs_buf_is_term (foreign-procedure __collect_safe "cs_buf_is_term" (int) scheme-object))

(define __cs_buf_env_get (foreign-procedure __collect_safe "cs_buf_env_get" (int) scheme-object))

(define __cs_buf_snapshot (foreign-procedure __collect_safe "cs_buf_snapshot" (int) void))
(define __cs_buf_undo (foreign-procedure __collect_safe "cs_buf_undo" (int) void))
(define __cs_buf_redo (foreign-procedure __collect_safe "cs_buf_redo" (int) void))

(define __cs_buf_search_regex (foreign-procedure "cs_buf_search_regex" (int int string int) scheme-object))

(define file-ext-mode (list))

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
               (define-local major-mode 'mode)
	       (let ([m-map (mode-gen-map-symb 'mode)])
                  (when (top-level-bound? m-map)
                     (when parent
	                (let ([p-map (mode-gen-map-symb 'parent)])
                           (if (local-symbol-bound? p-map)
                              (keymap-set-parent (top-level-value m-map) (get-local-symbol p-map))
                              ;; else
                              (keymap-set-parent (top-level-value m-map) p-map)
                           )
                        )
		     )
                     (buffer-set-keymap m-map)
                  )
		  (buffer-set-mode-name name)
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

(define buffer-set-mode-name
   (lambda (n)
      (call-foreign (__cs_buf_mode_name_set (current-buffer) (format "(~a)" n)))
   )
)

(define buffer-set-state-name
   (lambda (n)
      (call-foreign (__cs_buf_state_name_set (current-buffer) (format "~a" n)))
   )
)

(define %buffer-local-keymap
   (lambda ()
       (call-foreign (__cs_buf_kmap_get (current-buffer)))
   )
)

(define buffer-keymap
   (lambda ()
      (keymap-get-parent (%buffer-local-keymap))
   )
)

(define buffer-set-keymap
   (lambda (sym)
      (let ([lmap (%buffer-local-keymap)])
         (keymap-set-parent lmap sym)
      )
   )
)

(define current-buffer
   (lambda ()
      (call-foreign (__cs_buf_current_get))
   )
)

(define-syntax (with-current-buffer stx)
   (syntax-case stx ()
      ((_ buf exp ...)
       #`(let ([b buf])
            (fluid-let ([current-buffer (lambda () b)])
               (begin
                  exp
                  ...
               )
            )
         )
      )
   )
)

(define *buffer-enable-eof* #t)

(define cursor
   (lambda ()
      (call-foreign (__cs_buf_cursor_get (current-buffer)))
   )
)

(define cursor-set
   (lambda (p)
      (let ([c p])
         (when (and (not *buffer-enable-eof*)
                    (and (> c 0) (>= c (buffer-end-pos)))
               )
            (set! c (- (buffer-end-pos) 1))
         )
	 (call-foreign (__cs_buf_cursor_set (current-buffer) c))
         c
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
   (case-lambda
      [()
       (call-foreign (__cs_buf_name_get (current-buffer)))]

      [(b)
       (call-foreign (__cs_buf_name_get b))]
   )
)

(define buffer-set-name
   (lambda (n)
      (call-foreign (__cs_buf_name_set (current-buffer) n))
   )
)

(define buffer-set-readonly
   (case-lambda
      [(read-only?)
       (buffer-set-readonly (current-buffer) read-only?)]

      [(buf read-only?)
       (call-foreign (__cs_buf_readonly_set buf read-only?))]
   )
)

(define buffer-is-readonly?
   (case-lambda
      [()
       (buffer-is-readonly? (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_readonly_get buf))]
   )
)

(define-syntax (buffer-modify stx)
   (syntax-case stx ()
      ((_ exp ...)
       #`(if (buffer-is-readonly?)
            (begin
               (message "buffer is readonly")
               (cursor)
            )
            ;; else
            (begin
               exp
               ...
            )
         )
      )
   )
)

(define buffer-create
   (case-lambda
      [() 
       (let ([b (window-buffer (window-create))])
          (set-text-style '(:fg "white"))
          b
       )
      ]

      [(n) 
       (let ([b (buffer-create)])
          (buffer-set-name n)
          b
       )
      ]
   )
)

(define buffer-create-text
   (case-lambda
      [() 
       (let ([b (buffer-create)])
          (text-mode)
          b
       )
      ]

      [(n) 
       (let ([b (buffer-create n)])
          (text-mode)
          b
       )
      ]
   )
)

(define buffer-new
   (case-lambda
      [() 
       (buffer-new "")
      ]

      [(n) 
       (let ([b (call-foreign (__cs_buf_new n))])
          (with-current-buffer b
             (set-text-style '(:fg "white"))
          )
          b
       )
      ]
   )
)

(define buffer-delete
   (case-lambda
      [()
         (buffer-delete (current-buffer))]

      [(b)
         (call-foreign (__cs_buf_del b))]
   )
)

(define buffer-get
   (lambda (n)
      (call-foreign (__cs_buf_by_name n))
   )
)

(define buffer-open-file
   (lambda (f)
      (let ([ok (call-foreign (__cs_buf_file_open (current-buffer) f)) ])
         (when ok
            (for-each
               (lambda (ext-mode)
                  (let ([ext (path-extension (buffer-filename))])
                     (when (string=? ext (car ext-mode))
                        ((top-level-value (cdr ext-mode)))
                     ) 
                  )
               )
               file-ext-mode
            )
         )
      )
   )
)

(define buffer-reload-file
   (lambda ()
      (save-cursor
         (erase-buffer)
         (insert-file (buffer-filename))
         (buffer-save)
      )
   )
)

(define buffer-filename
   (lambda ()
      (call-foreign (__cs_buf_file_get (current-buffer)))
   )
)

(define buffer-set-filename
   (lambda (f)
      (let (
            [first-path (path-first f)]
            [path f]
           )
         (if (equal? first-path "~")
            (set! path
               (format "~a/~a"
                  (getenv "HOME")
                  (path-rest f)
               )
            )
         )
         (call-foreign (__cs_buf_file_set (current-buffer) path))
      )
   )
)

(define buffer-save
   (lambda ()
      (call-foreign (__cs_buf_save (current-buffer)))
   )
)

(define buffer-first
   (lambda ()
       (call-foreign (__cs_buf_first_get))
   )
)

(define buffer-next
   (case-lambda
      [()
       (call-foreign (__cs_buf_next_get (current-buffer)))]

      [(bid)
       (call-foreign (__cs_buf_next_get bid))]
   )
)

(define buffer-list
   (lambda ()
      (let ([buf (buffer-first)]
            [lst   '()]
	   )

         (while buf
            (when (and
                     (not (equal? (buffer-name buf) "*minibuf*"))
                     (not (equal? (buffer-name buf) "*topbar*"))
                  )
               (set! lst (append lst (list
                                        (list buf (buffer-name buf))
				     )
               )         )
            )
            (set! buf (buffer-next buf))
         )

	 lst
      )
   )
)

(define enable-insert
   (lambda (e)
      (call-foreign (__cs_buf_text_input_enable (current-buffer) e))
   )
)

(define set-text-style
   (lambda (s . e)
      (let ([a (append (list s) e)])
         (for-each
            (lambda (o)
               (cond
                  [(equal? (car o) ':fg)
		     (call-foreign (__cs_buf_text_fg_set (current-buffer) (color-name->number (cadr o))))]
                  [(equal? (car o) ':bg)
		     (call-foreign (__cs_buf_text_bg_set (current-buffer) (color-name->number (cadr o))))]
                  [(equal? (car o) ':attr)
		     (call-foreign (__cs_buf_text_style_set (current-buffer) (style-name->number (cadr o))))]
               )
            )
            a
         )
      )
   )
)

(define symbol->text-property-type
   (lambda (s)
      (case s
         [('style)     1]
         [('highlight) 2]
         [('keymap)    3]
         [('all)       10000]
         [else         0]
      )
   )
)

(define add-text-style-property
   (lambda (s e a)
      (let ([l (style->list a)])
         (call-foreign (__cs_buf_prop_style_add (current-buffer)
				  1
                                  (list-ref l 0)
                                  (list-ref l 1)
                                  (list-ref l 2)
                                  s e))
      )
   )
)

(define add-text-keymap-property
   (lambda (s e k)
      (call-foreign (__cs_buf_prop_kmap_add (current-buffer) k s e))
   )
)

(define add-text-property
   (lambda (s e p)
      (when (equal? 'style (car p))
         (add-text-style-property s e (cadr p))
      )
      (when (equal? 'keymap (car p))
         (add-text-keymap-property s e (cadr p))
      )
   )
)

(define remove-text-property
   (case-lambda
      [(s e t)
       (call-foreign (__cs_buf_prop_del (current-buffer) (symbol->text-property-type t) s e))]

      [(s e)
       (call-foreign (__cs_buf_prop_del (current-buffer) (symbol->text-property-type 'all) s e))]

      [(t)
       (call-foreign (__cs_buf_prop_del (current-buffer) (symbol->text-property-type t) -1 -1))]

      [()
       (call-foreign (__cs_buf_prop_del (current-buffer) (symbol->text-property-type 'all) -1 -1))]
   )
)

(define highlight-range
   (lambda (s e)
      (call-foreign (__cs_buf_prop_style_add (current-buffer) 2 -1 -1 -1 s e))
   )
)

(define highlight-clear
   (lambda ()
      (call-foreign (__cs_buf_prop_del (current-buffer) 2 -1 -1))
   )
)

(define insert
   (lambda (t . s)
      (if (equal? (length s) 0)
         (call-foreign (__cs_buf_text_insert (current-buffer) t))
         ;; else
         (begin
            (let ([c (cursor)])
               (let ([p (- (call-foreign (__cs_buf_text_insert (current-buffer) t)) 1)])
                  (for-each
                     (lambda (a)
                        (add-text-property c p a)
                     ) s
                  )
                  p
               )
            )
	 )
      )
   )
)

(define insert-nl
   (lambda ()
      (buffer-modify (call-foreign (__cs_buf_text_insert_nl (current-buffer) (cursor))))
   )
)

(define insert-empty-line-up
   (lambda ()
      (buffer-modify
         (move-prev-line-end)
         (if (equal? (cursor) 0)
            (save-cursor (insert-nl))
            ;; else
            (insert-nl)
         )
      )
   )
)

(define insert-empty-line
   (lambda ()
      (buffer-modify
         (move-line-end)
         (insert-nl)
      )
   )
)

(define insert-file
   (lambda (t)
      (buffer-modify (call-foreign (__cs_buf_text_insert_file (current-buffer) t)))
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

(define move-line-num
   (lambda (n)
      (cursor-set (next-line-pos (current-buffer) 0 (- n 1)))
   )
)

(define move-line-up
   (lambda ()
      (cursor-set (prev-line-pos))
   )
)

(define move-line-down
   (lambda ()
      (when (not (is-last-line?))
         (cursor-set (next-line-pos))
      )
   )
)

(define move-next-line
   (lambda ()
      (cursor-set (next-line-begin-pos))
   )
)

(define move-prev-line-end
   (lambda ()
      (cursor-set (prev-line-end-pos))
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

(define is-last-line?
   (lambda ()
      (<= (- (buffer-end-pos) (line-end-pos)) 1)
   )
)

(define move-each-line
   (lambda (fn)
      (let loop ()
         (move-line-begin)
         (fn)
         (when (not (is-last-line?))
            (move-next-line)
            (loop)
         )
      )
   )
)

(define text-obj-pos
   (lambda (buf curs obj num)
      (call-foreign (__cs_buf_text_obj_pos buf curs obj num))
   )
)

(define next-char-pos
   (case-lambda
      [()
       (text-obj-pos (current-buffer) (cursor) #\c 1)]

      [(s)
       (text-obj-pos (current-buffer) s #\c 1)]
   )
)

(define prev-char-pos
   (case-lambda
      [()
       (text-obj-pos (current-buffer) (cursor) #\c -1)]

      [(s)
       (text-obj-pos (current-buffer) s #\c -1)]
   )
)

(define next-word-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\w 1)
   )
)

(define prev-word-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\w -1)
   )
)

(define word-end-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\e 1)
   )
)

(define next-longword-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\W 1)
   )
)

(define prev-longword-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\W -1)
   )
)

(define longword-end-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\E 1)
   )
)

(define prev-line-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\l -1)
   )
)

(define next-line-pos
   (case-lambda
      [()
       (next-line-pos (current-buffer) (cursor))]

      [(s)
       (next-line-pos (current-buffer) s)]

      [(b s)
       (text-obj-pos b s #\l 1)]

      [(b s n)
       (text-obj-pos b s #\l n)]
   )
)

(define next-line-begin-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\L 1)
   )
)

(define prev-line-end-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\L -1)
   )
)

(define line-start-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\0 -1)
   )
)

(define line-finish-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\0 1)
   )
)

(define line-begin-pos
   (case-lambda
      [()
       (line-begin-pos (current-buffer) (cursor))]

      [(s)
       (line-begin-pos (current-buffer) s)]

      [(b s)
       (text-obj-pos b s #\1 -1)]
   )
)

(define line-end-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\1 1)
   )
)

(define buffer-begin-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\g 1)
   )
)

(define buffer-end-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\g -1)
   )
)

(define buffer-string
   (case-lambda
      [()
       (call-foreign (__cs_buf_text_get (current-buffer)
                          (buffer-begin-pos)
                          (- (buffer-end-pos) (buffer-begin-pos))))]

      [(s e)
       (call-foreign (__cs_buf_text_get (current-buffer) s (- e s)))]
   )
)

(define buffer-is-visible?
   (case-lambda
      [()
       (call-foreign (__cs_buf_is_visible (current-buffer)))]

      [(b)
       (call-foreign (__cs_buf_is_visible b))]
   )
)

(define buffer-is-term?
   (case-lambda
      [()
       (call-foreign (__cs_buf_is_term (current-buffer)))]

      [(b)
       (call-foreign (__cs_buf_is_term b))]
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

(define text-obj-range
   (lambda (buf curs obj t?)
      (call-foreign (__cs_buf_text_obj_range buf curs obj t?))
   )
)

(define extract-word
   (case-lambda
      [()
       (extract-word (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\w #t)])
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
       (let ([r (text-obj-range (current-buffer) s #\W #t)])
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
       (let ([r (text-obj-range (current-buffer) s #\l #f)])
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
       (let ([r (text-obj-range (current-buffer) s #\l #t)])
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
       (let ([r (text-obj-range (current-buffer) s #\[ #f)])
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
       (let ([r (text-obj-range (current-buffer) s #\[ #t)])
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
       (let ([r (text-obj-range (current-buffer) s #\{ #f)])
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
       (let ([r (text-obj-range (current-buffer) s #\{ #t)])
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
       (let ([r (text-obj-range (current-buffer) s #\< #f)])
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
       (let ([r (text-obj-range (current-buffer) s #\< #t)])
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
       (let ([r (text-obj-range (current-buffer) s #\( #f)])
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
       (let ([r (text-obj-range (current-buffer) s #\( #t)])
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
       (let ([r (text-obj-range (current-buffer) s #\" #f)])
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
       (let ([r (text-obj-range (current-buffer) s #\" #t)])
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
       (let ([r (text-obj-range (current-buffer) s #\' #f)])
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
       (let ([r (text-obj-range (current-buffer) s #\' #t)])
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
       (let ([r (text-obj-range (current-buffer) s #\` #f)])
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
       (let ([r (text-obj-range (current-buffer) s #\` #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define delete-range
   (lambda (s e)
      (buffer-modify (call-foreign (__cs_buf_text_range_del (current-buffer) s e)))
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
      (buffer-modify
         (let (
               [end (fn)]
               [start (cursor)]
              )
            (delete-range start end)
         )
      )
   )
)

(define cursor-obj-delete-inclusive
   (lambda (fn)
      (buffer-modify
         (let (
               [end (next-char-pos (fn))]
               [start (cursor)]
              )
            (delete-range start end)
         )
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
      (cursor-obj-delete-inclusive move-word-end)
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
      (cursor-obj-delete-inclusive move-longword-end)
   )
)

(define delete-next-line-begin
   (lambda ()
      (cursor-obj-delete move-next-line)
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

(define delete-line
   (lambda ()
      (move-line-end)
      (delete-prev-line-end)
      (if (equal? (cursor) 0)
         (delete-char)
         ;; else
         (if (equal? (buffer-end-pos) (1+ (line-end-pos)))
            (move-line-begin)
            ;; else
            (move-next-line)
         )
      )
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

(define erase-buffer
   (lambda ()
      (let (
            [s (buffer-begin-pos)]
            [e (buffer-end-pos)]
           )
         (remove-text-property s e)
         (delete-range s e)
      )
   )
)

(define mark-set
   (case-lambda
      [()
       (call-foreign (__cs_buf_mark_set (current-buffer) (cursor)))]

      [(s)
       (call-foreign (__cs_buf_mark_set (current-buffer) s))]
   )
)

(define mark-get
   (lambda ()
      (call-foreign (__cs_buf_mark_get (current-buffer)))
   )
)

(define mark-clear
   (lambda ()
      (call-foreign (__cs_buf_mark_clear (current-buffer)))
   )
)

(define mark-get-range
   (lambda ()
      (let (
            [m (mark-get)]
            [c (cursor)]
           )
           (let (
                 [s (min m c)]
                 [e (max m c)]
                )
	      (list s (next-char-pos e))
           )
      )
   )
)

(define mark-delete
   (lambda ()
      (let ([r (mark-get-range)])
         (delete-range (car r) (cadr r))
      )
   )
)

(define mark-copy
   (lambda ()
      (let ([r (mark-get-range)])
           (copybuf-copy (buffer-string (car r) (cadr r)))
      )
   )
)

(define mark-copy-append
   (lambda ()
      (let ([r (mark-get-range)])
           (copybuf-append (buffer-string (car r) (cadr r)))
      )
   )
)

(define mark-copy-linewise
   (lambda ()
      (let ([r (mark-get-range)])
           (copybuf-copy (buffer-string (car r) (cadr r)) #t)
      )
   )
)

(define mark-copy-append-linewise
   (lambda ()
      (let ([r (mark-get-range)])
           (copybuf-append (buffer-string (car r) (cadr r)) #t)
      )
   )
)

(define mark-highlight
   (case-lambda
      [(e)
       (mark-highlight (__cs_win_current_get) e)]

      [(wid e)
       (when wid (call-foreign (__cs_win_mark_highlight wid e)))]
   )
)

(define copy-line
   (lambda ()
      (copybuf-copy (buffer-string (line-begin-pos) (1+ (line-end-pos))) #t)
   )
)

(define buffer-env
   (lambda ()
      (call-foreign (__cs_buf_env_get (current-buffer)))
   )
)

(define-syntax (define-local stx)
   (syntax-case stx ()
	       ((_ s v)
		#`(define-top-level-value 's v (buffer-env))
               )
   )
)

(define local-symbol-bound?
   (lambda (sym)
      (top-level-bound? sym (buffer-env))
   )
)

(define get-local-symbol
   (lambda (sym)
      (top-level-value sym (buffer-env))
   )
)

(define set-local-symbol!
   (lambda (sym val)
      (set-top-level-value! sym val (buffer-env))
   )
)

(define-syntax (get-local stx)
   (syntax-case stx ()
	       ((_ s)
		#`(get-local-symbol 's)
               )
	       ((_ s e)
		#`(if (local-bound? s) (get-local s) e)
               )
   )
)

(define-syntax (set-local! stx)
   (syntax-case stx ()
	       ((_ s v)
		#`(set-local-symbol! 's v)
               )
   )
)

(define-syntax (local-bound? stx)
   (syntax-case stx ()
	       ((_ s)
		#`(local-symbol-bound? 's)
               )
   )
)

(define buffer-select-switch
   (lambda ()
      (let (
              [b (buffer-get (extract-longword))]
              [p (window-prev-selected)]
           )
         (when b
            (window-switch-buffer p b)
            (window-delete)
	    (window-select p)
         )
      )
   )
)

(define buffer-select-open
   (lambda ()
      (let (
            [b (buffer-get (extract-longword))]
           )
         (when b
            (window-delete)
            (window-create b)
         )
      )
   )
)

(define buffer-select-switch-or-open
   (lambda ()
      (if (get-local open-buffer)
         (buffer-select-open)
         ;; else
         (buffer-select-switch)
      )
   )
)

(define buffer-switch-or-open-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" buffer-select-switch-or-open)
      (bind-key map "<Esc>" window-delete)
      (bind-key map "q" window-delete)
      (bind-key map "j" (lambda ()
                                   (highlight-clear)
                                   (move-line-down)
                                   (highlight-range
                                      (line-begin-pos) (line-end-pos))))
      (bind-key map "k" (lambda ()
                                   (highlight-clear)
                                   (move-line-up)
                                   (highlight-range
                                      (line-begin-pos) (line-end-pos))))
      map
   )
)

(define buffer-switch-or-open
   (lambda (open?)
      (let (
              [b (buffer-create)]
              [l (buffer-list)]
           )
         (window-popup #t)
         (with-current-buffer b
            (define-local open-buffer open?)
            (if open?
               (buffer-set-name "Open buffer")
               ;; else
               (buffer-set-name "Switch buffer")
            )
            (buffer-set-keymap 'buffer-switch-or-open-map)
            (for-each
               (lambda (x)
                  (insert (format "~a\n" (cadr x)) '(style (:attr "bold")))
               ) l
            )
         )
         (move-buffer-begin)
         (highlight-range (line-begin-pos) (line-end-pos))
      )
   )
)

(define buffer-switch
   (lambda ()
      (buffer-switch-or-open #f)
   )
)

(define buffer-open
   (lambda ()
      (buffer-switch-or-open #t)
   )
)

(define buffer-snapshot
   (lambda()
      (call-foreign (__cs_buf_snapshot (current-buffer)))
   )
)

(define buffer-undo
   (lambda()
      (call-foreign (__cs_buf_undo (current-buffer)))
   )
)

(define buffer-redo
   (lambda()
      (call-foreign (__cs_buf_redo (current-buffer)))
   )
)

(define message
   (lambda (s)
      (let ([b (buffer-get "*Messages*")])
         (when b
            (with-current-buffer b
               (insert (format "~a\n" s))
            )
         )
         (run-hooks 'on-message-hook s)
      )
   )
)

(define current-cwd
   (lambda ()
      (view-cwd)
   )
)

(define buffer-set-cwd
   (lambda (cwd)
      (define-local current-cwd cwd)
   )
)

(define buffer-cwd
   (lambda ()
      (if (and (local-bound? current-cwd) (get-local current-cwd))
         (get-local current-cwd)
         ;; else
         (view-cwd)
      )
   )
)

(define search-reg "")

(define search-regex
   (case-lambda
      [(rx)
       (search-regex rx (cursor))]

      [(rx pos)
       (search-regex rx (cursor) +1)]

      [(rx pos dir)
       (call-foreign (__cs_buf_search_regex (current-buffer) pos rx dir))]
   )
)

(define search-next
   (lambda ()
      (cursor-set (search-regex search-reg (cursor) +1))
   )
)

(define search-prev
   (lambda ()
      (cursor-set (search-regex search-reg (cursor) -1))
   )
)

(define search-word-direction
   (lambda (word dir)
      (let ([pattern (format "\\<~a\\>" word)])
         (set! search-reg pattern)
         (cursor-set (search-regex pattern (cursor) dir))
      )
   )
)

(define search-word-forward
   (case-lambda
      [()
       (search-word-forward (extract-word))]

      [(w)
       (search-word-direction w +1)]
   )
)

(define search-word-backward
   (case-lambda
      [()
       (search-word-backward (extract-word))]

      [(w)
       (search-word-direction w -1)]
   )
)

(define search-regex-read
   (lambda ()
      (when (not (buffer-is-term?))
         (minibuf-read "/"
            (lambda (r)
               (set! search-reg r)
               (cursor-set (search-regex r))
            )
         )
      )
   )
)
