(define __cs_buf_new (foreign-procedure "cs_buf_new" (string) scheme-object))
(define __cs_buf_is_valid (foreign-procedure "cs_buf_is_valid" (int) scheme-object))
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
(define __cs_buf_text_input_enable (foreign-procedure __collect_safe "cs_buf_text_input_enable" (int boolean) void))

(define __cs_buf_text_fg_set (foreign-procedure "cs_buf_text_fg_set" (int int) void))
(define __cs_buf_text_bg_set (foreign-procedure "cs_buf_text_bg_set" (int int) void))
(define __cs_buf_text_style_set (foreign-procedure "cs_buf_text_style_set" (int int) void))
(define __cs_buf_text_fg_get (foreign-procedure "cs_buf_text_fg_get" (int) scheme-object))
(define __cs_buf_text_bg_get (foreign-procedure "cs_buf_text_bg_get" (int) scheme-object))
(define __cs_buf_text_style_get (foreign-procedure "cs_buf_text_style_get" (int) scheme-object))

(define __cs_buf_prop_style_add (foreign-procedure "cs_buf_prop_style_add" (int int int int int string int int string string boolean) scheme-object))
(define __cs_buf_prop_kmap_add (foreign-procedure "cs_buf_prop_kmap_add" (int int int int string string) scheme-object))
(define __cs_buf_prop_symbol_add (foreign-procedure "cs_buf_prop_symbol_add" (int string int int string string) scheme-object))
(define __cs_buf_prop_data_add (foreign-procedure "cs_buf_prop_data_add" (int scheme-object int int string string) scheme-object))
(define __cs_buf_prop_del (foreign-procedure "cs_buf_prop_del" (int int int int string string) void))
(define __cs_buf_prop_get (foreign-procedure "cs_buf_prop_get" (int int int int string) scheme-object))

(define __cs_buf_cursor_get (foreign-procedure __collect_safe "cs_buf_cursor_get" (int) scheme-object))
(define __cs_buf_cursor_set (foreign-procedure __collect_safe "cs_buf_cursor_set" (int int) void))
(define __cs_buf_line_num (foreign-procedure __collect_safe "cs_buf_line_num" (int int) scheme-object))
(define __cs_buf_mode_name_set (foreign-procedure "cs_buf_mode_name_set" (int string) void))
(define __cs_buf_mode_name_get (foreign-procedure "cs_buf_mode_name_get" (int) scheme-object))
(define __cs_buf_state_name_set (foreign-procedure "cs_buf_state_name_set" (int string) void))

(define __cs_buf_file_open (foreign-procedure "cs_buf_file_open" (int string) scheme-object))
(define __cs_buf_file_set (foreign-procedure "cs_buf_file_set" (int string) void))
(define __cs_buf_file_get (foreign-procedure "cs_buf_file_get" (int) scheme-object))
(define __cs_buf_save (foreign-procedure "cs_buf_save" (int) scheme-object))

(define __cs_buf_is_visible (foreign-procedure __collect_safe "cs_buf_is_visible" (int) scheme-object))
(define __cs_buf_is_term (foreign-procedure __collect_safe "cs_buf_is_term" (int) scheme-object))

(define __cs_buf_env_get (foreign-procedure __collect_safe "cs_buf_env_get" (int) scheme-object))

(define __cs_buf_snapshot (foreign-procedure __collect_safe "cs_buf_snapshot" (int) void))
(define __cs_buf_undo (foreign-procedure __collect_safe "cs_buf_undo" (int) void))
(define __cs_buf_redo (foreign-procedure __collect_safe "cs_buf_redo" (int) void))

(define __cs_buf_search_regex (foreign-procedure "cs_buf_search_regex" (int int string int) scheme-object))

(define __cs_buf_tag_set (foreign-procedure __collect_safe "cs_buf_tag_set" (int int) int))
(define __cs_buf_tag_bits (foreign-procedure __collect_safe "cs_buf_tag_bits" (int) int))
(define __cs_buf_tag_toggle (foreign-procedure __collect_safe "cs_buf_tag_toggle" (int int) int))
(define __cs_buf_tag_add (foreign-procedure __collect_safe "cs_buf_tag_add" (int int) int))
(define __cs_buf_tag_del (foreign-procedure __collect_safe "cs_buf_tag_del" (int int) int))

(define %dir-locals-ht (make-hashtable string-hash string=?))

(define file-match-mode (list))

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
      (call-foreign (__cs_buf_mode_name_set (current-buffer) (format "~a" n)))
   )
)

(define buffer-mode-name
   (case-lambda
      [()
       (buffer-mode-name (current-buffer))
      ]

      [(b)
       (call-foreign (__cs_buf_mode_name_get b))
      ]
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
      (keymap-parent (%buffer-local-keymap))
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

(define *buffer-enable-eof* #t)

(define-syntax (define-local stx)
   (syntax-case stx ()
	       ((_ s v)
		#`(define-top-level-value 's v (buffer-env))
               )
   )
)

(define dir-local-symbol-bound?
   (case-lambda
      [(sym)
       (dir-local-symbol-bound? (current-cwd) sym)
      ]

      [(dir sym)
       (let (
             [dir-env (hashtable-ref %dir-locals-ht dir #f)]
            )
          (and dir-env (top-level-bound? sym dir-env))
       )
      ]
   )
)

(define dir-get-local-symbol
   (lambda (sym)
      (let (
            [dir-env (hashtable-ref %dir-locals-ht (path-parent (buffer-filename)) #f)]
           )
         (if (and dir-env (top-level-bound? sym dir-env))
            (top-level-value sym dir-env)
            ;; else
            (let (
                  [cwd-env (hashtable-ref %dir-locals-ht (current-cwd) #f)]
                 )
               (top-level-value sym cwd-env)
            )
         )
      )
   )
)

(define dir-set-local-symbol!
   (lambda (dir sym val)
      (let (
            [dir-env (hashtable-ref %dir-locals-ht dir #f)]
           )
         (if dir-env
            (set-top-level-value! sym val dir-env)
            ;; else
            (let ([env (copy-environment (scheme-environment))])
               (hashtable-set! %dir-locals-ht dir env)
               (define-top-level-value sym val env)
            )
         )
      )
   )
)

(define-syntax (dir-set-local! stx)
   (syntax-case stx ()
	       ((_ dir sym val)
		#`(dir-set-local-symbol! dir 'sym val)
               )
   )
)

(define local-symbol-bound?
   (lambda (sym)
      (or (top-level-bound? sym (buffer-env))
          (dir-local-symbol-bound? (current-cwd) sym)
          (dir-local-symbol-bound? (path-parent (buffer-filename)) sym))
   )
)

(define get-local-symbol
   (lambda (sym)
      (if (top-level-bound? sym (buffer-env))
         (top-level-value sym (buffer-env))
         ;; else
         (dir-get-local-symbol sym)
      )
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

(define cursor
   (lambda ()
      (call-foreign (__cs_buf_cursor_get (current-buffer)))
   )
)

(define cursor-set
   (lambda (p)
      (when p
         (let ([c p])
            (when (and (not *buffer-enable-eof*)
                       (and (> c 0) (>= c (text-end-pos)))
                  )
               (set! c (- (text-end-pos) 1))
            )
	    (call-foreign (__cs_buf_cursor_set (current-buffer) c))
            c
         )
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

(define buffer-line-num
   (lambda (pos)
      (call-foreign (__cs_buf_line_num (current-buffer) pos))
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

(define buffer-is-valid?
   (lambda (bid)
      (call-foreign (__cs_buf_is_valid bid))
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

(define buffer-get-or-create
   (lambda (name)
      (let ([buf (buffer-get name)])
         (if buf
            buf
            ;; else
            (buffer-create name)
         )
      )
   )
)

(define buffer-open-file
   (lambda (f)
      (let ([ok (call-foreign (__cs_buf_file_open (current-buffer) f)) ])
         (when ok
            (for-each
               (lambda (match)
                  (let ([fname (buffer-filename)])
                     (when (pregexp-match (car match) fname)
                        ((top-level-value (cdr match)))
                     ) 
                  )
               )
               file-match-mode
            )
         )
      )
   )
)

(define buffer-reload
   (lambda ()
      (when (local-bound? buffer-reload-func)
         ((get-local buffer-reload-func))
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

(define buffer-list-by-tag
   (lambda (tag)
      (filter
         (lambda (b)
            (member tag (buffer-tags (first b)))
         )
         (buffer-list)
      )
   )
)

(define buffer-list-hidden-by-tag
   (lambda (tag)
      (filter
         (lambda (b)
            (and
               (member tag (buffer-tags (first b)))
               (not (buffer-is-visible? (first b)))
            )
         )
         (buffer-list)
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
         [':style     1]
         [':highlight 2]
         [':keymap    3]
         [':symbol    4]
         [':data      5]
         [':all       10000]
         [else         0]
      )
   )
)

(define __add-style-property
   (case-lambda
      [(style regex)
       (__add-style-property style -1 -1 regex)
      ]

      [(style start end)
       (__add-style-property style start end #f)
      ]

      [(style start end regex)
       (__add-style-property style start end #f #f)
      ]
      
      [(style start end regex name)
       (let ([l (if (symbol? style)
                    (list -1 -1 -1)
                    (style->list style)
                )
             ]
            )
         (call-foreign
            (__cs_buf_prop_style_add
               (current-buffer)
               1
               (list-ref l 0)
               (list-ref l 1)
               (list-ref l 2)
               (if (symbol? style)
                  (symbol->string style)
                  #f
               )
               start
               end
               regex
               name
               (plist-get style ':expand)
            )
         )
       )
      ]
   )
)

(define __add-keymap-property
   (case-lambda
      [(kmap regex)
       (__add-keymap-property kmap -1 -1 regex)
      ]

      [(kmap start end)
       (__add-keymap-property kmap start end #f)
      ]

      [(kmap start end regex)
       (__add-keymap-property kmap start end #f #f)
      ]
      
      [(kmap start end regex name)
       (call-foreign (__cs_buf_prop_kmap_add (current-buffer) kmap start end regex name))
      ]
   )
)

(define __add-symbol-property
   (case-lambda
      [(symbol start end)
       (__add-symbol-property symbol start end #f)
      ]

      [(symbol start end regex)
       (__add-symbol-property symbol start end #f #f)
      ]
      
      [(symbol start end regex name)
       (call-foreign (__cs_buf_prop_symbol_add (current-buffer) (symbol->string symbol) start end regex name))
      ]
   )
)

(define __add-data-property
   (case-lambda
      [(data start end)
       (__add-data-property data start end #f)
      ]

      [(data start end regex)
       (__add-data-property data start end #f #f)
      ]
      
      [(data start end regex name)
       (call-foreign (__cs_buf_prop_data_add (current-buffer) data start end regex name))
      ]
   )
)

(define add-text-property
   (case-lambda
      [(regex plist)
       (add-text-property -1 -1 regex plist)
      ]

      [(start end plist)
       (let (
             [name (plist-get plist ':name)]
             [regex (plist-get plist ':regex)]
            )
          (plist-for-each plist
             (lambda (prop val)
                (case prop
                   [':style (__add-style-property val start end regex name)]
                   [':keymap (__add-keymap-property val start end regex name)]
                   [':symbol (__add-symbol-property val start end regex name)]
                   [':data (__add-data-property val start end regex name)]
                )
             )
          )
       )
      ]
   )
)

(define remove-text-property
   (case-lambda
      [()
       (remove-text-property ':all -1 -1 #f)
      ]
    
      [(name)
       (remove-text-property ':all -1 -1 name)
      ]
    
      [(start end)
       (remove-text-property ':all start end #f)
      ]

      [(type start end)
       (remove-text-property type start end #f)
      ]

      [(type start end name)
       (call-foreign (__cs_buf_prop_del (current-buffer) (symbol->text-property-type type) start end #f name))
      ]
   )
)

(define get-text-property
   (case-lambda
      [(name)
       (get-text-property ':all -1 -1 name)
      ]
    
      [(start end)
       (get-text-property ':all start end #f)
      ]

      [(type start end)
       (get-text-property type start end #f)
      ]

      [(type start end name)
       (call-foreign (__cs_buf_prop_get (current-buffer) (symbol->text-property-type type) start end name))
      ]
   )
)

(define set-text-property
   (case-lambda
      [(start end plist)
       (set-text-property start end #f plist)
      ]
      
      [(start end name plist)
       (let ([type #f])
          (plist-for-each plist
             (lambda (prop val)
                (when (member prop '(:style :data :symbol :keymap))
                   (set! type prop)
                )
             )
          )
          (when type
             (remove-text-property type start end name)
             (add-text-property start end plist)
          )
       )
      ]
   )
)

(define highlight-range
   (lambda (s e)
      (call-foreign (__cs_buf_prop_style_add (current-buffer) 2 -1 -1 -1 "highlight" s e #f #f #f))
   )
)

(define highlight-clear
   (lambda ()
      (call-foreign (__cs_buf_prop_del (current-buffer) 2 -1 -1 #f #f))
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

(define buffer-is-vterm?
   (case-lambda
      [()
       (call-foreign (__cs_buf_is_term (current-buffer)))]

      [(b)
       (call-foreign (__cs_buf_is_term b))]
   )
)

(define buffer-env
   (lambda ()
      (call-foreign (__cs_buf_env_get (current-buffer)))
   )
)

(define buffer-window
   (case-lambda
      [()
       (buffer-window (current-buffer))
      ]

      [(b)
       (let (
             [win-lst (filter
                         (lambda (w)
                            (equal? (window-buffer (first w)) b)
                         ) (window-list)
                      )
             ]
            )
          (if (null? win-lst)
             #f
             ;; else
             (first (first win-lst))
          )
       )
      ]
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

(define current-cwd
   (lambda ()
      (frame-cwd)
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
         (current-cwd)
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
       (search-word-forward (text-word))]

      [(w)
       (search-word-direction w +1)]
   )
)

(define search-word-backward
   (case-lambda
      [()
       (search-word-backward (text-word))]

      [(w)
       (search-word-direction w -1)]
   )
)

(define search-regex-read
   (lambda ()
      (when (not (buffer-is-vterm?))
         (minibuf-read "/"
            (lambda (r)
               (set! search-reg r)
               (cursor-set (search-regex r))
            )
         )
      )
   )
)

(define buffer-tags
   (case-lambda
      [()
       (buffer-tags (current-buffer))]

      [(bid)
       (let (
             [tag-bits (call-foreign (__cs_buf_tag_bits bid))]
             [tag-ls (list)]
            )
          (for-each
             (lambda (bit)
                (when (fxbit-set? tag-bits bit)
                   (set! tag-ls (append tag-ls (list (1+ bit))))
                )
             ) (list 0 1 2 3 4 5 6 7 8)
          )
          tag-ls
       )
      ]
   )
)

(define buffer-set-tag
   (case-lambda
      [(tag)
       (call-foreign (__cs_buf_tag_set (current-buffer) tag))]

      [(bid tag)
       (call-foreign (__cs_buf_tag_set bid tag))]
   )
)

(define buffer-toggle-tag
   (case-lambda
      [(tag)
       (call-foreign (__cs_buf_tag_toggle (current-buffer) tag))]

      [(bid tag)
       (call-foreign (__cs_buf_tag_toggle bid tag))]
   )
)

(define buffer-tag+
   (case-lambda
      [(tag)
       (call-foreign (__cs_buf_tag_add (current-buffer) tag))]

      [(bid tag)
       (call-foreign (__cs_buf_tag_add bid tag))]
   )
)

(define buffer-tag-
   (case-lambda
      [(tag)
       (call-foreign (__cs_buf_tag_del (current-buffer) tag))]

      [(bid tag)
       (call-foreign (__cs_buf_tag_del bid tag))]
   )
)

