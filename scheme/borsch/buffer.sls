(library (borsch buffer)
   (export
      buffer-insert-text
      buffer-begin-pos
      buffer-end-pos
      buffer-cursor
      buffer-set-cursor

      buffer-id
      buffer-ref-count
      buffer-ref-get
      buffer-ref-put
      buffer-new
      buffer-delete
      buffer-list
      buffer-for-each
      buffer-find
      buffer-get-by-file
      current-buffer
      with-current-buffer
      buffer-line-num
      buffer-set-mode-name
      buffer-mode-name
      buffer-set-state-name
      buffer-state-name
      buffer-snapshot
      buffer-undo
      buffer-redo
      buffer-save
      buffer-reload
      buffer-filename
      buffer-set-filename
      buffer-env
      local-symbol-bound?
      get-local-symbol
      set-local-symbol!
      get-local
      set-local!
      local-bound?
      define-local
      buffer-name
      buffer-set-name
      buffer-set-readonly
      buffer-is-readonly?
      buffer-is-modified?
      buffer-is-dirty?
      buffer-set-dirty
      add-text-property
      remove-text-property
      get-text-property
      set-text-property
      highlight-range
      highlight-clear
      buffer-get
      buffer-keymap
      buffer-set-keymap
      bind-key-local
      define-mode
      enable-insert
      insert-enabled?
      buffer-is-valid?
      buffer-is-visible?
      buffer-set-vterm
      buffer-is-vterm?
      buffer-set-cwd
      buffer-cwd
      buffer-set-mark
      buffer-mark
      buffer-is-mark-set?)
   (import (chezscheme)
           (borsch base)
           (borsch style)
           (borsch lists)
           (borsch keymap))

(define __cs_buf_new (foreign-procedure "cs_buf_new" (string int) scheme-object))
(define __cs_buf_ref_get (foreign-procedure "cs_buf_ref_get" (int) scheme-object))
(define __cs_buf_ref_put (foreign-procedure "cs_buf_ref_put" (int) scheme-object))
(define __cs_buf_ref (foreign-procedure "cs_buf_ref" (int) scheme-object))
(define __cs_buf_del (foreign-procedure "cs_buf_del" (int) void))

(define __cs_buf_line_num (foreign-procedure "cs_buf_line_num" (int int) scheme-object))

(define __cs_buf_snapshot (foreign-procedure "cs_buf_snapshot" (int) void))
(define __cs_buf_undo (foreign-procedure "cs_buf_undo" (int) void))
(define __cs_buf_redo (foreign-procedure "cs_buf_redo" (int) void))
(define __cs_buf_save (foreign-procedure "cs_buf_save" (int) scheme-object))

(define __cs_buf_file_set (foreign-procedure "cs_buf_file_set" (int string) void))
(define __cs_buf_file_get (foreign-procedure "cs_buf_file_get" (int) scheme-object))

(define __cs_buf_env_get (foreign-procedure "cs_buf_env_get" (int) scheme-object))

(define __cs_buf_name_get (foreign-procedure "cs_buf_name_get" (int) scheme-object))
(define __cs_buf_name_set (foreign-procedure "cs_buf_name_set" (int string) void))
(define __cs_buf_is_modified (foreign-procedure "cs_buf_is_modified" (int) scheme-object))

(define __cs_buf_prop_style_add (foreign-procedure "cs_buf_prop_style_add" (int int int int int int string int int string string boolean wchar) scheme-object))
(define __cs_buf_prop_kmap_add (foreign-procedure "cs_buf_prop_kmap_add" (int int int int string string) scheme-object))
(define __cs_buf_prop_symbol_add (foreign-procedure "cs_buf_prop_symbol_add" (int string int int string string) scheme-object))
(define __cs_buf_prop_data_add (foreign-procedure "cs_buf_prop_data_add" (int scheme-object int int string string) scheme-object))
(define __cs_buf_prop_del (foreign-procedure "cs_buf_prop_del" (int int int int string string) void))
(define __cs_buf_prop_get (foreign-procedure "cs_buf_prop_get" (int int int int string) scheme-object))

(define __cs_buf_kmap_get (foreign-procedure "cs_buf_kmap_get" (int) scheme-object))

(define __cs_buf_is_valid (foreign-procedure "cs_buf_is_valid" (int) scheme-object))
(define __cs_buf_is_visible (foreign-procedure "cs_buf_is_visible" (int) scheme-object))
(define __cs_buf_is_term (foreign-procedure "cs_buf_is_term" (int) scheme-object))
(define __cs_buf_term_set (foreign-procedure "cs_buf_term_set" (int int) void))

(define __cs_buf_cursor_get (foreign-procedure "cs_buf_cursor_get" (int) scheme-object))
(define __cs_buf_cursor_set (foreign-procedure "cs_buf_cursor_set" (int int) void))
(define __cs_buf_text_insert (foreign-procedure "cs_buf_text_insert" (int string) scheme-object))
(define __cs_buf_text_obj_pos (foreign-procedure "cs_buf_text_obj_pos" (int int char int) scheme-object))
(define __cs_buf_text_get (foreign-procedure "cs_buf_text_get" (int int int) scheme-object))

(define $dir-locals-ht (make-hashtable string-hash string=?))

(define current-buffer (make-parameter #f))

(define $buffer-list (list))

(define-record-type $buffer
   (fields
      id
      (mutable is-readonly)
      (mutable is-input-enabled)
      (mutable mode-name)
      (mutable state-name)
      (mutable mark)
      (mutable is-dirty)))

(define (buffer-id buf)
   ($buffer-id buf) )

(define (buffer-obj-pos buf curs obj num)
   (call-foreign (__cs_buf_text_obj_pos (buffer-id buf) curs obj num)))

(define (buffer-insert-text b text)
   (call-foreign (__cs_buf_text_insert (buffer-id b) text)))

(define (buffer-begin-pos b)
   (buffer-obj-pos b -1 #\g 1))

(define (buffer-end-pos b)
   (buffer-obj-pos b -1 #\g -1))

(define (buffer-cursor b)
   (call-foreign (__cs_buf_cursor_get (buffer-id (current-buffer)))) )

(define (buffer-set-cursor b pos)
   (call-foreign (__cs_buf_cursor_set (buffer-id b) pos)) )

(define (buffer-insert b)
   (set! $buffer-list (append $buffer-list (list b)))
   (run-hooks 'buffer-insert-hook b))

(define (buffer-remove b)
   (set! $buffer-list (remove b $buffer-list))
   (run-hooks 'buffer-remove-hook b))

(define buffer-ref-count
   (case-lambda
      [()
       (buffer-ref-count (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_ref (buffer-id buf)))]))

(define buffer-ref-get
   (case-lambda
      [()
       (buffer-ref-get (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_ref_get (buffer-id buf)))]))

(define buffer-ref-put
   (case-lambda
      [()
       (buffer-ref-put (current-buffer))]

      [(buf)
       (when (= 1 (buffer-ref-count buf))
          (buffer-remove buf))
       (call-foreign (__cs_buf_ref_put (buffer-id buf)))]))

(define ($buffer-new name kmap)
   (make-$buffer (call-foreign (__cs_buf_new name (or kmap -1)))
                 #f #t "" "" #f #f))

(define buffer-new
   (case-lambda
      [() 
       (buffer-new "")]

      [(name) 
       (buffer-new name global-keymap)]

      [(name kmap) 
       (let ([b ($buffer-new name kmap)])
          (buffer-insert b)
          (with-current-buffer b
             (define-local pre-draw-func #f) )
          b)]))

(define buffer-delete
   (case-lambda
      [()
       (buffer-delete (current-buffer))]

      [(b)
       (call-foreign (__cs_buf_del (buffer-id b)))
       (buffer-remove b)]))

(define (buffer-list)
   $buffer-list)

(define (buffer-for-each fn)
   (for-each
      (lambda (b)
         (fn b))
      (buffer-list)))

(define (buffer-find fn)
   (let ([b (find
               (lambda (b)
                  (fn b))
               (buffer-list))])
      b))

(define (buffer-get-by-file file)
   (buffer-find
      (lambda (b)
         (equal? file (buffer-filename b)))))

(define-syntax (with-current-buffer stx)
   (syntax-case stx ()
      ((_ buf exp ...)
       #`(parameterize ([current-buffer buf])
            exp ... ))))

(define (buffer-line-num pos)
   (call-foreign (__cs_buf_line_num (buffer-id (current-buffer)) pos)))

(define (buffer-set-mode-name name)
   ($buffer-mode-name-set! (current-buffer) name)
   (buffer-set-dirty (current-buffer) #t) )

(define buffer-mode-name
   (case-lambda
      [()
       (buffer-mode-name (current-buffer))]

      [(b)
       ($buffer-mode-name b)]))

(define (buffer-set-state-name name)
   ($buffer-state-name-set! (current-buffer) name)
   (buffer-set-dirty (current-buffer) #t) )

(define buffer-state-name
   (case-lambda
      [()
       (buffer-state-name (current-buffer))]

      [(b)
       ($buffer-state-name b)]))

(define (buffer-snapshot)
   (call-foreign (__cs_buf_snapshot (buffer-id (current-buffer)))))

(define (buffer-undo)
   (buffer-set-dirty (current-buffer) #t)
   (call-foreign (__cs_buf_undo (buffer-id (current-buffer)))))

(define (buffer-redo)
   (buffer-set-dirty (current-buffer) #t)
   (call-foreign (__cs_buf_redo (buffer-id (current-buffer)))))

(define (buffer-save)
   (call-foreign (__cs_buf_save (buffer-id (current-buffer)))))

(define (buffer-reload)
   (when (local-bound? buffer-reload-func)
      (buffer-set-dirty (current-buffer) #t)
      ((get-local buffer-reload-func))))

(define buffer-filename
   (case-lambda
     [()
      (buffer-filename (current-buffer))]

     [(b)
      (call-foreign (__cs_buf_file_get (buffer-id b)))]))

(define (buffer-set-filename f)
   (let ([first-path (path-first f)]
         [path f])
      (if (equal? first-path "~")
         (set! path
            (format "~a/~a"
               (getenv "HOME")
               (path-rest f))))
      (buffer-set-dirty (current-buffer) #t)
      (call-foreign (__cs_buf_file_set (buffer-id (current-buffer)) path))))

(define (buffer-env)
   (call-foreign (__cs_buf_env_get (buffer-id (current-buffer)))))

(define dir-local-symbol-bound?
   (case-lambda
      [(sym)
       (dir-local-symbol-bound? (current-cwd) sym)]

      [(dir sym)
       (let ([dir-env (hashtable-ref $dir-locals-ht dir #f)])
          (and dir-env (top-level-bound? sym dir-env)))]))

(define (dir-get-local-symbol sym)
   (let ([dir-env (hashtable-ref $dir-locals-ht (path-parent (buffer-filename)) #f)])
      (if (and dir-env (top-level-bound? sym dir-env))
         (top-level-value sym dir-env)
         ;; else
         (let ([cwd-env (hashtable-ref $dir-locals-ht (current-cwd) #f)])
            (top-level-value sym cwd-env)))))

(define (dir-set-local-symbol! dir sym val)
   (let ([dir-env (hashtable-ref $dir-locals-ht dir #f)])
      (if dir-env
         (set-top-level-value! sym val dir-env)
         ;; else
         (let ([env (copy-environment (scheme-environment))])
            (hashtable-set! $dir-locals-ht dir env)
            (define-top-level-value sym val env)))))

(define-syntax (dir-set-local! stx)
   (syntax-case stx ()
      ((_ dir sym val)
       #`(dir-set-local-symbol! dir 'sym val))))

(define (local-symbol-bound? sym)
   (or (top-level-bound? sym (buffer-env))
       (dir-local-symbol-bound? (current-cwd) sym)
       (dir-local-symbol-bound? (path-parent (buffer-filename)) sym)))

(define (get-local-symbol sym)
   (if (top-level-bound? sym (buffer-env))
      (top-level-value sym (buffer-env))
      ;; else
      (dir-get-local-symbol sym)))

(define (set-local-symbol! sym val)
   (set-top-level-value! sym val (buffer-env)))

(define-syntax (get-local stx)
   (syntax-case stx ()
      ((_ s)
       #`(get-local-symbol 's))
      ((_ s e)
       #`(if (local-bound? s) (get-local s) e))))

(define-syntax (set-local! stx)
   (syntax-case stx ()
      ((_ s v)
       #`(set-local-symbol! 's v))))

(define-syntax (local-bound? stx)
   (syntax-case stx ()
      ((_ s)
       #`(local-symbol-bound? 's))))

(define-syntax (define-local stx)
   (syntax-case stx ()
      ((_ s v)
       #`(define-top-level-value 's v (buffer-env)))))

(define buffer-name
   (case-lambda
      [()
       (call-foreign (__cs_buf_name_get (buffer-id (current-buffer))))]

      [(b)
       (call-foreign (__cs_buf_name_get (buffer-id b)))]))

(define buffer-set-name
   (case-lambda
     [(n)
      (buffer-set-name (current-buffer) n)]

     [(b n)
      (call-foreign (__cs_buf_name_set (buffer-id b) n))
      (buffer-set-dirty b #t)
      ]))

(define buffer-set-readonly
   (case-lambda
      [(read-only?)
       (buffer-set-readonly (current-buffer) read-only?)]

      [(buf read-only?)
       ($buffer-is-readonly-set! buf read-only?)
       (buffer-set-dirty buf #t)]))

(define buffer-is-readonly?
   (case-lambda
      [()
       (buffer-is-readonly? (current-buffer))]

      [(buf)
       ($buffer-is-readonly buf)]))

(define buffer-is-modified?
   (case-lambda
      [()
       (buffer-is-modified? (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_is_modified (buffer-id buf)))]))

(define buffer-is-dirty?
   (case-lambda
      [()
       (buffer-is-dirty? (current-buffer))]

      [(buf)
       ($buffer-is-dirty buf)]))

(define buffer-set-dirty
   (case-lambda
      [(dirty?)
       (buffer-set-dirty (current-buffer) dirty?)]

      [(buf dirty?)
       ($buffer-is-dirty-set! buf dirty?)]))

(define (symbol->text-property-type s)
   (case s
      ['style:     1]
      ['highlight: 2]
      ['keymap:    3]
      ['symbol:    4]
      ['data:      5]
      ['all:       10000]
      [else         0]))

(define __add-style-property
   (case-lambda
      [(style regex)
       (__add-style-property style -1 -1 regex)]

      [(style start end)
       (__add-style-property style start end #f)]

      [(style start end regex)
       (__add-style-property style start end #f #f)]
      
      [(style start end regex name)
       (let ([l (if (symbol? style)
                    (list -1 -1 -1)
                    (style->list style))])
         (call-foreign
            (__cs_buf_prop_style_add
               (buffer-id (current-buffer))
               1
               (list-ref l 0)
               (list-ref l 1)
               (list-ref l 2)
               (list-ref l 4)
               (if (symbol? style)
                  (symbol->string style)
                  ;; else
                  #f)
               start
               end
               regex
               name
               (plist-get style 'expand:)
               (list-ref l 3))))]))

(define __add-keymap-property
   (case-lambda
      [(kmap regex)
       (__add-keymap-property kmap -1 -1 regex)]

      [(kmap start end)
       (__add-keymap-property kmap start end #f)]

      [(kmap start end regex)
       (__add-keymap-property kmap start end #f #f)]
      
      [(kmap start end regex name)
       (call-foreign (__cs_buf_prop_kmap_add (buffer-id (current-buffer)) kmap start end regex name))]))

(define __add-symbol-property
   (case-lambda
      [(symbol start end)
       (__add-symbol-property symbol start end #f)]

      [(symbol start end regex)
       (__add-symbol-property symbol start end #f #f)]
      
      [(symbol start end regex name)
       (call-foreign (__cs_buf_prop_symbol_add (buffer-id (current-buffer)) (symbol->string symbol) start end regex name))]))

(define __add-data-property
   (case-lambda
      [(data start end)
       (__add-data-property data start end #f)]

      [(data start end regex)
       (__add-data-property data start end #f #f)]
      
      [(data start end regex name)
       (call-foreign (__cs_buf_prop_data_add (buffer-id (current-buffer)) data start end regex name))]))

(define add-text-property
   (case-lambda
      [(regex plist)
       (add-text-property -1 -1 regex plist)]

      [(start end plist)
       (let ([name (plist-get plist ':name)]
             [regex (plist-get plist ':regex)])
          (plist-for-each plist
             (lambda (prop val)
                (case prop
                   ['style: (__add-style-property val start end regex name)]
                   ['keymap: (__add-keymap-property val start end regex name)]
                   ['symbol: (__add-symbol-property val start end regex name)]
                   ['data: (__add-data-property val start end regex name)])))
          (buffer-set-dirty (current-buffer) #t) )]))

(define remove-text-property
   (case-lambda
      [()
       (remove-text-property 'all: -1 -1 #f)]
    
      [(name)
       (remove-text-property 'all: -1 -1 name)]
    
      [(start end)
       (remove-text-property 'all: start end #f)]

      [(type start end)
       (remove-text-property type start end #f)]

      [(type start end name)
       (call-foreign (__cs_buf_prop_del (buffer-id (current-buffer)) (symbol->text-property-type type) start end #f name))
       (buffer-set-dirty (current-buffer) #t) ]))

(define get-text-property
   (case-lambda
      [(name)
       (get-text-property 'all: -1 -1 name)]
    
      [(start end)
       (get-text-property 'all: start end #f)]

      [(type start end)
       (get-text-property type start end #f)]

      [(type start end name)
       (call-foreign (__cs_buf_prop_get (buffer-id (current-buffer)) (symbol->text-property-type type) start end name))]))

(define set-text-property
   (case-lambda
      [(start end plist)
       (set-text-property start end #f plist)]
      
      [(start end name plist)
       (let ([type #f])
          (plist-for-each plist
             (lambda (prop val)
                (when (member prop '(style: data: symbol: keymap:))
                   (set! type prop))))
          (when type
             (remove-text-property type start end name)
             (add-text-property start end plist)))]))

(define (highlight-range s e)
   (call-foreign (__cs_buf_prop_style_add (buffer-id (current-buffer)) 2 -1 -1 -1 0 "highlight" s e #f #f #f)))

(define (highlight-clear)
   (call-foreign (__cs_buf_prop_del (buffer-id (current-buffer)) 2 -1 -1 #f #f)))

(define (buffer-get name)
   (call/cc
      (lambda (return)
         (for-each
            (lambda (b)
               (when (equal? name (buffer-name b))
                  (return b)))
            (buffer-list)))))

(define ($buffer-local-keymap)
   (call-foreign (__cs_buf_kmap_get (buffer-id (current-buffer)))))

(define (buffer-keymap)
   (keymap-parent ($buffer-local-keymap)))

(define (buffer-set-keymap sym)
   (let ([lmap ($buffer-local-keymap)])
      (keymap-set-parent lmap sym)))

(define (bind-key-local k p)
   (bind-key ($buffer-local-keymap) k p))

(define (mode-gen-map-symb m)
   (string->symbol
      (string-append (symbol->string m) "-map")))

(define (mode-gen-map-value m)
   (let ([s (mode-gen-map-symb m)])
      (if (top-level-bound? s)
         (top-level-value s)
         ;; else
         #f)))

(define (mode-gen-hook-symb m)
   (string->symbol
      (string-append (symbol->string m) "-hook")))

(define (mode-gen-hook-value h)
   (let ([s (mode-gen-hook-symb h)])
      (if (top-level-bound? s)
         (top-level-value s)
         ;; else
         #f)))

(define-syntax (define-mode stx)
   (syntax-case stx ()
      ((_ mode name parent exp ...)
       #`(define-top-level-value 'mode
            (lambda ()
               (when parent
                  ((top-level-value 'parent)))
               (define-local major-mode 'mode)
               (let ([m-map (mode-gen-map-symb 'mode)])
                  (when (top-level-bound? m-map)
                     (when parent
                        (let ([p-map (mode-gen-map-symb 'parent)])
                           (if (local-symbol-bound? p-map)
                              (keymap-set-parent (top-level-value m-map) (get-local-symbol p-map))
                              ;; else
                              (keymap-set-parent (top-level-value m-map) p-map))))
                     (buffer-set-keymap m-map))
                  (buffer-set-mode-name name)
                  exp
                  ...
                  (run-hooks
                     (mode-gen-hook-symb 'mode))))))))

(define (enable-insert e)
   ($buffer-is-input-enabled-set! (current-buffer) e))

(define (insert-enabled?)
   ($buffer-is-input-enabled (current-buffer)) )

(define (buffer-is-valid? bid)
   (call-foreign (__cs_buf_is_valid (buffer-id bid))))

(define buffer-is-visible?
   (case-lambda
      [()
       (call-foreign (__cs_buf_is_visible (buffer-id (current-buffer))))]

      [(b)
       (call-foreign (__cs_buf_is_visible (buffer-id b)))]))

(define buffer-set-vterm
   (case-lambda
      [(pid)
       (buffer-set-vterm (current-buffer) pid)]

      [(b pid)
       (call-foreign (__cs_buf_term_set (buffer-id b) pid))]))

(define buffer-is-vterm?
   (case-lambda
      [()
       (call-foreign (__cs_buf_is_term (buffer-id (current-buffer))))]

      [(b)
       (call-foreign (__cs_buf_is_term (buffer-id b)))]))

(define (buffer-set-cwd cwd)
   (define-local current-cwd cwd))

(define (buffer-cwd)
   (if (and (local-bound? current-cwd) (get-local current-cwd))
      (get-local current-cwd)
      ;; else
      (current-cwd)))

(define (buffer-set-mark buf pos)
   ($buffer-mark-set! buf pos) )

(define (buffer-mark buf)
   ($buffer-mark buf) )

(define (buffer-is-mark-set? buf)
   (if ($buffer-mark buf) #t #f) )

)
