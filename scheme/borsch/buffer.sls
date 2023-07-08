(library (borsch buffer)
   (export
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
      buffer-filename
      buffer-set-filename
      buffer-env
      define-local
      buffer-name
      buffer-set-name
      buffer-set-readonly
      buffer-is-readonly?
      buffer-is-modified?
      buffer-is-dirty?
      set-text-style
      add-text-property
      remove-text-property
      get-text-property
      set-text-property
      highlight-range
      highlight-clear
      buffer-get)
   (import (chezscheme)
           (borsch base)
           (borsch style)
           (borsch lists))

(define __cs_buf_current_get (foreign-procedure "cs_buf_current_get" () scheme-object))

(define __cs_buf_line_num (foreign-procedure "cs_buf_line_num" (int int) scheme-object))
(define __cs_buf_mode_name_set (foreign-procedure "cs_buf_mode_name_set" (int string) void))
(define __cs_buf_mode_name_get (foreign-procedure "cs_buf_mode_name_get" (int) scheme-object))
(define __cs_buf_state_name_set (foreign-procedure "cs_buf_state_name_set" (int string) void))
(define __cs_buf_state_name_get (foreign-procedure "cs_buf_state_name_get" (int) scheme-object))

(define __cs_buf_snapshot (foreign-procedure "cs_buf_snapshot" (int) void))
(define __cs_buf_undo (foreign-procedure "cs_buf_undo" (int) void))
(define __cs_buf_redo (foreign-procedure "cs_buf_redo" (int) void))
(define __cs_buf_save (foreign-procedure "cs_buf_save" (int) scheme-object))

(define __cs_buf_file_set (foreign-procedure "cs_buf_file_set" (int string) void))
(define __cs_buf_file_get (foreign-procedure "cs_buf_file_get" (int) scheme-object))

(define __cs_buf_env_get (foreign-procedure "cs_buf_env_get" (int) scheme-object))

(define __cs_buf_name_get (foreign-procedure "cs_buf_name_get" (int) scheme-object))
(define __cs_buf_name_set (foreign-procedure "cs_buf_name_set" (int string) void))
(define __cs_buf_readonly_set (foreign-procedure "cs_buf_readonly_set" (int boolean) void))
(define __cs_buf_readonly_get (foreign-procedure "cs_buf_readonly_get" (int) scheme-object))
(define __cs_buf_is_modified (foreign-procedure "cs_buf_is_modified" (int) scheme-object))
(define __cs_buf_is_dirty (foreign-procedure "cs_buf_is_dirty" (int) scheme-object))

(define __cs_buf_text_fg_set (foreign-procedure "cs_buf_text_fg_set" (int int) void))
(define __cs_buf_text_bg_set (foreign-procedure "cs_buf_text_bg_set" (int int) void))
(define __cs_buf_text_style_set (foreign-procedure "cs_buf_text_style_set" (int int) void))
(define __cs_buf_text_fg_get (foreign-procedure "cs_buf_text_fg_get" (int) scheme-object))
(define __cs_buf_text_bg_get (foreign-procedure "cs_buf_text_bg_get" (int) scheme-object))
(define __cs_buf_text_style_get (foreign-procedure "cs_buf_text_style_get" (int) scheme-object))

(define __cs_buf_prop_style_add (foreign-procedure "cs_buf_prop_style_add" (int int int int int int string int int string string boolean wchar) scheme-object))
(define __cs_buf_prop_kmap_add (foreign-procedure "cs_buf_prop_kmap_add" (int int int int string string) scheme-object))
(define __cs_buf_prop_symbol_add (foreign-procedure "cs_buf_prop_symbol_add" (int string int int string string) scheme-object))
(define __cs_buf_prop_data_add (foreign-procedure "cs_buf_prop_data_add" (int scheme-object int int string string) scheme-object))
(define __cs_buf_prop_del (foreign-procedure "cs_buf_prop_del" (int int int int string string) void))
(define __cs_buf_prop_get (foreign-procedure "cs_buf_prop_get" (int int int int string) scheme-object))

(define __cs_buf_by_name (foreign-procedure "cs_buf_by_name" (string) scheme-object))

(define current-buffer-tmp (make-parameter #f))

(define-syntax (with-current-buffer stx)
   (syntax-case stx ()
      ((_ buf exp ...)
       #`(parameterize ([current-buffer-tmp buf])
            exp ... ))))

(define (current-buffer)
   (or (current-buffer-tmp)
       (call-foreign (__cs_buf_current_get))))
       
(define (buffer-line-num pos)
   (call-foreign (__cs_buf_line_num (current-buffer) pos)))

(define (buffer-set-mode-name n)
   (call-foreign (__cs_buf_mode_name_set (current-buffer) (format "~a" n))))

(define buffer-mode-name
   (case-lambda
      [()
       (buffer-mode-name (current-buffer))]

      [(b)
       (call-foreign (__cs_buf_mode_name_get b))]))

(define (buffer-set-state-name n)
   (call-foreign (__cs_buf_state_name_set (current-buffer) (format "~a" n))))

(define buffer-state-name
   (case-lambda
      [()
       (buffer-state-name (current-buffer))]

      [(b)
       (call-foreign (__cs_buf_state_name_get b))]))

(define (buffer-snapshot)
   (call-foreign (__cs_buf_snapshot (current-buffer))))

(define (buffer-undo)
   (call-foreign (__cs_buf_undo (current-buffer))))

(define (buffer-redo)
   (call-foreign (__cs_buf_redo (current-buffer))))

(define (buffer-save)
   (call-foreign (__cs_buf_save (current-buffer))))

(define buffer-filename
   (case-lambda
     [()
      (buffer-filename (current-buffer))]

     [(b)
      (call-foreign (__cs_buf_file_get b))]))

(define (buffer-set-filename f)
   (let ([first-path (path-first f)]
         [path f])
      (if (equal? first-path "~")
         (set! path
            (format "~a/~a"
               (getenv "HOME")
               (path-rest f))))
      (call-foreign (__cs_buf_file_set (current-buffer) path))))

(define (buffer-env)
   (call-foreign (__cs_buf_env_get (current-buffer))))

(define-syntax (define-local stx)
   (syntax-case stx ()
      ((_ s v)
       #`(define-top-level-value 's v (buffer-env)))))

(define buffer-name
   (case-lambda
      [()
       (call-foreign (__cs_buf_name_get (current-buffer)))]

      [(b)
       (call-foreign (__cs_buf_name_get b))]))

(define buffer-set-name
   (case-lambda
     [(n)
      (buffer-set-name (current-buffer) n)]

     [(b n)
      (call-foreign (__cs_buf_name_set b n))]))

(define buffer-set-readonly
   (case-lambda
      [(read-only?)
       (buffer-set-readonly (current-buffer) read-only?)]

      [(buf read-only?)
       (call-foreign (__cs_buf_readonly_set buf read-only?))]))

(define buffer-is-readonly?
   (case-lambda
      [()
       (buffer-is-readonly? (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_readonly_get buf))]))

(define buffer-is-modified?
   (case-lambda
      [()
       (buffer-is-modified? (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_is_modified buf))]))

(define buffer-is-dirty?
   (case-lambda
      [()
       (buffer-is-dirty? (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_is_dirty buf))]))

(define (set-text-style s . e)
   (let ([a (append (list s) e)])
      (for-each
         (lambda (o)
            (cond
               [(equal? (car o) 'fg:)
                (call-foreign (__cs_buf_text_fg_set (current-buffer) (color-name->number (cadr o))))]
               [(equal? (car o) 'bg:)
                (call-foreign (__cs_buf_text_bg_set (current-buffer) (color-name->number (cadr o))))]
               [(equal? (car o) 'attr:)
                (call-foreign (__cs_buf_text_style_set (current-buffer) (style-name->number (cadr o))))]))
         a)))

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
               (current-buffer)
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
       (call-foreign (__cs_buf_prop_kmap_add (current-buffer) kmap start end regex name))]))

(define __add-symbol-property
   (case-lambda
      [(symbol start end)
       (__add-symbol-property symbol start end #f)]

      [(symbol start end regex)
       (__add-symbol-property symbol start end #f #f)]
      
      [(symbol start end regex name)
       (call-foreign (__cs_buf_prop_symbol_add (current-buffer) (symbol->string symbol) start end regex name))]))

(define __add-data-property
   (case-lambda
      [(data start end)
       (__add-data-property data start end #f)]

      [(data start end regex)
       (__add-data-property data start end #f #f)]
      
      [(data start end regex name)
       (call-foreign (__cs_buf_prop_data_add (current-buffer) data start end regex name))]))

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
                   ['data: (__add-data-property val start end regex name)]))))]))

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
       (call-foreign (__cs_buf_prop_del (current-buffer) (symbol->text-property-type type) start end #f name))]))

(define get-text-property
   (case-lambda
      [(name)
       (get-text-property 'all: -1 -1 name)]
    
      [(start end)
       (get-text-property 'all: start end #f)]

      [(type start end)
       (get-text-property type start end #f)]

      [(type start end name)
       (call-foreign (__cs_buf_prop_get (current-buffer) (symbol->text-property-type type) start end name))]))

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
   (call-foreign (__cs_buf_prop_style_add (current-buffer) 2 -1 -1 -1 0 "highlight" s e #f #f #f)))

(define (highlight-clear)
   (call-foreign (__cs_buf_prop_del (current-buffer) 2 -1 -1 #f #f)))

(define (buffer-get n)
   (call-foreign (__cs_buf_by_name n)))

)
