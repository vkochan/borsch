(define key-cb
   (lambda (p)
      (let ([code (foreign-callable (lambda () (try p)) () void)])
	 (lock-object code)
	 (foreign-callable-entry-point code)
      )
   )
)

(define __cs_kmap_add (foreign-procedure "cs_kmap_add" (string) scheme-object))
(define __cs_kmap_parent_set (foreign-procedure "cs_kmap_parent_set" (int string int) void))
(define __cs_kmap_parent_get (foreign-procedure __collect_safe "cs_kmap_parent_set" (int) scheme-object))
(define __cs_kmap_del (foreign-procedure __collect_safe "cs_kmap_del" (int) void))

(define __cs_bind_key (foreign-procedure "cs_bind_key" (string void* int string) int))
(define __cs_unbind_key (foreign-procedure "cs_unbind_key" (string int) int))

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

(define make-empty-keymap
   (lambda ()
     (__cs_kmap_add "")
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

(define keymap-parent
   (lambda (k)
       (__cs_kmap_parent_get k)
   )
)

(define keymap-set-parent
   (lambda (k p)
       (if (symbol? p)
          (__cs_kmap_parent_set k (symbol->string p) -1)
          ;; else
          (__cs_kmap_parent_set k "" p)
       )
   )
)

(define keymap-del
   (lambda (k)
       (__cs_kmap_del k)
   )
)
