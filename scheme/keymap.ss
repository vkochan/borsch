(define key-cb
   (lambda (p)
      (let ([code (foreign-callable (lambda () (try p)) () void)])
	 (lock-object code)
	 (foreign-callable-entry-point code)
      )
   )
)

(define global-keymap 1)

(define %keymap-get
   (lambda (m)
      (if (and (symbol? m) (current-buffer) (local-symbol-bound? m))
         (get-local-symbol m)
         ;; else
         m
      )
   )
)

(define bind-key
   (case-lambda
      [(k p)
       (bind-key 0 k p)]

      [(m k t)
       (let ([map (%keymap-get m)])
          (if (procedure? t)
             (__cs_bind_key k (key-cb t) map "")
             ;; else
             (__cs_bind_key k 0 map (symbol->string t))
          )
       )
      ]
   )
)

(define bind-key-local
   (lambda (k p)
      (bind-key (%buffer-local-keymap) k p)
   )
)

(define unbind-key
   (case-lambda
      [(k)
       (__cs_unbind_key k 0)]

      [(m k)
       (let ([map (%keymap-get m)])
          (__cs_unbind_key k map)
       )
      ]
   )
)

(define make-empty-keymap
   (lambda ()
     (call-foreign (__cs_kmap_add ""))
   )
)

(define make-keymap
   (case-lambda
      [()
       (call-foreign (__cs_kmap_add (symbol->string 'global-keymap)))]

      [(p)
       (call-foreign (__cs_kmap_add (symbol->string p)))]
   )
)

(define keymap-parent
   (lambda (k)
       (call-foreign (__cs_kmap_parent_get k))
   )
)

(define keymap-set-parent
   (lambda (k p)
       (if (symbol? p)
          (call-foreign (__cs_kmap_parent_set k (symbol->string p) -1))
          ;; else
          (call-foreign (__cs_kmap_parent_set k "" p))
       )
   )
)

(define keymap-del
   (lambda (k)
       (call-foreign (__cs_kmap_del k))
   )
)
