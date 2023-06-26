(define (key-cb p)
   (let ([code (foreign-callable (lambda () (try (p) )) () void)])
      (lock-object code)
      (foreign-callable-entry-point code) ))

(define __cs_kmap_add (foreign-procedure "cs_kmap_add" (string) scheme-object))
(define __cs_kmap_parent_set (foreign-procedure "cs_kmap_parent_set" (int string int) void))
(define __cs_kmap_parent_get (foreign-procedure "cs_kmap_parent_get" (int) scheme-object))
(define __cs_kmap_del (foreign-procedure "cs_kmap_del" (int) void))

(define __cs_bind_key (foreign-procedure "cs_bind_key" (string void* int string) int))
(define __cs_unbind_key (foreign-procedure "cs_unbind_key" (string int) int))

(define global-keymap 1)

(define (%keymap-get m)
   (if (and (symbol? m) (current-buffer) (local-symbol-bound? m))
      (get-local-symbol m)
      ;; else
      m ))

(define bind-key
   (case-lambda
      [(k p)
       (bind-key global-keymap k p)]

      [(m k t)
       (let ([map (%keymap-get m)])
          (if (procedure? t)
             (__cs_bind_key k (key-cb t) map "")
             ;; else
             (__cs_bind_key k 0 map (symbol->string t)) ))] ))

(define (bind-key-local k p)
   (bind-key (%buffer-local-keymap) k p))

(define unbind-key
   (case-lambda
      [(k)
       (__cs_unbind_key k 0)]

      [(m k)
       (let ([map (%keymap-get m)])
          (__cs_unbind_key k map) )]))

(define (make-empty-keymap)
   (call-foreign (__cs_kmap_add "")) )

(define make-keymap
   (case-lambda
      [()
       (call-foreign (__cs_kmap_add (symbol->string 'global-keymap)))]

      [(p)
       (call-foreign (__cs_kmap_add (symbol->string p))) ]))

(define (keymap-parent k)
   (call-foreign (__cs_kmap_parent_get k)) )

(define (keymap-set-parent k p)
   (if (symbol? p)
      (call-foreign (__cs_kmap_parent_set k (symbol->string p) -1))
      ;; else
      (call-foreign (__cs_kmap_parent_set k "" p)) ))

(define (keymap-del k)
   (call-foreign (__cs_kmap_del k)) )
