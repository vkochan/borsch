(library (borsch keyword)
   (export define*)
   (import (chezscheme))

;; taken from https://github.com/laqrix/raytracer.git
(define-syntax (define* x)
  (define (define*-bad-syntax msg form subform)
    (raise
      (condition
         (make-message-condition msg)
         (make-syntax-violation form subform))))

  (syntax-case x ()
    [(_ (name ([field default] ...)) b1 b2 ...)
     #`(define* (name () ([field default] ...)) b1 b2 ...)]

    [(_ (name (req ...) ([field default] ...)) b1 b2 ...)
     (and (identifier? #'name)
          (let valid-fields? ([fields #'(field ...)] [seen '()])
            (syntax-case fields ()
              [(fn . rest)
               (and (identifier? #'fn)
                    (let ([f (datum fn)])
                      (when (memq f seen)
                        (define*-bad-syntax "duplicate field" x #'fn))
                      (valid-fields? #'rest (cons f seen))))]
              [() #t]
              [_ #f])))
     #'(begin
          (define-syntax (name x)
             (define (snull? x) (syntax-case x () [() #t] [_ #f]))
             (define (scar x) (syntax-case x () [(x . _) #'x]))
             (define (scdr x) (syntax-case x () [(_ . y) #'y]))

             (define (syntax-datum-eq? x y)
                (eq? (syntax->datum x) (syntax->datum y)))

             (define (bad-syntax msg form subform)
                (raise
                   (condition
                      (make-message-condition msg)
                      (make-syntax-violation form subform))))

             (define (define*-remove-binding f bindings)
                (syntax-case bindings ()
                   [((fn fv) . rest)
                    (if (syntax-datum-eq? #'fn f)
                       #'rest
                       #`((fn fv) #,@(define*-remove-binding f #'rest)))]))
           
             (define (define*-find-binding f bindings)
                (syntax-case bindings ()
                   [((fn fv) . rest)
                    (if (syntax-datum-eq? #'fn f)
                       #'fv
                       (define*-find-binding f #'rest))]
                   [() #f]))

            (define (build-args fields defaults bindings)
               (if (snull? fields)
                  '()
                   (let* ([f (scar fields)]
                          [v (define*-find-binding f bindings)])
                     (if v
                        (cons v (build-args (scdr fields) (scdr defaults)
                                (define*-remove-binding f bindings)))
                        (cons (scar defaults)
                           (build-args (scdr fields) (scdr defaults)
                              bindings))))))

            (define (valid-bindings? fields bindings seen)
               (syntax-case bindings ()
                  [((fn fv) . rest)
                   (and (identifier? #'fn)
                      (let ([f (datum fn)])
                         (when (memq f seen)
                            (bad-syntax "duplicate field" #'x #'fn))
                         (unless (memq f fields)
                            (bad-syntax "unknown field" #'x #'fn))
                         (valid-bindings? fields #'rest (cons f seen))))]
                  [() #t]
                  [_ #f]))

           (syntax-case x ()
             [(name req ... . bindings)
              (valid-bindings? '(field ...) #'bindings '())
              #`(defaults-proc req ... #,@(build-args #'(field ...) #'(default ...) #'bindings))]))
         (define (defaults-proc req ... field ...)
           b1 b2 ...))]))
)
