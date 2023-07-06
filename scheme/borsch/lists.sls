(library (borsch lists)
   (export
      first
      second

      plist-get
      plist-put
      plist-for-each
      list-empty?

      make-stack
      stack-empty?
      stack-push!
      stack-top
      stack-list
      stack-remove!
      stack-pop!
      add-to-list)
   (import (chezscheme))

(define first car)

(define (second ls)
   (car (cdr ls)))

(define plist-get
   (case-lambda
      ((plist prop)
       (plist-get plist prop #f))

      ((plist prop def)
       (let loop ([plist plist])
          (if (< (length plist) 2)
             def
             ;; else
             (let ([name (first plist)] [val (second plist)])
                (if (equal? name prop)
                   val
                   ;; else
                   (loop (cdr (cdr plist))))))))))

(define (plist-put plist prop value)
   (let loop ([plist plist] [set? #f])
      (if (< (length plist) 2)
         (if (not set?)
            (list prop value)
            ;; else
            (list)
         )
         ;; else
         (let ([rest (cdr (cdr plist))]
               [name (first plist)]
               [val (second plist)])
            (if (equal? name prop)
               (append (list prop value) (loop rest #t))
               ;; else
               (append (list name val) (loop rest set?)))))))

(define (plist-for-each plist fn)
   (let loop ([plist plist])
      (when (>= (length plist) 2)
         (let ([name (first plist)] [val (second plist)])
            (fn name val))
         (loop (cdr (cdr plist))))))

(define (list-empty? ls)
   (= (length ls) 0))

(define (make-stack)
   (let ([st (list)])
      (lambda (msg . args)
         (cond
            [(eqv? msg 'empty?) (null? st)]
            [(eqv? msg 'push!) (set! st (cons (car args) st))]
            [(eqv? msg 'top)   (if (null? st) #f (car st))]
            [(eqv? msg 'list)   st]
            [(eqv? msg 'remove!) (set! st (remove (first args) st))]
            [(eqv? msg 'pop!) (let ([v (car st)]) (set! st (cdr st)) v)]))))

(define (stack-empty? st)
   (st 'empty?))

(define (stack-push! st val)
   (st 'push! val))

(define (stack-top st)
   (st 'top))

(define (stack-list st)
   (st 'list))

(define (stack-remove! st val)
   (st 'remove! val))

(define (stack-pop! st)
   (st 'pop!))

(define (add-to-list lst el . rest)
   (set-top-level-value! lst (append (top-level-value lst)
                                     (list el)
                                     rest)))

)
