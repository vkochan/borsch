(library (borsch strings)
   (export
      any->string
      string-empty?
      string-split
      string-remove-nl
      string-index
      string-contains?
      string-trim-left
      string-trim-right
      string-trim
      string-pad-right
      string-join)
   (import (chezscheme))

   (define (any->string x)
      (call-with-string-output-port
         (lambda (p)
            (pretty-print x p))))


   (define-syntax (while stx)
      (syntax-case stx ()
                   ((_ condition expression ...)
                    #`(do ()
                        ((not condition))
                        expression
                        ...))))

   (define (string-empty? s)
      (and s (= (string-length s) 0)))

   (define (string-split str ch)
      (let ([len (string-length str)])
         (letrec
            ([split
                (lambda (a b)
                   (cond
                      ((>= b len)
                       (if (= a b) '() (cons (substring str a b) '())))
                      ((char=? ch (string-ref str b))
                       (if (= a b)
                          (split (+ 1 a) (+ 1 b))
                          ;; else
                          (cons (substring str a b) (split b b))))
                      (else (split a (+ 1 b)))))])
            (split 0 0))))

   (define (string-remove-nl s)
      (let ([chars (string->list s)])
         (list->string
            (filter
               (lambda (c)
                  (not (equal? c #\newline)))
               chars))))

   (define (string-index t s)
      (let* ([len (string-length s)]
             [max (- (string-length t) len)])
         (let loop ((i 0))
            (cond ((> i max) #f)
                  ((string=? s (substring t i (+ i len))) i)
                  (else (loop (+ i 1)))))))

   (define (string-contains? s c)
      (not (not (string-index s c))))

   (define string-trim-left
      (case-lambda
         [(s)
          (string-trim-left s '(#\space))]

         [(s t)
          (let ([n (string-length s)]
                [i 0])
             (while (and (< i n)
                         (member (string-ref s i) t))
                (set! i (+ 1 i)))
             (substring s i n))]))

   (define string-trim-right
      (case-lambda
         [(s)
          (string-trim-right s '(#\space))]

         [(s t)
          (let ([n (- (string-length s) 1)])
             (while (and (>= n 0)
                         (member (string-ref s n) t))
                (set! n (- n 1)))
             (substring s 0 (+ 1 n)))]))

   (define string-trim
      (case-lambda
         [(s)
          (string-trim s '(#\space))]

         [(s t)
          (string-trim-left (string-trim-right s t) t)]))

   (define (string-pad-right s k)
      (let ([slen (string-length s)])
         (if (= slen k)
            s
            ;; else
            (if (> slen k)
               (substring s 0 k)
               ;; else
               (let ([pad (make-string (- k slen) #\space)])
                  (string-append s pad))))))

   (define (string-join s j)
      (if (list? s)
         (let ([r ""] [i 0])
            (for-each
               (lambda (e)
                  (if (= i 0)
                     (set! r e)
                     ;; else
                     (set! r (string-append r j e)))
                  (set! i (+ 1 i)))
               s)
            r)
         ;; else
         s))
)
