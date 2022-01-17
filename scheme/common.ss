;; Misc helpers
(define file>
   (lambda (file str)
      (let ([p (open-output-file file 'truncate)])
          (put-string p str)
	  (close-port p)
      )
   )
)

(define file>>
   (lambda (file str)
      (let ([p (open-output-file file 'append)])
          (put-string p str)
	  (close-port p)
      )
   )
)

(define swap
   (lambda (x y)
      (let ([t x])
	 (set! x y)
	 (set! y t)
      )
   )
)

(define first car)

(define second
   (lambda (ls)
      (car (cdr ls))
   )
)

(define empty?
   (lambda (ls)
      (> (length ls) 0)
   )
)

(define (string-split str ch)
   (let ([len (string-length str)])
      (letrec
         ([split
	     (lambda (a b)
	        (cond
	           ((>= b len)
                    (if (= a b) '() (cons (substring str a b) '()))
                   )

	           ((char=? ch (string-ref str b))
                    (if (= a b)
                       (split (+ 1 a) (+ 1 b))
                       ;; else
                       (cons (substring str a b) (split b b))
                    )
                   )

                   (else (split a (+ 1 b)))
                )
              )
          ])
         (split 0 0)
      )
   )
)

(define % modulo)

(define fmt format)

(define lines-count
   (lambda (str)
      (let ([n (string-length str)]
            [i 0]
            [c 0])
         (do ([i i (1+ i)])
            ((>= i n ))

            (if (char=? (string-ref str i) #\newline)
               (set! c (1+ c))
            )
         )
         c
      )
   )
)

(define any->str
   (lambda (x)
      (call-with-string-output-port
         (lambda (p)
            (pretty-print x p)
         )
      )
   )
)

(define eval->str
   (lambda (s)
      (let ([e (eval s)])
         (if (and (not (equal? e #!eof))
                  (not (equal? e (void)))
		  )
            (if (and (list? e) (not (null? e)) (list? (first e)))
               (with-output-to-string
                  (lambda ()
                     (display "(\n")
                     (for-each
                        (lambda (i)
                           (display "   ")
                           (pretty-print i)
                        )
                        e
                     )
                     (display ")\n")
		  )
               )
               ;; else
               (any->str e)
            )
            ;; else
            ""
         )
      )
   )
)

(define eval-port->str
   (lambda (p)
      (eval->str (read p))
   )
)

(define sym->str symbol->string)

(define-syntax (while stx)
  (syntax-case stx ()
	       ((_ condition expression ...)
		#`(do ()
		    ((not condition))
		    expression
		    ...))))

(define err->str
   (lambda (e)
      (let-values ([(op g) (open-string-output-port)])
         (display-condition e op)
         (g)
      )
   )
)

(define (try fn . args)
   (let ([fn fn]
         [args args])
      (let (
            [ret (append '()
                    (call/cc
                       (lambda (k)
                          (with-exception-handler
                             (lambda (x)
                                (k (list 1 (err->str x)))
                             )

                             (lambda ()
	                        (k (list 0 (apply fn args)))
                             )
                          )
                       )
                    )
                 )
             ]
           )
         (when (= (car ret) 1)
            (run-hooks 'on-error-hook (cadr ret))
         )
         ret
      )
   )
)

(define __do-eval-file
   (lambda (in out)
      (let* ([ip (open-input-file in)]
            [op (open-output-file out 'truncate)]
            [out ""]
            [ret '()])

         (set! out (with-output-to-string
                      (lambda ()
                         (set! ret (try eval-port->str ip))
                      )
                   )
         )

	 (put-string op out)

         (put-string op (second ret))

         (if (not (= (first ret) 0))
	    (put-string op "\n")
         )

	 (flush-output-port op)
	 (close-port ip)
	 (close-port op)

	 (first ret)
      )
   )
)

(define run-hooks
   (lambda (symb . args)
      (if (top-level-bound? symb)
         (let ([hook-list (top-level-value symb)])
            (for-each
               (lambda (h)
                  (apply try (append (list h) args))
               ) hook-list
            )
         )
      )
   )
)

(define add-hook
   (lambda (h f)
      (if (not (top-level-bound? h))
         (define-top-level-value h (list))
      )
      (let ([h-lst (top-level-value h)])
         (if (not (member f h-lst))
            (set-top-level-value! h (append h-lst (list f)))
         )
      )
   )
)

(define remove-hook
   (lambda (h f)
      (if (top-level-bound? h)
         (let ([h-lst (top-level-value h)])
            (set-top-level-value! h (remove f h-lst))
         )
      )
   )
)

(define bit
   (lambda (n)
     (bitwise-arithmetic-shift-left 1 n)
   )
)

(define count-digits-num
   (lambda (n)
      (let loop ([e 0])
         (if (> (fx/ n (expt 10 e)) 0)
            (loop (1+ e))
            ;; else
            e
         )
      )
   )
)
