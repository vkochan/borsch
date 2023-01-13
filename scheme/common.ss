;; FFI
(define __cs_do_quit (foreign-procedure "cs_do_quit" () void))

(define foreign-mutex (make-mutex))

(define-syntax (call-foreign stx)
   (syntax-case stx ()
      ((_ exp ...)
       ;;#`(with-mutex foreign-mutex
       #`(begin
            exp
	    ...
         )
      )
   )
)

(define-syntax (while stx)
  (syntax-case stx ()
	       ((_ condition expression ...)
		#`(do ()
		    ((not condition))
		    expression
		    ...))))

(define-syntax (with-current-buffer stx)
   (syntax-case stx ()
      ((_ buf exp ...)
       #`(let ([b buf])
            (fluid-let ([current-buffer (lambda () b)])
               (begin
                  exp
                  ...
               )
            )
         )
      )
   )
)

(define message
   (lambda (s)
      (let ([b (buffer-get "*Messages*")])
         (when b
            (with-current-buffer b
               (text-insert (format "~a\n" s))
            )
         )
         (run-hooks 'message-hook s)
      )
   )
)

(define do-quit
   (lambda ()
      (run-hooks 'exit-hook)
      (call-foreign (__cs_do_quit))
   )
)

;; Misc helpers
(define list-dir
   (case-lambda
      [(dir)
       (list-dir dir "")
      ]
      
      [(dir opts)
       (let (
             [dl '()]
             [fl '()]
            )
         (for-each
            (lambda (e)
               (when (not (and (equal? (string-ref e 0) #\.)
                               (not (get-local show-hidden))))
                  (if (file-directory? (fmt "~a/~a" dir e))
                     (set! dl (append dl (list (string-append e "/"))))
                  )
                  (if (file-regular? (fmt "~a/~a" dir e))
                     (set! fl (append fl (list e)))
                  )
              )
            )
            (string-split
               (list-ref (process-get-output (format "ls -1 ~a ~a" opts dir)) 1)
               #\newline
            )
         )
         (append dl fl)
       )
      ]
   )
)

(define directory-list-files*
   (lambda (path)
      (let loop ([path path])
         (let (
               [ls '()]
              )
            (for-each
               (lambda (p)
                  (let ([path (string-append path "/" p)])
                     (if (file-regular? path)
                        (set! ls (append ls (list path)))
                        ;; else
                        (when (file-directory? path)
                           (set! ls (append ls (loop path)))
                        )
                     )
                  )
               )
               (directory-list path)
            )
            ls
         )
      )
   )
)

(define is-dir? file-directory?)
(define is-file? file-regular?)
(define is-link? file-symbolic-link?)
(define (is-file-or-link? p) (or (is-link? p) (is-file? p))) 

(define mkdir-p
   (lambda (path)
      (system (format "mkdir -p ~a" path))
   )
)

(define suffix+
   (lambda (str sfx)
      (cond
         ([list? str]
          (map
             (lambda (x)
	        (string-append x sfx)
             ) str
          )
         )
         (else (string-append str sfx))
      )
   )
)

(define prefix+
   (lambda (str pfx)
      (cond
         ([list? str]
          (map
             (lambda (x)
	        (string-append pfx x)
             ) str
          )
         )
         (else (string-append pfx str))
      )
   )
)

(define path+
   (lambda (p x)
      (suffix+ p (prefix+ x "/"))
   )
)

(define path-parent+
   (lambda (p x)
      (prefix+ p (suffix+ x "/"))
   )
)

(define path-expand
   (lambda (f)
      (let ([root (path-first f)])
         (if (equal? root "/")
            f
            ;; else
            (if (equal? root "~")
               (format "~a/~a" (getenv "HOME") (path-rest f))
               ;; else
               (string-append (current-cwd) "/" f)
            )
         )
      )
   )
)

(define path-exists?
   (lambda (f)
      (file-exists? (path-expand f))
   )
)

(define rm
   (lambda (f)
      (if (is-dir? f)
         (delete-directory f)
         ;; else
         (delete-file f)
      )
   )
)

(define path->file-list
   (lambda (p)
      (cond
         ((is-dir? p) (directory-list p))
         ((is-file-or-link? p) (list p))
         (else '())
      )
   )
)

(define rm-rf
   (lambda (p)
      (letrec ([rm-recur 
	       (lambda (p)
                  (let ([lst (path-parent+ (path->file-list p) p)])
		     (for-each
                        (lambda (f)
                           (if (is-file-or-link? f)
                              (rm f)
                              ;; else
                              (if (is-dir? f)
                                 (begin
                                    (rm-recur f)
                                    (rm f)
                                 )
                              )
                           )
                         ) lst
                      )
		   )
		   (rm p)
		 )
	       ])
          (rm-recur p)
       )
    )
)

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

(define first car)

(define second
   (lambda (ls)
      (car (cdr ls))
   )
)

(define plist-get
   (lambda (plist prop)
      (let loop ([plist plist])
         (if (< (length plist) 2)
            #f
            ;; else
            (let ([name (first plist)] [val (second plist)])
               (if (equal? name prop)
                  val
                  ;; else
                  (loop (cdr (cdr plist)))
               )
            )
         )
      )
   )
)

(define plist-put
   (lambda (plist prop value)
      (let loop ([plist plist] [set? #f])
         (if (< (length plist) 2)
            (if (not set?)
               (list prop value)
               ;; else
               (list)
            )
            ;; else
            (let (
                  [rest (cdr (cdr plist))]
                  [name (first plist)]
                  [val (second plist)]
                 )
               (if (equal? name prop)
                  (append (list prop value) (loop rest #t))
                  ;; else
                  (append (list name val) (loop rest set?))
               )
            )
         )
      )
   )
)

(define plist-for-each
   (lambda (plist fn)
      (let loop ([plist plist])
         (when (>= (length plist) 2)
            (let ([name (first plist)] [val (second plist)])
               (fn name val)
            )
            (loop (cdr (cdr plist)))
         )
      )
   )
)

(define list-empty?
   (lambda (ls)
      (= (length ls) 0)
   )
)

(define string-empty?
   (lambda (s)
      (and s (= (string-length s) 0))
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

(define string-remove-nl
   (lambda (s)
      (let (
            [chars (string->list s)]
           )
         (list->string
            (filter
               (lambda (c)
                  (not (equal? c #\newline))
               ) chars
            )
         )
      )
   )
)

(define (string-index t s)
   (let* (
          [len (string-length s)]
	  [max (- (string-length t) len)]
         )
      (let loop ((i 0))
         (cond ((> i max) #f)
	       ((string=? s (substring t i (+ i len))) i)
	       (else (loop (+ i 1)))
         )
      )
   )
)

(define string-contains?
   (lambda (s c)
      (not (not (string-index s c)))
   )
)

(define string-trim-left
   (case-lambda
      [(s)
       (string-trim-left s '(#\space))
      ]

      [(s t)
       (let ([n (string-length s)]
             [i 0]
            )
          (while (and (< i n) (member (string-ref s i) t))
             (set! i (1+ i))
          )
          (substring s i n)
       )
      ]
   )
)

(define string-trim-right
   (case-lambda
      [(s)
       (string-trim-right s '(#\space))
      ]

      [(s t)
       (let ([n (1- (string-length s))])
          (while (and (>= n 0) (member (string-ref s n) t))
             (set! n (1- n))
          )
          (substring s 0 (1+ n))
       )
      ]
   )
)

(define string-trim
   (case-lambda
      [(s)
       (string-trim s '(#\space))
      ]

      [(s t)
       (string-trim-left (string-trim-right s t) t)
      ]
   )
)

(define string-pad-right
   (lambda (s k)
      (let ([slen (string-length s)])
         (if (= slen k)
            s
            ;; else
            (if (> slen k)
               (substring s 0 k)
               ;; else
               (let ([pad (make-string (- k slen) #\space)])
                  (string-append s pad)
               )
            )
         )
      )
   )
)

(define string-join
   (lambda (s j)
      (if (list? s)
         (let ([r ""] [i 0])
            (for-each
               (lambda (e)
                  (if (= i 0)
                     (set! r e)
                     ;; else
                     (set! r (string-append r j e))
                  )
                  (set! i (1+ i))
               )
               s
            )
            r
         )
         ;; else
         s
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
            (run-hooks 'error-hook (cadr ret))
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
                  (let ([fn (if (symbol? h) (eval h) h)])
                     (apply try (append (list fn) args))
                  )
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

(define make-stack
   (lambda ()
      (let ([st (list)])
         (lambda (msg . args)
            (cond
               [(eqv? msg 'empty?) (null? st)]
               [(eqv? msg 'push!) (set! st (cons (car args) st))]
               [(eqv? msg 'top)   (car st)]
               [(eqv? msg 'pop!) (let ([v (car st)]) (set! st (cdr st)) v)]
            )
         )
      )
   )
)

(define stack-empty?
   (lambda (st)
      (st 'empty?)
   )
)

(define stack-push!
   (lambda (st val)
      (st 'push! val)
   )
)

(define stack-top
   (lambda (st)
      (st 'top)
   )
)

(define stack-pop!
   (lambda (st)
      (st 'pop!)
   )
)

(define add-to-list
    (lambda (lst el . rest)
        (set-top-level-value! lst (append (top-level-value lst) (list el) rest))
    )
)
