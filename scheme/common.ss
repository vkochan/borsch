;; FFI
(define __cs_do_quit (foreign-procedure "cs_do_quit" () void))

(define foreign-mutex (make-mutex))

(define-syntax (call-foreign stx)
   (syntax-case stx ()
      ((_ exp ...)
       ;;#`(with-mutex foreign-mutex
       #`(begin
            exp
	    ...))))

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
                  ...))))))

(define (message s)
   (let ([b (buffer-get "*Messages*")])
      (when b
         (with-current-buffer b
            (text-insert (format "~a\n" s))))
      (run-hooks 'message-hook s)))

(define (do-quit)
   (run-hooks 'exit-hook)
   (call-foreign (__cs_do_quit)))

;; Misc helpers
(define list-dir
   (case-lambda
      [(dir)
       (list-dir dir "")]
      
      [(dir opts)
       (let ([dl '()] [fl '()])
         (for-each
            (lambda (e)
               (when (not (and (equal? (string-ref e 0) #\.)
                               (not (get-local show-hidden))))
                  (if (file-directory? (fmt "~a/~a" dir e))
                     (set! dl (append dl (list (string-append e "/")))))
                  (if (file-regular? (fmt "~a/~a" dir e))
                     (set! fl (append fl (list e))))))
            (string-split (list-ref (process-get-output (format "ls -1 ~a ~a" opts dir)) 1)
                          #\newline))
         (append dl fl))]))

(define (directory-list-files* path)
   (let loop ([path path])
      (let ([ls '()])
         (for-each
            (lambda (p)
               (let ([path (string-append path "/" p)])
                  (if (file-regular? path)
                     (set! ls (append ls (list path)))
                     ;; else
                     (when (file-directory? path)
                        (set! ls (append ls (loop path)))))))
            (directory-list path))
         ls)))

(define is-dir? file-directory?)
(define is-file? file-regular?)
(define is-link? file-symbolic-link?)
(define (is-file-or-link? p)
   (or (is-link? p) (is-file? p))) 

(define (mkdir-p path)
   (system (format "mkdir -p ~a" path)))

(define (suffix+ str sfx)
   (cond
      ([list? str]
       (map
          (lambda (x)
             (string-append x sfx))
          str))
      (else (string-append str sfx))))

(define (prefix+ str pfx)
   (cond
      ([list? str]
       (map
          (lambda (x)
             (string-append pfx x))
          str))
      (else (string-append pfx str))))

(define (path+ p x)
   (suffix+ p (prefix+ x "/")))

(define (path-parent+ p x)
   (prefix+ p (suffix+ x "/")))

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
             (set! i (1+ i)))
          (substring s i n))]))

(define string-trim-right
   (case-lambda
      [(s)
       (string-trim-right s '(#\space))]

      [(s t)
       (let ([n (1- (string-length s))])
          (while (and (>= n 0)
                      (member (string-ref s n) t))
             (set! n (1- n)))
          (substring s 0 (1+ n)))]))

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
               (set! i (1+ i)))
            s)
         r)
      ;; else
      s))

(define % modulo)

(define fmt format)

(define (lines-count str)
   (let ([n (string-length str)]
         [i 0]
         [c 0])
      (do ([i i (1+ i)])
          ((>= i n ))
         (if (char=? (string-ref str i) #\newline)
            (set! c (1+ c))))
      c))

(define (any->str x)
   (call-with-string-output-port
      (lambda (p)
         (pretty-print x p))))

(define (eval->str s)
   (let ([e (eval s)])
      (if (and (not (equal? e #!eof))
               (not (equal? e (void))))
         (if (and (list? e) (not (null? e)) (list? (first e)))
            (with-output-to-string
               (lambda ()
                  (display "(\n")
                  (for-each
                     (lambda (i)
                        (display "   ")
                        (pretty-print i))
                     e)
                  (display ")\n")))
            ;; else
            (any->str e))
         ;; else
         "")))

(define (eval-port->str p)
   (eval->str (read p)))

(define sym->str symbol->string)

(define (err->str e)
   (let-values ([(op g) (open-string-output-port)])
      (display-condition e op)
      (g)))

(define-syntax try
   (syntax-rules (catch)
      ((_ body)
       (try body (catch #f)))

      ((_ body (catch catcher))
       (call-with-current-continuation
          (lambda (exit)
             (with-exception-handler
                (lambda (condition)
                   (run-hooks 'error-hook (err->str condition))
		   (when catcher
                      (catcher condition))
                   (exit condition))
                (lambda () body)))))))

(define (__do-eval-file in out)
   (let* ([ip (open-input-file in)]
          [op (open-output-file out 'truncate)]
          [out ""]
          [err? #f]
          [ret '()])
      (set! out (with-output-to-string
                   (lambda ()
                      (try
                         (set! ret (eval-port->str ip))
                         (catch
                            (lambda (ex)
                               (set! ret (err->str ex))
                               (set! err? #t))) ))))
      (put-string op out)
      (put-string op ret)
      (if err?
         (put-string op "\n"))
      (flush-output-port op)
      (close-port ip)
      (close-port op)
      (if err? 1 0)))

(define (run-hooks symb . args)
   (if (top-level-bound? symb)
      (let ([hook-list (top-level-value symb)])
         (for-each
            (lambda (h)
               (let ([fn (if (symbol? h) (eval h) h)])
                  (try (apply fn args))))
            hook-list))))

(define (add-hook h f)
   (if (not (top-level-bound? h))
      (define-top-level-value h (list)))
   (let ([h-lst (top-level-value h)])
      (if (not (member f h-lst))
         (set-top-level-value! h (append h-lst (list f))))))

(define (remove-hook h f)
   (if (top-level-bound? h)
      (let ([h-lst (top-level-value h)])
         (set-top-level-value! h (remove f h-lst)))))

(define (bit n)
   (bitwise-arithmetic-shift-left 1 n))

(define (count-digits-num n)
   (let loop ([e 0])
      (if (> (fx/ n (expt 10 e)) 0)
         (loop (1+ e))
         ;; else
         e)))

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

;; taken from https://github.com/laqrix/raytracer.git
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

(define (define*-build-args fields defaults bindings)
   (if (snull? fields)
      '()
       (let* ([f (scar fields)]
              [v (define*-find-binding f bindings)])
         (if v
            (cons v (define*-build-args (scdr fields) (scdr defaults)
                    (define*-remove-binding f bindings)))
            (cons (scar defaults)
               (define*-build-args (scdr fields) (scdr defaults)
                  bindings))))))

(define (define*-valid-bindings? fields bindings seen)
   (syntax-case bindings ()
     [((fn fv) . rest)
      (and (identifier? #'fn)
           (let ([f (datum fn)])
              (when (memq f seen)
                 (bad-syntax "duplicate field" #'x #'fn))
              (unless (memq f fields)
                 (bad-syntax "unknown field" #'x #'fn))
              (define*-valid-bindings? fields #'rest (cons f seen))))]
     [() #t]
     [_ #f]))

(define-syntax (define* x)
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
                        (bad-syntax "duplicate field" x #'fn))
                      (valid-fields? #'rest (cons f seen))))]
              [() #t]
              [_ #f])))
     #'(begin
         (define-syntax (name x)
           (syntax-case x ()
             [(name req ... . bindings)
              (define*-valid-bindings? '(field ...) #'bindings '())
              #`(defaults-proc req ... #,@(define*-build-args #'(field ...) #'(default ...) #'bindings))]))
         (define (defaults-proc req ... field ...)
           b1 b2 ...))]))
