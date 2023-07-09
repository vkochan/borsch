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

(define (path-expand f)
   (let ([root (path-first f)])
      (if (equal? root "/")
         f
         ;; else
         (if (equal? root "~")
            (format "~a/~a" (getenv "HOME") (path-rest f))
            ;; else
            (string-append (current-cwd) "/" f)))))

(define (path-exists? f)
   (file-exists? (path-expand f)))


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
            (any->string e))
         ;; else
         "")))

(define (eval-port->str p)
   (eval->str (read p)))

(define sym->str symbol->string)

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
                      (lambda (ex)
                         (set! ret (error->string ex))
                         (set! err? #t))) )))
      (put-string op out)
      (put-string op ret)
      (if err?
         (put-string op "\n"))
      (flush-output-port op)
      (close-port ip)
      (close-port op)
      (if err? 1 0)))

