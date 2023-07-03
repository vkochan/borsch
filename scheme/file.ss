(define (path-join p0 p1)
   (define (has-delim?)
      (not (or (equal? p0 "")
               (equal? p1 "")
               (equal? (string-ref p0 (- (string-length p0) 1)) #\/)
               (equal? (string-ref p1 0) #\/) )))

   (string-append p0 (if (has-delim?) "/" "") p1) )

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

(define file-is-directory? file-directory?)
(define file-is-regular? file-regular?)
(define file-is-link? file-symbolic-link?)

(define (file-is-regular/link? p)
   (or (file-is-link? p) (file-is-regular? p))) 

(define (file-delete f)
   (if (file-is-directory? f)
      (delete-directory f)
      ;; else
      (delete-file f)))

(define* (file-copy (from to) ([recur?: #f]))
   (when (equal? from to)
      (error 'file-copy "Files are the same" from))
   (when (not (file-exists? from))
      (error 'file-copy "Source file does not exist" from) )
   (when (and (not (file-exists? to))
              (not (file-exists? (path-parent to))) )
      (error 'file-copy "Destination file folder does not exist" to))
   (system (format "cp ~a ~a ~a" (if recur?: "-r" "") from to)) )

(define* (file-mkdir (path) ([recur?: #f][mode: #f]))
   (define (mkdir-try path)
      (if (not (file-exists? path))
         (mkdir path) ))

   (if (not recur?:)
      (mkdir path)
      ;; else
      (let loop ([parent (path-first path)]
                 [rest   (string-trim-right path '(#\/)) ])
         (if (equal? (path-first rest) "")
            (mkdir-try (path-join parent rest))
            ;; else
            (let ([rest (path-rest rest)])
               (when (not (equal? parent "/"))
                  (mkdir-try parent))
               (loop (path-join parent (path-first rest))
                     rest) )))))

(define (file-list p)
   (cond
      ((file-is-directory? p)  (directory-list p))
      ((file-is-regular/link? p) (list p))
      (else '() )))

(define* (file-find (path) ([for-each: #f] [match: #f]))
   (define px (if match: (pregexp match:) #f))

   (let loop ([path path])
      (let ([ls '()])
         (for-each
            (lambda (p)
               (let ([path (string-append path "/" p)]
                     [child-ls '()])
                  (when (or (not match:)
                            (and (file-is-regular/link? path)
                                 (pregexp-match px path)))
                     (when (file-is-directory? path)
                        (set! child-ls (loop path))) 
                     (if for-each:
                        (for-each: path)
                        ;; else
                        (set! ls (append ls (list path) child-ls)) ))))
            (cond
               ((file-is-directory? path) (directory-list path))
               (else (list path)) ))
         (when for-each:
            (for-each: path))
         ls)))

(define (file-delete-recursive p)
   (file-find p [for-each: (lambda (f)
                              (file-delete f) )]))

(define (file> file str)
   (let ([p (open-output-file file 'truncate)])
      (put-string p str)
      (close-port p)))

(define (file>> file str)
   (let ([p (open-output-file file 'append)])
      (put-string p str)
      (close-port p)))

(define (file-is-executable? path)
   (= 1 (fxbit-field (get-mode path) 6 7)))
