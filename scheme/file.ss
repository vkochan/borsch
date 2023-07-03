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

(define file-is-regular? file-regular?)
(define file-is-link? file-symbolic-link?)

(define (file-is-regular/link? p)
   (or (file-is-link? p) (file-is-regular? p))) 

(define (file-delete f)
   (if (file-directory? f)
      (delete-directory f)
      ;; else
      (delete-file f)))

(define (file-list p)
   (cond
      ((file-directory? p)  (directory-list p))
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
                     (when (file-directory? path)
                        (set! child-ls (loop path))) 
                     (if for-each:
                        (for-each: path)
                        ;; else
                        (set! ls (append ls (list path) child-ls)) ))))
            (cond
               ((file-directory? path) (directory-list path))
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
