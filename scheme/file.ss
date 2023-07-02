(define (file-open p)
   (let ([p (path-expand p)])
      (if (file-regular? p)
         (buffer-open-file p)
         ;; else
         (if (file-directory? p)
            (dirb p)
            ;; else
            (message (format "path does not exist: ~a" p))))))

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

(define (file-delete f)
   (if (file-directory? f)
      (delete-directory f)
      ;; else
      (delete-file f)))

(define* (file-list (p) ([recur? #f]))
   (cond
      (recur?               (file-list-recursive p))
      ((file-directory? p)  (directory-list p))
      ((is-file-or-link? p) (list p))
      (else '() )))

(define (file-list-recursive path)
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

(define (file-delete-recursive p)
   (letrec ([rm-recur 
               (lambda (p)
                  (let ([lst (path-parent+ (file-list p) p)])
                     (for-each
                        (lambda (f)
                           (if (is-file-or-link? f)
                              (file-delete f)
                              ;; else
                              (if (file-directory? f)
                                 (begin
                                    (rm-recur f)
                                    (file-delete f)))))
                        lst))
                     (file-delete p))])
      (rm-recur p)))

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
