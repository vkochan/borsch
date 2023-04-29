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

(define (rm f)
   (if (is-dir? f)
      (delete-directory f)
      ;; else
      (delete-file f)))

(define (path->file-list p)
   (cond
      ((is-dir? p) (directory-list p))
      ((is-file-or-link? p) (list p))
      (else '())))

(define (rm-rf p)
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
                                    (rm f)))))
                        lst))
                     (rm p))])
      (rm-recur p)))

(define (file> file str)
   (let ([p (open-output-file file 'truncate)])
      (put-string p str)
      (close-port p)))

(define (file>> file str)
   (let ([p (open-output-file file 'append)])
      (put-string p str)
      (close-port p)))
