(define __cs_buf_file_open (foreign-procedure "cs_buf_file_open" (int string) scheme-object))

(define file-match-mode (list))

(define buffer-create
   (case-lambda
      [() 
       (buffer-create "")]

      [(n) 
       (let ([b (buffer-new n)])
          (window-create b)
          b)]))

(define (buffer-open b)
   (or (buffer-is-visible? b) (window-create b)))

(define (buffer-get-or-create name)
   (or (buffer-get name)
       (buffer-create name)))

(define (buffer-open-file f)
   (let ([bid (buffer-get-by-file f)]
         [in-frame? #f]
         [win #f])
      (if bid
         (let ()
            (set! win (buffer-window bid))
            (for-all
               (lambda (w)
                  (when (equal? w win)
                     (set! in-frame? #t)
                     #f))
               (window-list))
            (if in-frame?
               (begin
                  (window-focus win))
               ;; else
               (window-create bid))
            bid)
         ;; else
         (let* ([b (buffer-create)]
                [ok (call-foreign (__cs_buf_file_open (buffer-id b) f))])
            (with-current-buffer b
               (buffer-set-name (buffer-filename))
               (text-mode)
               (when ok
                  (for-each
                     (lambda (match)
                        (let ([fname (buffer-filename)])
                           (when (pregexp-match (car match) fname)
                              ((top-level-value (cdr match))))))
                     file-match-mode)))
            b))))

(define buffer-window
   (case-lambda
      [()
       (buffer-window (current-buffer))]

      [(b)
       (let ([win-lst (filter
                         (lambda (w)
                            (equal? (window-buffer w) b))
                         (window-list))])
          (if (null? win-lst)
             #f
             ;; else
             (first win-lst)))]))

(define (buffer-run)
   (let ([fname (buffer-filename)])
      (if (file-is-executable? fname)
         (vterm (format "~a ; read" fname))
         ;; else
         (message (format "File is not executable: ~a" fname))
      )))

(define (file-open p)
   (let ([p (path-expand p)])
      (if (file-regular? p)
         (buffer-open-file p)
         ;; else
         (if (file-directory? p)
            (dirb p)
            ;; else
            (message (format "path does not exist: ~a" p))))))
