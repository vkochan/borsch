(define __cs_buf_new (foreign-procedure "cs_buf_new" (string) scheme-object))
(define __cs_buf_ref_get (foreign-procedure "cs_buf_ref_get" (int) scheme-object))
(define __cs_buf_ref_put (foreign-procedure "cs_buf_ref_put" (int) scheme-object))
(define __cs_buf_ref (foreign-procedure "cs_buf_ref" (int) scheme-object))
(define __cs_buf_del (foreign-procedure "cs_buf_del" (int) void))

(define __cs_buf_file_open (foreign-procedure "cs_buf_file_open" (int string) scheme-object))

(define %buffer-list% (list))

(define file-match-mode (list))

(define (buffer-insert b)
   (set! %buffer-list% (append %buffer-list% (list b)))
   (frame-insert-buffer b))

(define (buffer-remove b)
   (set! %buffer-list% (remove b %buffer-list%))
   (frame-remove-buffer b))

(define buffer-ref-count
   (case-lambda
      [()
       (buffer-ref-count (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_ref buf))]))

(define buffer-ref-get
   (case-lambda
      [()
       (buffer-ref-get (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_ref_get buf))]))

(define buffer-ref-put
   (case-lambda
      [()
       (buffer-ref-put (current-buffer))]

      [(buf)
       (when (= 1 (buffer-ref-count buf))
          (buffer-remove buf))
       (call-foreign (__cs_buf_ref_put buf))]))

(define buffer-new
   (case-lambda
      [() 
       (buffer-new "")]

      [(n) 
       (let ([b (call-foreign (__cs_buf_new n))])
          (buffer-insert b)
          (with-current-buffer b
             (set-text-style '(fg: "white")))
          b)]))

(define buffer-create
   (case-lambda
      [() 
       (buffer-create "")]

      [(n) 
       (let ([b (buffer-new n)])
          (buffer-set-name b n)
          (window-create b)
          b)]))

(define (buffer-open b)
   (or (buffer-is-visible? b) (window-create b)))

(define buffer-create-text
   (case-lambda
      [() 
       (let ([b (buffer-create)])
          (text-mode)
          b)]

      [(n) 
       (let ([b (buffer-create n)])
          (text-mode)
          b)]))

(define buffer-delete
   (case-lambda
      [()
       (buffer-delete (current-buffer))]

      [(b)
       (call-foreign (__cs_buf_del b))
       (buffer-remove b)]))

(define (buffer-get-or-create name)
   (or (buffer-get name)
       (buffer-create name)))

(define (buffer-open-file f)
   (let ([bid (buffer-get-by-file f)]
         [in-frame? #f]
         [wid 0])
      (if bid
         (let ()
            (set! wid (buffer-window bid))
            (for-all
               (lambda (w)
                  (when (equal? w wid)
                     (set! in-frame? #t)
                     #f))
               (window-list))
            (if in-frame?
               (begin
                  (window-focus wid))
               ;; else
               (window-create bid))
            bid)
         ;; else
         (let* ([b (buffer-create)]
                [ok (call-foreign (__cs_buf_file_open b f))])
            (with-current-buffer b
               (text-mode)
               (when ok
                  (for-each
                     (lambda (match)
                        (let ([fname (buffer-filename)])
                           (when (pregexp-match (car match) fname)
                              ((top-level-value (cdr match))))))
                     file-match-mode)))
            b))))

(define (buffer-list)
   %buffer-list%)

(define (buffer-for-each fn)
   (for-each
      (lambda (b)
         (fn b))
      (buffer-list)))

(define (buffer-find fn)
   (let ([b (find
               (lambda (b)
                  (fn b))
               (buffer-list))])
      b))

(define (buffer-get-by-file file)
   (buffer-find
      (lambda (b)
         (equal? file (buffer-filename b)))))

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
