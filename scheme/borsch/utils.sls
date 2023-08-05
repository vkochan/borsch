(library (borsch utils)
   (export
      buffer-get-or-create
      buffer-open-file)
   (import
      (chezscheme)
      (borsch base)
      (borsch file)
      (borsch lists)
      (borsch buffer)
      (borsch window)
      (pregexp) )

(define __cs_buf_file_open (foreign-procedure "cs_buf_file_open" (int string) scheme-object))

(define (buffer-get-or-create name)
   (or (buffer-get name)
       (create-buffer name)))

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
         (let* ([b (create-buffer)]
                [ok (call-foreign (__cs_buf_file_open (buffer-id b) f))])
            (window-reload-buffer b)
            (with-current-buffer b
               (buffer-set-name (buffer-filename))
               (buffer-init-file-mode))
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

)
