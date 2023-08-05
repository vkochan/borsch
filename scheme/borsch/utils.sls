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
   (or (get-buffer name)
       (create-buffer name)))

(define (buffer-open-file f)
   (let ([buf (get-buffer-by-file f)]
         [in-frame? #f])
      (if buf
         (let ()
            (if (buffer-is-visible? buf)
               (window-focus (buffer-window buf))
               ;; else
               (window-create buf))
            buf)
         ;; else
         (let* ([buf (create-buffer)]
                [ok (call-foreign (__cs_buf_file_open (buffer-id buf) f))])
            (window-reload-buffer buf)
            (with-current-buffer buf
               (buffer-set-name (buffer-filename))
               (buffer-init-file-mode))
            buf))))

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
