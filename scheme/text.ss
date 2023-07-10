(define __cs_buf_search_regex (foreign-procedure "cs_buf_search_regex" (int int string int) scheme-object))

(define (text-copy-line)
   (copybuf-copy (text-string (text-line-begin-pos) (1+ (text-line-end-pos))) #t))

(define (text-delete-selection)
   (let ([r (text-selection-range)])
      (text-delete-range (car r) (cadr r))))

(define (text-copy-selection)
   (let ([r (text-selection-range)])
      (copybuf-copy (text-string (car r) (cadr r)))))

(define (text-append-selection)
   (let ([r (text-selection-range)])
      (copybuf-append (text-string (car r) (cadr r)))))

(define (text-copy-selection-linewise)
   (let ([r (text-selection-range)])
      (copybuf-copy (text-string (car r) (cadr r)) #t)))

(define (text-append-selection-linewise)
   (let ([r (text-selection-range)])
      (copybuf-append (text-string (car r) (cadr r)) #t)))

(define (text-reload-file)
   (with-saved-cursor
      (text-delete)
      (text-insert-file (buffer-filename))
      (buffer-save)))

(define text-eval
   (case-lambda
      [()
       (text-eval (current-buffer))]

      [(buf)
       (with-current-buffer buf
          (with-input-from-string (string-append "\'" (text-string))
             (lambda ()
                (eval (read)))))]))

(define text-search-reg "")

(define text-search-regex
   (case-lambda
      [(rx)
       (text-search-regex rx (cursor))]

      [(rx pos)
       (text-search-regex rx (cursor) +1)]

      [(rx pos dir)
       (call-foreign (__cs_buf_search_regex (current-buffer) pos rx dir))]))

(define (text-search-next)
   (text-search-regex text-search-reg (cursor) +1))

(define (text-search-prev)
   (text-search-regex text-search-reg (cursor) -1))

(define (text-search-word-direction word dir)
   (let ([pattern (format "\\<~a\\>" word)])
      (set! text-search-reg pattern)
      (text-search-regex pattern (cursor) dir)))

(define text-search-word-forward
   (case-lambda
      [()
       (text-search-word-forward (text-word))]

      [(w)
       (text-search-word-direction w +1)]))

(define text-search-word-backward
   (case-lambda
      [()
       (text-search-word-backward (text-word))]

      [(w)
       (search-word-direction w -1)]))

(define (text-search-regex-read)
   (when (not (buffer-is-vterm?))
      (minibuf-read "/"
         (lambda (r)
            (set! text-search-reg r)
            (cursor-set (text-search-regex r))))))

