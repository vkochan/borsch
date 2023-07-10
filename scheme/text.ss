(define __cs_buf_text_range_del (foreign-procedure "cs_buf_text_range_del" (int int int) scheme-object))
(define __cs_buf_search_regex (foreign-procedure "cs_buf_search_regex" (int int string int) scheme-object))

(define-syntax (text-track-deletion stx)
   (syntax-case stx ()
      ((_ exp ...)
       #`(let ([del-text ""])
            (let ([del-hook (lambda (start end)
                               (let ([text (text-string start end)])
                                  (set! del-text (string-append del-text text))))])
               (define-local text-delete-hook del-hook)
               (begin
                  exp
                  ...)
               (set-local! text-delete-hook #f)
               del-text)))))

(define (text-delete-range s e)
   (text-modify
      (let ([del-hook (get-local text-delete-hook #f)])
         (when del-hook 
            (del-hook s e)))
      (call-foreign (__cs_buf_text_range_del (current-buffer) s e))))

(define (text-replace-range s e t)
   (text-delete-range s e)
   (cursor-set s)
   (text-insert t))

(define (cursor-obj-delete fn)
   (text-modify
      (let ([end (fn)]
            [start (cursor)])
         (text-delete-range start end))))

(define (cursor-obj-delete-inclusive fn)
   (text-modify
      (let ([end (text-next-char-pos (fn))]
            [start (cursor)])
         (text-delete-range start end))))

(define (text-delete-to-next-char)
   (cursor-obj-delete cursor-to-next-char))

(define text-delete-char text-delete-to-next-char)

(define (text-delete-to-prev-char)
   (cursor-obj-delete cursor-to-prev-char))

(define (text-delete-to-next-word)
   (cursor-obj-delete cursor-to-next-word))

(define text-delete-word text-delete-to-next-word)

(define (text-delete-to-prev-word)
   (cursor-obj-delete cursor-to-prev-word))

(define (text-delete-to-word-end)
   (cursor-obj-delete-inclusive cursor-to-word-end))

(define (text-delete-to-next-longword)
   (cursor-obj-delete cursor-to-next-longword))

(define text-delete-longword text-delete-to-next-longword)

(define (text-delete-to-prev-longword)
   (cursor-obj-delete cursor-to-prev-longword))

(define (text-delete-to-longword-end)
   (cursor-obj-delete-inclusive cursor-to-longword-end))

(define (text-delete-to-next-line-begin)
   (cursor-obj-delete cursor-to-next-line))

(define (text-delete-to-prev-line-end)
   (cursor-obj-delete cursor-to-prev-line-end))

(define (text-delete-to-line-start)
   (cursor-obj-delete cursor-to-line-start))

(define (text-delete-to-line-finish)
   (cursor-obj-delete cursor-to-line-finish))

(define (text-delete-to-line-begin)
   (cursor-obj-delete cursor-to-line-begin))

(define (text-delete-to-line-end)
   (cursor-obj-delete cursor-to-line-end))

(define (text-delete-line)
   (cursor-to-line-end)
   (text-delete-to-prev-line-end)
   (if (equal? (cursor) 0)
      (text-delete-char)
      ;; else
      (if (equal? (text-end-pos) (1+ (text-line-end-pos)))
         (cursor-to-line-begin)
         ;; else
         (cursor-to-next-line))))

(define (text-delete-to-begin)
   (cursor-obj-delete cursor-to-begin))

(define (text-delete-to-end)
   (cursor-obj-delete cursor-to-end))

(define (text-delete)
   (let ([s (text-begin-pos)]
         [e (text-end-pos)])
      (remove-text-property)
      (text-delete-range s e)))

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

