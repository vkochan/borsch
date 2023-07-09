(define __cs_buf_text_obj_range (foreign-procedure "cs_buf_text_obj_range" (int int char boolean) scheme-object))
(define __cs_buf_text_range_del (foreign-procedure "cs_buf_text_range_del" (int int int) scheme-object))
(define __cs_buf_mark_set (foreign-procedure "cs_buf_mark_set" (int int) void))
(define __cs_buf_mark_get (foreign-procedure "cs_buf_mark_get" (int) scheme-object))
(define __cs_buf_mark_clear (foreign-procedure "cs_buf_mark_clear" (int) void))
(define __cs_buf_mark_is_set (foreign-procedure "cs_buf_mark_is_set" (int) scheme-object))
(define __cs_win_mark_highlight (foreign-procedure "cs_win_mark_highlight" (int boolean) void))
(define __cs_buf_text_get (foreign-procedure "cs_buf_text_get" (int int int) scheme-object))
(define __cs_buf_search_regex (foreign-procedure "cs_buf_search_regex" (int int string int) scheme-object))

(define text-string
   (case-lambda
      [()
       (call-foreign (__cs_buf_text_get (current-buffer)
                          (text-begin-pos)
                          (- (text-end-pos) (text-begin-pos))))]

      [(start end)
       (let ([start (min start end)] [end (max start end)])
          (call-foreign (__cs_buf_text_get (current-buffer) start (- end start))))]))

(define text-char
   (case-lambda
      [()
         (text-char (cursor))]

      [(s)
         (let ([str (text-string s (text-next-char-pos s))])
            (if (> (string-length str) 0)
               (string-ref str 0)
               ;; else
               #f))]))

(define (text-obj-range buf curs obj t?)
   (call-foreign (__cs_buf_text_obj_range buf curs obj t?)))

(define text-word
   (case-lambda
      [()
       (text-word (cursor))]

      [(s)
       (if (local-bound? text-word-func)
          ((get-local text-word-func))
          ;; else
          (let ([r (text-obj-range (current-buffer) s #\w #t)])
             (text-string (car r) (cdr r))))]))

(define text-longword
   (case-lambda
      [()
       (text-longword (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\W #t)])
          (text-string (car r) (cdr r)))]))

(define text-object
   (case-lambda
      [()
       (text-object (cursor))]

      [(s)
       (let ([obj (if (text-is-selection-set?)
                      (let ([sel (text-selection)])
                         (text-clear-selection)
                         sel)
                      ;else
                      (text-longword s))])
          (string-trim (text-longword s)
                       '(#\space #\< #\> #\( #\) #\[ #\] #\" #\')))]))

(define text-line
   (case-lambda
      [()
       (text-line (cursor))]

      [(s)
       (text-string (text-line-begin-pos) (text-line-end-pos))]))

(define text-line-inner
   (case-lambda
      [()
       (text-line-inner (cursor))]

      [(s)
       (text-string (text-line-start-pos) (text-line-end-pos))]))

(define text-square-brackets
   (case-lambda
      [()
       (text-square-brackets (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\[ #f)])
          (text-string (car r) (cdr r)))]))

(define text-square-brackets-inner
   (case-lambda
      [()
       (text-square-brackets-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\[ #t)])
          (text-string (car r) (cdr r)))]))

(define text-curly-brackets
   (case-lambda
      [()
       (text-curly-brackets (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\{ #f)])
          (text-string (car r) (cdr r)))]))

(define text-curly-brackets-inner
   (case-lambda
      [()
       (text-curly-brackets-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\{ #t)])
          (text-string (car r) (cdr r)))]))

(define text-angle-brackets
   (case-lambda
      [()
       (text-angle-brackets (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\< #f)])
          (text-string (car r) (cdr r)))]))

(define text-angle-brackets-inner
   (case-lambda
      [()
       (text-angle-brackets-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\< #t)])
          (text-string (car r) (cdr r)))]))

(define text-parens
   (case-lambda
      [()
       (text-parens (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\( #f)])
          (text-string (car r) (cdr r)))]))

(define text-parens-inner
   (case-lambda
      [()
       (text-parens-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\( #t)])
          (text-string (car r) (cdr r)))]))

(define text-quote
   (case-lambda
      [()
       (text-quote (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\" #f)])
          (text-string (car r) (cdr r)))]))

(define text-quote-inner
   (case-lambda
      [()
       (text-quote-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\" #t)])
          (text-string (car r) (cdr r)))]))

(define text-single-quote
   (case-lambda
      [()
       (text-single-quote (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\' #f)])
          (text-string (car r) (cdr r)))]))

(define text-single-quote-inner
   (case-lambda
      [()
       (text-single-quote-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\' #t)])
          (text-string (car r) (cdr r)))]))

(define text-back-quote
   (case-lambda
      [()
       (text-back-quote (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\` #f)])
          (text-string (car r) (cdr r)))]))

(define text-back-quote-inner
   (case-lambda
      [()
       (text-back-quote-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\` #t)])
          (text-string (car r) (cdr r)))]))

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

(define text-set-selection
   (case-lambda
      [()
       (call-foreign (__cs_buf_mark_set (current-buffer) (cursor)))]

      [(s)
       (call-foreign (__cs_buf_mark_set (current-buffer) s))]))

(define (text-get-selection)
   (call-foreign (__cs_buf_mark_get (current-buffer))))

(define (text-is-selection-set?)
   (call-foreign (__cs_buf_mark_is_set (current-buffer))))

(define (text-selection-range)
   (let ([m (text-get-selection)]
         [c (cursor)])
     (let ([s (min m c)]
           [e (max m c)])
        (list s (text-next-char-pos e)))))

(define (text-selection)
   (let ([r (text-selection-range)])
      (text-string (car r) (cadr r))))

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

(define text-highlight-selection
   (case-lambda
      [(e)
       (text-highlight-selection (__cs_win_current_get) e)]

      [(wid e)
       (when wid (call-foreign (__cs_win_mark_highlight wid e)))]))

(define (text-clear-selection)
   (when (local-bound? text-clear-selection-hook)
      ((get-local text-clear-selection-hook)))
   (call-foreign (__cs_buf_mark_clear (current-buffer))))

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

