(define __cs_buf_text_insert (foreign-procedure "cs_buf_text_insert" (int string) scheme-object))
(define __cs_buf_text_insert_char (foreign-procedure "cs_buf_text_insert_char" (int char) scheme-object))
(define __cs_buf_text_insert_nl (foreign-procedure "cs_buf_text_insert_nl" (int int) scheme-object))
(define __cs_buf_text_insert_file (foreign-procedure "cs_buf_text_insert_file" (int string) scheme-object))
(define __cs_buf_text_obj_pos (foreign-procedure "cs_buf_text_obj_pos" (int int char int) scheme-object))
(define __cs_buf_text_obj_range (foreign-procedure "cs_buf_text_obj_range" (int int char boolean) scheme-object))
(define __cs_buf_text_range_del (foreign-procedure "cs_buf_text_range_del" (int int int) scheme-object))
(define __cs_buf_mark_set (foreign-procedure "cs_buf_mark_set" (int int) void))
(define __cs_buf_mark_get (foreign-procedure "cs_buf_mark_get" (int) scheme-object))
(define __cs_buf_mark_clear (foreign-procedure "cs_buf_mark_clear" (int) void))
(define __cs_buf_mark_is_set (foreign-procedure "cs_buf_mark_is_set" (int) scheme-object))
(define __cs_win_mark_highlight (foreign-procedure __collect_safe "cs_win_mark_highlight" (int boolean) void))
(define __cs_buf_text_get (foreign-procedure "cs_buf_text_get" (int int int) scheme-object))
(define __cs_buf_search_regex (foreign-procedure "cs_buf_search_regex" (int int string int) scheme-object))
(define __cs_buf_cursor_get (foreign-procedure __collect_safe "cs_buf_cursor_get" (int) scheme-object))
(define __cs_buf_cursor_set (foreign-procedure __collect_safe "cs_buf_cursor_set" (int int) void))

(define (cursor)
   (call-foreign (__cs_buf_cursor_get (current-buffer))))

(define (cursor-set p)
   (when p
      (let ([c p])
         (when (and (not *buffer-enable-eof*)
                    (and (> c 0) (>= c (text-end-pos))))
            (set! c (- (text-end-pos) 1)))
         (call-foreign (__cs_buf_cursor_set (current-buffer) c))
         c)))

(define-syntax (with-saved-cursor stx)
   (syntax-case stx ()
      ((_ exp ...)
       #`(let ([curs (cursor)])
            (begin
               exp
               ...)
            (cursor-set curs)))))

(define-syntax (text-modify stx)
   (syntax-case stx ()
      ((_ exp ...)
       #`(if (buffer-is-readonly?)
            (begin
               (message "buffer is readonly")
               (cursor))
            ;; else
            (begin
               exp
               ...)))))

(define (text-insert t . s)
   (if (equal? (length s) 0)
      (call-foreign (__cs_buf_text_insert (current-buffer) t))
      ;; else
      (begin
         (let ([c (cursor)])
            (let ([p (- (call-foreign (__cs_buf_text_insert (current-buffer) t)) 1)])
               (for-each
                  (lambda (a)
                     (add-text-property c p a))
                  s)
               p)))))

(define (text-append t . s)
   (cursor-to-end)
   (apply text-insert t s))

(define (text-insert-char char)
   (call-foreign (__cs_buf_text_insert_char (current-buffer) char)))

(define (text-insert-nl)
   (text-modify (call-foreign (__cs_buf_text_insert_nl (current-buffer) (cursor)))))

(define (text-insert-empty-line-up)
   (text-modify
      (cursor-to-prev-line-end)
      (if (equal? (cursor) 0)
         (with-saved-cursor (text-insert-nl))
         ;; else
         (text-insert-nl))))

(define (text-insert-empty-line)
   (text-modify
      (cursor-to-line-end)
      (text-insert-nl)))

(define (text-insert-file t)
   (text-modify (call-foreign (__cs_buf_text_insert_file (current-buffer) t))))

(define (cursor-to-next-char)
   (cursor-set (text-next-char-pos)))

(define (cursor-to-prev-char)
   (cursor-set (text-prev-char-pos)))

(define (cursor-to-next-word)
   (cursor-set (text-next-word-pos)))

(define (cursor-to-prev-word)
   (cursor-set (text-prev-word-pos)))

(define (cursor-to-word-end)
   (cursor-set (text-word-end-pos)))

(define (cursor-to-next-longword)
   (cursor-set (text-next-longword-pos)))

(define (cursor-to-prev-longword)
   (cursor-set (text-prev-longword-pos)))

(define (cursor-to-longword-end)
   (cursor-set (text-longword-end-pos)))

(define (cursor-to-line)
   (cursor-set (text-next-line-pos (current-buffer) 0 (- n 1))))

(define (cursor-to-line-up)
   (cursor-set (text-prev-line-pos)))

(define (cursor-to-line-down)
   (when (not (is-last-line?))
      (cursor-set (text-next-line-pos))))

(define (cursor-to-next-line)
   (cursor-set (text-next-line-begin-pos)))

(define (cursor-to-prev-line-end)
   (cursor-set (text-prev-line-end-pos)))

(define (cursor-to-line-start)
   (cursor-set (text-line-start-pos)))

(define (cursor-to-line-finish)
   (cursor-set (text-line-finish-pos)))

(define (cursor-to-line-begin)
   (cursor-set (text-line-begin-pos)))

(define (cursor-to-line-end)
   (cursor-set (text-line-end-pos)))

(define (cursor-to-begin)
   (cursor-set (text-begin-pos)))

(define (cursor-to-end)
   (cursor-set (text-end-pos)))

(define (is-last-line?)
   (<= (- (text-end-pos) (text-line-end-pos)) 1))

(define (cursor-to-each-line fn)
   (let loop ()
      (cursor-to-line-begin)
      (fn)
      (when (not (is-last-line?))
         (cursor-to-next-line)
         (loop))))

(define (text-obj-pos buf curs obj num)
   (call-foreign (__cs_buf_text_obj_pos buf curs obj num)))

(define text-next-char-pos
   (case-lambda
      [()
       (text-obj-pos (current-buffer) (cursor) #\c 1)]

      [(s)
       (text-obj-pos (current-buffer) s #\c 1)]))

(define text-prev-char-pos
   (case-lambda
      [()
       (text-obj-pos (current-buffer) (cursor) #\c -1)]

      [(s)
       (text-obj-pos (current-buffer) s #\c -1)]))

(define (text-next-word-pos)
   (text-obj-pos (current-buffer) (cursor) #\w 1))

(define (text-prev-word-pos)
   (text-obj-pos (current-buffer) (cursor) #\w -1))

(define (text-word-end-pos)
   (text-obj-pos (current-buffer) (cursor) #\e 1))

(define (text-next-longword-pos)
   (text-obj-pos (current-buffer) (cursor) #\W 1))

(define (text-prev-longword-pos)
   (text-obj-pos (current-buffer) (cursor) #\W -1))

(define (text-longword-end-pos)
   (text-obj-pos (current-buffer) (cursor) #\E 1))

(define (text-prev-line-pos)
   (text-obj-pos (current-buffer) (cursor) #\l -1))

(define text-next-line-pos
   (case-lambda
      [()
       (text-next-line-pos (current-buffer) (cursor))]

      [(s)
       (text-next-line-pos (current-buffer) s)]

      [(b s)
       (text-obj-pos b s #\l 1)]

      [(b s n)
       (text-obj-pos b s #\l n)]))

(define (text-next-line-begin-pos)
   (text-obj-pos (current-buffer) (cursor) #\L 1))

(define (text-prev-line-end-pos)
   (text-obj-pos (current-buffer) (cursor) #\L -1))

(define (text-line-start-pos)
   (text-obj-pos (current-buffer) (cursor) #\0 -1))

(define (text-line-finish-pos)
   (text-obj-pos (current-buffer) (cursor) #\0 1))

(define text-line-begin-pos
   (case-lambda
      [()
       (text-line-begin-pos (current-buffer) (cursor))]

      [(s)
       (text-line-begin-pos (current-buffer) s)]

      [(b s)
       (text-obj-pos b s #\1 -1)]))

(define text-line-end-pos
   (case-lambda
      [()
       (text-obj-pos (current-buffer) (cursor) #\1 1)]

      [(s)
       (text-obj-pos (current-buffer) s #\1 1)]))

(define (text-begin-pos)
   (text-obj-pos (current-buffer) (cursor) #\g 1))

(define (text-end-pos)
   (text-obj-pos (current-buffer) (cursor) #\g -1))

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

(define (text-get-selectiona)
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

