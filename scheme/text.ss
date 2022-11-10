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

(define message
   (lambda (s)
      (let ([b (buffer-get "*Messages*")])
         (when b
            (with-current-buffer b
               (insert (format "~a\n" s))
            )
         )
         (run-hooks 'message-hook s)
      )
   )
)

(define-syntax (buffer-modify stx)
   (syntax-case stx ()
      ((_ exp ...)
       #`(if (buffer-is-readonly?)
            (begin
               (message "buffer is readonly")
               (cursor)
            )
            ;; else
            (begin
               exp
               ...
            )
         )
      )
   )
)

(define insert
   (lambda (t . s)
      (if (equal? (length s) 0)
         (call-foreign (__cs_buf_text_insert (current-buffer) t))
         ;; else
         (begin
            (let ([c (cursor)])
               (let ([p (- (call-foreign (__cs_buf_text_insert (current-buffer) t)) 1)])
                  (for-each
                     (lambda (a)
                        (add-text-property c p a)
                     ) s
                  )
                  p
               )
            )
	 )
      )
   )
)

(define insert-char
   (lambda (char)
      (call-foreign (__cs_buf_text_insert_char (current-buffer) char))
   )
)

(define insert-nl
   (lambda ()
      (buffer-modify (call-foreign (__cs_buf_text_insert_nl (current-buffer) (cursor))))
   )
)

(define insert-empty-line-up
   (lambda ()
      (buffer-modify
         (move-prev-line-end)
         (if (equal? (cursor) 0)
            (save-cursor (insert-nl))
            ;; else
            (insert-nl)
         )
      )
   )
)

(define insert-empty-line
   (lambda ()
      (buffer-modify
         (move-line-end)
         (insert-nl)
      )
   )
)

(define insert-file
   (lambda (t)
      (buffer-modify (call-foreign (__cs_buf_text_insert_file (current-buffer) t)))
   )
)

(define move-next-char
   (lambda ()
      (cursor-set (next-char-pos))
   )
)

(define move-prev-char
   (lambda ()
      (cursor-set (prev-char-pos))
   )
)

(define move-next-word
   (lambda ()
      (cursor-set (next-word-pos))
   )
)

(define move-prev-word
   (lambda ()
      (cursor-set (prev-word-pos))
   )
)

(define move-word-end
   (lambda ()
      (cursor-set (word-end-pos))
   )
)

(define move-next-longword
   (lambda ()
      (cursor-set (next-longword-pos))
   )
)

(define move-prev-longword
   (lambda ()
      (cursor-set (prev-longword-pos))
   )
)

(define move-longword-end
   (lambda ()
      (cursor-set (longword-end-pos))
   )
)

(define move-line-num
   (lambda (n)
      (cursor-set (next-line-pos (current-buffer) 0 (- n 1)))
   )
)

(define move-line-up
   (lambda ()
      (cursor-set (prev-line-pos))
   )
)

(define move-line-down
   (lambda ()
      (when (not (is-last-line?))
         (cursor-set (next-line-pos))
      )
   )
)

(define move-next-line
   (lambda ()
      (cursor-set (next-line-begin-pos))
   )
)

(define move-prev-line-end
   (lambda ()
      (cursor-set (prev-line-end-pos))
   )
)

(define move-line-start
   (lambda ()
      (cursor-set (line-start-pos))
   )
)

(define move-line-finish
   (lambda ()
      (cursor-set (line-finish-pos))
   )
)

(define move-line-begin
   (lambda ()
      (cursor-set (line-begin-pos))
   )
)

(define move-line-end
   (lambda ()
      (cursor-set (line-end-pos))
   )
)

(define move-buffer-begin
   (lambda ()
      (cursor-set (buffer-begin-pos))
   )
)

(define move-buffer-end
   (lambda ()
      (cursor-set (buffer-end-pos))
   )
)

(define is-last-line?
   (lambda ()
      (<= (- (buffer-end-pos) (line-end-pos)) 1)
   )
)

(define move-each-line
   (lambda (fn)
      (let loop ()
         (move-line-begin)
         (fn)
         (when (not (is-last-line?))
            (move-next-line)
            (loop)
         )
      )
   )
)

(define text-obj-pos
   (lambda (buf curs obj num)
      (call-foreign (__cs_buf_text_obj_pos buf curs obj num))
   )
)

(define next-char-pos
   (case-lambda
      [()
       (text-obj-pos (current-buffer) (cursor) #\c 1)]

      [(s)
       (text-obj-pos (current-buffer) s #\c 1)]
   )
)

(define prev-char-pos
   (case-lambda
      [()
       (text-obj-pos (current-buffer) (cursor) #\c -1)]

      [(s)
       (text-obj-pos (current-buffer) s #\c -1)]
   )
)

(define next-word-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\w 1)
   )
)

(define prev-word-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\w -1)
   )
)

(define word-end-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\e 1)
   )
)

(define next-longword-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\W 1)
   )
)

(define prev-longword-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\W -1)
   )
)

(define longword-end-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\E 1)
   )
)

(define prev-line-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\l -1)
   )
)

(define next-line-pos
   (case-lambda
      [()
       (next-line-pos (current-buffer) (cursor))]

      [(s)
       (next-line-pos (current-buffer) s)]

      [(b s)
       (text-obj-pos b s #\l 1)]

      [(b s n)
       (text-obj-pos b s #\l n)]
   )
)

(define next-line-begin-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\L 1)
   )
)

(define prev-line-end-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\L -1)
   )
)

(define line-start-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\0 -1)
   )
)

(define line-finish-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\0 1)
   )
)

(define line-begin-pos
   (case-lambda
      [()
       (line-begin-pos (current-buffer) (cursor))]

      [(s)
       (line-begin-pos (current-buffer) s)]

      [(b s)
       (text-obj-pos b s #\1 -1)]
   )
)

(define line-end-pos
   (case-lambda
      [()
      (text-obj-pos (current-buffer) (cursor) #\1 1)]

      [(s)
      (text-obj-pos (current-buffer) s #\1 1)]
   )
)

(define buffer-begin-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\g 1)
   )
)

(define buffer-end-pos
   (lambda ()
      (text-obj-pos (current-buffer) (cursor) #\g -1)
   )
)

(define buffer-string
   (case-lambda
      [()
       (call-foreign (__cs_buf_text_get (current-buffer)
                          (buffer-begin-pos)
                          (- (buffer-end-pos) (buffer-begin-pos))))]

      [(start end)
       (let ([start (min start end)] [end (max start end)])
          (call-foreign (__cs_buf_text_get (current-buffer) start (- end start)))
       )
      ]
   )
)

(define extract-char
   (case-lambda
      [()
         (extract-char (cursor))]

      [(s)
         (let ([str (buffer-string s (next-char-pos s))])
            (if (> (string-length str) 0)
               (string-ref str 0)
               ;; else
               #f
            )
         )
      ]
   )
)

(define text-obj-range
   (lambda (buf curs obj t?)
      (call-foreign (__cs_buf_text_obj_range buf curs obj t?))
   )
)

(define extract-word
   (case-lambda
      [()
       (extract-word (cursor))]

      [(s)
       (if (local-bound? extract-word)
          ((get-local extract-word))
          ;; else
          (let ([r (text-obj-range (current-buffer) s #\w #t)])
             (buffer-string (car r) (cdr r))
          )
       )
      ]
   )
)

(define extract-longword
   (case-lambda
      [()
       (extract-longword (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\W #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-object
   (case-lambda
      [()
       (extract-object (cursor))]

      [(s)
       (let ([obj (if (selection-is-set?)
                      (let ([sel (selection-extract)])
                         (selection-clear)
                         sel
                      )
                      ;else
                      (extract-longword s)
                  )
             ])
          (string-trim (extract-longword s)
                       '(#\space #\< #\> #\( #\) #\[ #\] #\" #\'))
       )
      ]
   )
)

(define extract-line
   (case-lambda
      [()
       (extract-line (cursor))]

      [(s)
       (buffer-string (line-begin-pos) (line-end-pos))
      ]
   )
)

(define extract-line-inner
   (case-lambda
      [()
       (extract-line-inner (cursor))]

      [(s)
       (buffer-string (line-start-pos) (line-end-pos))
      ]
   )
)

(define extract-square-brackets
   (case-lambda
      [()
       (extract-square-brackets (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\[ #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-square-brackets-inner
   (case-lambda
      [()
       (extract-square-brackets-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\[ #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-curly-brackets
   (case-lambda
      [()
       (extract-curly-brackets (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\{ #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-curly-brackets-inner
   (case-lambda
      [()
       (extract-curly-brackets-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\{ #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-angle-brackets
   (case-lambda
      [()
       (extract-angle-brackets (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\< #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-angle-brackets-inner
   (case-lambda
      [()
       (extract-angle-brackets-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\< #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-parens
   (case-lambda
      [()
       (extract-parens (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\( #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-parens-inner
   (case-lambda
      [()
       (extract-parens-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\( #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-quote
   (case-lambda
      [()
       (extract-quote (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\" #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-quote-inner
   (case-lambda
      [()
       (extract-quote-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\" #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-single-quote
   (case-lambda
      [()
       (extract-single-quote (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\' #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-single-quote-inner
   (case-lambda
      [()
       (extract-single-quote-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\' #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-back-quote
   (case-lambda
      [()
       (extract-back-quote (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\` #f)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define extract-back-quote-inner
   (case-lambda
      [()
       (extract-back-quote-inner (cursor))]

      [(s)
       (let ([r (text-obj-range (current-buffer) s #\` #t)])
          (buffer-string (car r) (cdr r))
       )
      ]
   )
)

(define-syntax (extract-deletion stx)
   (syntax-case stx ()
      ((_ exp ...)
       #`(let ([del-text ""])
            (let (
                  [del-hook (lambda (start end)
                               (let ([text (buffer-string start end)])
                                  (set! del-text (string-append del-text text))
                               )
                            )
                  ]
                 )
               (define-local text-delete-hook del-hook)
               (begin
                  exp
                  ...
               )
               (set-local! text-delete-hook #f)
               del-text
            )
         )
      )
   )
)

(define delete-range
   (lambda (s e)
      (buffer-modify
         (let ([del-hook (get-local text-delete-hook #f)])
            (when del-hook 
               (del-hook s e)
            )
         )
         (call-foreign (__cs_buf_text_range_del (current-buffer) s e))
      )
   )
)

(define replace-range
   (lambda (s e t)
      (delete-range s e)
      (cursor-set s)
      (insert t)
   )
)

(define cursor-obj-delete
   (lambda (fn)
      (buffer-modify
         (let (
               [end (fn)]
               [start (cursor)]
              )
            (delete-range start end)
         )
      )
   )
)

(define cursor-obj-delete-inclusive
   (lambda (fn)
      (buffer-modify
         (let (
               [end (next-char-pos (fn))]
               [start (cursor)]
              )
            (delete-range start end)
         )
      )
   )
)

(define delete-next-char
   (lambda ()
      (cursor-obj-delete move-next-char)
   )
)
(define delete-char delete-next-char)

(define delete-prev-char
   (lambda ()
      (cursor-obj-delete move-prev-char)
   )
)

(define delete-next-word
   (lambda ()
      (cursor-obj-delete move-next-word)
   )
)
(define delete-word delete-next-word)

(define delete-prev-word
   (lambda ()
      (cursor-obj-delete move-prev-word)
   )
)

(define delete-word-end
   (lambda ()
      (cursor-obj-delete-inclusive move-word-end)
   )
)

(define delete-next-longword
   (lambda ()
      (cursor-obj-delete move-next-longword)
   )
)
(define delete-longword delete-next-longword)

(define delete-prev-longword
   (lambda ()
      (cursor-obj-delete move-prev-longword)
   )
)

(define delete-longword-end
   (lambda ()
      (cursor-obj-delete-inclusive move-longword-end)
   )
)

(define delete-next-line-begin
   (lambda ()
      (cursor-obj-delete move-next-line)
   )
)

(define delete-prev-line-end
   (lambda ()
      (cursor-obj-delete move-prev-line-end)
   )
)

(define delete-line-start
   (lambda ()
      (cursor-obj-delete move-line-start)
   )
)

(define delete-line-finish
   (lambda ()
      (cursor-obj-delete move-line-finish)
   )
)

(define delete-line-begin
   (lambda ()
      (cursor-obj-delete move-line-begin)
   )
)

(define delete-line-end
   (lambda ()
      (cursor-obj-delete move-line-end)
   )
)

(define delete-line
   (lambda ()
      (move-line-end)
      (delete-prev-line-end)
      (if (equal? (cursor) 0)
         (delete-char)
         ;; else
         (if (equal? (buffer-end-pos) (1+ (line-end-pos)))
            (move-line-begin)
            ;; else
            (move-next-line)
         )
      )
   )
)

(define delete-buffer-begin
   (lambda ()
      (cursor-obj-delete move-buffer-begin)
   )
)

(define delete-buffer-end
   (lambda ()
      (cursor-obj-delete move-buffer-end)
   )
)

(define erase-buffer
   (lambda ()
      (let (
            [s (buffer-begin-pos)]
            [e (buffer-end-pos)]
           )
         (remove-text-property)
         (delete-range s e)
      )
   )
)

(define copy-line
   (lambda ()
      (copybuf-copy (buffer-string (line-begin-pos) (1+ (line-end-pos))) #t)
   )
)

(define selection-set
   (case-lambda
      [()
       (call-foreign (__cs_buf_mark_set (current-buffer) (cursor)))]

      [(s)
       (call-foreign (__cs_buf_mark_set (current-buffer) s))]
   )
)

(define selection-get
   (lambda ()
      (call-foreign (__cs_buf_mark_get (current-buffer)))
   )
)

(define selection-is-set?
   (lambda ()
      (call-foreign (__cs_buf_mark_is_set (current-buffer)))
   )
)

(define selection-get-range
   (lambda ()
      (let (
            [m (selection-get)]
            [c (cursor)]
           )
           (let (
                 [s (min m c)]
                 [e (max m c)]
                )
	      (list s (next-char-pos e))
           )
      )
   )
)

(define selection-extract
   (lambda ()
      (let ([r (selection-get-range)])
         (buffer-string (car r) (cadr r))
      )
   )
)

(define selection-delete
   (lambda ()
      (let ([r (selection-get-range)])
         (delete-range (car r) (cadr r))
      )
   )
)

(define selection-copy
   (lambda ()
      (let ([r (selection-get-range)])
           (copybuf-copy (buffer-string (car r) (cadr r)))
      )
   )
)

(define selection-copy-append
   (lambda ()
      (let ([r (selection-get-range)])
           (copybuf-append (buffer-string (car r) (cadr r)))
      )
   )
)

(define selection-copy-linewise
   (lambda ()
      (let ([r (selection-get-range)])
           (copybuf-copy (buffer-string (car r) (cadr r)) #t)
      )
   )
)

(define selection-copy-append-linewise
   (lambda ()
      (let ([r (selection-get-range)])
           (copybuf-append (buffer-string (car r) (cadr r)) #t)
      )
   )
)

(define selection-highlight
   (case-lambda
      [(e)
       (selection-highlight (__cs_win_current_get) e)]

      [(wid e)
       (when wid (call-foreign (__cs_win_mark_highlight wid e)))]
   )
)

(define selection-clear
   (lambda ()
      (when (local-bound? selection-clear-hook)
         ((get-local selection-clear-hook))
      )
      (call-foreign (__cs_buf_mark_clear (current-buffer)))
   )
)

(define-syntax (buffer-modify stx)
   (syntax-case stx ()
      ((_ exp ...)
       #`(if (buffer-is-readonly?)
            (begin
               (message "buffer is readonly")
               (cursor)
            )
            ;; else
            (begin
               exp
               ...
            )
         )
      )
   )
)

(define buffer-reload-file
   (lambda ()
      (save-cursor
         (erase-buffer)
         (insert-file (buffer-filename))
         (buffer-save)
      )
   )
)

(define buffer-eval
   (case-lambda
      [()
       (buffer-eval (current-buffer))
      ]

      [(buf)
       (with-current-buffer buf
          (with-input-from-string (string-append "\'" (buffer-string))
             (lambda ()
                (eval (read))
             )
          )
       )
      ]
   )
)

