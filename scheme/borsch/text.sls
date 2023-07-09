(library (borsch text)
   (export
      cursor
      cursor-set
      with-saved-cursor
      cursor-to-next-char
      cursor-to-prev-char
      cursor-to-next-word
      cursor-to-prev-word
      cursor-to-word-end
      cursor-to-next-longword
      cursor-to-prev-longword
      cursor-to-longword-end
      cursor-to-line
      cursor-to-line-up
      cursor-to-line-down
      cursor-to-next-line
      cursor-to-prev-line-end
      cursor-to-line-start
      cursor-to-line-finish
      cursor-to-line-begin
      cursor-to-line-end
      cursor-to-begin
      cursor-to-end
      is-last-line?
      cursor-to-each-line
      text-insert
      text-append
      text-insert-char
      text-end-pos
      text-begin-pos
      text-line-end-pos
      text-line-begin-pos
      text-line-finish-pos
      text-line-start-pos
      text-prev-line-end-pos
      text-next-line-begin-pos
      text-next-line-pos
      text-prev-line-pos
      text-longword-end-pos
      text-prev-longword-pos
      text-next-longword-pos
      text-word-end-pos
      text-prev-word-pos
      text-next-word-pos
      text-prev-char-pos
      text-next-char-pos
   )
   (import
      (borsch base)
      (borsch buffer)
      (chezscheme))

(define __cs_buf_cursor_get (foreign-procedure "cs_buf_cursor_get" (int) scheme-object))
(define __cs_buf_cursor_set (foreign-procedure "cs_buf_cursor_set" (int int) void))

(define __cs_buf_text_insert (foreign-procedure "cs_buf_text_insert" (int string) scheme-object))
(define __cs_buf_text_insert_char (foreign-procedure "cs_buf_text_insert_char" (int int) scheme-object))

(define __cs_buf_text_obj_pos (foreign-procedure "cs_buf_text_obj_pos" (int int char int) scheme-object))

(define *buffer-enable-eof* #t)

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

(define (cursor)
   (call-foreign (__cs_buf_cursor_get (current-buffer))))

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

(define (cursor-to-line n)
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

)
