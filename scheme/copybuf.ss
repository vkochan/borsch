(define copybuf-is-linewise #f)
(define copybuf-reg "")

(define copybuf-copy
   (case-lambda
      [(s)
       (set! copybuf-is-linewise #f)
       (set! copybuf-reg s)]

      [(s l)
       (set! copybuf-is-linewise #t)
       (set! copybuf-reg s)]
   )
)

(define copybuf-append
   (case-lambda
      [(s)
       (set! copybuf-is-linewise #f)
       (set! copybuf-reg (string-append copybuf-reg " " s))]

      [(s l)
       (set! copybuf-is-linewise #t)
       (set! copybuf-reg (string-append copybuf-reg "\n" s))]
   )
)

(define copybuf-paste-inplace
   (lambda ()
      (insert copybuf-reg)
   )
)

(define copybuf-paste
   (lambda ()
      (if (not copybuf-is-linewise)
         (begin
            (when (not (equal? #\newline (extract-char)))
               (move-next-char)
            )
            (copybuf-paste-inplace)
            (move-prev-char)
          )
          ;; else
          (begin
             (move-line-end)
             (insert-nl)
             (save-cursor
                (copybuf-paste-inplace)
                (delete-char)
             )
          )
      )
   )
)

(define copybuf-paste-before
   (lambda ()
      (copybuf-paste-inplace)
      (move-prev-char)
   )
)
