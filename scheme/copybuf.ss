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

(define copybuf-has-xclip?
   (lambda ()
      (delay (program-exists? "xclip"))
   )
)

(define copybuf-clip-get
   (lambda ()
      (if (copybuf-has-xclip?)
         (let (
               [ret (process "xclip -o")]
              )
            (let (
                  [out (list-ref ret 0)]
                  [in (list-ref ret 1)]
                  [str ""]
                 )
               (while (not (port-eof? out))
                  (set! str (string-append str (get-string-some out)))
               )
               (close-port out)
               (close-port in)
               str
            )
         )
         ;; else
         "" 
      )
   )
)

(define copybuf-clip-put
   (lambda (str)
      (if (copybuf-has-xclip?)
         (let (
               [ret (process "xclip -i")]
              )
            (let (
                  [out (list-ref ret 0)]
                  [in (list-ref ret 1)]
                 )
               (put-string-some in str)
               (close-port out)
               (close-port in)
            )
         )
      )
   )
)
