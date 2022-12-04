(define copybuf-sync-with-clip #t)
(define copybuf-is-linewise #f)
(define copybuf-reg "")

(define copybuf-put
   (lambda (str)
      (set! copybuf-reg str)
      (when copybuf-sync-with-clip
         (copybuf-clip-put str)
      )
   )
)

(define copybuf-copy
   (case-lambda
      [(s)
       (set! copybuf-is-linewise #f)
       (copybuf-put s)]

      [(s l)
       (set! copybuf-is-linewise #t)
       (copybuf-put s)]
   )
)

(define copybuf-append
   (case-lambda
      [(s)
       (set! copybuf-is-linewise #f)
       (copybuf-put (string-append copybuf-reg " " s))]

      [(s l)
       (set! copybuf-is-linewise #t)
       (copybuf-put (string-append copybuf-reg "\n" s))]
   )
)

(define copybuf-paste-inplace
   (lambda ()
      (text-insert copybuf-reg)
   )
)

(define copybuf-paste
   (lambda ()
      (if (not copybuf-is-linewise)
         (begin
            (when (not (equal? #\newline (text-char)))
               (cursor-to-next-char)
            )
            (copybuf-paste-inplace)
            (cursor-to-prev-char)
          )
          ;; else
          (begin
             (cursor-to-line-end)
             (text-insert "\n")
             (with-saved-cursor
                (copybuf-paste-inplace)
                (text-delete-char)
             )
          )
      )
   )
)

(define copybuf-paste-before
   (lambda ()
      (copybuf-paste-inplace)
      (cursor-to-prev-char)
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
