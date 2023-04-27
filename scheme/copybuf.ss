(define copybuf-clip-app #f)
(define copybuf-clip-arg-get "")
(define copybuf-clip-arg-put "")
(define copybuf-sync-with-clip #t)
(define copybuf-is-linewise #f)
(define copybuf-reg "")

(define (copybuf-put str)
   (set! copybuf-reg str)
   (when copybuf-sync-with-clip
      (copybuf-clip-put str)))

(define copybuf-copy
   (case-lambda
      [(s)
       (set! copybuf-is-linewise #f)
       (copybuf-put s)]

      [(s l)
       (set! copybuf-is-linewise #t)
       (copybuf-put s)]))

(define copybuf-append
   (case-lambda
      [(s)
       (set! copybuf-is-linewise #f)
       (copybuf-put (string-append copybuf-reg " " s))]

      [(s l)
       (set! copybuf-is-linewise #t)
       (copybuf-put (string-append copybuf-reg "\n" s))]))

(define (copybuf-paste-inplace)
   (text-insert copybuf-reg))

(define (copybuf-paste)
   (if (not copybuf-is-linewise)
      (begin
         (when (not (equal? #\newline (text-char)))
            (cursor-to-next-char))
         (copybuf-paste-inplace)
         (cursor-to-prev-char))
      ;; else
      (begin
         (cursor-to-line-end)
         (text-insert "\n")
         (with-saved-cursor
            (copybuf-paste-inplace)
            (text-delete-char)))))

(define (copybuf-paste-before)
   (copybuf-paste-inplace)
   (cursor-to-prev-char))

(define (copybuf-check-clip-app)
   (cond
      [(program-exists? "xclip")
         (set! copybuf-clip-app "xclip")
         (set! copybuf-clip-arg-put "-i -selection clipboard")
         (set! copybuf-clip-arg-get "-o -selection clipboard")
         copybuf-clip-app]
      [(program-exists? "xsel")
         (set! copybuf-clip-app "xsel")
         (set! copybuf-clip-arg-put "-i -b")
         (set! copybuf-clip-arg-get "-o -b")
         copybuf-clip-app]
      [else #f]))

(define (copybuf-get-clip-app)
   (or copybuf-clip-app (copybuf-check-clip-app)))

(define (copybuf-clip-get)
   (let ([app (copybuf-get-clip-app)]
         [arg copybuf-clip-arg-get])
      (if app
         (let ([ret (process (format "~a ~a" app arg))])
            (let ([out (list-ref ret 0)]
                  [in (list-ref ret 1)]
                  [str ""])
               (while (not (port-eof? out))
                  (set! str (string-append str (get-string-some out))))
               (close-port out)
               (close-port in)
               str))
         ;; else
         "")))

(define (copybuf-clip-put str)
   (let ([app (copybuf-get-clip-app)]
         [arg copybuf-clip-arg-put])
      (if app
         (let ([ret (process (format "~a ~a" app arg))])
            (let ([out (list-ref ret 0)]
                  [in (list-ref ret 1)])
               (put-string-some in str)
               (close-port out)
               (close-port in))))))
