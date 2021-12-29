(define color-name->number
   (lambda (c)
      (cond
         [(equal? c "default") -1]
         [(equal? c "black") 0]
         [(equal? c "red") 1]
         [(equal? c "green") 2]
         [(equal? c "yellow") 3]
         [(equal? c "blue") 4]
         [(equal? c "magenta") 5]
         [(equal? c "cyan") 6]
         [(equal? c "white") 7]
         [(equal? c "bright-black") 8]
         [(equal? c "bright-red") 9]
         [(equal? c "bright-green") 10]
         [(equal? c "bright-yellow") 11]
         [(equal? c "bright-blue") 12]
         [(equal? c "bright-magenta") 13]
         [(equal? c "bright-cyan") 14]
         [(equal? c "bright-white") 15]
         [else -1]
      )
   )
)

(define color-number->name
   (lambda (c)
      (cond
         [(equal? c -1) "default"]
         [(equal? c 0) "black"]
         [(equal? c 1) "red"]
         [(equal? c 2) "green"]
         [(equal? c 3) "yellow"]
         [(equal? c 4) "blue"]
         [(equal? c 5) "magenta"]
         [(equal? c 6) "cyan"]
         [(equal? c 7) "white"]
         [(equal? c 8) "bright-black"]
         [(equal? c 9) "bright-red"]
         [(equal? c 10) "bright-green"]
         [(equal? c 11) "bright-yellow"]
         [(equal? c 12) "bright-blue"]
         [(equal? c 13) "bright-magenta"]
         [(equal? c 14) "bright-cyan"]
         [(equal? c 15) "bright-white"]
         [else "default"]
      )
   )
)

(define style-name->bit
   (lambda (s)
      (cond
         [(equal? s "normal")    (bit 0)]
         [(equal? s "bold")      (bit 1)]
         [(equal? s "dim")       (bit 2)]
         [(equal? s "underline") (bit 3)]
         [(equal? s "blink")     (bit 4)]
         [(equal? s "reverse")   (bit 5)]
         [(equal? s "invisible") (bit 6)]
         [else 0]
      )
   )
)

(define style-name->number
   (lambda (s)
      (let ([n 0])
         (for-each
            (lambda (o)
               (set! n (bitwise-ior n (style-name->bit o)))
            )
            (string-split s #\space)
         )
         n
      )
   )
)
