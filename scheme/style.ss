(define __cs_style_add (foreign-procedure "cs_style_add" (string int int int) scheme-object))
(define __cs_style_set (foreign-procedure "cs_style_set" (string int int int) scheme-object))
(define __cs_style_get (foreign-procedure "cs_style_get" (string) scheme-object))

(define (style-add symb prop)
   (let ([lst (style->list prop)])
      (call-foreign (__cs_style_add (symbol->string symb)
                                    (list-ref lst 0)
                                    (list-ref lst 1)
                                    (list-ref lst 2)))))

(define (style-modify symb prop)
   (let ([lst (style->list prop)])
      (call-foreign (__cs_style_set (symbol->string symb)
                                    (list-ref lst 0)
                                    (list-ref lst 1)
                                    (list-ref lst 2)))))

(define-syntax define-style
   (syntax-rules ()
      ((_ name prop ...)
         (style-add 'name prop ...))))

(define-syntax style-set
   (syntax-rules ()
      ((_ name prop ...)
         (style-modify 'name `(prop ...)))))

(define (color-name->number c)
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
      [else -1]))

(define (color-number->name c)
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
      [else "default"]))

(define (style-name->bit s)
   (cond
      [(equal? s "normal")    (bit 0)]
      [(equal? s "bold")      (bit 1)]
      [(equal? s "dim")       (bit 2)]
      [(equal? s "italic")    (bit 3)]
      [(equal? s "underline") (bit 4)]
      [(equal? s "blink")     (bit 5)]
      [(equal? s "reverse")   (bit 6)]
      [(equal? s "invisible") (bit 7)]
      [else 0]))

(define (style-name->number s)
   (let ([n 0])
      (for-each
         (lambda (o)
            (set! n (bitwise-ior n (style-name->bit o))))
         (string-split s #\space))
      n))

(define (style->list s)
   (let ([f -1]
         [b -1]
         [a 0])
        (let loop ([l s])
           (let ([t (car l)])
              (cond
                 [(equal? t 'fg:)
                  (set! f (color-name->number (cadr l)))
                  (when (> (length l) 2) (loop (cdr (cdr l))))
                 ]
                 [(equal? t 'bg:)
                  (set! b (color-name->number (cadr l)))
                  (when (> (length l) 2) (loop (cdr (cdr l))))
                 ]
                 [(equal? t 'attr:)
                  (set! a (style-name->number (cadr l)))
                  (when (> (length l) 2) (loop (cdr (cdr l))))])))
      (list f b a)))

(define (colors-show)
   (let ([cls '("default" "black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"
                "bright-black" "bright-red" "bright-green" "bright-yellow" "bright-magenta"
                "bright-cyan" "bright-white")])
      (with-current-buffer (buffer-create)
         (text-mode)
         (for-each
            (lambda (c)
               (text-insert (format "~a\n" c) `(style: (fg: ,c))))
            cls)
         (cursor-to-begin))))
