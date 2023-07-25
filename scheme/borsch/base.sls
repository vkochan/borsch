(library (borsch base)
   (export
      bit
      count-digits-num
      try
      error->string
      call-foreign
      while
      run-hooks
      add-hook
      remove-hook
      current-cwd
      with-current-cwd
      current-cwd-handler
      stack-trace->string)
   (import (chezscheme))

(define (bit n)
   (bitwise-arithmetic-shift-left 1 n))

(define (count-digits-num n)
   (let loop ([e 0])
      (if (> (fx/ n (expt 10 e)) 0)
         (loop (+ 1 e))
         ;; else
         e)))


(define (error->string e)
   (let-values ([(op g) (open-string-output-port)])
      (display-condition e op)
      (g)))

(define-syntax try
   (syntax-rules ()
      ((_ body)
       (try body #f))

      ((_ body catcher)
       (call-with-current-continuation
          (lambda (exit)
             (with-exception-handler
                (lambda (condition)
                   (run-hooks 'error-hook (stack-trace->string condition))
		   (when catcher
                      (catcher condition))
                   (exit condition))
                (lambda () body)))))))

(define (run-hooks symb . args)
   (if (top-level-bound? symb)
      (let ([hook-list (top-level-value symb)])
         (for-each
            (lambda (h)
               (let ([fn (if (symbol? h) (eval h) h)])
                  (try (apply fn args))))
            hook-list))))

(define (add-hook h f)
   (if (not (top-level-bound? h))
      (define-top-level-value h (list)))
   (let ([h-lst (top-level-value h)])
      (if (not (member f h-lst))
         (set-top-level-value! h (append h-lst (list f))))))

(define (remove-hook h f)
   (if (top-level-bound? h)
      (let ([h-lst (top-level-value h)])
         (set-top-level-value! h (remove f h-lst)))))

;;(define foreign-mutex (make-mutex))

(define-syntax (call-foreign stx)
   (syntax-case stx ()
      ((_ exp ...)
       ;;#`(with-mutex foreign-mutex
       #`(begin
            exp
	    ...))))

(define-syntax (while stx)
  (syntax-case stx ()
               ((_ condition expression ...)
                #`(do ()
                    ((not condition))
                    expression
                    ...))))

(define current-cwd-handler (make-parameter #f))
(define current-cwd-val (current-directory))
(define current-cwd-tmp (make-parameter #f))

(define current-cwd
   (case-lambda
      [()
       (if (current-cwd-tmp)
          (current-cwd-tmp)
          ;; else
          (let ([handler (current-cwd-handler)])
             (if handler
                (handler)
                ;; else
                current-cwd-val )))]

      [(cwd)
       (if (current-cwd-tmp)
          (current-cwd-tmp cwd)
          ;; else
          (let ([handler (current-cwd-handler)])
             (if handler
                (handler cwd)
                ;; else
                (set! current-cwd-val cwd) )))]))

(define-syntax (with-current-cwd stx)
   (syntax-case stx ()
      ((_ cwd exp ...)
       #`(parameterize ([current-cwd-tmp cwd])
            exp ... ))))

(define (stack-trace->string e)
   (define (get-func c)
      (let ((cc ((c 'code) 'name)))
         (if cc cc "--main--")))

   (with-output-to-string
      (lambda ()
         (display-condition e) (newline)
  
         (let p ((t (inspect/object (condition-continuation e))))
            (call/cc
               (lambda (ret)
                  (if (> (t 'depth) 1)
                      (begin
                         (call-with-values
                            (lambda () (t 'source-path))
                            (case-lambda
                               ((file line column)
                                (printf "\tat ~a (~a:~a,~a)\n" (get-func t) file line column))
                               (else (ret)) ))
                         (p (t 'link)) ))))))))
)
