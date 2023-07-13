(library (borsch frame)
   (export
      frame-delete
      frame-create
      frame-list
      current-frame
      frame-set-var!
      frame-get-var
      with-current-frame
      frame-switch
      frame-id
      frame-name
      frame-set-name
      frame-cwd
      frame-set-cwd
      frame-cwd-handler
      frame-prev-layout
      frame-set-prev-layout
      frame-layout
      frame-set-layout
      frame-buffer-list
      frame-insert-buffer
      frame-remove-buffer
      frame-for-each-buffer
      frame-find-buffer
      frame-get-buffer-by-file
      frame-get-buffer
      frame-is-sticky?
      frame-set-sticky
      frame-n-master
      frame-set-n-master
      frame-%-master
      frame-set-%-master
      frame-set-prev-focused-window
      frame-prev-focused-window
      frame-current-window
      frame-set-current-window
      frame-delete-window
      frame-initialize)
   (import
      (chezscheme)
      (borsch base)
      (borsch lists)
      (borsch buffer))

(define __cs_frame_current_set (foreign-procedure "cs_frame_current_set" (int) void))
(define __cs_frame_create (foreign-procedure "cs_frame_create" () scheme-object))
(define __cs_frame_delete (foreign-procedure "cs_frame_delete" (int) void))

(define-record-type %frame%
   (fields
      id
      (mutable name)
      (mutable cwd)
      (mutable prev-layout)
      (mutable layout)
      (mutable env)
      (mutable buffers)
      (mutable sticky)
      (mutable n-master)
      (mutable %-master)
      (mutable focus-stack)
      (mutable current-window)))

(define frames-ht (make-eq-hashtable))

(define (*frame-create*)
   (call-foreign (__cs_frame_create)))

(define (frame-delete fr)
   (call-foreign (__cs_frame_delete (frame-id fr)))
   (hashtable-delete! frames-ht (frame-id fr)))

(define frame-create
   (case-lambda
     [()
      (frame-create #f)]

     [(name)
      (let ([id (*frame-create*)])
         (let ([fr (make-%frame% id
                                 (or name
                                     (format "frame~a" id))
                                 (current-directory)
                                 #f
                                 'tiled
                                 (make-eq-hashtable)
                                 (list)
                                 #f
                                 1
                                 0.5
                                 (make-stack)
                                 #f)])
            (hashtable-set! frames-ht id fr)
            fr))]))

(define (frame-list)
   (vector->list (hashtable-values frames-ht)))

(define *current-frame* #f)

(define (current-frame)
   *current-frame*)

(define-syntax (frame-set-var! stx)
   (syntax-case stx ()
      ((_ f s v)
         #`(hashtable-set! (%frame%-env f) 's v))))

(define-syntax (frame-get-var stx)
   (syntax-case stx ()
      ((_ f s)
         #`(hashtable-ref (%frame%-env f) 's #f))))

(define-syntax (with-current-frame stx)
   (syntax-case stx ()
      ((_ frame exp ...)
       #`(let ([from (current-frame)]
               [to frame])
            (frame-switch to)
            (begin
               exp
               ...)
            (frame-switch from)))))

(define (frame-switch fr)
   (set! *current-frame* fr)
   (call-foreign (__cs_frame_current_set (frame-id fr)))
   (run-hooks 'frame-switch-hook fr))

(define frame-id
   (case-lambda
      [()
       (frame-id (current-frame))]

      [(fr)
       (%frame%-id fr)]))

(define frame-name
   (case-lambda
      [()
       (frame-name (current-frame))]

      [(fr)
       (%frame%-name fr)]))

(define frame-set-name
   (case-lambda
      [(name)
       (frame-set-name (current-frame) name)]

      [(fr n)
       (%frame%-name-set! fr n)
       (run-hooks 'change-frame-name-hook)]))

(define frame-cwd
   (case-lambda
      [()
       (frame-cwd (current-frame))]

      [(fr)
       (%frame%-cwd fr)]))

(define frame-set-cwd
   (case-lambda
      [(cwd)
       (frame-set-cwd (current-frame) cwd)]

      [(fr c)
       (%frame%-cwd-set! fr c)
       (run-hooks 'change-cwd-hook)]))

(define frame-cwd-handler
   (case-lambda
      [()
       (frame-cwd)]

      [(cwd)
       (frame-set-cwd cwd)]))

(define frame-prev-layout
   (case-lambda
      [()
       (frame-prev-layout (current-frame))]

      [(fr)
       (%frame%-prev-layout fr)]))

(define frame-set-prev-layout
   (case-lambda
      [(l)
       (frame-set-prev-layout (current-frame) l)]

      [(fr l)
       (%frame%-prev-layout-set! fr l)]))

(define frame-layout
   (case-lambda
      [()
       (frame-layout (current-frame))]

      [(fr)
       (%frame%-layout fr)]))

(define frame-set-layout
   (case-lambda
      [(l)
       (frame-set-layout (current-frame) l)]

      [(fr l)
       (%frame%-layout-set! fr l)]))

(define frame-buffer-list
   (case-lambda
      [()
       (frame-buffer-list (current-frame))]

      [(fr)
       (%frame%-buffers fr)]))

(define frame-insert-buffer
   (case-lambda
      [(b)
       (frame-insert-buffer (current-frame) b)]

      [(fr b)
       (when fr
          (%frame%-buffers-set! fr (append (%frame%-buffers fr) (list b))) )]))

(define frame-remove-buffer
   (case-lambda
      [(b)
       (frame-remove-buffer (current-frame) b)]

      [(fr b)
       (%frame%-buffers-set! fr (remove b (%frame%-buffers fr)))]))

(define (frame-for-each-buffer fn)
   (for-each
      (lambda (b)
         (fn b))
      (frame-buffer-list)))

(define (frame-find-buffer fn)
   (let ([b (find
               (lambda (b)
                  (fn b))
               (frame-buffer-list))])
      b))

(define (frame-get-buffer-by-file file)
   (frame-find-buffer
      (lambda (b)
         (equal? file (buffer-filename b)))))

(define (frame-get-buffer name)
   (frame-find-buffer
      (lambda (b)
         (equal? name (buffer-name b)))))

(define frame-is-sticky?
   (case-lambda
      [()
       (frame-is-sticky? (current-frame))]

      [(fr)
       (%frame%-sticky fr)]))

(define frame-set-sticky
   (case-lambda
      [(l)
       (frame-set-sticky (current-frame) l)]

      [(fr l)
       (%frame%-sticky-set! fr l)]))

(define frame-n-master
   (case-lambda
      [()
       (frame-n-master (current-frame))]

      [(fr)
       (%frame%-n-master fr)]))

(define frame-set-n-master
   (case-lambda
      [(n)
       (frame-set-n-master (current-frame) n)]

      [(fr n)
       (%frame%-n-master-set! fr n)]))

(define frame-%-master
   (case-lambda
      [()
       (frame-%-master (current-frame))]

      [(fr)
       (%frame%-%-master fr)]))

(define frame-set-%-master
   (case-lambda
      [(n)
       (frame-set-%-master (current-frame) n)]

      [(fr n)
       (%frame%-%-master-set! fr n)]))

(define frame-set-prev-focused-window
   (case-lambda
      [(w)
       (frame-set-prev-focused-window (current-frame) w)]

      [(fr w)
       (when w
          (let ([st (%frame%-focus-stack fr)])
             (stack-remove! st w)
             (stack-push! st w)))]))

(define frame-prev-focused-window
   (case-lambda
      [()
       (frame-prev-focused-window (current-frame))]

      [(fr)
       (let ([st (%frame%-focus-stack fr)])
          (stack-top st))]))

(define frame-current-window
   (case-lambda
      [()
       (frame-current-window (current-frame))]

      [(fr)
       (%frame%-current-window fr)]))

(define frame-set-current-window
   (case-lambda
      [(w)
       (frame-set-current-window (current-frame) w)]

      [(fr w)
       (%frame%-current-window-set! fr w)]))

(define frame-delete-window
   (case-lambda
      [(w)
       (frame-delete-window (current-frame) w)]

      [(fr w)
       (let ([st (%frame%-focus-stack fr)])
          (stack-remove! st w))]))

(define (frame-initialize)
   (current-cwd-handler frame-cwd-handler)
   (add-hook 'buffer-insert-hook
             (lambda (b)
                (frame-insert-buffer b) ))
   (add-hook 'buffer-remove-hook
             (lambda (b)
                (frame-remove-buffer b) )))
)
