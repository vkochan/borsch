(define __cs_frame_current_set (foreign-procedure __collect_safe "cs_frame_current_set" (int) void))
(define __cs_frame_create (foreign-procedure __collect_safe "cs_frame_create" () scheme-object))
(define __cs_frame_delete (foreign-procedure __collect_safe "cs_frame_delete" (int) void))

(define-record-type %frame%
   (fields
      id
      (mutable name)
      (mutable cwd)
      (mutable prev-layout)
      (mutable env)))

(define frames-ht (make-eq-hashtable))

(define (*frame-create*)
   (call-foreign (__cs_frame_create)))

(define (frame-delete fid)
   (call-foreign (__cs_frame_delete fid)))

(define (frame-create name)
   (let ([id (*frame-create*)])
      (let ([fr (make-%frame% id
                              name
                              (current-directory)
                              #f
                              (make-eq-hashtable))])
         (hashtable-set! frames-ht id fr)
         fr)))

(define (frame-list)
   (vector->list (hashtable-values frames-ht)))

;;(define frame-all (frame-create "all"))
(define frame-1 (frame-create ""))
(define frame-2 (frame-create ""))
(define frame-3 (frame-create ""))
(define frame-4 (frame-create ""))
(define frame-5 (frame-create ""))
(define frame-6 (frame-create ""))
(define frame-7 (frame-create ""))
(define frame-8 (frame-create ""))
(define frame-9 (frame-create ""))

(define *current-frame* frame-1)

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
