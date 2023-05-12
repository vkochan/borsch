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
      (mutable %-master)))

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
                              'tiled
                              (make-eq-hashtable)
                              (list)
                              #f
                              1
                              0.5)])
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
   (window-layout-set-changed #t)
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

(define frame-list-buffer
   (case-lambda
      [()
       (frame-list-buffer (current-frame))]

      [(fr)
       (%frame%-buffers fr)]))

(define frame-insert-buffer
   (case-lambda
      [(b)
       (frame-insert-buffer (current-frame) b)]

      [(fr b)
       (%frame%-buffers-set! fr (append (%frame%-buffers fr) (list b)))]))

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
      (frame-list-buffer)))

(define (frame-find-buffer fn)
   (let ([b (find
               (lambda (b)
                  (fn b))
               (frame-list-buffer))])
      b))

(define (frame-get-buffer-by-file file)
   (frame-find-buffer
      (lambda (b)
         (equal? file (buffer-filename b)))))

(define (frame-get-buffer name)
   (frame-find-buffer
      (lambda (b)
         (equal? name (buffer-name b)))))

(define (frame-get-or-create-buffer name)
   (or (frame-get-buffer name)
       (buffer-create name)))

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
