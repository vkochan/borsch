(define __cs_frame_current_set (foreign-procedure __collect_safe "cs_frame_current_set" (int) void))
(define __cs_frame_create (foreign-procedure __collect_safe "cs_frame_create" () scheme-object))
(define __cs_frame_delete (foreign-procedure __collect_safe "cs_frame_delete" (int) void))

(define frame-ht (make-eq-hashtable))

(define (*frame-create*)
   (call-foreign (__cs_frame_create))
)

(define (frame-delete fid)
   (call-foreign (__cs_frame_delete fid))
)

(define frame-create
   (lambda (name)
      (let (
            [fr (make-eq-hashtable)]
            [id (*frame-create*)]
           )
         (hashtable-set! fr 'cwd (current-directory))
         (hashtable-set! fr 'prev-layout #f)
         (hashtable-set! fr 'name name)
         (hashtable-set! fr 'id id)
         (hashtable-set! frame-ht id fr)   
         fr
      )
   )
)

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

(define current-frame
   (lambda ()
      *current-frame*
   )
)

(define-syntax (frame-set-var! stx)
   (syntax-case stx ()
      ((_ f s v)
         #`(hashtable-set! f 's v)
      )
   )
)

(define-syntax (frame-get-var stx)
   (syntax-case stx ()
      ((_ f s)
         #`(hashtable-ref f 's #f)
      )
   )
)

(define-syntax (with-current-frame stx)
   (syntax-case stx ()
      ((_ frame exp ...)
       #`(let (
               [from (current-frame)]
               [to frame]
              )
            (frame-switch to)
            (begin
               exp
               ...
            )
            (frame-switch from)
         )
      )
   )
)

(define frame-switch
   (lambda (fr)
      (set! *current-frame* fr)
      (call-foreign (__cs_frame_current_set (frame-id fr)))
      (run-hooks 'frame-switch-hook fr)
   )
)

(define frame-switch-1
   (lambda ()
      (frame-switch frame-1)
   )
)

(define frame-switch-2
   (lambda ()
      (frame-switch frame-2)
   )
)

(define frame-switch-3
   (lambda ()
      (frame-switch frame-3)
   )
)

(define frame-switch-4
   (lambda ()
      (frame-switch frame-4)
   )
)

(define frame-switch-5
   (lambda ()
      (frame-switch frame-5)
   )
)

(define frame-switch-6
   (lambda ()
      (frame-switch frame-6)
   )
)

(define frame-switch-7
   (lambda ()
      (frame-switch frame-7)
   )
)

(define frame-switch-8
   (lambda ()
      (frame-switch frame-8)
   )
)

(define frame-switch-9
   (lambda ()
      (frame-switch frame-9)
   )
)

#;(define frame-switch-all
   (lambda ()
      (frame-switch frame-all)
   )
)

(define frame-id
   (case-lambda
      [()
       (frame-id (current-frame))]

      [(fr)
       (frame-get-var fr id)]
   )
)

(define frame-name
   (case-lambda
      [()
       (frame-name (current-frame))]

      [(fr)
       (frame-get-var fr name)]
   )
)

(define frame-set-name
   (case-lambda
      [(name)
       (frame-set-name (current-frame) name)
      ]

      [(fr n)
       (frame-set-var! fr name n)
       (run-hooks 'change-frame-name-hook)
      ]
   )
)

(define frame-cwd
   (case-lambda
      [()
       (frame-cwd (current-frame))]

      [(fr)
       (frame-get-var fr cwd)]
   )
)

(define frame-set-cwd
   (case-lambda
      [(cwd)
       (frame-set-cwd (current-frame) cwd)]

      [(fr c)
       (frame-set-var! fr cwd c)
       (run-hooks 'change-cwd-hook)
      ]
   )
)

(define frame-prev-layout
   (case-lambda
      [()
       (frame-prev-layout (current-frame))]

      [(fr)
       (frame-get-var fr prev-layout)]
   )
)

(define frame-set-prev-layout
   (case-lambda
      [(l)
       (frame-set-prev-layout (current-frame) l)]

      [(fr l)
       (frame-set-var! fr prev-layout l)]
   )
)
