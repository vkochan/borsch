;; C-g p c - configure
;; C-g p C - configure with params
;; C-g p b - build
;; C-g p B - build with params
;; C-g p o - open existing project (and close the current one)
;; C-g p r - deploy project

(define-record-type project (fields name cwd (mutable inherite) (mutable xxx)))

(define-syntax define-project
   (lambda (stx)
      (syntax-case stx ()
         [(_ name cwd)
          #`(let ([proj (make-project (symbol->string 'name) cwd #f)])
               (define-top-level-value 'name proj)
            )
         ]
         [(_ name cwd rest ...)
          (let loop ([opts #'(rest ...)] [fields '()])
             (syntax-case opts (inherit xxx)
                [((xxx x) rest-opts ...)
                 (loop #'(rest-opts ...)
                       (append fields
                               (list #'(project-xxx-set! proj x)))
                 )
                ]
                [((inherit inh) rest-opts ...)
                 (loop #'(rest-opts ...)
                       (append fields
                               (list #'(project-inherite-set! proj inh)))
                 )
                ]
                [()
                 #`(let ([proj (make-project (symbol->string 'name) cwd #f #f)])
                      #,@fields
                      (define-top-level-value 'name proj)
                   )
                ]
              )
           )
         ]
      )
   )
)

(define-project proj-xxx "/home"
   (xxx 'yyy)
   (inherit 'make)
)

;;(project-inherite proj-xxx)
;;(project-xxx proj-xxx)
;;(define-syntax my-syntax
;;   (lambda (stx)
;;      (syntax-case stx ()
;;         [(_ v)
;;            #`(begin
;;                 #'#,(map
;;                      (lambda (x)
;;                         x
;;                      ) (list 1 2 3)
;;                   )
;;              )
;;         ]
;;      )
;;   )
;;)

;;(define xxx 1)
;;(my-syntax xxx)
proj-xxx
