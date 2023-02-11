(define git-exists?
   (lambda ()
      (delay (program-exists? "git"))
   )
)

(define git-branch-name
   (lambda ()
      (let (
            [out (process-get-output (format "git -C ~a rev-parse --abbrev-ref HEAD 2> /dev/null" (current-cwd)))]
           )
         (string-remove-nl (list-ref out 1))
      )
   )
)

(define git-short-status-string
   (lambda ()
      (let (
            [out (process-get-output (format "git -C ~a status 2> /dev/null" (current-cwd)))]
            [branch (git-branch-name)]
            [status ""]
           )
         (when (string-contains? (list-ref out 1) "modified")
            (set! status (format "~a+" status))
         )
         (when (string-contains? (list-ref out 1) "ahead")
            (set! status (format "~a>" status))
         )
         (when (string-contains? (list-ref out 1) "behind")
            (set! status (format "~a<" status))
         )
         (when (string-contains? (list-ref out 1) "diverged")
            (set! status (format "~a!" status))
         )
         (if (equal? status "")
            branch
            ;; else
            (format "~a ~a" branch status)
         )
      )
   )
)

(define git-cmd-format
   (lambda (cmd)
      (format "git -C ~a ~a" (current-cwd) cmd)
   )
)

(define git-repo?
   (lambda (path)
      (let ([ret (process-get-output (git-cmd-format (format "ls-files --error-unmatch ~a" path)))])
         (let ([status (list-ref ret 0)] [output (list-ref ret 1)])
            (eq? 0 status)
         )
      )
   )
)

(define git-cmd-read
   (lambda (cmd)
      (let ([ret (process-get-output (git-cmd-format cmd))])
         (let ([status (list-ref ret 0)] [output (list-ref ret 1)])
            (when (not (eq? 0 status))
               (message output)
            )
            output
         )
      )
   )
)

(define git-cmd-list
   (lambda (cmd)
      (string-split (git-cmd-read cmd) #\nul)
   )
)

(define git-checkout-branch
   (lambda (b)
      (git-cmd-read (format "checkout ~a" b))
      (if (not (equal? b (git-branch-name)))
         (message (format "could not switch to ~a" b))
      )
   )
)

(define git-branch-list
   (case-lambda
      [()
       (git-branch-list "")
      ]

      [(opts)
       (let ([ret (git-cmd-read (format "branch ~a" opts))])
          (let ([ls (string-split ret #\newline)])
             (let ([blist (list)])
                (for-each
                   (lambda (b)
                      (let ([bs (string-split b #\ )])
                         ;; handle '->'
                         (if (< (length bs) 2)
                            ;; handle '*'
                            (if (> (length bs) 1)
                               (set! blist (append blist (list (list-ref bs 1))))
                               ;; else
                               (set! blist (append blist (list (list-ref bs 0))))
                            )
                         )
                      )
                   ) ls
                )
                blist
             )
          )
       )
      ]
   )
)
