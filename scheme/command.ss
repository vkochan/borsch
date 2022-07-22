(define-record-type command (fields name func))

(define command-list '())

(define find-command
   (lambda (name)
      (let ()
         (call/cc
            (lambda (ret)
               (for-each
                  (lambda (c)
                     (when (equal? (command-name c) name)
                        (ret c)
                     )
                  ) command-list
               )
               (ret #f)
            )
         )
      )
   )
)

(define add-command
   (lambda (name func)
      (let (
            [cmd (make-command name func)]
           )
         (remove-command name)
         (set! command-list (append command-list (list cmd)))
      )
   )
)

(define match-command
   (lambda (name)
      (let ()
         (filter
            (lambda (c)
               (string-contains? (command-name c) name)
            ) command-list
         )
      )
   )
)

(define remove-command
   (lambda (name)
      (let (
            [cmd (find-command name)]
           )
         (when cmd
            (set! command-list (remove cmd command-list))
         )
      )
   )
)
