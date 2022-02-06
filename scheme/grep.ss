(define grep-fmt-cmd
   (lambda (word dir)
      (let ([cmd (format "grep -H -rnI --exclude-dir=.git ~a ~a" word dir)])
         cmd
      )
   )
)

(define grep
   (case-lambda
      [()
       (minibuf-read "Search:"
          (lambda (s)
             (grep s)
          )
       )
      ]

      [(s)
       (grep s (current-cwd))]

      [(s d)
       (let (
             [cmd (grep-fmt-cmd s d)]
             [b (buffer-create)]
            )
          (with-buffer b
             (buffer-set-name (format "Grep: ~a" s))
             (text-mode)
          )
          (process-start b cmd)
          #t
       )
      ]
   )
)
