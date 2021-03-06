(define grep-fmt-cmd
   (lambda (word dir)
      (let ([cmd (format "grep -H -rnI --exclude-dir=.git ~a ~a" word dir)])
         cmd
      )
   )
)

(define grep-reload-buffer
   (lambda ()
      (grep (get-local grep-search-word)
            (get-local grep-search-dir)
            (current-buffer))
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
       (let ([cwd (get-local current-dir (current-cwd))])
          (grep s cwd)
       )
      ]

      [(s d)
       (let ([cwd (get-local current-dir (current-cwd))])
          (grep s cwd (buffer-create-text))
       )
      ]

      [(s d b)
       (let (
             [cmd (grep-fmt-cmd s d)]
            )
          (with-current-buffer b
             (define-local buffer-reload-func grep-reload-buffer)
             (define-local major-mode 'grep-mode)
             (define-local grep-search-word s)
             (define-local grep-search-dir d)
             (buffer-set-name (format "Search: ~a" s))
             (buffer-set-mode-name "Grep")
             (erase-buffer)
          )
          (process-create cmd b)
          #t
       )
      ]
   )
)
