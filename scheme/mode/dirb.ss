(define dirb-get-entry
   (lambda (dir)
      (let (
            [trail-sep? (eq? (string-length (path-last dir)) 0)]
           )
         (if trail-sep?
            (path-parent dir)
            ;; else
            dir
         )
      )
   )
)

(define dirb-ls
   (lambda (dir)
      (let ([opts (if (get-local show-hidden) "-a" "")])
         (string-split
            (process-read (format "ls -1 ~a ~a" opts dir))
            #\newline)
      )
   )
)

(define dirb-open-dir
   (lambda (cwd)
      (let (
            [dir (dirb-get-entry cwd)]
            [dl '()]
            [fl '()]
           )
         (buffer-set-readonly #f)
         (set-local! current-cwd dir)
         (buffer-set-name dir)
	 (erase-buffer)
         (for-each
            (lambda (e)
               (when (not (and (equal? (string-ref e 0) #\.)
                               (not (get-local show-hidden))))
                  (if (file-directory? (fmt "~a/~a" dir e))
                     (set! dl (append dl (list e)))
                  )
                  (if (file-regular? (fmt "~a/~a" dir e))
                     (set! fl (append fl (list e)))
                  )
              )
            ) (dirb-ls dir)
         )
         (for-each
            (lambda (d)
               (insert (fmt "~a/\n" d) '(style (:attr "bold")))
            ) dl
         )
         (for-each
            (lambda (f)
               (insert (fmt "~a\n" f))
            ) fl
         )
         (move-buffer-begin)
         (buffer-set-readonly #t)
      )
   )
)

(define dirb-open-parent
    (lambda ()
       (let (
             [p (path-parent (get-local current-cwd))]
             [s (get-local prev-cursor)]
            )
          (when (> (string-length p) 0)
             (dirb-open-dir p)
             (when (not (stack-empty? s))
                (cursor-set (stack-pop! s))
             )
          )
       )
    )
)

(define dirb-open-entry
    (lambda ()
       (let* (
               [e (extract-line-inner)]
               [p (fmt "~a/~a" (get-local current-cwd) e)] 
             )
          (if (file-directory? p)
             (let ([s (get-local prev-cursor)])
                (stack-push! s (cursor))
                (dirb-open-dir p)
             )
          )
          (if (file-regular? p)
             (let ([b (buffer-create)])
                (with-buffer b
                   (text-mode)
                   (buffer-open-file p)
                )
             )
          )
       )
    )
)

(define dirb-show-hidden
   (lambda ()
      (set-local! show-hidden
         (not (get-local show-hidden)))
      (dirb-open-dir (get-local current-cwd))
   )
)

(define dirb-goto-home
   (lambda ()
      (dirb-open-dir "~")
   )
)

(define dirb-goto-cwd
   (lambda ()
      (dirb-open-dir (view-cwd))
   )
)

(define dirb-set-cwd
   (lambda ()
      (view-set-cwd (get-local current-cwd))
      (dirb-open-dir (view-cwd))
   )
)

(define dirb-create-new-file
   (lambda ()
      (minibuf-read "new file:"
         (lambda (f)
            (let ([p (open-output-file (string-append (get-local current-cwd) "/" f))])
               (close-port p)
            )
            (dirb-open-dir (get-local current-cwd))
         )
      )
   )
)

(define dirb-create-new-dir
   (lambda ()
      (minibuf-read "new dir:"
         (lambda (f)
            (mkdir (string-append (get-local current-cwd) "/" f))
            (dirb-open-dir (get-local current-cwd))
         )
      )
   )
)

(define dirb-delete-entry
   (lambda ()
      (minibuf-ask "Delete entry(s) ?"
         (lambda (v)
            (let*(
                  [e (extract-line-inner)]
                  [p (string-append (get-local current-cwd) "/" e)]
                 )
               (when (eq? v 'yes)
                  (rm-rf p)
                  (dirb-open-dir (get-local current-cwd))
               )
            )
         )
      )
   )
)

(define dirb-rename-entry
   (lambda ()
      (let ([entry (dirb-get-entry (extract-line-inner))])
         (set-local! defval entry)
         (minibuf-read "rename:" entry
            (lambda (v)
               (let (
                     [old (string-append (get-local current-cwd) "/" (get-local defval))]
                     [new (string-append (get-local current-cwd) "/" v)]
                    )
                  (rename-file old new)
                  (dirb-open-dir (get-local current-cwd))
               )
            )
         )
      )
   )
)

(define dirb-grep
   (lambda ()
      (grep)
   )
)

(define dirb-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" dirb-open-entry)
      (bind-key map "h" dirb-open-parent)
      (bind-key map "l" dirb-open-entry)
      (bind-key map "." dirb-show-hidden)
      (bind-key map "~" dirb-goto-home)
      (bind-key map "w" dirb-goto-cwd)
      (bind-key map "W" dirb-set-cwd)
      (bind-key map "^" dirb-create-new-file)
      (bind-key map "+" dirb-create-new-dir)
      (bind-key map "d" dirb-delete-entry)
      (bind-key map "r" dirb-rename-entry)
      (bind-key map "s" dirb-grep)
      map
   )
)

(define-mode dirb-mode "Dirb" text-mode
   (define-local current-cwd (view-cwd))
   (define-local show-hidden #f)
   (define-local prev-cursor (make-stack))
   (define-local defval #f)
   (set-local! linenum-enable #f)
)

(define dirb
   (case-lambda
      [()
       (dirb (view-cwd))
      ]

      [(cwd)
       (let ([b (buffer-create)])
          (with-buffer b
             (buffer-set-readonly #t)
             (dirb-mode)
             (set-local! current-cwd cwd)
             (dirb-open-dir cwd)
             (stack-push! (get-local prev-cursor) (cursor))
          )
       )
      ]
   )
)
