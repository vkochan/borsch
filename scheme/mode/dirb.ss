(define dirb-open-dir
   (lambda (cwd)
      (let (
            [trail-sep? (eq? (string-length (path-last cwd)) 0)]
            [dl '()]
            [fl '()]
           )
         (if trail-sep?
            (set-local! current-cwd (path-parent cwd))
            ;; else
            (set-local! current-cwd cwd)
         )
         (buffer-set-name cwd)
	 (erase-buffer)
         (for-each
            (lambda (e)
               (when (not (and (equal? (string-ref e 0) #\.)
                               (not (get-local show-hidden))))
                  (if (file-directory? (fmt "~a/~a" cwd e))
                     (set! dl (append dl (list e)))
                  )
                  (if (file-regular? (fmt "~a/~a" cwd e))
                     (set! fl (append fl (list e)))
                  )
              )
            ) (directory-list cwd)
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
	 (dirb-move-begin)
      )
   )
)

(define dirb
   (case-lambda
      [()
       (dirb (view-cwd))
      ]

      [(cwd)
       (let ([b (buffer-create)])
          (with-buffer b
             (define-local current-cwd cwd)
             (define-local show-hidden #f)
             (buffer-set-keymap 'dirb-map)
             (dirb-open-dir cwd)
          )
       )
      ]
   )
)

(define dirb-move-line-down
    (lambda ()
       (highlight-clear)
       (move-line-down)
       (highlight-range
          (line-begin-pos)
          (line-end-pos))
    )
)

(define dirb-move-line-up
    (lambda ()
       (highlight-clear)
       (move-line-up)
       (highlight-range
          (line-begin-pos)
          (line-end-pos))
    )
)

(define dirb-move-begin
   (lambda ()
      (highlight-clear)
      (move-buffer-begin)
      (highlight-range
         (line-begin-pos)
         (line-end-pos))
   )
)

(define dirb-move-end
   (lambda ()
      (highlight-clear)
      (move-buffer-end)
      (highlight-range
         (line-begin-pos)
         (line-end-pos))
   )
)

(define dirb-open-parent
    (lambda ()
       (let ([p (path-parent (get-local current-cwd))])
          (if (> (string-length p) 0)
             (dirb-open-dir p)
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
             (dirb-open-dir p)
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

(define dirb-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" dirb-open-entry)
      (bind-key map "h" dirb-open-parent)
      (bind-key map "j" dirb-move-line-down)
      (bind-key map "k" dirb-move-line-up)
      (bind-key map "l" dirb-open-entry)
      (bind-key map "g g" dirb-move-begin)
      (bind-key map "G" dirb-move-end)
      (bind-key map "." dirb-show-hidden)
      map
   )
)

(define-mode dirb-mode "Dirb" #f
   #f
)
