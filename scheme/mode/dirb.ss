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

(define dirb-current-dir
   (lambda ()
      (get-local current-dir)
   )
)

(define dirb-set-current-dir
   (lambda (dir)
      (set-local! current-dir dir)
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
         (dirb-set-current-dir dir)
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
             [p (path-parent (dirb-current-dir))]
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

(define dirb-entry-path
    (case-lambda 
       [()
        (dirb-entry-path (cursor))
       ]

       [(pos)
        (let ([e (extract-line-inner pos)])
           (dirb-get-entry (fmt "~a/~a" (dirb-current-dir) e))
        )
       ]
    )
)

(define dirb-open-entry
    (lambda ()
       (let* (
               [e (extract-line-inner)]
               [p (fmt "~a/~a" (dirb-current-dir) e)] 
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
      (dirb-open-dir (dirb-current-dir))
   )
)

(define dirb-goto-home
   (lambda ()
      (dirb-open-dir (getenv "HOME"))
   )
)

(define dirb-goto-cwd
   (lambda ()
      (dirb-open-dir (view-cwd))
   )
)

(define dirb-set-cwd
   (lambda ()
      (view-set-cwd (dirb-current-dir))
      (dirb-open-dir (view-cwd))
   )
)

(define dirb-list-selection
   (lambda ()
      (get-local selected)
   )
)

(define dirb-create-new-file
   (lambda ()
      (minibuf-read "new file:"
         (lambda (f)
            (let ([p (open-output-file (string-append (dirb-current-dir) "/" f))])
               (close-port p)
            )
            (dirb-open-dir (dirb-current-dir))
         )
      )
   )
)

(define dirb-create-new-dir
   (lambda ()
      (minibuf-read "new dir:"
         (lambda (f)
            (mkdir (string-append (dirb-current-dir) "/" f))
            (dirb-open-dir (dirb-current-dir))
         )
      )
   )
)

(define dirb-delete-entry-cursor
   (lambda ()
      (minibuf-ask "Delete entry(s) ?"
         (lambda (v)
            (let*(
                  [e (extract-line-inner)]
                  [p (string-append (dirb-current-dir) "/" e)]
                 )
               (when (eq? v 'yes)
                  (rm-rf p)
                  (dirb-open-dir (dirb-current-dir))
               )
            )
         )
      )
   )
)

(define dirb-delete-selection
   (lambda ()
      (minibuf-ask (format "Delete selected (~d) entry(s) ?" (length (dirb-list-selection)))
         (lambda (v)
            (when (eq? v 'yes)
               (for-each
                  (lambda (p)
                     (rm-rf p)
                  )
                  (dirb-list-selection)
               )
               (dirb-open-dir (dirb-current-dir))
            )
         )
      )
   )
)

(define dirb-delete-entry
   (lambda ()
      (if (> (length (dirb-list-selection)) 0)
         (dirb-delete-selection)
         ;; else
         (dirb-delete-entry-cursor)
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
                     [old (string-append (dirb-current-dir) "/" (get-local defval))]
                     [new (string-append (dirb-current-dir) "/" v)]
                    )
                  (rename-file old new)
                  (dirb-open-dir (dirb-current-dir))
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

(define dirb-draw-selection
   (lambda (w)
      (let ([slist (get-local selected)])
         (if (> (length slist) 0)
            (window-set-sidebar-width w 2)
            ;; else
            (window-set-sidebar-width w 0)
         )
         (when (> (length slist) 0)
            (let ([lines (window-viewport-lines-coord w)])
               (for-each
                  (lambda (c)
                     (let (
                           [line-pos (list-ref c 3)]
                           [line-y (list-ref c 1)]
                          )
                        (let ([path (dirb-entry-path line-pos)])
                           (if (member path slist)
                              (window-draw-sidebar w 0 line-y "*")
                              ;; else
                              (window-draw-sidebar w 0 line-y " ")
                           )
                        )
                     )
                  ) lines
               )
            )
         )
      )
   )
)

(define dirb-select-entry
   (lambda ()
      (let (
            [slist (get-local selected)]
            [path (dirb-entry-path)]
           )
         (if (member path slist)
            (set! slist (remove path slist))
            ;; else
            (set! slist (append slist (list path)))
         )
         (set-local! selected slist)
         (dirb-draw-selection (current-window))
      )
   )
)

(define dirb-clear-selection
   (lambda ()
      (set-local! selected (list))
      (dirb-draw-selection (current-window))
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
      (bind-key map "<Space>" dirb-select-entry)
      (bind-key map "<Esc>" dirb-clear-selection)
      map
   )
)

(define-mode dirb-mode "Dirb" text-mode
   (define-local current-dir (view-cwd))
   (define-local show-hidden #f)
   (define-local prev-cursor (make-stack))
   (define-local selected (list))
   (define-local window-draw-hook dirb-draw-selection)
   (define-local defval #f)
   (set-local! linenum-enable #f)
)

(define dirb
   (case-lambda
      [()
       (dirb (view-cwd))
      ]

      [(dir)
       (let ([b (buffer-create)])
          (with-buffer b
             (buffer-set-readonly #t)
             (dirb-mode)
             (set-local! current-dir dir)
             (dirb-open-dir dir)
             (stack-push! (get-local prev-cursor) (cursor))
          )
       )
      ]
   )
)
