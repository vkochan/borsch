(define dirb-dir-style '(:fg "blue" :attr "bold"))
(define dirb-file-executable-style '(:fg "yellow" :attr "bold"))
(define dirb-file-regular-style '(:fg "white"))

(define dirb-selection-list (list))

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
            (list-ref (process-get-output (format "ls -1 ~a ~a" opts dir)) 1)
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
               (insert (fmt "~a/\n" d) `(style ,dirb-dir-style))
            ) dl
         )
         (for-each
            (lambda (f)
               (if (= 0 (bitwise-and #o100 (get-mode (fmt "~a/~a" dir f))))
                  (insert (fmt "~a\n" f) `(style ,dirb-file-regular-style))
                  ;; else
                  (insert (fmt "~a\n" f) `(style ,dirb-file-executable-style))
               )
            ) fl
         )
         (move-buffer-begin)
         (buffer-set-readonly #t)
      )
   )
)

(define dirb-reload
   (lambda ()
      (save-cursor (dirb-open-dir (dirb-current-dir)))
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
                (with-current-buffer b
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
      (dirb-reload)
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

(define dirb-get-selection
   (lambda ()
      dirb-selection-list
   )
)

(define dirb-set-selection
   (lambda (v)
      (set! dirb-selection-list v)
   )
)

(define dirb-create-file
   (lambda ()
      (minibuf-read "new file:"
         (lambda (f)
            (let ([p (open-output-file (string-append (dirb-current-dir) "/" f))])
               (close-port p)
            )
            (dirb-reload)
         )
      )
   )
)

(define dirb-create-dir
   (lambda ()
      (minibuf-read "new dir:"
         (lambda (f)
            (mkdir (string-append (dirb-current-dir) "/" f))
            (dirb-reload)
         )
      )
   )
)

(define dirb-clear-selection
   (lambda ()
      (dirb-set-selection (list))
      (dirb-draw-selection (current-window))
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
                  (dirb-reload)
               )
            )
         )
      )
   )
)

(define dirb-paste-selection
   (lambda ()
      (let ([count (length (dirb-get-selection))])
         (if (> count 0)
            (begin
               (if (> count 1)
                  (for-each
                     (lambda (p)
                        (system (format "cp -r ~a ~a" p (dirb-current-dir)))
                     )
                     (dirb-get-selection)
                  )
                  ;; else - single file, check if copy to same dir
                  (let ([p (first (dirb-get-selection))])
                     (if (equal? (path-parent p) (dirb-current-dir))
                        (begin
                           (set-local! defval (path-last p))
                           (minibuf-read "rename:" (path-last p)
                              (lambda (v)
                                 (let (
                                       [old (string-append (dirb-current-dir) "/" (get-local defval))]
                                       [new (string-append (dirb-current-dir) "/" v)]
                                      )
                                    (system (format "cp -r ~a ~a" old new))
                                    (dirb-reload)
                                    (dirb-clear-selection)
                                 )
                              )
                           )
                        )
                        ;; else
                        (begin
                           (dirb-reload)
                           (dirb-clear-selection)
                           (system (format "cp -r ~a ~a" p (dirb-current-dir)))
                           (message (format "~d files were copied" count))
                        )
                     )
                  )
               )
               (dirb-reload)
               (dirb-clear-selection)
               (message (format "~d files were copied" count))
            )
            ;; else
            (begin
               (message "No files were selected")
            )
         )
      )
   )
)

(define dirb-move-selection
   (lambda ()

      (define has-same-dest?
         (lambda ()
            (exists
               (lambda (p)
                  (equal? (path-parent p) (dirb-current-dir))
               )
               (dirb-get-selection)
            )
         )
      )
      
      (let ([count (length (dirb-get-selection))])
         (if (> count 0)
            (begin
               (if (not (has-same-dest?))
                  (begin
                     (for-each
                        (lambda (p)
                           (let (
                                 [dest (format "~a/~a" (dirb-current-dir) (path-last p))]
                                )
                              (system (format "mv ~a ~a" p dest))
                           )
                        )
                        (dirb-get-selection)
                     )
                     (dirb-open-dir (dirb-current-dir))
                     (dirb-clear-selection)
                     (message (format "~d entries were moved" count))
                  )
                  ;; else
                  (begin
                     (message "Could not move entries with same destination")
                  )
               )
            )
            ;; else
            (begin
               (message "No entries were selected")
            )
         )
      )
   )
)

(define dirb-delete-selection
   (lambda ()
      (minibuf-ask (format "Delete selected (~d) entry(s) ?" (length (dirb-get-selection)))
         (lambda (v)
            (when (eq? v 'yes)
               (for-each
                  (lambda (p)
                     (rm-rf p)
                  )
                  (dirb-get-selection)
               )
               (dirb-clear-selection)
               (dirb-reload)
            )
         )
      )
   )
)

(define dirb-copy-path-selection
   (lambda ()
      (let ([count (length (dirb-get-selection))])
         (if (> count 0)
            (begin
               (copybuf-copy "")
               (for-each
                  (lambda (p)
                     (copybuf-append (format (if (= count 1) "~a" "~a\n") p))
                  )
                  (dirb-get-selection)
               )
               (dirb-clear-selection)
            )
            ;; else
            (begin
               (message "No files were selected")
            )
         )
      )
   )
)

(define dirb-delete-entry
   (lambda ()
      (if (> (length (dirb-get-selection)) 0)
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
                  (dirb-reload)
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
      (let ([slist (dirb-get-selection)])
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
            [slist (dirb-get-selection)]
            [path (dirb-entry-path)]
           )
         (if (member path slist)
            (set! slist (remove path slist))
            ;; else
            (set! slist (append slist (list path)))
         )
         (dirb-set-selection slist)
         (dirb-draw-selection (current-window))
      )
   )
)

(define dirb-create-tar-archive
   (lambda ()
      (let ([path (dirb-entry-path)])
         (system (format "tar -czf ~a.tar.gz -C ~a ~a" path (dirb-current-dir) (path-last path)))
         (dirb-reload)
      )
   )
)

(define dirb-extract-tar-archive
   (lambda ()
      (let* (
             [path (dirb-entry-path)]
             [match (pregexp-match ".*\\.(tar)$|.*\\.(tar\\.gz)$|.*\\.(tar\\.bz2)$" path)]
            )
         (if match
            (begin
               (system (format "tar -xf ~a -C ~a" path (dirb-current-dir)))
               (dirb-reload)
            )
            ;; else
            (message "file archive is not supported")
         )
      )
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
      (bind-key map "^" dirb-create-file)
      (bind-key map "+" dirb-create-dir)
      (bind-key map "p" dirb-paste-selection)
      (bind-key map "m" dirb-move-selection)
      (bind-key map "y" dirb-copy-path-selection)
      (bind-key map "d" dirb-delete-entry)
      (bind-key map "r" dirb-rename-entry)
      (bind-key map "s" dirb-grep)
      (bind-key map "<Space>" dirb-select-entry)
      (bind-key map "<Esc>" dirb-clear-selection)
      (bind-key map "`" (lambda () (term #f "" (dirb-current-dir))))
      (bind-key map "c" dirb-create-tar-archive)
      (bind-key map "x" dirb-extract-tar-archive)
      map
   )
)

(define-mode dirb-mode "Dirb" text-mode
   (define-local current-dir (view-cwd))
   (define-local show-hidden #f)
   (define-local prev-cursor (make-stack))
   (define-local window-draw-hook dirb-draw-selection)
   (define-local defval #f)
   (define-local buffer-reload-func dirb-reload)
   (set-local! linenum-enable #f)
)

(define dirb
   (case-lambda
      [()
       (dirb (view-cwd))
      ]

      [(dir)
       (let ([b (buffer-create)])
          (with-current-buffer b
             (buffer-set-readonly #t)
             (dirb-mode)
             (dirb-set-current-dir dir)
             (dirb-open-dir dir)
             (stack-push! (get-local prev-cursor) (cursor))
          )
       )
      ]
   )
)
