(define dirb-dir-style '(fg: "blue" attr: "bold"))
(define dirb-file-executable-style '(fg: "yellow" attr: "bold"))
(define dirb-file-regular-style '(fg: "white"))
(define dirb-external-opener "xdg-open")

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

(define dirb-insert-entry
   (lambda (dir path style)
      (let (
            [slist (dirb-get-selection)]
            [entry path]
           )
         (if (file-directory? (fmt "~a/~a" dir path))
            (set! entry (fmt "~a/\n" path))
         )
         (if (file-regular? (fmt "~a/~a" dir path))
            (set! entry (fmt "~a\n" path))
         )
         (when (member (fmt "~a/~a" dir path) slist)
            (set! style (plist-put style 'bg: "blue"))
         )
         (text-insert entry `(style: ,style))
      )
   )
)

(define dirb-open-dir
   (lambda (cwd)
      (let (
            [slist (dirb-get-selection)]
            [dir (dirb-get-entry cwd)]
            [dl '()]
            [fl '()]
           )
         (buffer-set-readonly #f)
         (dirb-set-current-dir dir)
         (buffer-set-name dir)
	 (text-delete)
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
               (dirb-insert-entry dir d dirb-dir-style)
            ) dl
         )
         (for-each
            (lambda (f)
               (if (= 0 (bitwise-and #o100 (get-mode (fmt "~a/~a" dir f))))
                  (dirb-insert-entry dir f dirb-file-regular-style)
                  ;; else
                  (dirb-insert-entry dir f dirb-file-executable-style)
               )
            ) fl
         )
         (cursor-goto-begin)
         (buffer-set-readonly #t)
         (if (> (length slist) 0)
            (buffer-set-state-name (format "[sel:~a]" (length slist)))
            ;; else
            (buffer-set-state-name "")
         )
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

(define dirb-next-entry
    (lambda ()
       (cursor-goto-line-down)
       (cursor-goto-line-begin)
    )
)

(define dirb-prev-entry
    (lambda ()
       (cursor-goto-line-up)
       (cursor-goto-line-begin)
    )
)

(define dirb-entry-path
    (case-lambda 
       [()
        (dirb-entry-path (cursor))
       ]

       [(pos)
        (let ([e (text-line-inner pos)])
           (dirb-get-entry (fmt "~a/~a" (dirb-current-dir) e))
        )
       ]
    )
)

(define dirb-open-entry
    (lambda ()
       (let* (
               [e (text-line-inner)]
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
      (dirb-open-dir (current-cwd))
   )
)

(define dirb-set-cwd
   (lambda ()
      (frame-set-cwd (dirb-current-dir))
      (dirb-open-dir (current-cwd))
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
      (dirb-reload)
   )
)

(define dirb-delete-entry-cursor
   (lambda ()
      (minibuf-ask "Delete entry(s) ?"
         (lambda (v)
            (let*(
                  [e (text-line-inner)]
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
      (let ([entry (dirb-get-entry (text-line-inner))])
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

(define dirb-select-entry
   (lambda ()
      (let (
            [prop (first (get-text-property 'style: (text-line-begin-pos) (text-line-end-pos)))]
            [style '()]
            [slist (dirb-get-selection)]
            [path (dirb-entry-path)]
           )
         (if (member path slist)
            (let ()
               (set! style (plist-put (plist-get prop 'style:) 'bg: "default"))
               (set! slist (remove path slist))
            )
            ;; else
            (let ()
               (set! style (plist-put (plist-get prop 'style:) 'bg: "blue"))
               (set! slist (append slist (list path)))
            )
         )
         (set-text-property (plist-get prop 'start:) (plist-get prop 'end:)
            `(style: ,style)
         )
         (dirb-set-selection slist)
         (if (> (length slist) 0)
            (buffer-set-state-name (format "[sel:~a]" (length slist)))
            ;; else
            (buffer-set-state-name "")
         )
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

(define dirb-open-entry-externally
   (lambda ()
      (let ([path (dirb-entry-path)])
         (system (format "~a ~s" dirb-external-opener path))
      )
   )
)

(define dirb-find-file
   (lambda ()
      (minibuf-find-file (dirb-current-dir) "-type f")
   )
)

(define dirb-find-dir
   (lambda ()
      (minibuf-find-file (dirb-current-dir) "-type d")
   )
)

(define dirb-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" dirb-open-entry)
      (bind-key map "h" dirb-open-parent)
      (bind-key map "j" dirb-next-entry)
      (bind-key map "k" dirb-prev-entry)
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
      (bind-key map "`" (lambda () (vterm #f "" (dirb-current-dir))))
      (bind-key map "c" dirb-create-tar-archive)
      (bind-key map "x" dirb-extract-tar-archive)
      (bind-key map "o" dirb-open-entry-externally)
      (bind-key map "f f" dirb-find-file)
      (bind-key map "f d" dirb-find-dir)
      map
   )
)

(define-mode dirb-mode "Dirb" text-mode
   (define-local current-dir (current-cwd))
   (define-local show-hidden #f)
   (define-local prev-cursor (make-stack))
   (define-local defval #f)
   (define-local buffer-reload-func dirb-reload)
   (set-local! linenum-enable #f)
)

(define dirb
   (case-lambda
      [()
       (dirb (current-cwd))
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

(bind-key text-mode-normal-map "g d" (lambda () (dirb (path-parent (buffer-filename)))))
