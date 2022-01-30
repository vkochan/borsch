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

(define dirb-open-dir
   (lambda (cwd)
      (let (
            [dir (dirb-get-entry cwd)]
            [dl '()]
            [fl '()]
           )
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
            ) (directory-list dir)
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
             (define-local defval #f)
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

(define dirb-scroll-halfpage-up
  (lambda ()
    (highlight-clear)
    (cursor-set (window-scroll-halfpage-up))
    (highlight-range
       (line-begin-pos)
       (line-end-pos))
  )
)

(define dirb-scroll-halfpage-down
  (lambda ()
    (highlight-clear)
    (cursor-set (window-scroll-halfpage-down))
    (highlight-range
       (line-begin-pos)
       (line-end-pos))
  )
)

(define dirb-scroll-page-down
  (lambda ()
    (highlight-clear)
    (cursor-set (window-scroll-page-down))
    (highlight-range
       (line-begin-pos)
       (line-end-pos))
  )
)

(define dirb-scroll-page-up
  (lambda ()
    (highlight-clear)
    (cursor-set (window-scroll-page-up))
    (highlight-range
       (line-begin-pos)
       (line-end-pos))
  )
)

(define dirb-viewport-begin
  (lambda ()
    (highlight-clear)
    (cursor-set (window-viewport-begin))
    (highlight-range
       (line-begin-pos)
       (line-end-pos))
  )
)

(define dirb-viewport-end
  (lambda ()
    (highlight-clear)
    (cursor-set (window-viewport-end))
    (highlight-range
       (line-begin-pos)
       (line-end-pos))
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

(define dirb-enter-new-file
   (lambda (b f)
      (with-buffer b
         (let ([p (open-output-file (string-append (get-local current-cwd) "/" f))])
	    (close-port p)
         )
         (dirb-open-dir (get-local current-cwd))
      )
   )
)

(define dirb-create-new-file
   (lambda ()
      (minibuf-read "new file:" dirb-enter-new-file)
   )
)

(define dirb-enter-new-dir
   (lambda (b f)
      (with-buffer b
         (mkdir (string-append (get-local current-cwd) "/" f))
         (dirb-open-dir (get-local current-cwd))
      )
   )
)

(define dirb-create-new-dir
   (lambda ()
      (minibuf-read "new dir:" dirb-enter-new-dir)
   )
)

(define dirb-delete-answer
   (lambda (b v)
      (let*(
            [e (extract-line-inner)]
            [p (string-append (get-local current-cwd) "/" e)]
           )
         (with-buffer b
            (when (eq? v 'yes)
               (rm-rf p)
               (dirb-open-dir (get-local current-cwd))
            )
         )
      )
   )
)

(define dirb-delete-entry
   (lambda ()
      (minibuf-ask "Delete entry(s) ?" dirb-delete-answer)
   )
)

(define dirb-enter-rename-entry
   (lambda (b v)
      (let (
            [old (string-append (get-local current-cwd) "/" (get-local defval))]
            [new (string-append (get-local current-cwd) "/" v)]
           )
         (rename-file old new)
         (dirb-open-dir (get-local current-cwd))
      )
   )
)

(define dirb-rename-entry
   (lambda ()
      (let ([entry (dirb-get-entry (extract-line-inner))])
         (set-local! defval entry)
         (minibuf-read "rename:" entry dirb-enter-rename-entry)
      )
   )
)

(define dirb-grep
   (lambda ()
      (grep)
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
      (bind-key map "C-u" dirb-scroll-halfpage-up)
      (bind-key map "C-d" dirb-scroll-halfpage-down)
      (bind-key map "C-f" dirb-scroll-page-down)
      (bind-key map "C-b" dirb-scroll-page-up)
      (bind-key map "H" dirb-viewport-begin)
      (bind-key map "L" dirb-viewport-end)
      (bind-key map "G" dirb-move-end)
      (bind-key map "." dirb-show-hidden)
      (bind-key map "~" dirb-goto-home)
      (bind-key map "w" dirb-goto-cwd)
      (bind-key map "W" dirb-set-cwd)
      (bind-key map "n f" dirb-create-new-file)
      (bind-key map "n d" dirb-create-new-dir)
      (bind-key map "d" dirb-delete-entry)
      (bind-key map "r" dirb-rename-entry)
      (bind-key map "s" dirb-grep)
      map
   )
)

(define-mode dirb-mode "Dirb" #f
   #f
)
