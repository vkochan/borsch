(define text-mode-set-keymap
   (lambda (m)
      (keymap-set-parent (get-local text-mode-map) (get-local-symbol m))
   )
)

(define text-mode-cmd
   (lambda ()
      (text-mode-set-keymap 'text-mode-cmd-local-map)
      (buffer-set-state-name "")
      (buffer-snapshot)
      (enable-insert #f)
      (mark-clear)
      (mark-highlight #f)
   )
)

(define text-mode-ins
   (lambda ()
      (buffer-modify
         (text-mode-set-keymap 'text-mode-ins-local-map)
         (buffer-set-state-name "<I>")
         (enable-insert #t)
      )
   )
)

(define text-mode-vis
   (lambda ()
      (text-mode-set-keymap 'text-mode-vis-local-map)
      (buffer-set-state-name "<V>")
      (enable-insert #f)
      (mark-set)
      (mark-highlight #t)
   )
)

(define text-mode-vis-linewise
   (lambda ()
      (text-mode-set-keymap 'text-mode-vis-linewise-local-map)
      (buffer-set-state-name "<V *L*>")
      (enable-insert #f)
      (mark-set (line-begin-pos))
      (move-line-end)
      (mark-highlight #t)
   )
)

(define get-file-location
   (lambda (s)
      (if (file-regular? s)
         (values s 0)
         ;; else
         (begin
            (let ([grep-line (string-split s #\:)])
               (if (> (length grep-line) 1)
                  (values (list-ref grep-line 0) (string->number (list-ref grep-line 1)))
                  ;; else
                  (values (list-ref grep-line 0) 0)
               )
            )
         )
      )
   )
)

(define file-open-at-cursor
   (lambda ()
      (let-values ([(f l) (get-file-location (extract-longword))])
         (let ([p (if (equal? #\/ (string-ref f 0)) f (string-append (view-cwd) "/" f))])
            (if (file-regular? p)
               (let ([b (buffer-create)])
                  (with-current-buffer b
                     (text-mode)
                     (buffer-open-file p)
                     (move-line-num l)
                  )
               )
               ;; else
               (if (file-directory? p)
                  (dirb p)
               )
            )
         )
      )
   )
)

(define text-mode-save
   (lambda ()
      (if (buffer-save)
         (message (format "~a is saved" (buffer-filename)))
         ;; else
         (message (format "Can't save ~a" (buffer-filename)))
      )
   )
)

(define text-mode-save-file
   (lambda ()
      (let ([f (buffer-filename)])
         (if (not (equal? f ""))
            (text-mode-save)
            ;; else
            (begin
               (minibuf-read "Save as:" (buffer-name)
                  (lambda (f)
                     (define-local tmp-file-name f)
                     (if (file-exists? f)
                        (minibuf-ask (format "~a already exists, overwrite ?" f)
                           (lambda (a)
                              (if (equal? a 'yes)
                                 (begin
                                    (buffer-set-filename (get-local tmp-file-name))
                                    (text-mode-save)
                                 )
                              )
                           )
                        )
                        ;; else
                        (begin
                           (buffer-set-filename f)
                           (text-mode-save)
                        )
                     )
                  )
               )
            )
         )
      )
   )
)

(define text-mode-cmd-map
   (let ([map (make-keymap)])
      (bind-key map "h" (lambda () (move-prev-char)))
      (bind-key map "l" (lambda () (move-next-char)))
      (bind-key map "j" (lambda () (move-line-down)))
      (bind-key map "k" (lambda () (move-line-up)))
      (bind-key map "w" (lambda () (move-next-word)))
      (bind-key map "W" (lambda () (move-next-longword)))
      (bind-key map "b" (lambda () (move-prev-word)))
      (bind-key map "B" (lambda () (move-prev-longword)))
      (bind-key map "e" (lambda () (move-word-end)))
      (bind-key map "E" (lambda () (move-longword-end)))
      (bind-key map "0" (lambda () (move-line-begin)))
      (bind-key map "$" (lambda () (move-line-end)))
      (bind-key map "x" (lambda () (delete-next-char) (buffer-snapshot)))
      (bind-key map "X" (lambda () (delete-prev-char) (buffer-snapshot)))
      (bind-key map "D" (lambda () (delete-line-end) (buffer-snapshot)))
      (bind-key map "d d" (lambda () (delete-line) (buffer-snapshot)))
      (bind-key map "d w" (lambda () (delete-word) (buffer-snapshot)))
      (bind-key map "d W" (lambda () (delete-longword) (buffer-snapshot)))
      (bind-key map "d e" (lambda () (delete-word-end) (buffer-snapshot)))
      (bind-key map "d E" (lambda () (delete-longword-end) (buffer-snapshot)))
      (bind-key map "d b" (lambda () (delete-prev-word) (buffer-snapshot)))
      (bind-key map "d B" (lambda () (delete-prev-longword) (buffer-snapshot)))
      (bind-key map "d 0" (lambda () (delete-line-begin) (buffer-snapshot)))
      (bind-key map "d $" (lambda () (delete-line-end) (buffer-snapshot)))
      (bind-key map "g g" (lambda () (move-buffer-begin)))
      (bind-key map "G" (lambda () (cursor-set (- (buffer-end-pos) 1)) (move-line-start)))
      (bind-key map "H" (lambda () (cursor-set (window-viewport-begin))))
      (bind-key map "L" (lambda () (cursor-set (window-viewport-end))))
      (bind-key map "C-u" (lambda () (cursor-set (window-scroll-halfpage-up))))
      (bind-key map "C-d" (lambda () (cursor-set (window-scroll-halfpage-down))))
      (bind-key map "C-f" (lambda () (cursor-set (window-scroll-page-down))))
      (bind-key map "C-b" (lambda () (cursor-set (window-scroll-page-up))))
      (bind-key map "i" (lambda () (text-mode-ins)))
      (bind-key map "a" (lambda () (move-next-char) (text-mode-ins)))
      (bind-key map "A" (lambda () (move-line-end) (text-mode-ins)))
      (bind-key map "o" (lambda () (insert-empty-line) (text-mode-ins)))
      (bind-key map "O" (lambda () (insert-empty-line-up) (text-mode-ins)))
      (bind-key map "C" (lambda () (delete-line-end) (text-mode-ins)))
      (bind-key map "c w" (lambda () (delete-word) (text-mode-ins)))
      (bind-key map "c W" (lambda () (delete-longword) (text-mode-ins)))
      (bind-key map "c e" (lambda () (delete-word-end) (text-mode-ins)))
      (bind-key map "c E" (lambda () (delete-longword-end) (text-mode-ins)))
      (bind-key map "c b" (lambda () (delete-prev-word) (text-mode-ins)))
      (bind-key map "c B" (lambda () (delete-prev-longword) (text-mode-ins)))
      (bind-key map "c 0" (lambda () (delete-line-begin) (text-mode-ins)))
      (bind-key map "c $" (lambda () (delete-line-end) (text-mode-ins)))
      (bind-key map "S" (lambda () (move-line-begin) (delete-line-end) (text-mode-ins)))
      (bind-key map "v" (lambda () (text-mode-vis)))
      (bind-key map "V" (lambda () (text-mode-vis-linewise)))
      (bind-key map "p" (lambda () (copybuf-paste) (buffer-snapshot)))
      (bind-key map "P" (lambda () (copybuf-paste-before) (buffer-snapshot)))
      (bind-key map "Y" (lambda () (copy-line)))
      (bind-key map "y y" (lambda () (copy-line)))
      (bind-key map "y p" (lambda () (copybuf-copy (buffer-filename))))
      (bind-key map "C-r" (lambda () (buffer-redo)))
      (bind-key map "u" (lambda () (buffer-undo)))
      (bind-key map "g f" (lambda () (file-open-at-cursor)))
      (bind-key map "g r" (lambda () (if (local-bound? buffer-reload-func) ((get-local buffer-reload-func)))))
      (bind-key map "*" (lambda () (search-word-forward)))
      (bind-key map "/" (lambda () (search-regex-read)))
      (bind-key map "n" (lambda () (search-next)))
      (bind-key map "N" (lambda () (search-prev)))
      (bind-key map "C-s" (lambda () (text-mode-save-file)))
      (bind-key map ":" (lambda () (minibuf-cmd)))
      map
   )
)

(define text-mode-ins-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (insert-nl)))
      (bind-key map "<Backspace>" (lambda () (delete-prev-char)))
      (bind-key map "<Esc>" text-mode-cmd)
      map
   )
)

(define text-mode-vis-map
   (let ([map (make-keymap 'text-mode-cmd-map)])
      (bind-key map "<Esc>" text-mode-cmd)
      (bind-key map "x" (lambda () (mark-delete) (text-mode-cmd)))
      (bind-key map "d" (lambda () (mark-delete) (text-mode-cmd)))
      (bind-key map "y" (lambda () (mark-copy) (text-mode-cmd)))
      (bind-key map "a" (lambda () (mark-copy-append) (text-mode-cmd)))
      (bind-key map "A" (lambda () (mark-copy-append-linewise) (text-mode-cmd)))
      map
   )
)

(define text-mode-vis-linewise-map
   (let ([map (make-keymap)])
      (bind-key map "<Esc>" text-mode-cmd)
      (bind-key map "x" (lambda () (mark-delete) (text-mode-cmd)))
      (bind-key map "d" (lambda () (mark-delete) (text-mode-cmd)))
      (bind-key map "l" (lambda () (move-line-down)))
      (bind-key map "j" (lambda () (move-line-down) (move-line-end)))
      (bind-key map "k" (lambda () (move-line-up) (move-line-end)))
      (bind-key map "y" (lambda () (mark-copy-linewise) (text-mode-cmd)))
      (bind-key map "a" (lambda () (mark-copy-append) (text-mode-cmd)))
      (bind-key map "A" (lambda () (mark-copy-append-linewise) (text-mode-cmd)))
      map
   )
)

(define text-mode-linenum-width
   (lambda (w)
      (if (not (buffer-is-term? (window-buffer w)))
         (begin
            (let ([b (window-buffer w)])
               (with-current-buffer b
                  (let* (
                         [end (line-begin-pos (window-viewport-end w))]
                         [coord (window-viewport-coord w end)]
                        )
                     (if coord
                        (count-digits-num (list-ref coord 2))
                        ;; else
                        0
                     )
                  )
               )
            )
         )
         ;; else
         0
      )
   )
)

(define text-mode-linenum-draw
   (lambda (w)
      (when (and (local-bound? linenum-enable) (get-local linenum-enable))
         (let ([width (1+ (text-mode-linenum-width w))])
            (when (not (eq? width (window-sidebar-width w)))
               (window-set-sidebar-width w width)
            )
            (let ([lines (window-viewport-lines-coord w)])
               (for-each
                  (lambda (c)
                     (window-draw-sidebar w 0 (list-ref c 1) (format "~a" (list-ref c 2)))
                  ) lines
               )
            )
         )
      )
   )
)

(define-mode text-mode "Text" #f
   (when (not (local-bound? text-mode-map))
      (let ([map (make-keymap)])
         (define-local text-mode-map map)
         (buffer-set-keymap map)
      )
   )

   (or (local-bound? text-mode-vis-local-map)
       (define-local text-mode-vis-local-map (make-keymap 'text-mode-vis-map)))

   (or (local-bound? text-mode-vis-linewise-local-map)
       (define-local text-mode-vis-linewise-local-map (make-keymap 'text-mode-vis-linewise-map)))

   (or (local-bound? text-mode-cmd-local-map)
       (define-local text-mode-cmd-local-map (make-keymap 'text-mode-cmd-map)))

   (or (local-bound? text-mode-ins-local-map)
      (define-local text-mode-ins-local-map (make-keymap 'text-mode-ins-map)))

   (or (local-bound? buffer-reload-func)
      (define-local buffer-reload-func buffer-reload-file))

   (define-local window-draw-hook text-mode-linenum-draw)
   (define-local linenum-enable #t)
   (text-mode-cmd)
)
