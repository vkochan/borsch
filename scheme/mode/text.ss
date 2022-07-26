(define text-mode-set-keymap
   (lambda (m)
      (keymap-set-parent (get-local text-mode-map) (get-local-symbol m))
   )
)

(define text-mode-normal
   (lambda ()
      (text-mode-set-keymap 'text-mode-normal-local-map)
      (buffer-set-state-name "")
      (buffer-snapshot)
      (enable-insert #f)
      (selection-highlight #f)
   )
)

(define text-mode-insert
   (lambda ()
      (buffer-modify
         (text-mode-set-keymap 'text-mode-insert-local-map)
         (buffer-set-state-name "<I>")
         (enable-insert #t)
      )
   )
)

(define text-mode-visual
   (lambda ()
      (text-mode-set-keymap 'text-mode-visual-local-map)
      (buffer-set-state-name "<V>")
      (enable-insert #f)
      (selection-set)
      (selection-highlight #t)
   )
)

(define text-mode-visual-linewise
   (lambda ()
      (text-mode-set-keymap 'text-mode-visual-linewise-local-map)
      (buffer-set-state-name "<V *L*>")
      (enable-insert #f)
      (selection-set (line-begin-pos))
      (move-line-end)
      (selection-highlight #t)
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

(define file-open
   (lambda (p)
      (if (file-regular? p)
         (let ([b (buffer-create)])
            (with-current-buffer b
               (text-mode)
               (buffer-open-file p)
            )
         )
         ;; else
         (if (file-directory? p)
            (dirb p)
         )
      )
   )
)

(define file-open-at-cursor
   (lambda ()
      (let-values ([(f l) (get-file-location (extract-object))])
         (let ([p (if (equal? #\/ (string-ref f 0)) f (string-append (buffer-cwd) "/" f))])
            (file-open p)
            (move-line-num l)
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

(define text-mode-command
   (lambda ()
      (minibuf-read ":"
        (lambda (val)
            (let ([line (string->number val)])
               (if line
                  (move-line-num line)
                  ;; else
                  (message "Unknown command")
               )
            )
         )
      )
   )
)

(define text-mode-normal-map
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
      (bind-key map "i" (lambda () (text-mode-insert)))
      (bind-key map "a" (lambda () (move-next-char) (text-mode-insert)))
      (bind-key map "A" (lambda () (move-line-end) (text-mode-insert)))
      (bind-key map "o" (lambda () (insert-empty-line) (text-mode-insert)))
      (bind-key map "O" (lambda () (insert-empty-line-up) (text-mode-insert)))
      (bind-key map "C" (lambda () (delete-line-end) (text-mode-insert)))
      (bind-key map "c w" (lambda () (delete-word) (text-mode-insert)))
      (bind-key map "c W" (lambda () (delete-longword) (text-mode-insert)))
      (bind-key map "c e" (lambda () (delete-word-end) (text-mode-insert)))
      (bind-key map "c E" (lambda () (delete-longword-end) (text-mode-insert)))
      (bind-key map "c b" (lambda () (delete-prev-word) (text-mode-insert)))
      (bind-key map "c B" (lambda () (delete-prev-longword) (text-mode-insert)))
      (bind-key map "c 0" (lambda () (delete-line-begin) (text-mode-insert)))
      (bind-key map "c $" (lambda () (delete-line-end) (text-mode-insert)))
      (bind-key map "S" (lambda () (move-line-begin) (delete-line-end) (text-mode-insert)))
      (bind-key map "v" (lambda () (text-mode-visual)))
      (bind-key map "V" (lambda () (text-mode-visual-linewise)))
      (bind-key map "p" (lambda () (copybuf-paste) (buffer-snapshot)))
      (bind-key map "P" (lambda () (copybuf-paste-before) (buffer-snapshot)))
      (bind-key map "Y" (lambda () (copy-line)))
      (bind-key map "y y" (lambda () (copy-line)))
      (bind-key map "y p" (lambda () (copybuf-copy (buffer-filename))))
      (bind-key map "C-r" (lambda () (buffer-redo)))
      (bind-key map "u" (lambda () (buffer-undo)))
      (bind-key map "g f" (lambda () (file-open-at-cursor)))
      (bind-key map "g r" (lambda () (if (local-bound? buffer-reload-func) ((get-local buffer-reload-func)))))
      (bind-key map "g ^" (lambda () (dirb (path-parent (buffer-filename)))))
      (bind-key map "*" (lambda () (search-word-forward)))
      (bind-key map "/" (lambda () (search-regex-read)))
      (bind-key map "n" (lambda () (search-next)))
      (bind-key map "N" (lambda () (search-prev)))
      (bind-key map "C-s" (lambda () (text-mode-save-file)))
      (bind-key map ":" (lambda () (text-mode-command)))
      map
   )
)

(define text-mode-insert-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (insert-nl)))
      (bind-key map "<Backspace>" (lambda () (delete-prev-char)))
      (bind-key map "<Esc>" text-mode-normal)
      (bind-key map "M-<Space>" text-mode-normal)
      map
   )
)

(define text-mode-visual-file-open
   (lambda ()
      (let* (
             [f (selection-extract)]
             [p (if (equal? #\/ (string-ref f 0)) f (string-append (buffer-cwd) "/" f))]
            )
         (selection-clear)
         (when (file-exists? p)
            (file-open p)
         )
      )
   )
)

(define text-mode-visual-map
   (let ([map (make-keymap 'text-mode-normal-map)])
      (bind-key map "<Esc>" selection-clear)
      (bind-key map "M-<Space>" selection-clear)
      (bind-key map "x" (lambda () (selection-delete) (selection-clear)))
      (bind-key map "d" (lambda () (selection-delete) (selection-clear)))
      (bind-key map "y" (lambda () (selection-copy) (selection-clear)))
      (bind-key map "a" (lambda () (selection-copy-append) (selection-clear)))
      (bind-key map "A" (lambda () (selection-copy-append-linewise) (selection-clear)))
      (bind-key map "g f" text-mode-visual-file-open)
      map
   )
)

(define text-mode-visual-line-fixup
   (lambda ()
      (if (>= (cursor) (selection-get))
         (begin
            (selection-set (line-begin-pos (selection-get)))
            (move-line-end)
         )
         ;; else
         (begin
            (selection-set (line-end-pos (selection-get)))
            (move-line-begin)
         )
      )
   )
)

(define text-mode-visual-line-move-up
   (lambda ()
      (move-line-up)
      (text-mode-visual-line-fixup)
   )
)

(define text-mode-visual-line-move-down
   (lambda ()
      (move-line-down)
      (text-mode-visual-line-fixup)
   )
)

(define text-mode-visual-linewise-map
   (let ([map (make-keymap)])
      (bind-key map "<Esc>" selection-clear)
      (bind-key map "M-<Space>" selection-clear)
      (bind-key map "x" (lambda () (selection-delete) (selection-clear)))
      (bind-key map "d" (lambda () (selection-delete) (selection-clear)))
      (bind-key map "l" (lambda () (move-line-down)))
      (bind-key map "j" (lambda () (text-mode-visual-line-move-down)))
      (bind-key map "k" (lambda () (text-mode-visual-line-move-up)))
      (bind-key map "y" (lambda () (selection-copy-linewise) (selection-clear)))
      (bind-key map "a" (lambda () (selection-copy-append) (selection-clear)))
      (bind-key map "A" (lambda () (selection-copy-append-linewise) (selection-clear)))
      (bind-key map "G" (lambda () (move-buffer-end)))
      map
   )
)

(define text-mode-linenum-width
   (lambda (w)
      (if (not (buffer-is-vterm? (window-buffer w)))
         (begin
            (let ([b (window-buffer w)])
               (with-current-buffer b
                  (count-digits-num (buffer-line-num (buffer-end-pos)))
               )
            )
         )
       )
   )
)

(define text-mode-linenum-draw
   (lambda (w)
      (when (and (local-bound? linenum-enable) (get-local linenum-enable))
         (let ([width (text-mode-linenum-width w)])
            (when (not (eq? width (window-sidebar-width w)))
               (window-set-sidebar-width w (1+ width))
            )
            (let ([lines (window-viewport-lines-coord w)])
               (for-each
                  (lambda (c)
                     (let ([line (list-ref c 2)])
                        (window-draw-sidebar w 0 (list-ref c 1)
                                                 (format (string-append "~" (number->string width) "@a") line))
                     )
                  ) lines
               )
            )
         )
      )
   )
)

(define text-mode-insert-char
   (lambda (char)
      (insert (string char))
   )
)

(define-mode text-mode "Text" #f
   (when (not (local-bound? text-mode-map))
      (let ([map (make-keymap)])
         (define-local text-mode-map map)
         (buffer-set-keymap map)
      )
   )

   (or (local-bound? text-mode-visual-local-map)
       (define-local text-mode-visual-local-map (make-keymap 'text-mode-visual-map)))

   (or (local-bound? text-mode-visual-linewise-local-map)
       (define-local text-mode-visual-linewise-local-map (make-keymap 'text-mode-visual-linewise-map)))

   (or (local-bound? text-mode-normal-local-map)
       (define-local text-mode-normal-local-map (make-keymap 'text-mode-normal-map)))

   (or (local-bound? text-mode-insert-local-map)
       (define-local text-mode-insert-local-map (make-keymap 'text-mode-insert-map)))

   (or (local-bound? buffer-reload-func)
       (define-local buffer-reload-func buffer-reload-file))

   (define-local window-draw-hook text-mode-linenum-draw)
   (define-local text-insert-hook text-mode-insert-char)
   (define-local selection-clear-hook text-mode-normal)
   (define-local linenum-enable #t)
   (text-mode-normal)
)
