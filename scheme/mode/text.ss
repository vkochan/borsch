(define delbuf-is-linewise #f)
(define delbuf-reg "")

(define (delbuf-put s)
   (set! delbuf-reg s))

(define (delbuf-paste-inplace)
   (text-insert delbuf-reg))

(define (delbuf-paste)
   (if (not delbuf-is-linewise)
      (begin
         (when (not (equal? #\newline (text-char)))
            (cursor-to-next-char))
         (delbuf-paste-inplace)
         (cursor-to-prev-char))
       ;; else
      (let ([has-newline (equal? (string-ref delbuf-reg 0) #\newline)])
         (cursor-to-line-end)
         (when (not has-newline) (text-insert "\n"))
         (with-saved-cursor
            (delbuf-paste-inplace)
            (when (not has-newline) (text-delete-char))))))

(define (text-mode-set-keymap m)
   (keymap-set-parent (get-local text-mode-map) (get-local-symbol m)))

(define (text-mode-normal)
   (text-mode-set-keymap 'text-mode-normal-local-map)
   (buffer-set-state-name "")
   (buffer-snapshot)
   (enable-insert #f) )

(define (text-mode-insert)
   (text-modify
      (text-mode-set-keymap 'text-mode-insert-local-map)
      (buffer-set-state-name "<I>")
      (enable-insert #t)))

(define (text-mode-visual)
   (text-mode-set-keymap 'text-mode-visual-local-map)
   (buffer-set-state-name "<V>")
   (enable-insert #f)
   (text-set-selection) )

(define (text-mode-visual-linewise)
   (text-mode-set-keymap 'text-mode-visual-linewise-local-map)
   (buffer-set-state-name "<V *L*>")
   (enable-insert #f)
   (text-set-selection (text-line-begin-pos))
   (cursor-to-line-end) )

(define (get-file-location s)
   (if (file-regular? s)
      (values s 0)
      ;; else
      (begin
         (let ([grep-line (string-split s #\:)])
            (if (> (length grep-line) 1)
               (values (list-ref grep-line 0) (string->number (list-ref grep-line 1)))
               ;; else
               (values (list-ref grep-line 0) 0))))))

(define (file-open-at-cursor)
   (let-values ([(f l) (get-file-location (text-object))])
      (let ([p (if (equal? #\/ (string-ref f 0)) f (string-append (buffer-cwd) "/" f))])
         (file-open p)
         (cursor-to-line l))))

(define (text-mode-save)
   (if (buffer-save)
      (message (format "~a is saved" (buffer-filename)))
      ;; else
      (message (format "Can't save ~a" (buffer-filename)))))

(define (text-mode-save-file)
   (text-modify
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
                                 (text-mode-save)))))
                     ;; else
                     (begin
                        (buffer-set-filename f)
                        (text-mode-save))))))))))

(define (text-mode-command)
   (minibuf-read ":"
     (lambda (val)
         (let ([line (string->number val)])
            (if line
               (cursor-to-line line)
               ;; else
               (message "Unknown command"))))))

(define (text-mode-delete fn)
   (set! delbuf-is-linewise #f)
   (delbuf-put (text-track-deletion (fn)))
   (buffer-snapshot))

(define (text-mode-delete-linewise fn)
   (set! delbuf-is-linewise #t)
   (delbuf-put (text-track-deletion (fn)))
   (buffer-snapshot))

(define (text-mode-join-lines)
   (cursor-to-line-end)
   (text-delete-to-next-char)
   (text-insert " ")
   (cursor-to-prev-char)
   (text-delete-word)
   (when (not (eq? (cursor) (text-end-pos)))
      (text-insert " ")))

(define (text-mode-search fn)
   (let ([pos (fn)])
      (cursor-set pos)))

(define (text-search-regex-read)
   (when (not (buffer-is-vterm?))
      (minibuf-read "/"
         (lambda (r)
            (text-search-reg r)
            (cursor-set (text-search-regex r))))))

(define text-mode-normal-map
   (let ([map (make-keymap)])
      (bind-key map "h" (lambda () (cursor-to-prev-char)))
      (bind-key map "l" (lambda () (cursor-to-next-char)))
      (bind-key map "j" (lambda () (cursor-to-line-down)))
      (bind-key map "k" (lambda () (cursor-to-line-up)))
      (bind-key map "w" (lambda () (cursor-to-next-word)))
      (bind-key map "W" (lambda () (cursor-to-next-longword)))
      (bind-key map "b" (lambda () (cursor-to-prev-word)))
      (bind-key map "B" (lambda () (cursor-to-prev-longword)))
      (bind-key map "e" (lambda () (cursor-to-word-end)))
      (bind-key map "E" (lambda () (cursor-to-longword-end)))
      (bind-key map "0" (lambda () (cursor-to-line-begin)))
      (bind-key map "$" (lambda () (cursor-to-line-end)))
      (bind-key map "x" (lambda () (text-mode-delete text-delete-to-next-char)))
      (bind-key map "X" (lambda () (text-mode-delete text-delete-to-prev-char)))
      (bind-key map "D" (lambda () (text-mode-delete text-delete-to-line-end)))
      (bind-key map "d d" (lambda () (text-mode-delete-linewise text-delete-line)))
      (bind-key map "d w" (lambda () (text-mode-delete text-delete-word)))
      (bind-key map "d W" (lambda () (text-mode-delete text-delete-longword)))
      (bind-key map "d e" (lambda () (text-mode-delete text-delete-to-word-end)))
      (bind-key map "d E" (lambda () (text-mode-delete text-delete-to-longword-end)))
      (bind-key map "d b" (lambda () (text-mode-delete text-delete-to-prev-word)))
      (bind-key map "d B" (lambda () (text-mode-delete text-delete-to-prev-longword)))
      (bind-key map "d 0" (lambda () (text-mode-delete text-delete-to-line-begin)))
      (bind-key map "d $" (lambda () (text-mode-delete text-delete-to-line-end)))
      (bind-key map "J" (lambda () (text-mode-join-lines)))
      (bind-key map "g g" (lambda () (cursor-to-begin)))
      (bind-key map "G" (lambda () (cursor-set (- (text-end-pos) 1)) (cursor-to-line-start)))
      (bind-key map "H" (lambda () (cursor-set (window-begin-pos))))
      (bind-key map "L" (lambda () (cursor-set (window-end-pos))))
      (bind-key map "C-u" (lambda () (cursor-set (window-scroll-halfpage-up))))
      (bind-key map "C-d" (lambda () (cursor-set (window-scroll-halfpage-down))))
      (bind-key map "C-f" (lambda () (cursor-set (window-scroll-page-down))))
      (bind-key map "C-b" (lambda () (cursor-set (window-scroll-page-up))))
      (bind-key map "i" (lambda () (text-mode-insert)))
      (bind-key map "a" (lambda () (cursor-to-next-char) (text-mode-insert)))
      (bind-key map "A" (lambda () (cursor-to-line-end) (text-mode-insert)))
      (bind-key map "o" (lambda () (text-insert-empty-line) (text-mode-insert)))
      (bind-key map "O" (lambda () (text-insert-empty-line-up) (text-mode-insert)))
      (bind-key map "C" (lambda () (text-delete-to-line-end) (text-mode-insert)))
      (bind-key map "c w" (lambda () (text-delete-word) (text-mode-insert)))
      (bind-key map "c W" (lambda () (text-delete-longword) (text-mode-insert)))
      (bind-key map "c e" (lambda () (text-delete-to-word-end) (text-mode-insert)))
      (bind-key map "c E" (lambda () (text-delete-to-longword-end) (text-mode-insert)))
      (bind-key map "c b" (lambda () (text-delete-to-prev-word) (text-mode-insert)))
      (bind-key map "c B" (lambda () (text-delete-to-prev-longword) (text-mode-insert)))
      (bind-key map "c 0" (lambda () (text-delete-to-line-begin) (text-mode-insert)))
      (bind-key map "c $" (lambda () (text-delete-to-line-end) (text-mode-insert)))
      (bind-key map "S" (lambda () (cursor-to-line-begin) (text-delete-to-line-end) (text-mode-insert)))
      (bind-key map "v" (lambda () (text-mode-visual)))
      (bind-key map "V" (lambda () (text-mode-visual-linewise)))
      (bind-key map "^ d" (lambda () (delbuf-paste) (buffer-snapshot)))
      (bind-key map "p" (lambda () (text-paste) (buffer-snapshot)))
      (bind-key map "P" (lambda () (text-paste-before) (buffer-snapshot)))
      (bind-key map "Y" (lambda () (text-copy-line)))
      (bind-key map "y y" (lambda () (text-copy-line)))
      (bind-key map "y p" (lambda () (copybuf-copy (buffer-filename))))
      (bind-key map "C-r" (lambda () (buffer-redo)))
      (bind-key map "u" (lambda () (buffer-undo)))
      (bind-key map "g f" (lambda () (file-open-at-cursor)))
      (bind-key map "g r" (lambda () (if (local-bound? buffer-reload-func) ((get-local buffer-reload-func)))))
      (bind-key map "g ^" (lambda () (dirb (path-parent (buffer-filename)))))
      (bind-key map "*" (lambda () (text-mode-search text-search-word-forward)))
      (bind-key map "/" (lambda () (text-mode-search text-search-regex-read)))
      (bind-key map "n" (lambda () (text-mode-search text-search-next)))
      (bind-key map "N" (lambda () (text-mode-search text-search-prev)))
      (bind-key map "C-s" (lambda () (text-mode-save-file)))
      (bind-key map ":" (lambda () (text-mode-command)))
      map))

(define (text-insert-clipboard)
   (text-insert (copybuf-clip-get)))

(define text-mode-insert-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (text-insert-nl)))
      (bind-key map "C-v" (lambda () (text-insert-clipboard)))
      (bind-key map "<Backspace>" (lambda () (text-delete-to-prev-char)))
      (bind-key map "<Esc>" text-mode-normal)
      (bind-key map "M-<Space>" text-mode-normal)
      map))

(define (text-mode-visual-file-open)
   (let* ([f (text-selection)]
          [p (if (equal? #\/ (string-ref f 0)) f (string-append (buffer-cwd) "/" f))])
      (text-clear-selection)
      (when (file-exists? p)
         (file-open p))))

(define text-mode-visual-map
   (let ([map (make-keymap 'text-mode-normal-map)])
      (bind-key map "<Esc>" text-clear-selection)
      (bind-key map "M-<Space>" text-clear-selection)
      (bind-key map "x" (lambda () (text-delete-selection) (text-clear-selection)))
      (bind-key map "d" (lambda () (text-mode-delete text-delete-selection) (text-clear-selection)))
      (bind-key map "y" (lambda () (text-copy-selection) (text-clear-selection)))
      (bind-key map "a" (lambda () (text-append-selection) (text-clear-selection)))
      (bind-key map "A" (lambda () (text-append-selection-linewise) (text-clear-selection)))
      (bind-key map "g f" text-mode-visual-file-open)
      map))

(define (text-mode-visual-line-fixup)
   (if (>= (cursor) (text-get-selection))
      (begin
         (text-set-selection (text-line-begin-pos (text-get-selection)))
         (cursor-to-line-end))
      ;; else
      (begin
         (text-set-selection (text-line-end-pos (text-get-selection)))
         (cursor-to-line-begin))))

(define (text-mode-visual-line-move-up)
   (cursor-to-line-up)
   (text-mode-visual-line-fixup))

(define (text-mode-visual-line-move-down)
   (cursor-to-line-down)
   (text-mode-visual-line-fixup))

(define text-mode-visual-linewise-map
   (let ([map (make-keymap)])
      (bind-key map "<Esc>" text-clear-selection)
      (bind-key map "M-<Space>" text-clear-selection)
      (bind-key map "x" (lambda () (text-delete-selection) (text-clear-selection)))
      (bind-key map "d" (lambda () (text-mode-delete-linewise text-delete-selection) (text-clear-selection)))
      (bind-key map "l" (lambda () (cursor-to-line-down)))
      (bind-key map "j" (lambda () (text-mode-visual-line-move-down)))
      (bind-key map "k" (lambda () (text-mode-visual-line-move-up)))
      (bind-key map "y" (lambda () (text-copy-selection-linewise) (text-clear-selection)))
      (bind-key map "a" (lambda () (text-append-selection) (text-clear-selection)))
      (bind-key map "A" (lambda () (text-append-selection-linewise) (text-clear-selection)))
      (bind-key map "G" (lambda () (cursor-to-end)))
      map))

(define (text-mode-linenum-width w)
   (if (not (buffer-is-vterm? (window-buffer w)))
      (begin
         (let ([b (window-buffer w)])
            (with-current-buffer b
               (count-digits-num (buffer-line-num (text-end-pos))))))))

(define text-mode-linenum-draw
   (lambda (w)
      (when (and (local-bound? linenum-enable) (get-local linenum-enable))
         (let ([width (text-mode-linenum-width w)])
            (when (not (eq? width (window-sidebar-width w)))
               (window-set-sidebar-width w width))
            (let ([lines (window-lines-coord w)]
                  [wh    (window-inner-height w)])
               (while (> wh 0)
                  (window-draw-text w 0 (- wh 1)
                                        (format (string-append "~" (number->string width) "@a ") ""))
                  (set! wh (- wh 1)))
               (for-each
                  (lambda (c)
                     (let ([line (list-ref c 2)])
                        (window-draw-text w 0 (list-ref c 1)
                                          (format (string-append "~" (number->string width) "@a ") line)
                                          '(fg: "white" bg: "bright-blue"))))
                  lines)
               )))))

(define (text-mode-insert-char char)
   (text-insert-char char))

(define buffer-create-text
   (case-lambda
      [() 
       (let ([b (create-buffer)])
          (text-mode)
          b)]

      [(n) 
       (let ([b (create-buffer n)])
          (text-mode)
          b)]))

(define-mode text-mode "Text" #f
   (when (not (local-bound? text-mode-map))
      (let ([map (make-keymap)])
         (define-local text-mode-map map)
         (buffer-set-keymap map)))
   (or (local-bound? text-mode-visual-local-map)
       (define-local text-mode-visual-local-map (make-keymap 'text-mode-visual-map)))
   (or (local-bound? text-mode-visual-linewise-local-map)
       (define-local text-mode-visual-linewise-local-map (make-keymap 'text-mode-visual-linewise-map)))
   (or (local-bound? text-mode-normal-local-map)
       (define-local text-mode-normal-local-map (make-keymap 'text-mode-normal-map)))
   (or (local-bound? text-mode-insert-local-map)
       (define-local text-mode-insert-local-map (make-keymap 'text-mode-insert-map)))
   (or (local-bound? buffer-reload-func)
       (define-local buffer-reload-func text-reload-file))
   (define-local text-draw-hook text-mode-linenum-draw)
   (define-local text-insert-hook text-mode-insert-char)
   (define-local text-clear-selection-hook text-mode-normal)
   (define-local linenum-enable #t)
   (text-mode-normal))
