(define text-mode-set-keymap
   (lambda (m)
      (keymap-set-parent (get-local text-mode-map) m)
   )
)

(define text-mode-cmd
   (lambda ()
      (text-mode-set-keymap 'text-mode-cmd-map)
      (buffer-set-mode "Text <N>")
      (buffer-snapshot)
      (enable-insert #f)
      (mark-clear)
      (mark-highlight #f)
   )
)

(define text-mode-ins
   (lambda ()
      (text-mode-set-keymap 'text-mode-ins-map)
      (buffer-set-mode "Text <I>")
      (enable-insert #t)
   )
)

(define text-mode-vis
   (lambda ()
      (text-mode-set-keymap 'text-mode-vis-map)
      (buffer-set-mode "Text <V>")
      (enable-insert #f)
      (mark-set)
      (mark-highlight #t)
   )
)

(define text-mode-vis-linewise
   (lambda ()
      (text-mode-set-keymap 'text-mode-vis-linewise-map)
      (buffer-set-mode "Text <V *L*>")
      (enable-insert #f)
      (mark-set (line-begin-pos))
      (move-line-end)
      (mark-highlight #t)
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
      (bind-key map "x" (lambda () (delete-next-char) (buffer-snapshot)))
      (bind-key map "X" (lambda () (delete-prev-char) (buffer-snapshot)))
      (bind-key map "D" (lambda () (delete-line-end) (buffer-snapshot)))
      (bind-key map "d d" (lambda () (delete-line) (buffer-snapshot)))
      (bind-key map "g g" (lambda () (move-buffer-begin)))
      (bind-key map "G" (lambda () (move-buffer-end)))
      (bind-key map "H" (lambda () (cursor-set (window-viewport-begin))))
      (bind-key map "L" (lambda () (cursor-set (window-viewport-end))))
      (bind-key map "C-u" (lambda () (cursor-set (window-scroll-halfpage-up))))
      (bind-key map "C-d" (lambda () (cursor-set (window-scroll-halfpage-down))))
      (bind-key map "C-f" (lambda () (cursor-set (window-scroll-page-down))))
      (bind-key map "C-b" (lambda () (cursor-set (window-scroll-page-up))))
      (bind-key map "i" (lambda () (text-mode-ins)))
      (bind-key map "o" (lambda () (insert-empty-line) (text-mode-ins)))
      (bind-key map "O" (lambda () (insert-empty-line-up) (text-mode-ins)))
      (bind-key map "v" (lambda () (text-mode-vis)))
      (bind-key map "V" (lambda () (text-mode-vis-linewise)))
      (bind-key map "p" (lambda () (paste-from-register) (buffer-snapshot)))
      (bind-key map "P" (lambda () (paste-from-register-before) (buffer-snapshot)))
      (bind-key map "Y" (lambda () (copy-line)))
      (bind-key map "y y" (lambda () (copy-line)))
      (bind-key map "C-r" (lambda () (buffer-redo)))
      (bind-key map "u" (lambda () (buffer-undo)))
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
               (with-buffer b
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
      (let ([b (window-buffer w)])
         (with-buffer b
            (when (local-bound? linenum-enable)
               (let ([width (text-mode-linenum-width w)])
                  (window-set-sidebar-width w (1+ width))
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
   )
)

(define-mode text-mode "Text" #f
   (when (not (local-bound? text-mode-map))
      (let ([map (make-keymap)])
         (define-local text-mode-map map)
         (buffer-set-keymap map)
      )
   )
   (text-mode-cmd)
   (define-local linenum-enable #t)
   (add-hook 'window-draw-hook text-mode-linenum-draw)
)
