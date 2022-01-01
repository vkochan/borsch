(define text-mode-cmd
   (lambda ()
      (buffer-set-keymap 'text-mode-cmd-map)
      (buffer-set-mode "Text <N>")
      (enable-insert #f)
      (mark-clear)
      (mark-highlight #f)
   )
)

(define text-mode-ins
   (lambda ()
      (buffer-set-keymap 'text-mode-ins-map)
      (buffer-set-mode "Text <I>")
      (enable-insert #t)
   )
)

(define text-mode-vis
   (lambda ()
      (buffer-set-keymap 'text-mode-vis-map)
      (buffer-set-mode "Text <V>")
      (enable-insert #f)
      (mark-set)
      (mark-highlight #t)
   )
)

(define text-mode-vis-linewise
   (lambda ()
      (buffer-set-keymap 'text-mode-vis-linewise-map)
      (buffer-set-mode "Text <V *L*>")
      (enable-insert #f)
      (mark-set (line-begin-pos))
      (move-line-end)
      (mark-highlight #t)
   )
)

(define text-mode-map
   (let ([map (make-keymap)])
      map
   )
)

(define text-mode-cmd-map
   (let ([map (make-keymap 'text-mode-map)])
      (bind-key map "h" (lambda () (move-prev-char)))
      (bind-key map "l" (lambda () (move-next-char)))
      (bind-key map "j" (lambda () (move-next-line)))
      (bind-key map "k" (lambda () (move-prev-line)))
      (bind-key map "w" (lambda () (move-next-word)))
      (bind-key map "W" (lambda () (move-next-longword)))
      (bind-key map "b" (lambda () (move-prev-word)))
      (bind-key map "B" (lambda () (move-prev-longword)))
      (bind-key map "e" (lambda () (move-word-end)))
      (bind-key map "E" (lambda () (move-longword-end)))
      (bind-key map "x" (lambda () (delete-next-char)))
      (bind-key map "X" (lambda () (delete-prev-char)))
      (bind-key map "D" (lambda () (delete-line-end)))
      (bind-key map "d d" (lambda () (delete-line)))
      (bind-key map "g g" (lambda () (move-buffer-begin)))
      (bind-key map "G" (lambda () (move-buffer-end)))
      (bind-key map "i" (lambda () (text-mode-ins)))
      (bind-key map "o" (lambda () (insert-empty-line) (text-mode-ins)))
      (bind-key map "O" (lambda () (insert-empty-line-up) (text-mode-ins)))
      (bind-key map "v" (lambda () (text-mode-vis)))
      (bind-key map "V" (lambda () (text-mode-vis-linewise)))
      (bind-key map "p" (lambda () (paste-from-register)))
      (bind-key map "P" (lambda () (paste-from-register-before)))
      (bind-key map "Y" (lambda () (copy-line)))
      (bind-key map "y y" (lambda () (copy-line)))
      map
   )
)

(define text-mode-ins-map
   (let ([map (make-keymap 'text-mode-map)])
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
      map
   )
)

(define text-mode-vis-linewise-map
   (let ([map (make-keymap 'text-mode-map)])
      (bind-key map "<Esc>" text-mode-cmd)
      (bind-key map "x" (lambda () (mark-delete) (text-mode-cmd)))
      (bind-key map "l" (lambda () (move-next-line)))
      (bind-key map "j" (lambda () (move-next-line) (move-line-end)))
      (bind-key map "k" (lambda () (move-prev-line) (move-line-end)))
      (bind-key map "y" (lambda () (mark-copy-linewise) (text-mode-cmd)))
      map
   )
)

(define-mode text-mode "Text" #f
   (enable-insert #t)
   (text-mode-cmd)
)
