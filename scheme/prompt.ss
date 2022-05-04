(define prompt-pass-value-cmd
   (lambda ()
      (let (
            [b (get-local prompt-orig-buf)]
            [f (get-local prompt-cb)]
            [s (buffer-string)]
           )
         (window-delete)
         (f b s)
      )
   )
)

(define prompt-read-map
   (let ([map (make-keymap)])
      (bind-key map "<Esc>" window-delete)
      (bind-key map "<Enter>" prompt-pass-value-cmd)
      (bind-key map "<Backspace>" delete-prev-char)
      map
   )
)

(define prompt-read
   (lambda (s f)
      (let (
            [b (buffer-create)]
            [c (current-buffer)]
           )
         (with-current-buffer b
            (define-local prompt-orig-buf c)
            (define-local prompt-cb f)
            (buffer-set-name s)
            (buffer-set-keymap 'prompt-read-map)
            (enable-insert #t)
            (window-popup #t)
            (window-set-height 3)
         )
      )
   )
)
