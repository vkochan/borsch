(define __cs_minibuf_create (foreign-procedure "cs_minibuf_create" () scheme-object))

(define minibuf-window #f)
(define minibuf-buffer #f)

(define minibuf-cancel-read
   (lambda ()
      (enable-insert #f)
      (set-local! input-mode #f)
      (erase-buffer)
      (window-select (window-prev-selected))
   )
)

(define minibuf-send-value
   (lambda ()
      (let (
            [s  (buffer-string (get-local prompt-pos) (buffer-end-pos))]
            [fn (get-local func-value)]
            [b  (get-local orig-buf)]
           )
         (set-local! input-mode #f)
         (enable-insert #f)
         (erase-buffer)
         (window-select (window-prev-selected))
         (fn b s)
      )
   )
)

(define minibuf-delete-prev-char
   (lambda ()
      (let ([p (get-local prompt-pos)])
         (when (> (cursor) p)
            (delete-prev-char)
         )
      )
   )
)

(define minibuf-move-prev-char
   (lambda ()
      (let ([p (get-local prompt-pos)])
         (when (> (cursor) p)
            (move-prev-char)
         )
      )
   )
)

(define minibuf-move-next-char
   (lambda ()
      (move-next-char)
   )
)

(define minibuf-prompt-map
   (let ([map (make-empty-keymap)])
      (bind-key map "<Backspace>" minibuf-delete-prev-char)
      (bind-key map "<Enter>" minibuf-send-value)
      (bind-key map "<Esc>" minibuf-cancel-read)
      (bind-key map "C-h" minibuf-move-prev-char)
      (bind-key map "C-l" minibuf-move-next-char)
      (bind-key map "C-g q q" do-quit)
      map
   )
)

(define minibuf-create
   (lambda ()
      (let ([m (__cs_minibuf_create)])
         (set! minibuf-buffer (window-buffer m))
         (set! minibuf-window m)

         (with-buffer minibuf-buffer
            (define-local input-mode #f)
            (define-local func-value #f)
            (define-local prompt-pos 0)
            (define-local orig-buf 0)
            (enable-insert #t)
         )
      )
   )
)

(add-hook 'on-message-hook
   (lambda (m)
      (when minibuf-buffer
         (window-set-height minibuf-window (1+ (lines-count m)))
         (with-buffer minibuf-buffer
            (insert (format "~a\n" m))
            (cursor-set 0)
         )
      )
   )
)

(add-hook 'on-error-hook
   (lambda (e)
      (when minibuf-buffer
         (window-set-height minibuf-window (1+ (lines-count e)))
         (with-buffer minibuf-buffer
            (when (not (get-local input-mode))
               (insert (format "~a\n" e) '(style (:fg "red")))
               (cursor-set 0)
            )
         )
      )
   )
)

(add-hook 'key-press-hook
   (lambda (k)
      (when minibuf-buffer
         (with-buffer minibuf-buffer
            (when (not (get-local input-mode))
               (window-set-height minibuf-window 1)
               (erase-buffer)
            )
         )
      )
   )
)

(define minibuf-read
   (lambda (str fn)
      (let (
            [b (buffer-current)]
           )
         (with-buffer minibuf-buffer
            (enable-insert #t)
            (erase-buffer)
            (insert str)
            (set-local! prompt-pos (cursor))
            (set-local! input-mode #t)
            (set-local! func-value fn)
            (set-local! orig-buf b)
            (buffer-set-keymap 'minibuf-prompt-map)
         )
         (window-select minibuf-window)
      )
   )
)
