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
   (lambda (v)
      (let (
            [fn (get-local func-value)]
            [b  (get-local orig-buf)]
           )
         (set-local! input-mode #f)
         (enable-insert #f)
         (erase-buffer)
         (window-select (window-prev-selected))
         (fn v)
      )
   )
)

(define minibuf-prompt-send-value
   (lambda ()
      (let (
            [s  (buffer-string (get-local prompt-pos) (buffer-end-pos))]
           )
         (minibuf-send-value s)
      )
   )
)

(define minibuf-prompt-delete-prev-char
   (lambda ()
      (let ([p (get-local prompt-pos)])
         (when (> (cursor) p)
            (delete-prev-char)
         )
      )
   )
)

(define minibuf-prompt-move-prev-char
   (lambda ()
      (let ([p (get-local prompt-pos)])
         (when (> (cursor) p)
            (move-prev-char)
         )
      )
   )
)

(define minibuf-prompt-move-next-char
   (lambda ()
      (move-next-char)
   )
)

(define minibuf-prompt-copybuf-paste
   (lambda ()
      (copybuf-paste)
      (move-next-char)
   )
)

(define minibuf-prompt-map
   (let ([map (make-empty-keymap)])
      (bind-key map "<Backspace>" minibuf-prompt-delete-prev-char)
      (bind-key map "<Enter>" minibuf-prompt-send-value)
      (bind-key map "<Esc>" minibuf-cancel-read)
      (bind-key map "C-h" minibuf-prompt-move-prev-char)
      (bind-key map "C-l" minibuf-prompt-move-next-char)
      (bind-key map "C-p" minibuf-prompt-copybuf-paste)
      (bind-key map "C-g q q" do-quit)
      map
   )
)

(define minibuf-answer-yes
   (lambda ()
      (minibuf-send-value 'yes)
   )
)

(define minibuf-answer-no
   (lambda ()
      (minibuf-send-value 'no)
   )
)

(define minibuf-ask-map
   (let ([map (make-empty-keymap)])
      (bind-key map "<Esc>" minibuf-cancel-read)
      (bind-key map "y" minibuf-answer-yes)
      (bind-key map "n" minibuf-answer-no)
      (bind-key map "C-g q q" do-quit)
      map
   )
)

(define minibuf-create
   (lambda ()
      (let ([m (call-foreign (__cs_minibuf_create))])
         (set! minibuf-buffer (window-buffer m))
         (set! minibuf-window m)

         (with-current-buffer minibuf-buffer
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
         (with-current-buffer minibuf-buffer
            (insert (format "~a\n" m))
            (cursor-set 0)
         )
      )
   )
)

(add-hook 'error-hook
   (lambda (e)
      (when minibuf-buffer
         (window-set-height minibuf-window (1+ (lines-count e)))
         (with-current-buffer minibuf-buffer
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
         (with-current-buffer minibuf-buffer
            (when (not (get-local input-mode))
               (window-set-height minibuf-window 1)
               (erase-buffer)
            )
         )
      )
   )
)

(define minibuf-interactive-func
   (lambda (map str def fn)
      (let (
            [b (current-buffer)]
           )
         (with-current-buffer minibuf-buffer
            (erase-buffer)
            (insert str)
            (set-local! prompt-pos (cursor))
            (set-local! input-mode #t)
            (set-local! func-value fn)
            (set-local! orig-buf b)
            (buffer-set-keymap map)
            (when def
               (insert def)
            )
         )
         (window-select minibuf-window)
      )
   )
)

(define minibuf-read
   (case-lambda
      [(str fn)
       (minibuf-read str #f fn)
      ]

      [(str def fn)
       (with-current-buffer minibuf-buffer
          (enable-insert #t)
       )
       (minibuf-interactive-func 'minibuf-prompt-map str def fn)
      ]
   )
)

(define minibuf-ask
   (lambda (str fn)
      (with-current-buffer minibuf-buffer
         (enable-insert #f)
      )
      (minibuf-interactive-func 'minibuf-ask-map (format "~a y/n" str) #f fn)
   )
)

(define minibuf-eval
   (lambda (s)
      (let (
            [code (open-string-input-port s)]
            [ret '()]
            [out ""]
           )
         (set! out (with-output-to-string
                      (lambda ()
                         (set! ret (try eval-port->str code))
                      )
                   )
         )
         (close-port code)
         (set! out (string-append out (second ret)))
         (message out)
      )
   )
)

(define minibuf-cmd
   (lambda ()
      (with-current-buffer minibuf-buffer
         (enable-insert #t)
      )
      (minibuf-interactive-func 'minibuf-prompt-map ":" #f
         (lambda (val)
            (let ([line (string->number val)])
               (if line
                  (move-line-num line)
                  ;; else
                  (minibuf-eval val)
               )
            )
         )
      )
   )
)
