(define minibuf-window #f)
(define minibuf-buffer #f)

(define (minibuf-select-previous)
   (let ([prev-sel (window-prev-selected)])
      (buffer-set-keymap 1)
      (window-select prev-sel)))

(define (minibuf-cancel-read)
   (enable-insert #f)
   (set-local! input-mode #f)
   (text-delete)
   (minibuf-select-previous)
)

(define (minibuf-send-value v)
   (let ([fn (get-local func-value)]
         [b  (get-local orig-buf)])
      (set-local! input-mode #f)
      (enable-insert #f)
      (text-delete)
      (minibuf-select-previous)
      (fn v)))

(define (minibuf-prompt-send-value)
   (let ([s  (text-string (get-local prompt-pos) (text-end-pos))])
      (minibuf-send-value s)))

(define (minibuf-prompt-delete-prev-char)
   (let ([p (get-local prompt-pos)])
      (when (> (cursor) p)
         (text-delete-to-prev-char))))

(define (minibuf-prompt-move-prev-char)
   (let ([p (get-local prompt-pos)])
      (when (> (cursor) p)
         (cursor-to-prev-char))))

(define (minibuf-prompt-move-next-char)
   (cursor-to-next-char))

(define (minibuf-prompt-copybuf-paste)
   (copybuf-paste)
   (cursor-to-next-char))

(define minibuf-prompt-map
   (let ([map (make-empty-keymap)])
      (bind-key map "<Backspace>" minibuf-prompt-delete-prev-char)
      (bind-key map "<Enter>" minibuf-prompt-send-value)
      (bind-key map "<Esc>" minibuf-cancel-read)
      (bind-key map "C-h" minibuf-prompt-move-prev-char)
      (bind-key map "C-l" minibuf-prompt-move-next-char)
      (bind-key map "C-p" minibuf-prompt-copybuf-paste)
      (bind-key map "C-g q q" do-quit)
      map))

(define (minibuf-answer-yes)
   (minibuf-send-value 'yes))

(define (minibuf-answer-no)
   (minibuf-send-value 'no))

(define minibuf-ask-map
   (let ([map (make-empty-keymap)])
      (bind-key map "<Esc>" minibuf-cancel-read)
      (bind-key map "y" minibuf-answer-yes)
      (bind-key map "n" minibuf-answer-no)
      (bind-key map "C-g q q" do-quit)
      map))

(define (minibuf-insert-char char)
   (text-insert-char char))

(define (minibuf-create)
   (let ([m (widget-create "*minibuf*" 0 (- (ui-screen-height) 1) (ui-screen-width) 1 'bottom)])
      (set! minibuf-buffer (window-buffer m))
      (set! minibuf-window m)
      (with-current-buffer minibuf-buffer
         (define-local text-insert-hook minibuf-insert-char)
         (define-local input-mode #f)
         (define-local func-value #f)
         (define-local prompt-pos 0)
         (define-local orig-buf 0)
         (define-local orig-win 0)
         (enable-insert #t))))

(add-hook 'message-hook
   (lambda (m)
      (when minibuf-buffer
         (window-set-height minibuf-window (1+ (lines-count m)))
         (with-current-buffer minibuf-buffer
            (text-insert (format "~a\n" m))
            (cursor-set 0)))))

(add-hook 'error-hook
   (lambda (e)
      (when minibuf-buffer
         (window-set-height minibuf-window (1+ (lines-count e)))
         (with-current-buffer minibuf-buffer
            (when (not (get-local input-mode))
               (text-insert (format "~a\n" e) '(style: (fg: "red")))
               (cursor-set 0))))))

(add-hook 'key-press-hook
   (lambda (k)
      (when minibuf-buffer
         (with-current-buffer minibuf-buffer
            (when (not (get-local input-mode))
               (window-set-height minibuf-window 1)
               (text-delete))))))

(define minibuf-interactive-func
   (lambda (map str def fn)
      (let ([b (current-buffer)])
         (with-current-buffer minibuf-buffer
            (text-delete)
            (text-insert str)
            (set-local! text-insert-hook minibuf-insert-char)
            (set-local! prompt-pos (cursor))
            (set-local! input-mode #t)
            (set-local! func-value fn)
            (set-local! orig-buf b)
            (buffer-set-keymap map)
            (when def
               (text-insert def)))
         (window-select minibuf-window))))

(define minibuf-read
   (case-lambda
      [(str fn)
       (minibuf-read str #f fn)]

      [(str def fn)
       (with-current-buffer minibuf-buffer
          (enable-insert #t))
       (minibuf-interactive-func 'minibuf-prompt-map str def fn)]))

(define (minibuf-ask str fn)
   (with-current-buffer minibuf-buffer
      (enable-insert #f))
   (minibuf-interactive-func 'minibuf-ask-map (format "~a y/n" str) #f fn))

(define (try-eval->string s)
    (let ([code (open-string-input-port s)]
          [ret '()]
          [out ""])
       (set! out (with-output-to-string
                    (lambda ()
                       (set! ret (try eval-port->str code)))))
       (close-port code)
       (string-append out (second ret))))

(define (minibuf-eval)
   (with-current-buffer minibuf-buffer
      (enable-insert #t))
   (minibuf-interactive-func 'minibuf-prompt-map "Eval:" #f
      (lambda (val)
         (message (try-eval->string val)))))

(define (minibuf-clear)
   (let ([b (get-local orig-buf)]
         [w (get-local orig-win)])
      (enable-insert #f)
      (text-delete)
      (set-local! input-mode #f)
      (window-set-height minibuf-window 1)
      (minibuf-select-previous)))

(define (minibuf-complete-handle o)
   (let ([fn (get-local func-value)])
      (minibuf-clear)
      (when o
         (fn o))))

(define minibuf-complete
   (case-lambda
     [(lst fn)
      (minibuf-complete lst fn "")]

     [(lst fn prompt)
      (minibuf-complete lst fn prompt #f)]
     
     [(lst fn prompt init)
      (let ([b (current-buffer)])
         (with-current-buffer minibuf-buffer
            (set-local! input-mode #t)
            (set-local! func-value fn)
            (set-local! orig-buf (if (equal? minibuf-buffer (current-buffer)) #f (current-buffer)))
            (set-local! orig-win (if (equal? minibuf-window (current-window)) #f (current-window)))
            (window-set-height minibuf-window 11)
            (window-select minibuf-window)
            (complete lst minibuf-complete-handle prompt init)))]))

(define minibuf-complete-path
   (case-lambda
      [(fn)
       (minibuf-complete-path fn "")]
      
      [(fn prompt)
       (let ()
          (minibuf-complete
             (lambda ()
                (let ([root (get-local minibuf-complete-path-root)]
                      [text (get-local complete-text)])
                   (list-dir
                      (string-append
                         (if (path-absolute? text) "" root)
                         (path-parent text)))))
             #f
             prompt
             (lambda ()
                (define-local minibuf-complete-path-root (string-append (current-cwd) "/"))
                (define-local minibuf-complete-path-func fn)
                (define-local complete-search-word-func
                   (lambda ()
                      (path-last (get-local complete-text))))
                (define-local complete-choose-func
                   (lambda ()
                      (let ([text (path-parent (get-local complete-text))])
                         (set-local! complete-text
                            (string-append
                               text
                               (if (or (path-absolute? text) (string-empty? text))
                                  ""
                                  ;; else
                                  "/")
                               (complete-selected-value)))
                         (complete-match)
                         (complete-draw))))
                (define-local complete-select-func
                   (lambda ()
                      (let ([fn (get-local minibuf-complete-path-func)]
                            [root (get-local minibuf-complete-path-root)]
                            [text (path-parent (get-local complete-text))]
                            [val (complete-selected-value)])
                         (minibuf-clear)
                         (fn
                            (string-append
                               (if (path-absolute? text) "" root)
                               text "/" val ))))))))]))

(define minibuf-find-file
   (case-lambda
      [()
       (minibuf-find-file (current-cwd))]
      
      [(cwd)
       (minibuf-find-file (current-cwd) "")]

      [(cwd opts)
       (let ([find-cmd (format "cd ~a; find * ~a -not -path '*/\\.*';" cwd opts)]
             [flist '()])
          (with-process-temp-buffer find-cmd
             (minibuf-complete
                (string-split (text-string) #\newline)
                (lambda (f)
                   (file-open (string-append cwd "/" f)))
                "Find file")))]))
