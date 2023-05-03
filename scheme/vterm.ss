(define __cs_term_keys_send (foreign-procedure "cs_term_keys_send" (int string) int))
(define __cs_term_text_get (foreign-procedure "cs_term_text_get" (int) scheme-object))
(define __cs_term_current_line_get (foreign-procedure "cs_term_current_line_get" (int) scheme-object))

(define vterm-send-keys
   (case-lambda
      [(keys)
       (call-foreign (__cs_term_keys_send (current-buffer) keys))]

      [(bid keys)
       (call-foreign (__cs_term_keys_send bid keys))]))

(define (vterm-send-text text)
   (process-send-text (get-local process) text))

(define vterm-string
   (case-lambda
      [()
       (vterm-string (current-buffer))]

      [(b)
       (call-foreign (__cs_term_text_get b))]))

(define vterm-current-line
   (case-lambda
      [()
       (vterm-current-line (current-buffer))]

      [(b)
       (call-foreign (__cs_term_current_line_get b))]))

(define (vterm-mode-copy-enter)
   (let* ([b (buffer-new)]
          [c (current-buffer)]
          [s (vterm-string c)])
      (with-current-buffer b
         (text-mode)
         (bind-key-local "<Esc>" vterm-mode-copy-exit)
         (bind-key-local "q" vterm-mode-copy-exit)
         (buffer-set-name "Term Copy")
         (define-local orig-buf c)
         (window-switch-buffer b)
         (text-insert s))))

(define (vterm-mode-copy-exit)
   (let ([c (current-buffer)])
      (window-switch-buffer (get-local orig-buf))
      (buffer-delete c)))

(define (vterm-mode-paste)
   (vterm-send-text copybuf-reg))

(define (vterm-mode-paste-clipboard)
   (vterm-send-text (copybuf-clip-get)))

(define vterm-mode-map
  (let ([map (make-keymap)])
     (bind-key map "C-y" vterm-mode-copy-enter)
     (bind-key map "C-p" vterm-mode-paste)
     (bind-key map "C-v" vterm-mode-paste-clipboard)
     map))

(define vterm
   (case-lambda
      [()
       (vterm #f "" (current-cwd))]

      [(prog)
       (vterm prog "" (current-cwd))]

      [(prog title)
       (vterm prog title (current-cwd))]

      [(prog title cwd)
       (let ([p (with-current-cwd cwd (process-create prog #f #f #f #t #t))]
             [b (buffer-new)])
          (with-current-buffer b
             (define-local major-mode 'vterm-mode)
             (define-local vterm-filter-func #f)
             (define-local process p)
             (buffer-set-keymap 'vterm-mode-map)
             (buffer-set-mode-name "VTerm")
             (buffer-set-vterm (process-pid p))
             (when (not (string-empty? title))
                (buffer-set-name title)))
          (window-create b)
          b)]))
