(library (borsch vterm)
   (export
      vterm-send-keys
      vterm-send-text
      vterm-string
      vterm-current-line
      vterm
      vterm-mode-map)
   (import
      (chezscheme)
      (borsch base)
      (borsch buffer)
      (borsch copybuf)
      (borsch window)
      (borsch keymap)
      (borsch process)
      (borsch text)
      (borsch strings))

(define c_tcgetpgrp (foreign-procedure "tcgetpgrp" (int) int))

(define __cs_term_keys_send (foreign-procedure "cs_term_keys_send" (int string) int))
(define __cs_term_text_get (foreign-procedure "cs_term_text_get" (int) scheme-object))
(define __cs_term_current_line_get (foreign-procedure "cs_term_current_line_get" (int) scheme-object))

(define vterm-send-keys
   (case-lambda
      [(keys)
       (vterm-send-keys (current-buffer) keys)]

      [(buf keys)
       (call-foreign (__cs_term_keys_send (buffer-id buf) keys)) ]))

(define (vterm-send-text text)
   (process-send-text (get-local process) text))

(define vterm-string
   (case-lambda
      [()
       (vterm-string (current-buffer))]

      [(buf)
       (call-foreign (__cs_term_text_get (buffer-id buf))) ]))

(define vterm-current-line
   (case-lambda
      [()
       (vterm-current-line (current-buffer))]

      [(buf)
       (call-foreign (__cs_term_current_line_get (buffer-id buf))) ]))

(define (vterm-mode-copy-enter)
   (let* ([b (make-buffer)]
          [c (current-buffer)]
          [s (vterm-string c)])
      (with-current-buffer b
         (bind-key-local "<Esc>" vterm-mode-copy-exit)
         (bind-key-local "q" vterm-mode-copy-exit)
         (buffer-set-name "Term Copy")
         (define-local orig-buf c))
      (window-switch-buffer b)
      (current-buffer b)
      (text-insert s)))

(define (vterm-mode-copy-exit)
   (let ([c (current-buffer)])
      (window-switch-buffer (get-local orig-buf))
      (delete-buffer c)))

(define (vterm-mode-paste)
   (vterm-send-text (copybuf-reg)))

(define (vterm-mode-paste-clipboard)
   (vterm-send-text (copybuf-clip-get)))

(define vterm-mode-map
  (let ([map (make-keymap)])
     (bind-key map "C-y" vterm-mode-copy-enter)
     (bind-key map "C-p" vterm-mode-paste)
     (bind-key map "C-v" vterm-mode-paste-clipboard)
     map))

(define (vterm-sync-title)
   (let*([proc     (get-local process)]
         [pid_fg   (c_tcgetpgrp (process-pty proc))] )
      (when (not (negative? pid_fg))
         (buffer-set-name
            (call-with-input-file (format "/proc/~a/cmdline" pid_fg)
               (lambda (p)
                  (get-string-some p) ))))))

(define vterm
   (case-lambda
      [()
       (vterm #f "" (current-cwd))]

      [(prog)
       (vterm prog "" (current-cwd))]

      [(prog title)
       (vterm prog title (current-cwd))]

      [(prog title cwd)
       (let ([p (with-current-cwd cwd (make-process [cmd: prog] [pty?: #t]))]
             [b (make-buffer)])
          (with-current-buffer b
             (define-local major-mode 'vterm-mode)
             (define-local linenum-enable #f)
             (define-local process p)
             (set-local! pre-draw-func vterm-sync-title)
             (buffer-set-keymap 'vterm-mode-map)
             (buffer-set-mode-name "VTerm")
             (buffer-set-vterm (process-pid p))
             (when (not (string-empty? title))
                (buffer-set-name title)))
          (window-create b)
          b)]))
)
