(define __cs_buf_new (foreign-procedure "cs_buf_new" (string) scheme-object))
(define __cs_buf_ref_get (foreign-procedure "cs_buf_ref_get" (int) scheme-object))
(define __cs_buf_ref_put (foreign-procedure "cs_buf_ref_put" (int) scheme-object))
(define __cs_buf_ref (foreign-procedure "cs_buf_ref" (int) scheme-object))
(define __cs_buf_is_valid (foreign-procedure "cs_buf_is_valid" (int) scheme-object))
(define __cs_buf_del (foreign-procedure "cs_buf_del" (int) void))
(define __cs_buf_kmap_get (foreign-procedure "cs_buf_kmap_get" (int) scheme-object))
(define __cs_buf_kmap_set (foreign-procedure "cs_buf_kmap_set" (int string) scheme-object))
(define __cs_buf_text_input_enable (foreign-procedure "cs_buf_text_input_enable" (int boolean) void))

(define __cs_buf_file_open (foreign-procedure "cs_buf_file_open" (int string) scheme-object))

(define __cs_buf_is_visible (foreign-procedure "cs_buf_is_visible" (int) scheme-object))
(define __cs_buf_is_term (foreign-procedure "cs_buf_is_term" (int) scheme-object))
(define __cs_buf_term_set (foreign-procedure "cs_buf_term_set" (int int) void))

(define %dir-locals-ht (make-hashtable string-hash string=?))

(define %buffer-list% (list))

(define file-match-mode (list))

(define (mode-gen-map-symb m)
   (string->symbol
      (string-append (symbol->string m) "-map")))

(define (mode-gen-map-value m)
   (let ([s (mode-gen-map-symb m)])
      (if (top-level-bound? s)
         (top-level-value s)
         ;; else
         #f)))

(define (mode-gen-hook-symb m)
   (string->symbol
      (string-append (symbol->string m) "-hook")))

(define (mode-gen-hook-value h)
   (let ([s (mode-gen-hook-symb h)])
      (if (top-level-bound? s)
         (top-level-value s)
         ;; else
         #f)))

(define-syntax (define-mode stx)
   (syntax-case stx ()
      ((_ mode name parent exp ...)
       #`(define-top-level-value 'mode
            (lambda ()
               (when parent
                  ((top-level-value 'parent)))
               (define-local major-mode 'mode)
               (let ([m-map (mode-gen-map-symb 'mode)])
                  (when (top-level-bound? m-map)
                     (when parent
                        (let ([p-map (mode-gen-map-symb 'parent)])
                           (if (local-symbol-bound? p-map)
                              (keymap-set-parent (top-level-value m-map) (get-local-symbol p-map))
                              ;; else
                              (keymap-set-parent (top-level-value m-map) p-map))))
                     (buffer-set-keymap m-map))
                  (buffer-set-mode-name name)
                  exp
                  ...
                  (run-hooks
                     (mode-gen-hook-symb 'mode))))))))

(define (%buffer-local-keymap)
   (call-foreign (__cs_buf_kmap_get (current-buffer))))

(define (buffer-keymap)
   (keymap-parent (%buffer-local-keymap)))

(define (buffer-set-keymap sym)
   (let ([lmap (%buffer-local-keymap)])
      (keymap-set-parent lmap sym)))

(define *buffer-enable-eof* #t)

(define dir-local-symbol-bound?
   (case-lambda
      [(sym)
       (dir-local-symbol-bound? (current-cwd) sym)]

      [(dir sym)
       (let ([dir-env (hashtable-ref %dir-locals-ht dir #f)])
          (and dir-env (top-level-bound? sym dir-env)))]))

(define (dir-get-local-symbol sym)
   (let ([dir-env (hashtable-ref %dir-locals-ht (path-parent (buffer-filename)) #f)])
      (if (and dir-env (top-level-bound? sym dir-env))
         (top-level-value sym dir-env)
         ;; else
         (let ([cwd-env (hashtable-ref %dir-locals-ht (current-cwd) #f)])
            (top-level-value sym cwd-env)))))

(define (dir-set-local-symbol! dir sym val)
   (let ([dir-env (hashtable-ref %dir-locals-ht dir #f)])
      (if dir-env
         (set-top-level-value! sym val dir-env)
         ;; else
         (let ([env (copy-environment (scheme-environment))])
            (hashtable-set! %dir-locals-ht dir env)
            (define-top-level-value sym val env)))))

(define-syntax (dir-set-local! stx)
   (syntax-case stx ()
      ((_ dir sym val)
       #`(dir-set-local-symbol! dir 'sym val))))

(define (local-symbol-bound? sym)
   (or (top-level-bound? sym (buffer-env))
       (dir-local-symbol-bound? (current-cwd) sym)
       (dir-local-symbol-bound? (path-parent (buffer-filename)) sym)))

(define (get-local-symbol sym)
   (if (top-level-bound? sym (buffer-env))
      (top-level-value sym (buffer-env))
      ;; else
      (dir-get-local-symbol sym)))

(define (set-local-symbol! sym val)
   (set-top-level-value! sym val (buffer-env)))

(define-syntax (get-local stx)
   (syntax-case stx ()
      ((_ s)
       #`(get-local-symbol 's))
      ((_ s e)
       #`(if (local-bound? s) (get-local s) e))))

(define-syntax (set-local! stx)
   (syntax-case stx ()
      ((_ s v)
       #`(set-local-symbol! 's v))))

(define-syntax (local-bound? stx)
   (syntax-case stx ()
      ((_ s)
       #`(local-symbol-bound? 's))))

(define (buffer-insert b)
   (set! %buffer-list% (append %buffer-list% (list b)))
   (frame-insert-buffer b))

(define (buffer-remove b)
   (set! %buffer-list% (remove b %buffer-list%))
   (frame-remove-buffer b))

(define buffer-ref-count
   (case-lambda
      [()
       (buffer-ref-count (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_ref buf))]))

(define buffer-ref-get
   (case-lambda
      [()
       (buffer-ref-get (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_ref_get buf))]))

(define buffer-ref-put
   (case-lambda
      [()
       (buffer-ref-put (current-buffer))]

      [(buf)
       (when (= 1 (buffer-ref-count buf))
          (buffer-remove buf))
       (call-foreign (__cs_buf_ref_put buf))]))

(define buffer-new
   (case-lambda
      [() 
       (buffer-new "")]

      [(n) 
       (let ([b (call-foreign (__cs_buf_new n))])
          (buffer-insert b)
          (with-current-buffer b
             (set-text-style '(fg: "white")))
          b)]))

(define buffer-create
   (case-lambda
      [() 
       (buffer-create "")]

      [(n) 
       (let ([b (buffer-new n)])
          (buffer-set-name b n)
          (window-create b)
          b)]))

(define (buffer-open b)
   (or (buffer-is-visible? b) (window-create b)))

(define buffer-create-text
   (case-lambda
      [() 
       (let ([b (buffer-create)])
          (text-mode)
          b)]

      [(n) 
       (let ([b (buffer-create n)])
          (text-mode)
          b)]))

(define (buffer-is-valid? bid)
   (call-foreign (__cs_buf_is_valid bid)))

(define buffer-delete
   (case-lambda
      [()
       (buffer-delete (current-buffer))]

      [(b)
       (call-foreign (__cs_buf_del b))
       (buffer-remove b)]))

(define (buffer-get-or-create name)
   (or (buffer-get name)
       (buffer-create name)))

(define (buffer-open-file f)
   (let ([bid (buffer-get-by-file f)]
         [in-frame? #f]
         [wid 0])
      (if bid
         (let ()
            (set! wid (buffer-window bid))
            (for-all
               (lambda (w)
                  (when (equal? w wid)
                     (set! in-frame? #t)
                     #f))
               (window-list))
            (if in-frame?
               (begin
                  (window-focus wid))
               ;; else
               (window-create bid))
            bid)
         ;; else
         (let* ([b (buffer-create)]
                [ok (call-foreign (__cs_buf_file_open b f))])
            (with-current-buffer b
               (text-mode)
               (when ok
                  (for-each
                     (lambda (match)
                        (let ([fname (buffer-filename)])
                           (when (pregexp-match (car match) fname)
                              ((top-level-value (cdr match))))))
                     file-match-mode)))
            b))))

(define (buffer-reload)
   (when (local-bound? buffer-reload-func)
      ((get-local buffer-reload-func))))

(define (buffer-list)
   %buffer-list%)

(define (buffer-for-each fn)
   (for-each
      (lambda (b)
         (fn b))
      (buffer-list)))

(define (buffer-find fn)
   (let ([b (find
               (lambda (b)
                  (fn b))
               (buffer-list))])
      b))

(define (buffer-get-by-file file)
   (buffer-find
      (lambda (b)
         (equal? file (buffer-filename b)))))

(define (enable-insert e)
   (call-foreign (__cs_buf_text_input_enable (current-buffer) e)))

(define buffer-is-visible?
   (case-lambda
      [()
       (call-foreign (__cs_buf_is_visible (current-buffer)))]

      [(b)
       (call-foreign (__cs_buf_is_visible b))]))

(define buffer-set-vterm
   (case-lambda
      [(pid)
       (buffer-set-vterm (current-buffer) pid)]

      [(b pid)
       (call-foreign (__cs_buf_term_set b pid))]))

(define buffer-is-vterm?
   (case-lambda
      [()
       (call-foreign (__cs_buf_is_term (current-buffer)))]

      [(b)
       (call-foreign (__cs_buf_is_term b))]))

(define buffer-window
   (case-lambda
      [()
       (buffer-window (current-buffer))]

      [(b)
       (let ([win-lst (filter
                         (lambda (w)
                            (equal? (window-buffer w) b))
                         (window-list))])
          (if (null? win-lst)
             #f
             ;; else
             (first win-lst)))]))

(define (buffer-set-cwd cwd)
   (define-local current-cwd cwd))

(define (buffer-cwd)
   (if (and (local-bound? current-cwd) (get-local current-cwd))
      (get-local current-cwd)
      ;; else
      (current-cwd)))

(define (buffer-run)
   (let ([fname (buffer-filename)])
      (if (file-is-executable? fname)
         (vterm (format "~a ; read" fname))
         ;; else
         (message (format "File is not executable: ~a" fname))
      )))

(define (file-open p)
   (let ([p (path-expand p)])
      (if (file-regular? p)
         (buffer-open-file p)
         ;; else
         (if (file-directory? p)
            (dirb p)
            ;; else
            (message (format "path does not exist: ~a" p))))))
