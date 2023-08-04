(library (borsch complete)
   (export
      complete)
   (import (chezscheme)
      (borsch lists)
      (borsch strings)
      (borsch copybuf)
      (borsch keymap)
      (borsch buffer)
      (borsch text)
      (borsch window) )
   
(define (complete-item-name i)
   (if (pair? i)
      (car i)
      ;; else
      i))

(define (complete-item-value i)
   (if (pair? i)
      (cdr i)
      ;; else
      i))

(define (complete-search-word)
   (let ([search-fn (get-local complete-search-word-func)])
      (if search-fn
         (search-fn)
         ;; else
         (get-local complete-text))))

(define (complete-match)
   (let ([text-lst (string-split (complete-search-word) #\space)]
         [lst (get-local complete-list)])
      (set-local! complete-index 0)
      (set-local! complete-result
                  (filter
                     (lambda (x)
                        (let ([nmatch 0])
                           (for-each
                              (lambda (t)
                                 (when (string-contains? (complete-item-name x) t)
                                    (set! nmatch (+ 1 nmatch))))
                              text-lst)
                           (eq? nmatch (length text-lst))))
                     (if (procedure? lst) (lst) lst)))))

(define (complete-draw)
      (text-delete)
      (text-insert (get-local complete-prompt))
      (set-local! complete-prompt-pos (cursor))
      (text-insert (get-local complete-text))
      (with-saved-cursor
         (let* ([lst-idx (get-local complete-index)]
                [lst (get-local complete-result)]
                [sel (modulo lst-idx (get-local complete-max-lines))]
                [line-idx 0])
            (when (< lst-idx (length lst))
               (let* ([max-lines (get-local complete-max-lines)]
                      [skip (* max-lines (fxdiv lst-idx max-lines))])
                  (set! lst (list-tail lst skip))
                  (set! lst (list-head lst (min max-lines (length lst))))))
            (when (not (null? lst))
               (for-each
                  (lambda (x)
                     (let ([name (complete-item-name x)])
                        (if (= line-idx sel)
                           (text-insert (format "\n> ~a" name) '(style: (attr: "bold" fg: "blue")))
                           ;; else
                           (text-insert (format "\n  ~a" name))))
                     (set! line-idx (+ 1 line-idx)))
                  lst)))))

(define (complete-update-text)
   (let ()
      (set-local! complete-prompt-cursor (cursor))
      (set-local! complete-text (text-string (get-local complete-prompt-pos) (text-line-end-pos)))
      (complete-match)
      (complete-draw)))

(define (complete-insert-text char-or-string)
   (if (string? char-or-string) (text-insert char-or-string) (text-insert-char char-or-string))
   (complete-update-text))

(define (complete-delete-prev-char)
   (let ([p (get-local complete-prompt-pos)])
      (when (> (cursor) p)
         (let ()
            (text-delete-to-prev-char)
            (complete-update-text)))))

(define (complete-cancel)
   (set-local! text-insert-hook #f)
   ((get-local complete-fn) #f))

(define (complete-selected-value)
   (let ([lst-idx (get-local complete-index)]
         [result (get-local complete-result)])
      (complete-item-value (first (list-tail result lst-idx)))))

(define (complete-select-value)
   (let ([select-fn (get-local complete-select-func)])
      (if select-fn
         (select-fn)
         ;; else
         (begin
            (set-local! text-insert-hook #f)
            ((get-local complete-fn) (complete-selected-value))))))

(define (complete-choose)
   (let ([choose-fn (get-local complete-choose-func)])
      (if choose-fn
         (choose-fn)
         ;; else
         (complete-insert-text (complete-selected-value)))))

(define (complete-copy-value)
   (copybuf-put (complete-selected-value)))

(define (complete-paste-value)
   (text-insert (string-trim (copybuf-reg) '(#\space #\newline)))
   (complete-update-text))

(define (complete-append-value)
   (copybuf-append (format "\n~a" (complete-selected-value))))

(define (complete-prompt-move-prev-char)
   (let ([p (get-local complete-prompt-pos)])
      (when (> (cursor) p)
         (cursor-to-prev-char))
      (set-local! complete-prompt-cursor (cursor))))

(define (complete-prompt-move-next-char)
   (when (< (cursor) (text-line-end-pos))
      (cursor-to-next-char)
      (set-local! complete-prompt-cursor (cursor))))

(define (complete-list-move-down)
   (let ([result (get-local complete-result)]
         [idx (get-local complete-index)])
      (when (and (not (null? result))
                 (< idx (- (length result) 1)))
         (set-local! complete-index (+ 1 idx))
         (complete-draw))))

(define (complete-list-move-up)
   (let ([result (get-local complete-result)]
         [idx (get-local complete-index)])
      (when (and (not (null? result))
                 (> idx 0))
         (set-local! complete-index (- idx 1))
         (complete-draw))))

(define complete-map
   (let ([map (make-empty-keymap)])
      (bind-key map "<Backspace>" complete-delete-prev-char)
      (bind-key map "<Enter>" complete-select-value)
      (bind-key map "<Esc>" complete-cancel)
      (bind-key map "<Tab>" complete-choose)
      (bind-key map "C-y" complete-copy-value)
      (bind-key map "C-p" complete-paste-value)
      (bind-key map "C-a" complete-append-value)
      (bind-key map "C-h" complete-prompt-move-prev-char)
      (bind-key map "C-l" complete-prompt-move-next-char)
      (bind-key map "C-j" complete-list-move-down)
      (bind-key map "C-k" complete-list-move-up)
      ;;(bind-key map "C-g q q" do-quit)
      map))

(define complete
   (case-lambda
      [(ls fn)
       (complete ls fn "")]

      [(ls fn prompt)
       (complete ls fn prompt "")]

      [(ls fn prompt init)
       (let ()
         (define-local text-insert-hook complete-insert-text)
         (define-local complete-prompt-cursor 0)
         (define-local complete-prompt-pos 0)
         (define-local complete-prompt (format "~a:" prompt))
         (define-local complete-text "")
         (define-local complete-index 0)
         (define-local complete-max-lines (- (window-inner-height) 1))
         (define-local complete-result '())
         (define-local complete-list ls)
         (define-local complete-fn fn)
         (define-local complete-search-word-func #f)
         (define-local complete-choose-func #f)
         (define-local complete-select-func #f)
         (buffer-set-keymap complete-map)
         (enable-insert #t)
         (when init (init))
         (complete-match)
         (complete-draw)
         (cursor-set (get-local complete-prompt-pos))
         (set-local! complete-prompt-cursor (cursor)))]))
)
