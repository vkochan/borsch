(define complete-item-name
   (lambda (i)
      (if (pair? i)
         (car i)
         ;; else
         i
      )
   )
)

(define complete-item-value
   (lambda (i)
      (if (pair? i)
         (cdr i)
         ;; else
         i
      )
   )
)

(define complete-search-word
   (lambda ()
      (let (
            [search-fn (get-local complete-search-word-func)]
           )
         (if search-fn
            (search-fn)
            ;; else
            (get-local complete-text)
         )
      )
   )
)

(define complete-match
   (lambda ()
      (let (
            [text-lst (string-split (complete-search-word) #\space)]
            [lst (get-local complete-list)]
           )
         (set-local! complete-index 0)
         (set-local! complete-result
                     (filter
                        (lambda (x)
                           (let ([nmatch 0])
                              (for-each
                                 (lambda (t)
                                    (when (string-contains? (complete-item-name x) t)
                                       (set! nmatch (1+ nmatch))
                                    )
                                 ) text-lst
                              )
                              (eq? nmatch (length text-lst))
                           )
                        ) (if (procedure? lst) (lst) lst)
                     )
         )
      )
   )
)

(define complete-draw
   (lambda ()
      (text-delete)
      (text-insert (get-local complete-prompt))
      (set-local! complete-prompt-pos (cursor))
      (text-insert (get-local complete-text))

      (save-cursor
      (let* (
             [lst-idx (get-local complete-index)]
             [lst (get-local complete-result)]
             [sel (% lst-idx (get-local complete-max-lines))]
             [line-idx 0]
            )
         (when (< lst-idx (length lst))
            (let* (
                   [max-lines (get-local complete-max-lines)]
                   [skip (* max-lines (fxdiv lst-idx max-lines))]
                  )
               (set! lst (list-tail lst skip))
               (set! lst (list-head lst (min max-lines (length lst))))
            )
         )
         (when (not (null? lst))
            (for-each
               (lambda (x)
                  (let ([name (complete-item-name x)])
                     (if (= line-idx sel)
                        (text-insert (format "\n> ~a" name) '(:style (:attr "bold" :fg "blue")))
                        ;; else
                        (text-insert (format "\n  ~a" name))
                     )
                  )
                  (set! line-idx (1+ line-idx))
               ) lst
            )
         )
      )
      )
      (cursor-set (get-local complete-prompt-cursor))
   )
)

(define complete-update-text
   (lambda ()
      (let ()
         (set-local! complete-prompt-cursor (cursor))
         (set-local! complete-text (text-string (get-local complete-prompt-pos) (text-line-end-pos)))
         (complete-match)
         (complete-draw)
      )
   )
)

(define complete-insert-text
   (lambda (char-or-string)
      (if (string? char-or-string) (text-insert char-or-string) (text-insert-char char-or-string))
      (complete-update-text)
   )
)

(define complete-delete-prev-char
   (lambda ()
      (let (
            [p (get-local complete-prompt-pos)]
           )
         (when (> (cursor) p)
            (let ()
               (text-delete-prev-char)
               (complete-update-text)
            )
         )
      )
   )
)

(define complete-cancel
   (lambda ()
      (set-local! text-insert-hook #f)
      ((get-local complete-fn) #f)
   )
)

(define complete-selected-value
   (lambda ()
      (let (
            [lst-idx (get-local complete-index)]
            [result (get-local complete-result)]
           )
         (complete-item-value (first (list-tail result lst-idx)))
      )
   )
)

(define complete-select-value
   (lambda ()
      (let (
            [select-fn (get-local complete-select-func)]
           )
         (if select-fn
            (select-fn)
            ;; else
            (begin
               (set-local! text-insert-hook #f)
               ((get-local complete-fn) (complete-selected-value))
            )
         )
      )
   )
)

(define complete-choose
   (lambda ()
      (let (
            [choose-fn (get-local complete-choose-func)]
           )
         (if choose-fn
            (choose-fn)
            ;; else
            (complete-insert-text (complete-selected-value))
         )
      )
   )
)

(define complete-copy-value
   (lambda ()
      (copybuf-put (complete-selected-value))
   )
)

(define complete-paste-value
   (lambda ()
      (copybuf-paste)
      (complete-update-text)
   )
)

(define complete-append-value
   (lambda ()
      (copybuf-append (format "\n~a" (complete-selected-value)))
   )
)

(define complete-prompt-move-prev-char
   (lambda ()
      (let ([p (get-local complete-prompt-pos)])
         (when (> (cursor) p)
            (cursor-goto-prev-char)
         )
         (set-local! complete-prompt-cursor (cursor))
      )
   )
)

(define complete-prompt-move-next-char
   (lambda ()
      (when (< (cursor) (text-line-end-pos))
         (cursor-goto-next-char)
         (set-local! complete-prompt-cursor (cursor))
      )
   )
)

(define complete-list-move-down
   (lambda ()
      (let (
            [result (get-local complete-result)]
            [idx (get-local complete-index)]
           )
         (when (and (not (null? result))
                    (< idx (- (length result) 1))
               )
            (set-local! complete-index (1+ idx))
            (complete-draw)
         )
      )
   )
)

(define complete-list-move-up
   (lambda ()
      (let (
            [result (get-local complete-result)]
            [idx (get-local complete-index)]
           )
         (when (and (not (null? result))
                    (> idx 0)
               )
            (set-local! complete-index (1- idx))
            (complete-draw)
         )
      )
   )
)

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
      (bind-key map "C-g q q" do-quit)
      map
   )
)

(define complete
   (case-lambda
      [(ls fn)
       (complete ls fn "")
      ]

      [(ls fn prompt)
       (complete ls fn prompt "")
      ]

      [(ls fn prompt init)
       (let ()
         (define-local text-insert-hook complete-insert-text)
         (define-local complete-prompt-cursor 0)
         (define-local complete-prompt-pos 0)
         (define-local complete-prompt (format "~a:" prompt))
         (define-local complete-text "")
         (define-local complete-index 0)
         (define-local complete-max-lines (1- (window-inner-height)))
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
         (set-local! complete-prompt-cursor (cursor))
       )
      ]
   )
)
