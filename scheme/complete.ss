(define complete-match
   (lambda ()
      (let (
            [text (get-local complete-text)]
            [lst (get-local complete-list)]
           )
         (set-local! complete-index 0)
         (set-local! complete-result
                     (filter
                        (lambda (x)
                           (string-contains? x text)
                        ) lst
                     )
         )
      )
   )
)

(define complete-draw
   (lambda ()
      (erase-buffer)
      (insert (get-local complete-prompt))
      (set-local! complete-prompt-pos (cursor))
      (insert (get-local complete-text))

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
                  (if (= line-idx sel)
                     (insert (format "\n> ~a" x) '(style (:attr "bold" :fg "blue")))
                     ;; else
                     (insert (format "\n  ~a" x))
                  )
                  (set! line-idx (1+ line-idx))
               ) lst
            )
         )
      )

      (cursor-set (get-local complete-prompt-cursor))
   )
)

(define complete-insert-char
   (lambda (char)
      (insert (string char))
      (set-local! complete-prompt-cursor (cursor))
      (set-local! complete-text (buffer-string (get-local complete-prompt-pos) (line-end-pos)))
      (complete-match)
      (complete-draw)
   )
)

(define complete-prompt-delete-prev-char
   (lambda ()
      (let ([p (get-local complete-prompt-pos)])
         (when (> (cursor) p)
            (delete-prev-char)
            (set-local! complete-text (extract-line-inner (get-local complete-prompt-pos)))
            (set-local! complete-prompt-cursor (cursor))
            (set-local! complete-text (buffer-string (get-local complete-prompt-pos) (line-end-pos)))
            (complete-match)
            (complete-draw)
         )
      )
   )
)

(define complete-cancel
   (lambda ()
      ((get-local complete-fn) #f)
   )
)

(define complete-select-value
   (lambda ()
      (let (
            [lst-idx (get-local complete-index)]
            [result (get-local complete-result)]
           )
         ((get-local complete-fn) (first (list-tail result lst-idx)))
      )
   )
)

(define complete-prompt-move-prev-char
   (lambda ()
      (let ([p (get-local complete-prompt-pos)])
         (when (> (cursor) p)
            (move-prev-char)
         )
         (set-local! complete-prompt-cursor (cursor))
      )
   )
)

(define complete-prompt-move-next-char
   (lambda ()
      (move-next-char)
      (set-local! complete-prompt-cursor (cursor))
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
      (bind-key map "<Backspace>" complete-prompt-delete-prev-char)
      (bind-key map "<Enter>" complete-select-value)
      (bind-key map "<Esc>" complete-cancel)
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
       (let ()
         (define-local text-insert-hook complete-insert-char)
         (define-local complete-prompt-cursor 0)
         (define-local complete-prompt-pos 0)
         (define-local complete-prompt (format "~a:" prompt))
         (define-local complete-text "")
         (define-local complete-index 0)
         (define-local complete-max-lines 10)
         (define-local complete-result '())
         (define-local complete-list ls)
         (define-local complete-fn fn)
         (buffer-set-keymap complete-map)
         (enable-insert #t)
         (complete-match)
         (complete-draw)
         (cursor-set (get-local complete-prompt-pos))
         (set-local! complete-prompt-cursor (cursor))
       )
      ]
   )
)
