(define-record-type mail-entry
   (fields
      id
      date
      from
      to
      cc
      subject
   )
)

(define mail-is-syncing? #f)

(define mail-user "")

(define mail-timer #f)

(define mail-start-sync
   (lambda ()
      (when (not mail-timer)
         (set! mail-timer
            (make-timer (* 10 60 1000)
               (lambda ()
                  (when (not mail-is-syncing?)
                     (set! mail-is-syncing? #t)
                     (message "Syncing mail ...")
                     (process-create
                        "mbsync -a && notmuch new" #f #f
                        (lambda (status buf-out buf-err)
                           (set! mail-is-syncing? #f)
                           (message "Mail synced")
                        )
                     )
                  )
               )
            )
         )
      )
   )
)

(define mail-enable
   (lambda ()
      (mail-start-sync)
   )
)
 
(define-record-type mail-entry
   (fields
      id
      date
      from
      to
      cc
      subject
   )
)

(define mail-open-entry
   (lambda ()
      (let ([plist (get-property 'data (1+ (cursor)) (+ 2 (cursor)))])
         (let ([entry (cdr (assoc ':data (first plist)))])
            (let ([b (buffer-create (format "Message:~a" (mail-entry-id entry)))])
               (text-mode)
               (message "Loading mail ...")
               (process-create (format "notmuch show --format=mbox id:~a" (mail-entry-id entry)) b
                  (lambda (status buf-out buf-err)
                     (with-current-buffer buf-out
                        (move-buffer-begin)
                        (let loop ()
                           (let ([l (extract-line)])
                              (when (not (pregexp-match "^\n|^\r\n" l))
                                 (if (not (pregexp-match "^From:|^FROM:|^To:|^TO:|^Cc:|^CC:|^Date:|^DATE:|^Subject:|^SUBJECT:" l))
                                    (delete-line)
                                    ;; else
                                    (move-next-line)
                                 )
                                 (loop)
                              ) 
                           )
                        )
                        (move-buffer-begin)
                        (message "Done")
                     )
                  )
               )
            )
         )
      )
   )
)

(define mail-thread-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (mail-open-entry)))
      map
   )
)

(define-mode mail-thread-mode "Mail:Thread" text-mode
   (set-local! linenum-enable #f)
)

(define mail-render-thread
   (lambda (tid buf-out)
      (with-current-buffer buf-out
         (let ([l (buffer-eval)])
            (with-current-buffer (buffer-create)
               (buffer-set-name (format "Thread:~a:" tid))
               (mail-thread-mode)
               (erase-buffer)
               (for-each
                  (lambda (th)
                     (let loop ([th th] [depth 0])
                        (let (
                              [m (first th)]
                              [tl (second th)]
                             )
                           (let ([hl (plist-get m ':headers)])
                              (when hl
                                 (let (
                                       [subj (plist-get hl ':Subject)]
                                       [from (plist-get hl ':From)]
                                       [to (plist-get hl ':To)]
                                       [cc (plist-get hl ':Cc)]
                                       [date (plist-get hl ':Date)]
                                       [date_rel (plist-get m ':date_relative)]
                                       [id   (plist-get m ':id)]
                                       [curs (cursor)]
                                      )
                                    (insert (format "[~a] [~a] " (string-pad-right date_rel 10) (string-pad-right from 15)))
                                    (let ([n 0])
                                       (while (< n depth)
                                          (insert "-")
                                          (set! n (1+ n))
                                       )
                                    )
                                    (insert (format "~a\n" (string-pad-right subj (- 100 depth))))
                                    (let ([entry (make-mail-entry id date from to cc subj)])
                                       (add-data-property entry curs (cursor))
                                    )
                                 )
                              )
                           )
                           (for-each
                              (lambda (th)
                                 (loop th (1+ depth))
                              )
                              tl
                           )
                        )
                     )
                  )
                  (first l)
               )
               (move-buffer-begin)
            )
         )
      )
   )
)

(define mail-open-thread
   (lambda ()
      (let ([plist (get-property 'data (1+ (cursor)) (+ 2 (cursor)))])
         (letrec (
               [tid (cdr (assoc ':data (first plist)))]
               [buf-ret (buffer-new)]
              )
            (process-create (format "notmuch show --entire-thread=true --format=sexp --body=false thread:~a" tid) buf-ret
               (lambda (status buf-out buf-err)
                  (mail-render-thread tid buf-out)
                  (buffer-delete buf-out)
               )
            )
         )
      )
   )
)

(define mail-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (mail-open-thread)))
      map
   )
)

(define-mode mail-mode "Mail" text-mode
   (set-local! linenum-enable #f)
)

(define mail-render-thread-list
   (lambda (buf-ret)
      (with-current-buffer buf-ret
         (let ([l (buffer-eval)])
            (with-current-buffer (buffer-get-or-create "Mail")
               (mail-mode)
               (erase-buffer)
               (for-each
                  (lambda (th)
                     (let (
                           [subj (plist-get th ':subject)]
                           [from (plist-get th ':authors)]
                           [date (plist-get th ':date_relative)]
                           [id   (plist-get th ':thread)]
                           [curs (cursor)]
                          )
                        (insert (format "[~a] [~a] " (string-pad-right date 10) (string-pad-right from 15)))
                        (insert (format "~a\n" (string-pad-right subj 100)))
                        (add-data-property id curs (cursor))
                     )
                  )
                  l
               )
               (move-buffer-begin)
            )
         )
      )
   )
)

(define mail-default-query "notmuch search --format=sexp --limit=500 \"(to:~a or cc:~a)\"") 

(define mail
   (lambda ()
      (let ([buf-ret (buffer-new)])
         (process-create (format mail-default-query mail-user mail-user) buf-ret
            (lambda (status buf-out buf-err)
               (mail-render-thread-list buf-out)
               (buffer-delete buf-out)
            )
         )
      )
   )
)