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

(define mail-fullname-var "")
(define mail-fullname
   (case-lambda
      [()
       mail-fullname-var
      ]

      [(fullname)
       (set! mail-fullname-var fullname)
      ]
   )
)

(define mail-username-var "")
(define mail-username
   (case-lambda
      [()
       mail-username-var
      ]

      [(username)
       (set! mail-username-var username)
      ]
   )
)

(define mail-sync-cmd-var "mbsync -a")
(define mail-sync-cmd
   (case-lambda
      [()
       mail-sync-cmd-var
      ]

      [(cmd)
       (set! mail-sync-cmd-var cmd)
      ]
   )
)

(define mail-timer #f)

(define mail-sync
   (lambda ()
      (when (not mail-is-syncing?)
         (set! mail-is-syncing? #t)
         (message "Syncing mail ...")
         (process-create
            (format "~a && notmuch new" (mail-sync-cmd)) #f #f
            (lambda (status buf-out buf-err)
               (set! mail-is-syncing? #f)
               (message "Mail synced")
            )
         )
      )
   )
)

(define mail-start-sync
   (lambda ()
      (when (not mail-timer)
         (set! mail-timer
            (make-timer (* 10 60 1000)
               (lambda () (mail-sync))
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

(define mail-set-tag
   (lambda (id tag)
      (process-create (format "notmuch tag ~a id:~a" tag id))
   )
)

(define mail-add-tag
   (lambda (id tag)
      (mail-set-tag id (format "+~a" tag))
   )
)

(define mail-del-tag
   (lambda (id tag)
      (mail-set-tag id (format "-~a" tag))
   )
)

(define mail-send-message
   (lambda (msg)
      (let (
            [proc (process-create "msmtp -t --read-envelope-from" #f
                     (lambda (status buf-out buf-err)
                        (if (eq? 0 status)
                           (message "Mail sent successfully")
                           ;; else
                           (message "Mail sending failed")
                        )
                     )
                  )
            ]
           )
         (let (
               [proc-out (process-port-out proc)]
               [proc-in (process-port-in proc)]
              )
            (put-string proc-in msg)
            (close-port proc-out)
            (close-port proc-in)
         )
      )
   )
)

(define mail-send-buffer
   (lambda ()
      (mail-send-message (buffer-string))
   )
)

(define mail-reply-message
   (case-lambda
     [()
      (mail-reply-message (get-local mail-message-id))
     ]

     [(id)
      (let ([buf (buffer-create (format "Reply:~a" id))])
         (with-current-buffer buf
            (text-mode)
            (bind-key-local "C-c C-c" mail-send-buffer)
         )
         (process-create (format "notmuch reply id:~a" id) buf)
      )
     ]
   )
)

(define mail-open-entry
   (lambda ()
      (let ([plist (get-property 'data (1+ (cursor)) (+ 2 (cursor)))])
         (let ([entry (cdr (assoc ':data (first plist)))])
            (let ([b (buffer-create (format "Message:~a" (mail-entry-id entry)))])
               (define-local mail-message-id (mail-entry-id entry))
               (bind-key-local "r" mail-reply-message)
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
                        (mail-del-tag (mail-entry-id entry) "unread")
                        (buffer-set-readonly #t)
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

(define mail-reply-entry
   (lambda ()
      (let ([plist (get-property 'data (1+ (cursor)) (+ 2 (cursor)))])
         (let ([entry (cdr (assoc ':data (first plist)))])
            (mail-reply-message (mail-entry-id entry))
         )
      )
   )
)

(define mail-thread-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (mail-open-entry)))
      (bind-key map "r" (lambda () (mail-reply-entry)))
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
               (buffer-set-name (format "Thread:~a" tid))
               (mail-thread-mode)
               (buffer-set-readonly #f)
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
                                       [tags (plist-get m ':tags)]
                                      )
                                    (insert (format "[~a] [~a] " (string-pad-right date_rel 15) (string-pad-right from 15)))
                                    (let ([n 0])
                                       (while (< n depth)
                                          (insert "-")
                                          (set! n (1+ n))
                                       )
                                    )
                                    (insert (format "~a\n" (string-pad-right subj (- 100 depth)))
                                       (if (member "unread" tags)
                                          '(style (:attr "bold"))
                                          ;; else
                                          '(style (:attr "normal"))
                                       )
                                    )
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
               (buffer-set-readonly #t)
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

(define mail-prompt-filter
   (lambda ()
      (minibuf-read "Filter: " (get-local mail-query)
         (lambda (qry)
            (mail
	       (if (string-empty? qry)
                  (mail-default-query)
		  ;; else
		  qry
               )
            )
         )
      )
   )
)

(define mail-new-message
   (lambda ()
      (let ([buf (buffer-create "*New Message*")])
         (insert "Subject:\n")
	 (insert (format "From: ~a <~a>\n" (mail-fullname) (mail-username)))
	 (insert "To: \n")
	 (insert "\n")
         (text-mode)
         (bind-key-local "C-c C-c" mail-send-buffer)
      )
   )
)

(define mail-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (mail-open-thread)))
      (bind-key map "m" (lambda () (mail-new-message)))
      (bind-key map ":" (lambda () (mail-prompt-filter)))
      map
   )
)

(define-mode mail-mode "Mail" text-mode
   (set-local! linenum-enable #f)
)

(define mail-reload-thread-list
   (lambda ()
      (mail (get-local mail-query))
   )
)

(define mail-render-thread-list
   (lambda (qry buf-draw buf-ret)
      (with-current-buffer buf-ret
         (let ([l (buffer-eval)])
            (with-current-buffer buf-draw
               (define-local buffer-reload-func mail-reload-thread-list)
               (define-local mail-query qry)
               (mail-mode)
               (buffer-set-readonly #f)
               (erase-buffer)
               (for-each
                  (lambda (th)
                     (let (
                           [subj (plist-get th ':subject)]
                           [from (plist-get th ':authors)]
                           [date (plist-get th ':date_relative)]
                           [id   (plist-get th ':thread)]
                           [tags (plist-get th ':tags)]
                           [curs (cursor)]
                          )
                        (insert (format "[~a] [~a] " (string-pad-right date 15) (string-pad-right from 15)))
                        (insert (format "~a\n" (string-pad-right subj 100))
                           (if (member "unread" tags)
                              '(style (:attr "bold"))
                              ;; else
                              '(style (:attr "normal"))
                           )
                        )
                        (add-data-property id curs (cursor))
                     )
                  )
                  l
               )
               (buffer-set-readonly #t)
               (move-buffer-begin)
            )
         )
      )
   )
)

(define mail-default-query
   (lambda ()
      (format "to:~a or cc:~a" (mail-username) (mail-username))
   )
) 

(define mail
   (case-lambda
      [()
       (mail (mail-default-query))
      ]

      [(qry)
       (let (
             [buf-draw (buffer-get-or-create "Mail")]
             [cmd (format "notmuch search --format=sexp --limit=500 '~a'" qry)]
             [buf-ret (buffer-new)]
            )
         (mail-sync)
         (process-create cmd buf-ret
            (lambda (status buf-out buf-err)
               (mail-render-thread-list qry buf-draw buf-out)
               (buffer-delete buf-out)
            )
         )
       )
      ]
   )
)
