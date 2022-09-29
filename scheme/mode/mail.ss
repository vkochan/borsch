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

(define mail-config-dir
   (lambda ()
      (string-append (config-dir) "/mail")
   )
)

(define mail-notmuch-config
   (lambda ()
      (string-append (mail-config-dir) "/notmuch-config")
   )
)

(define mail-notmuch-cmd
   (lambda (cmd)
      (format "notmuch --config=~a ~a" (mail-notmuch-config) cmd)
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

(define mail-dir-var "")
(define mail-dir
   (case-lambda
      [()
       mail-dir-var
      ]

      [(dir)
       (set! mail-dir-var dir)
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
   (case-lambda
      [(func)
       (let ([func func])
          (when (not mail-is-syncing?)
             (set! mail-is-syncing? #t)
             (message "Syncing mail ...")
             (process-create
                (format "~a && ~a" (mail-sync-cmd) (mail-notmuch-cmd "new")) #f #f
                (lambda (status buf-out buf-err)
                   (set! mail-is-syncing? #f)
                   (message "Mail synced")
                   (when func (func))
                )
             )
          )
       )
      ]
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

(define mail-message-set-tag
   (lambda (id tag)
      (system (mail-notmuch-cmd (format "tag ~a id:~a" tag id)))
   )
)

(define mail-message-add-tag
   (lambda (id tag)
      (mail-message-set-tag id (format "+~a" tag))
   )
)

(define mail-message-del-tag
   (lambda (id tag)
      (mail-message-set-tag id (format "-~a" tag))
   )
)

(define mail-thread-set-tag
   (lambda (id tag)
      (system (mail-notmuch-cmd (format "tag ~a thread:~a" tag id)))
   )
)

(define mail-thread-add-tag
   (lambda (id tag)
      (mail-thread-set-tag id (format "+~a" tag))
   )
)

(define mail-thread-del-tag
   (lambda (id tag)
      (mail-thread-set-tag id (format "-~a" tag))
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
         (process-create (mail-notmuch-cmd (format "reply id:~a" id)) buf)
      )
     ]
   )
)

(define mail-dump-entry
   (lambda ()
      (let ([plist (get-property 'data (cursor) (+ 1 (cursor)))])
         (let ([entry (plist-get (first plist) 'data)])
            (let ([b (buffer-create (format "Message-dump:~a" (if (mail-entry? entry) (mail-entry-id entry) entry)))])
               (text-mode)
               (process-create
                  (if (mail-entry? entry)
                     (mail-notmuch-cmd (format "show --format=sexp --entire-thread=false id:~a" (mail-entry-id entry)))
                     ;;
                     (mail-notmuch-cmd (format "show --format=sexp --entire-thread=true --body=false thread:~a" entry))
                  )
                  b
               )
            )
         )
      )
   )
)

(define mail-render-message
   (lambda (sexp)
      (let ([headers (plist-get sexp ':headers)])
         (let (
               [subj (plist-get headers ':Subject)]
               [from (plist-get headers ':From)]
               [to (plist-get headers ':To)]
               [cc (plist-get headers ':Cc)]
               [date (plist-get headers ':Date)]
              )
            (when subj (insert (format "Subject: ~a\n" subj)))
            (when from (insert (format "From: ~a\n" from)))
            (when to (insert (format "To: ~a\n" to)))
            (when cc (insert (format "Cc: ~a\n" cc)))
            (when date (insert (format "Date: ~a\n" date)))
         )
      )
      (let loop ([content-list (plist-get sexp ':body)])
         (for-each
            (lambda (content)
               (let (
                     [type (plist-get content ':content-type)]
                     [body (plist-get content ':content)]
                    )
                  (if (list? body)
                     (loop body)
                     (when (and (string? body) (equal? type "text/plain"))
                        (insert "\n")
                        (insert body)
                     )
                  )
               )
            )
            content-list
         )
      )
   )
)

(define mail-open-entry
   (lambda ()
      (let ([plist (get-property 'data (cursor) (+ 1 (cursor)))])
         (let ([entry (plist-get (first plist) 'data)])
            (let ([b (buffer-create (format "Message:~a" (mail-entry-id entry)))])
               (define-local mail-message-id (mail-entry-id entry))
               (bind-key-local "r" mail-reply-message)
               (text-mode)
               (message "Loading mail ...")

               (process-create (mail-notmuch-cmd (format "show --format=sexp --entire-thread=false id:~a" (mail-entry-id entry))) b
                  (lambda (status buf-out buf-err)
                     (let ([sexp (buffer-eval buf-out)])
                        (with-current-buffer b
                           (erase-buffer)
                           (mail-for-each-message
                              (lambda (m depth)
                                 (mail-render-message m)
                                 (mail-message-del-tag (mail-entry-id entry) "unread")
                                 (buffer-set-readonly #t)
                                 (move-buffer-begin)
                                 (message "Done")
                              )
                              sexp
                           )
                        )
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
      (let ([plist (get-property 'data (cursor) (+ 1 (cursor)))])
         (let ([entry (plist-get (first plist) 'data)])
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

(define mail-for-each-message
   (lambda (fn sexp)
      (for-each
         (lambda (th)
            (let loop ([th th] [depth 0])
               (let (
                     [m (first th)]
                     [tl (second th)]
                    )
                  (when (not (equal? m 'nil))
                     (fn m depth)
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
         (first sexp)
      )
   )
)

(define mail-render-thread
   (lambda (tid buf-out)
      (with-current-buffer buf-out
         (let ([sexp (buffer-eval)])
            (with-current-buffer (buffer-create)
               (buffer-set-name (format "Thread:~a" tid))
               (mail-thread-mode)
               (buffer-set-readonly #f)
               (erase-buffer)

               (mail-for-each-message
                  (lambda (m depth)
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
                                 (add-property curs (1- (cursor)) `(data ,entry))
                              )
                           )
                        )
                     )
                  )
                  sexp
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
      (let ([plist (get-property 'data (cursor) (+ 1 (cursor)))])
         (letrec (
               [tid (plist-get (first plist) 'data)]
               [buf-ret (buffer-new)]
              )
            (process-create (mail-notmuch-cmd (format "show --entire-thread=true --format=sexp --body=false thread:~a" tid)) buf-ret
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
      (minibuf-read "Search: "
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

(define mail-prompt-edit-filter
   (lambda ()
      (minibuf-read "Search: " (get-local mail-query)
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

(define mail-delete-thread
   (lambda ()
      (let ([plist (get-property 'data (cursor) (+ 1 (cursor)))])
         (let ([entry (plist-get (first plist) 'data)])
            (mail-thread-add-tag entry "deleted")
            (buffer-reload)
         )
      )
   )
)

(define mail-undelete-thread
   (lambda ()
      (let ([plist (get-property 'data (cursor) (+ 1 (cursor)))])
         (let ([entry (plist-get (first plist) 'data)])
            (mail-thread-del-tag entry "deleted")
            (buffer-reload)
         )
      )
   )
)

(define mail-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (mail-open-thread)))
      (bind-key map "d" (lambda () (mail-delete-thread)))
      (bind-key map "!" (lambda () (mail-undelete-thread)))
      (bind-key map "c" (lambda () (mail-new-message)))
      (bind-key map "s" (lambda () (mail-prompt-filter)))
      (bind-key map "S" (lambda () (mail-prompt-edit-filter)))
      map
   )
)

(define-mode mail-mode "Mail" text-mode
   (set-local! linenum-enable #f)
)

(define mail-reload-thread-list
   (lambda ()
      (mail-sync
         (lambda ()
            (mail (get-local mail-query))
         )
      )
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
                        (add-property curs (1- (cursor)) `(data ,id))
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

(define mail-default-query-var #f)

(define mail-default-query
   (case-lambda
      [()
       (if (not mail-default-query-var)
          (format "to:~a or cc:~a or from:~a" (mail-username) (mail-username) (mail-username))
          ;; else
          mail-default-query-var
       )
      ]

      [(qry)
       (set! mail-default-query-var qry)
      ]
   )
) 

(define mail-init
   (lambda ()
      (let ()
         (mkdir-p (mail-config-dir))
         (file> (mail-notmuch-config)
            (with-output-to-string
               (lambda ()
                  (printf "[database]\n")
                  (printf "path=~a\n" (mail-dir))
                  (printf "\n")
                  (printf "[user]\n")
                  (printf "name=~a\n" (mail-fullname))
                  (printf "primary_email=~a\n" (mail-username))
                  (printf "\n")
                  (printf "[search]\n")
                  (printf "exclude_tags=deleted;spam;\n")
                  (printf "\n")
                  (printf "[maildir]\n")
                  (printf "synchronize_flags=true\n")
                  (printf "\n")
                  (printf "[index]\n")
                  (printf "header.List=List-Id\n")
               )
            )
         )
      )
   )
)
(add-hook 'init-hook mail-init)

(define mail
   (case-lambda
      [()
       (mail (mail-default-query))
      ]

      [(qry)
       (let (
             [buf-draw (buffer-get-or-create "Mail")]
             [cmd (mail-notmuch-cmd (format "search --format=sexp --limit=500 '~a'" qry))]
             [buf-ret (buffer-new)]
            )
         (process-create cmd buf-ret
            (lambda (status buf-out buf-err)
               (save-cursor
                  (mail-render-thread-list qry buf-draw buf-out)
                  (move-line-begin)
               )
               (buffer-delete buf-out)
            )
         )
       )
      ]
   )
)
