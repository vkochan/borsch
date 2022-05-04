(define git-exists?
   (lambda ()
      (delay (program-exists? "git"))
   )
)

(define git-branch-name
   (lambda ()
      (let (
            [out (process-read (format "git -C ~a rev-parse --abbrev-ref HEAD 2> /dev/null" (view-cwd)))]
           )
         (string-remove-nl out)
      )
   )
)

(define git-short-status
   (lambda ()
      (let (
            [out (process-read (format "git -C ~a status 2> /dev/null" (view-cwd)))]
            [branch (git-branch-name)]
            [status ""]
           )
         (when (string-contains? out "modified")
            (set! status (format "~a+" status))
         )
         (when (string-contains? out "ahead")
            (set! status (format "~a>" status))
         )
         (when (string-contains? out "behind")
            (set! status (format "~a<" status))
         )
         (when (string-contains? out "diverged")
            (set! status (format "~a!" status))
         )
         (if (equal? status "")
            branch
            ;; else
            (format "~a ~a" branch status)
         )
      )
   )
)

(define git-cmd-format
   (lambda (cmd)
      (format "git -C ~a ~a" (view-cwd) cmd)
   )
)

(define git-cmd-read
   (lambda (cmd)
      (process-read (git-cmd-format cmd))
   )
)

(define git-cmd-list
   (lambda (cmd)
      (string-split (git-cmd-read cmd) #\nul)
   )
)

(define git-branch-list
   (case-lambda
      [()
       (git-branch-list "")
      ]

      [(opts)
       (let ([ret (git-cmd-read (format "branch ~a" opts))])
          (let ([ls (string-split ret #\newline)])
             (let ([blist (list)])
                (for-each
                   (lambda (b)
                      (let ([bs (string-split b #\ )])
                         ;; handle '->'
                         (if (< (length bs) 2)
                            ;; handle '*'
                            (if (> (length bs) 1)
                               (set! blist (append blist (list (list-ref bs 1))))
                               ;; else
                               (set! blist (append blist (list (list-ref bs 0))))
                            )
                         )
                      )
                   ) ls
                )
                blist
             )
          )
       )
      ]
   )
)

(define git-cmd-file-status
   (lambda (f)
      (let ([st-line (git-cmd-list (format "status -z ~a" f))])
         (if (> (length st-line) 0)
            (let ([st-list (string-split (list-ref st-line 0) #\ )])
               (if (> (length st-list) 0)
                  (list-ref st-list 0)
                  ;; else
                  ""
               )
            )
            ;; else
            ""
         )
      )
   )
)

(define git-file-status
   (lambda (f)
      (let ([st-pair (assoc f (get-local all-list))])
         (if st-pair
            (list-ref st-pair 1)
            ;; else
            (git-cmd-file-status f)
         )
      )
   )
)

(define git-staged-file-status
   (lambda (f)
      (let ([s (git-file-status f)])
         (cond
            [(equal? s "MM") "M"]
            [(equal? s "UU") "U"]
            [(equal? s "AD") "A"]
            [else s]
         )
      )
   )
)

(define git-unstaged-file-status
   (lambda (f)
      (let ([s (git-file-status f)])
         (cond
            [(equal? s "MM") "M"]
            [(equal? s "UU") "U"]
            [(equal? s "AD") "D"]
            [else s]
         )
      )
   )
)

(define git-list-staged
   (lambda ()
      (let (
            [ls (git-cmd-list "diff -z --name-only --cached")]
           )
         (filter
            (lambda (f)
               (not (equal? (git-staged-file-status f) "U"))
            ) ls
         )
      )
   )
)

(define git-list-unstaged
   (lambda ()
      (let (
            [ls (git-cmd-list "diff-files -z --name-only")]
            [dup (list)]
           )
         (filter
            (lambda (f)
               (let ([skip #f])
                  (set! skip (not (member f dup)))
	          (set! dup (append dup (list f)))
                  skip
               )
            ) ls
         )
      )
   )
)

(define git-list-untracked
   (lambda ()
      (git-cmd-list "ls-files -z --full-name --other")
   )
)

(define git-list-unmerged
   (lambda ()
      (git-cmd-list "diff-files -z --name-only --diff-filter=U")
   )
)

(define git-list-all
   (lambda ()
      (let (
            [st-files (git-cmd-list "status -z")]
            [ls (list)]
           )
         (for-each
            (lambda (s)
               (set! ls (append ls (list (reverse (string-split s #\ )))))
            ) st-files
         )
         ls
      )
   )
)

(define git-add-file-cmd
   (lambda (f)
      (git-cmd-read (format "add ~a" f))
   )
)

(define git-stage-file-cmd
   (lambda (f)
      (git-cmd-read (format "add -- ~a" f))
   )
)

(define git-revert-file-cmd
   (lambda (f)
      (git-cmd-read (format "checkout -- ~a" f))
   )
)

(define git-unstage-file-cmd
   (lambda (f)
      (git-cmd-read (format "reset -- ~a" f))
   )
)

(define git-untrack-file-cmd
   (lambda (f)
      (git-cmd (format "rm --cached ~a" f))
   )
)

(define git-status-diff-file
   (lambda (status file)
      (let ([b (buffer-create file)])
         (with-current-buffer b
            (text-mode)
            (buffer-set-mode-name "Diff")
            (insert
               (git-cmd-read
                  (format "diff ~a -- ~a"
                     (if (eq? status 'staged) "--cached" "")
                     file
                  )
               )
            )
            (buffer-set-readonly #t)
         )
      )
   )
)

(define git-staged-update
   (lambda ()
      (move-line-begin)
      (move-next-longword)
      (git-unstage-file-cmd (extract-longword))
      (git-show-status)
   )
)

(define git-staged-diff-file
   (lambda ()
      (move-line-begin)
      (move-next-longword)
      (git-status-diff-file 'staged (extract-longword))
   )
)

(define git-staged-map
   (let ([map (make-keymap)])
      (bind-key map "u" git-staged-update)
      (bind-key map "<Enter>" git-staged-diff-file)
      map
   )
)

(define git-unstaged-update
   (lambda ()
      (move-line-begin)
      (move-next-longword)
      (git-stage-file-cmd (extract-longword))
      (git-show-status)
   )
)

(define git-unstaged-diff-file
   (lambda ()
      (move-line-begin)
      (move-next-longword)
      (git-status-diff-file 'unstaged (extract-longword))
   )
)

(define git-unstaged-revert
   (lambda ()
      (minibuf-ask "Revert selected file ?"
         (lambda (v)
            (when (eq? v 'yes)
               (move-line-begin)
               (move-next-longword)
               (git-revert-file-cmd (extract-longword))
               (git-show-status)
            )
         )
      )
   )
)

(define git-unstaged-map
   (let ([map (make-keymap)])
      (bind-key map "u" git-unstaged-update)
      (bind-key map "!" git-unstaged-revert)
      (bind-key map "<Enter>" git-unstaged-diff-file)
      map
   )
)

(define git-untracked-update
   (lambda ()
      (move-line-begin)
      (git-add-file-cmd (extract-longword))
      (git-show-status)
   )
)

(define git-untracked-map
   (let ([map (make-keymap)])
      (bind-key map "u" git-untracked-update)
      map
   )
)

(define git-show-staged-status
   (lambda ()
      (let ([ls (get-local staged-list)])
         (when (not (equal? (length ls) 0))
            (insert (format "Staged (~a):\n" (length ls)) '(style (:attr "bold")))
	    (for-each
               (lambda (f)
                  (insert (format "~a ~a\n" (git-staged-file-status f) f) `(keymap ,git-staged-map))
               ) ls
            )
            (insert "\n")
         )
      )
   )
)

(define git-show-unstaged-status
   (lambda ()
      (let ([ls (get-local unstaged-list)])
         (when (not (equal? (length ls) 0))
            (insert (format "Not staged (~a):\n" (length ls)) '(style (:attr "bold")))
	    (for-each
               (lambda (f)
                  (insert (format "~a ~a\n" (git-unstaged-file-status f) f) `(keymap ,git-unstaged-map))
               ) ls
            )
            (insert "\n")
         )
      )
   )
)

(define git-show-unmerged-status
   (lambda ()
      (let ([ls (get-local unmerged-list)])
         (when (not (equal? (length ls) 0))
            (insert (format "Not merged (~a):\n" (length ls)) '(style (:attr "bold")))
	    (for-each
               (lambda (f)
                  (insert (format "~a\n" f))
               ) ls
            )
            (insert "\n")
         )
      )
   )
)

(define git-show-untracked-status
   (lambda ()
      (let ([ls (get-local untracked-list)])
         (when (not (equal? (length ls) 0))
            (insert (format "Untracked (~a):\n" (length ls)) '(style (:attr "bold")))
	    (for-each
               (lambda (f)
                  (insert (format "~a\n" f) `(keymap ,git-untracked-map))
               ) ls
            )
            (insert "\n")
         )
      )
   )
)

(define git-show-status
   (lambda ()
      (save-cursor
         (buffer-set-readonly #f)
         (erase-buffer)
         (set-local! all-list (git-list-all))
         (set-local! staged-list (git-list-staged))
         (set-local! unstaged-list (git-list-unstaged))
         (set-local! unmerged-list (git-list-unmerged))
         (set-local! untracked-list (git-list-untracked))
         (insert (format "On branch ~a\n" (git-branch-name)))
         (insert (git-cmd-read "log -1 --oneline"))
         (insert "\n")
         (git-show-staged-status)
         (git-show-unstaged-status)
         ; (git-show-unmerged-status)
         (git-show-untracked-status)
         (move-buffer-begin)
         (buffer-set-readonly #t)
      )
   )
)

(define git-create-commit
   (case-lambda
      [()
       (git-create-commit "")
      ]

      [(mode)
       (let ([c (current-buffer)])
          (let (
                [opt (if (eq? mode 'amend) "--amend" "")]
                [b (buffer-create)]
               )
             (with-current-buffer b
                (define-local status-buffer c)
                (text-mode)
                (if (eq? mode 'amend)
                   (insert (git-cmd-read "log --format=%B -n 1 HEAD"))
                )
                (buffer-set-mode-name "Git Commit")
                (bind-key-local "C-c C-c"
                   (lambda ()
                      (process-write (format "git -C ~a commit ~a -F -" (view-cwd) opt)
                                     (buffer-string))
                      (with-current-buffer (get-local status-buffer)
                         (git-show-status)
                      )
                      (window-delete)
                   )
                )
             )
          )
       )
      ]
   )
)

(define git-amend-commit
   (lambda ()
      (git-create-commit 'amend)
   )
)

(define git-pull-changes
   (lambda ()
      (with-process-temp-buffer (git-cmd-format "pull")
         (git-show-status)
      )
   )
)

(define git-pull-changes-and-show
   (lambda ()
      (let ([b (buffer-create)])
         (text-mode)
         (process-start b (git-cmd-format "pull"))
      )
   )
)

(define git-status-mode-map
   (let ([map (make-keymap)])
      (bind-key map "g r" (lambda () (git-show-status)))
      (bind-key map "c" (lambda () (git-create-commit)))
      (bind-key map "a" (lambda () (git-amend-commit)))
      (bind-key map "u" (lambda () (git-pull-changes)))
      map
   )
)

(define-mode git-status-mode "Git" text-mode
   (define-local all-list '())
   (define-local staged-list '())
   (define-local unstaged-list '())
   (define-local unmerged-list '())
   (define-local untracked-list '())
   (git-show-status)
   (set-local! linenum-enable #f)
   (buffer-set-name "git-status")
)

(define git-status
   (lambda ()
      (let ([b (buffer-create)])
         (git-status-mode)
      )
   )
)

(define git-select-branch
   (lambda ()
      (let (
            [b (extract-longword)]
           )
         (when b
            (git-cmd-read (format "checkout ~a" b))
            (if (not (equal? b (git-branch-name)))
               (message (format "could not switch to ~a" b))
            )
            (window-delete)
         )
      )
   )
)

(define git-switch-branch-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-select-branch)
      (bind-key map "<Esc>" window-delete)
      (bind-key map "q" window-delete)
      (bind-key map "j" (lambda ()
                                   (highlight-clear)
                                   (move-line-down)
                                   (highlight-range
                                      (line-begin-pos) (line-end-pos))))
      (bind-key map "k" (lambda ()
                                   (highlight-clear)
                                   (move-line-up)
                                   (highlight-range
                                      (line-begin-pos) (line-end-pos))))
      map
   )
)

(define git-switch-branch
   (lambda ()
      (let (
            [l (git-branch-list)]
            [b (buffer-create)]
           )
         (window-popup #t)
         (with-current-buffer b
            (buffer-set-name "Switch branch")
            (buffer-set-keymap 'git-switch-branch-map)
            (for-each
               (lambda (x)
                  (insert (format "~a\n" x) '(style (:attr "bold")))
               ) l
            )
         )
         (move-buffer-begin)
         (highlight-range (line-begin-pos) (line-end-pos))
      )
   )
)

(define git-create-and-switch-branch
   (lambda ()
      (minibuf-read "switch to new branch:"
         (lambda (b)
            (if (member b (git-branch-list))
               (message (format "branch ~a already exists" b))
               ;; else
               (begin
                  (git-cmd-read (format "checkout -b ~a" b))
                  (if (not (equal? b (git-branch-name)))
                     (message (format "could not switch to new ~a branch" b))
                  )
               )
            )
         )
      )
   )
)

(define git-insert-log
   (case-lambda
      [(obj)
       (git-insert-log obj 500)
      ]

      [(obj num)
       (let ([ls (git-cmd-read (format "log --no-merges -n ~a --pretty=format:\"%h  %d (%an) %s\" ~a" num obj))])
          (buffer-set-readonly #f)
          (insert ls)
          (buffer-set-readonly #t)
       )
      ]
   )
)

(define git-show-commit
   (lambda ()
      (move-line-begin)
      (let ([id (extract-word)])
         (let ([b (buffer-create)])
            (text-mode)
            (buffer-set-name (format "commit: ~a" id))
            (insert (git-cmd-read (format "show ~a" id)))
            (buffer-set-readonly #t)
            (move-buffer-begin)
         )
      )
   )
)

(define git-log-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-show-commit)
      map
   )
)

(define-mode git-log-mode "Git Log" text-mode
   (buffer-set-readonly #t)
   (set-local! linenum-enable #f)
   (git-insert-log (buffer-name))
)

(define git-show-log
   (case-lambda
      [()
       (git-show-log (git-branch-name))
      ]

      [(obj)
       (let ([b (buffer-create obj)])
          (with-current-buffer b
            (git-log-mode)
            (move-buffer-begin)
          )
        )
      ]
   )
)

(bind-key text-mode-cmd-map "g l" (lambda () (git-show-log (buffer-filename))))
(bind-key text-mode-cmd-map "g d" (lambda () (git-status-diff-file 'unstaged (buffer-filename))))
