(define git-exists?
   (lambda ()
      (delay (program-exists? "git"))
   )
)

(define git-branch-name
   (lambda ()
      (let (
            [out (process-get-output (format "git -C ~a rev-parse --abbrev-ref HEAD 2> /dev/null" (current-cwd)))]
           )
         (string-remove-nl (list-ref out 1))
      )
   )
)

(define git-short-status
   (lambda ()
      (let (
            [out (process-get-output (format "git -C ~a status 2> /dev/null" (current-cwd)))]
            [branch (git-branch-name)]
            [status ""]
           )
         (when (string-contains? (list-ref out 1) "modified")
            (set! status (format "~a+" status))
         )
         (when (string-contains? (list-ref out 1) "ahead")
            (set! status (format "~a>" status))
         )
         (when (string-contains? (list-ref out 1) "behind")
            (set! status (format "~a<" status))
         )
         (when (string-contains? (list-ref out 1) "diverged")
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
      (format "git -C ~a ~a" (current-cwd) cmd)
   )
)

(define git-cmd-read
   (lambda (cmd)
      (let ([ret (process-get-output (git-cmd-format cmd))])
         (let ([status (list-ref ret 0)] [output (list-ref ret 1)])
            (when (not (eq? 0 status))
               (message output)
            )
            output
         )
      )
   )
)

(define git-cmd-list
   (lambda (cmd)
      (string-split (git-cmd-read cmd) #\nul)
   )
)

(define git-checkout-branch
   (lambda (b)
      (git-cmd-read (format "checkout ~a" b))
      (if (not (equal? b (git-branch-name)))
         (message (format "could not switch to ~a" b))
      )
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

(define git-insert-branch-list
   (lambda ()
      (let ()
         (buffer-set-readonly #f)
         (erase-buffer)
         (insert (format "~a\n" (git-branch-name)) '(:style (:attr "bold")))
         (for-each
            (lambda (b)
               (insert (format "~a\n" b))
            )
            (git-branch-list "-a")
         )
         (move-buffer-begin)
         (buffer-set-readonly #t)
      )
   )
)

(define git-delete-branch
   (lambda (b)
      (git-cmd-read (format "branch -D ~a" b))
   )
)

(define git-branch-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (move-line-begin) (git-show-commit)))
      (bind-key map "g r" (lambda () (git-insert-branch-list)))
      (bind-key map "l" (lambda () (move-line-begin) (git-show-log (extract-object))))
      (bind-key map "c" (lambda () (move-line-begin) (git-checkout-branch (extract-object)) (git-insert-branch-list)))
      (bind-key map "d"
         (lambda ()
            (let ([b (extract-object)])
               (minibuf-ask
                  (format "Delete ~a branch ?" b) 
                  (lambda (yes-or-no)
                     (when (eq? yes-or-no 'yes)
                        (git-delete-branch b)
                        (git-insert-branch-list)
                     )
                  )
               )
            )
         )
      )
      map
   )
)

(define-mode git-branch-mode "Git Branch" text-mode
   (set-local! linenum-enable #f)
   (git-insert-branch-list)
)

(define git-show-branch
   (lambda ()
      (let ([b (buffer-create)])
         (git-branch-mode)
      )
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
      (let ([lst '()])
         (for-each
            (lambda (x)
               (let ([line (string-split x #\space)])
                  (when (equal? "??" (list-ref line 0))
                     (set! lst (append lst (list (list-ref line 1))))
                  )
               )
            ) (git-cmd-list "status --porcelain -unormal -z")
         )
         lst
      )
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
      (git-cmd-read (format "add ~a" (string-join f " ")))
   )
)

(define git-stage-file-cmd
   (lambda (f)
      (git-cmd-read (format "add -- ~a" (string-join f " ")))
   )
)

(define git-revert-file-cmd
   (lambda (f)
      (git-cmd-read (format "checkout -- ~a" (string-join f " ")))
   )
)

(define git-unstage-file-cmd
   (lambda (f)
      (git-cmd-read (format "reset -- ~a" (string-join f " ")))
   )
)

(define git-untrack-file-cmd
   (lambda (f)
      (git-cmd (format "rm --cached ~a" (string-join f " ")))
   )
)

(define git-status-diff
   (lambda (status)
      (let ([b (buffer-create)])
         (with-current-buffer b
            (diff-mode)
            (buffer-set-mode-name (if (eq? status 'staged) "Diff Staged" "Diff Unstaged"))
            (insert
               (git-cmd-read
                  (format "diff ~a"
                     (if (eq? status 'staged) "--cached" "")
                  )
               )
            )
            (buffer-set-readonly #t)
            (move-buffer-begin)
         )
      )
   )
)

(define git-status-diff-file
   (lambda (status file)
      (let ([b (buffer-create file)])
         (with-current-buffer b
            (diff-mode)
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
            (move-buffer-begin)
         )
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
                (save-cursor
                   (run-hooks 'git-commit-edit-hook)
                )
                (buffer-set-mode-name "Git Commit")
                (bind-key-local "C-c C-c"
                   (lambda ()
                      (process-with-input (format "git -C ~a commit ~a -F -" (current-cwd) opt)
                                          (buffer-string))
                      (with-current-buffer (get-local status-buffer)
                         (git-status-draw)
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
         (git-status-draw)
      )
   )
)

(define git-pull-changes-and-show
   (lambda ()
      (let ([b (buffer-create)])
         (text-mode)
         (process-create (git-cmd-format "pull") b)
      )
   )
)

(define git-switch-branch
   (lambda ()
      (minibuf-complete
         (git-branch-list)
         (lambda (b)
            (git-checkout-branch b)
         )
         "switch to branch"
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
       (let ([ls (git-cmd-read (format "log --no-merges -n ~a --pretty=format:\"%h  %d (%an <%ae>) %s\" ~a" num obj))])
          (buffer-set-readonly #f)
          (insert ls)
          (buffer-set-readonly #t)
       )
      ]
   )
)

(define git-show-commit
   (lambda ()
      (let ([id (extract-object)])
         (let ([b (buffer-create)])
            (diff-mode)
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
      (bind-key map "<Enter>" (lambda () (move-line-begin) (git-show-commit)))
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
            (move-each-line
               (lambda ()
                  (add-text-property (cursor) (word-end-pos) '(:style (:fg "green")))
               )
            )
            (move-buffer-begin)
          )
        )
      ]
   )
)

(define git-insert-blame
   (case-lambda
      [(file start end)
       (let ([ls (git-cmd-read (format "blame --root ~a -L~a,~a" file start end))])
          (buffer-set-readonly #f)
          (insert ls)
          (buffer-set-readonly #t)
       )
      ]
   )
)

(define git-blame-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (move-line-begin) (git-show-commit)))
      map
   )
)

(define-mode git-blame-mode "Git Blame" text-mode
   (buffer-set-readonly #t)
   (set-local! linenum-enable #f)
   (git-insert-blame (buffer-name) (get-local git-blame-start) (get-local git-blame-end))
   (move-buffer-begin)
   (move-each-line
      (lambda ()
         (add-text-property (cursor) (word-end-pos) '(:style (:fg "green")))
      )
   )
   (move-buffer-begin)
)

(define git-blame
   (lambda ()
      (let (
            [start (buffer-line-num (selection-get))]
            [end   (buffer-line-num (cursor))]
           )
         (selection-clear)

         (let ([b (buffer-create (buffer-filename))])
            (with-current-buffer b
               (define-local git-blame-start start)
               (define-local git-blame-end end)
               (git-blame-mode)
            )
         )
      )
   )
)

(define git-status-get-path
   (lambda ()
      (let ([plist (get-text-property ':data (line-begin-pos) (line-end-pos))])
         (plist-get (first plist) ':data)
      )
   )
)

(define git-status-open-staged-file-diff
   (lambda ()
      (git-status-diff-file 'staged (git-status-get-path))
   )
)

(define git-status-update-staged-file
   (lambda ()
      (git-unstage-file-cmd (git-status-get-path))
      (buffer-reload)
   )
)

(define git-status-open-staged-all-diff
   (lambda ()
      (git-status-diff 'staged)
   )
)

(define git-status-staged-all-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-staged-all-diff)
      map
   )
)

(define git-status-staged-file-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-staged-file-diff)
      (bind-key map "u" git-status-update-staged-file)
      map
   )
)

(define git-status-draw-staged
   (lambda ()
      (add-text-property
         (line-begin-pos) (line-end-pos)
        `(
          :keymap ,git-status-staged-all-map
          :style (:attr "bold")
         )
      )
      (move-next-line)
      (when (string-contains? (extract-line) "(use")
         (move-next-line)
      )
      (while (string-contains? (extract-line) "modified")
         (move-next-longword)
         (add-text-property (cursor) (longword-end-pos) '(:style (:fg "green")))
         (move-next-longword)
         ;; we stay at the file path word
         (add-text-property (cursor) (longword-end-pos) '(:style (:fg "green")))
         (add-text-property
            (line-begin-pos) (line-end-pos)
           `(
             :keymap ,git-status-staged-file-map
             :data ,(extract-longword)
            )
         )
         (move-next-line)
      )
   )
)

(define git-status-open-unstaged-file-diff
   (lambda ()
      (git-status-diff-file 'unstaged (git-status-get-path))
   )
)

(define git-status-update-unstaged-file
   (lambda ()
      (git-stage-file-cmd (git-status-get-path))
      (buffer-reload)
   )
)

(define git-status-revert-unstaged-file
   (lambda ()
      (minibuf-ask "Revert selected file ?"
         (lambda (v)
            (when (eq? v 'yes)
               (git-revert-file-cmd (git-status-get-path))
               (buffer-reload)
            )
         )
      )
   )
)

(define git-status-unstaged-file-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-unstaged-file-diff)
      (bind-key map "u" git-status-update-unstaged-file)
      (bind-key map "!" git-status-revert-unstaged-file)
      map
   )
)

(define git-status-open-unstaged-all-diff
   (lambda ()
      (git-status-diff 'unstaged)
   )
)

(define git-status-unstaged-all-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-unstaged-all-diff)
      map
   )
)

(define git-status-draw-unstaged
   (lambda ()
      (add-text-property
         (line-begin-pos) (line-end-pos)
        `(
          :keymap ,git-status-unstaged-all-map
          :style (:attr "bold")
         )
      )
      (move-next-line)
      (move-next-line)
      (move-next-line)
      (while (string-contains? (extract-line) "modified")
         (move-next-longword)
         (add-text-property (cursor) (longword-end-pos) '(:style (:fg "red")))
         (move-next-longword)
         ;; we stay at the file path word
         (add-text-property (cursor) (longword-end-pos) '(:style (:fg "red")))
         (add-text-property
            (line-begin-pos) (line-end-pos)
           `(
             :keymap ,git-status-unstaged-file-map
             :data ,(extract-longword)
            )
         )
         (move-next-line)
      )
   )
)

(define git-status-unmerged-file-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-unstaged-file-diff)
      (bind-key map "u" git-status-update-unstaged-file)
      map
   )
)

(define git-status-unmerged-all-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-unstaged-all-diff)
      map
   )
)

(define git-status-draw-unmerged
   (lambda ()
      (add-text-property
         (line-begin-pos) (line-end-pos)
        `(
          :keymap ,git-status-unmerged-all-map
          :style (:attr "bold")
         )
      )
      (move-next-line)
      (move-next-line)
      (while (string-contains? (extract-line) "modified")
         (move-next-longword)
         (add-text-property (cursor) (longword-end-pos) '(:style (:fg "red")))
         (move-next-longword)
         (add-text-property (cursor) (longword-end-pos) '(:style (:fg "red")))
         (move-next-longword)
         ;; we stay at the file path word
         (add-text-property (cursor) (longword-end-pos) '(:style (:fg "red")))
         (add-text-property
            (line-begin-pos) (line-end-pos)
           `(
             :keymap ,git-status-unmerged-file-map
             :data ,(extract-longword)
            )
         )
         (move-next-line)
      )
   )
)

(define git-status-update-untracked-file
   (lambda ()
      (git-add-file-cmd (git-status-get-path))
      (buffer-reload)
   )
)

(define git-status-untracked-file-map
   (let ([map (make-keymap)])
      (bind-key map "u" git-status-update-untracked-file)
      map
   )
)

(define git-status-draw-untracked
   (lambda ()
      (move-next-line)
      (move-next-line)
      (while (not (equal? "" (string-remove-nl (extract-line))))
         (move-next-longword)
         ;; we stay at the file path word
         (add-text-property (cursor) (longword-end-pos) '(:style (:fg "bright-black")))
         (add-text-property
            (line-begin-pos) (line-end-pos)
           `(
             :keymap ,git-status-untracked-file-map
             :data ,(extract-longword)
            )
         )
         (move-next-line)
      )
   )
)

(define git-status-draw-branch-status
   (lambda ()
      (insert-empty-line)
   )
)

(define git-status-draw
   (lambda ()
      (buffer-set-readonly #f)
      (set-local! git-status-cursor (cursor))
      (erase-buffer)
      (process-create "git status" (current-buffer)
         (lambda (status buf-out buf-err)
            (with-current-buffer buf-out
               (if (not (eq? 0 status))
                  #f
                  ;; else
                  (let ()
                     (move-buffer-begin)
                     (insert (git-cmd-read "log -1 --oneline"))
                     (insert "\n")
                     (move-each-line
                        (lambda ()
                           (let ([line (extract-line)])
                              (cond
                                 [(pregexp-match "^On branch" line) (git-status-draw-branch-status)]
                                 [(pregexp-match "detached at" line) (git-status-draw-branch-status)]
                                 [(pregexp-match "^Changes to be committed" line) (git-status-draw-staged)]
                                 [(pregexp-match "^Changes not staged" line) (git-status-draw-unstaged)]
                                 [(pregexp-match "^Unmerged paths" line) (git-status-draw-unmerged)]
                                 [(pregexp-match "^Untracked files" line) (git-status-draw-untracked)]
                              )
                           )
                        )
                     )
                     (cursor-set (get-local git-status-cursor))
                  )
               )
               (buffer-set-readonly #t)
            )
         )
      )
   )
)

(define git-status-mode-map
   (let ([map (make-keymap)])
      (bind-key map "c" (lambda () (git-create-commit)))
      (bind-key map "a" (lambda () (git-amend-commit)))
      (bind-key map "u" (lambda () (git-pull-changes)))
      map
   )
)

(define-mode git-status-mode "Git" text-mode
   (define-local buffer-reload-func git-status-draw)
   (define-local git-status-cursor (cursor))
   (set-local! linenum-enable #f)
   (buffer-set-name "git-status")
   (git-status-draw)
   (move-buffer-begin)
)

(define git-status
   (lambda ()
      (buffer-create)
      (git-status-mode)
   )
)

(bind-key dirb-mode-map "g l" (lambda () (git-show-log (dirb-entry-path))))
(bind-key text-mode-normal-map "g l" (lambda () (git-show-log (buffer-filename))))
(bind-key text-mode-normal-map "g D" (lambda () (git-status-diff-file 'unstaged (buffer-filename))))
(bind-key text-mode-normal-map "g c" (lambda () (git-show-commit)))
(bind-key text-mode-normal-map "g b" (lambda () (git-blame)))
(bind-key text-mode-visual-linewise-map "g b" (lambda () (git-blame)))
