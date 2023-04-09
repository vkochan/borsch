(define (git-insert-branch-list)
   (let ()
      (buffer-set-readonly #f)
      (text-delete)
      (text-insert (format "~a\n" (git-branch-name)) '(style: (attr: "bold")))
      (for-each
         (lambda (b)
            (text-insert (format "~a\n" b)))
         (git-branch-list "-a"))
      (cursor-to-begin)
      (buffer-set-readonly #t)))

(define (git-delete-branch b)
   (git-cmd-read (format "branch -D ~a" b)))

(define git-branch-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (cursor-to-line-begin) (git-show-commit)))
      (bind-key map "g r" (lambda () (git-insert-branch-list)))
      (bind-key map "l" (lambda () (cursor-to-line-begin) (git-show-log (text-object))))
      (bind-key map "c" (lambda () (cursor-gogo-line-begin) (git-checkout-branch (text-object)) (git-insert-branch-list)))
      (bind-key map "d"
         (lambda ()
            (let ([b (text-object)])
               (minibuf-ask
                  (format "Delete ~a branch ?" b) 
                  (lambda (yes-or-no)
                     (when (eq? yes-or-no 'yes)
                        (git-delete-branch b)
                        (git-insert-branch-list)))))))
      map))

(define-mode git-branch-mode "Git Branch" text-mode
   (set-local! linenum-enable #f)
   (git-insert-branch-list))

(define (git-show-branch)
   (let ([b (buffer-create)])
      (git-branch-mode)))

(define (git-cmd-file-status f)
   (let ([st-line (git-cmd-list (format "status -z ~a" f))])
      (if (> (length st-line) 0)
         (let ([st-list (string-split (list-ref st-line 0) #\ )])
            (if (> (length st-list) 0)
               (list-ref st-list 0)
               ;; else
               ""))
         ;; else
         "")))

(define (git-file-status f)
   (let ([st-pair (assoc f (get-local all-list))])
      (if st-pair
         (list-ref st-pair 1)
         ;; else
         (git-cmd-file-status f))))

(define (git-staged-file-status f)
   (let ([s (git-file-status f)])
      (cond
         [(equal? s "MM") "M"]
         [(equal? s "UU") "U"]
         [(equal? s "AD") "A"]
         [else s])))

(define (git-unstaged-file-status f)
   (let ([s (git-file-status f)])
      (cond
         [(equal? s "MM") "M"]
         [(equal? s "UU") "U"]
         [(equal? s "AD") "D"]
         [else s])))

(define (git-list-staged)
   (let ([ls (git-cmd-list "diff -z --name-only --cached")])
      (filter
         (lambda (f)
            (not (equal? (git-staged-file-status f) "U")))
         ls)))

(define (git-list-unstaged)
   (let ([ls (git-cmd-list "diff-files -z --name-only")]
         [dup (list)])
      (filter
         (lambda (f)
            (let ([skip #f])
               (set! skip (not (member f dup)))
	       (set! dup (append dup (list f)))
               skip))
         ls)))

(define (git-list-untracked)
   (let ([lst '()])
      (for-each
         (lambda (x)
            (let ([line (string-split x #\space)])
               (when (equal? "??" (list-ref line 0))
                  (set! lst (append lst (list (list-ref line 1)))))))
         (git-cmd-list "status --porcelain -unormal -z"))
      lst))

(define (git-list-unmerged)
   (git-cmd-list "diff-files -z --name-only --diff-filter=U"))

(define (git-list-all)
   (let ([st-files (git-cmd-list "status -z")]
         [ls (list)])
      (for-each
         (lambda (s)
            (set! ls (append ls (list (reverse (string-split s #\ ))))))
         st-files)
      ls))

(define (git-add-file-cmd f)
   (git-cmd-read (format "add ~a" (string-join f " "))))

(define (git-stage-file-cmd f)
   (git-cmd-read (format "add -- ~a" (string-join f " "))))

(define (git-stage-all-cmd)
   (git-cmd-read "add -u"))

(define (git-restore-file-cmd f)
   (git-cmd-read (format "checkout -- ~a" (string-join f " "))))

(define (git-restore-all-cmd)
   (git-cmd-read "checkout -- ."))

(define (git-unstage-file-cmd f)
   (git-cmd-read (format "reset -- ~a" (string-join f " "))))

(define (git-unstage-all-cmd)
   (git-cmd-read "reset"))

(define (git-untrack-file-cmd f)
   (git-cmd-read (format "rm --cached ~a" (string-join f " "))))

(define (git-status-diff status)
   (let ([b (buffer-create)])
      (with-current-buffer b
         (diff-mode)
         (buffer-set-mode-name (if (eq? status 'staged) "Diff Staged" "Diff Unstaged"))
         (text-insert
            (git-cmd-read
               (format "diff ~a"
                  (if (eq? status 'staged) "--cached" ""))))
         (buffer-set-readonly #t)
         (cursor-to-begin))))

(define (git-status-diff-file status file)
   (let ([b (buffer-create file)])
      (with-current-buffer b
         (diff-mode)
         (buffer-set-mode-name "Diff")
         (text-insert
            (git-cmd-read
               (format "diff ~a -- ~a"
                  (if (eq? status 'staged) "--cached" "")
                  file)))
         (buffer-set-readonly #t)
         (cursor-to-begin))))

(define git-create-commit
   (case-lambda
      [()
       (git-create-commit "")]

      [(mode)
       (let ([c (current-buffer)])
          (let ([opt (if (eq? mode 'amend) "--amend" "")]
                [b (buffer-create)])
             (with-current-buffer b
                (define-local status-buffer c)
                (text-mode)
                (if (eq? mode 'amend)
                   (text-insert (git-cmd-read "log --format=%B -n 1 HEAD")))
                (with-saved-cursor
                   (run-hooks 'git-commit-edit-hook))
                (buffer-set-mode-name "Git Commit")
                (bind-key-local "C-c C-c"
                   (lambda ()
                      (process-with-input (format "git -C ~a commit ~a -F -" (current-cwd) opt)
                                          (text-string))
                      (with-current-buffer (get-local status-buffer)
                         (git-status-draw))
                      (window-delete))))))]))

(define (git-amend-commit)
   (git-create-commit 'amend))

(define (git-pull-changes)
   (with-process-temp-buffer (git-cmd-format "pull")
      (git-status-draw)))

(define (git-pull-changes-and-show)
   (let ([b (buffer-create)])
      (text-mode)
      (process-create (git-cmd-format "pull") b)))

(define (git-switch-branch)
   (minibuf-complete
      (git-branch-list)
      (lambda (b)
         (git-checkout-branch b))
      "switch to branch"))

(define (git-create-and-switch-branch)
   (minibuf-read "switch to new branch:"
      (lambda (b)
         (if (member b (git-branch-list))
            (message (format "branch ~a already exists" b))
            ;; else
            (begin
               (git-cmd-read (format "checkout -b ~a" b))
               (if (not (equal? b (git-branch-name)))
                  (message (format "could not switch to new ~a branch" b))))))))

(define git-insert-log
   (case-lambda
      [(obj)
       (git-insert-log obj 500)]

      [(obj num)
       (let ([ls (git-cmd-read (format "log --no-merges -n ~a --pretty=format:\"%h  %d (%an <%ae>) %s\" ~a" num obj))])
          (buffer-set-readonly #f)
          (text-insert ls)
          (buffer-set-readonly #t))]))

(define (git-show-commit)
   (let ([id (text-object)])
      (let ([b (buffer-create)])
         (diff-mode)
         (buffer-set-name (format "commit: ~a" id))
         (text-insert (git-cmd-read (format "show ~a" id)))
         (buffer-set-readonly #t)
         (cursor-to-begin))))

(define git-log-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (cursor-to-line-begin) (git-show-commit)))
      map))

(define-mode git-log-mode "Git Log" text-mode
   (buffer-set-readonly #t)
   (set-local! linenum-enable #f)
   (git-insert-log (buffer-name)))

(define git-show-log
   (case-lambda
      [()
       (git-show-log (git-branch-name))]

      [(obj)
       (let ([b (buffer-create obj)])
          (with-current-buffer b
            (git-log-mode)
            (cursor-to-begin)
            (cursor-to-each-line
               (lambda ()
                  (add-text-property (cursor) (text-word-end-pos) '(style: (fg: "green")))))
            (cursor-to-begin)))]))

(define git-insert-blame
   (case-lambda
      [(file start end)
       (let ([ls (git-cmd-read (format "blame --root ~a -L~a,~a" file start end))])
          (buffer-set-readonly #f)
          (text-insert ls)
          (buffer-set-readonly #t))]))

(define git-blame-mode-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" (lambda () (cursor-to-line-begin) (git-show-commit)))
      map))

(define-mode git-blame-mode "Git Blame" text-mode
   (buffer-set-readonly #t)
   (set-local! linenum-enable #f)
   (git-insert-blame (buffer-name) (get-local git-blame-start) (get-local git-blame-end))
   (cursor-to-begin)
   (cursor-to-each-line
      (lambda ()
         (add-text-property (cursor) (text-word-end-pos) '(style: (fg: "green")))))
   (cursor-to-begin))

(define (git-blame)
   (let ([start (buffer-line-num (text-get-selection))]
         [end   (buffer-line-num (cursor))])
      (text-clear-selection)
      (let ([b (buffer-create (buffer-filename))])
         (with-current-buffer b
            (define-local git-blame-start start)
            (define-local git-blame-end end)
            (git-blame-mode)))))

(define (git-status-get-path)
   (let ([plist (get-text-property 'data: (text-line-begin-pos) (text-line-end-pos))])
      (plist-get (first plist) 'data:)))

(define (git-status-open-staged-file-diff)
   (git-status-diff-file 'staged (git-status-get-path)))

(define (git-status-update-staged-file)
   (git-unstage-file-cmd (git-status-get-path))
   (buffer-reload))

(define (git-status-open-staged-all-diff)
   (git-status-diff 'staged))

(define (git-status-update-staged-all)
   (git-unstage-all-cmd)
   (buffer-reload))

(define git-status-staged-all-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-staged-all-diff)
      (bind-key map "u" git-status-update-staged-all)
      map))

(define git-status-staged-file-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-staged-file-diff)
      (bind-key map "u" git-status-update-staged-file)
      map))

(define (git-status-draw-staged)
   (add-text-property
      (text-line-begin-pos) (text-line-end-pos)
     `(
       keymap: ,git-status-staged-all-map
       style: (attr: "bold")
      ))
   (cursor-to-next-line)
   (when (string-contains? (text-line) "(use")
      (text-delete-line))
   (when (string-empty? (text-line))
      (text-delete-line))
   (while (pregexp-match "(modified:)|(new file:)|(deleted:)|(renamed:)" (text-line))
      (text-delete-longword)
      (when (pregexp-match "new file:" (text-line))
         (text-delete-longword))
      (when (pregexp-match "renamed:" (text-line))
         (text-delete-longword)
         (text-delete-longword))
      (text-delete-longword)
      ;; we stay at the file path word
      (add-text-property (cursor) (text-longword-end-pos) '(style: (fg: "green")))
      (add-text-property
         (text-line-begin-pos) (text-line-end-pos)
        `(
          keymap: ,git-status-staged-file-map
          data: ,(text-longword)
         ))
      (cursor-to-next-line)))

(define (git-status-open-unstaged-file-diff)
   (git-status-diff-file 'unstaged (git-status-get-path)))

(define (git-status-update-unstaged-file)
   (git-stage-file-cmd (git-status-get-path))
   (buffer-reload))

(define (git-status-restore-unstaged-file)
   (minibuf-ask "Restore selected file ?"
      (lambda (v)
         (when (eq? v 'yes)
            (git-restore-file-cmd (git-status-get-path))
            (buffer-reload)))))

(define git-status-unstaged-file-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-unstaged-file-diff)
      (bind-key map "u" git-status-update-unstaged-file)
      (bind-key map "!" git-status-restore-unstaged-file)
      map))

(define (git-status-open-unstaged-all-diff)
   (git-status-diff 'unstaged))

(define (git-status-update-unstaged-all)
   (git-stage-all-cmd)
   (buffer-reload))

(define (git-status-restore-unstaged-all)
   (minibuf-ask "Restore all unstaged file(s) ?"
      (lambda (v)
         (when (eq? v 'yes)
            (git-restore-all-cmd)
            (buffer-reload)))))

(define git-status-unstaged-all-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-unstaged-all-diff)
      (bind-key map "u" git-status-update-unstaged-all)
      (bind-key map "!" git-status-restore-unstaged-all)
      map))

(define (git-status-draw-unstaged)
   (add-text-property
      (text-line-begin-pos) (text-line-end-pos)
     `(
       keymap: ,git-status-unstaged-all-map
       style: (attr: "bold")
      ))
   (cursor-to-next-line)
   (text-delete-line)
   (text-delete-line)
   (when (string-empty? (text-line))
      (text-delete-line))
   (while (pregexp-match "(modified:)|(deleted:)" (text-line))
      (text-delete-longword)
      (text-delete-longword)
      ;; we stay at the file path word
      (add-text-property (cursor) (text-longword-end-pos) '(style: (fg: "red")))
      (add-text-property
         (text-line-begin-pos) (text-line-end-pos)
        `(
          keymap: ,git-status-unstaged-file-map
          data: ,(text-longword)
         ))
      (cursor-to-next-line)))

(define git-status-unmerged-file-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-unstaged-file-diff)
      (bind-key map "u" git-status-update-unstaged-file)
      map))

(define git-status-unmerged-all-map
   (let ([map (make-keymap)])
      (bind-key map "<Enter>" git-status-open-unstaged-all-diff)
      map))

(define (git-status-draw-unmerged)
   (add-text-property
      (text-line-begin-pos) (text-line-end-pos)
     `(
       keymap: ,git-status-unmerged-all-map
       style: (attr: "bold")
      ))
   (cursor-to-next-line)
   (when (string-contains? (text-line) "(use")
      (text-delete-line))
   (text-delete-line)
   (when (string-empty? (text-line))
      (text-delete-line))
   (while (string-contains? (text-line) "modified")
      (text-delete-longword)
      (text-delete-longword)
      (text-delete-longword)
      ;; we stay at the file path word
      (add-text-property (cursor) (text-longword-end-pos) '(style: (fg: "red")))
      (add-text-property
         (text-line-begin-pos) (text-line-end-pos)
        `(
          keymap: ,git-status-unmerged-file-map
          data: ,(text-longword)
         ))
      (cursor-to-next-line)))

(define (git-status-update-untracked-file)
   (git-add-file-cmd (git-status-get-path))
   (buffer-reload))

(define git-status-untracked-file-map
   (let ([map (make-keymap)])
      (bind-key map "u" git-status-update-untracked-file)
      map))

(define (git-status-draw-untracked)
   (cursor-to-next-line)
   (text-delete-line)
   (when (string-empty? (text-line))
      (text-delete-line))
   (while (not (equal? "" (text-line)))
      (text-delete-word)
      ;; we stay at the file path word
      (add-text-property (cursor) (text-longword-end-pos) '(style: (fg: "bright-black")))
      (add-text-property
         (text-line-begin-pos) (text-line-end-pos)
        `(
          keymap: ,git-status-untracked-file-map
          data: ,(text-longword)
         ))
      (cursor-to-next-line)))

(define (git-status-draw-branch-status)
   (text-insert-empty-line))

(define git-status-draw
   (lambda ()
      (buffer-set-readonly #f)
      (set-local! git-status-cursor (cursor))
      (text-delete)
      (process-create "git status" (current-buffer)
         (lambda (status buf-out buf-err)
            (with-current-buffer buf-out
               (if (not (eq? 0 status))
                  #f
                  ;; else
                  (let ()
                     (cursor-to-begin)
                     (text-insert (git-cmd-read "log -1 --oneline"))
                     (text-insert "\n")
                     (cursor-to-each-line
                        (lambda ()
                           (let ([line (text-line)])
                              (cond
                                 [(pregexp-match "^On branch" line) (git-status-draw-branch-status)]
                                 [(pregexp-match "detached at" line) (git-status-draw-branch-status)]
                                 [(pregexp-match "^Changes to be committed" line) (git-status-draw-staged)]
                                 [(pregexp-match "^Changes not staged" line) (git-status-draw-unstaged)]
                                 [(pregexp-match "^Unmerged paths" line) (git-status-draw-unmerged)]
                                 [(pregexp-match "^Untracked files" line) (git-status-draw-untracked)]))))
                     (cursor-set (get-local git-status-cursor))))
               (buffer-set-readonly #t))))))

(define git-status-mode-map
   (let ([map (make-keymap)])
      (bind-key map "c" (lambda () (git-create-commit)))
      (bind-key map "a" (lambda () (git-amend-commit)))
      (bind-key map "U" (lambda () (git-pull-changes)))
      map))

(define-mode git-status-mode "Git" text-mode
   (define-local buffer-reload-func git-status-draw)
   (define-local git-status-cursor (cursor))
   (set-local! linenum-enable #f)
   (buffer-set-name "git-status")
   (git-status-draw)
   (cursor-to-begin))

(define (git-status)
   (buffer-create)
   (git-status-mode))

(define (git-rename-dirb-entry old new)
   (when (git-repo? old)
      (git-untrack-file-cmd old)
      (git-stage-file-cmd new)))

(add-hook 'dirb-rename-entry-hook git-rename-dirb-entry)
(bind-key dirb-mode-map "g l" (lambda () (git-show-log (dirb-entry-path))))
(bind-key text-mode-normal-map "g l" (lambda () (git-show-log (buffer-filename))))
(bind-key text-mode-normal-map "g D" (lambda () (git-status-diff-file 'unstaged (buffer-filename))))
(bind-key text-mode-normal-map "g c" (lambda () (git-show-commit)))
(bind-key text-mode-normal-map "g b" (lambda () (git-blame)))
(bind-key text-mode-visual-linewise-map "g b" (lambda () (git-blame)))
