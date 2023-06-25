(define topbar-window #f)
(define topbar-buffer #f)

(define tabs-ht (make-eq-hashtable))

(define *current-tab* #f)

(define (current-tab) *current-tab*)

(define (tab-set-current t) (set! *current-tab* t))

(define-record-type tab
   (fields
      index
      (mutable name)
      (mutable frame-stack)
      (mutable current-frame)))

(define (tab-frame-list t)
   (stack-list (tab-frame-stack t)))

(define topbar-draw
   (lambda ()
      (with-current-buffer topbar-buffer
         (text-delete)
         (vector-for-each
            (lambda (t)
               (let* ([fr (tab-current-frame t)]
                      [attr (if (equal? t (current-tab))
                                "bold"
                                "normal")]
                      [color (if (equal? t (current-tab))
                                 "white"
                                 ;; else
                                 (if (> (length (window-list fr)) 0)
                                    "green"
                                    ;;"bright-black"
                                    ;; else
                                    ;;"green"
                                    "bright-black"))])
                  (let ([fname (frame-name fr)]
                        [tname (tab-name t)])
                     (text-insert
                        (format "[~a~a~a]" (tab-index t)
                                           (if (equal? "" tname) "" (string-append ":" tname))
                                           (if (and (equal? t (current-tab))
                                                    (> (length (tab-frame-list t))
                                                       1)
                                                    (not (equal? fname "")))
                                              (string-append "|" fname)
                                              ;; else
                                              ""))
                       `(style: (fg: ,color attr: ,attr))))))
            (hashtable-values tabs-ht))
         (text-insert (layout-name))
         (text-insert (format "[~a]" (current-cwd)) '(style: (fg: "bright-yellow"))))))

(define (tab-switch-frame fr)
   (let ([t (current-tab)])
      (tab-current-frame-set! t fr)
      (stack-remove! (tab-frame-stack t) fr)
      (stack-push! (tab-frame-stack t) fr)
      (frame-switch fr)))

(define (tab-create-frame)
   (let ([t (current-tab)]
         [f (frame-create)])
      (stack-push! (tab-frame-stack t) f)
      (tab-switch-frame f)))

(define (tab-find-frame)
   (minibuf-complete
      (map
         (lambda (fr)
            (cons (frame-name fr)
                  fr))
         (tab-frame-list (current-tab)))
      (lambda (fr)
         (tab-switch-frame fr))
      "Switch to frame"))

(define (tab-delete-frame)
   (let ([f (current-frame)]
         [t (current-tab)])
      (when (> (length (tab-frame-list t))
               1)
         (stack-remove (tab-frame-stack t) f)
         (when (not (null? (tab-frame-list t)))
            (tab-switch-frame (first (tab-frame-list t))))
         (frame-delete f))))

(define (tab-rename-frame)
   (minibuf-read "Rename frame: " (frame-name (current-frame))
      (lambda (v)
         (frame-set-name v))))

(define (tab-rename)
   (minibuf-read "Rename tab: " (tab-name (current-tab))
      (lambda (v)
         (tab-name-set! (current-tab) v)
         (topbar-draw))))

(define (tab-create)
   (let* ([n (+ 1 (hashtable-size tabs-ht))]
          [t (make-tab n "" (make-stack) #f)]
          [f (frame-create "main")])
      (or (current-tab)
          (tab-set-current t))
      (stack-push! (tab-frame-stack t) f)
      (tab-current-frame-set! t f)
      (hashtable-set! tabs-ht n t)))

(define (tab-switch n)
   (let ([t (hashtable-ref tabs-ht n #f)])
      (when t
         (tab-set-current t)
         (frame-switch (tab-current-frame t)))))

(define (tab-switch-1)
    (tab-switch 1))

(define (tab-switch-2)
    (tab-switch 2))

(define (tab-switch-3)
    (tab-switch 3))

(define (tab-switch-4)
    (tab-switch 4))

(define (tab-switch-5)
    (tab-switch 5))

(define (tab-switch-6)
    (tab-switch 6))

(define (tab-switch-7)
    (tab-switch 7))

(define (tab-switch-8)
    (tab-switch 8))

(define (tab-switch-9)
    (tab-switch 9))

(define topbar-create
   (lambda ()
      (let ([w (widget-create "*topbar*" 0 0 (ui-screen-width) 1 'top)])
         (set! topbar-buffer (window-buffer w))
         (set! topbar-window w))
      (for-each
         (lambda (n)
            (tab-create))
         '(1 2 3 4 5 6 7 8 9))
      (tab-switch 1)
      (add-hook 'frame-switch-hook
         (lambda (frame)
            (topbar-draw)))
      (add-hook 'change-frame-name-hook
         (lambda ()
            (topbar-draw)))
      (add-hook 'change-cwd-hook
         (lambda ()
            (topbar-draw)))
      (add-hook 'layout-changed-hook
         (lambda ()
            (topbar-draw)))
      (add-hook 'window-toggle-maximize-hook
         (lambda (w)
            (topbar-draw)))
      (topbar-draw)))
