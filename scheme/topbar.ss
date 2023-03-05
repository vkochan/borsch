(define topbar-window #f)
(define topbar-buffer #f)

(define tabs-ht (make-eq-hashtable))

(define *current-tab* #f)

(define (current-tab)
   *current-tab*
)

(define (tab-set-current t)
   (set! *current-tab* t)
)

(define-record-type tab
   (fields
      index
      (mutable frame-list)
      (mutable current-frame)
   )
)

(define topbar-draw
   (lambda ()
      (with-current-buffer topbar-buffer
         (text-delete)
         (vector-for-each
            (lambda (t)
               (let* ([fr (tab-current-frame t)]
                      [color (if (equal? t (current-tab))
                                 "blue"
                                 ;; else
                                 (if (> (length (window-list fr)) 0)
                                    "green"
                                    ;;"bright-black"
                                    ;; else
                                    ;;"green"
                                    "bright-black"))])
                  (let ([name (or (frame-name fr) "")])
                     (text-insert
                        (format "[~a~a]" (tab-index t) (if (equal? "" name) "" (string-append ":" name)))
                       `(style: (fg: ,color))))))
            (hashtable-values tabs-ht)
         )
         (text-insert (layout-name))
         (text-insert (format "[~a]" (current-cwd)) '(style: (fg: "bright-yellow")))
      )
   )
)

(define (tab-create)
   (let* ([n (+ 1 (hashtable-size tabs-ht))]
          [t (make-tab n '() #f)]
          [f (frame-create "")]
         )
      (or (current-tab) (tab-set-current t))
      (tab-frame-list-set! t (list f))
      (tab-current-frame-set! t f)
      (hashtable-set! tabs-ht n t)
   )
)

(define (tab-switch n)
   (let ([t (hashtable-ref tabs-ht n #f)])
      (when t
         (tab-set-current t)
         (frame-switch (tab-current-frame t))
      )
   )
)

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
      (let ([w (widget-create "*topbar*" 0 0 (screen-width) 1 'top)])
         (set! topbar-buffer (window-buffer w))
         (set! topbar-window w)
      )
      (for-each
         (lambda (n)
            (tab-create)
         )
         '(1 2 3 4 5 6 7 8 9)
      )
      (tab-switch 1)
      (add-hook 'frame-switch-hook
         (lambda (frame)
            (topbar-draw)
         )
      )
      (add-hook 'change-frame-name-hook
         (lambda ()
            (topbar-draw)
         )
      )
      (add-hook 'change-cwd-hook
         (lambda ()
            (topbar-draw)
         )
      )
      (add-hook 'layout-changed-hook
         (lambda ()
            (topbar-draw)
         )
      )
      (add-hook 'window-toggle-maximize-hook
         (lambda (w)
            (topbar-draw)
         )
      )
      (topbar-draw)
   )
)
