(define __cs_topbar_create (foreign-procedure "cs_topbar_create" () scheme-object))

(define topbar-window #f)
(define topbar-buffer #f)

(define topbar-draw
   (lambda ()
      (with-current-buffer topbar-buffer
         (erase-buffer)
         (for-each
            (lambda (tag)
               (let (
                     [tag-color (if (equal? tag (current-view))
                                    "blue"
                                    ;; else
                                    (if (> (length (window-list-by-tag tag)) 0)
                                       "green"
                                       ;;"bright-black"
                                       ;; else
                                       ;;"green"
                                       "bright-black"
                                    )
                                )
                     ]
                     [vname (view-name tag)]
                    )
                  (let ([name (or vname "")])
                     (insert
                        (format "[~a~a]" tag (if (equal? "" name) "" (string-append ":" name)))
                       `(:style (:fg ,tag-color))
                     )
                  )
               )
            )
            '(1 2 3 4 5 6 7 8 9)
         )
         (insert (layout-name))
         (insert (format "[~a]" (current-cwd)) '(:style (:fg "bright-yellow")))
      )
   )
)

(define topbar-create
   (lambda ()
      (let ([w (call-foreign (__cs_topbar_create))])
         (set! topbar-buffer (window-buffer w))
         (set! topbar-window w)
      )
      (add-hook 'view-switch-hook
         (lambda (tag)
            (topbar-draw)
         )
      )
      (add-hook 'change-view-name-hook
         (lambda ()
            (topbar-draw)
         )
      )
      (add-hook 'change-cwd-hook
         (lambda ()
            (topbar-draw)
         )
      )
      (add-hook 'layout-switch-hook
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
