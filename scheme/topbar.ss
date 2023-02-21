(define topbar-window #f)
(define topbar-buffer #f)

(define topbar-draw
   (lambda ()
      (with-current-buffer topbar-buffer
         (text-delete)
         (for-each
            (lambda (fr)
               (let (
                     [fr-color (if (equal? fr (current-frame))
                                    "blue"
                                    ;; else
                                    (if (> (length (window-list fr)) 0)
                                       "green"
                                       ;;"bright-black"
                                       ;; else
                                       ;;"green"
                                       "bright-black"
                                    )
                                )
                     ]
                     [vname (frame-name fr)]
                    )
                  (let ([name (or vname "")])
                     (text-insert
                        (format "[~a~a]" (frame-id fr) (if (equal? "" name) "" (string-append ":" name)))
                       `(style: (fg: ,fr-color))
                     )
                  )
               )
            )
            (list frame-1 frame-2 frame-3 frame-4 frame-5 frame-6 frame-7 frame-8 frame-9)
         )
         (text-insert (layout-name))
         (text-insert (format "[~a]" (current-cwd)) '(style: (fg: "bright-yellow")))
      )
   )
)

(define topbar-create
   (lambda ()
      (let ([w (widget-create "*topbar*" 0 0 (screen-width) 1 'top)])
         (set! topbar-buffer (window-buffer w))
         (set! topbar-window w)
      )
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
