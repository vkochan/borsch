(define __cs_config_dir_get (foreign-procedure "cs_config_dir_get" () scheme-object))

(define config-dir
   (lambda ()
      ((delay (__cs_config_dir_get)))
   )
)

(load "pregexp.scm")
(load "common.ss")
(load "style.ss")
(load "keymap.ss")
(load "layout.ss")
(load "view.ss")
(load "window.ss")
(load "buffer.ss")
(load "command.ss")
(load "complete.ss")
(load "prompt.ss")
(load "copybuf.ss")
(load "topbar.ss")
(load "minibuf.ss")
(load "os.ss")
(load "process.ss")
(load "timer.ss")
(load "syntax.ss")
(load "mode/text.ss")
(load "mode/git.ss")
(load "mode/grep.ss")
(load "mode/vterm.ss")
(load "mode/dirb.ss")
(load "mode/scheme.ss")
(load "mode/c.ss")
(load "mode/dts.ss")
(load "mode/diff.ss")
(load "mode/gnumake.ss")
(load "mode/org.ss")
(load "mode/mail.ss")

(define message-recent "")

(define open-repl
   (lambda ()
      (vterm "borsch-eval -i" "eval")
   )
)

(define main-init
   (lambda (path)
      (when path
         (try load (string-append (config-dir) (if (equal? path "") "/init.ss" path)))
      )
   )
)

(define __on-event-handler
   (lambda (ev oid str)
       (define __evt->symb
	  (lambda (ev)
	     (case ev
		[1    'window-draw-hook ]
		[2    'pre-draw-hook ]
		[3    'post-draw-hook ]
		[100  'key-press-hook   ]
		[101  'text-insert-hook   ]
		[200  'process-exit-hook   ]
		[300  'vterm-filter-hook   ]
		[else #f]
             )
          )
       )

       (let ([h (__evt->symb ev)])
          (when h
             (if (or (eq? h 'post-draw-hook)
                     (eq? h 'pre-draw-hook))
                (run-hooks h)
                ;; else
		(if (eq? h 'vterm-filter-hook)
                   (begin
                      (with-current-buffer oid
                         (let ([fn (get-local vterm-filter-func #f)])
                            (when fn
                               (try fn str)
                            )
                         )
                      )
                   )
                   ;; else
                   (run-hooks h oid)
		)
             )
          )
       )
   )
)

(add-hook 'text-insert-hook
   (lambda (code)
      (let ([b (current-buffer)])
         (when b
            (when (local-bound? text-insert-hook)
               ((get-local text-insert-hook) (integer->char code))
            )
         )
      )
   )
)

(let ([m (buffer-new "*Messages*")])
   (with-current-buffer m
      (text-mode)
   )
)

(add-hook 'error-hook
   (lambda (e)
      (let ([m (buffer-get "*Messages*")])
         (when m
            (with-current-buffer m
               (insert (format "~a\n" e) '(style (:fg "red")))
            )
         )
      )
   )
)

(define new-text-buffer
   (lambda ()
      (let ([b (buffer-create)])
         (with-current-buffer b
            (text-mode)
         )
      )
   )
)

(define open-file-prompt
   (lambda ()
      (minibuf-read "open file:"
         (lambda (f)
            (file-open f)
         )
      )
   )
)

(add-hook 'window-draw-hook
   (lambda (w)
      (let ([b (window-buffer w)])
         (when b
            (with-current-buffer b
               (when (local-bound? window-draw-hook)
                  (let ([h (get-local window-draw-hook)])
                     (apply h (list w))
                  )
               )
            )
         )
      )
   )
)

(add-hook 'change-cwd-hook
   (lambda ()
      (let (
            [local-script (string-append (view-cwd) "/" ".borsch.ss")]
           )
         (when (file-exists? local-script)
            (load local-script)
         )
      )
   )
)

(add-hook 'message-hook
   (lambda (m)
      (set! message-recent m)
   )
)

(define minibuf-cmd
   (lambda ()
      (minibuf-complete
         (map
            (lambda (c)
               (cons (command-name c) c)
            )
            command-list
         )
         (lambda (c)
            ((command-func c))
         )
         "Cmd"
      )
   )
)

(minibuf-create)
(topbar-create)

(view-set-cwd (current-directory))

;;;;;;;;;;;;;;;;;;;;;;;
;; Default key bindings
;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "M-h"         window-select-left)
(bind-key "M-l"         window-select-right)
(bind-key "M-j"         window-select-lower)
(bind-key "M-k"         window-select-upper)
(bind-key "M-w d"       window-delete)
(bind-key "M-w h"       window-select-left)
(bind-key "M-w l"       window-select-right)
(bind-key "M-w j"       window-select-lower)
(bind-key "M-w k"       window-select-upper)
(bind-key "M-w d"       window-delete)
(bind-key "M-w +"       layout-n-master+)
(bind-key "M-w -"       layout-n-master-)
(bind-key "M-w H"       layout-%-master-)
(bind-key "M-w L"       layout-%-master+)
(bind-key "M-w s"       layout-toggle-sticky)
(bind-key "M-w f"       layout-switch-tiled)
(bind-key "M-w g"       layout-switch-grid)
(bind-key "M-w b"       layout-switch-bstack)
(bind-key "M-w m"       window-toggle-maximized)
(bind-key "M-w ."       window-set-minimized)
(bind-key "M-w <Enter>" window-set-master)
(bind-key "M-b n"       new-text-buffer)
(bind-key "M-b s"       buffer-switch)
(bind-key "M-b c"       window-close)
(bind-key "M-b o"       buffer-open)
(bind-key "M-1"         view-switch-1)
(bind-key "M-2"         view-switch-2)
(bind-key "M-3"         view-switch-3)
(bind-key "M-4"         view-switch-4)
(bind-key "M-5"         view-switch-5)
(bind-key "M-6"         view-switch-6)
(bind-key "M-7"         view-switch-7)
(bind-key "M-8"         view-switch-8)
(bind-key "M-9"         view-switch-9)
(bind-key "M-0"         view-switch-all)

(bind-key "C-g c"       vterm)
(bind-key "C-g C-x"     open-repl)
(bind-key "C-g x x"     window-delete)
(bind-key "C-g h"       window-select-left)
(bind-key "C-g l"       window-select-right)
(bind-key "C-g j"       window-select-lower)
(bind-key "C-g k"       window-select-upper)
(bind-key "C-g <Enter>" window-set-master)
(bind-key "C-g ."       window-set-minimized)

(bind-key "C-g v 1" view-switch-1)
(bind-key "C-g v 2" view-switch-2)
(bind-key "C-g v 3" view-switch-3)
(bind-key "C-g v 4" view-switch-4)
(bind-key "C-g v 5" view-switch-5)
(bind-key "C-g v 6" view-switch-6)
(bind-key "C-g v 7" view-switch-7)
(bind-key "C-g v 8" view-switch-8)
(bind-key "C-g v 9" view-switch-9)
(bind-key "C-g v 0" view-switch-all)

(bind-key "C-g w i"   layout-n-master+)
(bind-key "C-g w d"   layout-n-master-)
(bind-key "C-g w h"   layout-%-master-)
(bind-key "C-g w l"   layout-%-master+)
(bind-key "C-g C-s" layout-toggle-sticky)
(bind-key "C-g f"   layout-switch-tiled)
(bind-key "C-g g"   layout-switch-grid)
(bind-key "C-g b"   layout-switch-bstack)
(bind-key "C-g m"   window-toggle-maximized)
(bind-key "C-g n"   new-text-buffer)
(bind-key "C-g o"   open-file-prompt)

(bind-key "C-x b s" buffer-switch)
(bind-key "C-x b c" window-close)
(bind-key "C-x b o" buffer-open)

(bind-key "C-g y m" (lambda () (copybuf-put (pregexp-replace "\n$" message-recent ""))))
(bind-key "C-g y w" (lambda () (copybuf-put (current-cwd))))

(bind-key "C-x g s" git-status)
(bind-key "C-x g c" git-switch-branch)
(bind-key "C-x g b" git-create-and-switch-branch)
(bind-key "C-x g l" git-show-log)
(bind-key "C-x g u" git-pull-changes-and-show)

(bind-key "M-e" minibuf-eval)
(bind-key "M-x" minibuf-cmd)

(bind-key "C-x f d" dirb)

(bind-key "C-x s g" grep)

(bind-key "C-g q q" do-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start with a terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(vterm)
