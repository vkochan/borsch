(load "common.ss")
(load "style.ss")
(load "keymap.ss")
(load "tagbar.ss")
(load "layout.ss")
(load "view.ss")
(load "window.ss")
(load "buffer.ss")
(load "prompt.ss")
(load "copybuf.ss")
(load "minibuf.ss")
(load "process.ss")
(load "mode/grep.ss")
(load "mode/term.ss")
(load "mode/text.ss")
(load "mode/dirb.ss")
(load "mode/lisp.ss")

(define open-repl
   (lambda ()
      (term "borsch-eval -i" "eval")
   )
)

(minibuf-create)

(define __on-event-handler
   (lambda (ev oid)
       (define __evt->symb
	  (lambda (ev)
	     (case ev
		[1    'window-draw-hook ]
		[2    'pre-draw-hook ]
		[3    'post-draw-hook ]
		[100  'key-press-hook   ]
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
                (run-hooks h oid)
             )
          )
       )
   )
)

(let ([m (buffer-new "*Messages*")])
   (with-buffer m
      (text-mode)
   )
)

(add-hook 'on-error-hook
   (lambda (e)
      (let ([m (buffer-get "*Messages*")])
         (when m
            (with-buffer m
               (insert (format "~a\n" e) '(style (:fg "red")))
            )
         )
      )
   )
)

(tagbar-set-status-align 'left)

(add-hook 'tagbar-status-hook
   (lambda ()
      (tagbar-set-status (view-cwd))
   )
)

(define new-text-buffer
   (lambda ()
      (let ([b (buffer-create)])
         (with-buffer b
            (text-mode)
         )
      )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;
;; Default key bindings
;;;;;;;;;;;;;;;;;;;;;;;
(bind-key "C-g c"       term)
(bind-key "C-g C-x"     open-repl)
(bind-key "C-g x x"     window-delete)
(bind-key "M-h"         window-select-left)
(bind-key "C-g h"       window-select-left)
(bind-key "M-l"         window-select-right)
(bind-key "C-g l"       window-select-right)
(bind-key "M-j"         window-select-lower)
(bind-key "C-g j"       window-select-lower)
(bind-key "M-k"         window-select-upper)
(bind-key "C-g k"       window-select-upper)
(bind-key "C-g <Enter>" window-set-master)
(bind-key "C-g ."       window-set-minimized)

(bind-key "M-1"     view-switch-1)
(bind-key "C-g v 1" view-switch-1)
(bind-key "M-2"     view-switch-2)
(bind-key "C-g v 2" view-switch-2)
(bind-key "M-3"     view-switch-3)
(bind-key "C-g v 3" view-switch-3)
(bind-key "M-4"     view-switch-4)
(bind-key "C-g v 4" view-switch-4)
(bind-key "M-5"     view-switch-5)
(bind-key "C-g v 5" view-switch-5)
(bind-key "M-6"     view-switch-6)
(bind-key "C-g v 6" view-switch-6)
(bind-key "M-7"     view-switch-7)
(bind-key "C-g v 7" view-switch-7)
(bind-key "M-8"     view-switch-8)
(bind-key "C-g v 8" view-switch-8)
(bind-key "M-9"     view-switch-9)
(bind-key "C-g v 9" view-switch-9)
(bind-key "M-0"     view-switch-all)
(bind-key "C-g v 0" view-switch-all)

(bind-key "C-g i"   layout-n-master+)
(bind-key "C-g d"   layout-n-master-)
(bind-key "C-g H"   layout-%-master-)
(bind-key "C-g L"   layout-%-master+)
(bind-key "C-g C-s" layout-toggle-sticky)
(bind-key "C-g f"   layout-switch-tiled)
(bind-key "C-g g"   layout-switch-grid)
(bind-key "C-g b"   layout-switch-bstack)
(bind-key "C-g m"   window-toggle-maximized)
(bind-key "C-g n"   new-text-buffer)

(bind-key "C-x b s" buffer-switch)
(bind-key "C-x b c" window-close)
(bind-key "C-x b o" buffer-open)

(bind-key "C-x f d" dirb)

(bind-key "C-x s g" grep)

(bind-key "C-g q q" do-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start with a terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(term)
