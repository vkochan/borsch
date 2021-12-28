(load "common.ss")
(load "style.ss")
(load "keymap.ss")
(load "tagbar.ss")
(load "layout.ss")
(load "view.ss")
(load "buffer.ss")
(load "window.ss")

;; Event handling
(define __on-event-cb-list '())

(define __on-event-handler
   (lambda (ev wid)
       (define __evt->symb
	  (lambda (ev)
	     (case ev
	        [0  'window-create   ]
		[1  'window-select   ]
		[2  'window-minimize ]
		[3  'window-maximize ]
		[4  'window-delete   ]
		[20 'view-switch  ]
		[40 'layout-switch]
		[else #f]
             )
          )
       )

       (for-each
	  (lambda (f)
	     (try f (__evt->symb ev) wid)
          )
	  __on-event-cb-list
       )
   )
)
;;

;; FFI
(define __cs_copy_buf_get (foreign-procedure __collect_safe "cs_copy_buf_get" () scheme-object))
(define __cs_copy_buf_set (foreign-procedure "cs_copy_buf_set" (string) int))

(define __cs_do_quit (foreign-procedure "cs_do_quit" () void))
;;

;; Public API
(define copy-buffer-set #f)
(define copy-buffer+ #f)

(define copy-buffer
   (lambda ()
      (__cs_copy_buf_get)
   )
)

(define text-mode-cmd
   (lambda ()
      (buffer-set-keymap 'text-mode-cmd-map)
      (buffer-set-mode "Text <N>")
      (buffer-set-input #f)
      (mark-clear)
      (window-highlight-mark #f)
   )
)

(define text-mode-ins
   (lambda ()
      (buffer-set-keymap 'text-mode-ins-map)
      (buffer-set-mode "Text <I>")
      (buffer-set-input #t)
   )
)

(define text-mode-vis
   (lambda ()
      (buffer-set-keymap 'text-mode-vis-map)
      (buffer-set-mode "Text <V>")
      (buffer-set-input #f)
      (mark-set)
      (window-highlight-mark #t)
   )
)

(define text-mode-map
   (let ([map (make-keymap)])
      map
   )
)

(define text-mode-cmd-map
   (let ([map (make-keymap 'text-mode-map)])
      (bind-key map "h" (lambda () (move-prev-char)))
      (bind-key map "l" (lambda () (move-next-char)))
      (bind-key map "j" (lambda () (move-next-line)))
      (bind-key map "k" (lambda () (move-prev-line)))
      (bind-key map "w" (lambda () (move-next-word)))
      (bind-key map "W" (lambda () (move-next-longword)))
      (bind-key map "b" (lambda () (move-prev-word)))
      (bind-key map "B" (lambda () (move-prev-longword)))
      (bind-key map "e" (lambda () (move-word-end)))
      (bind-key map "E" (lambda () (move-longword-end)))
      (bind-key map "x" (lambda () (delete-next-char)))
      (bind-key map "X" (lambda () (delete-prev-char)))
      (bind-key map "g g" (lambda () (move-buffer-begin)))
      (bind-key map "G" (lambda () (move-buffer-end)))
      (bind-key map "i" text-mode-ins)
      (bind-key map "v" text-mode-vis)
      map
   )
)

(define text-mode-ins-map
   (let ([map (make-keymap 'text-mode-map)])
      (bind-key map "<Enter>" (lambda () (insert-nl)))
      (bind-key map "<Backspace>" (lambda () (delete-prev-char)))
      (bind-key map "<Esc>" text-mode-cmd)
      map
   )
)

(define text-mode-vis-map
   (let ([map (make-keymap 'text-mode-cmd-map)])
      (bind-key map "<Esc>" text-mode-cmd)
      (bind-key map "x" (lambda () (mark-delete)))
      map
   )
)

(define-mode text-mode "Text" #f
   (buffer-set-input #t)
   (text-mode-cmd)
)

(define do-quit __cs_do_quit)

;;;;;;;;;;;;;;;;;;;;;;;
;; Default key bindings
;;;;;;;;;;;;;;;;;;;;;;;
(bind-key "C-g c"       window-shell)
(bind-key "C-g C-x"     window-eval)
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

(bind-key "C-g q q" do-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start with a shell window
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(window-shell)
