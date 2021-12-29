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
(define __cs_do_quit (foreign-procedure "cs_do_quit" () void))
;;

(define reg "")
(define reg-is-linewise #f)

;; Public API
(define copy-to-register
   (case-lambda
      [(s)
       (set! reg-is-linewise #f)
       (set! reg s)]

      [(s l)
       (set! reg-is-linewise #t)
       (set! reg s)]
   )
)

(define paste-from-register-inplace
   (lambda ()
      (insert reg)
   )
)

(define paste-from-register
   (lambda ()
      (if (not reg-is-linewise)
         (begin
            (when (not (equal? #\newline (extract-char)))
               (move-next-char)
            )
            (paste-from-register-inplace)
            (move-prev-char)
          )
          ;; else
          (begin
             (move-line-end)
             (insert-nl)
             (save-cursor
                (paste-from-register-inplace)
                (delete-char)
             )
          )
      )
   )
)

(define paste-from-register-before
   (lambda ()
      (paste-from-register-inplace)
      (move-prev-char)
   )
)

(define text-mode-cmd
   (lambda ()
      (buffer-set-keymap 'text-mode-cmd-map)
      (buffer-set-mode "Text <N>")
      (enable-insert #f)
      (mark-clear)
      (mark-highlight #f)
   )
)

(define text-mode-ins
   (lambda ()
      (buffer-set-keymap 'text-mode-ins-map)
      (buffer-set-mode "Text <I>")
      (enable-insert #t)
   )
)

(define text-mode-vis
   (lambda ()
      (buffer-set-keymap 'text-mode-vis-map)
      (buffer-set-mode "Text <V>")
      (enable-insert #f)
      (mark-set)
      (mark-highlight #t)
   )
)

(define text-mode-vis-linewise
   (lambda ()
      (buffer-set-keymap 'text-mode-vis-linewise-map)
      (buffer-set-mode "Text <V *L*>")
      (enable-insert #f)
      (mark-set (line-begin-pos))
      (move-line-end)
      (mark-highlight #t)
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
      (bind-key map "D" (lambda () (delete-line-end)))
      (bind-key map "d d" (lambda () (delete-line)))
      (bind-key map "g g" (lambda () (move-buffer-begin)))
      (bind-key map "G" (lambda () (move-buffer-end)))
      (bind-key map "i" (lambda () (text-mode-ins)))
      (bind-key map "v" (lambda () (text-mode-vis)))
      (bind-key map "V" (lambda () (text-mode-vis-linewise)))
      (bind-key map "p" (lambda () (paste-from-register)))
      (bind-key map "P" (lambda () (paste-from-register-before)))
      (bind-key map "Y" (lambda () (copy-line)))
      (bind-key map "y y" (lambda () (copy-line)))
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
      (bind-key map "x" (lambda () (mark-delete) (text-mode-cmd)))
      (bind-key map "y" (lambda () (mark-copy) (text-mode-cmd)))
      map
   )
)

(define text-mode-vis-linewise-map
   (let ([map (make-keymap 'text-mode-map)])
      (bind-key map "<Esc>" text-mode-cmd)
      (bind-key map "x" (lambda () (mark-delete) (text-mode-cmd)))
      (bind-key map "l" (lambda () (move-next-line)))
      (bind-key map "j" (lambda () (move-next-line) (move-line-end)))
      (bind-key map "k" (lambda () (move-prev-line) (move-line-end)))
      (bind-key map "y" (lambda () (mark-copy-linewise) (text-mode-cmd)))
      map
   )
)

(define-mode text-mode "Text" #f
   (enable-insert #t)
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
