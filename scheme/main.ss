(load "common.ss")
(load "style.ss")
(load "keymap.ss")
(load "tagbar.ss")
(load "layout.ss")
(load "view.ss")
(load "window.ss")
(load "buffer.ss")
(load "prompt.ss")
(load "minibuf.ss")
(load "process.ss")
(load "grep.ss")
(load "mode/term.ss")
(load "mode/text.ss")
(load "mode/dirb.ss")
(load "mode/lisp.ss")

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

(define append-to-register
   (case-lambda
      [(s)
       (set! reg-is-linewise #f)
       (set! reg (string-append reg " " s))]

      [(s l)
       (set! reg-is-linewise #t)
       (set! reg (string-append reg "\n" s))]
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
		[100  'key-press-hook   ]
		[1000 'idle-hook   ]
		[else #f]
             )
          )
       )

       (let ([h (__evt->symb ev)])
          (when h
             (if (eq? h 'idle-hook)
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
