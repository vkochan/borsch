(load "common.ss")
(load "style.ss")
(load "keymap.ss")
(load "tagbar.ss")
(load "layout.ss")
(load "view.ss")
(load "buffer.ss")
(load "window.ss")
(load "mode/text.ss")

;; FFI
(define __cs_do_quit (foreign-procedure "cs_do_quit" () void))

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

(bind-key "C-b s" buffer-switch)
(bind-key "C-b q" window-close)
(bind-key "C-b o" buffer-open)

(bind-key "C-g q q" do-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start with a shell window
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(window-shell)
