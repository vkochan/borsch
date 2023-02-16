(define __cs_config_dir_get (foreign-procedure "cs_config_dir_get" () scheme-object))

(define config-dir
   (lambda ()
      ((delay (__cs_config_dir_get)))
   )
)

(include "pregexp.scm")
(include "common.ss")
(include "style.ss")
(include "keymap.ss")
(include "screen.ss")
(include "layout.ss")
(include "frame.ss")
(include "window.ss")
(include "buffer.ss")
(include "text.ss")
(include "command.ss")
(include "complete.ss")
(include "copybuf.ss")
(include "topbar.ss")
(include "process.ss")
(include "vterm.ss")
(include "minibuf.ss")
(include "os.ss")
(include "timer.ss")
(include "syntax.ss")
(include "git.ss")
(include "mode/text.ss")
(include "mode/dirb.ss")
(include "mode/git.ss")
(include "mode/grep.ss")
(include "mode/scheme.ss")
(include "mode/c.ss")
(include "mode/dts.ss")
(include "mode/diff.ss")
(include "mode/gnumake.ss")
(include "mode/org.ss")
(include "mode/mail.ss")

(define message-recent "")

(define open-repl
   (lambda ()
      (vterm "borsch-eval -i" "eval")
   )
)

(define load-init-script
   (lambda (init)
      (let ([init-script (if (string-empty? init)
                            (string-append (config-dir) "/init.ss")
                            ;; else
                            init)]
           )
         (when (file-exists? init-script)
            (try load init-script)
            (run-hooks 'init-hook)
         )
      )
   )
)

(define main-init
   (lambda (init)
      (when init
         (load-init-script init))
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
               (text-insert (format "~a\n" e) '(style: (fg: "red")))
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
      (minibuf-complete-path
         (lambda (f)
            (file-open f)
         )
         "open file"
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
            [local-script (string-append (current-cwd) "/" ".borsch.ss")]
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

(define minibuf-buffer-list->complete
   (lambda ()
      (map
         (lambda (b)
            (let (
                  [mode (buffer-mode-name (first b))]
                  [name (buffer-name (first b))]
                 )
               (cons (format "(~a) ~a" mode name) (first b))
            )
         )
         (buffer-list)
      )
   )
)

(define minibuf-switch-buffer
   (lambda ()
      (minibuf-complete
         (minibuf-buffer-list->complete)
         (lambda (b)
            (window-switch-buffer b)
         )
         "Switch to buffer"
      )
   )
)

(define minibuf-open-buffer
   (lambda ()
      (minibuf-complete
         (minibuf-buffer-list->complete)
         (lambda (b)
            (window-create b)
         )
         "Open buffer"
      )
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

(frame-set-cwd (current-directory))

(define window-delete-non-sticky
   (lambda ()
      (window-for-each
         (lambda (wid)
            (when (not (window-is-sticky? wid))
               (window-delete wid)
            )
         )
      )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;
;; Default key bindings
;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "M-h"         window-select-left)
(bind-key "M-l"         window-select-right)
(bind-key "M-j"         window-select-lower)
(bind-key "M-k"         window-select-upper)
(bind-key "M-w d"       window-delete)
(bind-key "M-w o d"     window-delete-non-sticky)
(bind-key "M-w h"       window-select-left)
(bind-key "M-w l"       window-select-right)
(bind-key "M-w j"       window-select-lower)
(bind-key "M-w k"       window-select-upper)
(bind-key "M-w +"       layout-n-master+)
(bind-key "M-w -"       layout-n-master-)
(bind-key "M-w H"       layout-%-master-)
(bind-key "M-w L"       layout-%-master+)
(bind-key "M-w s"       layout-toggle-sticky)
(bind-key "M-w f"       layout-switch-tiled)
(bind-key "M-w g"       layout-switch-grid)
(bind-key "M-w b"       layout-switch-bstack)
(bind-key "M-w m"       window-toggle-maximized)
(bind-key "M-w <Enter>" window-set-master)
(bind-key "M-b n"       new-text-buffer)
(bind-key "M-b s"       minibuf-switch-buffer)
(bind-key "M-b c"       window-close)
(bind-key "M-b o"       minibuf-open-buffer)
(bind-key "M-1"         frame-switch-1)
(bind-key "M-2"         frame-switch-2)
(bind-key "M-3"         frame-switch-3)
(bind-key "M-4"         frame-switch-4)
(bind-key "M-5"         frame-switch-5)
(bind-key "M-6"         frame-switch-6)
(bind-key "M-7"         frame-switch-7)
(bind-key "M-8"         frame-switch-8)
(bind-key "M-9"         frame-switch-9)
(bind-key "M-0"         frame-switch-all)

(bind-key "C-g c"       vterm)
(bind-key "C-g C-x"     open-repl)
(bind-key "C-g x x"     window-delete)
(bind-key "C-g x o"     window-delete-non-sticky)
(bind-key "C-g h"       window-select-left)
(bind-key "C-g l"       window-select-right)
(bind-key "C-g j"       window-select-lower)
(bind-key "C-g k"       window-select-upper)
(bind-key "C-g <Enter>" window-set-master)

(bind-key "C-g f 1" frame-switch-1)
(bind-key "C-g f 2" frame-switch-2)
(bind-key "C-g f 3" frame-switch-3)
(bind-key "C-g f 4" frame-switch-4)
(bind-key "C-g f 5" frame-switch-5)
(bind-key "C-g f 6" frame-switch-6)
(bind-key "C-g f 7" frame-switch-7)
(bind-key "C-g f 8" frame-switch-8)
(bind-key "C-g f 9" frame-switch-9)
(bind-key "C-g f 0" frame-switch-all)

(bind-key "C-g w i"   layout-n-master+)
(bind-key "C-g w d"   layout-n-master-)
(bind-key "C-g w h"   layout-%-master-)
(bind-key "C-g w l"   layout-%-master+)
(bind-key "C-g C-s" layout-toggle-sticky)
(bind-key "C-g w f"   layout-switch-tiled)
(bind-key "C-g w g"   layout-switch-grid)
(bind-key "C-g w b"   layout-switch-bstack)
(bind-key "C-g m"   window-toggle-maximized)
(bind-key "C-g n"   new-text-buffer)
(bind-key "C-g o"   open-file-prompt)

(bind-key "C-x b s" minibuf-switch-buffer)
(bind-key "C-x b c" window-close)
(bind-key "C-x b o" minibuf-open-buffer)

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
(bind-key "C-x f f" minibuf-find-file)

(bind-key "C-x s g" grep)

(bind-key "C-g q q" do-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start with a terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(vterm)
