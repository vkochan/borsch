(define *config-dir* #f)
(define (config-dir)
   *config-dir*)
   
(define __cs_runtime_init (foreign-procedure "cs_runtime_init" () int))
(define __cs_runtime_cleanup (foreign-procedure "cs_runtime_cleanup" () void))
(define __cs_library_directory (foreign-procedure "cs_library_directory" () scheme-object))

(define (runtime-init)
   (set! *config-dir* (string-append (getenv "HOME") "/.config/borsch"))
   (library-directories (list (__cs_library_directory)
                              (config-dir)))
   (compile-imported-libraries #t)
   (__cs_runtime_init))

(define (runtime-cleanup)
   (__cs_runtime_cleanup))

(runtime-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "borsch.ss")

(import (borsch))

(include "pregexp.scm")
(include "common.ss")
(include "file.ss")
(include "style.ss")
(include "keymap.ss")
(include "ui.ss")
(include "frame.ss")
(include "layout.ss")
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
(include "ssh.ss")
(include "git.ss")
(include "mode/text.ss")
(include "mode/dirb.ss")
(include "mode/git.ss")
(include "mode/grep.ss")
(include "mode/org.ss")
(include "mode/mail.ss")
(include "mode/prog/c.ss")
(include "mode/prog/dts.ss")
(include "mode/prog/diff.ss")
(include "mode/prog/scheme.ss")
(include "mode/prog/gnumake.ss")

(define message-recent "")
(define message-buf #f)

(define (open-repl)
   (vterm "borsch-eval -i" "eval"))

(define (load-init-script init)
   (let ([init-script (if (string-empty? init)
                         (string-append (config-dir) "/init.ss")
                         ;; else
                         init)])
      (when (file-exists? init-script)
         (try (load init-script) ))))

(define is-running? #t)

(define (sigterm-handle-func num)
   (set! is-running? #f))

(define (main-init args)
   (let ([do-init? #t]
         [init-script ""]
         [alen (length args)]
         [ui-type 0]
         [i 0])
      (while (< i (length args))
         (let ([a (list-ref args i)])
            (cond
               ((equal? a "-n")
                (set! do-init? #f))
               ((equal? a "-g")
                (set! ui-type 1))
               ((equal? a "-i")
                (when (<= i alen)
                   (set! init-script (path-last (list-ref args (+ i 1))))
                   ))))
         (set! i (+ i 1)))
      (ui-init ui-type)
      (run-hooks 'init-hook)
      (minibuf-create)
      (topbar-create)
      (init-key-bindings)
      (let ([m (buffer-new "*Messages*")])
         (buffer-ref-get m)
         (set! message-buf m)
         (frame-remove-buffer m)
         (with-current-buffer m
            (text-mode)))
      (init-hooks)
      (vterm)
      (when do-init?
         (load-init-script init-script))
      (register-signal-handler 15 sigterm-handle-func)
      (while is-running?
         (ui-process))
      (runtime-cleanup)))

(define (__on-event-handler ev oid str)
   (define (__evt->symb ev)
      (case ev
         [2    'pre-draw-hook ]
         [3    'post-draw-hook ]
         [100  'key-press-hook   ]
         [101  'text-insert-hook   ]
         [200  'process-exit-hook   ]
         [300  'process-filter-hook   ]
         [else #f]))

   (let ([h (__evt->symb ev)])
      (when h
         (if (or (eq? h 'post-draw-hook)
                 (eq? h 'pre-draw-hook))
            (run-hooks h)
            ;; else
            (if (eq? h 'process-filter-hook)
               (run-hooks h oid str)
               ;; else
               (run-hooks h oid))))))

(define (init-hooks)
   (add-hook 'text-insert-hook
      (lambda (code)
         (let ([b (current-buffer)])
            (when b
               (when (local-bound? text-insert-hook)
                  ((get-local text-insert-hook) code))))))

   (add-hook 'error-hook
      (lambda (e)
         (let ([m (buffer-get "*Messages*")])
            (when m
               (with-current-buffer m
                  (text-insert (format "~a\n" e) '(style: (fg: "red"))))))))

   (add-hook 'text-draw-hook
      (lambda (w)
         (let ([b (window-buffer w)])
            (when b
               (with-current-buffer b
                  (when (local-bound? text-draw-hook)
                     (let ([h (get-local text-draw-hook)])
                        (apply h (list w)))))))))

   (add-hook 'change-cwd-hook
      (lambda ()
         (let ([local-script (string-append (current-cwd) "/" ".borsch.ss")])
            (when (file-exists? local-script)
               (load local-script)))))

   (add-hook 'message-hook
      (lambda (m)
         (set! message-recent m))))

(define (new-text-buffer)
   (let ([b (buffer-create)])
      (with-current-buffer b
         (text-mode))))

(define (open-file-prompt)
   (minibuf-complete-path
      (lambda (f)
         (file-open f))
      "open file"))

(define minibuf-buffer-list->complete
   (case-lambda
      [()
       (minibuf-buffer-list->complete (lambda (b) #t))]

      [(lst)
       (minibuf-buffer-list->complete lst (lambda (b) #t))]

      [(lst fn-filter)
       (map
            (lambda (b)
               (let ([mode (buffer-mode-name b)]
                     [name (buffer-name b)])
                  (cons (format "(~a) ~a" mode name) b)))
            (filter fn-filter lst))]))

(define (minibuf-buffer-list-in-frame->complete)
   (minibuf-buffer-list->complete
      (append (frame-buffer-list) (list message-buf))
      (lambda (b) (not (buffer-is-visible? b)))))

(define (minibuf-buffer-list-all->complete)
   (minibuf-buffer-list->complete (buffer-list)))

(define (minibuf-switch-buffer-all)
   (minibuf-complete (minibuf-buffer-list-all->complete)
                     (lambda (b)
                        (window-switch-buffer b))
                     "Switch to buffer"))

(define (minibuf-open-buffer-all)
   (minibuf-complete (minibuf-buffer-list-all->complete)
                     (lambda (b)
                        (window-create b))
                     "Open buffer"))

(define (minibuf-switch-buffer-in-frame)
   (minibuf-complete (minibuf-buffer-list-in-frame->complete)
                     (lambda (b)
                        (window-switch-buffer b))
                     "Switch to buffer in frame"))

(define (minibuf-open-buffer-in-frame)
   (minibuf-complete (minibuf-buffer-list-in-frame->complete)
                     (lambda (b)
                        (window-create b))
                     "Open buffer in frame"))

(define (minibuf-cmd)
   (minibuf-complete
      (map (lambda (c)
              (cons (command-name c) c))
           command-list)
      (lambda (c)
         ((command-func c)))
      "Cmd"))

(define (window-delete-non-sticky)
   (window-for-each
      (lambda (wid)
         (when (and (not (window-is-sticky? wid))
                    (not (equal? wid (current-window))))
            (window-delete wid)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Default key bindings
;;;;;;;;;;;;;;;;;;;;;;;

(define (init-key-bindings)
   (bind-key "M-h"         window-focus-left)
   (bind-key "M-l"         window-focus-right)
   (bind-key "M-j"         window-focus-lower)
   (bind-key "M-k"         window-focus-upper)
   (bind-key "M-w d"       window-delete)
   (bind-key "M-w o d"     window-delete-non-sticky)
   (bind-key "M-w h"       window-focus-left)
   (bind-key "M-w l"       window-focus-right)
   (bind-key "M-w j"       window-focus-lower)
   (bind-key "M-w k"       window-focus-upper)
   (bind-key "M-w +"       layout-n-master+)
   (bind-key "M-w -"       layout-n-master-)
   (bind-key "M-w H"       layout-%-master-)
   (bind-key "M-w L"       layout-%-master+)
   (bind-key "M-w s"       layout-toggle-sticky)
   (bind-key "M-w f"       layout-set-tiled)
   (bind-key "M-w g"       layout-set-grid)
   (bind-key "M-w b"       layout-set-bstack)
   (bind-key "M-w m"       layout-toggle-maximized)
   (bind-key "M-w <Enter>" window-set-master)
   (bind-key "M-b n"       new-text-buffer)
   (bind-key "M-b S"       minibuf-switch-buffer-all)
   (bind-key "M-b s"       minibuf-switch-buffer-in-frame)
   (bind-key "M-b c"       window-close)
   (bind-key "M-b O"       minibuf-open-buffer-all)
   (bind-key "M-b o"       minibuf-open-buffer-in-frame)
   (bind-key "M-1"         tab-switch-1)
   (bind-key "M-2"         tab-switch-2)
   (bind-key "M-3"         tab-switch-3)
   (bind-key "M-4"         tab-switch-4)
   (bind-key "M-5"         tab-switch-5)
   (bind-key "M-6"         tab-switch-6)
   (bind-key "M-7"         tab-switch-7)
   (bind-key "M-8"         tab-switch-8)
   (bind-key "M-9"         tab-switch-9)
   (bind-key "M-f d" dirb)
   (bind-key "M-f f" minibuf-find-file)

   (bind-key "C-g f f"     tab-find-frame)
   (bind-key "C-g f d"     tab-delete-frame)
   (bind-key "C-g f n"     tab-create-frame)
   (bind-key "C-g f r"     tab-rename-frame)

   (bind-key "C-g c"       vterm)
   (bind-key "C-g C-x"     open-repl)
   (bind-key "C-g x x"     window-delete)
   (bind-key "C-g x o"     window-delete-non-sticky)
   (bind-key "C-g h"       window-focus-left)
   (bind-key "C-g l"       window-focus-right)
   (bind-key "C-g j"       window-focus-lower)
   (bind-key "C-g k"       window-focus-upper)
   (bind-key "C-g <Enter>" window-set-master)

   (bind-key "C-c C-r" buffer-run)

   (bind-key "C-g t 1" tab-switch-1)
   (bind-key "C-g t 2" tab-switch-2)
   (bind-key "C-g t 3" tab-switch-3)
   (bind-key "C-g t 4" tab-switch-4)
   (bind-key "C-g t 5" tab-switch-5)
   (bind-key "C-g t 6" tab-switch-6)
   (bind-key "C-g t 7" tab-switch-7)
   (bind-key "C-g t 8" tab-switch-8)
   (bind-key "C-g t 9" tab-switch-9)
   (bind-key "C-g t r" tab-rename)

   (bind-key "C-g w i"   layout-n-master+)
   (bind-key "C-g w d"   layout-n-master-)
   (bind-key "C-g w h"   layout-%-master-)
   (bind-key "C-g w l"   layout-%-master+)
   (bind-key "C-g C-s" layout-toggle-sticky)
   (bind-key "C-g w f"   layout-set-tiled)
   (bind-key "C-g w g"   layout-set-grid)
   (bind-key "C-g w b"   layout-set-bstack)
   (bind-key "C-g m"   layout-toggle-maximized)
   (bind-key "C-g n"   new-text-buffer)
   (bind-key "C-g o"   open-file-prompt)

   (bind-key "C-x b S" minibuf-switch-buffer-all)
   (bind-key "C-x b s" minibuf-switch-buffer-in-frame)
   (bind-key "C-x b c" window-close)
   (bind-key "C-x b O" minibuf-open-buffer-all)
   (bind-key "C-x b o" minibuf-open-buffer-in-frame)

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

   (bind-key "C-g q q" do-quit))
