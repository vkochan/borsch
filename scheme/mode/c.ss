(define c-mode-compile-buffer
   (lambda ()
      (term (format "(gcc -c ~a) || read" (buffer-filename)))
   )
)

(define c-mode-compile-and-run-buffer
   (lambda ()
      (let (
            [prog (path-root (buffer-filename))]
            [file (buffer-filename)]
           )
         (term (format "(gcc ~a -o ~a && ~a) ; read" file prog prog))
      )
   )
)

(define-mode c-mode "C" text-mode
   (bind-key-local "C-c C-c" c-mode-compile-buffer)
   (bind-key-local "C-c C-r" c-mode-compile-and-run-buffer)
)

(add-to-list 'file-ext-mode '("h" . c-mode))
(add-to-list 'file-ext-mode '("c" . c-mode))
