(define c-compile-options
   (lambda ()
      (get-local c-compile-options "")
   )
)

(define c-compile-buffer
   (case-lambda
      [()
       (c-compile-buffer "-c" #f)
      ]

      [(ext-opts fn)
       (let ([buf-out (buffer-new)] [buf-err (buffer-new)] [fn fn])
          (process-create
             (format "gcc ~a ~a ~a" (c-compile-options) (buffer-filename) ext-opts)
             buf-out
             buf-err
             (lambda (status out err)
                (if (eq? status 0)
                   (begin
                      (message "Compilation is successful")
                      (buffer-delete out)
                      (buffer-delete err)
                   )
                   ;; else
                   (begin
                      (buffer-delete out)
                      (window-create err)
                      (text-mode)
                      (message "Compilation failed")
                   )
                )
                (when fn
                   (fn status out err)
                )
             )
          )
       )
      ]
   )
)

(define c-compile-and-run-buffer
   (lambda ()
      (let (
            [prog (path-root (buffer-filename))]
            [file (buffer-filename)]
           )
         (c-compile-buffer
            (format "-o ~a" prog)
            (lambda (status out err)
               (when (eq? status 0)
                  (vterm (format "~a ; read" prog))
               )
            )
         )
      )
   )
)

(define (c-compile-and-eval-buffer)
   (define (wrap-expr expr)
      (string-append
         "
         #include <stdio.h>

         #define print_val(X) _Generic((X), \\
            double: print_val_double, \\
            default: print_val_void,  \\
            float: print_val_float,  \\
            char*: print_val_cptr, \\
            int: print_val_int, \\
            unsigned int: print_val_uint, \\
            long: print_val_long, \\
            unsigned long: print_val_ulong, \\
            char: print_val_char, \\
            unsigned char: print_val_uchar \\
         )(X)

         void print_val_void(void *val) { printf(\"(void)\\n\"); }
         void print_val_double(double val) { printf(\"%lf\\n\", val); }
         void print_val_float(float val) { printf(\"%f\\n\", val); }
         void print_val_uint(unsigned int val) { printf(\"%u (0x%x)\\n\", val, val); }
         void print_val_int(int val) { printf(\"%d (0x%x)\\n\", val, val); }
         void print_val_ulong(unsigned long val) { printf(\"%lu (0x%lx)\\n\", val, val); }
         void print_val_long(long val) { printf(\"%ld (0x%lx)\\n\", val, val); }
         void print_val_char(char val) { printf(\"%d (0x%x)\\n\", val, val); }
         void print_val_uchar(unsigned char val) { printf(\"%u (0x%x)\\n\", val, val); }
         void print_val_cptr(char *val) { printf(\"[\\\"%s\\\"]\\n\", val); }

         void main(void)
         {
             print_val((
             {
             /* c-mode-start-of-eval-expr */
             "
"            " expr
             ";
             }
             ));
         }")
   )

   (define (on-eval-exit status buf-out buf-err)
      (with-current-buffer buf-out
         (text-append "<<<<<<<<<<<<<<<<<<<<\n\n"))
      (buffer-open buf-out))

   (let* ([prog (format "/tmp/borsch-c-eval-~a" (random 65000))]
          [cmd (format "gcc -x c -o ~a - && ~a" prog prog)]
          [buf (or (frame-get-buffer "c-eval-output") (buffer-new "c-eval-output"))])
      (with-current-buffer buf
         (text-append ">>>>>>>>>>>>>>>>>>>>\n"))
      (let* ([p (process-create cmd buf (lambda (status out err)
                                           (delete-file prog)
                                           (on-eval-exit status out err)))]
             [port-in (process-port-in p)])
         (put-string port-in (wrap-expr (text-string)))
         (close-port port-in))
      ))

(define (c-assembler-output)
   (define (on-gcc-exit status buf-out buf-err)
      (with-current-buffer buf-out
         (text-mode))
      (window-create buf-out))

   (let ([cmd "gcc -x c -masm=intel -fverbose-asm -O0 -S -o- -"]
         [buf (buffer-new)])
      (let* ([p (process-create cmd buf on-gcc-exit)]
             [port-in (process-port-in p)])
         (put-string port-in (text-string))
         (close-port port-in))))

(define-mode c-mode "C" text-mode
   (bind-key-local "C-c C-c" c-compile-buffer)
   (bind-key-local "C-c C-r" c-compile-and-run-buffer)
   (bind-key-local "C-c C-e" c-compile-and-eval-buffer)
   (bind-key-local "C-c C-a" c-assembler-output)
   (syntax-set-lang 'c)
)

(add-to-list 'file-match-mode '(".*\\.h$" . c-mode))
(add-to-list 'file-match-mode '(".*\\.c$" . c-mode))

(define c-syntax-keywords-match
   "[
    \"break\"
    \"case\"
    \"const\"
    \"continue\"
    \"default\"
    \"do\"
    \"else\"
    \"enum\"
    \"extern\"
    \"for\"
    \"if\"
    \"inline\"
    \"return\"
    \"sizeof\"
    \"static\"
    \"struct\"
    \"switch\"
    \"typedef\"
    \"union\"
    \"volatile\"
    \"while\"
    \"#define\"
    \"#elif\"
    \"#else\"
    \"#endif\"
    \"#if\"
    \"#ifdef\"
    \"#ifndef\"
    \"#include\"
     (preproc_directive)
    ] @keyword"
)

(define c-syntax-preproc-match
   "[
    \"#define\"
    \"#elif\"
    \"#else\"
    \"#endif\"
    \"#if\"
    \"#ifdef\"
    \"#ifndef\"
    \"#include\"
     (preproc_directive)
    ] @preproc"
)

(define c-syntax-type-id-match         "(type_identifier) @type")
(define c-syntax-type-primitive-match  "(primitive_type) @type")
(define c-syntax-type-sized-spec-match "(sized_type_specifier) @type")

(define c-syntax-string-match "(string_literal) @string")
(define c-syntax-string-sys-lib-match "(system_lib_string) @string")

(define c-syntax-func-match "(call_expression function: (identifier) @function)")
(define c-syntax-func-call-match "(call_expression function: (field_expression field: (field_identifier) @function))")
(define c-syntax-func-decl-match "(function_declarator declarator: (identifier) @function) (preproc_function_def name: (identifier) @function.special)")

(define c-syntax-operator-match
   "[
    \"--\"
    \"-\"
    \"-=\"
    \"->\"
    \"=\"
    \"!=\"
    \"|=\"
    \"&=\"
    \"*\"
    \"&\"
    \"~\"
    \"|\"
    \"!\"
    \"&&\"
    \"+\"
    \"++\"
    \"+=\"
    \"<\"
    \"==\"
    \">\"
    \"||\"
   ] @operator"
)

(define c-syntax-constant-null-match "(null) @constant")
(define c-syntax-constant-match "((identifier) @constant (#match? @constant \"^[A-Z][A-Z\\d_]*$\"))")
(define c-syntax-number-match "(number_literal) @number")
(define c-syntax-char-match "(char_literal) @number")

(define c-syntax-property-match "(field_identifier) @property")
(define c-syntax-label-match "(statement_identifier) @label")
(define c-syntax-variable-match "(identifier) @variable")

(define c-syntax-delimiter-match "[\".\" \";\"] @delimiter")

(define c-syntax-comment-match "(comment) @comment")

(define c-syntax-keywords-style      '(fg: "bright-yellow"))
(define c-syntax-preproc-style       '(fg: "green"))
(define c-syntax-type-style          '(fg: "green"))
(define c-syntax-string-style        '(fg: "bright-yellow"))
(define c-syntax-func-style          '(fg: "cyan"))
(define c-syntax-operator-style      '(fg: "yellow" attr: "bold"))
(define c-syntax-delimiter-style     '(fg: "green"))
(define c-syntax-comment-style       '(fg: "bright-black"))
(define c-syntax-variable-style      '(fg: "cyan"))
(define c-syntax-number-style        '(fg: "blue"))
(define c-syntax-constant-style      '(fg: "blue"))
(define c-syntax-label-style         '(fg: "red"))

(syntax-set-style 'c c-syntax-constant-null-match c-syntax-constant-style)
(syntax-set-style 'c c-syntax-keywords-match c-syntax-keywords-style)
(syntax-set-style 'c c-syntax-type-id-match c-syntax-type-style)
(syntax-set-style 'c c-syntax-type-primitive-match c-syntax-type-style)
(syntax-set-style 'c c-syntax-type-sized-spec-match c-syntax-type-style)
(syntax-set-style 'c c-syntax-string-match c-syntax-string-style)
(syntax-set-style 'c c-syntax-string-sys-lib-match c-syntax-string-style)
(syntax-set-style 'c c-syntax-operator-match c-syntax-operator-style)
(syntax-set-style 'c c-syntax-delimiter-match c-syntax-delimiter-style)
(syntax-set-style 'c c-syntax-comment-match c-syntax-comment-style)
(syntax-set-style 'c c-syntax-variable-match c-syntax-variable-style)
(syntax-set-style 'c c-syntax-number-match c-syntax-number-style)
(syntax-set-style 'c c-syntax-preproc-match c-syntax-preproc-style)
;;(syntax-set-style 'c c-syntax-property-match c-syntax-variable-style)
;;(syntax-set-style 'c c-syntax-label-match c-syntax-label-style)
(syntax-set-style 'c c-syntax-func-call-match c-syntax-func-style)
(syntax-set-style 'c c-syntax-func-decl-match c-syntax-func-style)
(syntax-set-style 'c c-syntax-func-match c-syntax-func-style)
