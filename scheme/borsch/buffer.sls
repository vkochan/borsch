(library (borsch buffer)
   (export
      current-buffer
      with-current-buffer
      buffer-line-num
      buffer-set-mode-name
      buffer-mode-name
      buffer-set-state-name
      buffer-state-name
      buffer-snapshot
      buffer-undo
      buffer-redo
      buffer-filename
      buffer-set-filename
      buffer-env
      define-local
      buffer-name
      buffer-set-name
      buffer-set-readonly
      buffer-is-readonly?
      buffer-is-modified?
      buffer-is-dirty?)
   (import (chezscheme)
           (borsch base))

(define __cs_buf_current_get (foreign-procedure "cs_buf_current_get" () scheme-object))

(define __cs_buf_line_num (foreign-procedure "cs_buf_line_num" (int int) scheme-object))
(define __cs_buf_mode_name_set (foreign-procedure "cs_buf_mode_name_set" (int string) void))
(define __cs_buf_mode_name_get (foreign-procedure "cs_buf_mode_name_get" (int) scheme-object))
(define __cs_buf_state_name_set (foreign-procedure "cs_buf_state_name_set" (int string) void))
(define __cs_buf_state_name_get (foreign-procedure "cs_buf_state_name_get" (int) scheme-object))

(define __cs_buf_snapshot (foreign-procedure "cs_buf_snapshot" (int) void))
(define __cs_buf_undo (foreign-procedure "cs_buf_undo" (int) void))
(define __cs_buf_redo (foreign-procedure "cs_buf_redo" (int) void))

(define __cs_buf_file_set (foreign-procedure "cs_buf_file_set" (int string) void))
(define __cs_buf_file_get (foreign-procedure "cs_buf_file_get" (int) scheme-object))

(define __cs_buf_env_get (foreign-procedure "cs_buf_env_get" (int) scheme-object))

(define __cs_buf_name_get (foreign-procedure "cs_buf_name_get" (int) scheme-object))
(define __cs_buf_name_set (foreign-procedure "cs_buf_name_set" (int string) void))
(define __cs_buf_readonly_set (foreign-procedure "cs_buf_readonly_set" (int boolean) void))
(define __cs_buf_readonly_get (foreign-procedure "cs_buf_readonly_get" (int) scheme-object))
(define __cs_buf_is_modified (foreign-procedure "cs_buf_is_modified" (int) scheme-object))
(define __cs_buf_is_dirty (foreign-procedure "cs_buf_is_dirty" (int) scheme-object))

(define current-buffer-tmp (make-parameter #f))

(define-syntax (with-current-buffer stx)
   (syntax-case stx ()
      ((_ buf exp ...)
       #`(parameterize ([current-buffer-tmp buf])
            exp ... ))))

(define (current-buffer)
   (or (current-buffer-tmp)
       (call-foreign (__cs_buf_current_get))))
       
(define (buffer-line-num pos)
   (call-foreign (__cs_buf_line_num (current-buffer) pos)))

(define (buffer-set-mode-name n)
   (call-foreign (__cs_buf_mode_name_set (current-buffer) (format "~a" n))))

(define buffer-mode-name
   (case-lambda
      [()
       (buffer-mode-name (current-buffer))]

      [(b)
       (call-foreign (__cs_buf_mode_name_get b))]))

(define (buffer-set-state-name n)
   (call-foreign (__cs_buf_state_name_set (current-buffer) (format "~a" n))))

(define buffer-state-name
   (case-lambda
      [()
       (buffer-state-name (current-buffer))]

      [(b)
       (call-foreign (__cs_buf_state_name_get b))]))

(define (buffer-snapshot)
   (call-foreign (__cs_buf_snapshot (current-buffer))))

(define (buffer-undo)
   (call-foreign (__cs_buf_undo (current-buffer))))

(define (buffer-redo)
   (call-foreign (__cs_buf_redo (current-buffer))))

(define buffer-filename
   (case-lambda
     [()
      (buffer-filename (current-buffer))]

     [(b)
      (call-foreign (__cs_buf_file_get b))]))

(define (buffer-set-filename f)
   (let ([first-path (path-first f)]
         [path f])
      (if (equal? first-path "~")
         (set! path
            (format "~a/~a"
               (getenv "HOME")
               (path-rest f))))
      (call-foreign (__cs_buf_file_set (current-buffer) path))))

(define (buffer-env)
   (call-foreign (__cs_buf_env_get (current-buffer))))

(define-syntax (define-local stx)
   (syntax-case stx ()
      ((_ s v)
       #`(define-top-level-value 's v (buffer-env)))))

(define buffer-name
   (case-lambda
      [()
       (call-foreign (__cs_buf_name_get (current-buffer)))]

      [(b)
       (call-foreign (__cs_buf_name_get b))]))

(define buffer-set-name
   (case-lambda
     [(n)
      (buffer-set-name (current-buffer) n)]

     [(b n)
      (call-foreign (__cs_buf_name_set b n))]))

(define buffer-set-readonly
   (case-lambda
      [(read-only?)
       (buffer-set-readonly (current-buffer) read-only?)]

      [(buf read-only?)
       (call-foreign (__cs_buf_readonly_set buf read-only?))]))

(define buffer-is-readonly?
   (case-lambda
      [()
       (buffer-is-readonly? (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_readonly_get buf))]))

(define buffer-is-modified?
   (case-lambda
      [()
       (buffer-is-modified? (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_is_modified buf))]))

(define buffer-is-dirty?
   (case-lambda
      [()
       (buffer-is-dirty? (current-buffer))]

      [(buf)
       (call-foreign (__cs_buf_is_dirty buf))]))

)
