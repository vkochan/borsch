(define ssh-address-regex "^(\\w+\\@)?((?:\\w|\\.|\\-)+)(?:\\:([0-9]+)?(\\/\\w*)?)?$")

(define-record-type ssh-address
   (fields
      user
      host
      port
      path))

(define (ssh-parse-address addr)
   (let ([m (pregexp-match ssh-address-regex addr)])
      (if m
         (let ([user (list-ref m 1)]
               [host (list-ref m 2)]
               [port (list-ref m 3)]
               [path (list-ref m 4)])
            (make-ssh-address
               (if user
                  (substring user 0 (- (string-length user) 1))
                  ;; else
                  (getenv "USER"))
               host
               (string->number (or port "22"))
               path))   
         ;; else
         #f)))
