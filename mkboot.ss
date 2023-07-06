(reset-handler abort)

(source-directories '("scheme"))
(library-directories '("scheme"))

(compile-imported-libraries #t)

(make-boot-file "borsch.boot" '("scheme" "petite") "scheme/main.ss")
