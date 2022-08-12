(define __cs_os_environment_get (foreign-procedure "cs_os_environment_get" () scheme-object))

(define os-environment (call-foreign (__cs_os_environment_get)))
