#lang racket
;;;; Configuration and runtime parameters
(provide %kname% %kprefix% %modules% %modnames%)
(define %kname% "ghost")
(define %kprefix% (string-append "kernel-" %kname% "-x86_64-"))
(define %modules% '("loop" "zfs" "spl"  "vboxnetadp"
                            "vboxnetflt" "vboxdrv" ))
(define %modnames% (map (lambda(s)(string-append s ".ko")) %modules%))
