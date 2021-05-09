#lang racket
(provide %kname% %modules% %modnames%)
(define %kname% "ghost")
(define %modules% '("loop" "zfs" "spl"  "vboxnetadp"
                            "vboxnetflt" "vboxdrv" ))
(define %modnames% (map (lambda(s)(string-append s ".ko")) %modules%))
