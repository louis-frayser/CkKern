#lang racket
(define-syntax-rule (inc! x) (begin (set! x (+  x 1)) x))

(define seq 0)
(define %modules '("loop" "zfs" "spl" "vboxpci" "vboxnetadp"
                          "vboxnetflt" "vboxdrv" ))
(define %modnames (map (lambda(s)(string-append s ".ko")) %modules))

(define (required-module? p)
 ; (displayln p)
  (and (>  (inc! seq ) 512) (error "too much!"))
  (member  (last (string-split (path->string p) "/")) %modnames string=?))


(define mod-dirs (directory-list "/lib/modules"))

(define test-path "/lib/modules/4.19.82-gentoo-lf00")

(define (modules-found modpath)
  (find-files required-module? modpath #:follow-links? #f))

(define (modules-ok? modpath)
  (= (length (modules-found  modpath)) (length modules)))

(define (show-critical-modules-found modpath)
  (map (lambda(p)(path->string (file-name-from-path p))) (modules-found modpath)))

(modules-ok? test-path)

#|
(if (not (= (length found) (length %modules))
         (displayln (string-append "e ("  "): missing some modules")
|#