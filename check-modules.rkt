#lang racket
(provide critical-modules-exist?)
(define stderr (current-error-port))

(define-syntax-rule (inc! x) (begin (set! x (+  x 1)) x))

(define seq #f) ; Counter check for too many files processed
(define %modules '("loop" "zfs" "spl" "vboxpci" "vboxnetadp"
                          "vboxnetflt" "vboxdrv" ))
(define %modnames (map (lambda(s)(string-append s ".ko")) %modules))


;;(define mod-dirs (directory-list "/lib/modules"))

;;; Find the critical modules in the specified path
(define (show-critical-modules-found modpath)
  (define (modules-found modpath) 
 
    (= (length (modules-found  modpath)) (length %modules)))
  (map (lambda(p)(path->string (file-name-from-path p))) 
       (modules-found modpath)))

(define (critical-modules-exist? kver)
  (define (required-module? p)
    (and (>  (inc! seq ) (* 256 (length %modules))) (error "too much!"))
    (member (last (string-split (path->string p) "/")) %modnames
            string=?)) 
  (define (modules-ok? modpath) 
    (find-files required-module? modpath #:follow-links? #f))
  (set! seq 0)
  (let ((modpath (string-append "/lib/modules/" kver)))
    (if (not (modules-ok? modpath))
        (displayln (string-append "e (" modpath "): missing some critical modules") stderr)
        #t)))

#|
(define (run-test)
  (critical-modules-exist? "4.19.82-gentoo-lf00"))

(run-test)
|#