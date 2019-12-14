#lang racket
(provide critical-modules-exist?)
(define stderr (current-error-port))

(define-syntax-rule (inc! x) (begin (set! x (+  x 1)) x))

(define seq #f) ; Counter check for too many files processed
(define %modules '("loop" "zfs" "spl" "vboxpci" "vboxnetadp"
                          "vboxnetflt" "vboxdrv" ))
(define %modnames (map (lambda(s)(string-append s ".ko")) %modules))

#|
;;; Find the critical modules in the specified path
(map (lambda(p)(path->string (file-name-from-path p))) 
         (modules-found modpath)))
  |#

;;; Determine if al the critcal modules exest.
;;; 1. find the modules in /lib/modules/kver that are critical
;;; 2. Determine if the count is the same as the number of criticals
(define (required-module? p)
  (and (>  (inc! seq ) (* 256 (length %modules))) (error "too much!"))
  (member (last (string-split (path->string p) "/")) %modnames
          string=?)) 


(define (critical-modules-exist? kver)
  (define (modules-found modpath)
    (find-files required-module? modpath #:follow-links? #f))
  (define (modules-ok? modpath)
    (display (modules-found modpath))
    (= (length (modules-found  modpath)) (length %modules)))

  (set! seq 0)
  (let ((modpath (string-append "/lib/modules/" kver)))  
    (if (not (modules-ok? modpath))
        (begin (displayln (string-append "e (" modpath "): missing some critical modules") stderr) #f)
        #t)))


  