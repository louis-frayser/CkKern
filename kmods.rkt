#lang racket
;(provide critical-modules-exists?)
(require "params.rkt" "util.rkt" "io.rkt")
(require (only-in srfi/13 string< string-drop string-drop-right string-prefix?
           string-suffix? string-contains))
;; ------ Modules ----------------------------------------

(provide critical-modules-exist? mod-version verify-modules)

;;; Return modules version based on a kernel filename
(define (mod-version img-path)
  (let*((basename (last (string-split img-path "/")))
        (generic (string-trim basename #:right? #t ".old")))
    (remove-prefixes %kprefixes% generic)))

(define (critical-modules-exist? kver)
  ;;; Determine if all the critcal modules exist.
  ;;; 1. find the modules in /lib/modules/kver that are critical
  ;;; 2. Determine if the count is the same as the number of criticals
  
  (define seq #f) ; Counter check for too many files processed
  (define-syntax-rule (inc! x) (begin (set! x (+  x 1)) x))

  (define (required-module? p)
    (and (>  (inc! seq ) (* 256 (length %modules%))) (error "too much!"))
    (member (last (string-split (path->string p) "/")) %modnames%
            string=?)) 

  (define (modules-found modpath)
    (set! seq 0)
    (find-files required-module? modpath #:follow-links? #f))


  ;; Verify all critical modules exists in given modpath
  (define (simple-module-name m)
    (car (string-split (path->string (file-name-from-path m)) ".")))
  
  (define (modules-ok? modpath)
    (cond ((empty? %modules%) #t)
          ((directory-exists? modpath) 
           (let ((mfound (modules-found modpath)))
             (cond      
               ((= (length mfound) (length %modules%))
                #t)
               (else
                (let* ( (found  (sort (map simple-module-name mfound) string< ))
                        (expected (sort  %modules% string< ))
                        (missing (set-subtract expected found)))
                  (display (format "e - missing ~a" missing) stderr)         
                  #f)))))
          (else 
           (display
            (format "e - (~s): modules directory missing"
                    modpath) stderr) #f)))

  (let ((modpath (string-append "/lib/modules/" kver)))  
    (cond ((modules-ok? modpath)  #t)
          ( else (displayln (string-append " in (" modpath ").") stderr) #f)
          )))

;;; Verify critical modules are installed for all kerrnels in /boot
(define (verify-modules)
  (displayln "--\nVerifying critical modules for kernels in /boot...")
  (void (map (lambda(img)
               (let ((ret (critical-modules-exist? (mod-version img))))
                 (display (string-append %indent% (if ret "ok: " "fail: ")))
                 (displayln (basename img))
                 ret))
             (get-bootable-images)))) 
