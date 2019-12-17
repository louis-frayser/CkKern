#lang racket
;;;; IO for gather kernel information
;;;;
;; caution: Racket's string functions are diff from srfi's
(require srfi/13) ; Strings

(provide  boot-image-exists? booted-image-exists? 
          critical-modules-exist? get-bootable-images
          get-kernel-version
          grub-configured-for? sources-exist?)

(define stderr (current-error-port))
;;; ==========================================================
;;; /proc
;;; Check for current boot image unless another specified
(define (booted-image-exists? )
  (define (boot-image)
    (let ((proc-val
           (cadr
            (assoc "BOOT_IMAGE"
                   (map (lambda(s)(string-split s "="))
                        (string-split (file->string "/proc/cmdline" #:mode 'text)))
                   string=?))))
      (string-append "/boot" proc-val)))
  (file-exists? (boot-image)))
;; -------------------------------------------------------------------
;;; /boot

;; find kernel file  for kver
(define (boot-image-exists? kver)
  (file-exists?
   (string-append "/boot/kernel-genkernel-x86_64-" kver)))

(define (get-bootable-images)
  (define maybe-kernel?
    (lambda(p)
      (let ((s (path->string p)))
        (cond  ( (string=? "/boot" s) #t)
               ((string-prefix? "/boot/kernel-genkernel" s)  #t)
               (else  #f)))))
  (filter (lambda(s)(not (string=? s "/boot")))
          (map (lambda(p)(path->string p))
               (find-files maybe-kernel?
                           "/boot" #:follow-links? #f
                           #:skip-filtered-directory? #t))))

;; --------------------------------------------------------------------
;;; Process /usr/portage/suys-kernl/gentoo-sources 
(define (amd64ok? abuild)
  (define (keyword-line? l)
    (and (> (string-length l) 10) (string=? (substring l 0 10) "KEYWORDS=\"")))
  (call-with-input-file abuild
    (lambda(in)
      (do ((aline (read-line in) (read-line in)))
        ( (keyword-line? aline) 
          (and (string-contains aline "amd64" )
               (not (string-contains aline "~amd64"))))
        (when(eof-object? aline)
          (error (string-append "EOF on: " (path->string abuild))) )))))
   
(define (get-all-kernels)
  (define src "/usr/portage/sys-kernel/gentoo-sources")
  (filter (lambda(p) (string-suffix? ".ebuild" p))
          (map path->string
               (directory-list src #:build? #t))))

(define (get-blessed-kernels) (filter amd64ok?  (get-all-kernels)))

(define (test0) (let ((bs (get-blessed-kernels)))
                  (cons bs (length bs))))
 
;; --------------------------------------------------                     
;;; system()

(define (get-kernel-version)
  (string-trim-right (with-output-to-string 
                       (lambda () (system "uname -r")))))

;; --------------------------------------------------                     
;;; GRUB
(define (grub-configured-for? kver)
  (with-input-from-file "/boot/grub/grub.cfg"
    (lambda()
      (let loop ((aline (read-line)))
        (if (eof-object? aline)
            #f
            (if (string-contains? aline kver)
                #t
                (loop (read-line))))))))

;; ------ Modules ----------------------------------------

;;(provide critical-modules-exist?)
 
(define (critical-modules-exist? kver)
  ;;; Determine if all the critcal modules exist.
  ;;; 1. find the modules in /lib/modules/kver that are critical
  ;;; 2. Determine if the count is the same as the number of criticals
  (define %modules '("loop" "zfs" "spl" "vboxpci" "vboxnetadp"
                            "vboxnetflt" "vboxdrv" ))
  (define %modnames (map (lambda(s)(string-append s ".ko")) %modules))
  
  (define seq #f) ; Counter check for too many files processed
  (define-syntax-rule (inc! x) (begin (set! x (+  x 1)) x))

  (define (required-module? p)
    (and (>  (inc! seq ) (* 256 (length %modules))) (error "too much!"))
    (member (last (string-split (path->string p) "/")) %modnames
            string=?)) 

  (define (modules-found modpath)
    (set! seq 0)
    (find-files required-module? modpath #:follow-links? #f))
  (define (modules-ok? modpath)
    (= (length (modules-found  modpath)) (length %modules)))

  (let ((modpath (string-append "/lib/modules/" kver)))  
    (if (not (modules-ok? modpath))
        (begin (displayln (string-append "e (" modpath "): missing some critical modules") stderr) #f)
        #t)))
;;; --------------------------------------------------------
;;; Kernel sources /usr/src/linux--*
(define (sources-exist? kver)
  ;;; Trim the local suffix from kernel version
  ;;; 4.19.86-gentoo-lf00 => 4.19.86-gentoo
  (define (generic-kernel-version kver)
    (first (string-split kver "-" )))

  (define (ksrc-dir kver)
    ;;; Return the directory containg specific kernel sources
    (string-append "/usr/src/linux-" (generic-kernel-version kver) "-gentoo")) 
  (let (( ksrc-mak (string-append (ksrc-dir kver)"/Makefile")))
    (file-exists? ksrc-mak)))
;; --------------------------------------------------------
