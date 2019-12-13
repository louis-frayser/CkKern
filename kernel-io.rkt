#lang racket
;;;; IO for gather kernel information
;;;;
;; caution: Racket's string functions are diff from srfi's
(require srfi/13) ; Strings

(provide get-bootable-images get-kernel-version
         boot-image-exists? booted-image-exists? grub-configured-for?)
;; --------------------------------------------------------------------
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

;; -----------------------------------------------------
(define (run-tests)
  (displayln (cons 'bootImage: (boot-image-exists?)))
  (displayln (test0))
  (writeln (get-kernel-version))
  )