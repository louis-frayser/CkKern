#lang racket
;;;; IO to gather kernel information
;;;;
;; caution: Racket's string functions are diff from srfi's
(require srfi/13) ; Strings
(require "params.rkt" "util.rkt")

(provide  boot-image-exists? booted-image-exists? 
          check-disk-space df/boot-pct get-bootable-images
          get-kernel-version 
          grub-configured-for? sources-exist? stderr)

;;; ==========================================================
;;; /proc
;;; Check for current boot image unless another specified
(define (booted-image-exists? )
  (define (boot-image)
    (let ((proc-val
           (cadr
            (assoc "BOOT_IMAGE"
                   (map (lambda(s)(string-split s "="))
                        (string-split 
                         (file->string "/proc/cmdline" #:mode 'text)))
                   string=?))))
      (string-append "/boot" proc-val)))
  (file-exists? (boot-image)))
;; -------------------------------------------------------------------
;;; /boot

;; find kernel file  for kver
(define (boot-image-exists? kver)
  (define (found? pfx)
    (file-exists? (string-append "/boot/" pfx kver)))
     
  (let loop ( (pfxs %kprefixes%) )
    (let* ((done? (null? pfxs))
           ( is-found? (if done? #f (found? (car pfxs))) ))
      (cond (is-found? #t)
            (done?  #f)
            (else  (loop (cdr pfxs)))))))


(define (get-bootable-images) 
  (define maybe-kernel?
    (lambda(p)
      (let ((s (path->string p)))
        (cond  ((string-prefix? (string-append "/boot/vmlinuz-") s)  #t)
               ((string-prefix? (string-append "/boot/kernel-" %kname%) s)  #t)
               ((string=? "/boot" s) #t)
               (else  #f)))))
  (sort #:key extract-key
        (filter (lambda(s)(not (string=? s "/boot")))
                (map (lambda(p)(path->string p))
                     (find-files maybe-kernel?
                                 "/boot" #:follow-links? #f
                                 #:skip-filtered-directory? #t))) string<))
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
 
;;; ----------------------------------------------                  
;;; system()
(define-syntax-rule (shell command )
  (string-trim-right(with-output-to-string
                      (lambda()(system command)))))

(define (get-kernel-version)
  (string-trim-right (with-output-to-string 
                       (lambda () (system "uname -r")))))
;;; free space on /boot as a percentage
(define (df/boot-pct)
  (- 100 (string->number
          (shell (string-append 
                  "df /boot | { read ; read _fs _sz _us _av pct _mp;"
                  "  echo  ${pct%\\%} ; }")))))

;;; ------------------------------------------                     
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

;;; --------------------------------------------------------
;;; Kernel sources /usr/src/linux--*
(define (sources-exist? kver)
  ;;; Trim the local suffix from kernel version
  ;;; 4.19.86-gentoo-lf00 => 4.19.86-gentoo
  (define (generic-kernel-version kver)
    (first (string-split kver "-" )))

  (define (ksrc-dir kver)
    ;;; Return the directory containg specific kernel sources
    (string-append "/usr/src/linux-" (generic-kernel-version kver)
                   "-gentoo")) 
  (let (( ksrc-mak (string-append (ksrc-dir kver)"/Makefile")))
    (file-exists? ksrc-mak)))
;; --------------------------------------------------------

;;; Check for 10% space free on /boot
(define (check-disk-space)
  (displayln "--\nVerifying disk space on /boot...")
  (let ((bfree (df/boot-pct)) )
    (displayln
     (format "   I (/boot):  ~a% of disk is free!" bfree))
    (cond ( (< bfree 10)
            (displayln "   W - (/boot): space is low." stderr)
            #f)
          (else #t))))
;; --------------------------------------------------------
