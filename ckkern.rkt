#lang racket
(require srfi/13) ;; string-suffix?, string-contains
(require "kernel-io.rkt")
(require "check-modules.rkt")
(require "kernsrc.rkt")
;; ----------------------------------------------------
(define (basename str)
  (last (string-split str "/")))
;; ======================================================
;;; Very current kernel is installed correctly and has sources
;; 1. verify /boot/image /lib/modules for running kernel
;; 2. verify /lib/modules/kversion has all modules for current kern
;; 3. verify /usr/src/current-kernel-version/Makefile
(define (check-current-kernel)
  (define checks
    `(
      ("Check for boot image"         ,boot-image-exists?)
      ("Check for critical modules"   ,critical-modules-exist?)
      ("Check for kernel sources"     ,sources-exist?)
      ("Check for grub configuration" ,grub-configured-for? )))

  (let ((kver (get-kernel-version)))
    (displayln (string-append "Running kernel: " kver "..."))
    (map (lambda(pr)(if (not ((cadr pr) kver) )
                        (displayln (string-append "FAIL: " (car pr)))
                        (displayln (string-append "  ok: " (car pr)))))
         checks))
  #t)
                       
(check-current-kernel)
;; .................................................................
(define (drop-suffix sfx str)
  (if (string-suffix? sfx str)
      (string-drop-right str (string-length sfx))
      str))
(define (drop-prefix pfx str)
  (if (string-prefix? pfx str)
      (string-drop str (string-length pfx))
      str))
(define (mod-version img-path)
  (let*((basename (last (string-split img-path "/")))
        (generic (drop-suffix ".old" basename)))
    (drop-prefix "kernel-genkernel-x86_64-" generic)))

;;; Verify critical modules are installed for all kerrnels in /boot
(displayln "Verifying critical modules for main kernels in /boot")
(map (lambda(img)
       (let ((ret (critical-modules-exist? (mod-version img))))
         (display (if ret "  ok: " "fail: "))
         (displayln (basename img))
         ret))
     (get-bootable-images))
#|
TODO...
;;; Verify that the latest blessed kernel is installed in /usr/src
;;; Verify all local souces in /usr/src/linux-* are compiled to /boot
;;; Verify all kernels in /boot are configured in GRUB
;;; Check for extraneous (unblessed) kernels using space in /boot
;;; Check for unblessed sources in /usr/src/linux-*
;;; Check for extraneouse modules in /lib/modules (no matching kern)
;;; Maybe run lxc-config .. or duplicate it
;;; TODO ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
|#
#|

(map check-modules-for (get-kernel-images-paths))
(define %kver (get-kernel-version))
(boot-image-exists? %kver)
 |#
