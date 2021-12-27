#lang racket
#| ckkern
Checks the installed kernels for major modules and source inconsistencies
1. Verifies sources and modules for the running kernel
2. Verifies same for the latest "blessed" kernel
3. Same for rest of the kernels.
4. Check disk space in /boot
5. Check for extraneouse /lib/modules kmod directories
6. Check for obsolete or other nonstandard kernels
7. Other checks: Seee TODO at bottom of file.

Parameters
"params.rkt" holds the configuration (may be externalized to the commandline and a config file later)
|#
(require
  ;;; NOTE: string-trim,string-prefix?  from racket differs from srfi/13
  (only-in srfi/13 string-drop string-drop-right string-prefix?
           string-suffix? string-contains))

(require "params.rkt" "io.rkt" "util.rkt" "blessed.rkt" "kmods.rkt")

;; ======================================================
;;; Very current kernel is installed correctly and has sources
;; 1. verify /boot/kernel /lib/modules for running kernel
;; 2. verify /lib/modules/$kversion has all modules for current kern
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
    (for-each (lambda(pr)
                (display (string-append (car pr) "..."))
                (if (not ((cadr pr) kver) )
                    ;(displayln (string-append "FAIL: " (car pr)) stderr)
                    ;(displayln (string-append %indent% "ok: " (car pr)))))
                    (displayln  "FAIL"  stderr)
                    (displayln  "Ok" stderr)))
              checks))
  #t)

;; .................................................................

;;; Check for extraneous modules in /modules/<kver>
(define (check-for-extra-modules)
  ;;; Using the kernels's names in /boot determine their module directorie
  (define (required-moddirs)
    (remove-duplicates (map (lambda(p)(remove-prefixes (cons "/boot/" %kprefixes%) p))
                            (get-bootable-images))))
  
  (define (moddirs)
    (map path->string (directory-list "/lib/modules")))
  ;; entry:
  (let* ((xtras (set-subtract (moddirs) (required-moddirs)))
         (err? (pair? xtras))
         (mesg (if err? 
                   "w - Extra entries in /lib/modules:"
                   "i - Ok: No extra directories in /lib/modules")))
    (void
     (newline stderr)
     (displayln mesg stderr)
     (when err?
       (map (lambda(d)(fprintf stderr "~a ~s~n" %indent% d)) xtras)))))

(define (main)
  (check-current-kernel)
  (verify-modules)
  (check-disk-space)
  (check-for-extra-modules)
  (check-not-blessed) 
  )

;;; -------------------------------------------------------------
;;; Run or test?
(define %debug #f)

;;; When debugging
;;; 1. invoke (main) to run the productuction version
;;; 2. invoide (%test) to run only the UUT
;;; 3. invoke (run for everything)
(define (run)
  (main)
  (%test))

(define %test check-not-blessed)
(if %debug
    (%test)
    (main))

#|
TODO...
;;; Check for extraneous (unblessed) kernels using up space in /boot
;;; Verify that the currently running kernel is the latest.
;;;   *works only for "gentoo-sources" at the moment

;;;
;;; Verify that the all blessed kernels (in /boot) are installed in /usr/src
;;;     flag the latest blessed kernel.
;;;
;;; Check for unblessed sources in /usr/src/linux-*
;;;

;;; Support white/blacklist(green/redlined) kernels

;;; Verify all local sources in /usr/src/linux-* are compiled and installed

;;; Verify all kernels in /boot are configured in GRUB;
;;;   ie grub.cfg newer than every kernel.

;;; Check for registering of the running kernel's souces in Gentoo's world file 
;;; (warn if missing.)
;;;   this ensures the emerge -depclean won't remove the kernel's sources

;;; Maybe run lxc-config .. or duplicate it (*for each kernel or src?)
;;; Verify that linux-headers match kernel version
;;; Grep the code for items for FIXME items.

;;; Change handling of rt-sources (they are never blessed)
;;; Possibly support 'gentoo-kernel, 'gentoo-kernel-bin
;;; TODO ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
|#
