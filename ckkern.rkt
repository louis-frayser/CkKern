#lang racket
#| ckkern
Checks the installed kernels for major modules and source inconsistencies
1. Verifies sources and modules for the running kernel
2. Verifies same for the latest "blessed" kernel
3. Same for rest of the kernels.
4. Check disk space in /boot
5. Check for extraneouse modules
6. Check for obsolete or other nonstandard kernels
7. Other checks: Seee TODO at bottom of file.

Parameters
"params.rkt" holds the configuration (may be externalized to the commandline and a config file later)
|#
(require
  ;;; NOTE: string-trim,string-prefix?  from racket differs from srfi/13
  (only-in srfi/13 string-drop string-drop-right string-prefix?
           string-suffix? string-contains))

(require "params.rkt" "io.rkt" "util.rkt")

;; ----------------------------------------------------
(define (caddddr xs) (car (cddddr xs)))
(define (basename str)
  (last (string-split str "/")))

(define (incomplete pname)
  (error (format "debug: ~a is not yet completely implemented!" pname)))


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
(define (mod-version img-path)
  (let*((basename (last (string-split img-path "/")))
        (generic (string-trim basename #:right? #t ".old")))
    (remove-prefixes %kprefixes% generic)))

;;; Verify critical modules are installed for all kerrnels in /boot
(define (verify-modules)
  (displayln "--\nVerifying critical modules for kernels in /boot...")
  (void (map (lambda(img)
               (let ((ret (critical-modules-exist? (mod-version img))))
                 (display (string-append %indent% (if ret "ok: " "fail: ")))
                 (displayln (basename img))
                 ret))
             (get-bootable-images)))) 

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

;;; UUT
;;; Check for obsolete or non-standard kernels in /boot
;;; not (in-database) or masked
;;; This lists kernels installable on the system
;;; Anything else is obsolete or foreign
;;; FIXME: does not present rt-sources in a normalized format
;;;        needs to be rt-sources-$kver-rt{$rev}
(define (blessed-kpkg-versions)
  
  (define (has-valid-kprefix? pkg-ver)
    ;; valid kenel source package name prefix?
    ;(displayln pkg-ver)
    (foldl (lambda(pfx acc) (if (string-prefix? pfx pkg-ver) #t acc))
           #f
           %supported-kpkg-prefixes%))
  
  (filter has-valid-kprefix?
          (map path->string (directory-list %kdbdir%))))

(define (check-not-blessed)
  (define (imgtoks->kver ks)
    ;; Extract version from tokenized kernel name
    (if ( < (length ks) 2)
        (car ks)
        (let ((r (third ks)))
          (if (string=? r "r1")
              (string-append (car ks) "-" r)
              (car ks)))))
  (define (img->ksrc-ver pth)
    (let*((img (basename pth))
          (kname (string-trim img ".old" #:right? #t))
          (kbase (remove-prefixes %kprefixes% kname))
          (ks (string-split kbase "-" ))
          ( kver (imgtoks->kver ks))
          (pkg (second ks))
          (pv (format "~a-sources-~a" pkg kver))) 
      ; (displayln `(pth: ,pth -- ksrc-ver: ,pv))
      pv)) ; NOTE: "rt" kernels are never blessed

  (define (harram? kimage) 
    (not (member (img->ksrc-ver kimage)
                 (blessed-kpkg-versions))))

  (define images (get-bootable-images))
  (define harram (filter harram? images))

  (define (display-harram)
    (map (lambda(x)(displayln (string-append %indent% x) stderr)) harram)
    (void))
  (let ((q (length images))
        (hq (length harram)))
    (newline)
    (displayln (if (empty? harram)
                   (format "i - all ~a `/boot' kernel images are blessed." q)
                   (format "w - the following ~a of ~a images are not blessed:" hq q))
               stderr))
  (when (pair? harram)
    (display-harram)))

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
