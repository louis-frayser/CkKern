#lang racket
(require
  ;;; NOTE: string-trim,string-prefix?  from racket differs from srfi/13
  (only-in srfi/13 string-drop string-drop-right string-prefix?
           string-suffix? string-contains))

(require "params.rkt" "io.rkt")

;; ----------------------------------------------------
(define (caddddr xs) (car (cddddr xs)))
(define (basename str)
  (last (string-split str "/")))

(define (incomplete pname)
  (error (format "debug: ~a is not yet completely implemented!" pname)))
      
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
                        (displayln (string-append "FAIL: " (car pr)) stderr)
                        (displayln (string-append "  ok: " (car pr)))))
         checks))
  #t)
                       

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
    (drop-prefix %kprefix% generic)))

;;; Verify critical modules are installed for all kerrnels in /boot
(define (verify-modules)
  (displayln "--\nVerifying critical modules for main kernels in /boot...")
  (void (map (lambda(img)
               (let ((ret (critical-modules-exist? (mod-version img))))
                 (display (if ret "  ok: " "fail: "))
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
  (define (required-moddirs)
    (remove-duplicates
     (map (lambda(s)(string-trim (string-trim s (string-append "/boot/" %kprefix%)) ".old") )
          (get-bootable-images))))

  (define (moddirs)
    (map path->string (directory-list "/lib/modules")))
  (let* ((xtras (set-subtract (moddirs) (required-moddirs)))
         (err? (pair? xtras))
         (mesg (if err? 
                   "w - Extra entries in /lib/modules:"
                   "i - Ok: No extra directories in /lib/modules")))
    (void
     (newline stderr)
     (displayln mesg stderr)
     (when err?
       (map (lambda(d)(println d stderr)) xtras)))))

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
  (define (img->ksrc-ver pth)
    (let*((img (basename pth))
          (kname (drop-suffix ".old" img))
          (ks (string-split kname "-" ))
          ( kver (cadddr ks))
          (pkg (caddddr ks))
          (pv (format "~a-sources-~a" pkg kver)))
      (displayln (format "debug: checking: ~a" pv)) ; DEBUG
      pv))

  ;; (define x #false) ; DEBUG
  (define (haram? kimage)
    #|
    (when (not x)  ; DEBUG
      (set! x (blessed-kpkg-versions))
      (displayln (format "blessed-kpkg-versions: ~a" x)))
    |#
  (not (member (img->ksrc-ver kimage)
               (blessed-kpkg-versions))))
(define images (get-bootable-images))
(define halal (filter haram? images))

;(displayln (format "halal: ~a" halal) stderr)

(define (display-halal)
  (map (lambda(x)(displayln x stderr)) halal)
  (void))

(let ((q (length images))
      (hq (length halal)))
  (displayln (if (empty? halal)
                 (format "i - all ~a `/boot' kernel images are blessed")
                 (format "w - the following ~a of ~a images are not blessed:" hq q))
             stderr))
(when (pair? halal)
  (display-halal)))

(define (main)
  (check-current-kernel)
  (verify-modules)
  (check-disk-space)
  (check-for-extra-modules)
  ;;(check-not-blessed)
  )

;;; -------------------------------------------------------------
;;; Run or test?
(define %debug #t)

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
;;; Check for extraneous (unblessed) kernels using space in /boot
;;; Verify that the currently running kernel is the latest.
;;; 1) works only for gentoo-sources at the moment
;;;
;;; Verify that the latest blessed kernel is installed in /usr/src
;;; Verify all local sources in /usr/src/linux-* are compiled to /boot
;;; Verify all kernels in /boot are configured in GRUB
;;; Check for unblessed sources in /usr/src/linux-*
;;; Maybe run lxc-config .. or duplicate it
;;; Verify that linux-headers match kernel version
;;; TODO ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
|#
