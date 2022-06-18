#lang racket
(require "params.rkt" "util.rkt" "io.rkt")
(require
  ;;; NOTE: string-trim,string-prefix?  from racket differs from srfi/13
  (only-in srfi/13 string-drop string-drop-right string-prefix?
           string-suffix? string-contains))

(provide blessed-kpkg-versions check-not-blessed)
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
    (displayln
     (if (empty? harram)
         (format "i - all ~a `/boot' kernel images are blessed." q)
         (format "w - the following ~a of ~a images in /boot are not blessed:"
                 hq q))
     stderr))
  (when (pair? harram)
    (display-harram)))