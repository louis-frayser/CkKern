#lang racket
(require "kernel-io.rkt")
(provide sources-exist?)

;;; Trim the local suffix from kernel version
;;; 4.19.86-gentoo-lf00 => 4.19.86-gentoo
(define (generic-kernel-version kver)
  (first (string-split kver "-" )))

(define (ksrc-dir kver)
  (string-append "/usr/src/linux-" (generic-kernel-version kver) "-gentoo")) 

(define (sources-exist? kver)
  (let (( ksrc-mak (string-append (ksrc-dir kver)"/Makefile")))
    (file-exists? ksrc-mak)))
;; --------------------------------------------------------
(define (run-test)
  (define %kver (get-kernel-version))
  (define kver "4.19.86-gentoo-lf00")
  (sources-exist? kver)
  (directory-list "/usr/src"))
