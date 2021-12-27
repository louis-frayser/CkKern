#lang racket

#| Unit tests
Tests consolodaded here from individual modules
|#

(require "io.rkt" "util.rkt")

;;; Util...
;;(drop-suffix "xyz"  "foo-xyz")
;;(remove-prefixes '( "abc" "def" )  "abcdef-xxfooo")

#|

(map check-modules-for (get-kernel-images-paths))
(define %kver (get-kernel-version))
(boot-image-exists? %kver)
 |#

#| Check sources ...
(define (run-test)
  (define %kver (get-kernel-version))
  (define kver "4.19.86-gentoo-lf00")
  (sources-exist? kver)
  (directory-list "/usr/src"))
|#

#| /boot

;;; ---------------------------------------------------
(define (run-tests)
  (displayln (cons 'bootImage: (boot-image-exists?)))
  (displayln (test0))
  (writeln (get-kernel-version))
  )
|#
;;; This test requires a directory that is missing modules.
;;; It's purpose is to determine if the missing modules are
;;; detected.  It can also be run with an intact directory (for comparison)
(define (run-test)
  (map critical-modules-exist?
       '( "4.19.82-gentoo-lf00"
          "4.19.86-gentoo-lf00")))

(run-test)