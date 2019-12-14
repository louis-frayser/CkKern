#lang racket

#| Unit tests
Tests consolodaded here from individual modules
|#

(require "check-modules.rkt")


;;; This test requires a directory that is missing modules.
;;; It's purpose is to determine if the missing modules are
;;; detected.  It can also be run with an intact directory (for comparison)
(define (run-test)
  (map critical-modules-exist?
       '( "4.19.82-gentoo-lf00"
          "4.19.86-gentoo-lf00")))

(run-test)