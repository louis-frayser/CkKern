#lang racket

(provide remove-prefixes drop-suffix)

;(require racket/string) ; string-prefix?; string-suffix? (unlike srfi/13)
(require (only-in srfi/13 string-drop string-drop-right string-prefix?
                  string-suffix? string-contains))

(define (remove-prefixes pfxs str)
  (if (null? pfxs)
      str
      (remove-prefixes (cdr pfxs) (let ((pfx (car pfxs)))
                                    (if (string-prefix? pfx str )
                                        (substring str (string-length pfx))
                                        str))) ))

(define (drop-suffix sfx s)
  (if (string-suffix?  sfx s) ; racket vs srfi/13
      (substring s 0 (- (string-length s) (string-length sfx)))
      s))

