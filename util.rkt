#lang racket

(provide basename caddddr drop-suffix extract-key incomplete
         modname-string=? remove-prefixes)

(require "params.rkt")
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


;; ----------------------------------------------------
(define (caddddr xs) (car (cddddr xs)))
(define (basename str)
  (last (string-split str "/")))

(define (incomplete pname)
  (error (format "debug: ~a is not yet completely implemented!" pname)))

;;; --------------------- name strings --------------------------------
(define (modname-string=? s t)
  ;; compare basnames, ignoring comprssion suffixes
  (define splits (map (lambda(r)(string-split r ".")) (list s t)))
  ;(displayln `(splits , splits))
  (and
   (apply string=? (map car splits))
   #;(apply string=? (map cadr splits)))) ; comparing (("foo") ("foo" "ko"))
;;; ...................................................................

;;; Produce a sort key from a kernel filename
(define (extract-key s)  ; kernel-filename->module-dirname as sort-key
  ;; Remove prefixes, then return normalized_kver[-revision?]-localversion 
  (let* ((rhs (remove-prefixes %kprefixes% s))
         (lst (string-split rhs "-"))
         (v (map string->number (string-split (first lst ) ".")))
         (rest (string-join (cdr lst)))
         (v1 (string-join
              (map (lambda (n)
                     (~a #:width 4 #:left-pad-string "0" #:align 'right n)) v )
              ".")))
    (string-join (list v1 rest) "-")))