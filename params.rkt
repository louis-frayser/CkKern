#lang racket
;;;; Configuration and runtime parameters
;; FIXME: %kpefix%: calculate the infix "-ghost-" from $KNAME in /etc/genkernel.conf

(provide %indent% %kname%  %kdbdir% %kprefixes% %modules% %modnames%   %supported-kpkg-prefixes%
         stderr
 )
(define stderr (current-error-port))

;;; Recognizing kernel names
(define %kname% "ghost")
(define kern-prefix (string-append "kernel-" %kname% "-x86_64-"))
(define vm-prefix "vmlinuz-" )
(define %kprefixes% (list vm-prefix kern-prefix))


(define %modules% '("loop" "zfs" "spl"  "vboxnetadp"
                            "vboxnetflt" "vboxdrv" ))
(define %modnames% (map (lambda(s)(string-append s ".ko")) %modules%))

;;; Locating kernel package metadata
(define %kdbdir% "/var/db/repos/gentoo/metadata/md5-cache/sys-kernel")
(define %supported-kpkg-prefixes% '("gentoo-sources" "rt-sources"))



;;; Formattion output
(define %indent% "  ")