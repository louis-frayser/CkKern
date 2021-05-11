#lang racket
;;;; Configuration and runtime parameters
;; FIXME: %kpefix%: calculate the infix "-ghost-" from $KNAME in /etc/genkernel.conf

(provide %kname%  %kdbdir% %kprefix% %modules% %modnames%   %supported-kpkg-prefixes%
         ;;  %kpkgdb%
 )
(define %kname% "ghost")
(define %kprefix% (string-append "kernel-" %kname% "-x86_64-"))
(define %modules% '("loop" "zfs" "spl"  "vboxnetadp"
                            "vboxnetflt" "vboxdrv" ))
(define %modnames% (map (lambda(s)(string-append s ".ko")) %modules%))

;;; Locating kernel package metadata
(define %kdbdir% "/var/db/repos/gentoo/metadata/md5-cache/sys-kernel")
(define %supported-kpkg-prefixes% '("gentoo-sources" "rt-sources"))


