;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (forbidden wrap-cc)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages c)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages sdcc)
  #:use-module (gnu packages)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define* (wrap-cc cc
                  #:optional
                  (bin (package-name cc))
                  (name (string-append (package-name cc) "-wrapper")))
  (package/inherit cc
    (name name)
    (source #f)
    (build-system trivial-build-system)
    (outputs '("out"))
    (native-inputs '())
    (inputs '())
    (propagated-inputs `(("cc" ,cc)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((bin-dir (string-append (assoc-ref %build-inputs "cc") "/bin/"))
               (wrapper-dir (string-append (assoc-ref %outputs "out") "/bin/")))
           (mkdir-p wrapper-dir)
           (symlink (string-append bin-dir ,bin)
                    (string-append wrapper-dir "cc"))))))
    (synopsis (string-append "Wrapper for " bin))
    (description
     (string-append
      "Wraps " (package-name cc) " such that @command{" bin "} can be invoked
under the name @command{cc}."))))

(define-public gcc-wrapper (wrap-cc gcc))
(define-public gcc-toolchain-wrapper (wrap-cc gcc-toolchain "gcc"))
(define-public tcc-wrapper (wrap-cc tcc))
(define-public sdcc-wrapper (wrap-cc sdcc))
(define-public pcc-wrapper (wrap-cc pcc))
(define-public bcc-wrapper (wrap-cc dev86 "bcc" "bcc-wrapper"))
