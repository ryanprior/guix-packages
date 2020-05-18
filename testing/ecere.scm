;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (testing ecere)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages fontutils)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public ecere-sdk
  (package
   (name "ecere-sdk")
   (version "0.44.15")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/ecere/ecere-sdk.git")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256 (base32 "0idxqihy17pxkxr9mkfi9lm4jkid64gxn1n87fyfcdv6y6vfsdq2"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:validate-runpath? #f
      #:make-flags
      (list "prefix="
            (string-append "DESTDIR=" %output)
            (string-append "DESTLIBDIR=" %output "/lib")
            "CC=gcc"
            "ENABLE_SSL=y"
            (string-append "CFLAGS=-I"
                           (assoc-ref %build-inputs "freetype")
                           "/include/freetype2"))
      #:phases
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (invoke "make"
                      "V=1"
                      "install"
                      (string-append "DESTDIR=" out)
                      (string-append "DESTLIBDIR=" out "/lib")
                      "prefix=")))))))
   (inputs
    `(
       ("alsa-lib" ,alsa-lib)
       ("curl" ,curl)
       ("libffi" ,libffi)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg-turbo)
       ("ncurses" ,ncurses)
       ("libpng" ,libpng-1.2)
       ("sqlite" ,sqlite)
       ("libressl" ,libressl)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxrender" ,libxrender)
       ("mesa" ,mesa)
       ("ucl" ,ucl)
       ("upx" ,upx)
       ("zlib" ,zlib)
       ))
   (synopsis "Software development kit with GUI, 2D/3D graphics, networking, and
an IDE")
   (description "Ecere SDK provides an API for building apps targting desktop,
mobile and web platforms.  It includes an IDE and a compiler for the eC
programming language.")
   (home-page "https://ecere.org/")
   (license license:bsd-3)))

ecere-sdk
