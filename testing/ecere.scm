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
   (version "53ec01de1c42cf342a35dc125a4fef01ffb5fced")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/ecere/ecere-sdk.git")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256 (base32 "1sm9c656a2zdls7whc63p6h68fwlrkpm44bnj3k4q50ln2vfchaa"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:make-flags
      (list (string-append "prefix=" %output)
            (string-append "LIBDIR=" %output "/lib")
            (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib")
            "CC=gcc"
            "ENABLE_SSL=y"
            (string-append "CFLAGS=-I"
                           (assoc-ref %build-inputs "freetype")
                           "/include/freetype2")
            )
      #:phases
      (modify-phases %standard-phases
        (delete 'strip)
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (invoke "make"
                      "V=1"
                      "install"
                      "prefix="
                      (string-append "DESTDIR=" out))))))))
   (inputs
    (list alsa-lib curl libffi fontconfig freetype giflib libjpeg-turbo
          ncurses libpng-1.2 sqlite libressl libx11 libxext libxrender
          mesa ucl upx zlib))
   (synopsis "Software development kit with GUI, 2D/3D graphics, networking, and
an IDE")
   (description "Ecere SDK provides an API for building apps targting desktop,
mobile and web platforms.  It includes an IDE and a compiler for the eC
programming language.")
   (home-page "https://ecere.org/")
   (license license:bsd-3)))

ecere-sdk
