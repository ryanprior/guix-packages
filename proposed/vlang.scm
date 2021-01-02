;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (proposed vlang)
  #:use-module (gnu packages c)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages node)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define markdown-origin
  (let ((markdown-version "1ccfbcba945b649b61738b9c0455d31cf99564b2"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/vlang/markdown")
            (commit markdown-version)))
      (file-name (git-file-name "vlang-markdown" markdown-version))
      (sha256
       (base32 "0s982qiwy4s9y07x9fsy4yn642schplhp9hrw2libg2bx4sw43as")))))

(define-public wyhash
  (package
    (name "wyhash")
    (version "5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wangyi-fudan/wyhash")
                    (commit (string-append "wyhash_v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "03ljs5iw9zrm3bydwggjvpwrcwmsd75h3dv1j4am4hw3h22cjdjc"))))
    (build-system trivial-build-system) ;; source-only package
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (string-append (assoc-ref %outputs "out")))
                (src (string-append out "/include"))
                (doc (string-append out "/share/doc/" ,name "-" ,version)))
           (mkdir-p src)
           (mkdir-p doc)
           (chdir (assoc-ref %build-inputs "source"))
           (install-file "wyhash.h" src)
           (install-file "LICENSE" doc)
           (install-file "README.md" doc))
         #t)))
    (home-page "https://github.com/wangyi-fudan/wyhash")
    (synopsis "Embeddable hash function and random number generator.")
    (description "This package provides a portable hash function and random
number generator suitable for use in data structures.  Provided by default in
Zig, V, and Nim programming language standard libraries.")
    (license license:unlicense)))
