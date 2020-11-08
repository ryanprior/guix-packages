;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (contributed vala-language-server)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages graphviz)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) :prefix license:))


(define-public vala-0.50
  (package
    (inherit vala)
    (version "0.50.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/vala/"
                                  (version-major+minor version) "/"
                                  "vala-" version ".tar.xz"))
              (sha256
               (base32
                "0v4g2gvn7x7cl33h8sj1y2xyyskw5ayaj4jm2jrd3my3r439z3cm"))))))

(define-public vala-language-server
  (package
    (name "vala-language-server")
    ;; Note to maintainer: VLS must be built with a Vala toolchain the same
    ;; version or newer. Therefore when you update this package you may need
    ;; to update Vala too.
    (version "0.48.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/benwaffle/vala-language-server")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "12k095052jkvbiyz8gzkj6w7r7p16d5m18fyikl48yvh5nln8fw0"))))
    (build-system meson-build-system)
    (arguments '(#:glib-or-gtk? #t))
    (inputs
     `(("glib" ,glib)
       ("json-glib" ,json-glib)
       ("jsonrpc-glib" ,jsonrpc-glib)
       ("libgee" ,libgee)
       ("vala" ,vala-0.50)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/benwaffle/vala-language-server")
    (synopsis "Language server for Vala")
    (description "The Vala language server is an implementation of the Vala
language specification for the Language Server Protocol (LSP).  This tool is
used in text editing environments to provide a complete and integrated
feature-set for programming Vala effectively.")
    (license license:lgpl2.1+)))

