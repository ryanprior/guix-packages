;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ryan Prior <rprior@protonmail.com>

(define-module (testing comby)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system dune)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public comby
  (package
   (name "comby")
   (version "1.0.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/comby-tools/comby")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256 (base32 "1s43ri9hmxqys34gyacxfzr5s11j0bs0p8lvwapdq9dqzcb7lmi3"))))
   (build-system dune-build-system)
   (inputs
    `(("pcre" ,pcre)
      ("ocaml4.07-ppx-sexp-conv" ,ocaml4.07-ppx-sexp-conv)
      ("ocaml-yojson" ,ocaml-yojson)
      ("ocaml4.07-ppx-sexp-message" ,ocaml4.07-ppx-sexp-message)
      ("ocaml-bisect-ppx" ,ocaml-bisect-ppx)
      ("ocaml4.07-ppx-here" ,ocaml4.07-ppx-here)
      ("pkg-config" ,pkg-config)))
   (synopsis "Tool for structural code search and replace that supports many languages.")
   (description "Comby is a tool for changing the structure of code, with a
rule-based engine that understands the structure of many programming
languages.")
   (home-page "https://github.com/comby-tools/comby")
   (license license:asl2.0)))
