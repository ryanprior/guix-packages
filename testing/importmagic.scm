;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (gnu packages importmagic)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix build-system python)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public python-sexpdata
  (package
   (name "python-sexpdata")
   (version "0.0.3")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "sexpdata" version))
     (sha256
      (base32
       "1q4lsjyzzqrdv64l0pv4ij9nd8gqhvxqcrpxc2xpxs652sk2gj0s"))))
   (build-system python-build-system)
   (home-page "https://github.com/tkf/sexpdata")
   (synopsis "S-expression parser for Python")
   (description
    "sexpdata is a simple S-expression parser/serializer. It has simple load and
dump functions like pickle, json or PyYAML module.")
   (license license:bsd-3)))

(define-public python-importmagic
  (package
    (name "python-importmagic")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "importmagic" version))
       (sha256
        (base32
         "1n7qxa1snj06aw45mcfz7bxc46zp7fxj687140g2k6jcnyjmfxrz"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "http://github.com/alecthomas/importmagic")
    (synopsis "Library for adding, removing and managing Python imports")
    (description
     "Importmagic is a Python library for automatically managing imports by
finding unresolved symbols in Python code and their corresponding imports.")
    (license license:bsd-3)))

python-importmagic
