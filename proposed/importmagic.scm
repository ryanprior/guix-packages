;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (proposed importmagic)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix build-system python)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public emacs-importmagic
  (package
    (name "emacs-importmagic")
    (version "20180520.303")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://melpa.org/packages/importmagic-" version ".tar"))
       (sha256
        (base32 "0xk4i4x4836ksv2pr3aarpbkq6b5sz8c3y6f39fwf698v8zirhs9"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-epc" ,emacs-epc)
       ("emacs-f" ,emacs-f)
       ("python-epc" ,python-epc)
       ("python-importmagic" ,python-importmagic)))
    (home-page "https://github.com/anachronic/importmagic.el")
    (synopsis "Fix Python imports")
    (description "Importmagic.el fixes unresolved imports in Python buffers.")
    (license license:gpl3)))
