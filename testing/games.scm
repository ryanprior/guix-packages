;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (testing games)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages sdl)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public openglad
  (package
    (name "openglad")
    (version "859478dbd90896483c91bda4db89467e8b2006e7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openglad/openglad.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sjddvcvi6p8yrsixzywcv0xzskziar09d1g95hp04mm8ja7n4ri"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             ;; actually gonna have to get the path to sdl and set some flags
             ;; https://github.com/openglad/openglad/blob/master/INSTALL#L19-L29
             (invoke "premake4" "gmake"))))))
    (inputs
     `(("sdl2" ,sdl2)))
    (native-inputs
     `(("premake" ,premake4)))
    (home-page "https://www.openglad.org/")
    (synopsis "Hack-and-slash game")
    (description "Gladiator is an arcade game where you set out to destroy
your enemies and pick up treasure. Sharpen the skills of your team as you
progress through the levels, and each character class (Fighter, Mage, etc.)
has a number of special abilities which come into play as you progress.")
    (license license:gpl2)))

openglad
