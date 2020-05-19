(define-module (contributed pantheon)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public granite
  (package
    (name "granite")
    (version "5.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/elementary/granite/archive/"
                                  version ".tar.gz"))
              (sha256 (base32 "1dash69rii1zpr2smmc89gkwnl6jj4sfpkin9r1afy20xcf8fhrh"))))
    (build-system meson-build-system)
    (inputs
     `(("gtk" ,gtk+)
       ("gtk+-bin" ,gtk+ "bin"))) ; for gtk-update-icon-cache
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("libgee" ,libgee)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/granite")
    (synopsis "Library that extends GTK with common widgets and utilities")
    (description "Granite is a companion library for GTK+ and GLib.  Among other
things, it provides complex widgets and convenience functions designed for use
in apps built for the Pantheon desktop.")
    (license license:lgpl3+)))

(define-public pantheon-calculator
  (package
    (name "pantheon-calculator")
    (version "1.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/calculator.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1csxsr2c8qvl97xz9ahwn91z095nzgr0i1mbcb1spljll2sr9lkj"))))
    (build-system meson-build-system)
    (arguments '(#:glib-or-gtk? #t))
    (inputs
     `(("granite" ,granite)
       ("gtk" ,gtk+)))
    (native-inputs
     `(("glib" ,glib)
       ("cmake" ,cmake)
       ("libgee" ,libgee)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/calculator")
    (synopsis "Desktop calculator")
    (description "Calculator is an application for performing simple arithmatic.
It is the default calculator application in the Pantheon desktop.")
    (license license:gpl3)))
