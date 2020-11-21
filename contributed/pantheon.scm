;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (contributed pantheon)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
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
    (version "5.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/granite")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13qfhq8xndikk6kmybibs6a4ddyp6mhvbsp2yy4qr7aiiyxf7mna"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-icon-cache
           (lambda _
             (setenv "DESTDIR" "/")
             #t)))))
    (inputs
     `(("glib" ,glib)
       ("gtk" ,gtk+)
       ("libgee" ,libgee)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
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

(define-public sideload
  (package
    (name "sideload")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/sideload")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mlc3nm2navzxm8k1rwpbw4w6mv30lmhqybm8jqxd4v8x7my73vq"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags (list (string-append "-Dflatpak="
                                              (assoc-ref %build-inputs "flatpak")
                                              "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'set-environment-variables
           (lambda _
             ;; Disable compiling schemas and updating desktop databases
             (setenv "DESTDIR" "/")
             #t))
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/io.elementary.sideload"))
                    (link (string-append out "/bin/sideload")))
               (symlink bin link)
               #t))))))
    (inputs
     `(("flatpak" ,flatpak)
       ("glib" ,glib)
       ("granite" ,granite)
       ("gtk" ,gtk+)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("libgee" ,libgee)
       ("libostree" ,libostree)
       ("libxml2" ,libxml2)))
    (propagated-inputs
     ;; Sideload needs these in the environment to fetch data securely from
     ;; Flatpak remotes.
     `(("gnupg" ,gnupg)
       ("gpgme" ,gpgme)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/sideload")
    (synopsis "Graphical application to side-load Flatpaks")
    (description "Sideload handles flatpakref files, like those you might find
on Flathub or another third-party website providing a Flatpak app for
download.")
    (license license:gpl3+)))
