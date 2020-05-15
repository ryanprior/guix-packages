(define-module (gnu packages elementary)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gperf)
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
       ("gtk+-bin" ,gtk+ "bin"))) ;; for gtk-update-icon-cache
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("libgee" ,libgee)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/granite")
    (synopsis "Library that extends GTK with common widgets and utilities")
    (description "Granite is a companion library for GTK+ and GLib. Among other
things, it provides complex widgets and convenience functions designed for use
in apps built for elementary OS.")
    (license license:lgpl3+)))

(define-public sideload
  (package
    (name "sideload")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/elementary/sideload/archive/"
                                  version ".tar.gz"))
              (sha256 (base32 "0i236hr3iflp4hp0fxczxdkj9d21gq75ai5s3a0z1djqzq4083z5"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags (list (string-append "-Dflatpak="
                                              (assoc-ref %build-inputs "flatpak")
                                              "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/io.elementary.sideload"))
                    (link (string-append out "/bin/sideload")))
               (symlink bin link)))))))
    (inputs
     `(("flatpak" ,flatpak)
       ("granite" ,granite)
       ("gtk" ,gtk+)
       ("libostree" ,libostree)))
    (propagated-inputs
     `(("glib-networking" ,glib-networking)))
    (native-inputs
     `(("cmake" ,cmake)
       ("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("libgee" ,libgee)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/sideload")
    (synopsis "Graphical application to side-load Flatpaks")
    (description "Sideload handles flatpakref files, like those you might find
on Flathub or another third-party website providing a Flatpak app for
download.")
    (license license:gpl3)))

(define-public appstream
  (package
    (name "appstream")
    (version "0.12.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ximion/appstream.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r4q7xi1xvpjcnyzkzb4pshhvd4agz7cc5nbb3kqb22054zab2qj"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list "-Dvapi=true"
             "-Dstemming=false"
             "-Dapidocs=false"
             "-Dinstall-docs=false")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-stemmer-inc-dirs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "meson.build"
                 (("include_directories\\(\\['\\/usr\\/include'\\]\\)")
                  "''")
                 (("subdir\\('docs\\/'\\)")
                  ""))
               (substitute* "data/meson.build"
                 (("\\/etc")
                   (string-append out "/etc")))
               #t))))))
    (native-inputs
     `(("libxml2" ,libxml2)
       ("gettext" ,gettext-minimal)
       ("libxslt" ,libxslt)
       ("glib2" ,glib)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("pkg-config" ,pkg-config)
       ("libsoup" ,libsoup)
       ("gobject-introspection" ,gobject-introspection)
       ("libyaml" ,libyaml)
       ("vala" ,vala)
       ("gperf" ,gperf)
       ("cmake" ,cmake)
       ("lmdb" ,lmdb)))
    (home-page "https://www.freedesktop.org/wiki/Distributions/AppStream/")
    (synopsis "Provides the foundation to build software-center applications")
    (description "AppStream is a cross-distribution effort for enhancing the way
we interact with the software repositories provided by GNU/Linux distributions
by standardizing software component metadata.")
    (license license:gpl2)))

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
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/calculator")
    (synopsis "Desktop calculator")
    (description "Calculator is an application for performing simple arithmatic.
It is the default calculator application in the Pantheon desktop
environment.")
    (license license:gpl3)))

(define-public vte
  (package
    (name "vte")
    (version "0.60.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/vte/"
                                  (version-major+minor version) "/"
                                  "vte-" version ".tar.xz"))
              (sha256
               (base32
                "19ccbw0yca78h5qcnm8claj4fg1pj68nj1fsjqqfpzhj7w72i81m"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list "-Dvapi=true"
             "-D_systemd=false")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("vala" ,vala)
       ("gobject-introspection" ,gobject-introspection)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("gperf" ,gperf)
       ("xmllint" ,libxml2)))
    (propagated-inputs
     `(("gtk+" ,gtk+)                   ; required by vte-2.91.pc
       ("gnutls" ,gnutls)               ; ditto
       ("pcre2" ,pcre2)))               ; ditto
    (home-page "https://www.gnome.org/")
    (synopsis "Virtual Terminal Emulator")
    (description
     "VTE is a library (libvte) implementing a terminal emulator widget for
GTK+, and a minimal sample application (vte) using that.  Vte is mainly used in
gnome-terminal, but can also be used to embed a console/terminal in games,
editors, IDEs, etc.")
    (license license:lgpl2.1+)))

(define-public pantheon-terminal
  (package
    (name "pantheon-terminal")
    (version "5.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/terminal.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "119iwmzbpkj4nmxinqfsh73lx23g8gbl6ha6wc4mc4fq9hpnc9c2"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/io.elementary.terminal"))
                    (link (string-append out "/bin/pantheon-terminal")))
               (symlink bin link)))))))
    (inputs
     `(("granite" ,granite)
       ("gtk" ,gtk+)
       ("vte" ,vte)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("appstream" ,appstream)
       ("libgee" ,libgee)
       ("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/terminal")
    (synopsis "Graphical terminal with opinionated design and thoughtful touches")
    (description "A lightweight, beautiful, and simple terminal application.
Comes with sane defaults, browser-like tabs, sudo paste protection, smart
copy/paste, and little to no configuration.")
    (license license:lgpl3)))

pantheon-terminal
