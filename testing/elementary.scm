(define-module (gnu packages elementary)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix licenses)
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
    (license lgpl3+)))

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
    (license gpl3)))

sideload
