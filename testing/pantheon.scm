;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 L  p R n  d n <guix@lprndn.info>

(define-module (testing pantheon)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages inkscape)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public pantheon-gtk-theme
  (package
    (name "pantheon-gtk-theme")
    (version "5.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/stylesheet.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0aqq0d21mqgrfiyhpfa8k51wxw2pia0qlsgp0sli79v7nwn3ykbq"))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (build-system meson-build-system)
    (home-page "https://github.com/elementary/stylesheet")
    (synopsis "GTK theme for Pantheon")
    (description "Theme for the Pantheon desktop environment using a custom
designed GTK-CSS stylesheet.")
    (license license:gpl3+)))

(define-public pantheon-icon-theme
  (package
    (name "pantheon-icon-theme")
    (version "5.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/icons.git")
                    (commit version)))
              (sha256
               (base32 "0rs68cb39r9vq85pr8h3mgmyjpj8bkhkxr5cz4cn5947kf776wg9"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list "-Dvolume_icons=false")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("inkscape" ,inkscape)
       ("xcursorgen" ,xcursorgen)
       ("librsvg" ,librsvg)))
    (inputs
     `(("gtk+" ,gtk+)))
    (propagated-inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)))
    (home-page "https://github.com/elementary/icons")
    (synopsis "Icons for Pantheon")
    (description "Vector icon theme for Pantheon.  It is not a comprehensive
universal icon set; its coverage is tailored for the software available in
elementary OS, for which Pantheon is the first-party desktop environment.")
    (license license:gpl3+)))
