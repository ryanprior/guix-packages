(define-module (gnu packages harvey)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk))

(define-public harvey
  (package
    (name "harvey")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/danrabbit/harvey"
                                  "/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0n57h4541v2vdphwb87qzn3k2p7ppyzkbdnw2fkhbd7awvcfbn2i"))))
    (build-system meson-build-system)
    (inputs
     `(("vala" ,vala)
       ("gtk" ,gtk+)))
    (native-inputs
     `(("glib" ,glib "bin")))
    (arguments
     `(#:tests? #f))
    (home-page "https://appcenter.elementary.io/com.github.danrabbit.harvey/")
    (synopsis "Calculate and visualize color contrast")
    (description "The hero that Gotham needs right now.
Harvey calculates and visualizes color contrast.
It checks a given set of colors for WCAG contrast compliance.")
    (license gpl2+)))
