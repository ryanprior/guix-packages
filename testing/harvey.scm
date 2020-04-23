(define-module (gnu packages harvey)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk))

(define-public harvey
  (package
    (name "harvey")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/danrabbit/harvey/archive/"
                                  version ".tar.gz"))
              (sha256 (base32 "0n57h4541v2vdphwb87qzn3k2p7ppyzkbdnw2fkhbd7awvcfbn2i"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/com.github.danrabbit.harvey"))
                    (link (string-append out "/bin/harvey")))
               (symlink bin link)))))))
    (inputs
     `(("gtk" ,gtk+)))
    (native-inputs
     `(("vala" ,vala)
       ("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)))
    (home-page "https://appcenter.elementary.io/com.github.danrabbit.harvey/")
    (synopsis "Calculate and visualize color contrast")
    (description "Harvey calculates and visualizes color contrast.
It checks a given set of colors for WCAG contrast compliance.")
    (license gpl2+)))

harvey
