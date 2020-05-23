;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (proposed proton)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages vpn)
  #:use-module (guix build-system python)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public protonvpn-cli
  (package
    (name "protonvpn-cli")
    (version "2.2.2")
    (source
     (origin
       ;; PyPI has a ".whl" file but not a proper source release.
       ;; Thus, fetch code from Git.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonVPN/linux-cli.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ixjb02kj4z79whm1izd8mrn2h0rp9cmw4im1qvp93rahqxdd4n8"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; no tests in repo
    (native-inputs
     `(("docopt" ,python-docopt)))
    (inputs
     `(("pythondialog" ,python-pythondialog)
       ("requests" ,python-requests)))
    (propagated-inputs
     `(("openvpn" ,openvpn)))
    (synopsis "Command-line client for ProtonVPN")
    (description
     "Official CLI for ProtonVPN, a secure point-to-point virtual private
networking service with a gratis tier.  It can automatically find and connect
to the fastest servers or use TOR over VPN.  The gratis tier offers unlimited
bandwidth for up to 10 devices.")
    (home-page "https://github.com/ProtonVPN/linux-cli")
    (license license:gpl3+)))
