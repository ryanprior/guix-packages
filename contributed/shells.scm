;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (contributed shells)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages web)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public oil
  (package
    (name "oil")
    (version "0.8.pre6") ; "Despite the pre4 version qualifier, this is by far
                         ; the best Oil release ever… I may change the version
                         ; numbering scheme in the near future to reflect this."
                         ; - upstream on whether to ship pre4 in Guix
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.oilshell.org/download/oil-"
                           version ".tar.gz"))
       (sha256
        (base32
         "11nfwn5b1w74hv78065jg2zm45mqzi59381b0f649j7n3g7yp3iq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:strip-binaries? #f ; Strip breaks the binary.
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             (substitute* "configure"
               ((" cc ") " gcc "))
             (let ((out (assoc-ref outputs "out")))
               (invoke
                "./configure"
                (string-append "--prefix=" out)
                "--with-readline"))))
         (replace 'check ; The tests are not distributed in the tarballs but
                         ; upstream recommends running this smoke test.
           (lambda _
             (let* ((oil "_bin/oil.ovm"))
               (invoke/quiet oil "osh" "-c" "echo hi")
               (invoke/quiet oil "osh" "-n" "configure")))))))
    (native-inputs
     `(("readline" ,readline)))
    (home-page "https://www.oilshell.org")
    (synopsis "A Unix shell")
    (description "Oil is a Unix shell and programming language. It's our upgrade
path from Bash.")
    (license (list license:psfl ; Tarball vendors python2.7
                   license:asl2.0))))

oil
