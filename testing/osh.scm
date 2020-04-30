(define-module (gnu packages osh)
  #:use-module (gnu packages python)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages web)
  #:use-module (gnu packages readline)
  #:use-module (guix download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public osh
  (package
    (name "osh")
    (version "0.8.pre4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.oilshell.org/download/oil-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0m2p8p5hi2r14xx9pphsa0ar56kqsa33gr2w2blc3jx07aqhjpzy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
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
         (delete 'strip)
         ;; (replace 'check
         ;;   (lambda* (#:key outputs #:allow-other-keys)
         ;;     (let* ((out (assoc-ref outputs "out"))
         ;;            (oil (string-append out "/_build/oil.ovm"))
         ;;            (osh "_build/osh"))
         ;;       (symlink oil osh)
         ;;       (invoke osh "-c" "echo hi")
         ;;       (invoke osh "-n" "configure"))))
         )))
    (native-inputs
     `(("glib" ,glib)
       ("python2" ,python-2.7)
       ("libyajl" ,libyajl)
       ("readline" ,readline)))
    (home-page "https://www.oilshell.org")
    (synopsis "A Unix shell")
    (description "Oil is taking shell seriously as a programming language,
rather than treating it as a text-based UI that can be abused to write
programs.")
    (license license:asl2.0)))

osh
