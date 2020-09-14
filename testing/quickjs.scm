(define-module (testing quickjs)
  #:use-module (gnu packages time)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public quickjs
  (package
    (name "quickjs")
    (version "2020-09-06")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bellard.org/quickjs/quickjs-"
                                  version ".tar.xz"))
              (sha256
               (base32 "05vpnnzmws7plnwsnk2brwf7whyj84l5xl0iahi1xdn6rpla6880"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "prefix="
             (string-append "DESTDIR=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'check
           (lambda _
             (invoke "make" "microbench"))))))
    (home-page "https://bellard.org/quickjs/")
    (synopsis "Small embeddable Javascript engine")
    (description "QuickJS supports the ES2020 specification including modules,
asynchronous generators, proxies, BigInt and BigDecimal.  It can compile
Javascript sources to executables with no external dependency.  It includes a
command line interpreter with contextual colorization implemented in
Javascript and a small built-in standard library with C library wrappers.")
    (license license:expat)))
