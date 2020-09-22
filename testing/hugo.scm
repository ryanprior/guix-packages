;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (testing hugo)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public go-github-com-puerkitobio-urlesc
  (package
    (name "go-github-com-puerkitobio-urlesc")
    (version "de5bf2ad457846296e2031421a34e2568e304e35")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PuerkitoBio/urlesc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0n0srpqwbaan1wrhh2b7ysz543pjs1xw2rghvqyffg9l0g8kzgcw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/PuerkitoBio/urlesc"))
    (home-page "https://github.com/PuerkitoBio/urlesc")
    (synopsis "Implements query escaping as per RFC 3986")
    (description
     "This package escapes queries as per RFC 3986.  It contains some parts of
the net/url package, modified so as to allow some reserved characters
incorrectly escaped by net/url.")
    (license license:expat)))

(define-public go-github-com-puerkitobio-purell
  (package
    (name "go-github-com-puerkitobio-purell")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PuerkitoBio/purell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0c525frsxmalrn55hzzsxy17ng8avkd40ga0wxfw9haxsdjgqdqy"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/PuerkitoBio/purell"))
    (propagated-inputs
     `(("go-github-com-puerkitobio-urlesc" ,go-github-com-puerkitobio-urlesc)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang-org-x-text" ,go-golang-org-x-text)))
    (home-page "https://github.com/PuerkitoBio/purell")
    (synopsis "Normalizes URLs")
    (description
     "Purell is a tiny Go library to normalize URLs.  It returns a pure URL; a
Pure-ell.  Based on the Wikipedia article for URLs and RFC 3986.")
    (license license:expat)))

(define-public go-github-com-armon-go-radix
  (package
    (name "go-github-com-armon-go-radix")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/armon/go-radix")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1m1k0jz9gjfrk4m7hjm7p03qmviamfgxwm2ghakqxw3hdds8v503"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/armon/go-radix"))
    (home-page "https://github.com/armon/go-radix")
    (synopsis "Package that implements a radix tree")
    (description
     "This package provides a single radix tree implementation optimized for
sparse nodes.")
    (license license:expat)))

(define-public go-github-com-go-sql-driver-mysql
  (package
    (name "go-github-com-go-sql-driver-mysql")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-sql-driver/mysql")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11x0m9yf3kdnf6981182r824psgxwfaqhn3x3in4yiidp0w0hk3v"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ;; tests require a network connection
       #:import-path "github.com/go-sql-driver/mysql"))
    (home-page "https://github.com/go-sql-driver/mysql")
    (synopsis "MySQL driver for golang")
    (description
     "This is a pure Go implementaton of the MySQL API, compatible with
golang's database/sql package.")
    (license license:mpl2.0)))

(define-public go-github-com-jmespath-go-jmespath
  (package
    (name "go-github-com-jmespath-go-jmespath")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jmespath/go-jmespath")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18zyr9nlywmwp3wpzcjxrgq9s9d2mmc6zg6xhsna00m663nkyc3n"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jmespath/go-jmespath"))
    (native-inputs
     `(("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://github.com/jmespath/go-jmespath")
    (synopsis "Golang implementation of JMESPath")
    (description
     "This package implements JMESPath, a query language for JSON.  It
transforms one JSON document into another through a JMESPath expression.")
    (license license:asl2.0)))

(define-public go-github-com-aws-sdk
  (package
    (name "go-github-com-aws-sdk")
    (version "1.34.27")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pfy76xy60hqcrgkg407gpxk0rmv0njdlzbpmvp2gzglnfm1rk5w"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go/aws"
       #:unpack-path "github.com/aws/aws-sdk-go"))
    (propagated-inputs
     `(("go-github-com-go-sql-driver-mysql" ,go-github-com-go-sql-driver-mysql)
       ("go-github-com-jmespath-go-jmespath" ,go-github-com-jmespath-go-jmespath)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://github.com/aws/aws-sdk-go")
    (synopsis "Library to access Amazon Web Services (AWS)")
    (description
     "This is the official AWS SDK for the Go programming language.")
    (license license:asl2.0)))

(define-public go-github-com-bep-debounce
  (package
    (name "go-github-com-bep-debounce")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bep/debounce")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1vxa9cz2z0cmjixz3gjfp18fzliy9d2q7q6cz0zqs7yqbpjn5f55"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/bep/debounce"))
    (home-page "https://github.com/bep/debounce")
    (synopsis "Debouncer written in Go")
    (description
     "This package removes high-frequency signals from an input, limiting the
timeframe on which it will change.")
    (license license:expat)))

(define-public go-github-com-bep-gitmap
  (package
    (name "go-github-com-bep-gitmap")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bep/gitmap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hvynpflvbn6g1vgrv37njn8005qxdq8l1289ymr37a2p5c1jncm"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ;; tests require the .git directory
       #:import-path "github.com/bep/gitmap"))
    (native-inputs
     `(("git" ,git)))
    (home-page "https://github.com/bep/gitmap")
    (synopsis "Creates a map from filenames to info objects in a Git repo.")
    (description
     "Gitmap provides a fairly fast method to create a map from all the
filenames to info objects for a given revision of a Git repo.")
    (license license:expat)))

(define-public go-golang.org-x-xerrors
  (let ((commit "5ec99f83aff198f5fbd629d6c8d8eb38a04218ca")
        (revision "0"))
    (package
      (name "go-golang.org-x-xerrors")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/xerrors")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dbzc3gmf2haazpv7cgmv97rq40g2xzwbglc17vas8dwhgwgwrzb"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/xerrors"))
      (synopsis "Go 1.13 error values")
      (description
       "This package holds the transition packages for the new Go 1.13 error values.")
      (home-page "https://godoc.org/golang.org/xerrors")
      (license license:bsd-3))))

(define-public go-github-com-google-go-cmp-cmp-0.5.2
  (package
    (inherit go-github-com-google-go-cmp-cmp)
    (name "go-github-com-google-go-cmp-cmp")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-cmp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0qchy411jm9q2l9mf7x3ry2ycaqp9xdhf2nx14qrpzcxfigv2705"))))
    (propagated-inputs
     `(("go-golang.org-x-xerrors" ,go-golang.org-x-xerrors)))))

(define-public hugo
  (package
    (name "hugo")
    (version "0.69.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gohugoio/hugo.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wx13c3nck7zm1j6rhyksgsp1cv6mlmkwhkyijv153d117g7aslz"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gohugoio/hugo"))
    (native-inputs
     `(("go-github-com-burntsushi-locker" ,go-github-com-burntsushi-locker)
       ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
       ("go-github-com-puerkitobio-purell" ,go-github-com-puerkitobio-purell)
       ("go-github-com-alecthomas-chroma" ,go-github-com-alecthomas-chroma)
       ("go-github-com-armon-go-radix" ,go-github-com-armon-go-radix)
       ("go-github-com-aws-sdk" ,go-github-com-aws-sdk)))
    ;; There are a bunch of dependencies; see hugo-deps.org
    ;; TODO figure out which deps are already packaged
    ;; TODO package more go deps as necessary
    (home-page "https://gohugo.io/")
    (synopsis "Fast static site generator")
    (description "Hugo takes a directory with content and templates and renders
them into a full HTML website.")
    (license license:asl2.0)))

hugo
