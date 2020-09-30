;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (testing hugo)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
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

(define-public go-github-com-evanw-esbuild
  (package
    (name "go-github-com-evanw-esbuild")
    (version "0.7.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/evanw/esbuild")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "126q9s21d7569cv3whik19p23xs6zf5lv56pqmz9bw87lw2bvrjp"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/evanw/esbuild/cmd/esbuild"
       #:unpack-path "github.com/evanw/esbuild"))
    (propagated-inputs
     `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/evanw/esbuild")
    (synopsis "An extremely fast JavaScript bundler and minifier.")
    (description
     "This is a JavaScript bundler and minifier.  It packages up JavaScript
and TypeScript code for distribution on the web.")
    (license license:expat)))

(define-public go-github-com-fortytw2-leaktest
  (package
    (name "go-github-com-fortytw2-leaktest")
    (version "d73c753520d9250e8f091d70d468a99c71f8bceb")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fortytw2/leaktest")
             (commit version)))
       (file-name (git-file-name name "master"))
       (sha256
        (base32
         "15aqrdfz9gf7k571f6l5dfjy3apjxaxrpj60rqv9bfz2sqw8inlf"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/fortytw2/leaktest"))
    (home-page "https://github.com/fortytw2/leaktest")
    (synopsis "Goroutine leak detector.")
    (description
     "This package takes a snapshot of running goroutines at the start of a
test, and at the end—compares the two et voilà.")
    (license license:expat)))

(define-public go-github-com-frankban-quicktest
  (package
    (name "go-github-com-frankban-quicktest")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/frankban/quicktest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fbxrd6xgxq2r6v4cv6qrlpkhpdaxqb28hnw1hjf4lvcv4kibj6x"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/frankban/quicktest"))
    (propagated-inputs
     `(("go-github-com-google-go-cmp-cmp" ,go-github-com-google-go-cmp-cmp-0.5.2)
       ("go-github-com-kr-pretty" ,go-github-com-kr-pretty)))
    (home-page "https://github.com/frankban/quicktest")
    (synopsis "Quick helpers for testing Go applications")
    (description
     "Package quicktest provides a collection of Go helpers for writing
tests.")
    (license license:expat)))

(define-public go-github-com-bep-golibsass
  (package
    (name "go-github-com-bep-golibsass")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bep/golibsass")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xk3m2ynbydzx87dz573ihwc4ryq0r545vz937szz175ivgfrhh3"))
       (snippet
        '(begin
           ((@ (guix build utils) delete-file-recursively) "libsass_src")
           #t))))
    (build-system go-build-system)
    (arguments
     '(#:build-flags '("-tags" "dev")
       #:import-path "github.com/bep/golibsass/libsass"
       #:unpack-path "github.com/bep/golibsass"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key import-path #:allow-other-keys)
             (invoke "go" "test" import-path "-tags" "dev"))))))
    (propagated-inputs
     `(("libsass" ,libsass)))
    (native-inputs
     `(("go-github-com-frankban-quicktest" ,go-github-com-frankban-quicktest)))
    (home-page "https://github.com/bep/golibsass")
    (synopsis "Easy to use Go bindings for LibSass")
    (description
     "This package provides SCSS compiler support for Go applications.")
    (license license:expat)))


(define-public go-github-com-bep-tmc
  (package
    (name "go-github-com-bep-tmc")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bep/tmc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1jn3j7mh77a5bbwh3hx3r0blzbdj2fvk5wvcfa8fr45qp2zlpbhx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/bep/tmc"))
    (propagated-inputs
     `(("go-github-com-bep-debounce" ,go-github-com-bep-debounce)
       ("go-github-com-frankban-quicktest" ,go-github-com-frankban-quicktest)
       ("go-github-com-google-go-cmp-cmp" ,go-github-com-google-go-cmp-cmp-0.5.2)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://github.com/bep/tmc")
    (synopsis "Debouncer written in Go")
    (description
     "This package removes high-frequency signals from an input, limiting the
timeframe on which it will change.")
    (license license:expat)))

(define-public go-github-com-disintegration-gift
  (package
    (name "go-github-com-disintegration-gift")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/disintegration/gift")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0zdki6ydqgxl7lg23f4885w8ij34sdg8xv7b7yp6c7ffi2ikk07f"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/disintegration/gift"))
    (home-page "https://github.com/disintegration/gift")
    (synopsis "Go image filtering toolkit")
    (description
     "This package provides a set of useful image processing filters.")
    (license license:expat)))

(define-public go-github-com-dustin-go-humanize
  (package
    (name "go-github-com-dustin-go-humanize")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dustin/go-humanize")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1kqf1kavdyvjk7f8kx62pnm7fbypn9z1vbf8v2qdh3y7z7a0cbl3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/dustin/go-humanize"))
    (home-page "https://github.com/dustin/go-humanize")
    (synopsis "Formatters for units to human friendly sizes")
    (description
     "This package provides functions for helping humanize times and sizes, like “7 hours ago.”")
    (license license:expat)))

(define-public go-github-com-ghodss-yaml
  (package
    (name "go-github-com-ghodss-yaml")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ghodss/yaml")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0skwmimpy7hlh7pva2slpcplnm912rp3igs98xnqmn859kwa5v8g"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/ghodss/yaml"))
    (propagated-inputs
     `(("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://github.com/ghodss/yaml")
    (synopsis "Marshal and unmarshal YAML in Golang.")
    (description
     "This package converts YAML to JSON using go-yaml and then uses
json.Marshal and json.Unmarshal to convert to or from the struct.")
    (license license:expat)))

(define-public go-github-com-getkin-kin-openapi
  (package
    (name "go-github-com-getkin-kin-openapi")
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/getkin/kin-openapi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01bqrxf40s2pvcf55szi719g9nwz4yh6c01kr0v581jl249jk1am"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/getkin/kin-openapi/openapi3"
       #:unpack-path "github.com/getkin/kin-openapi"))
    (propagated-inputs
     `(("go-github-com-ghodss-yaml" ,go-github-com-ghodss-yaml)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://github.com/getkin/kin-openapi")
    (synopsis "OpenAPI 3.0 implementation for Go.")
    (description
     "This package handles OpenAPI files.  It targets the latest OpenAPI
version (currently 3), but the project contains support for older OpenAPI
versions too.")
    (license license:expat)))

(define-public go-github-com-gorilla-websocket
  (package
    (name "go-github-com-gorilla-websocket")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/websocket")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mkm9w6kjkrlzab5wh8p4qxkc0icqawjbvr01d2nk6ykylrln40s"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/websocket"))
    (home-page "https://github.com/gorilla/websocket")
    (synopsis "A fast, well-tested and widely used WebSocket implementation for Go")
    (description
     "The Gorilla WebSocket package provides a complete and tested
implementation of the WebSocket protocol.")
    (license license:expat)))

(define-public go-github-com-neurosnap-sentences
  (package
    (name "go-github-com-neurosnap-sentences")
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neurosnap/sentences")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1b64xv5anfbnq6354jaygxapwgkdhbszzi604b96sm682brwl0p7"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/neurosnap/sentences"))
    (home-page "https://github.com/neurosnap/sentences")
    (synopsis "Multilingual command line sentence tokenizer for golang.")
    (description
     "This command line utility will convert a blob of text into a list of
sentences.")
    (license license:expat)))

(define-public go-golang.org-x-exp
  (let ((commit "c827fd4f18b9f31f0f90e954ffcf1b6a562ce30a")
        (revision "0"))
    (package
      (name "go-golang.org-x-exp")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/golang/exp")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0jy1vfk9z84bxj6qm03rdpd8dzy83lwb2gfmd4h4ic0s51lyf6gz"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/exp"
         ;; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'build))))
      (synopsis "Experimental and deprecated packages for golang.")
      (description
       "This package holds experimental and unreliable libraries for golang.
Some may one day be promoted to the main golang packages, or they may be
modified arbitrarily or even disappear altogether.")
      (home-page "https://godoc.org/golang.org/x/exp")
      (license license:bsd-3))))

(define-public go-gonum-org-v1-gonum
  (package
    (name "go-gonum-org-v1-gonum")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gonum/gonum")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03iy9691qh3r9dzy764n65qbbvf4yfmj1kd6cqs6cynlkzhg90i9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gonum.org/v1/gonum"))
    (propagated-inputs
     `(("go-golang.org-x-exp" ,go-golang.org-x-exp)
       ("go-golang-org-x-tools" ,go-golang-org-x-tools)))
    (home-page "https://www.gonum.org/")
    (synopsis "Numeric libraries for golang.")
    (description
     "Gonum is a set of numeric libraries for the Go programming language.  It
contains libraries for matrices, statistics, optimization, and more.")
    (license license:expat)))

(define-public go-github-com-jdkato-prose
  (package
    (name "go-github-com-jdkato-prose")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jdkato/prose")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0n9jv5qb8bv3dkmizbn3pjrkswjbz2xxcn8zyn5hrxnk7cjs62vr"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jdkato/prose"))
    (propagated-inputs
     `(("go-github-com-neurosnap-sentences" ,go-github-com-neurosnap-sentences)
       ("go-gonum-org-v1-gonum" ,go-gonum-org-v1-gonum)))
    (home-page "https://github.com/jdkato/prose")
    (synopsis "Library for text processing, including tokenization,
part-of-speech tagging, and named-entity extraction.")
    (description
     "Prose is a natural language processing library in pure Go.  It supports
tokenization, segmentation, part-of-speech tagging, and named-entity
extraction.")
    (license license:expat)))

(define-public go-github-com-kyokomi-emoji
  (package
    (name "go-github-com-kyokomi-emoji")
    (version "2.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kyokomi/emoji")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0abjjslfip135595v085z1c8pns0lgmm4scqdwxx3nxy4njwqmph"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kyokomi/emoji"))
    (home-page "https://github.com/kyokomi/emoji")
    (synopsis "Emoji terminal output for golang")
    (description
     "The emoji package provides methods for interpolating emoji into strings
and character sequences.")
    (license license:expat)))

(define-public go-github-com-magefile-mage
  (package
    (name "go-github-com-magefile-mage")
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magefile/mage")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0c77xgz2bz4j9sh9v7f49iqyamc4lvvldcmn6v50hk98s9193gbf"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/magefile/mage"))
    (home-page "https://magefile.org/")
    (synopsis "Build tool and task runner like make/rake using Go")
    (description
     "Mage is a make-like build tool using Go.  You write plain-old go
functions, and Mage automatically uses them as Makefile-like runnable
targets.")
    (license license:asl2.0)))

(define-public go-github-com-joho-godotenv
  (package
    (name "go-github-com-joho-godotenv")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joho/godotenv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ri8if0pc3x6jg4c3i8wr58xyfpxkwmcjk3rp8gb398a1aa3gpjm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/joho/godotenv"))
    (home-page "https://github.com/joho/godotenv")
    (synopsis "Loads environment variables from a file.")
    (description
     "Dotenv load variables from a .env file when the environment is
bootstrapped.")
    (license license:expat)))

(define-public go-github-com-gobuffalo-envy
  (package
    (name "go-github-com-gobuffalo-envy")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gobuffalo/envy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "119lb8i5pzbfwddmrfmjjai9m5np4p485bqxhb82jzsxavzc88m7"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ;; TODO tests require mutable filesystem
       #:import-path "github.com/gobuffalo/envy"))
    (propagated-inputs
     `(("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github-com-joho-godotenv" ,go-github-com-joho-godotenv)
       ("go-github-com-rogpeppe-go-internal" ,go-github-com-rogpeppe-go-internal)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/gobuffalo/envy")
    (synopsis "Makes working with environment variables in Go trivial.")
    (description
     "Envy provides convenience methods for working with environment
variables.")
    (license license:expat)))

(define-public go-github-com-markbates-inflect
  (package
    (name "go-github-com-markbates-inflect")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/markbates/inflect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0pd50b8q6bib84yab14csd6nc08hfdapzbh1nnw6qrmc1zxi7r7m"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/markbates/inflect"))
    (propagated-inputs
     `(("go-github-com-gobuffalo-envy" ,go-github-com-gobuffalo-envy)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/markbates/inflect")
    (synopsis "An inflection engine for golang")
    (description
     "The inflect package transforms text into different styles,
capitalizations, spacing, and so on.")
    (license license:expat)))

(define-public go-github-com-mitchellh-hashstructure
  (package
    (name "go-github-com-mitchellh-hashstructure")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/hashstructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0zgl5c03ip2yzkb9b7fq9ml08i7j8prgd46ha1fcg8c6r7k9xl3i"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mitchellh/hashstructure"))
    (home-page "https://github.com/mitchellh/hashstructure")
    (synopsis "Get hash values for arbitrary values in golang.")
    (description
     "This package can be used to key values in a hash (for use in a map, set,
etc.) that are complex.  The most common use case is comparing two values
without sending data across the network, caching values locally (de-dup), and
so on.")
    (license license:expat)))

(define-public go-github-com-gomarkdown-markdown
  (package
    (name "go-github-com-gomarkdown-markdown")
    (version "8c8b3816f167b780c855b6412793ffd5de35ef05")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gomarkdown/markdown")
             (commit  version)))
       (file-name (git-file-name name "master"))
       (sha256
        (base32
         "141l462gxjqhk58s92h0v73wjvavgn9x8yfj9dmjq5s8ckxqfs8q"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gomarkdown/markdown"))
    (home-page "https://github.com/gomarkdown/markdown")
    (synopsis "Markdown parser and HTML renderer for golang.")
    (description
     "This package provides a library for parsing Markdown documents and
rendering them to HTML.  It supports common extensions.")
    (license license:expat)))

(define-public go-github-com-miekg-mmark
  (package
    (name "go-github-com-miekg-mmark")
    (version "1.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/miekg/mmark")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0q2zrwa2vwk7a0zhmi000zpqrc01zssrj9c5n3573rg68fksg77m"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/miekg/mmark"))
    (propagated-inputs
     `(("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
       ("go-github-com-gomarkdown-markdown" ,go-github-com-gomarkdown-markdown)
       ("go-github-com-google-go-cmp-cmp" ,go-github-com-google-go-cmp-cmp)))
    (home-page "https://github.com/miekg/mmark")
    (synopsis "Markdown processor for golang.")
    (description
     "This package provides an advanced markdown dialect that processes files
to produce internet-drafts in XML RFC 7991 format.  Mmark can produce
xml2rfc (aforementioned RFC 7991), RFC 7749 (xml2rfc version 2 - now
deprecated), HTML5 output, markdown and manual pages.")
    (license license:expat)))

(define-public go-github-com-nfnt-resize
  (package
    (name "go-github-com-nfnt-resize")
    (version "83c6a9932646f83e3267f353373d47347b6036b2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nfnt/resize")
             (commit  version)))
       (file-name (git-file-name name "master"))
       (sha256
        (base32
         "005cpiwq28krbjf0zjwpfh63rp4s4is58700idn24fs3g7wdbwya"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nfnt/resize"))
    (home-page "https://github.com/nfnt/resize")
    (synopsis "Pure golang image resizing.")
    (description
     "This package provides image resizing for the Go programming language
with common interpolation methods.")
    (license license:expat)))

(define-public go-github-com-muesli-smartcrop
  (package
    (name "go-github-com-muesli-smartcrop")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/muesli/smartcrop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10ns8nvxjpykgh1rapg1pn0p3r9qvrjifw2p23yha85d9wnk8i1x"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/muesli/smartcrop"))
    (propagated-inputs
     `(("go-golang-org-x-image" ,go-golang-org-x-image)
       ("go-github-com-nfnt-resize" ,go-github-com-nfnt-resize)))
    (home-page "https://github.com/muesli/smartcrop")
    (synopsis "Finds good image crops for arbitrary crop sizes.")
    (description
     "Smartcrop implements an algorithm to find good crops for images.  It's
based on Jonas Wagner's smartcrop.js.")
    (license license:expat)))

(define-public go-github-com-nicksnyder-go-i18n
  (package
    (name "go-github-com-nicksnyder-go-i18n")
    (version "1.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nicksnyder/go-i18n")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zf53fxngc2q9h0jcnf0kccz8h124rl954r92d1k55pk49l97fgx"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ; tests getting "open active.en.toml: permission denied"
       #:import-path "github.com/nicksnyder/go-i18n/v2/goi18n"
       #:unpack-path "github.com/nicksnyder/go-i18n"))
    (inputs
     `(("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://github.com/nicksnyder/go-i18n")
    (synopsis "Translate your Go program into multiple languages.")
    (description
     "This package is a Go library and command-line utility that helps you
translate Go programs into multiple languages.")
    (license license:expat)))

(define-public go-github-com-niklasfasching-go-org
  (package
    (name "go-github-com-niklasfasching-go-org")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/niklasfasching/go-org")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wf44ky7kcr1zylv6n475biai04h5ffa4i640sg7dv01a22nsbap"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/niklasfasching/go-org"))
    (propagated-inputs
     `(("go-github-com-alecthomas-chroma" ,go-github-com-alecthomas-chroma)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-github-com-dlclark-regexp2" ,go-github-com-dlclark-regexp2)
       ("go-github-com-danwakefield-fnmatch" ,go-github-com-danwakefield-fnmatch)))
    (home-page "https://github.com/niklasfasching/go-org")
    (synopsis "Emacs org-mode parser with HTML & pretty printed rendering.")
    (description
     "This package parses org-mode files and provides HTML export with
sensible output.  It does not exactly reproduce the output of
org-html-export.  The parser supports a reasonable subset of org-mode; it is
huge and this package follows the 80/20 rule.")
    (license license:expat)))

(define-public go-github-com-rwcarlsen-goexif
  (let ((commit "9e8deecbddbd4989a3e8d003684b783412b41e7a")
        (revision "0"))
    (package
      (name "go-github-com-rwcarlsen-goexif")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rwcarlsen/goexif")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1drqhzplg72lvrf3qmb9awbggnjqp23hwn2pgvksi3spv17kc9h2"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/rwcarlsen/goexif/exif"
         #:unpack-path "github.com/rwcarlsen/goexif"))
      (home-page "https://github.com/rwcarlsen/goexif")
      (synopsis "Decode embedded EXIF meta data from image files.")
      (description
       "This package provides decoding of basic exif and tiff encoded data.")
      (license license:expat))))

(define-public go-github-com-sanity-io-litter
  (package
    (name "go-github-com-sanity-io-litter")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sanity-io/litter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jr6v0k1ffimgzy5wdf17r0mrwn9cafhnrsyjxz5vqmigvykikw9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/sanity-io/litter"))
    (propagated-inputs
     `(("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/sanity-io/litter")
    (synopsis "Pretty printer library for Go data structures.")
    (description
     "This package prints Go literals, the better with which to litter your
output.  Litter output is syntactically correct golang code.  You can use
Litter to emit data during debug, and it's also suitable for “snapshot data”
in unit tests since it produces deterministic output.")
    (license license:expat)))

(define-public go-github-com-spf13-fsync
  (package
    (name "go-github-com-spf13-fsync")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spf13/fsync")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1z6v7pvlqphq9lqk5sa80ynj3x21yk6s84i41hivgda73lzffg5n"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/spf13/fsync"))
    (propagated-inputs
     `(("go-github-com-spf13-afero" ,go-github-com-spf13-afero)))
    (home-page "https://github.com/spf13/fsync")
    (synopsis "Keeps files or directories in sync.")
    (description
     "This package performs minimal I/O to synchronize the states and contents
of files and directories.")
    (license license:expat)))

(define-public go-github-com-matryer-try
  (package
    (name "go-github-com-matryer-try")
    (version "1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matryer/try")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15f0m5ywihivnvwzcw0mh0sg27aky9rkywvxqszxka9q051qvsmy"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ; tests broken
       #:import-path "github.com/matryer/try"))
    (native-inputs
     `(("go-github-com-cheekybits-is" ,go-github-com-cheekybits-is)))
    (home-page "https://github.com/matryer/try")
    (synopsis "Get hash values for arbitrary values in golang.")
    (description
     "This package can be used to key values in a hash (for use in a map, set,
etc.) that are complex.  The most common use case is comparing two values
without sending data across the network, caching values locally (de-dup), and
so on.")
    (license license:expat)))

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
     '(#:build-flags '("-tags" "extended")
       #:import-path "github.com/gohugoio/hugo"))
    (native-inputs
     `(("go-github-com-burntsushi-locker" ,go-github-com-burntsushi-locker)
       ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
       ("go-github-com-puerkitobio-purell" ,go-github-com-puerkitobio-purell)
       ("go-github-com-puerkitobio-urlesc" ,go-github-com-puerkitobio-urlesc)
       ("go-github-com-alecthomas-chroma" ,go-github-com-alecthomas-chroma)
       ("go-github-com-alecthomas-repr" ,go-github-com-alecthomas-repr)
       ("go-github-com-armon-go-radix" ,go-github-com-armon-go-radix)
       ("go-github-com-aws-sdk" ,go-github-com-aws-sdk)
       ("go-github-com-bep-debounce" ,go-github-com-bep-debounce)
       ("go-github-com-bep-gitmap" ,go-github-com-bep-gitmap)
       ("go-github-com-bep-golibsass" ,go-github-com-bep-golibsass)
       ("go-github-com-bep-tmc" ,go-github-com-bep-tmc)
       ("go-github-com-disintegration-gift" ,go-github-com-disintegration-gift)
       ("go-github-com-dustin-go-humanize" ,go-github-com-dustin-go-humanize)
       ("go-github-com-evanw-esbuild" ,go-github-com-evanw-esbuild)
       ("go-github-com-fortytw2-leaktest" ,go-github-com-fortytw2-leaktest)
       ("go-github-com-frankban-quicktest" ,go-github-com-frankban-quicktest)
       ("go-github-com-fsnotify-fsnotify" ,go-github-com-fsnotify-fsnotify)
       ("go-github-com-getkin-kin-openapi" ,go-github-com-getkin-kin-openapi)
       ("go-github-com-ghodss-yaml" ,go-github-com-ghodss-yaml)
       ("go-github-com-gobwas-glob" ,go-github-com-gobwas-glob)
       ;; github.com/gohugoio/testmodBuilder -- circular dep??
       ("go-github-com-google-go-cmp-cmp" ,go-github-com-google-go-cmp-cmp-0.5.2)
       ("go-github-com-gorilla-websocket" ,go-github-com-gorilla-websocket)
       ("go-github-com-jdkato-prose" ,go-github-com-jdkato-prose)
       ("go-github-com-kyokomi-emoji" ,go-github-com-kyokomi-emoji)
       ("go-github-com-magefile-mage" ,go-github-com-magefile-mage)
       ("go-github-com-markbates-inflect" ,go-github-com-markbates-inflect)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-github-com-miekg-mmark" ,go-github-com-miekg-mmark)
       ("go-github-com-mitchellh-hashstructure" ,go-github-com-mitchellh-hashstructure)
       ("go-github-com-mitchellh-mapstructure" ,go-github-com-mitchellh-mapstructure)
       ("go-github-com-muesli-smartcrop" ,go-github-com-muesli-smartcrop)
       ("go-github-com-nfnt-resize" ,go-github-com-nfnt-resize)
       ("go-github-com-nicksnyder-go-i18n")
       ("go-github-com-niklasfasching-go-org" ,go-github-com-niklasfasching-go-org)
       ("go-github-com-olekukonko-tablewriter" ,go-github-com-olekukonko-tablewriter)
       ("go-github-com-pelletier-go-toml" ,go-github-com-pelletier-go-toml)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-rogpeppe-go-internal" ,go-github-com-rogpeppe-go-internal)
       ("go-github-com-russross-blackfriday" ,go-github-com-russross-blackfriday)
       ("go-github-com-rwcarlsen-goexif" ,go-github-com-rwcarlsen-goexif)
       ("go-github-com-sanity-io-litter" ,go-github-com-sanity-io-litter)
       ("go-github-com-spf13-afero" ,go-github-com-spf13-afero)
       ("go-github-com-spf13-cast" ,go-github-com-spf13-cast)
       ("go-github-com-spf13-cobra" ,go-github-com-spf13-cobra)
       ("go-github-com-spf13-fsync" ,go-github-com-spf13-fsync)
       ("go-github-com-spf13-jwalterweatherman" ,go-github-com-spf13-jwalterweatherman)
       ("go-github-com-spf13-pflag" ,go-github-com-spf13-pflag)
       ("go-github-com-spf13-viper" ,go-github-com-spf13-viper)
       ;; tdewolff/minify/v2
       ("go-github-com-yuin-goldmark" ,go-github-com-yuin-goldmark)
       ;; yuin/goldmark-highlighting
       ;; gocloud.dev
       ("go-golang-org-x-image" ,go-golang-org-x-image)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ;; x/sync
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ;; google.golang.org/api
       ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
       ("go-gopkg-in-ini-v1" ,go-gopkg-in-ini-v1)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
       ))
    ;; There are a bunch of dependencies; see hugo-deps.org
    ;; TODO figure out which deps are already packaged
    ;; TODO package more go deps as necessary
    (home-page "https://gohugo.io/")
    (synopsis "Fast static site generator")
    (description "Hugo takes a directory with content and templates and renders
them into a full HTML website.")
    (license license:asl2.0)))
