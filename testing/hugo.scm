;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>

(define-module (testing hugo)
  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages syncthing)
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
the @code{net/url} package, modified so as to allow some reserved characters
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
     "This package provides a library to normalize URLs.  It returns a pure
URL (a Pure-ell.)  Based on the Wikipedia article for URLs and RFC 3986.")
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
golang's @code{database/sql} package.")
    (license license:mpl2.0)))

(define-public go-github-com-aws-sdk
  (package
    (name "go-github-com-aws-sdk")
    (version "1.35.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ky5lw2s2zpslnnqcs6hgsrwvwbxwgflb5jwf16dd4aga3vrg10c"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (propagated-inputs
     `(("go-github-com-go-sql-driver-mysql" ,go-github-com-go-sql-driver-mysql)
       ("go-github-com-jmespath-go-jmespath" ,go-github-com-jmespath-go-jmespath)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://github.com/aws/aws-sdk-go")
    (synopsis "Library to access Amazon Web Services (AWS).")
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
     "Gitmap creates a map from filenames to info objects for a given revision
of a Git repository.")
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
test and at the end, to assist in detecting memory leaks.")
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
       ("go-github-com-google-go-cmp-cmp" ,go-github-com-google-go-cmp-cmp)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)))
    (home-page "https://github.com/bep/tmc")
    (synopsis "Serialization library for golang.")
    (description
     "This package provides serialization with text-based formats like JSON
and YAML and custom type adapters for marshalling and unmarshalling.")
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
     "This package provides a set of image processing filters and
transformations including crop, flip, resize, rotate, blur, pixelate, and
adjustments for brightness, contrast and color balance.")
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
     "This package converts YAML to JSON using @code{go-yaml} and then uses
@code{json.Marshal} and @code{json.Unmarshal} to convert to or from the
struct.")
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
     "This package handles OpenAPI files.  It targets the OpenAPI spec up to
version 3.")
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
    (synopsis "WebSocket implementation for Golang")
    (description
     "This package implements of the WebSocket protocol in golang.  It passes
the Autobahn Test Suite for WebSocket standard compliance.")
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
     "The @code{sentences} tool converts a blob of text into a list of
sentences.")
    (license license:expat)))

(define-public go-golang-org-x-exp
  (let ((commit "c827fd4f18b9f31f0f90e954ffcf1b6a562ce30a")
        (revision "0"))
    (package
      (name "go-golang-org-x-exp")
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
     `(("go-golang-org-x-exp" ,go-golang-org-x-exp)
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
    (synopsis "Library for text processing in golang.")
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
    (synopsis "Write emoji to the terminal using golang")
    (description
     "The @code{emoji} package provides methods for interpolating emoji into strings
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
    (synopsis "Use and manage environment variables in golang.")
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
     "Smartcrop implements an algorithm to find aesthetic crops for images.
It's based on Jonas Wagner's @code{smartcrop.js}.")
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
org-html-export.  The parser supports a subset of org-mode.")
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
     "This package prints Golang literals values.  The output is syntactically
correct golang code.  You can use @code{Litter} to emit data during debug, and
it's also suitable for “snapshot data” in unit tests since it produces
deterministic output.")
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
     "This package calculates the minimal I/O to synchronize the states and
contents of files and directories, and performs that synchronization.")
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
    (synopsis "Retry library for golang.")
    (description
     "This package provides a facility to run a function and retry on error,
up to to a configurable maximum number of retries.")
    (license license:expat)))

(define-public go-github-com-tdewolff-test
  (package
    (name "go-github-com-tdewolff-test")
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tdewolff/test")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "12glhjb4cwp6yxwd17rwa6b4gxna3lm01bgc7yn9di58chc7lyh3"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ; tests broken
       #:import-path "github.com/tdewolff/test"))
    (home-page "https://github.com/tdewolff/test")
    (synopsis "Provides Go test helper functions.")
    (description
     "Test is a helper package written in Go.  It implements a few functions
that are useful for io testing, such as readers and writers that fail after N
consecutive reads/writes.")
    (license license:expat)))

(define-public go-github-com-tdewolff-parse
  (package
    (name "go-github-com-tdewolff-parse")
    (version "2.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tdewolff/parse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "185y4x0nd3d1x3444dqcbpcxw2y8h5d21yi38qyi2705km69fsjv"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tdewolff/parse"))
    (native-inputs
     `(("go-github-com-tdewolff-test" ,go-github-com-tdewolff-test)))
    (home-page "https://github.com/tdewolff/parse")
    (synopsis "Go parsers for web formats.")
    (description
     "This package contains lexers and parsers written in Go for CSS, HTML,
JavaScript, JSON, SVG, and XML.")
    (license license:expat)))


(define-public go-github-com-tdewolff-minify
  (package
    (name "go-github-com-tdewolff-minify")
    (version "2.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tdewolff/minify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ixv403g919igi1i6rqvdgikvi6jqnf26n462ir2ap2qk7r1prgr"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tdewolff/minify"))
    (propagated-inputs
     `(("go-github-com-cheekybits-is" ,go-github-com-cheekybits-is)
       ("go-github-com-dustin-go-humanize" ,go-github-com-dustin-go-humanize)
       ("go-github-com-fsnotify-fsnotify" ,go-github-com-fsnotify-fsnotify)
       ("go-github-com-matryer-try" ,go-github-com-matryer-try)
       ("go-github-com-spf13-pflag" ,go-github-com-spf13-pflag)
       ("go-github-com-tdewolff-parse" ,go-github-com-tdewolff-parse)
       ("go-github-com-tdewolff-test" ,go-github-com-tdewolff-test)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/tdewolff/minify")
    (synopsis "Go minifiers for web formats.")
    (description
     "This package provides HTML5, CSS3, JS, JSON, SVG and XML minifiers and
an interface to implement any other minifier.")
    (license license:expat)))

(define-public go-github-com-geertjohan-go-incremental
  (package
    (name "go-github-com-geertjohan-go-incremental")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GeertJohan/go.incremental")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ll6n84dk2yhkikxr5zch56qvm8q6jsz68xpdd3bph5fxkikgvfl"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/GeertJohan/go.incremental"))
    (home-page "https://github.com/GeertJohan/go.incremental")
    (synopsis "Concurency-safe incremental numbers.")
    (description
     "This package provides typed incremental counters that are
concurrency-safe.")
    (license license:expat)))

(define-public go-github-com-akavel-rsrc
  (package
    (name "go-github-com-akavel-rsrc")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/akavel/rsrc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lhxclsrv0sik5zi8x5sfm53bii1lw1bvz5f98vj1g9gl5llb4l7"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/akavel/rsrc"))
    (home-page "https://github.com/akavel/rsrc")
    (synopsis "Embed .ico & manifest resources in Go programs for Windows.")
    (description
     "The rsrc utility embeds binary resources in Go programs.  It generates a
.syso file with specified resources embedded in .rsrc section, aimed for
consumption by Go linker when building Windows excecutables.")
    (license license:expat)))

(define-public go-github-com-nkovacs-streamquote
  (package
    (name "go-github-com-nkovacs-streamquote")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nkovacs/streamquote")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08yddkjd0gkp8r6nw3r9arbcc2bmij73m8vvrqwl4704qiys3vyl"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nkovacs/streamquote"))
    (home-page "https://github.com/nkovacs/streamquote")
    (synopsis "A streaming version of Go's strconv.Quote.")
    (description
     "This package provides a library to quote the data in an io.Reader and
write it out to an io.Writer in constant memory space.")
    (license license:expat)))

(define-public go-github-com-valyala-bytebufferpool
  (package
    (name "go-github-com-valyala-bytebufferpool")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/valyala/bytebufferpool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01lqzjddq6kz9v41nkky7wbgk7f1cw036sa7ldz10d82g5klzl93"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/valyala/bytebufferpool"))
    (home-page "https://github.com/valyala/bytebufferpool")
    (synopsis "Anti-memory-waste byte buffer pool.")
    (description
     "This package provides a pool of byte buffers with anti-memory-waste
protection.  The pool may waste limited amount of memory due to
fragmentation.  This amount equals to the maximum total size of the byte
buffers in concurrent use.")
    (license license:expat)))

(define-public go-github-com-valyala-fasttemplate
  (package
    (name "go-github-com-valyala-fasttemplate")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/valyala/fasttemplate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0bd1hxxg1bzx307adpd1s4yjaa6q51cqk4qvqskfflxvgll79vdn"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/valyala/fasttemplate"))
    (propagated-inputs
     `(("go-github-com-valyala-bytebufferpool" ,go-github-com-valyala-bytebufferpool)))
    (home-page "https://github.com/valyala/fasttemplate")
    (synopsis "Template engine for Go.")
    (description
     "This package substitutes template placeholders with provided values.")
    (license license:expat)))

(define-public go-github-com-daaku-go-zipexe
  (package
    (name "go-github-com-daaku-go-zipexe")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/daaku/go.zipexe")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1lss8j1n9rmj3lb85r29q5ix72zx4g9r829v75v4b2m40rgi5w13"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/daaku/go.zipexe"))
    (propagated-inputs
     `(("go-github-com-geertjohan-go-incremental" ,go-github-com-geertjohan-go-incremental)
       ("go-github-com-akavel-rsrc" ,go-github-com-akavel-rsrc)
       ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github.com-jessevdk-go-flags" ,go-github.com-jessevdk-go-flags)
       ("go-github-com-nkovacs-streamquote" ,go-github-com-nkovacs-streamquote)
       ("go-github-com-valyala-fasttemplate" ,go-github-com-valyala-fasttemplate)))
    (home-page "https://github.com/daaku/go.zipexe")
    (synopsis "Open an executable binary file as a zip file.")
    (description
     "This package opens a zip file, with special handling for binaries that
are augmented with zip data.")
    (license license:expat)))

(define-public go-github-com-geertjohan-go-rice
  (package
    (name "go-github-com-geertjohan-go-rice")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GeertJohan/go.rice")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0m1pkqnx9glf3mlx5jdaby9yxccbl02jpjgpi4m7x1hb4s2gn6vx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/GeertJohan/go.rice"))
    (propagated-inputs
     `(("go-github-com-geertjohan-go-incremental" ,go-github-com-geertjohan-go-incremental)
       ("go-github-com-akavel-rsrc" ,go-github-com-akavel-rsrc)
       ("go-github-com-daaku-go-zipexe" ,go-github-com-daaku-go-zipexe)
       ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github.com-jessevdk-go-flags" ,go-github.com-jessevdk-go-flags)
       ("go-github-com-nkovacs-streamquote" ,go-github-com-nkovacs-streamquote)
       ("go-github-com-valyala-fasttemplate" ,go-github-com-valyala-fasttemplate)))
    (home-page "https://github.com/GeertJohan/go.rice")
    (synopsis "Bundle static resource data with an executable.")
    (description
     "This tool loads static resource data, like HTML or images, and bundles
them together with an executable file.")
    (license license:expat)))

(define-public go-github-com-alecthomas-kong
  (package
    (name "go-github-com-alecthomas-kong")
    (version "0.2.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/kong")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1x1i5knsf45bhpx5r76hfmb1pxvclbjmazm2g39qrax8l8i0k4m9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/alecthomas/kong"))
    (propagated-inputs
     `(("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/alecthomas/kong")
    (synopsis "Command-line parser for Go .")
    (description
     "Kong aims to support arbitrarily complex command-line structures with as
little developer effort as possible.  To achieve that, command-lines are
expressed as Go types, with the structure and tags directing how the command
line is mapped onto the struct.")
    (license license:expat)))

(define-public go-github-com-alecthomas-kong-hcl
  (package
    (name "go-github-com-alecthomas-kong-hcl")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/kong-hcl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0q95qix59jzp36a264sr9wgjvq6h8cv6kzmfxi3pgr262jg6avkm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/alecthomas/kong-hcl"))
    (propagated-inputs
     `(("go-github-com-alecthomas-kong" ,go-github-com-alecthomas-kong)
       ("go-github-com-alecthomas-repr" ,go-github-com-alecthomas-repr)
       ("go-github-com-hashicorp-hcl" ,go-github-com-hashicorp-hcl)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/alecthomas/kong-hcl")
    (synopsis "Kong configuration loader for HCL.")
    (description
     "This package loads Kong configuration values from HCL files.")
    (license license:expat)))

(define-public go-github-com-gorilla-csrf
  (package
    (name "go-github-com-gorilla-csrf")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/csrf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0iryq0z48yi7crfbd8jxyn7lh1gsglpiglvjgnf23bz6xfisssav"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/csrf"))
    (propagated-inputs
     `(
       ;; gorilla/securecookie
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
    (home-page "https://github.com/gorilla/csrf")
    (synopsis "CSRF prevention middleware for Go web applications & services.")
    (description
     "This package is a HTTP middleware library that provides cross-site
request forgery (CSRF) protection.")
    (license license:expat)))

(define-public go-github-com-gorilla-securecookie
  (package
    (name "go-github-com-gorilla-securecookie")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/securecookie")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "16bqimpxs9vj5n59vm04y04v665l7jh0sddxn787pfafyxcmh410"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/securecookie"))
    (home-page "https://github.com/gorilla/securecookie")
    (synopsis "Encodes and decodes secure cookie values for Go web applications.")
    (description
     "This package encodes and decodes authenticated and optionally encrypted
cookie values.  Secure cookies can't be forged, because their values are
validated using HMAC.  When encrypted, the content is also inaccessible to
malicious eyes.")
    (license license:expat)))

(define-public go-github-com-gorilla-csrf
  (package
    (name "go-github-com-gorilla-csrf")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/csrf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0iryq0z48yi7crfbd8jxyn7lh1gsglpiglvjgnf23bz6xfisssav"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/csrf"))
    (propagated-inputs
     `(("go-github-com-gorilla-securecookie" ,go-github-com-gorilla-securecookie)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
    (home-page "https://github.com/gorilla/csrf")
    (synopsis "CSRF prevention middleware for Go web applications & services.")
    (description
     "This package is a HTTP middleware library that provides cross-site
request forgery (CSRF) protection.")
    (license license:expat)))

(define-public go-github-com-felixge-httpsnoop
  (package
    (name "go-github-com-felixge-httpsnoop")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/felixge/httpsnoop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ncd8lar5zxiwjhsp315s4hsl4bhnm271h49jhyxc66r5yffgmac"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/felixge/httpsnoop"))
    (home-page "https://github.com/felixge/httpsnoop")
    (synopsis "Capture http related metrics.")
    (description
     "This package provides an easy way to capture http related
metrics (i.e. response time, bytes written, and http status code) from your
application's http.Handlers.")
    (license license:expat)))

(define-public go-github-com-gorilla-handlers
  (package
    (name "go-github-com-gorilla-handlers")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/handlers")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15gycdz9lkjnsvvichsbdf25vf6pi1sfn41khhz53iqf300l0w0s"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ; tests require network connection
       #:import-path "github.com/gorilla/handlers"))
    (propagated-inputs
     `(("go-github-com-felixge-httpsnoop" ,go-github-com-felixge-httpsnoop)))
    (home-page "https://github.com/gorilla/handlers")
    (synopsis "CSRF prevention middleware for Go web applications & services.")
    (description
     "This package is a HTTP middleware library that provides cross-site
request forgery (CSRF) protection.")
    (license license:expat)))

(define-public go-github-com-yuin-goldmark-highlighting
  (let ((commit "60d527fdb691b855b41a5b21ac612baca3a1dc1a")
        (revision "0"))
    (package
      (name "go-github-com-yuin-goldmark-highlighting")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yuin/goldmark-highlighting")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0l9ziiand3fagx5mimpplq45fnrnbfv6i4h546q3d3bpvm8d7cv3"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/yuin/goldmark-highlighting"))
      (propagated-inputs
       `(("go-github-com-geertjohan-go-rice" ,go-github-com-geertjohan-go-rice)
         ("go-github-com-alecthomas-chroma" ,go-github-com-alecthomas-chroma)
         ("go-github-com-alecthomas-kong-hcl" ,go-github-com-alecthomas-kong-hcl)
         ("go-github-com-dlclark-regexp2" ,go-github-com-dlclark-regexp2)
         ("go-github-com-gorilla-csrf" ,go-github-com-gorilla-csrf)
         ("go-github-com-gorilla-handlers" ,go-github-com-gorilla-handlers)
         ("go-github-com-gorilla-mux" ,go-github-com-gorilla-mux)
         ("go-github-com-yuin-goldmark" ,go-github-com-yuin-goldmark)
         ("go-github-com-danwakefield-fnmatch" ,go-github-com-danwakefield-fnmatch)))
      (home-page "https://github.com/yuin/goldmark-highlighting")
      (synopsis "Go minifiers for web formats.")
      (description
       "This package provides HTML5, CSS3, JS, JSON, SVG and XML minifiers and
an interface to implement any other minifier.")
      (license license:expat))))

(define-public go-golang-org-x-mod
  (package
    (name "go-golang-org-x-mod")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/mod")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ldgbx2zpprbsfn6p8pfgs4nn87gwbfcv2z0fa7n8alwsq2yw78q"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/mod"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (propagated-inputs
     `(("golang.org/x/crypto" ,go-golang-org-x-crypto)
       ("golang.org/x/tools" ,go-golang-org-x-tools)
       ("golang.org/x/xerrors" ,go-golang-org-x-xerrors)))
    (home-page "https://pkg.go.dev/mod/golang.org/x/mod")
    (synopsis "Library for golang module mechanics.")
    (description
     "This package provides tools that work directly with, and manipulate, Go
modules.")
    (license license:expat)))

(define-public go-github-com-golang-mock
  (package
    (name "go-github-com-golang-mock")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/mock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zb4n285mv85dh3y2f8fqbs30h2dhzw7vbczr6z0zg2gqr6vzg0q"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golang/mock/gomock"
       #:unpack-path "github.com/golang/mock"))
    (inputs
     `(("golang.org/x/mod" ,go-golang-org-x-mod)
       ("golang.org/x/tools" ,go-golang-org-x-tools)))
    (home-page "https://github.com/golang/mock")
    (synopsis "Mocking framework for golang.")
    (description
     "This package provides a framework for creating or automatically
generating mock interfaces, useful for testing.")
    (license license:asl2.0)))

(define-public go-github-com-google-martian
  (package
    (name "go-github-com-google-martian")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/martian")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ngvs4l6a06s8scnliq9i5pwvwpzfpa5wf9skg0gcvysli1rm3fm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/martian"))
    (inputs
     `(("golang.org/x/net" ,go-golang-org-x-net)))
    (home-page "https://github.com/google/martian")
    (synopsis "Library for building custom HTTP/S proxies")
    (description
     "Martian Proxy is a programmable HTTP proxy designed to be used for
testing.  It assists in verification that all (or some subset) of requests are
secure, mocking external services at the network layer, injecting headers,
modifying cookies or performing other mutations of HTTP requests and
responses, verifying that pingbacks happen when you think they should, and
unwrapping encrypted traffic.")
    (license license:asl2.0)))

(define-public go-github-com-chzyer-logex
  (package
    (name "go-github-com-chzyer-logex")
    (version "1.1.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chzyer/logex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08pbjj3wx9acavlwyr055isa8a5hnmllgdv5k6ra60l5y1brmlq4"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/chzyer/logex"
       ;; tests have hard-coded values that are broken with recent versions of go
       #:tests? #f))
    (home-page "https://github.com/chzyer/logex")
    (synopsis "Logging library for golang")
    (description
     "This package provides a library for logging events in golang.  It
supports error tracing, log levels, and wrapping by the standard log
library.")
    (license license:expat)))

(define-public go-github-com-chzyer-test
  (let ((commit "a1ea475d72b168a29f44221e0ad031a842642302")
        (revision "0"))
    (package
      (name "go-github-com-chzyer-test")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/chzyer/test")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0rns2aqk22i9xsgyap0pq8wi4cfaxsri4d9q6xxhhyma8jjsnj2k"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/chzyer/test"))
      (propagated-inputs
       `(("github.com/chzyer/logex" ,go-github-com-chzyer-logex)))
      (home-page "https://github.com/chzyer/test")
      (synopsis "Test utilities for golang")
      (description
       "This package provides functions for testing golang code.")
      (license license:expat))))

(define-public go-github-com-chzyer-readline
  (package
    (name "go-github-com-chzyer-readline")
    (version "1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chzyer/readline")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1qd2qhjps26x4pin2614w732giy89p22b2qww4wg15zz5g2365nk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/chzyer/readline"))
    (native-inputs
     `(("github.com/chzyer/test" ,go-github-com-chzyer-test)))
    (home-page "https://github.com/chzyer/readline")
    (synopsis "Implementation of GNU readline-like library in golang.")
    (description
     "This package provides line editing functions similar to GNU's readline,
written in pure golang.")
    (license license:expat)))

(define-public go-github-com-ianlancetaylor-demangle
  (let ((commit "28f6c0f3b63983aaa99575ca3b693afff7996387")
        (revision "0"))
    (package
      (name "go-github-com-ianlancetaylor-demangle")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ianlancetaylor/demangle")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0rsq3622gd40f1x1l7caidsxrmzg1993ich2higwd94fqbxs1r83"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/ianlancetaylor/demangle"))
      (home-page "https://github.com/ianlancetaylor/demangle")
      (synopsis "Demangler for C++ symbols.")
      (description
       "This package demangles C++ symbol names.  For example, @code{_Z3fooc}
might demangle to @code{foo(char)}.")
      (license license:expat))))

(define-public go-github-com-google-pprof
  (let ((commit "8ef5528bdba278d96081925d3d835926abc9d367")
        (revision "0"))
    (package
      (name "go-github-com-google-pprof")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/pprof")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0ncj96hxgdg9bx5c21gfwv19vy9crvz14hvi2wliqj4mhs6pj7a1"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/google/pprof"))
      (inputs
       `(("github.com/chzyer/logex" ,go-github-com-chzyer-logex)
         ("github.com/chzyer/readline" ,go-github-com-chzyer-readline)
         ("github.com/chzyer/test" ,go-github-com-chzyer-test)
         ("github.com/ianlancetaylor/demangle" ,go-github-com-ianlancetaylor-demangle)
         ("golang.org/x/sys" ,go-golang-org-x-sys)))
      (home-page "https://github.com/google/pprof")
      (synopsis "Tool for visualization and analysis of profiling data")
      (description
       "This tool reads a collection of profiling samples in
@code{profile.proto} format and generates reports to visualize and help
analyze the data.  It can generate both text and graphical reports.")
      (license license:asl2.0))))

(define-public go-golang-org-x-lint
  (let ((commit "738671d3881b9731cc63024d5d88cf28db875626")
        (revision "0"))
    (package
      (name "go-golang-org-x-lint")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/golang/lint")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0jkiz4py59jjnkyxbxifpf7bsar11lbgmj5jiq2kic5k03shkn9c"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/lint"
         ;; tests don't pass
         #:tests? #f))
      (inputs
       `(("golang.org/x/tools" ,go-golang-org-x-tools)))
      (home-page "https://github.com/golang/lint")
      (synopsis "Linter for golang source code.")
      (description
       "This tool prints out style mistakes and makes suggestions.  It is
concerned with coding style, seeking to match the accepted style of the open
source Go project.")
      (license license:expat))))

(define-public go-github-com-kisielk-gotool
  (package
    (name "go-github-com-kisielk-gotool")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kisielk/gotool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "14af2pa0ssyp8bp2mvdw184s5wcysk6akil3wzxmr05wwy951iwn"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kisielk/gotool"))
    (home-page "https://github.com/kisielk/gotool")
    (synopsis "Utility functions used to implement the standard @code{cmd/go} tool.")
    (description
     "This package contains utility functions used to implement the standard
@{cmd/go} tool, to assist in writing tools with similar semantics.")
    (license license:expat)))

(define-public go-golang-org-x-tools-0.5
  (let ((commit "9647ced2713a54c8e24f17480b65cf5a3cef1708")
        (revision "0"))
    (package
      (name "go-golang-org-x-tools")
      (version (git-version "0.5.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/tools")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-tools-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1vnidc8kaisdyprylsibddpdksm84c6qr528768yvi93crdmddls"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/tools"
         ;; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           ;; Source-only package
           (delete 'build))))
      (synopsis "Tools that support the Go programming language")
      (description "This package provides miscellaneous tools that support the
Go programming language.")
      (home-page "https://go.googlesource.com/tools/")
      (license license:bsd-3))))

(define-public go-honnef-co-go-tools
  (package
    (name "go-honnef-co-go-tools")
    (version "0.0.1-2020.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dominikh/go-tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1r83gx7k4fiz3wlshhniz1i39xv492nni1nvfxjfqgnmkavb6r4x"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "honnef.co/go/tools"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (inputs
     `(("github.com/BurntSushi/toml" ,go-github-com-burntsushi-toml)
       ("github.com/google/renameio" ,go-github-com-google-renameio)
       ("github.com/kisielk/gotool" ,go-github-com-kisielk-gotool)
       ("github.com/rogpeppe/go-internal" ,go-github-com-rogpeppe-go-internal)
       ("golang.org/x/mod" ,go-golang-org-x-mod)
       ("golang.org/x/tools" ,go-golang-org-x-tools-0.5)))
    (home-page "https://staticcheck.io/")
    (synopsis "Linter for the Go programming language.")
    (description
     "This tool uses static analysis to find bugs and performance issues,
offer simplifications, and enforce style rules.")
    (license license:expat)))

(define-public staticcheck
  (package
    (inherit go-honnef-co-go-tools)
    (name "staticcheck")
    (arguments
     `(#:import-path "honnef.co/go/tools/cmd/staticcheck"
       #:unpack-path "honnef.co/go/tools"))))

(define-public go-github-com-iancoleman-strcase
  (package
    (name "go-github-com-iancoleman-strcase")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iancoleman/strcase")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hxz1i4m82qkw0m4zlkhw427yid4isn6k5cp2q1627sg6jwlk63r"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/iancoleman/strcase"))
    (home-page "https://github.com/iancoleman/strcase")
    (synopsis "Convert text to @code{snake_case} or @code{camelCase}.")
    (description
     "This package provides functions to convert a string to various cases,
e.g. snake case, camel case, or 8 others.")
    (license license:expat)))

(define-public go-github-com-golang-protobuf
  (package
    (name "go-github-com-golang-protobuf-proto")
    (version "1.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/golang/protobuf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m5z81im4nsyfgarjhppayk4hqnrwswr3nix9mj8pff8x9jvcjqw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golang/protobuf"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (synopsis "Go support for Protocol Buffers")
    (description "This package provides Go support for the Protocol Buffers
data serialization format.")
    (home-page "https://github.com/golang/protobuf")
    (license license:bsd-3)))

(define-public go-google-golang-org-protobuf
  (package
    (name "go-google-golang-org-protobuf")
    (version "1.25.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/protocolbuffers/protobuf-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0apfl42x166dh96zfq5kvv4b4ax9xljik6bq1mnvn2240ir3mc23"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "google.golang.org/protobuf"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (propagated-inputs
     `(("github.com/golang/protobuf" ,go-github-com-golang-protobuf)
       ("github.com/google/go-cmp" ,go-github-com-google-go-cmp-cmp)))
    (home-page "https://github.com/protocolbuffers/protobuf-go")
    (synopsis "Go support for Google's protocol buffers.")
    (description
     "Go implementation for protocol buffers, which is a language-neutral,
platform-neutral, extensible mechanism for serializing structured data.")
    (license license:expat)))

(define-public protoc-gen-go
  (package
    (inherit go-google-golang-org-protobuf)
    (name "protoc-gen-go")
    (arguments
     '(#:import-path "google.golang.org/protobuf/cmd/protoc-gen-go"
       #:unpack-path "google.golang.org/protobuf"))))

(define-public go-google-golang-org-grpc
  (package
    (name "go-google-golang-org-grpc")
    (version "1.32.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc/grpc-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1bw145v7d9378xsvzkd66hi6drq6l4my5rshhb1zpiqh05gimacp"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/grpc/grpc-go"))
    (propagated-inputs
     `(("github.com/cncf/udpa/go" "TODO")
       ("github.com/envoyproxy/go-control-plane" "TODO")
       ("github.com/golang/glog" "TODO")
       ("github.com/golang/protobuf" ,go-github-com-golang-protobuf-proto)
       ("github.com/google/go-cmp" ,go-github-com-google-go-cmp-cmp)
       ("github.com/google/uuid" ,go-github-com-google-uuid)
       ("golang.org/x/net" ,go-golang-org-x-net)
       ("golang.org/x/oauth2" ,go-golang-org-x-oauth2)
       ("golang.org/x/sys" ,go-golang-org-x-sys)
       ("google.golang.org/genproto" "TODO")))
    (home-page "https://github.com/grpc/grpc-go")
    (synopsis "The Go language implementation of gRPC, an HTTP/2 based RPC.")
    (description
     "This package implements a general RPC framework for web, mobile and HTTP/2.")
    (license license:asl2.0)))

(define-public go-github-com-googleapis-gax-go
  (package
    (name "go-github-com-googleapis-gax-go")
    (version "2.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/gax-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1lxawwngv6miaqd25s3ba0didfzylbwisd2nz7r4gmbmin6jsjrx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/googleapis/gax-go"))
    (propagated-inputs
     `(("github.com/golang/protobuf" ,go-github-com-golang-protobuf-proto)
       ("golang.org/x/exp" ,go-golang-org-x-exp)
       ("golang.org/x/lint" ,go-golang-org-x-lint)
       ("golang.org/x/tools" ,go-golang-org-x-tools)
       ("google.golang.org/grpc" ,grpc)
       ("honnef.co/go/tools" ,go-honnef-co-go-tools)))
    (home-page "https://godoc.org/github.com/googleapis/gax-go")
    (synopsis "Google API Extensions for Go.")
    (description
     "Google API Extensions for Go is a set of modules which aids the
development of APIs for clients and servers based on gRPC and Google API
conventions.")
    (license license:expat)))

(define-public go-cloud-google-com-go
  (package
    (name "go-cloud-google-com-go")
    (version "0.68.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "008h1d2k77m61zzdjra1pm8cxjhpcb2f7aa6rfcqxnh3a0b6qzjs"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "cloud.google.com/go"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (propagated-inputs
     `(("github.com/golang/mock" ,go-github-com-golang-mock)
       ("github.com/golang/protobuf" ,go-github-com-golang-protobuf-proto)
       ("github.com/google/go-cmp" ,go-github-com-google-go-cmp-cmp)
       ("github.com/google/martian/v3" ,go-github-com-google-martian)
       ("github.com/google/pprof" ,go-github-com-google-pprof)
       ;; googleapis/gax-go/v2
       ;; jstemmer/go-junit-report
       ;; go.opencensus.io
       ;; x/lint
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang-org-x-oauth2" ,go-golang-org-x-oauth2)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-tools" ,go-golang-org-x-tools)
       ;; google.golang.org/api
       ;; google.golang.org/genproto
       ;; google.golang.org/grpc
       ))
    (home-page "https://cloud.google.com/")
    (synopsis "Google Cloud client libraries for Go")
    (description
     "This package provides client libraries for dozens of Google cloud
products.")
    (license license:asl2.0)))

(define-public go-gocloud-dev
  (package
    (name "go-gocloud-dev")
    (version "0.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-cloud")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0zmqm8k4gxvivhpq3gpdqf9lnm9qj1ryyg9nm0rh3cvman5y07ci"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gocloud.dev"))
    (propagated-inputs
     `(
       ;; lots of deps go here
       ("go-cloud-google-com-go" ,go-cloud-google-com-go)
       ;;
       ("go-github-com-aws-sdk" ,go-github-com-aws-sdk)
       ("go-github-com-fsnotify-fsnotify" ,go-github-com-fsnotify-fsnotify)
       ("go-github-com-go-sql-driver-mysql" ,go-github-com-go-sql-driver-mysql)
       ;;
       ;; protobuf
       ("go-github-com-google-go-cmp-cmp" ,go-github-com-google-go-cmp-cmp)
       ;; go-replayers
       ;; martian
       ("go-github-com-google-uuid" ,go-github-com-google-uuid)
       ;; google/wire
       ;; googleapis/gax-go
       ("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ;; go-ieproxy
       ("go-github-com-mitchellh-mapstructure" ,go-github-com-mitchellh-mapstructure)
       ;; opencensus
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang-org-x-oauth2" ,go-golang-org-x-oauth2)
       ("go-golang-org-x-sync" ,go-golang-org-x-sync)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-golang-org-x-tools" ,go-golang-org-x-tools)
       ;; x/xerrors
       ;; google.golang.org/api
       ;; google.golang.org/genproto
       ;; google.golang.org/grpc
       ))
    (home-page "https://gocloud.dev")
    (synopsis "A set of portable APIs for programming AWS, GCP and Azure.")
    (description
     "Go CDK provides commonly used, vendor-neutral generic APIs that you can
deploy across cloud providers.")
    (license license:asl2.0)))

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
       ("esbuild" ,esbuild)
       ("go-github-com-fortytw2-leaktest" ,go-github-com-fortytw2-leaktest)
       ("go-github-com-frankban-quicktest" ,go-github-com-frankban-quicktest)
       ("go-github-com-fsnotify-fsnotify" ,go-github-com-fsnotify-fsnotify)
       ("go-github-com-getkin-kin-openapi" ,go-github-com-getkin-kin-openapi)
       ("go-github-com-ghodss-yaml" ,go-github-com-ghodss-yaml)
       ("go-github-com-gobwas-glob" ,go-github-com-gobwas-glob)
       ;; github.com/gohugoio/testmodBuilder -- circular dep??
       ("go-github-com-google-go-cmp-cmp" ,go-github-com-google-go-cmp-cmp)
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
       ("go-github-com-tdewolff-minify" ,go-github-com-tdewolff-minify)
       ("go-github-com-yuin-goldmark" ,go-github-com-yuin-goldmark)
       ("go-github-com-yuin-goldmark-highlighting" ,go-github-com-yuin-goldmark-highlighting)
       ;; gocloud.dev
       ("go-golang-org-x-image" ,go-golang-org-x-image)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang.org-x-sync-errgroup" ,go-golang.org-x-sync-errgroup)
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
